%%------------------------------------------------------------------------------
%% Module: main
%% Purpose: Main module for the car simulator application using WX for graphics.
%% Exports: Various functions required for initialization, event handling and car management.
%%------------------------------------------------------------------------------

-module(main).

-behaviour(wx_object).

%% External includes
-include_lib("wx/include/wx.hrl").
-include("header.hrl").

%% Exports
-export([
	start/0,
	init/1,
	handle_event/2,
	handle_sync_event/3,
	handle_info/2,
	handle_cast/2,
	update_ets/2,
	add_to_cars_table/1
	]).

%% Constants
-define(Timer, 60).

%% Imports from other modules
-import(graphics, [
	init/0,
	createImages/0,
	printCars/3
	]).

-import(cars_manager, [
	cars_listener/5,
	move_car/2,
	put_accident_effect/1,
	delete_car/1
	]).

%% Server Name
-define(SERVER, ?MODULE).

%% State Record
-record(state,
 {frame,
	 panel,
	 dc,
	 paint,
	 list,
	 neighborhood,
	 car,
	 key,
	 accident
  }).

%%------------------------------------------------------------------------------
%% @doc Start the main server.
%% @end
%%------------------------------------------------------------------------------
start() ->
  wx_object:start({local,?SERVER},?MODULE,[],[]).

%%------------------------------------------------------------------------------
%% @doc Initialize the system, creating ETS tables, node connections and GUI elements.
%% @end
%%------------------------------------------------------------------------------
init([]) ->
    io:format("nit~p~n",[self()]),
    initialize_car_ets(),
    initialize_accident_ets(),
    initialize_node_monitoring(),
    initialize_node_connections(),
	{Frame, State} = initialize_graphics(),
    initialize_process_dictionary(),
    io:format("nit~p~n",[self()]),
    start_all_servers(),
    start_all_cars(),
    {Frame, State}.

%% Initialize ETS table for cars
initialize_car_ets() ->
    ets:new(cars, [set, public, named_table]).

%% Initialize ETS table for accidents
initialize_accident_ets() ->
    ets:new(accident, [set, public, named_table]).

%% Turn on node monitoring
initialize_node_monitoring() ->
    net_kernel:monitor_nodes(true),
    timer:sleep(200).

%% Connect to all known nodes
initialize_node_connections() ->
    Nodes = [?NODE1, ?NODE2, ?NODE3, ?NODE4],
    lists:foreach(fun(Node) -> net_kernel:connect_node(Node), timer:sleep(200) end, Nodes).

%% Store all node names in the process dictionary
initialize_process_dictionary() ->
    Nodes = [?NODE1, ?NODE2, ?NODE3, ?NODE4],
    lists:foreach(fun(Node) -> put(Node, Node) end, Nodes).

%% Initialize the graphics components
initialize_graphics() ->
    WxServer = wx:new(),
    Frame = wxFrame:new(WxServer, ?wxID_ANY, "MAP", [{size,{1600, 900}}]),
    Panel = wxPanel:new(Frame),
    DC = wxPaintDC:new(Panel),
    Paint = wxBufferedPaintDC:new(Panel),
    {Neighborhood, Car} = createImages(),

    wxFrame:show(Frame),
    erlang:send_after(?Timer, self(), timer),

    wxPanel:connect(Panel, paint, [callback]),
    wxFrame:connect(Frame, close_window),

    {Frame, #state{frame = Frame, panel = Panel, dc = DC, paint = Paint,
        neighborhood = Neighborhood, car = Car}}.


%%------------------------------------------------------------------------------
%% @doc Start all servers across nodes.
%% Each node starts its own server.
%% @end
%%------------------------------------------------------------------------------
start_all_servers() ->
    Nodes = [?NODE1, ?NODE2, ?NODE3, ?NODE4],
    io:format("start_all_servers~p~n",[self()]),
    
    lists:foreach(
        fun(Node) -> rpc:call(Node, server, start, [?NODE1, ?NODE2, ?NODE3, ?NODE4, ?Master])
        end, 
        Nodes
    ),
    
    io:format("lists:foreach(fun(Node) -> rpc:call(Node, server, start, [?NODE1, ?NODE2, ?NODE3, ?NODE4, ?Master]) end, Nodes),~p~n", [self()]).

%%------------------------------------------------------------------------------
%% @doc Start all cars across nodes.
%% Each node may start multiple cars based on its configuration.
%% @end
%%------------------------------------------------------------------------------
start_all_cars() ->
    Car1Args = [{a, 20, [{1595, 310}, left, r1, car], ?NODE1}],
    Car2Args = [{b, 20, [{1595, 605}, left, r2, car], ?NODE4}],
    Car3Args = [
	    {c, 20, [{260, 900}, up, r6, car], ?NODE3},
	    {d, 20, [{609, 900}, up, r4, car], ?NODE3},
	    {e, 20, [{935, 900}, up, r3, car], ?NODE4},
	    {f, 20, [{1297, 900}, up, r5, car], ?NODE4}
    ],
    
    lists:foreach(
        fun(Args) -> lists:foreach(
            fun({Name, Speed, Params, Node}) -> 
                rpc:call(Node, server, start_car, [Name, Speed, Params, Node])
            end, Args
        ) 
        end, 
        [Car1Args, Car2Args, Car3Args]
    ).


%%------------------------------------------------------------------------------
%% @doc Handle node going down.
%% Actions to be taken when a specific node goes down.
%% @end
%%------------------------------------------------------------------------------
handle_node_down(Node, Backup1, Backup2, Backup3, MonitorPC, State) ->
    backup_pc(Node, get(Backup1)),
    rpc:call(get(Backup1), server, update_monitor, [MonitorPC]),
    rpc:call(get(Backup2), server, update_monitor, [MonitorPC]),
    rpc:call(get(Backup3), server, update_monitor, [MonitorPC]),
    move_car(Node, ets:first(cars)),
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @doc Handle window close event.
%% Clean up and destroy window on close.
%% @end
%%------------------------------------------------------------------------------
handle_event(#wx{event = #wxClose{}}, State = #state {frame = Frame}) ->
    io:format("Closing the window...\n"),
    wxWindow:destroy(Frame),
    wx:destroy(),
    {stop, normal, State}.

%%------------------------------------------------------------------------------
%% @doc Handle paint event.
%% Redraws the window with the updated state.
%% @end
%%------------------------------------------------------------------------------
handle_sync_event(#wx{event=#wxPaint{}}, _, _State = #state{
    panel = Panel,
    neighborhood = Neighborhood,
    car = Car
}) ->
    DC2 = wxPaintDC:new(Panel),
    wxDC:clear(DC2),
    wxDC:drawBitmap(DC2, Neighborhood, {0, 0}),
    printCars(ets:first(cars), Panel, Car);

handle_sync_event(_Event, _, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @doc Handle timer events.
%% Triggers the update of the ETS and refreshes the window.
%% @end
%%------------------------------------------------------------------------------
handle_info(timer, State=#state{frame = Frame}) ->
  Nodes = [?NODE1, ?NODE2, ?NODE3, ?NODE4],
  lists:foreach(fun(Node) -> spawn(main, update_ets, [get(Node), ?Master]) end, Nodes),
  wxWindow:refresh(Frame),
  erlang:send_after(?Timer, self(), timer),
  {noreply, State};

handle_info({nodeup, Node}, State) ->
  io:format("The node ~p is up.~n", [Node]),
  {noreply, State};

handle_info({nodedown, Node}, State) ->
    io:format("The node ~p is currently down.~n", [Node]),
    case Node of
        ?NODE1 -> handle_node_down(?NODE1, ?NODE2, ?NODE3, ?NODE4, node_1, State);
        ?NODE2 -> handle_node_down(?NODE2, ?NODE3, ?NODE4, ?NODE1, node_2, State);
        ?NODE3 -> handle_node_down(?NODE3, ?NODE4, ?NODE1, ?NODE2, node_3, State);
        ?NODE4 -> handle_node_down(?NODE4, ?NODE1, ?NODE2, ?NODE3, node_4, State)
    end.

handle_cast({delete_car, Pid}, State) ->
    ets:delete(cars, Pid),
    {noreply, State};

handle_cast({accident, Car, L1, Car2, L2}, State) ->
    ets:insert(accident, {Car, L1}),
    ets:insert(accident, {Car2, L2}),
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @doc Check the connection status and handle disconnection.
%% Tries to ping the node and if successful, disconnects it from the master.
%% @end
%%------------------------------------------------------------------------------
handle_connection_issue(Node, Master) ->
    case net_adm:ping(Node) of
        pong -> rpc:call(Node, erlang, disconnect_node, [Master]);
        _ -> ok
    end.


%%------------------------------------------------------------------------------
%% @doc Insert data into the cars table.
%% The function is recursive, ending when it encounters the '$end_of_table' atom.
%% @end
%%------------------------------------------------------------------------------
add_to_cars_table('$end_of_table') ->
    ok;
add_to_cars_table(DataList) ->
    InsertData = fun(Key_Value) -> ets:insert(cars, Key_Value) end,
    lists:foreach(InsertData, DataList),
    ok.

%%------------------------------------------------------------------------------
%% @doc Backup the connected PCs in case of a failure.
%% Backs up PCs that were connected to a PC that went down.
%% @end
%%------------------------------------------------------------------------------
backup_pc(PCDown, NewPCList) ->
    NodeList = [?NODE1, ?NODE2, ?NODE3, ?NODE4],
    
    ConnectedPCs = [Node || Node <- NodeList, get(Node) == PCDown],
    
    io:format("Connected PCs: ~p~n", [ConnectedPCs]),
    
    UniqueConnectedPCs = lists:usort(ConnectedPCs),
    
    case UniqueConnectedPCs of
        [] ->
            io:format("No PCs were connected to ~p~n", [PCDown]);
        _ ->
            io:format("Backing up PCs connected to ~p...~n", [PCDown]),
            StorePC = fun(E) -> put(E, NewPCList) end,
            lists:foreach(StorePC, UniqueConnectedPCs),
            timer:sleep(500),
            io:format("Backup completed successfully!~n")
    end,
    ok.

%%------------------------------------------------------------------------------
%% @doc Update the ETS.
%% Calls the server to get the state of the cars and updates the local ETS.
%% Handles any errors that might arise during the RPC call.
%% @end
%%------------------------------------------------------------------------------
update_ets(Node, Master) ->
    Result =
        try
            rpc:call(Node, server, get_cars_state, [])
        catch
            _:_ -> problem
        end,
    case Result of
        {ok, List} ->
            add_to_cars_table(List);
        Else ->
            io:format("There is a problem: ~p~n", [Else]),
            handle_connection_issue(Node, Master),
            ok
    end.
