-module(server).
-behaviour(gen_server).

%% API
-export([start/5,
         get_cars_state/0,
         moved_car/6,
         remove_car_from_map/1,
         on_traffic_light/2,
         update_monitor/1,
         accident/4,
         remove_accident_effect/1,
         start_car/4,
         server_detect_junction/2,
         car_detected/2,
         light/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-define(SERVER, ?MODULE).
-record(state, {}).


% Start the server
start(NODE1, NODE2, NODE3, NODE4, Master) ->
    gen_server:start({global, node()}, ?MODULE, [NODE1, NODE2, NODE3, NODE4, Master], []).

%%  Functions for server

% Fetch car states
get_cars_state() ->
    gen_server:call(global:whereis_name(node()), getCarsState).

% Handle car movement
moved_car(Name, Speed, Start, Location, Con, PC) ->
    io:format("Car ~p moved with speed ~p from ~p to ~p in connection ~p from PC ~p~n",
              [Name, Speed, Start, Location, Con, PC]),
    gen_server:cast(global:whereis_name(node()), {updateCarLocation, Name, Speed, Start, Location, Con, PC}).

% Remove car from map
remove_car_from_map(Pid) ->
    io:format("Removing car with pid ~p from map~n", [Pid]),
    gen_server:cast(global:whereis_name(node()), {removeCarFromMap, Pid}).

% Traffic light detection
on_traffic_light(Car, {R, J}) ->
    io:format("Car ~p is on traffic light at position {~p, ~p}~n", [Car, R, J]),
    gen_server:cast(global:whereis_name(node()), {light, Car, {R, J}}).

% Monitor update for a node
update_monitor(PC) ->
    io:format("Updating monitor for PC ~p~n", [PC]),
    gen_server:cast(global:whereis_name(node()), {nodedown, PC}).

% Accident detection between cars
accident(Car, L1, Car2, L2) ->
    io:format("Accident between car ~p at location ~p and car ~p at location ~p~n", [Car, L1, Car2, L2]),
    gen_server:cast(global:whereis_name(node()), {accident, Car, L1, Car2, L2}).

% Remove accident effect
remove_accident_effect(Pid) ->
    io:format("Removing accident effect for car with pid ~p~n", [Pid]),
    gen_server:cast(global:whereis_name(node()), {removeAccEff, Pid}).

% Start a car
start_car(Name, Speed, Start, PC) ->
    io:format("start_car ~p~n", [Name]),
    gen_server:cast(global:whereis_name(node()), {start_car, Name, Speed, Start, PC}).

% Server detects junction
server_detect_junction(X, Y) ->
    io:format("server_detect_junction ~p~n", [X]),
    gen_server:call(global:whereis_name(node()), {server_detect_junction, X, Y}).

% Car detection
car_detected(CarA, CarB) ->
    io:format("car_detected ~p~n", [CarA]),
    gen_server:cast(global:whereis_name(node()), {ctc, CarA, CarB}).

%% gen_server callbacks

init([NODE1, NODE2, NODE3, NODE4, Master]) ->
    % Initialization
    io:format("~nServer initialized with PID: ~p~n", [self()]),
    
    put(master, Master),
    
    % Create tables
    ets:new(cars, [named_table, set, public]),
    ets:new(junction, [set, public, named_table]),
    ets:new(car_a_s, [set, public, named_table]),
    
    % Connect nodes
    connect_nodes(NODE1, NODE2, NODE3, NODE4),
    
    % Start traffic lights
    start_traffic_lights(),
    
    % Start car locations monitor
    UpdateCarLocationsServer = spawn(car_monitor, update_car_locations, [NODE1, NODE2, NODE3, NODE4]),
    put(update_car_locations, UpdateCarLocationsServer),
    
    {ok, #state{}}.



%% Handle asynchronous messages

% Update car location
handle_cast({updateCarLocation, Name, Speed, Start, Location, Con, PC}, State) ->
    cars:start(Name, get(update_car_locations), Speed, Start, Location, Con, PC),
    {noreply, State};

% Handle traffic light situations
handle_cast({light, Who, {R, J}}, State) ->
    spawn(server, light, [neighbor_streets(), Who, {R, J}]),
    {noreply, State};

% Handle node down situations
handle_cast({nodedown, PC}, State) ->
    Pid = get(update_car_locations),
    Pid ! {nodedown, PC},
    {noreply, State};

% Handle accidents
handle_cast({accident, Car, Car1b, Car2, Car2b}, State) ->
    rpc:call(get(master), cars_manager, on_accident, [Car, Car1b, Car2, Car2b]),
    {noreply, State};

% Remove car from map
handle_cast({removeCarFromMap, Pid}, State) ->
    timer:sleep(320),
    rpc:call(get(master), cars_manager, delete_car, [Pid]),
    ets:delete(cars, Pid),
    {noreply, State};

% Remove accident effects
handle_cast({removeAccEff, Pid}, State) ->
    rpc:call(get(master), cars_manager, put_accident_effect, [Pid]),
    {noreply, State};

% Handle car-to-car interactions
handle_cast({ctc, Car, Car2}, State) ->
    cars:stop(Car, Car2),
    {noreply, State};

% Start car
handle_cast({start_car, Name, Speed, Start, PC}, State) ->
    io:format("start_car initiated"),
    cars:start(Name, get(update_car_locations), Speed, Start, PC),
    {noreply, State};

% Handle other unforeseen messages
handle_cast(Else, State) ->
    io:format("error in server: ~p~n", [Else]),
    {noreply, State}.

%% Handle synchronous calls

% Get the current state of all cars
handle_call(getCarsState, _, State) ->
    CurrentCarsList = ets:tab2list(cars),
    {reply, {ok, CurrentCarsList}, State};

% Detect a junction
handle_call({server_detect_junction, X, Y}, _, State) ->
    Res = detect_junction(ets:first(junction), {X, Y}),
    {reply, Res, State};

% Handle other synchronous calls
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% Handle other messages and system events

% Handle system events and other messages
handle_info(_Info, State) ->
    {noreply, State}.

% Cleanup and shutdown
terminate(_Reason, _State) ->
    io:format("Server termination with PID: ~p~n", [self()]),
    ok.



%Helper Func

%% Initialize and start the traffic lights at given coordinates
start_traffic_lights() ->
	TrafficLights = [
		{{r1a, {r1, a}}, [{940, 310}]},
		{{r1b, {r1, b}}, [{620, 310}]},

		{{r1i, {r1, i}}, [{1305, 310}]},
		{{r1k, {r1, k}}, [{620, 310}]},

		{{r2j, {r2, j}}, [{1297, 605}]},
		{{r2e, {r2, e}}, [{260, 605}]},

		{{r2c, {r2, c}}, [{940, 605}]},
		{{r2d, {r2, d}}, [{620, 605}]},

		{{r3e, {r3, e}}, [{940, 605}]},
		{{r3g, {r3, g}}, [{940, 310}]},

		{{r4f, {r4, f}}, [{535, 320}]},
		{{r4h, {r4, h}}, [{535, 620}]},

		{{r5n, {r5, n}}, [{1297, 620}]},
		{{r5m, {r5, m}}, [{1297, 320}]},

		{{r6p, {r6, p}}, [{260, 620}]},
		{{r6o, {r6, o}}, [{260, 320}]}],
	
	lists:foreach(fun({{LightName, {Road, Junction}}, Coords}) ->
		traffic_light:start(LightName, {{Road, Junction}, Coords})
	end, TrafficLights).



%% Detects the closest junction to a given coordinate
detect_junction('$end_of_table', _) -> null;
detect_junction(Key, {X, Y}) ->
    case ets:lookup(junction, Key) of
        [{{_,J},[{XP,YP},_]}] ->
            D = math:sqrt(math:pow(X - (XP - 10), 2) + math:pow(Y - (YP - 10), 2)),
            if
                D =< 200 -> J;
                true -> detect_junction(ets:next(junction, Key), {X, Y})
            end;
        _ ->
            null
    end.

%% Determines which direction a car should take based on current location
light(Streets,  Who, {R,J}) ->    
    PossibleDirections = maps:get({R,J}, Streets),
    {Road, Dir} = select_random(PossibleDirections), 
    io:format("Roas ~p, Dir: ~p", [Road, Dir]),
     cars:turn(Who, {Dir, Road}).

%% Picks a random element from a list
select_random(List) ->
  lists:nth(rand:uniform(length(List)), List).
%% Defines the neighboring streets for given junctions
neighbor_streets()-> #{
      {r1,a} => [{r1,left},{r3,up}],
      {r1,b} => [{r1,left},{r4,up}],
      {r2,c} => [{r2,left},{r3,up}],
      {r2,d} => [{r2,left},{r4,up}],
      {r3,e} => [{r2,left},{r3,up}],
      {r4,f} => [{r1,left},{r4,up}],
      {r3,g} => [{r1,left},{r3,up}],
      {r4,h} => [{r2,left},{r4,up}],

      {r5,n} => [{r2,left},{r5,up}],
      {r5,m} => [{r1,left},{r5,up}],
      {r1,i} => [{r1,left},{r5,up}],
      {r1,k} => [{r1,left},{r6,up}],
      {r6,p} => [{r2,left},{r6,up}],
      {r6,o} => [{r1,left},{r6,up}],
      {r2,j} => [{r2,left},{r5,up}],
      {r2,e} => [{r2,left},{r6,up}]
  }.
%% Connects to the given nodes 
connect_nodes(Node1, Node2, Node3, Node4) ->
    net_kernel:connect_node(Node1),
    net_kernel:connect_node(Node2),
    net_kernel:connect_node(Node3),
    net_kernel:connect_node(Node4).
