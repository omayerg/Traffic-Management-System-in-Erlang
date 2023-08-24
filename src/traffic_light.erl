-module(traffic_light).
-behaviour(gen_statem).

%%% API
-export([start_link/0, start/2]).

%%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).

%%% Events
-export([timeout/0]).

%%% States
-export([red/3, green/3, yellow/3]).

-define(SERVER, ?MODULE).

-record(traffic_light_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

% Starts the traffic light process and links it to the calling process.
start_link() ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

% Starts the traffic light process with a given name and parameters.
start(Name,{{R,J},[{X,Y}]}) ->
  gen_statem:start({local,Name}, ?MODULE, {{R,J},[{X,Y}]}, []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================
init({{R,J},[{X,Y}]}) ->
    ets:insert(junction,{{R,J},[{X,Y},self()]}), % insert junction to ets
    case R of
      r5 -> 
          handle_road_green(R);
      r3 -> 
          handle_road_green(R);
      r4 -> 
          handle_road_green(R);
      r6 -> 
          handle_road_green(R);
      r1 -> 
          handle_road_yellow(R);
      r2 -> 
          handle_road_yellow(R);
      _ ->
        Result = case rand:uniform(3) of 
          1 -> {ok, red, #traffic_light_state{}, 6000}; 
          2 -> {ok, green, #traffic_light_state{}, 10000};
          3 -> {ok, yellow, #traffic_light_state{}, 4000}
        end,
        io:format("Road ~p is initialized randomly to ~p~n", [R, element(2, Result)]),
        Result
    end.




%% @private

% Specifies the callback mode.
callback_mode() ->
  state_functions.

%%% Events

% Generates a timeout event.
timeout() -> 
    gen_statem:cast(?MODULE,{time}).

%% @private

% Format status for diagnostics.
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

%%% Traffic Light State Handling

state_name(_EventType, _EventContent, State = #traffic_light_state{}) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.

red(timeout, 6000, State = #traffic_light_state{}) -> % after 2 seconds turn green
  {next_state, green, State, 10000}.

yellow(timeout, 4000, State = #traffic_light_state{}) -> % after 1 second, turn red for 2 seconds
  {next_state, red, State, 6000}.

green(timeout, 10000, State = #traffic_light_state{}) -> % after 3 seconds turn yellow for 1 second
  {next_state, yellow, State, 4000}.

%%% gen_statem callbacks

handle_event(_EventType, _EventContent, _StateName, State = #traffic_light_state{}) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

terminate(_Reason, _StateName, _State = #traffic_light_state{}) ->
  ok.

code_change(_OldVsn, StateName, State = #traffic_light_state{}, _Extra) ->
  {ok, StateName, State}.

%Helper Function---------------------------------------------------------------------
handle_road_green(R) ->
    io:format("Road ~p is initialized to green~n", [R]),
    {ok, green, #traffic_light_state{}, 10000}.

handle_road_yellow(R) ->
    io:format("Road ~p is initialized to yellow~n", [R]),
    {ok, yellow, #traffic_light_state{}, 4000}.
