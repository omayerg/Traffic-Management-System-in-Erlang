
-module(cars).
-behaviour(gen_statem).

%% API
-export([start_link/0,start/5,start/7]).

%% gen_statem callbacks
-export([
  init/1,
  format_status/2,
  state_name/3,
  handle_event/4,
  terminate/3,
  callback_mode/0
]).

%%Events
-export([close_to_car/2,close_to_junc/4
  ,accident/2,turn/2,can_continue/1]).
-export([stop/2,kill/1,switch_comp/3]).

%% States
-export([move_forward/3,be_carefuul/3,turn/3,
  wait_for_safe/3,intial_the_car/3]).

-define(SERVER, ?MODULE).

-record(cars_state, {nextTurnDir  ,nextTurnRoad, 
  speed,lightPid,sensor,sensor2,monitor}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).
start(Name,CarMonitor,Speed,Start,PC) -> % intialize
  gen_statem:start({local, Name}, ?MODULE, [Name,CarMonitor,Start,Speed,PC], []).
start(Name,CarMonitor,Speed,Start,Location,The_Prev_State,PC) ->  % intialize again when move computer 
  gen_statem:start({local, Name}, ?MODULE, [Name,CarMonitor,Start,Speed,Location,The_Prev_State,PC], []).


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init([Name,CarMonitor,Start,Speed,PC]) -> % initialize car when starting the program
 % put Name of car, car monitor, initial location and speed in process dictionary
  store_values(Name, CarMonitor, Start, Speed),
  CarData = {self(),Start,Name,Start,Speed,nal,PC},
 % io:format("Inserting CarData: ~p~n", [CarData]),
  ets:insert(cars, CarData), % insert the car to the cars ets
  CarMonitor! {add_to_monitor,self()}, % add the car to the monitor
  {ok,intial_the_car, #cars_state{speed = Speed,monitor = CarMonitor},20}; % send to first state

init([Name, CarMonitor, Start, Speed, Location, The_Prev_State, PC]) ->
    store_values(Name, CarMonitor, Start, Speed),
    ets:insert(cars, {self(), Location, Name, Start, Speed, The_Prev_State, PC}),
    CarMonitor ! {add_to_monitor, self()},

    spawn_sensors(CarMonitor, self()),

    case The_Prev_State of
        {move_forward} -> {ok, move_forward, #cars_state{}, Speed};
        {be_carefuul} -> {ok, be_carefuul, #cars_state{}, Speed};
        {turn, Dir, Road} -> {ok, turn, #cars_state{nextTurnDir = Dir, nextTurnRoad = Road}, Speed}
    end.



%%--------------------------------------------------------------------
%% @spec callback_mode() -> atom().
%%--------------------------------------------------------------------
callback_mode() ->
    state_functions.

%%% Event Functions

%% Event: Car is close to another car
close_to_car(Pid, OtherCar) -> 
    io:format("Car ~p is CLOSE to car ~p ~n", [Pid, OtherCar]),
    gen_statem:cast(Pid, {close_car, Pid, OtherCar}).

%% Event: Car is near a junction
close_to_junc(Pid, LState, {CarRouteId, Junction}, LP) -> 
    io:format("Car ~p is NEAR junction with state ~p, {CarRouteId, Junction} = {~p, ~p} and LP = ~p ~n", [Pid, LState, CarRouteId, Junction, LP]),
    gen_statem:cast(Pid, {close_junction, Pid, LState, {CarRouteId, Junction}, LP}).

%% Event: An accident occurred between two cars
accident(Pid, OtherCar) ->   
    io:format("ACCIDENT between ~p and ~p ~n", [Pid, OtherCar]),
    gen_statem:cast(Pid, {accident_event, Pid, OtherCar}).

%% Event: Car is preparing to turn
turn(Pid, {Dir, Road}) -> 
    io:format("Car ~p is TURNING in direction ~p on road ~p ~n", [Pid, Dir, Road]),
    gen_statem:cast(Pid, {turn, Pid, {Dir, Road}}).

%% Event: Car has distanced itself from another car
can_continue(Pid) -> 
    io:format("Car ~p has DISTANCED itself from another car ~n", [Pid]),
    gen_statem:cast(Pid, {far_away, Pid}).

%% Event: Car needs to come to a halt
stop(Pid, OtherCar) -> 
    io:format("Car ~p needs to STOP because of car ~p ~n", [Pid, OtherCar]),
    gen_statem:cast(Pid, {stop, Pid, OtherCar}).

%% Event: Terminate car's operation
kill(Pid) ->  
    io:format("TERMINATING operation of car ~p ~n", [Pid]),
    gen_statem:cast(Pid, {kill, Pid}).

%% Event: Car transitioned to a different processing unit
switch_comp(Pid, From, To) -> 
    io:format("Car ~p SWITCHED from ~p to ~p ~n", [Pid, From, To]),
    gen_statem:cast(Pid, {switch, Pid, From, To}).


%%--------------------------------------------------------------------
%% @spec format_status(Opt, [PDict, StateName, State]) -> term()
%%--------------------------------------------------------------------
format_status(_Opt, [_PDict, _StateName, _State]) ->
    Status = some_term,
    Status.

%%--------------------------------------------------------------------
%% State Transitions for the Car FSM (Finite State Machine)
%%--------------------------------------------------------------------
state_name(_EventType, _EventContent, State) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.

% Initializes the car by spawning its car_a_s.
intial_the_car(timeout, 20, State = #cars_state{}) ->
   % Get monitor from current state
   Monitor = State#cars_state.monitor,

   % Spawn car car_a_s
   spawn_sensors(Monitor, self()),

   % Define next state
   NextStateName = move_forward,

   % Transition to next state and get car's speed
   {next_state, NextStateName, State, get(speed)}.

%-----------------------------move_forward--------------------------------------%


% When car gets close to another car, this function handles the event.
move_forward(cast,{close_car,Pid,OtherCar},State = #cars_state{}) ->
   % Notify the server about the detected car
   server:car_detected(Pid,OtherCar),

   % Update car's state in ets to 'be_careful'
   ets:update_element(cars,self(),[{6,{be_carefuul}}]),

   % Define the next state
   NextStateName = be_carefuul,

   % Transition to the 'be_careful' state and get car's speed
   {next_state, NextStateName, State,get(speed)};

% Handles the event when the car approaches a junction. The car will either keep going or stop based on the traffic light.
move_forward(cast,{close_junction,Pid,T,{CarRouteId,Junction},LP},_) -> 

  % Check the state of the traffic light
  case T of
    green -> 
      % If green, transition to 'be_careful' state and notify the server about the junction
      NextStateName = be_carefuul,
      ets:update_element(cars,self(),[{6,{be_carefuul}}]),
      server:on_traffic_light(Pid,{CarRouteId,Junction}),
      {next_state, NextStateName, #cars_state{lightPid = LP},get(speed)};

    _ -> 
      % For any other light state, transition to 'wait_for_safe', update state to 'move_forward' and notify the server
      NextStateName = wait_for_safe,
      ets:update_element(cars,self(),[{6,{move_forward}}]),
      server:on_traffic_light(Pid,{CarRouteId,Junction}),
      {next_state, NextStateName, #cars_state{lightPid = LP}}
  end;

% Handles the event when the car gets into an accident.
move_forward(cast,{accident_event,Pid,_},_) -> 
  % Terminate the car's car_a_s
  kill_and_delete_sensors(),
  
  % Retrieve necessary car data
  {MyName, MyCarMon, MyStart, MySpeed} = retrieve_values(),

  % Pause for 5 seconds
  timer:sleep(5000),
  
  % Notify server to update its state about the car and the accident
  server:remove_car_from_map(Pid),

  % Stop the car and send its data to the monitor
  {stop,{accident,MyName,MyCarMon,MyStart,MySpeed}};



% Handles the move forward action upon a timeout. The car will move straight with a speed of 20.
move_forward(timeout,20,State = #cars_state{}) ->

   % Retrieve car's current state from the ets table
   [{Pid,[{CarX,CarY},CarDirection,CarRouteId,Speed],_,_,_,_,_}] = ets:lookup(cars,self()),

   % Update car's position in the ets table based on its current direction
   if
      CarDirection == up -> ets:update_element(cars,Pid,[{2,[{CarX,CarY -1 },CarDirection,CarRouteId,Speed]}]); % Move up
      CarDirection == down -> ets:update_element(cars,Pid,[{2,[{CarX,CarY +1 },CarDirection,CarRouteId,Speed]}]); % Move down
      CarDirection == right -> ets:update_element(cars,Pid,[{2,[{CarX + 1,CarY },CarDirection,CarRouteId,Speed]}]); % Move right
      true -> ets:update_element(cars,Pid,[{2,[{CarX - 1,CarY},CarDirection,CarRouteId,Speed]}]) % Default: Move left
   end,

   % Update car's state to move_forward in the ets table
   NextStateName = move_forward,
   ets:update_element(cars,self(),[{6,{move_forward}}]),

   % Transition to the move_forward state with a speed of 20
   {next_state, NextStateName, State,20};


% Responds to a stop message, transitioning the car to a safe waiting state.
move_forward(cast,{stop,_},State = #cars_state{}) ->
   % Set next state to 'wait_for_safe' and update the ets table accordingly
   NextStateName = wait_for_safe,
   ets:update_element(cars,self(),[{6,{move_forward}}]),

   % Transition to 'wait_for_safe' state
   {next_state, NextStateName, State};


% Handles the scenario when a car is instructed to switch to a different processing component (PC).
move_forward(cast,{switch,Pid,From,To},_) -> 

   % Terminate the car's car_a_s and delete their references
   kill_and_delete_sensors(),

   % Retrieve essential car attributes
   {MyName, _MyCarMon, MyStart, MySpeed} = retrieve_values(),
   [{_,C,_,_,_,_,_}] = ets:lookup(cars,Pid),
   The_Prev_State =  {move_forward},

   % Depending on the target PC, send a message to the server and define the next action
   case To of
      node_1 -> 
         server:remove_car_from_map(Pid), 
         {stop,{move_to_comp1,MyName,MyStart,MySpeed,C,From,To,The_Prev_State}};
      node_2 -> 
         server:remove_car_from_map(Pid), 
         {stop,{move_to_comp2,MyName,MyStart,MySpeed,C,From,To,The_Prev_State}};
      node_3 -> 
         server:remove_car_from_map(Pid), 
         {stop,{move_to_comp3,MyName,MyStart,MySpeed,C,From,To,The_Prev_State}};
      node_4 -> 
         server:remove_car_from_map(Pid), 
         {stop,{move_to_comp4,MyName,MyStart,MySpeed,C,From,To,The_Prev_State}}
   end;

% Responds to a kill message, terminating the car and its car_a_s.
move_forward(cast,{kill,Pid},_) -> 

   % Terminate the car's car_a_s
   kill_and_delete_sensors(),

   % Retrieve essential car attributes
   {MyName, MyCarMon, MyStart, MySpeed} = retrieve_values(),

   % Notify the server to update its state about the car
   server:remove_car_from_map(Pid),

   % Stop the car and send its details to the monitor
   {stop,{outOfRange,MyName,MyCarMon,MyStart,MySpeed}};


% Handles unwanted or unexpected messages, maintaining the car's movement forward.
move_forward(cast,_,State = #cars_state{}) -> 

   % Set next state to 'move_forward' and update the ets table accordingly
   NextStateName = move_forward,
   ets:update_element(cars,self(),[{6,{move_forward}}]),

   % Transition to 'move_forward' state and retrieve the car's speed
   {next_state, NextStateName, State,get(speed)}.

%-----------------------------be_carefuul--------------------------------------%

% Handles the scenario when the car, in a cautious state, gets into an accident.
be_carefuul(cast,{accident_event,Pid,_},_) -> 

   % Terminate the car's car_a_s
   kill_and_delete_sensors(),

   % Retrieve essential car attributes
   {MyName, MyCarMon, MyStart, MySpeed} = retrieve_values(),

   % Pause the operation for 5 seconds
   timer:sleep(5000),

   % Notify the server to update its state about the car and the accident
   server:remove_car_from_map(Pid),

   % Stop the car's operation and send its details to the monitor
   {stop,{accident,MyName,MyCarMon,MyStart,MySpeed}};


% Handles the cautious movement action upon a timeout. The car will move in its current direction at a speed of 20.
be_carefuul(timeout,20,State = #cars_state{}) ->

   % Retrieve car's current state from the ets table
   [{Pid,[{CarX,CarY},CarDirection,CarRouteId,Speed],_,_,_,_,_,_}] = ets:lookup(cars,self()),

   % Update car's position in the ets table based on its current direction
   if
      CarDirection == up -> ets:update_element(cars,Pid,[{2,[{CarX,CarY -1 },CarDirection,CarRouteId,Speed]}]); % Move up
      CarDirection == down -> ets:update_element(cars,Pid,[{2,[{CarX,CarY +1 },CarDirection,CarRouteId,Speed]}]); % Move down
      CarDirection == right -> ets:update_element(cars,Pid,[{2,[{CarX + 1,CarY },CarDirection,CarRouteId,Speed]}]); % Move right
      true -> ets:update_element(cars,Pid,[{2,[{CarX - 1,CarY},CarDirection,CarRouteId,Speed]}]) % Default: Move left
   end,

   % Update car's state to 'be_careful' in the ets table
   NextStateName = be_carefuul,
   ets:update_element(cars,self(),[{6,{be_carefuul}}]),

   % Transition to the 'be_careful' state with a speed of 20
   {next_state, NextStateName, State,20};


% Handles the scenario when the car, in a cautious state, receives a turn instruction.
be_carefuul(cast,{turn,_,{Dir, Road}},State = #cars_state{}) -> 

   % Retrieve car's current state from the ets table
   [{_,[{_,_},CarDirection,_,_,_],_,_,_,_,_,_}] = ets:lookup(cars,self()),

   % Determine if the car should continue straight or initiate a turn
   case CarDirection == Dir of
      true ->
         % If the car is already heading in the direction of the turn instruction, continue straight
         NextStateName1 = move_forward,
         {next_state, NextStateName1, State,get(speed)};
      _ ->
         % If the turn direction differs from the car's current direction, initiate a turn
         NextStateName = turn,
         ets:update_element(cars,self(),[{6,{turn,Dir,Road}}]),
         {next_state, NextStateName, #cars_state{nextTurnDir = Dir,nextTurnRoad = Road},get(speed)}
   end;


% Responds to a stop instruction when close proximity to another car is detected.
be_carefuul(cast,{stop,_,OtherCar},State = #cars_state{}) ->

   % Spawn a sensor to check the distance to the other car
   SensorPid = spawn(car_a_s,can_continue,[self(),OtherCar]), 

   % Register the spawned sensor in the ets table
   ets:insert(car_a_s,{SensorPid,self()}),

   % Retrieve the car's monitor
   Monitor = get(carMon),

   % Notify the car's monitor about the new sensor
   Monitor ! {add_to_monitor,SensorPid}, 

   % Transition the car to the 'wait_for_safe' state
   NextStateName = wait_for_safe, 
   ets:update_element(cars,self(),[{6,{move_forward}}]),
   {next_state, NextStateName, State};


% This function handles the scenario when a car in the 'be_careful' state is commanded to switch to a different PC (Processing Component).
be_carefuul(cast,{switch,Pid,From,To},_) -> 

   % Kills the car_a_s associated with the car. This is typically done before a transition, 
   % to ensure the car doesn't process outdated or irrelevant sensor data after switching PCs.
   kill_and_delete_sensors(),

   % Retrieve the car's details.
  {MyName,_MyCarMon, MyStart, MySpeed} = retrieve_values(),

   
   % Get the car's current state from the ets table.
   [{_,C,_,_,_,_,_}] = ets:lookup(cars,Pid),
   The_Prev_State =  {be_carefuul},

   % Depending on the target PC (`To`), the car will be removed from its current map 
   % and be scheduled to be moved to the respective PC with its associated details.
   case To of
      node_1 -> 
         server:remove_car_from_map(Pid), 
         {stop,{move_to_comp1,MyName,MyStart,MySpeed,C,From,To,The_Prev_State}};
      node_2 -> 
         server:remove_car_from_map(Pid), 
         {stop,{move_to_comp2,MyName,MyStart,MySpeed,C,From,To,The_Prev_State}};
      node_3 -> 
         server:remove_car_from_map(Pid), 
         {stop,{move_to_comp3,MyName,MyStart,MySpeed,C,From,To,The_Prev_State}};
      node_4 -> 
         server:remove_car_from_map(Pid), 
         {stop,{move_to_comp4,MyName,MyStart,MySpeed,C,From,To,The_Prev_State}}
   end;



% This function handles the scenario when a car in the 'be_careful' state is commanded to be killed.
be_carefuul(cast,{kill,Pid},_) -> 

   % Kills the car_a_s associated with the car. 
   % Ensures the car doesn't process any additional sensor data before it's killed.
   kill_and_delete_sensors(),

   % Retrieve the car's details.
   {MyName, MyCarMon, MyStart, MySpeed} = retrieve_values(),

   % Remove the car from the server map.
   server:remove_car_from_map(Pid),

   % Stop the car and send its details to the monitor.
   {stop,{outOfRange,MyName,MyCarMon,MyStart,MySpeed}};

% If a car in the 'be_careful' state receives a message that it's close to another car, it simply continues being careful.
be_carefuul(cast,{close_car,_,_},State = #cars_state{}) -> 

   % No change in the state since the car is already being careful.
   NextStateName = be_carefuul,
   ets:update_element(cars,self(),[{6,{be_carefuul}}]),
   {next_state, NextStateName, State,get(speed)};

% For any other unexpected or unwanted messages received in 'be_careful' state, log an error.
be_carefuul(cast,Else,State = #cars_state{}) -> 

   % Print the unexpected message to the console.
   io:format("error in be_carefuul: ~p~n",[Else]),

   % Continue being in the 'be_careful' state.
   NextStateName = be_carefuul,
   ets:update_element(cars,self(),[{6,{be_carefuul}}]),
   {next_state, NextStateName, State,get(speed)}.

%-----------------------------turn--------------------------------------%

% This function handles the logic when a car is in the process of turning or needs to decide to turn.
turn(timeout,20,State = #cars_state{}) ->

   % Get car's details.
   [{Pid,[{CarX,CarY},CarDirection,CarRouteId,Speed],_,_,_,_,_}] = ets:lookup(cars,self()), 

   % Retrieve the next direction and road from the current state.
   Dir = State#cars_state.nextTurnDir,
   Road = State#cars_state.nextTurnRoad,

   % Compute the target CarX and CarY coordinates based on the road's center.
   {X_target, Y_target} = road_center(Road),

   % Determine if the car has reached the target CarX or CarY based on its current and desired direction.
   In_Dest = case {CarDirection, Dir} of
        {up, _} -> CarY == Y_target;
        {down, _} -> CarY == Y_target;
        {right, _} -> CarX == X_target;
        {left, _} -> CarX == X_target;
        _ -> false
   end,

   % If the car reaches the destination, update its direction and state.
   if
      In_Dest ->
         ets:update_element(cars,Pid,[{2,[{CarX ,CarY },Dir,Road,Speed]}]), % Update car's current direction.
         NextStateName = move_forward,  % Transition to move_forward state.
         ets:update_element(cars,self(),[{6,{move_forward}}]),
         {next_state, NextStateName, #cars_state{nextTurnDir = Dir , nextTurnRoad = Road },20};
      true ->  
         % Update car's position based on the current and desired direction.
          case {CarDirection, Dir} of
            {up, left} -> ets:update_element(cars,Pid,[{2,[{CarX,CarY - 1 },CarDirection,CarRouteId,Speed]}]);
            {up, right} -> ets:update_element(cars,Pid,[{2,[{CarX,CarY - 1 },CarDirection,CarRouteId,Speed]}]);
            {down, left} -> ets:update_element(cars,Pid,[{2,[{CarX,CarY + 1 },CarDirection,CarRouteId,Speed]}]);
            {down, right} -> ets:update_element(cars,Pid,[{2,[{CarX,CarY + 1 },CarDirection,CarRouteId,Speed]}]);
            {right, up} -> ets:update_element(cars,Pid,[{2,[{CarX + 1,CarY },CarDirection,CarRouteId,Speed]}]);
            {right, down} -> ets:update_element(cars,Pid,[{2,[{CarX + 1,CarY },CarDirection,CarRouteId,Speed]}]);
            {left, up} -> ets:update_element(cars,Pid,[{2,[{CarX - 1,CarY },CarDirection,CarRouteId,Speed]}]);
            {left, down} -> ets:update_element(cars,Pid,[{2,[{CarX - 1,CarY },CarDirection,CarRouteId,Speed]}])
          end,
         NextStateName = turn,  % Stay in turn state.
         ets:update_element(cars,self(),[{6,{turn,0,State#cars_state.nextTurnDir,State#cars_state.nextTurnRoad}}]),
         {next_state, NextStateName, #cars_state{nextTurnDir = Dir , nextTurnRoad = Road },20}
   end;

% If the car is close to a junction in the 'turn' state, it continues to be in the 'turn' state.
turn(cast,{close_junction,_,_,_,_},State = #cars_state{}) -> 

   Dir = State#cars_state.nextTurnDir, 
   Road = State#cars_state.nextTurnRoad,
   
   % Update car's state to keep turning.
   NextStateName = turn,
   ets:update_element(cars,self(),[{6,{turn,State#cars_state.nextTurnDir,State#cars_state.nextTurnRoad}}]),
   {next_state, NextStateName, #cars_state{nextTurnDir = Dir , nextTurnRoad = Road },get(speed)};


% This function determines the car's behavior when it is close to another car while turning.
turn(cast,{close_car,_,_},State = #cars_state{}) ->

   % Retrieve the next direction and road from the current state.
   Dir = State#cars_state.nextTurnDir, 
   Road = State#cars_state.nextTurnRoad,

   % Stay in the 'turn' state as it's still close to another car.
   NextStateName = turn,
   ets:update_element(cars,self(),[{6,{turn,State#cars_state.nextTurnDir,State#cars_state.nextTurnRoad}}]),
   {next_state, NextStateName, #cars_state{nextTurnDir = Dir , nextTurnRoad = Road },get(speed)};

% This function determines the car's behavior when it gets into an accident while turning.
turn(cast,{accident_event,Pid,_},_) -> 

   % Delete all car_a_s associated with this car.
   kill_and_delete_sensors(),

   % Retrieve car's essential details.
   {MyName, MyCarMon, MyStart, MySpeed} = retrieve_values(),

   % Wait for 3.5 seconds (3500 milliseconds).
   timer:sleep(3500),

   % Remove this car from the map.
   server:remove_car_from_map(Pid),

   % Terminate the car and send its data to the monitor.
   {stop,{accident,MyName,MyCarMon,MyStart,MySpeed}};

% This function handles the behavior when the car is instructed to switch its processing component (PC) while it is turning.
turn(cast,{switch,Pid,From,To},State = #cars_state{}) ->

   % Terminate all car_a_s associated with this car.
   kill_and_delete_sensors(),

   % Capture the car's current state (turning, the next turn direction, and the next turn road) into the variable 'The_Prev_State'.
   The_Prev_State =  {turn,State#cars_state.nextTurnDir,State#cars_state.nextTurnRoad},

   % Retrieve some of the car's properties.
   {MyName, _MyCarMon, MyStart, MySpeed} = retrieve_values(),

   % Lookup the car's data from the 'cars' table using its process ID (Pid).
   [{_,C,_,_,_,_,_}] = ets:lookup(cars,Pid),

   % Based on the target processing component (PC), remove the car from the map and send a 'stop' message with the relevant details.
   % Each case corresponds to a different target PC.
   case To of
     node_1 -> 
      server:remove_car_from_map(Pid),
      {stop,{move_to_comp1,MyName,MyStart,MySpeed,C,From,To,The_Prev_State}};
     node_2 ->
       server:remove_car_from_map(Pid), 
      {stop,{move_to_comp2,MyName,MyStart,MySpeed,C,From,To,The_Prev_State}};
     node_3 -> 
      server:remove_car_from_map(Pid), 
      {stop,{move_to_comp3,MyName,MyStart,MySpeed,C,From,To,The_Prev_State}};
     node_4 -> 
      server:remove_car_from_map(Pid),
      {stop,{move_to_comp4,MyName,MyStart,MySpeed,C,From,To,The_Prev_State}}
   end;

% This function handles the behavior when the car is instructed to terminate while it's in the 'turn' state.
turn(cast,{kill,Pid},_) ->

   % Terminate all car_a_s associated with this car.
   kill_and_delete_sensors(),

   % Retrieve essential properties of the car.
   {MyName, MyCarMon, MyStart, MySpeed} = retrieve_values(),

   % Remove this car from the server's map.
   server:remove_car_from_map(Pid),

   % Send a stop message with the reason being 'outOfRange' and include the car's properties in the message.
   {stop,{outOfRange,MyName,MyCarMon,MyStart,MySpeed}};

% This function handles the default behavior for any unrecognized or unwanted messages that the car might receive while it's turning.
turn(cast,Else,State = #cars_state{}) ->

   % Print the unexpected message to the console.
   io:format("error in be_carefuul: ~p~n",[Else]),
   
   % Get the car's intended direction and road from the state.
   Dir = State#cars_state.nextTurnDir,
   Road = State#cars_state.nextTurnRoad,

   % Update the car's state in the 'cars' ETS table to remain in the 'turn' state.
   NextStateName = turn,
   ets:update_element(cars,self(),[{6,{turn,State#cars_state.nextTurnDir,State#cars_state.nextTurnRoad}}]),

   % Respond that the car should continue in the 'turn' state with its current direction and road. The car's speed is also retrieved.
   {next_state, NextStateName,#cars_state{nextTurnDir = Dir , nextTurnRoad = Road },get(speed)}.

%-----------------------------wait_for_safe--------------------------------------%

% This function is triggered when a 'turn' message is received by a car in the 'wait_for_safe' state.
wait_for_safe(cast,{turn,_,{Dir, Road}},State = #cars_state{}) ->
   % Retrieve the light's PID from the car's state.
   LP = State#cars_state.lightPid,

   % Update the car's state in the 'cars' ETS table to 'move_forward'.
   NextStateName = wait_for_safe, 
   ets:update_element(cars,self(),[{6,{move_forward}}]),

   % The car continues in the 'wait_for_safe' state and updates its direction and road properties.
   {next_state, NextStateName, #cars_state{nextTurnDir = Dir,nextTurnRoad = Road,lightPid = LP},get(speed)};

% This function checks the traffic light state after waiting for a specific timeout.
wait_for_safe(timeout,20,State = #cars_state{}) ->
   LP = State#cars_state.lightPid, 
   Dir = State#cars_state.nextTurnDir, 
   Road = State#cars_state.nextTurnRoad,

   % Check the state of the traffic light using its PID.
   case sys:get_state(LP) of
      {green,_} -> 
         % If the light is green, check the car's direction and decide to either drive straight or turn.
         [{_,[{_,_},CarDirection,_,_],_,_,_,_,_}] = ets:lookup(cars,self()),
         case CarDirection == Dir of
            true -> 
               % Drive straight if the car's current direction matches its intended direction.
               NextStateName1 = move_forward,
               {next_state, NextStateName1, State,get(speed)};
            _ ->  
               % If not, the car should turn.
               NextStateName = turn,
               ets:update_element(cars,self(),[{6,{turn,Dir,Road}}]),
               {next_state, NextStateName, #cars_state{nextTurnDir = Dir,nextTurnRoad = Road},get(speed)}
         end;
      _ -> 
         % If the light isn't green, the car continues to wait in the 'wait_for_safe' state.
         NextStateName = wait_for_safe,
         ets:update_element(cars,self(),[{6,{move_forward}}]),
         {next_state, NextStateName, #cars_state{nextTurnDir = Dir,nextTurnRoad = Road, lightPid = LP},20}
   end;


% This function is triggered when the car is close to a junction while in the 'wait_for_safe' state.
wait_for_safe(cast,{close_junction,_,_,_,LP},State = #cars_state{}) ->

   % Retrieve the car's intended direction and road from its state.
   Dir = State#cars_state.nextTurnDir, 
   Road = State#cars_state.nextTurnRoad,

   % Update the car's state in the 'cars' ETS table to 'move_forward'.
   NextStateName = wait_for_safe, 
   ets:update_element(cars,self(),[{6,{move_forward}}]),

   % The car continues in the 'wait_for_safe' state, updating its direction, road, and light PID properties.
   {next_state, NextStateName, #cars_state{nextTurnDir = Dir, nextTurnRoad = Road, lightPid = LP}, get(speed)};

% This function is triggered when the car receives a 'far_away' message, implying it's far from another car in the 'wait_for_safe' state.
wait_for_safe(cast,{far_away,_},State = #cars_state{}) -> 

   % Update the car's state in the 'cars' ETS table to 'move_forward'.
   NextStateName = move_forward, 
   ets:update_element(cars,self(),[{6,{move_forward}}]),

   % The car continues moving forward with its current state.
   {next_state, NextStateName, State, get(speed)};

% This function handles the situation when the car gets into an accident in the 'wait_for_safe' state.
wait_for_safe(cast,{accident_event,Pid,_},_) -> 

   % Disable all car_a_s associated with the car.
   kill_and_delete_sensors(),

   % Retrieve the car's data such as name, monitor, start position, and speed.
   {MyName, MyCarMon, MyStart, MySpeed} = retrieve_values(),
   timer:sleep(3500),

   % Remove the car from the map a
   server:remove_car_from_map(Pid),

   % Send the accident data to a monitor and stop the car's process.
   {stop,{accident,MyName,MyCarMon,MyStart,MySpeed}};



% This function handles the scenario when a 'kill' message is received by a car in the 'wait_for_safe' state.
wait_for_safe(cast,{kill,Pid},_) -> 

   % Disable all car_a_s associated with the car.
   kill_and_delete_sensors(),

   % Retrieve the car's data such as name, monitor, start position, and speed.
   {MyName, MyCarMon, MyStart, MySpeed} = retrieve_values(),

   % Remove the car from the map.
   server:remove_car_from_map(Pid),

   % Stop the car's process and indicate it's out of range.
   {stop,{outOfRange,MyName,MyCarMon,MyStart,MySpeed}};

% This function handles the default behavior for any unrecognized or unwanted messages that the car might receive while in 'wait_for_safe' state.
wait_for_safe(cast,_,State = #cars_state{}) -> 

   % Retrieve the light's PID, intended direction, and road from the car's state.
   LP = State#cars_state.lightPid, 
   Dir = State#cars_state.nextTurnDir, 
   Road = State#cars_state.nextTurnRoad,

   % Update the car's state in the 'cars' ETS table to 'move_forward'.
   NextStateName = wait_for_safe, 
   ets:update_element(cars,self(),[{6,{move_forward}}]),

   % The car continues in the 'wait_for_safe' state, updating its light PID, direction, and road properties.
   {next_state, NextStateName,#cars_state{lightPid = LP, nextTurnDir = Dir, nextTurnRoad = Road }}.

%%----------------------------end----------------------------------------
%%----------------------------end----------------------------------------
%%----------------------------end----------------------------------------

handle_event(_EventType, _EventContent, _StateName, State) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%%--------------------------------------------------------------------

terminate(_Reason, _StateName, _State) ->
    ok.


%Helper Function---------------------------------------------------------------------

% Spawns car_a_s for a given car and stores their PIDs associated with the car's PID.
spawn_sensors(CarMonitor, Pid) ->
    SensorProcs = [
        {sensor, autonomous_system, [Pid, ets:first(cars)]},
        {sensor2, autonomous_system_2, [Pid, ets:first(junction)]}
    ],
    lists:foreach(fun({Sensor, SensorType, Params}) ->
        SensorPid = spawn(car_a_s, SensorType, Params),
        ets:insert(car_a_s, {SensorPid, Pid}),
        put(Sensor, SensorPid),
        CarMonitor ! {add_to_monitor, Pid}
    end, SensorProcs).

% Terminates the car_a_s associated with a car and removes their entries from the 'car_a_s' ETS table.
kill_and_delete_sensors() ->
    Sensors = [get(sensor),get(sensor2)],
    lists:foreach(fun(Sensor) ->
        exit(Sensor, kill),
        ets:delete(car_a_s, Sensor)
    end, Sensors).

% Retrieves stored values for the car's name, monitor, start position, and speed.
retrieve_values() ->
    MyName = get(name),
    MyCarMon = get(carMon),
    MyStart = get(start),
    MySpeed = get(speed),
    {MyName, MyCarMon, MyStart, MySpeed}.

% Stores values for the car's name, monitor, start position, and speed.
store_values(Name, CarMonitor, Start, Speed) ->
    put(name, Name),
    put(carMon, CarMonitor),
    put(start, Start),
    put(speed, Speed).

% Returns the center coordinates for each specified road.
road_center(Road) ->
    case Road of
        r1 -> {x, 310};
        r2 -> {x, 605};
        r3 -> {935, y};
        r4 -> {612, y};
        r5 -> {1297, y};
        r6 -> {260, y}
    end.
