
-module(car_a_s).


%% API
-export([autonomous_system/2,
  autonomous_system_2/2,
  can_continue/2]).

autonomous_system(Pid, '$end_of_table') ->
    %% When end of table is reached, start again from the first car entry.
    autonomous_system(Pid, ets:first(cars));
autonomous_system(Pid, CarEts) when Pid /= CarEts ->  % ensure Pid and CarEts are different right at the start
    %% Retrieve car details from the ETS table using Pid.
    [{_, [{CarX, CarY}, CarDirection, _, _], _, _, _, _, _}] = ets:lookup(cars, Pid),
    %% Retrieve other car's details from the ETS table using CarEts.
    [{OtherCarPid, [{OtherCarX, OtherCarY}, _, _, _], _, _, _, _, _}] = ets:lookup(cars, CarEts),
    
    %% Calculate distance between the two cars.
    Distance = abs(CarX - OtherCarX) + abs(CarY - OtherCarY),
    case Distance<125 of
        true->
            case Distance<45 of
            true->   
                 io:format("Debug: Inside Distance <= 45 and Pid not equal to OtherCarPid~n", []),
                cars:accident(Pid, OtherCarPid),
                server:accident(Pid, {CarX, CarY}, OtherCarPid, {OtherCarX, OtherCarY}),
                timer:sleep(3500);
            _->
                io:format("Distance of Pid ~p: ~p , CarDirection ~p  ~n", [Pid,Distance,CarDirection]),
                case CarDirection of
                    left-> 
                        case {CarX-OtherCarX =< 125, CarX-OtherCarX >= -10} of
                            {true,true}->
                                cars:close_to_car(Pid, OtherCarPid),
                                io:format(" should stop left:-- ~p<125 ~n", [Pid]),
                                timer:sleep(2500);
                            {_,_}->ok
                        end;
                    right->
                        case { OtherCarX-CarX =< 125, OtherCarX-CarX >= -10} of
                            {true,true}->
                                cars:close_to_car(Pid, OtherCarPid),
                                cars:stop(Pid, OtherCarPid),
                                io:format(" should stop right: ~p<100 ~n", [Pid]),
                                timer:sleep(2500);
                            {_,_}->ok
                        end;
                    up->
                        case {CarY-OtherCarY =< 125, CarY-OtherCarY >= -10} of
                            {true,true}->
                                cars:close_to_car(Pid, OtherCarPid),
                                cars:stop(Pid, OtherCarPid),
                                io:format(" should stop up: ~p<100 ~n", [Pid]),
                                timer:sleep(2500);
                            {_,_}->ok
                        end;
                    down->
                        case {OtherCarY-CarY =< 125, OtherCarY-CarY >= -10} of
                            {true,true}->
                                cars:close_to_car(Pid, OtherCarPid),
                                cars:stop(Pid, OtherCarPid),
                                io:format(" should stop down: ~p<100 ~n", [Pid]),
                                timer:sleep(2500);
                            {_,_}->ok
                        end 
                    end % Close the case for CarDirection
            end;% Close the case for Distance<45
        _ ->ok
    end, % Close the case for Distance<125
 
 %% Recursion: Continue processing next car in ETS tables.
    case ets:member(cars, OtherCarPid) of 
        true ->  
            autonomous_system(Pid, ets:next(cars, OtherCarPid));
        _ -> 
             %% If the OtherCarPid does not exist in the cars table, start from the first car entry.
            autonomous_system(Pid, ets:first(cars))
    end;
autonomous_system(Pid, _) ->
    %% If Pid and CarEts are the same (or any other unexpected scenario)
    autonomous_system(Pid, ets:first(cars)).




%% Updates car's position and checks if there's a need to switch components.
autonomous_system_2(Pid,'$end_of_table') -> autonomous_system_2(Pid,ets:first(junction));
autonomous_system_2(Pid,JunctionEts) ->
    %% Retrieve details of a car from ETS table
    [{_, [{CarX, CarY}, CarDirection, RouteId, Speed], _, _, _, _, _}] = ets:lookup(cars, Pid),
    CarRouteId=RouteId,
    [{{JunctionRouteId,_},[{JunctionX,JunctionY},LightPid]}] = ets:lookup(junction, JunctionEts),
    Distance_2 = abs(CarX-JunctionX)+abs(CarY-JunctionY),
    DistanceX = CarX - 800,
    DistanceY = CarY - 450,

    %% Update the position based on conditions
    case {CarX, CarY, CarDirection, DistanceX, DistanceY} of
        {_, _, left, DistanceX, _} when CarX >= 800, CarY =< 450, DistanceX =< 1 ->
            ets:update_element(cars, Pid, [{2, [{CarX - 2, CarY}, CarDirection, RouteId, Speed]}]),
            cars:switch_comp(Pid, node_1, node_2);

        {_, _, down, _, DistanceY} when CarX >= 800, CarY =< 450, DistanceY >= -1 ->
            ets:update_element(cars, Pid, [{2, [{CarX, CarY + 2}, CarDirection, RouteId, Speed]}]),
            cars:switch_comp(Pid, node_1, node_4);

        {_, _, right, DistanceX, _} when CarX =< 800, CarY =< 450, DistanceX >= -1 ->
            ets:update_element(cars, Pid, [{2, [{CarX + 2, CarY}, CarDirection, RouteId, Speed]}]),
            cars:switch_comp(Pid, node_2, node_1);

        {_, _, down, _, DistanceY} when CarX =< 800, CarY =< 450, DistanceY >= -1 ->
            ets:update_element(cars, Pid, [{2, [{CarX, CarY + 2}, CarDirection, RouteId, Speed]}]),
            cars:switch_comp(Pid, node_2, node_3);

        {_, _, up, _, DistanceY} when CarX =< 800, CarY >= 450, DistanceY =< 1 ->
            ets:update_element(cars, Pid, [{2, [{CarX, CarY - 2}, CarDirection, RouteId, Speed]}]),
            cars:switch_comp(Pid, node_3, node_2);

        {_, _, left, DistanceX, _} when CarX >= 800, CarY >= 450, DistanceX =< 1 ->
            ets:update_element(cars, Pid, [{2, [{CarX - 2, CarY}, CarDirection, RouteId, Speed]}]),
            cars:switch_comp(Pid, node_4, node_3);

        {_, _, up, _, DistanceY} when CarX >= 800, CarY >= 450, DistanceY =< 1 ->
            ets:update_element(cars, Pid, [{2, [{CarX, CarY - 2}, CarDirection, RouteId, Speed]}]),
            cars:switch_comp(Pid, node_4, node_1);

        _ when CarX < 0; CarY < 0; CarX > 1600; CarY > 950 -> 
            cars:kill(Pid);

        _ ->
            ok
    end,
 
        %% Check conditions if car is close to junction based on direction
    case CarRouteId of
        JunctionRouteId when Distance_2 =< 90 ->
            case CarDirection of
                left when CarX-JunctionX =< 90, CarX-JunctionX >= 0 -> 
                    cars:close_to_junc(Pid, sys:get_state(LightPid), JunctionEts, LightPid),
                    timer:sleep(3500);
                right when JunctionX-CarX =< 90 , JunctionX-CarX >= 0-> 
                    cars:close_to_junc(Pid, sys:get_state(LightPid), JunctionEts, LightPid),
                    timer:sleep(3500);
                up when CarY-JunctionY =< 90, CarY-JunctionY >= 0 -> 
                    cars:close_to_junc(Pid, sys:get_state(LightPid), JunctionEts, LightPid),
                    timer:sleep(3500);
                down when JunctionY-CarY =< 90, JunctionY-CarY -> 
                    cars:close_to_junc(Pid, sys:get_state(LightPid), JunctionEts, LightPid),
                    timer:sleep(3500);
                _ -> ok
            end;
        _ -> ok
    end,
    %% Recursion
    autonomous_system_2(Pid, ets:next(junction, JunctionEts)).



%% Checks if the specified car is far from another car.
can_continue(CarPid, OtherCarPid) ->
    %% Retrieve details of the first car from ETS table
    [{_, [{CarX, CarY}, CarDirection, _, _], _, _, _, _, _}] = ets:lookup(cars, CarPid),
    
    %% Check if the other car exists in the ETS table
    case ets:member(cars, OtherCarPid) of
        true ->
            %% Retrieve details of the other car from ETS table
            [{_, [{OtherCarX, OtherCarY}, _, _, _], _, _, _, _, _}] = ets:lookup(cars, OtherCarPid),
            
            %% Check if the car is far based on its direction
            case CarDirection of
                left when CarX - OtherCarX >= 150 -> 
                    cars:can_continue(CarPid);
                right when OtherCarX - CarX >= 150 -> 
                    cars:can_continue(CarPid);
                up when CarY - OtherCarY >= 150 -> 
                    cars:can_continue(CarPid);
                down when OtherCarY - CarY >= 150 -> 
                    cars:can_continue(CarPid);
                _ -> 
                    can_continue(CarPid, OtherCarPid)
            end;

        false -> 
            cars:can_continue(CarPid)
    end.




