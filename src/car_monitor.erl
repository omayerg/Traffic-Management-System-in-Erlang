-module(car_monitor).

-export([update_car_locations/4]).

%% Main function to update car locations. It waits for messages and reacts accordingly.
update_car_locations(NODE1, NODE2, NODE3, NODE4) ->
    receive
        %% If a node is down, adjust the configuration accordingly
        {nodedown, PC} ->
            io:format("Node down detected: ~p~n", [PC]),
            {NewNode1, NewNode2, NewNode3, NewNode4} = node_configuration(NODE1, NODE2, NODE3, NODE4, PC),
            update_car_locations(NewNode1, NewNode2, NewNode3, NewNode4);
        %% Add a process to be monitored
        {add_to_monitor, Pid} ->
            io:format("Adding Pid: ~p to monitor~n", [Pid]),
            monitor(process, Pid),
            update_car_locations(NODE1, NODE2, NODE3, NODE4);
        %% Handle reasons for car movement or status updates
        {_, _, _, _, Reason} ->
            io:format("Received message with reason: ~p~n", [Reason]),
            case Reason of
                {outOfRange, Name, _, Start, Speed} ->
                    [{X, Y}, _, _, _] = Start,
                    Node = start_node(X, Y, NODE1, NODE2, NODE3, NODE4),
                    rpc:call(Node, server, start_car, [Name, Speed, Start, Node]),
                    update_car_locations(NODE1, NODE2, NODE3, NODE4);
                {accident, Name, _, Start, Speed} ->
                    % Similar logic for an accident as outOfRange
                    [{X, Y}, _, _, _] = Start,
                    Node = start_node(X, Y, NODE1, NODE2, NODE3, NODE4),
                    rpc:call(Node, server, start_car, [Name, Speed, Start, Node]),
                    update_car_locations(NODE1, NODE2, NODE3, NODE4);
                {MoveTo, Name, Start, Speed, C, _, _, The_Prev_State} when MoveTo == move_to_comp1; MoveTo == move_to_comp2; MoveTo == move_to_comp3; MoveTo == move_to_comp4 ->
                    Node = case MoveTo of
                        move_to_comp1 -> NODE1;
                        move_to_comp2 -> NODE2;
                        move_to_comp3 -> NODE3;
                        move_to_comp4 -> NODE4
                    end,
                    rpc:call(Node, server, moved_car, [Name, Speed, Start, C, The_Prev_State, Node]),
                    update_car_locations(NODE1, NODE2, NODE3, NODE4);
                killed -> update_car_locations(NODE1, NODE2, NODE3, NODE4);
                normal -> update_car_locations(NODE1, NODE2, NODE3, NODE4);
                _ -> update_car_locations(NODE1, NODE2, NODE3, NODE4)
            end
    after 0 -> update_car_locations(NODE1, NODE2, NODE3, NODE4)
    end.


% Helper function to determine new node configurations when a node is down
node_configuration(NODE1, NODE2, NODE3, NODE4, DownNode) ->
    io:format("Reconfiguring nodes due to node down: ~p~n", [DownNode]),
    case DownNode of
        node_1 when NODE3 == NODE1, NODE4 == NODE1 -> {NODE2, NODE2, NODE2, NODE2};
        node_1 when NODE3 == NODE1 -> {NODE2, NODE2, NODE2, NODE4};
        node_1 when NODE4 == NODE1 -> {NODE2, NODE2, NODE3, NODE2};
        node_1 -> {NODE2, NODE2, NODE3, NODE4};

        node_2 when NODE1 == NODE2, NODE4 == NODE2 -> {NODE3, NODE3, NODE3, NODE3};
        node_2 when NODE1 == NODE2 -> {NODE3, NODE3, NODE3, NODE4};
        node_2 when NODE4 == NODE2 -> {NODE1, NODE3, NODE3, NODE3};
        node_2 -> {NODE1, NODE3, NODE3, NODE4};

        node_3 when NODE1 == NODE3, NODE2 == NODE3 -> {NODE4, NODE4, NODE4, NODE4};
        node_3 when NODE1 == NODE3 -> {NODE4, NODE2, NODE4, NODE4};
        node_3 when NODE2 == NODE3 -> {NODE1, NODE4, NODE4, NODE4};
        node_3 -> {NODE1, NODE2, NODE4, NODE4};

        node_4 when NODE2 == NODE4, NODE3 == NODE4 -> {NODE1, NODE1, NODE1, NODE1};
        node_4 when NODE2 == NODE4 -> {NODE1, NODE1, NODE3, NODE1};
        node_4 when NODE3 == NODE4 -> {NODE1, NODE2, NODE1, NODE1};
        node_4 -> {NODE1, NODE2, NODE3, NODE1}
    end.

% Helper function to determine the node for starting a car based on its position
start_node(X, Y, NODE1, NODE2, NODE3, NODE4) ->
    io:format("Determining start node for X: ~p, Y: ~p~n", [X, Y]),
    case {X, Y} of
        {X, Y} when X >= 800, Y =< 450 -> NODE1;
        {X, Y} when X >= 800, Y >= 450 -> NODE4;
        {X, Y} when X =< 800, Y =< 450 -> NODE2;
        {X, Y} when X =< 800, Y >= 450 -> NODE3;
        _ -> error
    end.