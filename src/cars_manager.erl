-module(cars_manager).
-include_lib("wx/include/wx.hrl").
-include("header.hrl").

-export([
	move_car/2,
	on_accident/4,
	delete_car/1
	]).


move_car(_, '$end_of_table') -> ok;
move_car(NodeDown, Key) ->
    [{_, Location, Name, Start, Speed, The_Prev_State, Node}] = ets:lookup(cars, Key),
    Next = ets:next(cars, Key),
    NewNode = case Node of
                ?NODE1 -> get(?NODE2);
                ?NODE2 -> get(?NODE3);
                ?NODE3 -> get(?NODE4);
                ?NODE4 -> get(?NODE1)
            end,
    case Node == NodeDown of
        true ->
            rpc:call(NewNode, server, moved_car, [Name, Speed, Start, Location, The_Prev_State, NewNode]),
            ets:delete(cars, Key),
            move_car(NodeDown, Next);
        false -> move_car(NodeDown, Next)
    end.
	


delete_car(Pid) ->
    wx_object:cast(main, {delete_car, Pid}).

on_accident(Car, Car1b, Car2, Car2b) ->
    wx_object:cast(main, {accident, Car, Car1b, Car2, Car2b}).

