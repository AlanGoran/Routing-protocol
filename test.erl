-module(test).
-export([startRoutingNodes/1,broadcast/0,update/0,send/0,remove/0,stop/0]).


startRoutingNodes(PID) -> % PID = 'sweden@130.123.112.23'
	routy:start(r1, stockholm),
	routy:start(r2, london),
	routy:start(r3, mumbai),
	routy:start(r4, sydney),
	routy:start(r5, vegas),	

	r1 ! {add, london, {r2, PID}},
	r2 ! {add, mumbai, {r3, PID}},
	r1 ! {add, mumbai, {r3, PID}},
	r3 ! {add, sydney , {r4, PID}},
	r3 ! {add, vegas, {r5, PID}},
	r5 ! {add, stockholm, {r1, PID}},
	r4 ! {add, stockholm, {r1, PID}}.

broadcast()->
    r1 ! broadcast, 
    r2 ! broadcast, 
    r3 ! broadcast,
    r4 ! broadcast,
    r5 ! broadcast.

update()->
    r1 ! update, 
    r2 ! update,
    r3 ! update,
    r4 ! update,
    r5 ! update.


send()->
    r1 ! {send, sydney, "Hey! What is the weather like? - Stockholm"},
    timer:sleep(500),
    r4 ! {send, stockholm, "30 degrees, my man! - Sydney"}.


remove() ->
    r1 ! {remove, mumbai}. 


stop()-> %Close all the nodes in the network
    r1 ! stop,
    r2 ! stop,
    r3 ! stop,
    r4 ! stop,
    r5 ! stop.



