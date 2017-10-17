-module(intf).
-export([new/0,add/4,remove/2,lookup/2,ref/2,name/2,list/1,broadcast/2]).

%return an empty set of interfaces
new() ->
	[].
	

%add a new entry to the set and return the new set of interfaces.
add(Name, Ref, Pid, Intf) -> 
	case lists:member({Name,Ref,Pid}, Intf) of
	    true ->
	        Intf;
	    false ->
	        [{Name,Ref,Pid}|Intf]
	end.


%remove an entry given a name of an interface, return a new set of interfaces.
remove(Name, Intf) -> 
	lists:keydelete(Name,1,Intf).


%find the process identifier given a name, return {ok, Pid} if found otherwise notfound.
lookup(Name, Intf) -> 
	case lists:keyfind(Name,1,Intf) of 
		{_,_,Pid} -> 
			{ok, Pid};
		false -> 
			notfound
	end.


%find the reference given a name and return {ok, Ref} or notfound.
ref(Name, Intf) ->
	case lists:keyfind(Name,1,Intf) of
		{_,Ref,_} -> 
			{ok, Ref};
		false -> 
			notfound
	end.


%find the name of an entry given a reference and return {ok, Name} or notfound.
name(Ref, Intf) -> 
	case lists:keyfind(Ref,2,Intf) of
		{Name,_,_} -> 
			{ok, Name};
		false -> 
			notfound
	end.


%return a list with all names
list(Intf) -> 
	lists:map(fun({Name,_,_}) -> Name end, Intf). % Picks out all the "Names" in each entry in the whole list and puts them in a list


%send the message to all interface processes.
broadcast(Message, Intf) ->
	lists:map(fun({_,_,P}) -> P ! Message end, Intf).
	

