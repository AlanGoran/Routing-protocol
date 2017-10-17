-module(dijkstra).
-export([update/4, iterate/3, table/2, route/2]).

% returns the length of the shortest path to the node or 0 if the node is not found.
entry(Node, Sorted) ->
	case lists:keyfind(Node, 1, Sorted) of % keyfind returns false if key not found, so we want to return 0 in that case
		false -> 
			0; % see update for the reason behing this 0
		{_, Length, _} -> 
			Length
	end.



% replaces the entry for Node in Sorted with a new entry having a new length N and 
% Gateway. The resulting list should of course be sorted.
replace(Node, N, Gateway, Sorted) ->
	case lists:keyfind(Node, 1, Sorted) of
		false -> 
			"Not found";
		{_, _, _} -> 
			NewEntry = {Node, N, Gateway}, % new entry created
			DeletedSortedList = lists:keydelete(Node, 1, Sorted), % old entry removed from list
			lists:keysort(2,[NewEntry | DeletedSortedList]) % new entry added to list and then sorted based on N
	end.

% update the list Sorted given the information that Node can be reached in N hops 
% using Gateway. If no entry is found then no new entry is added. Only if we have 
% a better (shorter) path should we replace the existing entry.
update(Node, N, Gateway, Sorted) ->
	Length = entry(Node, Sorted),
	if
		Length>N -> % if the node is not found in the list then Length = 0 which makes this statement never true and therefore nothing is replaced
			replace(Node, N, Gateway, Sorted);
		true -> % works as an 'else' branch
			Sorted
	end.

% construct a table given a sorted list of nodes, a map and a table constructed so far.
iterate(Sorted, Map, Table) ->
	case Sorted of
		[] ->					% Step 1: If there are no more entries in the sorted list then we are done and the given routing table is complete.
			Table;
		[{_, inf, _} | _] -> 	% Step 2: If the first entry is a dummy entry with an infinite path to a city we know that the rest of the sorted list is also of infinite length and the given routing table is complete.
			Table;
		[FirstEntry | Rest] ->  % Step 3: Otherwise, take the first entry in the sorted list, find the nodes in the map reachable from this entry and for each of these nodes update the Sorted list. The entry that you took from the sorted list is added to the routing table.
			{FirstNode, N, Gateway} = FirstEntry,
			
			%% ALTERNATIVE CODE
			% ReachList = map:reachable(FirstNode, Map),
			% % update sorted by looping through all the reachable elements from the FirstNode and setting the hops between the reachable and gateway to N+1 (because another "hop" was just added) 
			% UpdatedSorted = lists:foldl(fun(Reach,Acc) -> update(Reach,N+1,Gateway,Acc) end, Rest, ReachList),
			% % add FirstNode and Gateway wo Table and iterate again with updated Sorted 
			% iterate(UpdatedSorted, Map,[{FirstNode, Gateway} | Table])


			case lists:keyfind(FirstNode, 1, Map) of
				false -> 
					iterate(Rest, Map,[{FirstNode, Gateway} | Table]);
				{_, ReachList} ->
					% update Sorted by looping through all the reachable elements from the FirstNode and setting the hops between the reachable and gateway to N+1 (because another "hop" was just added) 
					UpdatedSorted = lists:foldl(fun(Reach,Acc) -> update(Reach,N+1,Gateway,Acc) end, Rest, ReachList),
					% add FirstNode and Gateway wo Table and iterate again with updated Sorted 
					iterate(UpdatedSorted, Map,[{FirstNode, Gateway} | Table])
			end
	end.

% construct a routing table given the gateways and a map.
table(Gateways, Map) ->
    AllNodes = map:all_nodes(Map),
    % loop through AllNodes and do the following
    List = lists:map(fun(Node) ->
			case lists:member(Node, Gateways) of
			true ->
				{Node, 0, Node};
			false ->
				{Node, inf, unknown}
			end
		end, AllNodes),
    Sorted = lists:keysort(2,List),
    iterate(Sorted, Map, []).


%  search the routing table and return the gateway suitable to route messages to a node. If a gateway is found we should return {ok, Gateway} otherwise we return notfound.
route(Node, Table) ->
     case lists:keyfind(Node,1,Table) of
     	{Node, Gateway} -> 
     		{ok, Gateway};
     	false -> 
     		notfound
	end.   













