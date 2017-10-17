-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).


% returns an empty map (a empty list)
new() ->
	[].

% updates the Map to reflect that Node has directional links to 
% all nodes in the list Links. The old entry is removed.
update(Node, Links, Map) ->
	Umap = lists:keydelete(Node, 1, Map), % sweeps through Map and deletes the links that are linked to the current Node and replaces it with the new given one
	[{Node, Links} | Umap ].


% returns the list of nodes directly reachable from Node.
reachable(Node, Map) ->
	case lists:keyfind(Node, 1, Map) of % keyfind returns false if key not found, so we want to return empty list in that case
		false -> 
			[];
		{_, ReachList} -> 
			ReachList
	end.

% returns a list of all nodes in the map, also the ones without 
% outgoing links. So if berlin is linked to london but london 
% does not have any outgoing links (and thus no entry in the 
% list), london should still be in the returned list.
all_nodes(Map) ->
	lists:usort(lists:flatmap(fun({Node, Links}) -> [Node | Links] end, Map)).

