:- module(spreading_activation_basic, 
        [
            spreading_activation/3,
            activation/2
        ]).

:- use_module(library(lists)).
:- use_module(utils).
:- unknown(_, fail).
:- discontiguous node/2.
:- discontiguous node_properties/2.
:- discontiguous arc/3.
:- discontiguous arc_properties/2.

% uncomment this if you're using SWI
% :- use_module(library(statistics)).



%% spreading_activation(+StartNodes, +FiringThreshold, +DecayRate)
%
% This procedure performs the spreading activation algorithm on the graph defined from the original instances of Graph Brain (restructured in the
% list-based formalism) to identify which are its most important nodes starting from a list of 'start nodes'.
% In this basic formulation, the spreading activation algorithm will stop if no nodes have an activation value above the firing threshold 
% or all nodes have already been fired.
%
% The procedure loads automatically the list-based graph from 'outputs' directory. If more than one eligible list-based graph is available,
% the user is prompted to choose the appropriate one.
%
% The algorithm results can be checked by invoking the 'activation' procedure:
%   - e.g. activation(0, X) X is the activation value for node with id 0
%
% Arguments:
%   StartNodes: List of node identifiers to use as 'start nodes' (their activation value will be set to 1);
%   FiringThreshold: The minimum activation value necessary for a node to keep spreading over to its neighbors;
%   DecayRate: The activation value decay when it is spread to neighboring nodes (ex. with decay rate 0.9 and 
%     activation value for current node 0.7, the final value will be 0.9 * 0.7).

spreading_activation(StartNodes, FiringThreshold, DecayRate) :-

    select_file('list_graph', GraphFile),
    atom_concat('outputs/', GraphFile, GraphPath),
    write('\n***** Loading Graph instances file *****\n'),
    ensure_loaded(GraphPath),
    write('Successfully loaded!\n\n'),

    % at the beginning of a new page rank call, the old rank must be
    % re-initialized, thus relative predicate is retracted for each node
    retractall(node_sa_info(_, _, _)),

    % retrieve all unique nodes
    write('***** Finding all unique NodeIDs *****\n'),
    findall(X, node_properties(X, _), NodeIDsList),
    sort(NodeIDsList, NodeIDs), % to remove duplicates
    length(NodeIDs, NNodes),
    format('Found ~d unique nodes!\n\n', [NNodes]),

    subtract(NodeIDs, StartNodes, NotStartingNodes),
    
    assert_init_all(NotStartingNodes, 0),
    assert_init_all(StartNodes, 1),

    write('***** Starting Spreading Activation computation *****\n'),
    time(spread_iter(FiringThreshold, DecayRate)),
    
    write('\nCheck Spreading Activation value of each node using the activation predicate! (e.g. activation(0, X) X is the SA value of node with id 0)\n').


%% activation(+NodeID, -ActVal)
%
% Predicate to call after calling the 'spreading_activation' predicate in order to check the activation value of the nodes
% of the graph:
%   - e.g. activation(0, X) X is the activation value for node with id 0
%
% Arguments:
%   NodeID: Id of the node for which the activation value should be retrieved
%   ActVal: Activation value of the node with id NodeID

activation(NodeID, ActVal) :- node_sa_info(NodeID, ActVal, _).


% assert_init_all(+NodeIDs, +DefaultValue)
%
% Initialized all 'node_sa_info' predicates for each node in the graph with the defined DefaultValue.
% 'node_sa_info' predicates will contain info regarding activation values of each node and their states
%   - 1 if already fired, 0 otherwise
%
% Arguments:
%   NodeIDs: List of identifiers for all nodes in the graph;
%   DefaultValue: The default activation value to set for all the nodes.

assert_init_all([], _).

assert_init_all([NodeID|NodeIDs], DefaultValue) :-
    assertz(node_sa_info(NodeID, DefaultValue, 0)),
    assert_init_all(NodeIDs, DefaultValue).


% spread_iter(+FiringThreshold, +DecayRate)
%
% Simplified predicate which will call the 'spread_iter/3' predicate with specified arguments + the number of iterations initialized to 0.

spread_iter(FiringThreshold, DecayRate) :-
    spread_iter(FiringThreshold, DecayRate, 0).


% spread_iter(+FiringThreshold, +DecayRate, +NIter)
%
% Performs a recursive iteration of the spreading activation algorithm. Finds all unfired nodes with an activation value greater than 
% the firing threshold and fires them.
%
% Once no more nodes can be fired, the algorithm will stop and a message will be displayed to show the total number of iterations.
%
% Arguments:
%   FiringThreshold: The minimum activation value necessary for a node to keep spreading over to its neighbors;
%   DecayRate: The activation value decay when it is spread to neighboring nodes (ex. with decay rate 0.9 and 
%      activation value for current node 0.7, the final value will be 0.9 * 0.7);
%   NIter: Current iteration number. It is initialized to 0.

spread_iter(FiringThreshold, DecayRate, NIter) :-
    findall(UnfiredNodeID, (node_sa_info(UnfiredNodeID, ActVal, 0), ActVal > FiringThreshold), NodesToFire),
    length(NodesToFire, NodesToFireLen),
    NodesToFireLen =\= 0,
    !,
    format('Iteration ~d ---> ~d nodes can be fired\n', [NIter, NodesToFireLen]),
    spread_unfired(NodesToFire, DecayRate),
    NewNIter is NIter + 1,
    spread_iter(FiringThreshold, DecayRate, NewNIter).

spread_iter(_, _, NIter) :-
    format('Iteration ~d ---> No nodes can be fired\n', [NIter]).


% spread_unfired(+UnfiredNodeIDs, +DecayRate)
%
% Spreads the activation value of all the given nodes to their neighbors. The activation value strength when spread to neighboring 
% nodes is reduced by the DecayRate factor. 
% Note that the graph will be converted to a Directed Graph if it is a Multi Graph (more than one link connecting same subject-object nodes)
%
% All spread nodes state will be also changed from unfired to fired.
%
% Arguments:
%   UnfiredNodeIDs: List of identifiers for all nodes which will be spread;
%   DecayRate: The activation value decay when it is spread to neighboring nodes (ex. with decay rate 0.9 and 
%      activation value for current node 0.7, the final value will be 0.9 * 0.7).

spread_unfired([UnfiredNodeID|UnfiredNodeIDs], DecayRate) :-
    retract(node_sa_info(UnfiredNodeID, ActVal, 0)),
    asserta(node_sa_info(UnfiredNodeID, ActVal, 1)),  % node is now fired

    findall(ToNodeID, arc(_, UnfiredNodeID, ToNodeID), OutgoingLinksList),
    sort(OutgoingLinksList, OutgoingLinks),  % to remove duplicates
    adjust_activation_value(OutgoingLinks, ActVal, DecayRate),
    
    spread_unfired(UnfiredNodeIDs, DecayRate).

spread_unfired([], _).


% adjust_activation_value(+NodeIDsToAdjust, +SourceNodeActVal, +DecayRate)
%
% Modified the activation value of the given nodes by summing their actual value with the product between the activation value of the 
% source node with the specified decay rate.
%
% Arguments:
%   NodeIDsToAdjust: List of identifiers for all nodes which will have their activation value modified;
%   SourceNodeActValue: Activation value of the source node pointing at the nodes in NodeIDsToAdjust;
%   DecayRate: The activation value decay when it is spread to neighboring nodes (ex. with decay rate 0.9 and 
%      activation value for current node 0.7, the final value will be 0.9 * 0.7).

adjust_activation_value([NodeIDToAdjust|NodeIDsToAdjust], SourceNodeActVal, DecayRate) :-
    retract(node_sa_info(NodeIDToAdjust, OldActVal, FiredState)),
    NewActVal is OldActVal + (SourceNodeActVal * 1 * DecayRate),  % 1 = arc weight
    limit_activation_value(NewActVal, LimitedNewActVal),
    assertz(node_sa_info(NodeIDToAdjust, LimitedNewActVal, FiredState)),
    adjust_activation_value(NodeIDsToAdjust, SourceNodeActVal, DecayRate).

adjust_activation_value([], _, _).


% limit_activation_value(+ActVal, -NewActVal)
%
% The spreading activation algorithm requires all activation values to be between 0 and 1 for all nodes. 
% When an activation value is modified, this procedure is called to set it to 0 (IF activation value LESS THAN 0)
% or to 1 (IF activation value GREATER THAN 1). If the modified activation value is in the range [0, 1] nothing is done. 
%
% Arguments:
%   ActVal: Activation value which may need to be limited;
%   NewActVal: Newly limited activation value.

limit_activation_value(ActVal, 1) :-
    ActVal > 1,
    !.

limit_activation_value(ActVal, 0) :-
    ActVal < 0,
    !.

limit_activation_value(ActVal, ActVal).



% node_properties(1, []).
% node_properties(2, []).
% node_properties(3, []).
% node_properties(4, []).
% node_properties(5, []).
% node_properties(6, []).
% node_properties(7, []).
% node_properties(8, []).
% node_properties(9, []).
% node_properties(10, []).
% node_properties(11, []).
% node_properties(12, []).
% node_properties(13, []).
% node_properties(14, []).
% node_properties(15, []).
% node_properties(16, []).




% arc(48188, 1, 2).

% arc(48189, 2, 3).

% arc(48191, 3, 11).
% arc(48192, 3, 4).

% arc(48193, 4, 5).

% arc(48194, 5, 6).

% arc(48194, 6, 7).

% arc(48194, 7, 8).

% arc(48194, 8, 9).

% arc(48194, 9, 10).

% arc(48191, 11, 12).

% arc(48191, 12, 13).

% arc(48191, 13, 14).

% arc(48191, 14, 15).

% arc(48191, 15, 16).
