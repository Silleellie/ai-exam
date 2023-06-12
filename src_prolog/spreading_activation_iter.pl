:- module(spreading_activation_iter, 
        [
            spreading_activation/3,
            spreading_activation/6,
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
% Simplified predicate which will call the 'spreading_activation/6' predicate with specified arguments
% + initialized Epsilon to 0.0001 and NMaxIter to 100
%
% Arguments:
%   StartNodes: List of node identifiers to use as 'start nodes' (their activation value will be set to 1);
%   FiringThreshold: The minimum activation value necessary for a node to keep spreading over to its neighbors;
%   DecayRate: The activation value decay when it is spread to neighboring nodes.

spreading_activation(StartNodes, FiringThreshold, DecayRate) :-

    GeometricDecayFactor = 0.8,
    Epsilon = 0.0001,
    NMaxIter = 100,

    spreading_activation(StartNodes, FiringThreshold, DecayRate, GeometricDecayFactor, Epsilon, NMaxIter).


%% spreading_activation(+StartNodes, +FiringThreshold, +DecayRate, +GeometricDecayFactor, +Epsilon, +NMaxIter)
%
% This procedure performs the spreading activation algorithm on the graph defined from the original instances of Graph Brain (restructured in the
% list-based formalism) to identify which are its most important nodes starting from a list of 'start nodes'.
% In this 'eps' formulation, the spreading activation algorithm will stop if the L1 norm between the activation values of nodes at the beginning of 
% the iteration compared to the values at the end of it is less than the specified Epsilon.
%
% Note that, in this formulation:
%   - nodes that have been previously fired may fire again;
%   - the geometric decay rate is used rather than the linear one, as in the basic formulation.
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
%   DecayRate: The activation value decay when it is spread to neighboring nodes.
%   GeometricDecayFactor: Factor by which the DecayRate will be reduced at the end of the iteration;
%   Epsilon: Threshold value, the algorithm will stop once StopCrit is lower than this value;ù
%   NMaxIter: Maximum number of iterations after which the algorithm will stop regardless of convergence

spreading_activation(StartNodes, FiringThreshold, DecayRate, GeometricDecayFactor, Epsilon, NMaxIter) :-

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

    write('***** Starting Spreading Activation computation (Stopping criterion is the L1 Norm) *****\n'),
    time(spread_iter_eps(NodeIDs, FiringThreshold, DecayRate, GeometricDecayFactor, Epsilon, NMaxIter)),
    
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
    assertz(node_sa_info(NodeID, DefaultValue, DefaultValue)),
    assert_init_all(NodeIDs, DefaultValue).


% new_iteration_init(+NodeIDs)
%
% At the start of a new iteration, "new" activation values become "old", and are thus copied in the second argument of 'node_sa_info' predicate.
%
% Arguments:
%   NodeIDs: List of identifiers for all nodes in the graph.

new_iteration_init([]).

new_iteration_init([NodeID|NodeIDs]) :-
    retract(node_sa_info(NodeID, _, Value)),
    assertz(node_sa_info(NodeID, Value, Value)),
    new_iteration_init(NodeIDs).


% spread_iter_eps(+NodeIDs, +FiringThreshold, +DecayRate, +NIter, +StopCrit, +GeometricDecayFactor, +Epsilon, +NMaxIter)
%
% Performs a recursive iteration of the spreading activation 'eps' algorithm. Spreads the activation of all nodes with
% activation value greater than threshold and computes L1 norm to check if it should stop iterating.
%
% If it keeps iterating, the DecayFactor is also reduced by the GeometricDecayFactor in the next iteration.
%
% Once the L1 norm is lower than the Epsilon, the algorithm will stop and a message will be displayed to show the L1 norm.
% If the number of iterations is greater than the number of maximum iterations, the algorithm will stop regardless of convergence.
%
% Arguments:
%   NodeIDs: All node ids in the graph;
%   FiringThreshold: The minimum activation value necessary for a node to keep spreading over to its neighbors;
%   DecayRate: The activation value decay when it is spread to neighboring nodes (ex. with decay rate 0.9 and 
%     activation value for current node 0.7, the final value will be 0.9 * 0.7);
%   NIter: Current iteration number. It is initialized to 0;
%   StopCrit: L1 norm associated with the previous iteration. It is initialized to 1;
%   GeometricDecayFactor: Factor by which the DecayRate will be reduced at the end of the iteration;
%   Epsilon: Threshold value, the algorithm will stop once StopCrit is lower than this value;
%   NMaxIter: Maximum number of iterations after which the algorithm will stop regardless of convergence.

spread_iter_eps(NodeIDs, FiringThreshold, DecayRate, GeometricDecayFactor, Epsilon, NMaxIter) :-
    spread_iter_eps(NodeIDs, FiringThreshold, DecayRate, 0, 1, GeometricDecayFactor, Epsilon, NMaxIter).

spread_iter_eps(NodeIDs, FiringThreshold, DecayRate, NIter, StopCrit, GeometricDecayFactor, Epsilon, NMaxIter) :-
    NIter < NMaxIter,
    StopCrit >= Epsilon,

    format('Iteration ~d ---> Stopping Criterion = ~e \t[MaxIter=~d, Epsilon=~e]\n', [NIter, StopCrit, NMaxIter, Epsilon]),
    !,
    spread_activation_value(NodeIDs, DecayRate, FiringThreshold),
    
    findall(SingleStopCrit, (node_sa_info(_, ActValOld, ActValNew), SingleStopCrit is abs(ActValNew - ActValOld)), StopCritList),
    sum_list(StopCritList, NewStopCrit),

    new_iteration_init(NodeIDs),
    NewNIter is NIter + 1,
    NewDecayRate is DecayRate * GeometricDecayFactor,
    spread_iter_eps(NodeIDs, FiringThreshold, NewDecayRate, NewNIter, NewStopCrit, GeometricDecayFactor, Epsilon, NMaxIter).

spread_iter_eps(_, _, _, NIter, StopCrit, _, Epsilon, _) :-
    ((StopCrit < Epsilon) -> 
        format('\nConvergence reached in ~d iterations! Stopping criterion = ~e (< Epsilon=~e)\n', [NIter, StopCrit, Epsilon]);
        format('\nConvergence not reached in ~d iterations! Stopping criterion = ~e (>= Epsilon=~e)\n', [NIter, StopCrit, Epsilon])).


% spread_activation_value(+NodeIDs, +DecayRate, +FiringThreshold)
%
% Spreads the activation of all nodes in the graph having activation value greater than FiringThreshold to all neighboring nodes.
% Note that the most recent activation value in the current iteration will always be spread and not the oldest one.
% Also note that the graph will be converted to a Directed Graph if it is a Multi Graph (more than one link connecting same subject-object nodes).
%
% Arguments:
%   NodeIDs: All node ids in the graph;
%   DecayRate: The activation value decay when it is spread to neighboring nodes (ex. with decay rate 0.9 and 
%     activation value for current node 0.7, the final value will be 0.9 * 0.7);
%   FiringThreshold: The minimum activation value necessary for a node to keep spreading over to its neighbors.

spread_activation_value([NodeID|NodeIDs], DecayRate, FiringThreshold) :-
    node_sa_info(NodeID, _, SourceActVal),
    SourceActVal > FiringThreshold,
    !,
    findall(ToNodeID, arc(_, NodeID, ToNodeID), OutgoingLinksList),
    sort(OutgoingLinksList, OutgoingLinks),  % to remove duplicates
    adjust_activation_value(OutgoingLinks, SourceActVal, DecayRate),
    
    spread_activation_value(NodeIDs, DecayRate, FiringThreshold).

spread_activation_value([_|NodeIDs], DecayRate, FiringThreshold) :-
    spread_activation_value(NodeIDs, DecayRate, FiringThreshold).

spread_activation_value([], _, _).


% adjust_activation_value(+NodeIDsToAdjust, +SourceNodeActVal, +DecayRate)
%
% Modified the activation value of the given nodes by summing their actual value with the product between the activation value of the 
% source node with the specified decay rate.
%
% Arguments:
%   NodeIDsToAdjust: List of identifiers for all nodes which will have their activation value modified;
%   SourceNodeActValue: Activation value of the source node pointing at the nodes in NodeIDsToAdjust;
%   DecayRate: The activation value decay when it is spread to neighboring nodes (ex. with decay rate 0.9 and 
%     activation value for current node 0.7, the final value will be 0.9 * 0.7).

adjust_activation_value([NodeIDToAdjust|NodeIDsToAdjust], SourceNodeActVal, DecayRate) :-
    retract(node_sa_info(NodeIDToAdjust, OldActVal, NewActVal)),
    SpreadedActVal is NewActVal + (SourceNodeActVal * 1 * DecayRate), % 1 = arc weight
    limit_activation_value(SpreadedActVal, LimitedSpreadedActVal),
    assertz(node_sa_info(NodeIDToAdjust, OldActVal, LimitedSpreadedActVal)),
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
