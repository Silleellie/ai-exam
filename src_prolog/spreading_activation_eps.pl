:- module(spreading_activation_eps, 
        [
            spreading_activation/3,
            spreading_activation/6,
            activation/2
        ]).

:- use_module(library(lists)).
:- unknown(_, fail).
:- set_output(user_output).  % all prints will be visualized in stdout
:- discontiguous node/2.
:- discontiguous node_properties/2.
:- discontiguous arc/3.
:- discontiguous arc_properties/2.

% uncomment this if you're using SWI
% :- use_module(library(statistics)).


activation(NodeID, ActVal) :- node_sa_info(NodeID, ActVal, _).


spreading_activation(StartNodes, FiringThreshold, DecayRate) :-

    GeometricDecayFactor = 0.8,
    Epsilon = 0.0001,
    NMaxIter = 100,

    spreading_activation(StartNodes, FiringThreshold, DecayRate, GeometricDecayFactor, Epsilon, NMaxIter).

spreading_activation(StartNodes, FiringThreshold, DecayRate, GeometricDecayFactor, Epsilon, NMaxIter) :-

    write('Please enter file path for graph instances (in list format): '),
    read(GraphFile),
    write('\n***** Loading Graph instances file *****\n'),
    ensure_loaded(GraphFile),
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
    spread_iter_eps(NodeIDs, FiringThreshold, DecayRate, GeometricDecayFactor, Epsilon, NMaxIter),
    
    write('\nCheck Spreading Activation value of each node using the activation predicate! (e.g. activation(0, X) X is the SA value of node with id 0)').


assert_init_all([], _).

assert_init_all([NodeID|NodeIDs], DefaultValue) :-
    assertz(node_sa_info(NodeID, DefaultValue, DefaultValue)),
    assert_init_all(NodeIDs, DefaultValue).


new_iteration_init([]).

new_iteration_init([NodeID|NodeIDs]) :-
    retract(node_sa_info(NodeID, _, Value)),
    assertz(node_sa_info(NodeID, Value, Value)),
    new_iteration_init(NodeIDs).


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


spread_activation_value([NodeID|NodeIDs], DecayRate, FiringThreshold) :-
    node_sa_info(NodeID, _, SourceActVal),
    SourceActVal > FiringThreshold,
    !,
    findall(ToNodeID, arc(_, NodeID, ToNodeID), OutgoingLinksList),
    sort(OutgoingLinksList, OutgoingLinks), % to remove duplicates
    adjust_activation_value(OutgoingLinks, SourceActVal, DecayRate),
    
    spread_activation_value(NodeIDs, DecayRate, FiringThreshold).

spread_activation_value([_|NodeIDs], DecayRate, FiringThreshold) :-
    spread_activation_value(NodeIDs, DecayRate, FiringThreshold).

spread_activation_value([], _, _).


adjust_activation_value([NodeIDToAdjust|NodeIDsToAdjust], SourceNodeActVal, DecayRate) :-
    retract(node_sa_info(NodeIDToAdjust, OldActVal, NewActVal)),
    SpreadedActVal is NewActVal + (SourceNodeActVal * 1 * DecayRate), % 1 = arc weight
    limit_activation_value(SpreadedActVal, LimitedSpreadedActVal),
    assertz(node_sa_info(NodeIDToAdjust, OldActVal, LimitedSpreadedActVal)),
    adjust_activation_value(NodeIDsToAdjust, SourceNodeActVal, DecayRate).

adjust_activation_value([], _, _).


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
