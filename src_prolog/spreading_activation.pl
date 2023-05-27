:- discontiguous node/2.
:- discontiguous node_properties/2.
:- discontiguous arc/3.
:- discontiguous arc_properties/2.
:- ensure_loaded('../data/processed/listexportedGraph.pl').
:- use_module(library(lists)).




limit_activation_value(ActVal, 1) :-
    ActVal > 1,
    !.

limit_activation_value(ActVal, 0) :-
    ActVal < 0,
    !.

limit_activation_value(ActVal, ActVal).


adjust_activation_value([NodeIDToAdjust|NodeIDsToAdjust], SourceNodeActVal, DecayRate) :-
    retract(node_sa_info(NodeIDToAdjust, OldActVal, FiredState)),
    NewActVal is OldActVal + (SourceNodeActVal * 0.9 * DecayRate), %%%%%%%%%%%%%%%%%%%% SET TO 1 LATER!
    limit_activation_value(NewActVal, LimitedNewActVal),
    assertz(node_sa_info(NodeIDToAdjust, LimitedNewActVal, FiredState)),
    adjust_activation_value(NodeIDsToAdjust, SourceNodeActVal, DecayRate).
adjust_activation_value([], _, _).


spread_unfired([UnfiredNodeID|UnfiredNodeIDs], DecayRate) :-
    retract(node_sa_info(UnfiredNodeID, ActVal, 0)),
    asserta(node_sa_info(UnfiredNodeID, ActVal, 1)),  % node is now fired

    findall(ToNodeID, arc(_, UnfiredNodeID, ToNodeID), OutgoingLinksList),
    list_to_set(OutgoingLinksList, OutgoingLinks),
    adjust_activation_value(OutgoingLinks, ActVal, DecayRate),
    
    spread_unfired(UnfiredNodeIDs, DecayRate).

spread_unfired([], _).


spread_iter(FiringThreshold, DecayRate) :-
    findall(UnfiredNodeID, (node_sa_info(UnfiredNodeID, ActVal, 0), ActVal > FiringThreshold), NodesToFire),
    length(NodesToFire, NodesToFireLen),
    NodesToFireLen =\= 0,
    !,
    spread_unfired(NodesToFire, DecayRate),
    spread_iter(FiringThreshold, DecayRate).

spread_iter(_, _).


assert_init_all([], _).
assert_init_all([NodeID|NodeIDs], DefaultValue) :-
    assertz(node_sa_info(NodeID, DefaultValue, 0)),
    assert_init_all(NodeIDs, DefaultValue).


prova :-

    FiringThreshold = 0.33, 
    DecayRate = 0.85,
    Weight = 0.9,
    
    StartNodes = [0, 1],

    % write('Please enter file path for graph instances (in list format): '),
    % read(GraphFile),
    % write('\n***** Loading Graph instances file *****\n'),
    % ensure_loaded(GraphFile),
    % write('Successfully loaded!\n\n'),

    % at the beginning of a new page rank call, the old rank must be
    % re-initialized, thus relative predicate is retracted for each node
    retractall(node_sa_info(_, _, _)),

    % retrieve all unique nodes
    write('***** Finding all unique NodeIDs *****\n'),
    findall(X, node_properties(X, _), NodeIDsList),
    list_to_set(NodeIDsList, NodeIDs),
    length(NodeIDs, NNodes),
    format('Found ~d unique nodes!\n\n', [NNodes]),

    subtract(NodeIDs, StartNodes, NotStartingNodes),
    
    assert_init_all(NotStartingNodes, 0),
    assert_init_all(StartNodes, 1),

    spread_iter(FiringThreshold, DecayRate).







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
