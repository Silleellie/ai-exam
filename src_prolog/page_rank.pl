:- use_module(library(lists)).
% :- use_module(library(apply_macros)).
:- use_module(library(yall)).
:- use_module(library(thread)).
% :- use_module(library(lambda)).
% :- use_module(library(apply)).
% :- ensure_loaded(listexportedGraph).


lambda([], _, _).
lambda([Index|Indexes], RElem, ValueToAssign) :-
    Value is RElem * ValueToAssign,
    vector(Index, CurrentVal),

    Final is CurrentVal + Value,
    asserta(vector(Index, Final)),
    retract(vector(Index, CurrentVal)),
    lambda(Indexes, RElem, ValueToAssign).



single_iter([NodeId|NodeIds], [RElem|Rs]) :-
    !,
    findall(ToNodeId, arc(_, NodeId, ToNodeId), OutgoingLinks),
    length(OutgoingLinks, LengthOutgoing),
    ((LengthOutgoing \== 0) -> (ValueToAssign is 1/LengthOutgoing); ValueToAssign = 0),
    lambda(OutgoingLinks, RElem, ValueToAssign),

    single_iter(NodeIds, Rs).

single_iter([], []).



assert_init([]).
assert_init([NodeId|NodeIds]) :-
    asserta(vector(NodeId, 0)),
    assert_init(NodeIds).

assert_init(0).




prova :-

    write('NodeIDs computation\n'),
    time(findall(X, node_properties(X, _), NodeIDs)),

    write('Initial R computation\n'),
    length(NodeIDs, N),
    time(findall(1/N, between(1, N, _), OldR)),

    write('Page Rank\n'),
    assert_init(NodeIDs),
    time(single_iter(NodeIDs, OldR)).





% node(0, 'Person').
% node(0, 'retrocomputing').
% node_properties(0, ['deathDate'-'08/04/2012', 'name'-'Jack', 'gender'-'M', 'subClass'-'Person', 'birthDate'-'13/12/1928', 'surname'-'Tramiel']).
% node(1, 'retrocomputing').
% node(1, 'Organization').
% node_properties(1, ['name'-'Commodore Business Machines', 'closingDate'-'29/04/1994', 'subClass'-'Organization', 'acronym'-'CBM']).
% node(2, 'Person').
% node(2, 'retrocomputing').
% node_properties(2, ['diedIn'-'335869', 'bornIn'-'335215', 'gender'-'M', 'nationality'-'usa', 'subClass'-'Person', 'surname'-'Peddle', 'deathDate'-'15/12/2019', 'nickname'-'Chuck', 'name'-'Charles Ingerham', 'title'-'Eng', 'birthDate'-'25/11/1937']).


% arc(48187, 0, 0).
% arc_properties(48187, ['subClass'-'expresses']).
% arc(48185, 0, 1).
% arc_properties(48185, ['subClass'-'expresses']).
% arc(343416, 1, 0).
% arc_properties(343416, ['subClass'-'similarTo']).
% arc(209143, 1, 2).
% arc_properties(209143, ['subClass'-'expresses']).
% arc(86995, 2, 1).
% arc_properties(86995, ['subClass'-'expresses']).


node_properties(0, []).
node_properties(1, []).
node_properties(2, []).
node_properties(3, []).
node_properties(4, []).
node_properties(5, []).

arc(48187, 0, 0).
arc(48188, 0, 1).
arc(48189, 0, 2).

arc(48190, 1, 1).
arc(48191, 1, 5).

arc(48192, 2, 0).
arc(48193, 2, 4).
arc(48194, 2, 3).

arc(48195, 3, 3).
arc(48195, 3, 5).

arc(48196, 4, 0).