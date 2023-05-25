:- use_module(library(lists)).
% :- use_module(library(apply_macros)).
:- use_module(library(yall)).
:- use_module(library(thread)).
% :- use_module(library(lambda)).
% :- use_module(library(apply)).
:- ensure_loaded(listexportedGraph).


lambda([], N, IngoingSum, IngoingSum).
lambda([IngoingNode|IngoingNodes], N, Acc, IngoingSum) :-
    rank(IngoingNode, OldVal, _),
    findall(ToNodeId, arc(_, IngoingNode, ToNodeId), OutgoingLinks),
    length(OutgoingLinks, L),
    NewAcc is Acc + (OldVal / L),
    lambda(IngoingNodes, N, NewAcc, IngoingSum).


single_iter([NodeId|NodeIds], N, DampingFactor, DanglingValue) :-
    !,
    findall(FromNodeId, arc(_, FromNodeId, NodeId), IngoingLinks),
    lambda(IngoingLinks, N, 0, IngoingSum),
    IngoingSumWithTeleport is ((1 - DampingFactor) / N) + DampingFactor * (IngoingSum + DanglingValue),
    retract(rank(NodeId, OldVal, _)),
    asserta(rank(NodeId, OldVal, IngoingSumWithTeleport)),
    single_iter(NodeIds, N, DampingFactor, DanglingValue).

single_iter([], N, DampingFactor, DanglingValue).



power_iter(NodeIds, N, DampingFactor, NIter, Error, Epsilon, NMaxIter) :-
    NIter < NMaxIter,
    Error >= Epsilon,
    !,
    findall(ValueToSum, (rank(NodeId, RankValue, _), \+arc(_, NodeId, _), ValueToSum is RankValue * (1/N)), ValuesToSum),
    sumlist(ValuesToSum, DanglingWeight),
    single_iter(NodeIds, N, DampingFactor, DanglingWeight),

    format('~e\n', [Error]),

    findall(SingleError, (rank(_, ROld, RNew), abs(RNew - ROld, SingleError)), RErrorList),
    sumlist(RErrorList, NewError),
    
    new_iteration(NodeIds),
    NewNIter is NIter + 1,
    power_iter(NodeIds, N, DampingFactor, NewNIter, NewError, Epsilon, NMaxIter).

power_iter(NodeIds, N, DampingFactor, NIter, Error, Epsilon, NMaxIter) :-
    ((Error < Epsilon) ->
        format('Convergence in ~d steps!. Error = ~e', [NIter, Error]);
        format('Not Converged in ~d steps!. Error = ~e', [NIter, Error])).


rank(X, Y) :- rank(X, _, Y).


new_iteration([]).
new_iteration([NodeId|NodeIds]) :-
    retract(rank(NodeId, _, Value)),
    asserta(rank(NodeId, Value, Value)),
    new_iteration(NodeIds).


assert_init([], DefaultValue).
assert_init([NodeId|NodeIds], DefaultValue) :-
    asserta(rank(NodeId, DefaultValue, DefaultValue)),
    assert_init(NodeIds, DefaultValue).


prova :-

    retractall(rank(_, _, _)),

    write('NodeIDs computation\n'),
    time(findall(X, node_properties(X, _), NodeIDs)),
    length(NodeIDs, N),

    write('Page Rank\n'),
    DefaultValue is 1/N,
    assert_init(NodeIDs, DefaultValue),

    DampingFactor = 0.85,
    MaxIter = 100,
    Epsilon = 0.00001,

    time(power_iter(NodeIDs, N, DampingFactor, 0, 1, Epsilon, MaxIter)).



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


% node_properties(0, []).
% node_properties(1, []).
% node_properties(2, []).
% node_properties(3, []).
% node_properties(4, []).
% node_properties(5, []).

% arc(48188, 0, 1).
% arc(48189, 0, 2).

% arc(48191, 1, 5).

% arc(48192, 2, 0).
% arc(48193, 2, 4).
% arc(48194, 2, 3).

% arc(48195, 3, 5).

% arc(48196, 4, 0).

% arc(48197, 5, 0).