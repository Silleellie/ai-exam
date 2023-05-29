:- module(page_rank, 
        [
            page_rank/0,
            page_rank/3,
            page_rank/5,
            rank/2,
            power_iter/6
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


rank(NodeID, RankVal) :- node_pr_info(NodeID, _, RankVal, _).


page_rank :-

    DampingFactor = 0.85,
    Epsilon = 0.000001,
    MaxIter = 100,
    RankStartVector = [],
    PersonalizationVector = [],

    page_rank(DampingFactor, Epsilon, MaxIter, RankStartVector, PersonalizationVector).

page_rank(DampingFactor, Epsilon, MaxIter) :-
    
    RankStartVector = [],
    PersonalizationVector = [],

    page_rank(DampingFactor, Epsilon, MaxIter, RankStartVector, PersonalizationVector).

page_rank(DampingFactor, Epsilon, MaxIter, RankStartVector, PersonalizationVector) :-

    write('Please enter file path for graph instances (in list format): '),
    read(GraphFile),
    write('\n***** Loading Graph instances file *****\n'),
    ensure_loaded(GraphFile),
    write('Successfully loaded!\n\n'),

    % at the beginning of a new page rank call, the old rank must be
    % re-initialized, thus relative predicate is retracted for each node
    retractall(node_pr_info(_, _, _, _)),

    % retrieve all unique nodes
    write('***** Finding all unique NodeIDs *****\n'),
    findall(X, node_properties(X, _), NodeIDsList),
    sort(NodeIDsList, NodeIDs), % to remove duplicates
    length(NodeIDs, NNodes),
    format('Found ~d unique nodes!\n\n', [NNodes]),

    % assert node_pr_info predicate containing normalized init rank value and number of outlinks
    write('***** Asserting start rank values *****\n'),
    length(RankStartVector, RLen),
    ((RLen > 0) ->
        (
            write('Found custom rank start vector, initial rank vector will be set accordingly\n\n'),
            findall(RVal, member(_-RVal, RankStartVector), RankStartValues),
            sum_list(RankStartValues, RankNormalizationValue)
        ) ;
        (
            write('No rank start vector found, initial rank will be set to 1 / N for all nodes (N = Number of Nodes)\n\n'),
            RankNormalizationValue = NNodes
        )
    ),
    assert_init_all(NodeIDs, RankStartVector, RankNormalizationValue),
    
    % normalizing and filling personalization vector with possible missing nodes
    write('***** Personalization vector *****\n'),
    length(PersonalizationVector, PersonalizationLength),
    
    ((PersonalizationLength > 0) ->
        (
            write('Found personalization vector, personalization values will be set accordingly\n\n'), 
            findall(PersVal, member(_-PersVal, PersonalizationVector), PersonalizationValues),
            sum_list(PersonalizationValues, PersNormalizationValue)
        ) ;
        (   
            write('No personalization vector found\n\n'),
            PersNormalizationValue = NNodes
        )
    ),
    fill_personalization(NodeIDs, PersonalizationVector, PersNormalizationValue, FilledPersonalizationVector),

    % start page rank computation
    write('***** Starting Page Rank computation (Stopping criterion is the L1 Norm) *****\n'),

    time(power_iter(NodeIDs, FilledPersonalizationVector, NNodes, DampingFactor, Epsilon, MaxIter)),
    
    write('\nCheck PR value of each node using the rank predicate! (e.g. rank(0, X) X is the PR value of node with id 0)').


assert_init_all([], _, _).

assert_init_all([NodeID|NodeIDs], RankStartVector, NormalizationValue) :-
    findall(ToNodeID, arc(_, NodeID, ToNodeID), OutgoingLinksList),
    sort(OutgoingLinksList, OutgoingLinks), % to remove duplicates
    length(OutgoingLinks, OutLen),
    assert_init_single(NodeID, RankStartVector, NormalizationValue, OutLen),
    assert_init_all(NodeIDs, RankStartVector, NormalizationValue).


assert_init_single(NodeID, [], NormalizationValue, OutLen) :-
    !,
    NormalizedRankStartValue is 1 / NormalizationValue,
    assertz(node_pr_info(NodeID, NormalizedRankStartValue, NormalizedRankStartValue, OutLen)).

assert_init_single(NodeID, RankStartVector, NormalizationValue, OutLen) :-
    member(NodeID-RankStartValue, RankStartVector),
    !,
    NormalizedRankStartValue is RankStartValue / NormalizationValue,
    assertz(node_pr_info(NodeID, NormalizedRankStartValue, NormalizedRankStartValue, OutLen)).

assert_init_single(NodeID, _, _, OutLen) :-
    assertz(node_pr_info(NodeID, 0, 0, OutLen)).


fill_personalization([], _, _, []).

fill_personalization([_|NodeIDs], [], NormalizationValue, [PersVal|FilledPersonalizationVector]) :-
    PersVal is 1 / NormalizationValue,
    !,
    fill_personalization(NodeIDs, [], NormalizationValue, FilledPersonalizationVector).

fill_personalization([NodeID|NodeIDs], PersonalizationVector, NormalizationValue, [NormalizedPersVal|FilledPersonalizationVector]) :-
    member(NodeID-PersVal, PersonalizationVector),
    !,
    NormalizedPersVal is PersVal / NormalizationValue,
    fill_personalization(NodeIDs, PersonalizationVector, NormalizationValue, FilledPersonalizationVector).

fill_personalization([_|NodeIDs], PersonalizationVector, NormalizationValue, [0|FilledPersonalizationVector]) :-
    fill_personalization(NodeIDs, PersonalizationVector, NormalizationValue, FilledPersonalizationVector).


new_iteration_init([]).

new_iteration_init([NodeID|NodeIDs]) :-
    retract(node_pr_info(NodeID, _, Value, NOutgoingLink)),
    assertz(node_pr_info(NodeID, Value, Value, NOutgoingLink)),
    new_iteration_init(NodeIDs).


power_iter(NodeIDs, PersonalizationVector, NNodes, DampingFactor, Epsilon, NMaxIter) :-
    power_iter(NodeIDs, PersonalizationVector, NNodes, DampingFactor, 0, 1, Epsilon, NMaxIter).

power_iter(NodeIDs, PersonalizationVector, NNodes, DampingFactor, NIter, StopCrit, Epsilon, NMaxIter) :-
    NIter < NMaxIter,
    StopCrit >= Epsilon,
    !,
    compute_dangling_sum(NodeIDs, DanglingSum),
    single_iter(NodeIDs, PersonalizationVector, NNodes, DampingFactor, DanglingSum),

    format('Iteration ~d ---> Stopping Criterion = ~e \t[MaxIter=~d, Epsilon=~e]\n', [NIter, StopCrit, NMaxIter, Epsilon]),

    findall(SingleStopCrit, (node_pr_info(_, ROld, RNew, _), SingleStopCrit is abs(RNew - ROld)), StopCritList),
    sum_list(StopCritList, NewStopCrit),
    
    new_iteration_init(NodeIDs),
    NewNIter is NIter + 1,
    power_iter(NodeIDs, PersonalizationVector, NNodes, DampingFactor, NewNIter, NewStopCrit, Epsilon, NMaxIter).

power_iter(_, _, _, _, NIter, StopCrit, Epsilon, _) :-
    ((StopCrit < Epsilon) -> 
        format('\nConvergence reached in ~d iterations! Stopping criterion = ~e (< Epsilon=~e)\n', [NIter, StopCrit, Epsilon]);
        format('\nConvergence not reached in ~d iterations! Stopping criterion = ~e (>= Epsilon=~e)\n', [NIter, StopCrit, Epsilon])).


single_iter([NodeID|NodeIDs], [PersVal|PersonalizationVector], NNodes, DampingFactor, DanglingSum) :-
    findall(FromNodeID, arc(_, FromNodeID, NodeID), IngoingLinksList),
    sort(IngoingLinksList, IngoingLinks),  % to remove duplicates
    sum_pr_ingoing(IngoingLinks, NNodes, IngoingSum),
    NewRankVal is DampingFactor * (IngoingSum + PersVal * DanglingSum) + (1 - DampingFactor) * PersVal,
    retract(node_pr_info(NodeID, OldRankVal, _, NOutgoingLink)),
    assertz(node_pr_info(NodeID, OldRankVal, NewRankVal, NOutgoingLink)),
    single_iter(NodeIDs, PersonalizationVector, NNodes, DampingFactor, DanglingSum).

single_iter([], [], _, _, _).


compute_dangling_sum(NodeIDs, DanglingSum) :-
    compute_dangling_sum(NodeIDs, 0, DanglingSum).

compute_dangling_sum([], DanglingSum, DanglingSum).

compute_dangling_sum([NodeID|NodeIDs], Acc, DanglingSum) :-
    node_pr_info(NodeID, RankValue, _, _),
    \+arc(_, NodeID, _),
    !,
    NewAcc is Acc + RankValue,
    compute_dangling_sum(NodeIDs, NewAcc, DanglingSum).

compute_dangling_sum([_|NodeIDs], Acc, DanglingSum) :-
    compute_dangling_sum(NodeIDs, Acc, DanglingSum).


sum_pr_ingoing(IngoingLinks, NNodes, IngoingSum) :-
    sum_pr_ingoing(IngoingLinks, NNodes, 0, IngoingSum).

sum_pr_ingoing([], _, IngoingSum, IngoingSum).

sum_pr_ingoing([IngoingNode|IngoingNodes], NNodes, Acc, IngoingSum) :-
    node_pr_info(IngoingNode, OldRankVal, _, NOutgoingLink),
    NewAcc is Acc + (OldRankVal / NOutgoingLink),
    sum_pr_ingoing(IngoingNodes, NNodes, NewAcc, IngoingSum).


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
