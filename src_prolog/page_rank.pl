:- module(page_rank, 
        [
            page_rank/0,
            page_rank/3,
            page_rank/5,
            rank/2,
            power_iter/6
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


%% page_rank
% 
% Simplified predicate which will call the 'page_rank/5' predicate with all arguments initialized as follows:
%   - DampingFactor set to 0.85;
%   - Epsilon set to 1e-6;
%   - MaxIter set to 100;
%   - RankStartVector set to empty list (Uniform starting distribution);
%   - PersonalizationVector set to empty list (Unform personalization distribution);

page_rank :-

    DampingFactor = 0.85,
    Epsilon = 0.000001,
    MaxIter = 100,
    RankStartVector = [],
    PersonalizationVector = [],

    page_rank(DampingFactor, Epsilon, MaxIter, RankStartVector, PersonalizationVector).


%% page_rank(+DampingFactor, +Epsilon, +MaxIter)
% 
% Simplified predicate which will call the 'page_rank/5' predicate with specified arguments + other initialized as follows:
%   - RankStartVector set to empty list (Uniform starting distribution);
%   - PersonalizationVector set to empty list (Unform personalization distribution);

page_rank(DampingFactor, Epsilon, MaxIter) :-
    
    RankStartVector = [],
    PersonalizationVector = [],

    page_rank(DampingFactor, Epsilon, MaxIter, RankStartVector, PersonalizationVector).


%% page_rank(+DampingFactor, +Epsilon, +MaxIter, +RankStartVector, +PersonalizationVector)
%
% This procedure performs the page rank algorithm on the graph defined from the original instances of Graph Brain (restructured in the
% list-based formalism).
% If RankStartVector is an empty list, inital page rank values for each node are set to 1/N, where N is the total number of nodes of the graph.
% Otherwise, inital rank values for each node could be specified in a key-value list format
%   - e.g. [0-0.5, 1-2.5, ...]
% Values of this list will be first normalized by their sum (in order to have all values in [0,1] range). For all nodes of the graph which
% are not present in the RankStartVector parameter, 0 as inital rank value will be assigned
%
% If the PersonalizationVector is an empty list, all personalization values are set to 1/N, where N is the total number of nodes 
% in the graph. This means that all nodes have same probability of being chosen during the teleport of the random surfer.
% Via the PersonalizationVector parameter, personalization values for each node could be specified in a key-value list format
%   - e.g. [0-1.3, 22-3.98, ...]
% Values of this list will be first normalized by their sum (in order to have all values in [0,1] range). For all nodes of the graph which
% are not present in the PersonalizationVector parameter, 0 as personalization value will be assigned
%
% The procedure loads automatically the list-based graph from 'outputs' directory. If more than one eligible list-based graph is available,
% the user is prompted to choose the appropriate one.
%
% The algorithm results can be checked by invoking the 'rank' procedure:
%   - e.g. rank(0, X) X is the page rank value for node with id 0
%
% Arguments:
%   DampingFactor: Damping parameter of Page Rank used to decide probability of jumping to random node or to follow outgoing links from a node;
%   Epsilon: Tolerance w.r.t. L1 norm to check if convergence has been reached;
%   MaxIter: Number of maximum iterations of the PageRank algorithm, after which the algorithm will stop regardless of having reached convergence or not;
%   RankStartVector: Vector of NodeID-Value pairs defining the values which will be used to initialize the rank vector;
%   PersonalizationVector: Vector of NodeID-Value pairs defining the personalization values of selected nodes.

page_rank(DampingFactor, Epsilon, MaxIter, RankStartVector, PersonalizationVector) :-

    select_file('list_graph', GraphFile),
    atom_concat('outputs/', GraphFile, GraphPath),
    write('\n***** Loading Graph instances file *****\n'),
    ensure_loaded(GraphPath),
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


%% rank(+NodeID, -RankVal)
%
% Predicate to call after calling the 'page_rank' predicate in order to check the page rank value of the nodes
% of the graph:
%   - e.g. rank(0, X) X is the rank value for node with id 0
%
% Arguments:
%   NodeID: Id of the node for which the activation value should be retrieved
%   RankVal: Rank value of the node with id NodeID

rank(NodeID, RankVal) :- node_pr_info(NodeID, _, RankVal, _).


% assert_init_all(+NodeIDs, +RankStartVector, +NormalizationValue)
%
% Initialize all 'node_pr_info' predicates for each node in the graph with their initial rank value and number of outlinks.
% If the RankStartVector is empty, inital rank value of each node will be set to 1 divided by the NormalizationValue.
% Otherwise, initial rank value of each node will be set to its value present in the RankStartVector list divided by the NormalizationValue.
% For nodes not appearing in the RankStartVector list, inital rank value will be set to 0.
%
% Arguments:
%   NodeIDs: List of identifiers for all nodes in the graph;
%   RankStartVector: Key-Value list containing the starting value for the PageRank ranking;
%   NormalizationValue: Value by which each RankValue of each node present as keys in RankStartVector list will be normalized.

assert_init_all([], _, _).

assert_init_all([NodeID|NodeIDs], RankStartVector, NormalizationValue) :-
    findall(ToNodeID, arc(_, NodeID, ToNodeID), OutgoingLinksList),
    sort(OutgoingLinksList, OutgoingLinks),  % to remove duplicates
    length(OutgoingLinks, OutLen),
    assert_init_single(NodeID, RankStartVector, NormalizationValue, OutLen),
    assert_init_all(NodeIDs, RankStartVector, NormalizationValue).


% assert_init_single(+NodeID, +RankStartVector, +NormalizationValue, +OutLen)
%
% Used to initialize a sigle node in the 'assert_init_all' procedure.
%
% Arguments:
%   NodeIDs: List of identifiers for all nodes in the graph;
%   RankStartVector: Vector containing the staring value for the PageRank ranking;
%   NormalizationValue: Value by which each initial Rank Value will be normalized;
%   OutLen: number of outgoing arcs from the node with NodeID.

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


% fill_personalization(+NodeIDs, +PersonalizationVector, +NormalizationValue, -FilledPersonalizationVector)
%
% Used to completely initialize the personalization vector so that it has a value for each node in the graph. 
% There are two different cases:
%   - The starting PersonalizationVector is empty, therefore uniform distribution will be applied to all personalization values;
%   - The starting PersonalizationVector contains values:
%       -- If NodeID X has an associated personalization value, the final value is given by PersonalizationValue (X) / NormalizationValue
%       -- Otherwise, the final personalization value is set to 0.
%
% Arguments:
%   NodeIDs: List of identifiers for all nodes in the graph;
%   PersonalizationVector: Vector of NodeID-Value pairs defining the personalization values of selected nodes;
%   NormalizationValue: Value by which each initial Personalization Value will be normalized;
%   FilledPersonalizationVector: Complete personalization vector with a value for each node.

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


% new_iteration_init(+NodeIDs)
%
% At the start of a new iteration, "new" page rank values become "old", and are thus copied in the second argument of 'node_pr_info' predicate.
%
% Arguments:
%   NodeIDs: List of identifiers for all nodes in the graph.

new_iteration_init([]).

new_iteration_init([NodeID|NodeIDs]) :-
    retract(node_pr_info(NodeID, _, Value, NOutgoingLink)),
    assertz(node_pr_info(NodeID, Value, Value, NOutgoingLink)),
    new_iteration_init(NodeIDs).


% power_iter(+NodeIDs, +PersonalizationVector, +NNodes, +DampingFactor, +Epsilon, +NMaxIter)
%
% Simplified predicate which will call the 'page_rank/8' predicate with specified arguments + other initialized as follows:
%   - NIter set to 0;
%   - StopCrit set to 1 (since at the first iteration the error committed is initialized to the maximum possible);

power_iter(NodeIDs, PersonalizationVector, NNodes, DampingFactor, Epsilon, NMaxIter) :-
    power_iter(NodeIDs, PersonalizationVector, NNodes, DampingFactor, 0, 1, Epsilon, NMaxIter).


% power_iter(+NodeIDs, +PersonalizationVector, +NNodes, +DampingFactor, +NIter, +StopCrit, +Epsilon, +NMaxIter)
%
% Performs a recursive iteration of the page rank algorithm. Computes a single page rank iteration (Rank Vector multipled to 
% web matrix M) and checks if the stopping criterion with L1 norm has been reached (L1 norm less than Epsilon).
%
% To take into account Dangling Nodes, the algorithm also computes the sum of their rank, which 
% will be used as a normalization factor during computation of the rank values for the other nodes.
%
% Once the L1 norm is lower than the Epsilon, the algorithm will stop and a message will be displayed to show the L1 norm.
% If the number of iterations is greater than the number of maximum iterations, the algorithm will stop regardless of convergence.
%
% Arguments:
%   NodeIDs: All node ids in the graph;
%   PersonalizationVector: Complete personalization vector with a value for each node;
%   NNodes: Total number of nodes in the graph;
%   DampingFactor: Damping parameter of Page Rank used to decide probability of jumping to a random node or to follow outgoing links from a node;
%   NIter: Current iteration number. It is initialized to 0;
%   StopCrit: L1 norm associated with the previous iteration. It is initialized to 1;
%   Epsilon: Threshold value, the algorithm will stop once StopCrit is lower than this value;
%   NMaxIter: Maximum number of iterations after which the algorithm will stop regardless of convergence.

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


% single_iter(+NodeIDs, +PersonalizationVector, +NNodes, +DampingFactor, +DanglingSum)
%
% Performs a single iteration of the page rank algorithm, computes the page rank vector and updates the vlaues in the 'node_pr_info' predicate.
% Also note that the graph will be converted to a Directed Graph if it is a Multi Graph (more than one link connecting same subject-object nodes).
%
% Arguments:
%   NodeIDs: All node ids in the graph;
%   PersonalizationVector: Complete personalization vector with a value for each node;
%   NNodes: Total number of nodes in the graph;
%   DampingFactor: Damping parameter of Page Rank used to decide probability of jumping to random node or to follow outgoing links from a node;
%   DanglingSum: Summatory of the rank values of the dangling nodes in the graph.

single_iter([NodeID|NodeIDs], [PersVal|PersonalizationVector], NNodes, DampingFactor, DanglingSum) :-
    findall(FromNodeID, arc(_, FromNodeID, NodeID), IngoingLinksList),
    sort(IngoingLinksList, IngoingLinks),  % to remove duplicates
    sum_pr_ingoing(IngoingLinks, NNodes, IngoingSum),
    NewRankVal is DampingFactor * (IngoingSum + PersVal * DanglingSum) + (1 - DampingFactor) * PersVal,
    retract(node_pr_info(NodeID, OldRankVal, _, NOutgoingLink)),
    assertz(node_pr_info(NodeID, OldRankVal, NewRankVal, NOutgoingLink)),
    single_iter(NodeIDs, PersonalizationVector, NNodes, DampingFactor, DanglingSum).

single_iter([], [], _, _, _).


% compute_dangling_sum(+NodeIDs, -DanglingSum)
%
% Computes the summatory of the rank values for the dangling nodes, this value will be used during 
% page rank computation as a normalization value.
%
% Arguments:
%   NodeIDs: All node ids in the graph;
%   DanglingSum: Summatory of the rank values of the dangling nodes in the graph.

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


% sum_pr_ingoing(+IngoingLinks, +NNodes, -IngoingSum)
%
% Given the identifiers of all nodes having an outgoing link pointing to a specific node, computes the ingoing sum of that specific node
% by summing the rank value of each node pointing to it normalized by the number of their outgoing links.
%
% Arguments:
%   NodeIDs: All node ids in the graph;
%   NNodes: Total number of nodes in the graph;
%   DanglingSum: Summatory of the rank values of the dangling nodes in the graph.

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
