:- use_module(library(lists)).
:- use_module(library(apply_macros)).
:- use_module(library(yall)).
% :- use_module(library(lambda)).
% :- ensure_loaded(listexportedGraph).



% page_rank_power_iter()

% # build_column_m(RankValue, ColumnIndex, OutgoingLinks, [], []).

% # build_column_m(RankValue, ColumnIndex, OutgoingLinks, [NodeId|NodeIds], [[RowIndex, ColumnIndex, RankValue]|ColumnM]) :-
% #     member(NodeId, OutgoingLinks),
% #     !,
% #     length(NodeIds, RowIndex),
% #     build_column_m(RankValue, ColumnIndex, OutgoingLinks, NodeIds, ColumnM).

% # build_column_m(RankValue, ColumnIndex, OutgoingLinks, [NodeId|NodeIds], ColumnM) :-
% #     build_column_m(RankValue, ColumnIndex, OutgoingLinks, NodeIds, ColumnM).


% # build_matrix_m(N, AllNodeIds, [], []).

% # build_matrix_m(N, AllNodeIds, [NodeId|NodeIds], [MColumn|MColumns]) :-
% #     retrieve_outgoing_list(NodeId-OutgoingLinks),
% #     length(OutgoingLinks, NodeAriety),
% #     NodeAriety \== 0,
% #     !,
% #     Mj is 1 / NodeAriety,
% #     length(MColumns, ColumnIndex),
% #     build_column_m(Mj, ColumnIndex, OutgoingLinks, AllNodeIds, MColumn),
% #     build_matrix_m(N, AllNodeIds, NodeIds, MColumns).

% # build_matrix_m(N, AllNodeIds, [NodeId|NodeIds], [MColumn|MColumns]) :-
% #     length(MColumns, ColumnIndex),
% #     findall([RowIndex, ColumnIndex, 0], between(1, M, RowIndex), MColumn),
% #     build_matrix_m(N, AllNodeIds, NodeIds, MColumns).


build_row_m(NColumns, OutgoingLinks, [(NodeId, ColumnValue)|ColumnValues], [(ColumnIndex, ColumnValue)|RowM]) :-
    ColumnValue \== 0,  % ariety of NodeId is not 0
    member(NodeId, OutgoingLinks),  % The row we are building does not have an outgoing link to NodeId
    !,
    length(ColumnValues, NNodeIdsToProcess),
    ColumnIndex is NColumns - NNodeIdsToProcess - 1, % Column index should start from 0 so remove 1 from total length
    build_row_m(NColumns, OutgoingLinks, ColumnValues, RowM).

build_row_m(NColumns, OutgoingLinks, [(NodeId, ColumnValue)|ColumnValues], RowM) :-
    build_row_m(NColumns, OutgoingLinks, ColumnValues, RowM).

build_row_m(NColumns, OutgoingLinks, [], []).


build_column_values([NodeId|NodeIds], [(NodeId, ColumnValue)|ColumnValues]) :-
    retrieve_outgoing_list(NodeId-OutgoingLinks),
    length(OutgoingLinks, NodeAriety),
    NodeAriety \== 0,
    !,
    ColumnValue is 1 / NodeAriety,
    build_column_values(NodeIds, ColumnValues).

build_column_values([NodeId|NodeIds], [(NodeId, 0)|ColumnValues]) :-
    build_column_values(NodeIds, ColumnValues).

build_column_values([], []).


build_m_by_row(NColumns, ColumnValues, [NodeId|NodeIds], [RowM|Matrix]) :-
    retrieve_outgoing_list(NodeId-OutgoingLinks),
    build_row_m(NColumns, OutgoingLinks, ColumnValues, RowM),
    !,
    build_m_by_row(NColumns, ColumnValues, NodeIds, Matrix).

build_m_by_row(NColumns, ColumnValues, [], []).


build_matrix_m(NColumns, ColumnValues, NodeIds, Matrix) :-
    write('Build column values\n'),
    build_column_values(NodeIds, ColumnValues),
    write('Build m by row\n'),
    build_m_by_row(NColumns, ColumnValues, NodeIds, Matrix).


% multiply_m_row_to_r([(ColumnIndex, MElement)|MRowElements], OldR, NewRElement) :-
%     nth0(ColumnIndex, OldR, RElement),
%     NewRElement is NewRElementToAdd + MElement * RElement,
%     write(NewRElement),
%     multiply_m_row_to_r(MRowElements, OldR, NewRElement).

% multiply_m_row_to_r([], OldR, 0).





m_row_times_r(OldR, (ColumnIndex, MElement), Result) :-
    nth0(ColumnIndex, OldR, RElement),
    Result is RElement * MElement.


multiply_m_row_to_r(MRow, R, ScalarResult) :-
    maplist(m_row_times_r(R), MRow, MultiplicationVector),
    sumlist(MultiplicationVector, ScalarResult).



% maplist(\(ColumnIndex, MRowElement)^(MRowElement * ),MRow).

multiply_m_r(Beta, [MRow|MRows], R, [NewRRowElementBeta|NewR]) :-
    !,
    multiply_m_row_to_r(MRow, R, NewRRowElement),
    % sumlist(NewRRow, NewRRowSum),
    NewRRowElementBeta is NewRRowElement * Beta,
    multiply_m_r(Beta, MRows, R, NewR).
    
multiply_m_r(Beta, [], R, []).


power_iter(Matrix, OldR, N, Beta, Epsilon, NIter, NIterMax, SuperFinalR) :-
    multiply_m_r(Beta, Matrix, OldR, NewR),
    RandomTeleport is (1 - Beta) / N,
    % maplist(\X^Y^(Y is X + RandomTeleport), NewR, FinalNewR).
    maplist({RandomTeleport}/[X, Y]>>(Y is X + RandomTeleport), NewR, FinalNewR),
    % maplist(\X^Y^Z^(Z is abs(X-Y)), NewR, OldR, Final).
    maplist([X, Y, Z]>>(Z is abs(X-Y)), NewR, OldR, SubtractionVector),
    sumlist(SubtractionVector, ErrorCommitted),
    SuccNIter is NIter + 1,
    ( (SuccNIter < NIterMax, Epsilon =< ErrorCommitted) ->
        power_iter(Matrix, NewR, N, Beta, Epsilon, SuccNIter, NIterMax, SuperFinalR) ;
        SuperFinalR = NewR ).


page_rank(Beta, Epsilon, NIterMax, NewR) :-
    write('Finding all nodes\n'),
    time(findall(X, node_properties(X, _), NodeIds)),
    length(NodeIds, N),
    write('Building uniform R\n'),
    time(findall(1/N, between(1, N, _), R)),
    write('Building M!\n'),
    time(build_matrix_m(N, ColumnValues, NodeIds, Matrix)),
    write('Computing PageRank'),
    time(power_iter(Matrix, R, N, Beta, Epsilon, 0, NIterMax, NewR)).
    
    
% [0->[1, 2, 3], 1->[]]
% [0<-[4,5],]

retrieve_outgoing_list(NodeId-OutgoingLinks) :-
    node_properties(NodeId, _),
    findall(ToNodeId, arc(ArcId, NodeId, ToNodeId), OutgoingLinks).


node(0, 'Person').
node(0, 'retrocomputing').
node_properties(0, ['deathDate'-'08/04/2012', 'name'-'Jack', 'gender'-'M', 'subClass'-'Person', 'birthDate'-'13/12/1928', 'surname'-'Tramiel']).
node(1, 'retrocomputing').
node(1, 'Organization').
node_properties(1, ['name'-'Commodore Business Machines', 'closingDate'-'29/04/1994', 'subClass'-'Organization', 'acronym'-'CBM']).
node(2, 'Person').
node(2, 'retrocomputing').
node_properties(2, ['diedIn'-'335869', 'bornIn'-'335215', 'gender'-'M', 'nationality'-'usa', 'subClass'-'Person', 'surname'-'Peddle', 'deathDate'-'15/12/2019', 'nickname'-'Chuck', 'name'-'Charles Ingerham', 'title'-'Eng', 'birthDate'-'25/11/1937']).


arc(48187, 0, 0).
arc_properties(48187, ['subClass'-'expresses']).
arc(48185, 0, 1).
arc_properties(48185, ['subClass'-'expresses']).
arc(343416, 1, 0).
arc_properties(343416, ['subClass'-'similarTo']).
arc(209143, 1, 2).
arc_properties(209143, ['subClass'-'expresses']).
arc(86995, 2, 1).
arc_properties(86995, ['subClass'-'expresses']).