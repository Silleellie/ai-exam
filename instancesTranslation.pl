:- ensure_loaded(library(lists)).

openFile(FilePath) :-
    open(FilePath, 'read', Stream),
    open('test.pl', 'write', NewStream),
    set_input(Stream),
    set_output(NewStream).

closeFile :-
    current_input(InputStream),
    current_output(OutputStream),
    close(InputStream),
    close(OutputStream),
    set_input(user_input),
    set_output(user_output).

readLines([]) :-
    current_input(InputStream),
    at_end_of_stream(InputStream).

% readLines(ArgumentsList) :-
%     current_input(InputStream),
%     \+ at_end_of_stream(InputStream),
%     read(Clause),
%     Clause =.. [node, _PredicateId|Arguments],
%     append(ArgumentsList, Arguments, ConcatenatedList).
    % readLines(ConcatenatedList).

readLines(ArgumentsList) :-
    current_input(InputStream),
    \+ at_end_of_stream(InputStream),
    read(Clause),
    Clause =.. [node_properties, PredicateId|Arguments] ,
    append(ArgumentsList, Arguments, ConcatenatedList),
    writeHighLevelClause(PredicateId, ConcatenatedList).
    % readLines([]).

% 0, ['Person', 'retrocomputing', [deathDate-'08/04/2012', name-'Jack', gender-'M', subClass-'Person', birthDate-'13/12/1928', surname-'Tramiel']]
writeHighLevelClause(PredicateId, ConcatenatedList) :-
    last(ConcatenatedList, PropertiesList),
    member(subClass-UpperPredicateName, PropertiesList),
    delete(PropertiesList, subClass-UpperPredicateName, CleanedPropertiesList),
    downcase_atom(UpperPredicateName, PredicateName),
    findall(AttributeValue, member(_AttributeKey-AttributeValue, CleanedPropertiesList), AttributeValueList),
    Clause =.. [PredicateName|AttributeValueList],
    portray_clause(Clause).

go :-
    % writeln('Please enter file name: '),
    % read(F),
    openFile('exp_graph_extrasmall.pl'),
    readLines([]),
    closeFile.
