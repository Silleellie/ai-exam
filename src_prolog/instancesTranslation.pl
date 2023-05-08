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


readLines(InfoGatheredList) :-
    current_input(InputStream),
    \+ at_end_of_stream(InputStream),
    !,
    read(Clause),
    disambiguateCase(Clause, InfoGatheredList, NewInfoGatheredList),
    readLines(NewInfoGatheredList).

readLines([]).


% when node 
disambiguateCase(ClauseRead, InfoGatheredList, NewInfoGatheredList) :-
    ClauseRead =.. [node, _PredicateId, OntologyName],
    atom_chars(OntologyName, [FirstLetter|_T]),
    char_type(FirstLetter, lower),
    !,
    append(InfoGatheredList, [fromOntology-OntologyName], NewInfoGatheredList).

disambiguateCase(ClauseRead, InfoGatheredList, NewInfoGatheredList) :-
    ClauseRead =.. [node, _PredicateId, TopLevelName],
    atom_chars(TopLevelName, [FirstLetter|_T]),
    char_type(FirstLetter, upper),
    !,
    append(InfoGatheredList, [fromTopLevel-TopLevelName], NewInfoGatheredList).

disambiguateCase(ClauseRead, InfoGatheredList, []) :-
    ClauseRead =.. [node_properties, PredicateId, PredicateArguments],
    !,
    writeClause(PredicateId, InfoGatheredList, PredicateArguments).

disambiguateCase(ClauseRead, [], []) :-
    ClauseRead =.. [arc, PredicateId, PredicateName, SubjectId, ObjectId],
    !,
    ClauseToWrite =.. [PredicateName, PredicateId, SubjectId, ObjectId],
    portray_clause(ClauseToWrite).

disambiguateCase(ClauseRead, InfoGatheredList, []).


writeClause(PredicateId, SideInfoGathered, PredicateArguments) :-
    member(subClass-ClassName, PredicateArguments),
    delete(PredicateArguments, subClass-ClassName, CleanedPredicateArguments),
    fillMissing(ClassName, CleanedPredicateArguments, CompletePredicateArguments),
    !,
    effectivelyWriteClause(PredicateId, ClassName, CompletePredicateArguments, SideInfoGathered).

writeClause(PredicateId, SideInfoGathered, PredicateArguments).
    % format(user_output, '[WARNING] Predicate with id ~p was skipped, because subClass attribute was missing or it was not present in the schema.\n', PredicateId).


effectivelyWriteClause(PredicateId, ClassName, CompletePredicateArguments, SideInfoGathered) :-
    member(fromTopLevel-TopLevelName, SideInfoGathered),
    !,
    TopLevelClause =.. [fromTopLevel, PredicateId, TopLevelName],
    portray_clause(TopLevelClause),

    findall(OntologyName, member(fromOntology-OntologyName, SideInfoGathered), OntologiesList),
    OntologyClause =.. [fromOntology, PredicateId, OntologiesList],
    portray_clause(OntologyClause),

    HighLevelClause =.. [ClassName, PredicateId|CompletePredicateArguments],
    portray_clause(HighLevelClause).

effectivelyWriteClause(PredicateId, ClassName, CompletePredicateArguments, SideInfoGathered) :-
    findall(OntologyName, member(fromOntology-OntologyName, SideInfoGathered), OntologiesList),
    OntologyClause =.. [fromOntology, PredicateId, OntologiesList],
    portray_clause(OntologyClause),

    HighLevelClause =.. [ClassName, PredicateId|CompletePredicateArguments],
    portray_clause(HighLevelClause).


% for entities
fillMissing(PredicateName, PredicateArguments, CompleteArgumentsValues) :-
    Clause =.. [PredicateName, AllPredicateArguments],
    !,
    call(Clause),
    complete(PredicateArguments, AllPredicateArguments, CompleteArgumentsValues).

% for arcs
fillMissing(PredicateName, PredicateArguments, CompleteArgumentsValues) :-
    Clause =.. [PredicateName, _SubjectObjectList, AllPredicateArguments],
    !,
    call(Clause),
    complete(PredicateArguments, AllPredicateArguments, CompleteArgumentsValues).


complete(KeyValueList, [], []).

complete(KeyValueList, [PropKey|CompletePredicateArguments], [PropValue|R]) :-
    member(PropKey-PropValue, KeyValueList),
    !,
    complete(KeyValueList, CompletePredicateArguments, R).

complete(KeyValueList, [PropKey|CompletePredicateArguments], ['null'|R]) :-
    complete(KeyValueList, CompletePredicateArguments, R).


go :-
    % writeln('Please enter file name: '),
    % read(InstancesF),
    openFile('listexp_graph_small.pl'),
    % writeln('Please enter translated xml schema in prolog: '),
    % read(SchemaF),
    ensure_loaded('food_custom.pl'),
    readLines([]),
    closeFile.
