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


readLines(ArgumentsList) :-
    current_input(InputStream),
    \+ at_end_of_stream(InputStream),
    !,
    read(Clause),
    disambiguateCase(Clause, ArgumentsList, NewArgumentsList),
    readLines(NewArgumentsList).

readLines([]).


% when node 
disambiguateCase(ClauseRead, ArgumentsList, ResultList) :-
    ClauseRead =.. [node, _PredicateId, OntologyName],
    atom_chars(OntologyName, [FirstLetter|_T]),
    char_type(FirstLetter, lower),
    !,
    append(ArgumentsList, [fromOntology-OntologyName], ResultList).

disambiguateCase(ClauseRead, ArgumentsList, ResultList) :-
    ClauseRead =.. [node, _PredicateId, TopLevelName],
    atom_chars(TopLevelName, [FirstLetter|_T]),
    char_type(FirstLetter, upper),
    !,
    append(ArgumentsList, [fromTopLevel-TopLevelName], ResultList).

disambiguateCase(ClauseRead, ArgumentsList, []) :-
    ClauseRead =.. [node_properties, PredicateId, Arguments],
    !,
    writeClause(PredicateId, ArgumentsList, Arguments).

disambiguateCase(ClauseRead, [], []) :-
    ClauseRead =.. [arc, PredicateId, PredicateName, SubjectId, ObjectId],
    !,
    ClauseToWrite =.. [PredicateName, PredicateId, SubjectId, ObjectId],
    portray_clause(ClauseToWrite).

disambiguateCase(ClauseRead, ArgumentsList, []).


writeClause(PredicateId, SideInfoGathered, ClauseAttributes) :-
    member(subClass-ClassName, ClauseAttributes),
    delete(ClauseAttributes, subClass-ClassName, CleanedClauseAttributes),
    fillMissing(ClassName, CleanedClauseAttributes, CompleteClauseAttributes),
    !,
    effectivelyWriteClause(PredicateId, ClassName, CompleteClauseAttributes, SideInfoGathered).

writeClause(PredicateId, SideInfoGathered, ClauseAttributes) :-
    format(user_output, '[WARNING] Predicate with id ~p was skipped, because subClass attribute was missing or it was not present in the schema.\n', PredicateId).


effectivelyWriteClause(PredicateId, ClassName, CompleteClauseAttributes, SideInfoGathered) :-
    member(fromTopLevel-TopLevelName, SideInfoGathered),
    !,
    TopLevelClause =.. [fromTopLevel, PredicateId, TopLevelName],
    portray_clause(TopLevelClause),

    findall(OntologyName, member(fromOntology-OntologyName, SideInfoGathered), OntologiesList),
    OntologyClause =.. [fromOntology, PredicateId, OntologiesList],
    portray_clause(OntologyClause),

    HighLevelClause =.. [ClassName, PredicateId|CompleteClauseAttributes],
    portray_clause(HighLevelClause).

effectivelyWriteClause(PredicateId, ClassName, CompleteClauseAttributes, SideInfoGathered) :-
    findall(OntologyName, member(fromOntology-OntologyName, SideInfoGathered), OntologiesList),
    OntologyClause =.. [fromOntology, PredicateId, OntologiesList],
    portray_clause(OntologyClause),

    HighLevelClause =.. [ClassName, PredicateId|CompleteClauseAttributes],
    portray_clause(HighLevelClause).


% for entities
fillMissing(PredicateName, PropertiesList, CompletePropertiesList) :-
    Clause =.. [PredicateName, AllPropertiesList],
    !,
    call(Clause),
    complete(PropertiesList, AllPropertiesList, CompletePropertiesList).

% for arcs
fillMissing(PredicateName, PropertiesList, CompletePropertiesList) :-
    Clause =.. [PredicateName, _SubjectObjectList, AllPropertiesList],
    !,
    call(Clause),
    complete(PropertiesList, AllPropertiesList, CompletePropertiesList).


complete(KeyValueList, [], []).

complete(KeyValueList, [PropKey|CompletePropertiesList], [PropValue|R]) :-
    member(PropKey-PropValue, KeyValueList),
    !,
    complete(KeyValueList, CompletePropertiesList, R).

complete(KeyValueList, [PropKey|CompletePropertiesList], ['null'|R]) :-
    complete(KeyValueList, CompletePropertiesList, R).


go :-
    % writeln('Please enter file name: '),
    % read(InstancesF),
    openFile('listexp_graph_small.pl'),
    % writeln('Please enter translated xml schema in prolog: '),
    % read(SchemaF),
    ensure_loaded('food_custom.pl'),
    readLines([]),
    closeFile.
