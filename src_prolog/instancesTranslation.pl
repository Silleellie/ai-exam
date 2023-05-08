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
    append(ArgumentsList, [fromOntology-OntologyName], ResultList),
    !.

disambiguateCase(ClauseRead, ArgumentsList, ResultList) :-
    ClauseRead =.. [node, _PredicateId, TopLevelName],
    atom_chars(TopLevelName, [FirstLetter|_T]),
    char_type(FirstLetter, upper),
    append(ArgumentsList, [fromTopLevel-TopLevelName], ResultList),
    !.

disambiguateCase(ClauseRead, ArgumentsList, []) :-
    ClauseRead =.. [node_properties, PredicateId, Arguments],
    !,
    writeHighLevelClause(PredicateId, Arguments),
    writeSideInfo(PredicateId, ArgumentsList).  % inverted order for now because if predicate isn't in schema we don't write side infos

disambiguateCase(ClauseRead, [], []) :-
    ClauseRead =.. [arc, PredicateId, PredicateName, SubjectId, ObjectId],
    !,
    ClauseToWrite =.. [PredicateName, PredicateId, SubjectId, ObjectId],
    portray_clause(ClauseToWrite).

disambiguateCase(ClauseRead, ArgumentsList, []).

% 0, [fromOntology-retrocomputing, fromOntology-general, fromTopLevel-Person]
writeSideInfo(PredicateId, SideInfoArgumentsList) :-
    findall(OntologyName, member(fromOntology-OntologyName, SideInfoArgumentsList), OntologiesList),
    OntologyClause =.. [fromOntology, PredicateId, OntologiesList],
    portray_clause(OntologyClause),
    member(fromTopLevel-TopLevelName, SideInfoArgumentsList),
    TopLevelClause =.. [fromTopLevel, PredicateId, TopLevelName],
    portray_clause(TopLevelClause).


% 0, [deathDate-08/04/2012,name-Jack,gender-M,subClass-Person,birthDate-13/12/1928,surname-Tramiel]
writeHighLevelClause(PredicateId, PropertiesList) :-
    member(subClass-UpperPredicateName, PropertiesList),
    delete(PropertiesList, subClass-UpperPredicateName, CleanedPropertiesList),
    % downcase_atom(UpperPredicateName, PredicateName),
    fillMissing(UpperPredicateName, CleanedPropertiesList, CompleteValuesList),
    !, % predicate exist in schema, so we don't backtrack to skip it
    Clause =.. [UpperPredicateName, PredicateId|CompleteValuesList],
    portray_clause(Clause).

% when predicate doesn't exist in schema, we skip it
writeHighLevelClause(PredicateId, PropertiesList).


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
