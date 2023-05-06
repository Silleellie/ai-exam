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
    writeSideInfo(PredicateId, ArgumentsList),
    writeHighLevelClause(PredicateId, Arguments).


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
