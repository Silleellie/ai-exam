:- ensure_loaded(library(lists)).
% :- use_module(library(listing)).
% :- set_prolog_flag(unknown, fail).

openFile(FilePath) :-
    open(FilePath, 'read', Stream),
    open('data/processed/high_level_translation.pl', 'write', NewStream),
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
    append(InfoGatheredList, [instanceDomains-OntologyName], NewInfoGatheredList).

disambiguateCase(ClauseRead, InfoGatheredList, NewInfoGatheredList) :-
    ClauseRead =.. [node, _PredicateId, TopLevelName],
    atom_chars(TopLevelName, [FirstLetter|_T]),
    char_type(FirstLetter, upper),
    !,
    append(InfoGatheredList, [topLevelClass-TopLevelName], NewInfoGatheredList).

disambiguateCase(ClauseRead, InfoGatheredList, []) :-
    ClauseRead =.. [node_properties, PredicateId, PredicateArguments],
    !,
    writeClause(PredicateId, InfoGatheredList, PredicateArguments).

disambiguateCase(ClauseRead, [], [subjectId-SubjectId, objectId-ObjectId]) :-
    ClauseRead =.. [arc, PredicateId, SubjectId, ObjectId],
    !.

disambiguateCase(ClauseRead, InfoGatheredList, []) :-
    ClauseRead =.. [arc_properties, PredicateId, PredicateArguments],
    !,
    writeClause(PredicateId, InfoGatheredList, PredicateArguments).

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
    member(topLevelClass-TopLevelName, SideInfoGathered),
    !,
    TopLevelClause =.. [topLevelClass, PredicateId, TopLevelName],
    portray_clause(TopLevelClause),

    findall(OntologyName, member(instanceDomains-OntologyName, SideInfoGathered), OntologiesList),
    OntologyClause =.. [instanceDomains, PredicateId, OntologiesList],
    portray_clause(OntologyClause),

    HighLevelClause =.. [ClassName, PredicateId|CompletePredicateArguments],
    portray_clause(HighLevelClause).

effectivelyWriteClause(PredicateId, ClassName, CompletePredicateArguments, SideInfoGathered) :-
    member(subjectId-SubjectId, SideInfoGathered),
    member(objectId-ObjectId, SideInfoGathered),
    !,
    HighLevelClause =.. [ClassName, PredicateId, SubjectId, ObjectId|CompletePredicateArguments],
    portray_clause(HighLevelClause).

effectivelyWriteClause(PredicateId, ClassName, CompletePredicateArguments, SideInfoGathered) :-
    findall(OntologyName, member(instanceDomains-OntologyName, SideInfoGathered), OntologiesList),
    OntologyClause =.. [instanceDomains, PredicateId, OntologiesList],
    portray_clause(OntologyClause),
    
    HighLevelClause =.. [ClassName, PredicateId|CompletePredicateArguments],
    portray_clause(HighLevelClause).


fillMissing(PredicateName, PredicateArguments, CompleteArgumentsValues) :-
    gather_attributes(PredicateName, AllPredicateArguments),
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
    openFile('data/processed/listexportedGraph.pl'),
    % writeln('Please enter translated xml schema in prolog: '),
    % read(SchemaF),
    ensure_loaded('data/processed/retrocomputing.pl'),
    readLines([]),
    closeFile.
