:- module(high_level_translator,
        [
            translate/0,
            translate/2
        ]).

:- use_module(library(lists)).
:- use_module(utils).
:- unknown(_, fail).


translate :-
    select_file('list_graph', InstancesF),
    select_file('schema', SchemaF),
    atom_concat('outputs/', InstancesF, InstancesPath),
    atom_concat('outputs/', SchemaF, SchemaPath),

    translate(InstancesPath, SchemaPath).

translate(InstancesF, SchemaF) :-
    open_files(InstancesF),
    ensure_loaded(SchemaF),
    translate_lines,
    close_files,
    absolute_file_name('outputs/high_level_translation.pl', AbsoluteOutput),
    format(user_output, '\nHigh level KB saved into ~p!\n', [AbsoluteOutput]).


open_files(FilePath) :-
    open(FilePath, 'read', Stream),
    open('outputs/high_level_translation.pl', 'write', NewStream),
    set_input(Stream),
    set_output(NewStream).


close_files :-
    current_input(InputStream),
    current_output(OutputStream),
    close(InputStream),
    close(OutputStream),
    set_input(user_input),
    set_output(user_output).


translate_lines :-
    translate_lines([], 0).

translate_lines(InfoGatheredList, Acc) :-
    current_input(InputStream),
    \+ at_end_of_stream(InputStream),
    !,
    Rest is Acc mod 100000,
    ((Acc =\= 0, Rest =:= 0) -> format(user_output, 'Processed ~d lines...\n', [Acc]); true),
    NewAcc is Acc + 1,
    read(Clause),
    disambiguate_case(Clause, InfoGatheredList, NewInfoGatheredList),
    translate_lines(NewInfoGatheredList, NewAcc).

translate_lines([], Acc).


% when node 
disambiguate_case(ClauseRead, InfoGatheredList, NewInfoGatheredList) :-
    ClauseRead =.. [node, _PredicateId, OntologyName],
    atom_chars(OntologyName, [FirstLetter|_T]),
    char_type(FirstLetter, lower),
    !,
    append(InfoGatheredList, [instanceDomains-OntologyName], NewInfoGatheredList).

disambiguate_case(ClauseRead, InfoGatheredList, NewInfoGatheredList) :-
    ClauseRead =.. [node, _PredicateId, TopLevelName],
    atom_chars(TopLevelName, [FirstLetter|_T]),
    char_type(FirstLetter, upper),
    !,
    append(InfoGatheredList, [topLevelClass-TopLevelName], NewInfoGatheredList).

disambiguate_case(ClauseRead, InfoGatheredList, []) :-
    ClauseRead =.. [node_properties, PredicateId, PredicateArguments],
    !,
    prepare_clause(PredicateId, InfoGatheredList, PredicateArguments).

disambiguate_case(ClauseRead, [], [subjectId-SubjectId, objectId-ObjectId]) :-
    ClauseRead =.. [arc, _, SubjectId, ObjectId],
    !.

disambiguate_case(ClauseRead, InfoGatheredList, []) :-
    ClauseRead =.. [arc_properties, PredicateId, PredicateArguments],
    !,
    prepare_clause(PredicateId, InfoGatheredList, PredicateArguments).

disambiguate_case(_, _, []).


prepare_clause(PredicateId, SideInfoGathered, PredicateArguments) :-
    member(subClass-ClassName, PredicateArguments),
    delete(PredicateArguments, subClass-ClassName, CleanedPredicateArguments),
    fill_missing(ClassName, CleanedPredicateArguments, CompletePredicateArguments),
    !,
    write_clause(PredicateId, ClassName, CompletePredicateArguments, SideInfoGathered).

prepare_clause(PredicateId, _, _).
    % format(user_output, '[WARNING] Predicate with id ~p was skipped, because subClass attribute was missing or it was not present in the schema.\n', [PredicateId]).


fill_missing(PredicateName, PredicateArguments, CompleteArgumentsValues) :-
    gather_attributes(PredicateName, AllPredicateArguments),
    complete(PredicateArguments, AllPredicateArguments, CompleteArgumentsValues).


complete(_, [], []).

complete(KeyValueList, [PropKey|CompletePredicateArguments], [PropValue|R]) :-
    member(PropKey-PropValue, KeyValueList),
    !,
    complete(KeyValueList, CompletePredicateArguments, R).

complete(KeyValueList, [_|CompletePredicateArguments], ['null'|R]) :-
    complete(KeyValueList, CompletePredicateArguments, R).


write_clause(PredicateId, ClassName, CompletePredicateArguments, SideInfoGathered) :-
    member(topLevelClass-TopLevelName, SideInfoGathered),
    !,
    TopLevelClause =.. [topLevelClass, PredicateId, TopLevelName],
    portray_clause(TopLevelClause),

    findall(OntologyName, member(instanceDomains-OntologyName, SideInfoGathered), OntologiesList),
    OntologyClause =.. [instanceDomains, PredicateId, OntologiesList],
    portray_clause(OntologyClause),

    HighLevelClause =.. [ClassName, PredicateId|CompletePredicateArguments],
    portray_clause(HighLevelClause).

write_clause(PredicateId, ClassName, CompletePredicateArguments, SideInfoGathered) :-
    member(subjectId-SubjectId, SideInfoGathered),
    member(objectId-ObjectId, SideInfoGathered),
    !,
    HighLevelClause =.. [ClassName, PredicateId, SubjectId, ObjectId|CompletePredicateArguments],
    portray_clause(HighLevelClause).

write_clause(PredicateId, ClassName, CompletePredicateArguments, SideInfoGathered) :-
    findall(OntologyName, member(instanceDomains-OntologyName, SideInfoGathered), OntologiesList),
    OntologyClause =.. [instanceDomains, PredicateId, OntologiesList],
    portray_clause(OntologyClause),
    
    HighLevelClause =.. [ClassName, PredicateId|CompletePredicateArguments],
    portray_clause(HighLevelClause).
