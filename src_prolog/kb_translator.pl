:- module(high_level_translator,
        [
            translate/0,
            translate/2
        ]).

:- use_module(library(lists)).
:- use_module(utils).
:- unknown(_, fail).


%% translate
%
% It will convert the knowledge base, restructured in the list based formalism after it has been processed from the 'KBRestructurer' 
% Java class, into a higher-level formalism. 
% In order to do so, a schema describing the instances attributes must be provided to check consistency between
% their values and their formal definition. Also the XML schema must be processed through the appropriate Java class ('SchemaToProlog')
% before calling this predicate.
%
% Both the restructured knowledge base and the GraphBrain schema converted into Prolog rules must be present in the 'outputs' folder,
% when more than one possible kb/schema is available, the user will be prompted to choose the appropriate one.

translate :-
    select_file('list_graph', InstancesF),
    select_file('schema', SchemaF),
    atom_concat('outputs/', InstancesF, InstancesPath),
    atom_concat('outputs/', SchemaF, SchemaPath),

    translate(InstancesPath, SchemaPath).

%% translate(+InstancesF, +SchemaF)
%
% Same functioning as translate/0, but takes as arguments explicitly the list-based instances and prolog schema file paths 
% instead of loading them automatically from the 'outputs' folder

translate(InstancesF, SchemaF) :-
    open_files(InstancesF),
    ensure_loaded(SchemaF),
    translate_lines,
    close_files,
    absolute_file_name('outputs/high_level_translation.pl', AbsoluteOutput),
    format(user_output, '\nHigh level KB saved into ~p!\n', [AbsoluteOutput]).


% open_files(+FilePath)
%
% Used to open the files which will be manipulated by the 'translate' predicate during execution.
%
% The input FilePath represents the path where the file containing the instances to convert is stored.
% This file will be read from the program and a new 'high_level_translation.pl' file will be created in the 'outputs'
% folder in order to write the converted instances.
%
% Input and output streams of the program will be redirected to these files accordingly.
%
% Arguments:
%   FilePath: path where the file containing the instances to convert into the higher formalism is stored

open_files(FilePath) :-
    open(FilePath, 'read', Stream),
    open('outputs/high_level_translation.pl', 'write', NewStream),
    set_input(Stream),
    set_output(NewStream).


% close_files
%
% Used to close the files manipulated by the 'translate' predicate during execution.
% Input and output stream are redirected to standard input and standard output respectively.

close_files :-
    current_input(InputStream),
    current_output(OutputStream),
    close(InputStream),
    close(OutputStream),
    set_input(user_input),
    set_output(user_output).


% translate_lines
%
% Dummy predicate which will call the 'translate_lines/2' predicate with default arguments.

translate_lines :-
    translate_lines([], 0).

% translate_lines(+InfoGatheredList, +Acc)
%
% Recursive predicate which will read a line from the redirected Input stream and write it to the redirected Output stream as a clause
% adhering to the higher level formalism.
%
% InfoGatheredList is a list which will be filled and emptied after each entity/arc has been processed.
% - It will be filled with side information about the current entity/arc to process (instance domains associated, top level class associated);
% - It will be emptied as soon as the current entity/arc is written to the redirected output, ready to host side information of the new
%   entity/arc
%
% Arguments:
%   InfoGatheredList: List containing side information on a particular entity or relationship which still has to be written (to be initialized empty);
%   Acc: Number of processed lines. It is used to show the progress of the process. It should be initialized to 0.

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


% disambiguate_case(+ClauseRead, +InfoGatheredList, -NewInfoGatheredList)
%
% Understands which type of clause is being read from the input file and processes it accordingly.
% There are five different cases in total:
%
%   - Clause regarding an instance domain to which an entity is associated          [node(EntityID, InstanceDomain)]
%   - Clause regarding the top level class associated to an entity                  [node(EntityID, TopLevelClass)]
%   - Clause regarding the attributes of an entity                                  [node_properties(EntityID, PropertiesList)]
%   - Clause regarding the subject and object of a relationship                     [arc(ArcID, SubjectID, ObjectID)]
%   - Clause regarding the attributes of a relationship                             [arc_properties(ArcID, PropertiesList)]
%
% Additionally, if a clause cannot be associated to any of these cases, it is considered unknown and it will be skipped.
%
% Arguments:
%   ClauseRead: The clause read from the input file;
%   InfoGatheredList: List containing side information on a particular entity or relationship which still has to be written;
%   NewInfoGatheredList: List containing the updated side information for the entity/arc which is being processed
%     - It is the empty list when the entity/arc has been fully processed and written to redirected output.

disambiguate_case(ClauseRead, InfoGatheredList, NewInfoGatheredList) :-
    ClauseRead =.. [node, _PredicateId, OntologyName],  % node(EntityID, InstanceDomain)
    atom_chars(OntologyName, [FirstLetter|_T]),
    char_type(FirstLetter, lower),
    !,
    append(InfoGatheredList, [instanceDomains-OntologyName], NewInfoGatheredList).

disambiguate_case(ClauseRead, InfoGatheredList, NewInfoGatheredList) :-
    ClauseRead =.. [node, _PredicateId, TopLevelName],  % node(EntityID, TopLevelClass)
    atom_chars(TopLevelName, [FirstLetter|_T]),
    char_type(FirstLetter, upper),
    !,
    append(InfoGatheredList, [topLevelClass-TopLevelName], NewInfoGatheredList).

disambiguate_case(ClauseRead, InfoGatheredList, []) :-
    ClauseRead =.. [node_properties, PredicateId, PredicateArguments],  % node_properties(EntityID, PropertiesList)
    !,
    prepare_clause(PredicateId, InfoGatheredList, PredicateArguments).

disambiguate_case(ClauseRead, [], [subjectId-SubjectId, objectId-ObjectId]) :-
    ClauseRead =.. [arc, _, SubjectId, ObjectId],  % arc(ArcID, SubjectID, ObjectID)
    !.

disambiguate_case(ClauseRead, InfoGatheredList, []) :-
    ClauseRead =.. [arc_properties, PredicateId, PredicateArguments],  % arc_properties(ArcID, PropertiesList)
    !,
    prepare_clause(PredicateId, InfoGatheredList, PredicateArguments).

disambiguate_case(_, _, []).  % unknown clause


% prepare_clause(+PredicateId, +SideInfoGathered, +PredicateArguments)
%
% Performs the operations needed to transform a clause from the Java list representation to the higher lever formalism and
% writes it to the redirected output stream. In particular, the operations are the following:
%
%   1. Pop the Entity/Arc Class Name from the PredicateArguments list ('subClass' property)
%   2. Fill attributes which appear in the Entity/Arc Class schema but not in the PredicateArguments list with 'null' values
%
% Once these operatons are completed successfully, the clause is written to file, otherwise, if any operation fails, the clause is skipped.
%
% Arguments:
%   PredicateId: The identifier associated with the Entity/Arc;
%   SideInfoGathered: List containing additional information related to the Entity/Arc which is being processed (Top Level Class Name,
%     Instance Domains);
%   PredicateArguments: List containing the Entity/Arc instance attributes in the key-value format (attributeName-attributeValue).

prepare_clause(PredicateId, SideInfoGathered, PredicateArguments) :-
    member(subClass-ClassName, PredicateArguments),
    delete(PredicateArguments, subClass-ClassName, CleanedPredicateArguments),
    fill_missing(ClassName, CleanedPredicateArguments, CompletePredicateArguments),
    !,
    write_clause(PredicateId, ClassName, CompletePredicateArguments, SideInfoGathered).

prepare_clause(PredicateId, _, _).


% fill_missing(+PredicateName, +KeyValueArguments, -CompleteValueArguments)
%
% Given a list of key-value attributes for a given predicate, retrieves the complete list of attribute names for the Entity/Arc from the schema,
% and completes it adding 'null' for any attribute name present in the schema but not as key in the KeyValueArguments parameter.
% The order of the attributes for the final list will respect the order of the attributes specified for the Entity/Arc in the schema.
%
% Arguments:
%   PredicateName: The name of the predicate for which attributes will be retrieved from the schema;
%   KeyValueArguments: Key-value list containing the attribute name and related value of the predicate which is being processed;
%   CompleteValueArguments: List containing completed values from KeyValue Arguments parameter + 'null' for each
%     attribute present in the schema but missing from KeyValueArguments.

fill_missing(PredicateName, KeyValueArguments, CompleteValueArguments) :-
    gather_attributes(PredicateName, CompleteKeyArguments),
    complete(KeyValueArguments, CompleteKeyArguments, CompleteValueArguments).


% complete(+KeyValueArguments, +CompleteKeyArguments, -CompleteValueArguments)
%
% Given the key-value list of attributes specified for an instance and the complete set of attribute names retrieved from the schema,
% it will return a list containing all values which are expected to be present for that particular instance according to the schema.
% 'null' values will be inserted for any missing attribute in the KeyValueArguments list.
% The order of the attributes values in CompleteValueArguments will respect the order of the attributes specified in the schema.
%
% Arguments: 
%   KeyValueArguments: Key-value list containing the attribute name and related value of the predicate which is being processed;
%   CompleteKeyArguments: List of all attribute names associated to an Entity/Arc in the schema;
%   CompleteValueArguments: List containing completed values from KeyValue Arguments parameter + 'null' for each
%     attribute present in the schema but missing from KeyValueArguments.

complete(_, [], []).

complete(KeyValueArguments, [PropKey|CompleteKeyArguments], [PropValue|CompleteValueArguments]) :-
    member(PropKey-PropValue, KeyValueArguments),
    !,
    complete(KeyValueArguments, CompleteKeyArguments, CompleteValueArguments).

complete(KeyValueArguments, [_|CompleteKeyArguments], ['null'|CompleteValueArguments]) :-
    complete(KeyValueArguments, CompleteKeyArguments, CompleteValueArguments).


% write_clause(+PredicateId, +ClassName, +CompleteValueArguments, +SideInfoGathered)
%
% Given all the processed information related to a clause from the input file, writes the higher level formalism clause into
% the redirected output stream. There are three different possibilities:
%
%   - Entity clause, for which both the topLevelClass and instanceDomains clauses associated to it were gathered
%   - Entity clause, for which only the instanceDomains clause associated to it was gathered (topLevelClass information is missing)
%   - Arc clause (with the involved Subject and Object instances identifiers)
%
% Arguments:
%   PredicateId: Predicate id of the clause from the input file which is being processed;
%   ClassName: Name of the Entity/Arc class of the predicate which is being processed;
%   CompleteValueArguments is the list containing completed values from KeyValue Arguments parameter + 'null' for each
%     attribute present in the schema but missing from KeyValueArguments;
%   SideInfoGathered list contains additional information related to the Entity/Arc (Top Level Class Name, Instance Domains).

write_clause(PredicateId, ClassName, CompleteValueArguments, SideInfoGathered) :-
    member(topLevelClass-TopLevelName, SideInfoGathered),
    !,
    TopLevelClause =.. [topLevelClass, PredicateId, TopLevelName],
    portray_clause(TopLevelClause),  % topLevelClass(EntityID, TopLevelClassName)

    findall(OntologyName, member(instanceDomains-OntologyName, SideInfoGathered), OntologiesList),
    OntologyClause =.. [instanceDomains, PredicateId, OntologiesList],
    portray_clause(OntologyClause),  % instanceDomains(EntityID, InstanceDomainsList)

    HighLevelClause =.. [ClassName, PredicateId|CompleteValueArguments],
    portray_clause(HighLevelClause).  % EntityClassName(EntityID, PropertiesList)
    
write_clause(PredicateId, ClassName, CompleteValueArguments, SideInfoGathered) :-
    member(subjectId-SubjectId, SideInfoGathered),
    member(objectId-ObjectId, SideInfoGathered),
    !,
    HighLevelClause =.. [ClassName, PredicateId, SubjectId, ObjectId|CompleteValueArguments],
    portray_clause(HighLevelClause).  % ArcClassName(ArcID, SubjectEntityID, ObjectEntityID, PropertiesList)

write_clause(PredicateId, ClassName, CompleteValueArguments, SideInfoGathered) :-
    findall(OntologyName, member(instanceDomains-OntologyName, SideInfoGathered), OntologiesList),
    OntologyClause =.. [instanceDomains, PredicateId, OntologiesList],
    portray_clause(OntologyClause),  % instanceDomains(EntityID, InstanceDomainsList)
    
    HighLevelClause =.. [ClassName, PredicateId|CompleteValueArguments],
    portray_clause(HighLevelClause).  % EntityClassName(EntityID, PropertiesList)
