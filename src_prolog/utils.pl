:- module(utils,
        [
            select_file/2
        ]).
:- use_module(library(system)).
:- use_module(library(lists)).


% FileType = list_graph or schema

select_file(FileType, SelectedFilePath) :-
    directory_files('outputs', ListDir),

    ((FileType == 'list_graph') -> Prefix = 'list_' ; Prefix = 'schema_'),
    get_valid_filenames(ListDir, Prefix, FilePaths),
    select_file_path(FilePaths, FileType, SelectedFilePath).


get_valid_filenames([FilePath|T1], Prefix, [FilePath|T2]) :-
    atom_concat(Prefix, X, FilePath),
    atom_concat(_, '.pl', X),
    !,
    get_valid_filenames(T1, Prefix, T2).

get_valid_filenames([_|ListDir], Prefix, OtherFilePaths) :-
    get_valid_filenames(ListDir, Prefix, OtherFilePaths).

get_valid_filenames([], _, []).


select_file_path(FilePaths, FileType, SelectedFilePath) :-
    length(FilePaths, 1),
    !,
    nth0(0, FilePaths, SelectedFilePath),
    ((FileType == 'list_graph') ->
     format(user_output, 'Found one possible exported graph in list format: ~p will be used\n', [SelectedFilePath]) ;
     format(user_output, 'Found one possible schema file: ~p will be used\n', [SelectedFilePath])).

select_file_path(FilePaths, FileType, _) :-
    length(FilePaths, 0),
    !,
    ((FileType == 'list_graph') ->
     write('[ERROR] No prolog file containing exported graph in list format in outputs folder!\n') ;
     write('[ERROR] No schema prolog file in outputs folder!\n')),
    fail.

select_file_path(FilePaths, FileType, SelectedFilePath) :-
    length(FilePaths, LenFilePaths),
    ((FileType == 'list_graph') ->
         format(user_output, 'Found ~d possible exported graphs in list format, please choose the correct one to use:\n', [LenFilePaths]) ;
         format(user_output, 'Found ~d possible schema files, please choose the correct one to use:\n', [LenFilePaths])),
    print_possible_file_paths(FilePaths),
    select_file_path_multiple_choice(FilePaths, LenFilePaths, SelectedFilePath).

select_file_path_multiple_choice(FilePaths, LenFilePaths, SelectedFilePath) :-
    read(Choice),
    between(1, LenFilePaths, Choice),
    !,
    nth(Choice, FilePaths, SelectedFilePath).

select_file_path_multiple_choice(FilePaths, LenFilePaths, SelectedFilePath) :-
    write(user_output, 'Choice was not a valid number, please insert an appropriate value\n'),
    select_file_path_multiple_choice(FilePaths, LenFilePaths, SelectedFilePath).

print_possible_file_paths(FilePaths) :-
    print_possible_file_paths(FilePaths, 1).
print_possible_file_paths([FilePath|FilePaths], Enum) :-
    format(user_output, '~d ---> ~p\n', [Enum, FilePath]),
    NewEnum is Enum + 1,
    print_possible_file_paths(FilePaths, NewEnum).
print_possible_file_paths([], _).
