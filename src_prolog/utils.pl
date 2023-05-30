:- module(utils,
        [
            select_file/2
        ]).
:- use_module(library(system)).
:- use_module(library(lists)).



%% select_file(+FileType, -SelectedFilePath)
%
% Returns relative path for the file to load stored in the 'outputs' directory. The file to load depends on the FileType argument value:
%
%   - 'list_graph': Path of the KB restructured in list format using the 'KBRestructurer' Java class will be returned;
%   - 'schema': Path of the schema restructured from XML to Prolog using the 'SchemaToProlog' Java class will be returned.
%
% If more than one file eligible for loading is available, the user will be prompted to choose only one of them.
% If no files are eligible for loading, the program prints an error and then fails.
%
% Arguments:
%     FileType: Either 'list_graph' or 'schema';
%     SelectedFilePath: The relative file path of the prolog file to load.

select_file(FileType, SelectedFilePath) :-
    directory_files('outputs', ListDir),

    ((FileType == 'list_graph') -> Prefix = 'list_' ; Prefix = 'schema_'),
    get_valid_filenames(ListDir, Prefix, FilePaths),
    select_file_path(FilePaths, FileType, SelectedFilePath).


% get_valid_filenames(+FilePaths, +Prefix, -EligibleFilePaths)
%
% Given a list of file paths and a specified prefix, all file paths which begin with that prefix and 
% have the '.pl' extension are retrieved.
%
% Arguments:
%   FilePaths: List of paths for all available files in the 'outputs' directory;
%   Prefix: Starting characters of the eligible file paths;
%   EligibleFilePaths: List of all paths from the FilePaths list having Prefix as starting characters and '.pl' as final characters

get_valid_filenames([FilePath|FilePaths], Prefix, [FilePath|EligibleFilePaths]) :-
    atom_concat(Prefix, X, FilePath),
    atom_concat(_, '.pl', X),
    !,
    get_valid_filenames(FilePaths, Prefix, EligibleFilePaths).

get_valid_filenames([_|ListDir], Prefix, EligibleFilePaths) :-
    get_valid_filenames(ListDir, Prefix, EligibleFilePaths).

get_valid_filenames([], _, []).


% select_file_path(+EligibleFilePaths, +FileType, -SelectedFilePath)
%
% Given a list of eligible file paths and a specified file type ('list_graph' or 'schema'), retrieves a file path from the 
% eligible file paths list. There are three possible cases in total:
%
%   - There is only one eligible path, in which case it will be returned directly;
%   - There is no eligible path, an error message will be shown and the procedure will fail;
%   - More than one eligible path, the user will be prompted to select only one of these files.
%
% Arguments:
%   EligibleFilePaths: List of paths filtered using the 'get_valid_filenames' procedure;
%   FileType: Either 'list_graph' or 'schema';
%   SelectedFilePath: File path retrieved from the ones in the EligibleFilePaths list.

select_file_path(EligibleFilePaths, FileType, SelectedFilePath) :-
    length(EligibleFilePaths, 1),
    !,
    nth0(0, EligibleFilePaths, SelectedFilePath),
    ((FileType == 'list_graph') ->
     format(user_output, 'Found one possible exported graph in list format: ~p will be used\n', [SelectedFilePath]) ;
     format(user_output, 'Found one possible schema file: ~p will be used\n', [SelectedFilePath])).

select_file_path(EligibleFilePaths, FileType, _) :-
    length(EligibleFilePaths, 0),
    !,
    ((FileType == 'list_graph') ->
     write('[ERROR] No prolog file containing exported graph in list format in outputs folder!\n') ;
     write('[ERROR] No schema prolog file in outputs folder!\n')),
    fail.

select_file_path(EligibleFilePaths, FileType, SelectedFilePath) :-
    length(EligibleFilePaths, LenFilePaths),
    ((FileType == 'list_graph') ->
         format(user_output, 'Found ~d possible exported graphs in list format, please choose the correct one to use:\n', [LenFilePaths]) ;
         format(user_output, 'Found ~d possible schema files, please choose the correct one to use:\n', [LenFilePaths])),
    print_eligible_file_paths(EligibleFilePaths),
    select_file_path_multiple_choice(EligibleFilePaths, LenFilePaths, SelectedFilePath).


% select_file_path_multiple_choice(+FilePaths, +Prefix, -EligibleFilePaths)
%
% Given the list of eligible file paths with at least two elements, they will be printed and the user
% will be prompted to select one of them. If the user makes an invalid choice (i.e. number not associated to any eligible file path),
% a warning is printed and the user is asked to make again a choice.  
%
% Arguments:
%   EligibleFilePaths: List of eligible file paths for loading that will be printed;
%   LenFilePaths: Number of all possible eligible file paths;
%   SelectedFilePath: The actual file path chosen by the user.

select_file_path_multiple_choice(EligibleFilePaths, LenEligibleFilePaths, SelectedFilePath) :-
    read(Choice),
    between(1, LenEligibleFilePaths, Choice),
    !,
    nth(Choice, EligibleFilePaths, SelectedFilePath).

select_file_path_multiple_choice(EligibleFilePaths, LenEligibleFilePaths, SelectedFilePath) :-
    write(user_output, 'Choice was not a valid number, please insert an appropriate value\n'),
    select_file_path_multiple_choice(EligibleFilePaths, LenEligibleFilePaths, SelectedFilePath).


% print_eligible_file_paths(+EligibleFilePaths)
%
% Given the list of eligible file paths, prints them with an auto incrementing integer in the following format:
%   1 ---> eligible_path_1
%   2 ---> eligible_path_2
%   ...
%   n ---> eligible_path_n
%
% Arguments:
%   EligibleFilePaths: List of eligible file paths for loading that will be printed.

print_eligible_file_paths(EligibleFilePaths) :-
    print_eligible_file_paths(EligibleFilePaths, 1).

print_eligible_file_paths([EligibleFilePath|EligibleFilePaths], Enum) :-
    format(user_output, '~d ---> ~p\n', [Enum, EligibleFilePath]),
    NewEnum is Enum + 1,
    print_eligible_file_paths(EligibleFilePaths, NewEnum).

print_eligible_file_paths([], _).
