:- module(debug_start, []).
/** <module> Dev mode starter file

    Consult this file to start the system in dev mode

*/

% Make sure we're on a reasonable version
%

%%	reasonable_version is nondet
%
%	succeeds if our version is 7.1.4 or better
%
reasonable_version :-
	current_prolog_flag(version_data, swi(Major, _, _, _)),
	Major > 7.
reasonable_version :-
	current_prolog_flag(version_data, swi(7, Minor, _, _)),
	Minor > 1.
reasonable_version :-
	current_prolog_flag(version_data, swi(7, 1, Patch, _)),
	Patch > 3.

check_version :- reasonable_version, !.
check_version :-
      current_prolog_flag(version_data, swi(Major, Minor, Patch, _)),
      format('OOOPS - you need swipl version 7.1.4 or better, you are on ~w.~w.~w~n',
	[Major, Minor, Patch]).

:- check_version.

%%	force_right_directory
%
%	Change the working directory to the directory this file
%	loaded from so we don't have weird relative path issues
%
force_right_directory :-
	source_file(check_version, File),
	file_directory_name(File, Dir),
	working_directory(_, Dir).

:- force_right_directory.

% Make it easier to read 'codes' style strings
:- portray_text(true).

% First we import the abstract path stuff
:- use_module(library(http/http_path)).

http:location(pldoc, root('gamejam/help/source'), [priority(10)]).

% load our application server
% We start it first
% so it doesn't collide with pldoc
:-ensure_loaded(load).
:- use_module(gamejam).
:- use_module(vworld).
:- reset_world.

% Now we can start pldoc. This starts our application server
% as well, a workaround for one server per process
:- gamejam_game_debug_port(Port), doc_server(Port).

:- gamejam_game_debug_port(Port),
	format(atom(URL), 'http://localhost:~w/', [Port]),
	www_open_url(URL).

% and our pldoc home page
:- gamejam_game_debug_port(Port),
	format(atom(URL), 'http://localhost:~w/gamejam/help/source/', [Port]),
	www_open_url(URL).

% and bring up the main module in the editor
:- edit('gamejam.pl').

% open the navigator
:- prolog_ide(open_navigator).

