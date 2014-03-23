:- module(load, []).
/** <module> Load this to start the production environment

*/

% A bit of possibly excessive abstraction, we load the
% server but don't run it in gamejam.pl
:-use_module(gamejam).
:- ensure_loaded(resources).
% make sure the handlers get loaded
:- ensure_loaded(file_handler).
:- ensure_loaded(styling).
:- ensure_loaded(main_pages).
:- ensure_loaded(gamepage).

