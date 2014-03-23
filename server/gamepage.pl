:- module(gamepage, []).
/** <module> The page where the game's actually played

This page connects to the game and the user then interacts by
pengine calls.

@author Anne Ogborn
@license lgpl
@version 0.1.0

*/
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_dispatch)).
:- use_module(file_handler).
:- use_module(library(http/http_session)).
:- use_module(agent_utils).
:- use_module(library(http/html_head)).
:- use_module(resources).
:- use_module(vworld).

:- http_handler(root(game), game_page_handler, [id(game)]).

game_page_handler(Request) :-
	ua_type(Request, mobile),
	game_page_handler_(Request).
game_page_handler(Request) :-
	ua_type(Request, desktop),
	game_page_handler_(Request).
game_page_handler(Request) :-
	ua_type(Request, _),
	game_page_handler_(Request).

game_page_handler_(_Request) :-
	player_id(ID),
	reply_html_page(desktop,
			[title('Crowd Pleasing')],
			\game_body(ID)).

game_body(ID) -->
	html([
	   \html_requires(jquery),
	   div(id(main), [\characters])
	]).


% TODO no player identity yet
player_id(1).

characters -->
	{
	     get_vworld(Nouns)
        },
	a_character(Nouns).

a_character([]) --> [].
a_character([noun_state(Name,X,Y,EmoIcon,BodyIcon) | Rest]) -->
	html(div([class(noun), style(['left: ~w, top: ~w'-[X,Y]]), id(Name)], [
		     img([class(emo), src(EmoIcon)]),
		     img([class(body), src(BodyIcon)])
		 ])),
	a_character(Rest).

% get_vworld([noun_state(P1,X,Y,EmoIcon,BodyIcon),noun_state(P1,X,Y,EmoIcon,BodyIcon),...])