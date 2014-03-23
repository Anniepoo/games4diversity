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
	reply_html_page(desktop_game,
			[meta([ 'http-equiv'(refresh),content(2)]),title('Crowd Pleasing')],
			\game_body(ID)).

game_body(_ID) -->
	html([
	   \html_requires(jquery),
	   \html_requires(pengine),
	   \html_requires(vworld_update),
	   div(id(main), [\characters])
	]).


% TODO no player identity yet
player_id(1).

characters -->
	{
	     vworld:get_vworld(Nouns)
        },
	a_character(Nouns).

a_character([]) --> [].
a_character([noun_state(Name,X,Y,person, EmoIcon,BodyIcon) | Rest]) -->
	html(div([class(noun), style(['left: ~wpx; top: ~wpx'-[X,Y]]), id(Name)], [
		     img([class(body), src(['/img/~w'-BodyIcon])]),
		     img([class(emo), src(['/img/~w'-EmoIcon])])
		 ])),
	a_character(Rest).
a_character([noun_state(_,_,_,place,_,_) | Rest]) -->
	a_character(Rest).

% get_vworld([noun_state(P1,X,Y,EmoIcon,BodyIcon),noun_state(P1,X,Y,EmoIcon,BodyIcon),...])
