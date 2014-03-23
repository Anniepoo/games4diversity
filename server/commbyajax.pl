:-module(commbyajax, [comm_by_ajax//0]).
/** <module> Since pengines is failing us we're doing comms by Ajax

*/

:- use_module(library(http/json_convert)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/json)).
:- use_module(library(http/html_write)).
:- use_module(vworld).

comm_by_ajax -->
	html([\html_requires(ajaxupdate)]).

:- http_handler(root(people), reply_people, [id(people)]).

reply_people(_Request) :-
	get_vworld(List),
	vworld_list_dict(List, ListOfDicts),
	write('Content-Type: application/json\n\n'),
	atom_json_dict(Text, people_data{list: ListOfDicts}, [as(atom)]),
	write(Text).

vworld_list_dict([], []).
vworld_list_dict(
    [noun_state(Name,X,Y,person, EmoIcon,BodyIcon, ToolTip) | Rest],
    [D | RestOut]) :-
    atom_concat('/img/', EmoIcon, EmoPath),
    atom_concat('/img/', BodyIcon, BodyPath),
    D = noun_state{
	name: Name,
	x: X,
	y: Y,
	emo: EmoPath,
	body: BodyPath,
        tooltip: ToolTip},
    vworld_list_dict(Rest, RestOut).
vworld_list_dict([_ | Rest], RestOut) :-
	vworld_list_dict(Rest, RestOut).
