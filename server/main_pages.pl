:- module(main_pages, []).
/** <module> Serve the lobby pages

*/
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(file_handler).

:- http_handler(root(.),
		http_redirect(moved, root('index.html')),
		[priority(90)]).
:- http_handler(root('index.htm'),
		http_redirect(moved, root('index.html')),
		[]).
:- http_handler(root(.),
		root_handler,
		[prefix, priority(-10)]).
:- http_handler(root('favicon.ico'),
		http_redirect(moved, files('/favicon.ico')),
		[]).

root_handler(Request) :-
	ua_type(Request, mobile),
	serve_files(mobile_files(.), Request).
root_handler(Request) :-
	ua_type(Request, desktop),
	serve_files(desktop_files(.), Request).
root_handler(Request) :-
	ua_type(Request, _),
	serve_files(mobile_files(.), Request).

ua_type(Request, mobile) :-
	member(user_agent(UA), Request),
	atom_codes(UA, Darn7UA),
	phrase(mobile_string, Darn7UA).
ua_type(_, desktop).

mobile_string --> any, key, any.

any --> [].
any --> [_], any.

key --> "mobile".
key --> "Mobile".
key --> "iphone".
key --> "android".
