:- module(file_handler, [serve_files/2]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).

:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

% we'll need to fiddle with abstract locations
:- use_module(library(http/http_path)).

% and we need file support
:- use_module(library(http/http_files)).

% give'm a session
:- use_module(library(http/http_session)).

% Add an abstract URI path root to serve assets from
%
http:location(files, root(f), []).
http:location(css, root(css), []).
http:location(js, root(js), []).

user:file_search_path(mobile_files, 'assets/mobile/html').
user:file_search_path(desktop_files, 'assets/desktop/html').
user:file_search_path(static_files, assets).
user:file_search_path(css, 'assets/css').
user:file_search_path(js, 'assets/js').

% handle /f/
:- http_handler(files(.),
		serve_files(static_files(.)) ,
		[prefix]).

% handle /css/
:- http_handler(css(.),
		serve_files(css(.)) ,
		[prefix, priority(10)]).

% handle /js/
:- http_handler(js(.),
		serve_files(js(.)) ,
		[prefix, priority(10)]).

%%	serve_files(+FileLocation:abstract_file, +Request:request) is
%	det
%
%	Serve a static file in response to request
%
serve_files(FileLocation, Request) :-
	http_reply_from_files(FileLocation, [], Request).

% What if the files' not there?
serve_files(_, Request) :-
      http_404([], Request).

