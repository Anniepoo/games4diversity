:- module(styling, []).
/** <module> styling hooks

*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).

user:head(desktop, Head) -->
        html(head([
		 \html_requires(css('gamejam.css')),
		 \html_requires(css('desktop.css')),
		 Head
                  ])).

user:head(desktop, Head) -->
        html(head([
		 \html_requires(css('gamejam.css')),
		 \html_requires(css('mobile.css')),
		 Head
                  ])).

%
user:body(desktop, Body) -->
        html(body([ Body
                  ])).

user:body(mobile, Body) -->
        html(body([ Body
                  ])).
