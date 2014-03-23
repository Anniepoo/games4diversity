:- module(vworld_apis, [pen_get_vworld/1]).
/** <module> external pengine visible api's

*/
:- use_module(vworld).

pen_get_vworld(List) :-
   repeat,   % yes, repeat success loop. get_vworld is volatile
   get_vworld(List).
