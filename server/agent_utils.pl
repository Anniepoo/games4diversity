:- module(agent_utils, [ua_type/2]).
/** <module> utilities to determine user agent (browser type)

@author Anne Ogborn
@license lgpl
@version 0.1.0

*/
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

