:- module(vworld, [get_vworld/1,reset_world/0,clear_world/0,add_persons_places/0,move_all/0,
          set_loc_goal/3,is_loc_type/1,
            noun_type/2,start_move_threads/0,stop_move_threads]).

% -----------------------
% Ontology and config
% -----------------------

is_stereotype(christian).
is_stereotype(gay).
is_stereotype(wmale25).
% same as christian but wanted to make sure we handle a third.  Could have used sutypes of LGBT


% location type and EffectRange
is_loc_type(church).
is_loc_type(sportsbar).
is_loc_type(disco).


% setup_type(PType,EffectRange,Haunt,Create,Cate).
setup_type(priest,200,church,1,christian).
setup_type(christian,100,church,5,christian).
setup_type(activist,200,disco,1,gay).
setup_type(gay,100,disco,5,gay).
setup_type(basher,200,sportsbar,1,wmale25).
setup_type(wmale25,100,sportsbar,3,wmale25).
setup_type(church,200,church,1,christian).
setup_type(sportsbar,200,sportsbar,1,wmale25).
setup_type(disco,200,disco,1,gay).


% every 20 seconds.
move_every(4).

% xS,yS - xE,yE, step every 20 seconds.
world_range(1,1,1000,1000,200).



% -----------------------
% External API
% -----------------------
   
% returns a list
get_vworld(List):- P=noun_state(_P1,_X,_Y,_NounType,_EmoIcon,_BodyIcon), findall(P,P,List).

set_loc_goal(P1,X,Y):- retractall(loc_goal(P1,_,_)),assert_now(loc_goal(P1,X,Y)).

reset_world :- clear_world, add_persons_places.

% clear_world
% add_persons_places


% -----------------------
% existential predicates
% -----------------------

:-dynamic(noun_type/2).

noun_stype(Disco1,Gay):-noun_type(Disco1,Disco),setup_type(Disco,_,_,_,Gay).

% loc(Person,X,Y).
% loc(Place,X,Y).
:-dynamic(loc/3).

noun_type_type(P1,T):-noun_type(P1,T1),(is_loc_type(T1) -> T=place; T=person),!.

noun_state(P1,X,Y,NounTT, EmoIcon,BodyIcon):- noun_type(P1,Body),
   noun_type_type(P1,NounTT),
   once((loc(P1,X,Y), 
   atom_concat(Body,'.PNG',BodyIcon),
   reaction_icon(P1,EmoIcon))).


% place a Person is traveling to

:-dynamic(loc_goal/3).

% -----------------------
% world util predicates
% -----------------------

reaction(P1,P2,Emo,Strengh):-
   loc(P1,X1,Y1),
   loc(P2,X2,Y2), 
   dist(X1,Y1,X2,Y2,Strengh),
   noun_react(P1,P2,Emo).


noun_react(P1,P2,R):-noun_stype(P1,T1),noun_stype(P2,T2),type_react(T1,T2,R).

reaction_icon(P1,EmoIconPNG):- closest_noun(P1,P2,_Close),
   reaction(P1,P2,Emo,Strengh),
   strengh_scale(Strengh,SS),
   atom_concat(Emo,SS,EmoIcon),
   atom_concat(EmoIcon,'.PNG',EmoIconPNG).

closest_noun(P1,P2,Dist):- noun_type(P1,T1), noun_type(P2,T2), T1 \= T2,
     loc(P1,X1,Y1),  loc(P2,X2,Y2), dist(X1,Y1,X2,Y2,Dist).

type_react(T1,T2,happy):-T1==T2.
type_react(_T1,_T2,fear).
strengh_scale(_,1).

% -----------------------
% world setup predicates
% -----------------------

% random_loc/2

random_loc(X,Y):-world_range(SX,SY,EX,EY,_),random_loc(SX,SY,EX,EY,X,Y).

%todo use circle  instead of square
random_loc(SX,SY,EX,EY,X,Y):-random_between(SX,EX,X),random_between(SY,EY,Y).

random_between(S,E,R):-Range is E - S, RM is random(Range),R is RM+S.

dist(X1,Y1,X2,Y2,D):- DX is X2-X1, DY is Y2-Y1, D is sqrt(DX*DX+DY*DY).

clear_world:-
              retractall_now(loc(_,_,_)),
              retractall_now(loc_goal(_,_,_)),
              retractall_now(noun_type(_,_)).


add_persons_places:- setup_type(Priest,_Range200,_FavLoc,Num,_StereoType),
      between(1,Num,N),atom_concat(Priest,N,Whatnot),assert_now(noun_type(Whatnot,Priest)),
      random_loc(X,Y),assert_now(loc(Whatnot,X,Y)),fail.
add_persons_places.


assert_now(X):-debugFmt(add(X)),assert(X).
retractall_now(X):-debugFmt(del(X)),retractall(X).

debugFmt(X):-debugFmt('Debug: ~w.~n',[X]).

debugFmt(F,A):-format(user_error,F,A),flush_output(user_error).

% -----------------------
% world play preds
% -----------------------
:-dynamic(started_move_threads/0).
start_move_threads:-started_move_threads,!.
start_move_threads:-
   asserta(started_move_threads),
   thread_create(move_all_thread,ID1,[at_exit(retract_self)]),
   thread_create(interpolate_thread,ID2,[at_exit(retract_self)]),
   asserta(movement_thread(ID1)),
   asserta(movement_thread(ID2)).

retract_self:-thread_self(ID),retractall(movement_thread(ID)).

stop_move_threads:- retract(movement_thread(ID)),thread_signal(ID,thread_exit(kill_move_threads)),fail.
stop_move_threads.

move_all_thread:-repeat,sleep(20),once(move_all1),fail.
interpolate_thread:-repeat,sleep(1),once(move_all_one_sec),fail.


move_all_one_sec:-noun_type(P1,Type),not(is_loc_type(Type)),move_for_one_sec(P1),fail.
move_all_one_sec:-!.

move_for_one_sec(P1):-
   loc(P1,X1,Y1),
   loc_goal(P1,X3,Y3),
   get_polar_coords(X3-X1,Y3-Y1,Angle,_GoDist),
   carts_for_polar_ofset(X1,Y1,Angle,100,X2,Y2),
   set_loc(P1,X2,Y2),!.

get_polar_coords(DX,DY,Ang,Dist):-Dist is sqrt(DX*DX+DY*DY), Ang is atan(DY/DX).

set_loc(P1,X1,Y1):-
   to_int(X1,X2),
   to_int(Y1,Y2),
   retractall(loc(P1,_,_)),
   assert(loc(P1,X2,Y2)).


move_all1:-debugFmt('startring to move_all!'),fail.
move_all1:-noun_type(P1,Type),not(is_loc_type(Type)),change_loc_goal(P1),fail.
move_all1:-debugFmt('completed to move_all!'),fail.
move_all1:-!.


make_between(In,Low,_High,Out):-In < Low,!,Out==Low.
make_between(In,_Low,High,Out):-In > High,!,Out==High.
make_between(In,_Low,_High,In).

change_loc_goal(P1):- 
   loc(P1, X1,Y1),!,
   world_range(SX,SY,EX,EY,DistPer20),
   GoDist is random(DistPer20),
   Angle is random(360)/57.29577951,
   carts_for_polar_ofset(X1,Y1,Angle,GoDist,X2,Y2),
   make_between(X2,SX,EX,X3),
   make_between(Y2,SY,EY,Y3),
   set_loc_goal(P1,X3,Y3).

change_loc_goal(P1):- 
   random_loc(X,Y),
   set_loc(P1, X,Y),
   set_loc_goal(P1,X,Y),!.


carts_for_polar_ofset(X1,Y1,Angle,GoDist,X2,Y2):-X2 is X1 + cos(Angle)*GoDist,Y2 is Y1 + sin(Angle)*GoDist.

%% move_all:-noun_type(P1,Type),


:-at_initialization(start_move_threads).
