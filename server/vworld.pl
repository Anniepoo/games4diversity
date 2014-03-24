:- module(vworld, [
            get_vworld/1,
            reset_world/0,
            clear_world/0,
            add_persons_places/0,
            move_all/0,
            set_loc_goal/3,
            is_loc_type/1,
            noun_type/2,
            start_move_threads/0,
            stop_move_threads/0]).

/** <module> The virtual world is ran here

This file brokers the game state.

@author Douglas R. Miles
@author Anne Ogborn
@license lgpl
@version 0.1.0

*/
:- use_module(library(pengines)).
:- use_module(library(sandbox)).
:- use_module(pengine_sandbox:vworld_apis).

:- style_check(+discontiguous).

% -----------------------
% Ontology and config
% -----------------------

is_stereotype(christian).
is_stereotype(gay).
is_stereotype(average).
% same as christian but wanted to make sure we handle a third.  Could have used sutypes of LGBT

% location type and EffectRange
is_loc_type(church).
is_loc_type(sportsbar).
is_loc_type(disco).

nop(_).

% setup_type(PType,EffectRange,Haunt,Create,Cate).
setup_type(priest,200,church,1,christian).
setup_type(activist,200,disco,1,gay).
setup_type(basher,200,sportsbar,1,average).
setup_type(church,300,church,1,christian).
setup_type(sportsbar,300,sportsbar,1,average).
setup_type(disco,300,disco,1,gay).

setup_type(average,100,sportsbar,3,average).
setup_type(gay,100,disco,5,gay).
setup_type(christian,100,church,5,christian).

type_effect_range(PType,EffectRange):-setup_type(PType,EffectRange,_Haunt,_Create,_Cate).


% xS,yS - xE,yE, step every 20 seconds.
world_range(1,200,1000,1000,800).

% -----------------------
% External API
% -----------------------

% returns a list

get_vworld(ListO):-  P=noun_state(_P1,_X,_Y,_NounType,_EmoIcon,_BodyIcon,_ToolTip), findall(P,P,List),!,ListO=List.

set_loc_goal(P1,X1,Y1):-
   to_int(X1,X2),
   to_int(Y1,Y2),
   retractall(loc_goal(P1,_,_)),assert_now(loc_goal(P1,X2,Y2)).

reset_world :- clear_world, add_persons_places.

% clear_world
% add_persons_places


% -----------------------
% existential predicates
% -----------------------

:-dynamic(noun_type/2).

noun_stype(Disco1,Gay):-noun_type(Disco1,Disco),setup_type(Disco,_,_,_,Gay).

% loc(Noun,X,Y).
:-dynamic(loc/3).

noun_type_type(P1,T):-noun_type(P1,T1),(is_loc_type(T1) -> T=place; T=person),!.

noun_state(P1,X,Y,NounTT,EmoIcon,BodyIcon,ToolTip):-
   noun_type(P1,_),
   once((loc(P1,X,Y),
         noun_type_type(P1,NounTT),
   once((body_icon(P1,BodyIcon))),
   reaction_icon(P1,EmoIcon),
   noun_info(P1,ToolTip))).

body_icon(P1,BodyIcon):-gender(P1,G),noun_type(P1,T),body_icon(G,T,BodyIcon),!.


:-dynamic(known_gender/2).
known_gender(priest1,male).
gender(P1,G):- known_gender(P1,G),!.
gender(P1,G):- (0 is random(2)->G=male;G=female),assert(known_gender(P1,G)),!.





noun_info(P1,Info):-body_icon(P1,BI),reaction_icon(P1,RI),gender(P1,G), ignore(noun_emo_vectors(P1,EmoVectors)), sformat(Info,'~w',[info(G,P1,BI,RI,EmoVectors)]),!.
noun_info(P1,P1).

% place a Person is traveling to

:-dynamic(loc_goal/3).

% -----------------------
% world util predicates
% -----------------------


reaction(P1,P2,_Emo,Value):-P1=P2,!,Value=0.0.
reaction(P1,P2,Emo,Value):-noun_stype(P1,S1),noun_stype(P2,S2),!,type_react(S1,S2,Emo),
                                  dist(P1,P2,D),noun_type(P2,Type),type_effect_range(Type,ER),
                                    drv(D , ER, Value),!.
drv(D0,_R,0.0):- D is D0,D < 1,!.
%%drv(D0,R0,V):-R is R0,D is D0, D < R ,  V is D,!.
drv(D0,R0,V):-R is R0,D is D0, D < R ,  V is 3 *(R / D),!.
drv(_,_,0.01).



noun_react(P1,P2,R):-noun_stype(P1,T1),noun_stype(P2,T2),type_react(T1,T2,R).


% -----------------------
% Graphics
% -----------------------
old_icons :- fail.


%% heads...
reaction_icon_typed(_Person,_Gender,_Type,Emo,Icon):- old_icons, atom_concat(Emo,'1.PNG',Icon).

reaction_icon_typed(_P1,_G,priest,angry,'WPreac_Angryl.png'). % all priest are male or females posing as males
reaction_icon_typed(_P1,_G,priest,happy,'WPreac_Happy.png').
reaction_icon_typed(_P1,male,_,happy,'WhMale_Happy.png').
reaction_icon_typed(_P1,female,_,happy,'WWoman_Happy.png').
reaction_icon_typed(_P1,male,_,neutral,'WhMale_Neutral.png').
reaction_icon_typed(_P1,female,_,neutral,'WWoman_Neutral.png').
reaction_icon_typed(_P1,male,_,angry,'WhMale_Angry.png').
reaction_icon_typed(_P1,female,_,angry,'BWoman_Angry.png').
reaction_icon_typed(_P1,male,_,fear,'WhMale_Fear.png').
reaction_icon_typed(_P1,female,_,fear,'WWoman_Fear.png').

%% bodies...
body_icon(_Gender,Type,Icon):- old_icons,atom_concat(Type,'.PNG',Icon).

body_icon(_,average,'wmale25.PNG').
body_icon(_,average,'WhMale.PNG').
body_icon(_,priest,'WPreac.png').
body_icon(male,gay,'BGayMa.png').
body_icon(_,gay,'WGayFe.png').
body_icon(male,priest,'WPreac.png').
body_icon(_,X,G):-atom_concat(X,'.PNG',G).
body_icon(male,_,'wmale25.PNG').



reaction_icon(P1,EmoIconPNG):-
  notrace(( noun_emo_most(P1,Emo,_Strengh))),
   gender(P1,G),noun_type(P1,T),
   reaction_icon_typed(P1,G,T,Emo,EmoIconPNG),!.

reaction_icon(P1,EmoIconPNG):-
   notrace((noun_emo_most(P1,Emo,Strengh))),
   strengh_scale(Strengh,SS),
   atom_concat(Emo,SS,EmoIcon),
   atom_concat(EmoIcon,'.PNG',EmoIconPNG).

is_emo(anger).
is_emo(neutral).
is_emo(fear).
is_emo(happy).

type_react(gay,gay,happy).
type_react(average,gay,fear).
type_react(christian,gay,anger).
type_react(gay,average,happy).
type_react(average,average,neutral).
type_react(christian,average,neutral).
type_react(gay,christian,fear).
type_react(average,christian,happy).
type_react(christian,christian,happy).

strengh_scale(_,1).

% -----------------------
% world setup predicates
% -----------------------

% random_loc/2

random_loc(X,Y):-world_range(SX,SY,EX,EY,_),random_loc(SX,SY,EX,EY,X,Y).

%todo use circle  instead of square
random_loc(SX,SY,EX,EY,X,Y):-random_between(SX,EX,X),random_between(SY,EY,Y).

random_between(S,E,R):-Range is E - S, RM is random(Range),R is RM+S.

dist( P, B,  R) :- loc(P, XA, YA),loc(B, XB, YB),DX is XA - XB,DY is YA - YB,R is sqrt(DX * DX + DY * DY).

dist(X1,Y1,X2,Y2,D):- DX is X2-X1, DY is Y2-Y1, D is sqrt(DX*DX+DY*DY).

clear_world:- retractall_now(loc(_,_,_)),
              retractall_now(loc_goal(_,_,_)),
              retractall_now(noun_type(_,_)).


add_persons_places:-assert_now(loc(church1, 294, 274)),assert_now(loc(disco1, 629 , 241)),assert_now(loc(sportsbar1, 903,341)),fail.
add_persons_places:- setup_type(Priest,_Range200,_FavLoc,Num,_StereoType),
      between(1,Num,N),atom_concat(Priest,N,Whatnot),assert_now(noun_type(Whatnot,Priest)),
      random_loc(X,Y),once(( loc(Whatnot,_,_) -> true ; assert_now(loc(Whatnot,X,Y) ))),fail.
add_persons_places.


assert_now(X):-debugFmt(add(X)),assert(X).
retractall_now(X):-debugFmt(del(X)),retractall(X).


debugFmt(X):-nop(debugFmt('Debug: ~w.~n',[X])).

debugFmt(F,A):-format(user_error,F,A),flush_output(user_error).

% -----------------------
% world play preds
% -----------------------
:-dynamic(started_move_threads/0).
start_move_threads:-started_move_threads,!.
start_move_threads:-
   asserta(started_move_threads),
%   thread_create(change_movement_goals,ID1,[alias(change_movement_goals),at_exit(retract_self)]),
   thread_create(interpolate_thread,ID2,[alias(interpolate_thread),at_exit(retract_self)]),
%   asserta(movement_thread(ID1)),
   asserta(movement_thread(ID2)).

retract_self:-thread_self(ID),retractall(movement_thread(ID)).

stop_move_threads:- retract(movement_thread(ID)),thread_signal(ID,thread_exit(kill_move_threads)),fail.
stop_move_threads.

%% blocks and emulates players every 20 seconds
%change_movement_goals:-repeat,sleep(20),once(move_all),fail.

%% blocks and advances frames 1 per second
interpolate_thread:-repeat,sleep(1),once(move_all_one_sec),fail.


% advances one frame
move_all_one_sec:-noun_type(P1,Type),not(is_loc_type(Type)),move_for_one_sec(P1),fail.
move_all_one_sec:-!. %%, make.  % to check for changed disk files!

move_for_one_sec(P1):- 
   loc_goal(P1,X3,Y3), %% only use if there was a goal_loc
   loc(P1,X1,Y1),
   get_polar_coords(X3-X1,Y3-Y1,Angle,_GoDist),
   carts_for_polar_ofset(X1,Y1,Angle,100,X2,Y2),
   set_loc(P1,X2,Y2),!.

move_for_one_sec(P1) :-
   loc(P1,X1,Y1),
   setof(P, noun_type(P, _), People), 
   lennard_jones_m(P1, People, 0,0, FX, FY),
   speed_cofactor(S),
   X2 is X1 + FX * S,
   Y2 is Y1 + FY * S,
   set_loc(P1,X2,Y2),!,
   dist(X1,Y1,X2,Y2,D),
   nop(debugFmt('~w trying move ~w to get from ~w,~w to ~w,~w ~n',[P1, D, X1,Y1,X2,Y2])).

noun_emo_most(P1,Emo,Strengh):-noun_emo_vectors(P1,[Strengh-Emo|_]).

noun_emo_vectors(P1,EmoVectors):-
      findall(Value-Emo,((is_emo(Emo),once((noun_emo_any(P1,Emo,Value))))),List),keysort(List,EmoVectorsR),reverse(EmoVectorsR,EmoVectors),!.

noun_emo_any(P1,Emo,Value):-setof(P, noun_type(P, _), People),
  rate_situation(Emo,P1,People,0.1,Value).

rate_situation(_Type,_P, [], W, W).
rate_situation(Type,P, [P|T], WIn, WOut) :- % skip oneself
    rate_situation(Type,P, T, WIn, WOut).
rate_situation(Type,P, [B|T], WIn, WOut) :-
    reaction(P, B,Type, R),
    WMid is WIn + R,
    rate_situation(Type,P, T, WMid,WOut).




type_sign(P1,P2,-1.0):-noun_stype(P1,S1),noun_stype(P2,S2),S1==S2,!.
type_sign(_P1,_P2,1.0).

speed_cofactor(200.0).

stability(tension,_P,_B,8.0):-!.
stability(_,_,_,8.0). % this keeps us out of some very ugly edge cases as we get near R = 0

mag(_Type,P1,P2,Sign) :- type_sign(P1,P2,Sign),!.

lj_coefficients(move,_,_,4096.0, -256000.0).
lj_coefficients(tension,_,_,4096.0, -256000.0).


  
% movement version
lennard_jones_m(P, T, NFX, NFY, FX, FY):-lennard_jones(move, P, T, NFX, NFY, FX, FY).

% adversion weighted
lennard_jones_a_w(P, T, NFX, NFY, FX, FY):-
   lennard_jones(tension, P, T, NFX, NFY, FX, FY),!.

   
lennard_jones(_Type,_P, [], FInX, FInY, FInX, FInY).
lennard_jones(Type,P, [P|T], FInX, FInY, FX, FY) :- % skip oneself
    lennard_jones(Type,P, T, FInX, FInY, FX, FY).
lennard_jones(Type,P, [B|T], FInX, FInY, FX, FY) :-
    lj_dist(Type, P, B, R),
    lj_coefficients(Type,P,B,LJA, LJB),
    unit_vector(P, B, R, UX, UY),
    RFA is ( LJA / R / R + LJB / R / R / R ),
    NFX is FInX + UX * RFA,
    NFY is FInY + UY * RFA,
    lennard_jones(Type,P, T, NFX, NFY, FX, FY).

lj_dist(Type, P, B,  R) :-
   stability(Type, P, B, Stability),
   mag(Type, P, B, Mag),
   dist(P,B,D),
   
  %% nop((noun_type(B,BType), type_effect_range(BType,RE), drv(D,RE*2,V1),V is V1*5 )),
   V is D,
   R is (V* Mag) + Stability.
    
unit_vector(P, B, R, UX, UY) :-
    loc(P, XA, YA),
    loc(B, XB, YB),
    DX is XA - XB,
        DY is YA - YB,
    UX is DX / R,
    UY is DY / R.

get_polar_coords(DX,DY,Ang,Dist):-Dist is sqrt(DX*DX+DY*DY), Ang is atan2(DY,DX).

set_loc(P1,X1,Y1):-
   to_int(X1,X2),
   to_int(Y1,Y2),
   world_range(SX,SY,EX,EY,_),
   make_between(X2,SX,EX,X3),
   make_between(Y2,SY,EY,Y3),
   retractall(loc(P1,_,_)),
   assert(loc(P1,X3,Y3)).

to_int(X1,X2):-X2 is round(X1).

move_all:-debugFmt('startring to move_all!'),fail.
move_all:-noun_type(P1,Type),not(is_loc_type(Type)),change_loc_goal(P1),fail.
move_all:-debugFmt('completed to move_all!'),fail.
move_all:-!.


make_between(In,Low,_High,Out):-In < Low,!,Out==Low.
make_between(In,_Low,High,Out):-In > High,!,Out==High.
make_between(In,_Low,_High,In).

change_loc_goal(P1):-
   loc(P1, X1,Y1),!,
   world_range(SX,SY,EX,EY,DistPer20),
   GoDist is random(DistPer20),
   Angle is (random(360)/57.29577951),
   carts_for_polar_ofset(X1,Y1,Angle,GoDist,X2,Y2),
   make_between(X2,SX,EX,X3),
   make_between(Y2,SY,EY,Y3),
   set_loc_goal(P1,X3,Y3),
   debugFmt('~w trying to get from ~w,~w to ~w,~w ~n',[P1, X1,Y1,X3,Y3]).

change_loc_goal(P1):- 
   random_loc(X,Y),
   set_loc(P1, X,Y),
   set_loc_goal(P1,X,Y),!.


carts_for_polar_ofset(X1,Y1,Angle,GoDist,X2,Y2):-X2 is X1 + cos(Angle)*GoDist,Y2 is Y1 + sin(Angle)*GoDist.

%% move_all:-noun_type(P1,Type),


:- start_move_threads.
