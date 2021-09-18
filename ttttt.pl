 %____________________________THE START OF GAME_________________________
%IT WILL INITIALIZE THE DATABASE FOR MAKING GAME GROUND

startgame:-
    asserta(gamelist(["","","","","","","","","","","","","","","",""])),
    asserta(turn("1")),asserta(turn2(1)),
    asserta(blocks(20,20)),
    asserta(points([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])),
    write("first player turn").

%____________________________THE END OF THE GAME_______________________
%IT WILL DELETE ALL THE DATABASES

endgame:- retractall(gamelist(_)),
    retractall(turn(_)),
    retractall(blocks(_))
    ,retractall(points(_)).

%____________________________IT WILL FIND THE INDEX BASED ON X & Y
findval(X,Y,N):- A is X*4,N is A+Y.
%_______________________INSERT A NEW BLOCK_____________________________

insertnew(X,Y,_):-
    X>3,X<0,Y>3,Y<0,write("not possible").
insertnew(X,Y,L):-
    findval(X,Y,N),gamelist(L),
    nth0(N,L,Elm),Elm\="",write("already full").
insertnew(X,Y,R):-
    findval(X,Y,N),gamelist(L),turn(A),turn2(A2),
    nth0(N,L,Elm),Elm="",putinplace(L,N,0,A,[],R),
    updategame(R),win(R,A),
    changeval(L,A2,X,Y,R1),updatepoints(R1),
    changeturn(A),decblock(A).

%__________________ put the block to the specified row and culmn
putinplace(_,_,16,_,Tmp,Tmp). %THE END CONDITION
putinplace(List,Pos,Index,Val,Tmp,Res):-
    Index=Pos,
    append(Tmp,[Val],Tmp2),
    Index2 is Index+1,
    putinplace(List,Pos,Index2,Val,Tmp2,Res).
%-- IF IT IS NOT THE  PART OF DATA WE WANT WE JUST COPY IT
putinplace(List,Pos,Index,Val,Tmp,Res):-
    Index\=Pos,
    nth0(Index,List,Tm),
    append(Tmp,[Tm],Tmp2),
    Index2 is Index+1,
    putinplace(List,Pos,Index2,Val,Tmp2,Res).

%___________________________MOVE THE BLOCK ____________________________
%THIS FUNCTION WILL MOVE AN EXISTED BLOCK TO ITS NEAR BLOCK
move(X0,Y0,Dir,Res,Num):-
    calc(X0,Y0,X1,Y1,Dir),
    X0=<3,X0>=0,Y0=<3,Y0>=0,
    X1=<3,X1>=0,Y1=<3,Y1>=0,
    Num>0,Num=<4,
    findval(X0,Y0,N0),findval(X1,Y1,N1), %--FIND THE INDEX OF START AND END OF MOVE
    gamelist(L)
    ,turn(A),
    nth0(N0,L,Elm0),getlast(Elm0,Lastword),Lastword=A,
    string_length(Elm0,Len),Num=<Len,
    nth0(N1,L,Elm1),string_length(Elm1,Len2),Num2 is Len2+Num,Num2=<8, %--THE BLOCK SIZE SHOULDNT BE BIGGER THAN 8
    split(Elm0,Num,L1,L2),string_concat(Elm1,L2,E3),
    putinplace(L,N0,0,L1,[],R1),
    updategame(R1),
    putinplace(R1,N1,0,E3,[],Res),
    updategame(Res),
    assert(lastupdate(Dir,Num,X1,Y1)),
    win(Res,"1"),win(Res,"2").
move(_,_,_,_,_):-write("error"). %-- IF ONE OF THEE EBOVE TERMS WERE FALSE

% _________IF WE WANT TO COUNTINUE THE MOVEMENT OF A SET OF BLOCKS
% if the user wants to move the sets continiously

again(Ans,Num,Res):-
    Ans="yes"
    ,lastupdate(_,N,_,_),
    Num>=N,write("number too big "),
    gamelist(Res),!.
again(Ans,Num,Res):-
    Ans="yes",
    lastupdate(Dir,N,X0,Y0),
    Num<N,retractall(lastupdate(_,_,_,_)),
    move(X0,Y0,Dir,Res,Num),!.
again(Ans):-
    Ans="no",
    retractall(lastupdate(_,_,_,_)),
    turn(A),changeturn(A).

%____________CALCULATE THE ROW AND COLUMN WHEN WE MOVE____________
calc(X0,Y0,X1,Y0,D):-D="up",X1 is X0-1.
calc(X0,Y0,X1,Y0,D):-D="down",X1 is X0+1.
calc(X0,Y0,X0,Y1,D):-D="left",Y1 is Y0-1.
calc(X0,Y0,X0,Y1,D):-D="right",Y1 is Y0+1.

%_____UPDATE THE AMOUNT OF THE DATA BASE______________________________
updategame(X):-
    retractall(gamelist(_)),assertz(gamelist(X)).
updateturn(X):-
    retractall(turn(_)),assertz(turn(X)),write("turn player"),write(X).
updateblocks(X,Y):-
    retractall(blocks(_,_)),asserta(blocks(X,Y)).
updatepoints(X):-
    retractall(points(_)),asserta(points(X)).
%________________CHANGE THE TYPE OF DATA
%CALL IT WHENEVER WE INSERT OR MOVE
changeturn(T):-T="1",updateturn("2"),gamelist(L),givemax(L).
changeturn(T):-T="2",updateturn("1"),gamelist(L),givemin(L).
changeturn2(T):-T=1, retractall(turn2(_)),asserta(turn2(-1)).
changeturn2(T):- T=-1, retractall(turn2(_)),asserta(turn2(1)).
%________________DECREMENT OF THE BLOCKS IN EACH INSERSION
decblock(T):-T="1",blocks(A,B),X is A-1,updateblocks(X,B).
decblock(T):-T="2",blocks(A,B),X is B-1,updateblocks(A,X).


% _____DETERMINE IF THE USERIS WIN OR NOT
copyl(L):-assertz(copylist(L)).%change the name
ens:-retractall(copylist(_)).%change the name

rowrecognizer(X,Y,_):-X>=4,X<0,Y>=4,Y<0,!.
rowrecognizer(X,Y,C):-
    X=3,
    copylist(L),
    findval(X,Y,N), nth0(N,L,Elm),
    getlast(Elm,Last),Last=C,writeln("win"),endgame,abort.
rowrecognizer(X,Y,C):-
    copylist(L),
    findval(X,Y,N),nth0(N,L,Elm),
    getlast(Elm,C2),C=C2,retractall(copylist(_)),
    putinplace(L,N,0,"Z",[],R),
    assertz(copylist(R)),
    calc(X,Y,X1,Y1,"down"),rowrecognizer(X1,Y1,C),
    calc(X,Y,X2,Y2,"left"),rowrecognizer(X2,Y2,C),
    calc(X,Y,X3,Y3,"right"),rowrecognizer(X3,Y3,C).
rowrecognizer(_,_,_):-!.

%------------------
colrecognizer(X,Y,_):-X>=4,X<0,Y>=4,Y<0,!.
colrecognizer(X,Y,C):-
    Y=3,
    copylist(L),
    findval(X,Y,N),nth0(N,L,Elm),
    getlast(Elm,C2),C=C2,writeln("win"),abort.
colrecognizer(X,Y,C):-
    copylist(L),
    findval(X,Y,N),nth0(N,L,Elm),
    getlast(Elm,C2),C=C2,retractall(copylist(_)),
    putinplace(L,N,0,"Z",[],R),assertz(copylist(R)),
    calc(X,Y,X1,Y1,"down"),colrecognizer(X1,Y1,C),
    calc(X,Y,X2,Y2,"up"),colrecognizer(X2,Y2,C),
    calc(X,Y,X3,Y3,"right"),colrecognizer(X3,Y3,C).
colrecognizer(_,_,_):-!.
% ______the equlity one of the users are out o blocks
tie:-blocks(0,_),writeln("tie"),endgame,abort.
tie:-blocks(_,0),writeln("tie"),endgame,abort.
tie:-!.
%_____________________________
checkwin(List,T):-
    checkrows(List,0,0,T),checkrows(List,0,1,T),checkrows(List,0,2,T),checkrows(List,0,3,T),
    checkcols(List,0,0,T),checkcols(List,1,0,T),checkcols(List,2,0,T),checkcols(List,3,0,T),
    tie.
win(List,Turn):-checkwin(List,Turn).%change the name
checkrows(List,X,Y,T):-copyl(List),rowrecognizer(X,Y,T),ens.
checkcols(List,X,Y,T):-copyl(List),colrecognizer(X,Y,T),ens.

%_______________________Huristic___________________________________
compute(_,_,16,_,Tmp,Tmp). %THE END CONDITION
compute(List,Pos,Index,Val,Tmp,Res):-
    Index=Pos,
    nth0(Index,List,TM),
    TM2 is TM + Val,
    append(Tmp,[TM2],Tmp2),
    Index2 is Index+1,
    compute(List,Pos,Index2,Val,Tmp2,Res).
%-- IF IT IS NOT THE  PART OF DATA WE WANT WE JUST COPY IT
compute(List,Pos,Index,Val,Tmp,Res):-
    Index\=Pos,
    nth0(Index,List,Tm),
    append(Tmp,[Tm],Tmp2),
    Index2 is Index+1,
    compute(List,Pos,Index2,Val,Tmp2,Res).

changeval(L,N,X,Y,R):- calc(X,Y,X1,Y1,"up"),calc(X,Y,X2,Y2,"down"),
    calc(X,Y,X3,Y3,"right"),calc(X,Y,X4,Y4,"left"),
    findval(X,Y,N),findval(X1,Y1,N1),findval(X2,Y2,N2),findval(X3,Y3,N3),
    findval(X4,Y4,N4),

     chanegeck(L,X1,Y1,R1,N),chanegeck(R1,X2,Y2,R2,N),chanegeck(R2,X3,Y3,R3,N),
    chanegeck(R3,X4,Y4,R4,N),chanegeck(R4,X,Y,R,N).
chanegeck(L,P,V,R):-X<0.
chanegeck(L,P,V,L):-X>15.
chanegeck(List,Pos,Val,Res):-(List,Pos,0,Val,[],Res).

bubble_sort(List,Sorted):-b_sort(List,[],Sorted).
b_sort([],Acc,Acc).
b_sort([H|T],Acc,Sorted):-bubble(H,T,NT,Max),b_sort(NT,[Max|Acc],Sorted).

bubble(X,[],[],X).
bubble(X,[Y|T],[Y|NT],Max):-X>Y,bubble(X,T,NT,Max).
bubble(X,[Y|T],[X|NT],Max):-X=<Y,bubble(Y,T,NT,Max).

 traverse(Elm,0,0):-gamelist(L),nth0(0,L,E),E=Elm,write("0,0 "),traverse(Elm,0,1).
 traverse(Elm,0,0):-traverse(Elm,0,1).
 traverse(Elm,0,1):-gamelist(L),nth0(0,L,E),E=Elm,write("0,1 "),traverse(Elm,0,2).
 traverse(Elm,0,1):-traverse(Elm,0,2).
 traverse(Elm,0,2):-gamelist(L),nth0(0,L,E),E=Elm,write("0,2 "),traverse(Elm,0,3).
 traverse(Elm,0,2):-traverse(Elm,0,3).
 traverse(Elm,0,3):-gamelist(L),nth0(0,L,E),E=Elm,write("0,3 "),traverse(Elm,1,0).
 traverse(Elm,0,3):-traverse(Elm,1,0).
 traverse(Elm,1,0):-gamelist(L),nth0(0,L,E),E=Elm,write("1,0 "),traverse(Elm,1,1).
 traverse(Elm,1,0):-traverse(Elm,1,1).
 traverse(Elm,1,1):-gamelist(L),nth0(0,L,E),E=Elm,write("1,1 "),traverse(Elm,1,2).
 traverse(Elm,1,1):-traverse(Elm,1,2).
 traverse(Elm,1,2):-gamelist(L),nth0(0,L,E),E=Elm,write("1,2 "),traverse(Elm,1,3).
 traverse(Elm,1,2):-traverse(Elm,1,3).
 traverse(Elm,1,3):-gamelist(L),nth0(0,L,E),E=Elm,write("1,3 "),traverse(Elm,2,0).
 traverse(Elm,1,3):-traverse(Elm,2,0).
 traverse(Elm,2,0):-gamelist(L),nth0(0,L,E),E=Elm,write("2,0 "),traverse(Elm,2,1).
 traverse(Elm,2,0):-traverse(Elm,2,1).
 traverse(Elm,2,1):-gamelist(L),nth0(0,L,E),E=Elm,write("2,1 "),traverse(Elm,2,2).
 traverse(Elm,2,1):-traverse(Elm,2,2).
 traverse(Elm,2,2):-gamelist(L),nth0(0,L,E),E=Elm,write("2,2 "),traverse(Elm,2,3).
 traverse(Elm,2,2):-traverse(Elm,2,3).
 traverse(Elm,2,3):-gamelist(L),nth0(0,L,E),E=Elm,write("2,3 "),traverse(Elm,3,0).
 traverse(Elm,2,3):-traverse(Elm,3,0).
 traverse(Elm,3,0):-gamelist(L),nth0(0,L,E),E=Elm,write("3,0 "),traverse(Elm,3,1).
 traverse(Elm,3,0):-traverse(Elm,3,1).
 traverse(Elm,3,1):-gamelist(L),nth0(0,L,E),E=Elm,write("3,1 "),traverse(Elm,3,2).
 traverse(Elm,3,1):-traverse(Elm,3,2).
 traverse(Elm,3,2):-gamelist(L),nth0(0,L,E),E=Elm,write("3,2 "),traverse(Elm,3,3).
 traverse(Elm,3,2):-traverse(Elm,3,3).
 traverse(Elm,3,3):-gamelist(L),nth0(0,L,E),E=Elm,write("3,3 "),!.
 traverse(_,3,3):-!.










 givemax(List):-bubble_sort(List,Sorted),nth0(15,Sorted,Elm),
 traverse(Elm,0,0).
 givemin(List):-bubble_sort(List,Sorted),nth0(0,Sorted,Elm),
 traverse(Elm,0,0).

%_______________________THE PREMADE FUNCTIONS______________________
%-- IT WILL SPLIT THE LIST IN TO TWO PEICES
split(S,In,L1,L2):-
    string_length(S,L),Index is L-In,sub_string(S,0,Index,_,L1),I2 is L-Index,sub_string(S,Index,I2,_,L2).

%--IT WILL GET THE LAST ELEMENT OF A STRING
getlast(St,Elm):-
    atom_string(Atom,St),sub_atom(Atom, _, 1, 0, E),atom_string(E,Elm).
%



