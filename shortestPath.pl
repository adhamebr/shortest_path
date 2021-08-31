%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% right

move([Grid,X,Y,Color,GX,GY],[Grid2,X2,Y2,Color,GX,GY]):-
	right([Grid,X,Y,Color,GX,GY],[Grid2,X2,Y2,Color,GX,GY]).



right([Grid,X,Y,Color,GX,GY],[Grid1,X2,Y2,Color,GX,GY]):-
    Y2 is Y+1,
    X2 is X+0,
    Y2<GY,
    Y2>=0,
    X2<GX,
    X2>=0,
    ColorIndex1 is X2*GY+Y2,
    element_at(Color,ColorIndex1,Grid,Grid1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% left

move([Grid,X,Y,Color,GX,GY],[Grid2,X2,Y2,Color,GX,GY]):-

	left([Grid,X,Y,Color,GX,GY],[Grid2,X2,Y2,Color,GX,GY]).



left([Grid,X,Y,Color,GX,GY],[Grid1,X2,Y2,Color,GX,GY]):-
    Y2 is Y-1,
    X2 is X+0,
    Y2<GY,
    Y2>=0,
    X2<GX,
    X2>=0,
    ColorIndex1 is X2*GY+Y2,
    element_at(Color,ColorIndex1,Grid,Grid1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% up

move([Grid,X,Y,Color,GX,GY],[Grid2,X2,Y2,Color,GX,GY]):-

	up([Grid,X,Y,Color,GX,GY],[Grid2,X2,Y2,Color,GX,GY]).



up([Grid,X,Y,Color,GX,GY],[Grid1,X2,Y2,Color,GX,GY]):-
    Y2 is Y+0,
    X2 is X-1,
    Y2<GY,
    Y2>=0,
    X2<GX,
    X2>=0,
    ColorIndex1 is X2*GY+Y2,
    element_at(Color,ColorIndex1,Grid,Grid1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% down

move([Grid,X,Y,Color,GX,GY],[Grid2,X2,Y2,Color,GX,GY]):-

	down([Grid,X,Y,Color,GX,GY],[Grid2,X2,Y2,Color,GX,GY]).



down([Grid,X,Y,Color,GX,GY],[Grid1,X2,Y2,Color,GX,GY]):-
    Y2 is Y+0,
    X2 is X+1,
    Y2<GY,
    Y2>=0,
    X2<GX,
    X2>=0,
    ColorIndex1 is X2*GY+Y2,
    element_at(Color,ColorIndex1,Grid,Grid1).






removeFirst([_|T],T):-!.

firstE([H|T],H).
secondE([F,S|T],S).



solve():-
    findall(X,dot(T,X),List),
    dot(Color,Z),
    grid(R,C),
    createGrid(R,C,['_'],Grid),

    firstE(List,FirstColor),
    secondE(List,SecColor),

    firstE(FirstColor,ColorRow1),
    secondE(FirstColor,ColorCol1),

    firstE(SecColor,ColorRow2),
    secondE(SecColor,ColorCol2),

    ColorIndex1 is ColorRow1*C+ColorCol1,
    element_at(Color,ColorIndex1,Grid,Grid1),
    ColorIndex2 is ColorRow2*C+ColorCol2,

    element_at(Color,ColorIndex2,Grid1,Grid2),
    Dif1=abs(ColorRow1-ColorRow2),
    Dif2=abs(ColorCol1-ColorCol2),
    Distance is Dif1+Dif2,

    go([ColorRow1,ColorCol1], [ColorRow2,ColorCol2],Grid2,[R,C],Color),!.

 %   findall([Grid3,X2,Y2,Color,GX,GY], move([Grid2,ColorRow1,ColorCol1,Color,R,C],[Grid3,X2,Y2,Color,GX,GY]), Children) ,
%	write(Children),!.


    %printGrid(R,C,Grid2),!.

go(Start,Goal,Grid,Dim,Color):-
		getHeuristic(Start, H, Goal),
		path([[Grid,Start,null, 0, H, H]],[],Goal,Dim,Color).%open, closed, goal, path_cost, heuristic, total cost , Grid


getHeuristic([R1,C1],H,[R2,C2]):-
    Dif1=abs(R1-R2),
    Dif2=abs(C1-C2),
    H is Dif1+Dif2.



path([], _, _,_,_,_):-

		write('No solution'),nl,!.

path(Open, Closed, Goal,Dim,Color):-

		getBestChild(Open, [Grid,Goal, Parent, PC, H, TC], RestOfOpen),

		write('A solution is found'),  nl ,
	firstE(Dim,R),
	secondE(Dim,C),
	write("Steps = "),nl,
	reverse(Closed,Closed1),
	printClosed(R,C,Closed1),
	write("Goal = "),nl,
	printGrid(R,C,Grid),!.
		%printsolution([Grid,Goal,Parent, PC, H, TC], Closed,Dim),!.

path(Open, Closed, Goal,Dim,Color):-

		getBestChild(Open, [Grid,State, Parent, PC, H, TC], RestOfOpen),

		getchildren(State, Open, Closed, Children, PC, Goal,Dim,Grid,Color),

		addListToOpen(Children , RestOfOpen, NewOpen),

		path(NewOpen, [[Grid,State, Parent, PC, H, TC] | Closed], Goal,Dim,Color).

addListToOpen(Children, [], Children).

addListToOpen(Children, [H|Open], [H|NewOpen]):-

		addListToOpen(Children, Open, NewOpen).



getchildren(State, Open ,Closed , Children, PC, Goal,Dim,Grid,Color):-

		findall(X, moves( State,Grid, Open, Closed, X, PC, Goal,Dim,Color), Children) .


moves( State,Grid, Open, Closed,[Grid2,[X2,Y2],State, NPC, H, TC], PC, Goal,[Nrow,Ncol],Color):-

		%move(State,Next),
        firstE(State,ColorRow1),
	secondE(State,ColorCol1),
		move([Grid,ColorRow1,ColorCol1,Color,Nrow,Ncol],[Grid2,X2,Y2,Color,GX,GY]) ,
		\+ member([Grid2,[X2,Y2], _, _, _, _],Open),

		\+ member([Grid2,[X2,Y2], _, _, _, _],Closed),

		NPC is PC + 1,

	getHeuristic([X2,Y2], H, Goal),

		TC is NPC + H.



getBestChild([Child], Child, []).

getBestChild(Open, Best, RestOpen):-

	getBestChild1(Open, Best),

	removeFromList(Best, Open, RestOpen).


removeFromList(_, [], []).

removeFromList(H, [H|T], V):-

	!, removeFromList(H, T, V).

removeFromList(H, [H1|T], [H1|T1]):-

	removeFromList(H, T, T1).


%gets the best state of the open list

getBestChild1([State], State).

getBestChild1([State|Rest], Best):-

	getBestChild1(Rest, Temp),

	getBest(State, Temp, Best).

getBest([Grid,State, Parent, PC, H, TC], [_,_, _, _, _, TC1], [Grid,State, Parent, PC, H, TC]):-

	TC < TC1, !.

getBest([_,_, _, _, _, _], [Grid1,State1, Parent1, PC1, H1, TC1], [Grid1,State1, Parent1, PC1, H1, TC1]).

index([V|_],0,V):-!.
index([_|T],I,V) :-
    I1 is I-1,
    index(T,I1,V).

element_at(E, 0, [_|Ls], [E|Ls]).
element_at(E, X, [L|Ls0], [L|Ls]) :-
    X1 is X-1,
    element_at(E, X1, Ls0, Ls).
assign(X,X).
printGrid(R,C,G):-
    assign(G,G1),
    Size is R*C,
    printGrid1(R,C,0,Size,G1).

printCol(0,_,_,_):-!.
printCol(C,Ctr,Size,Grid):-
    C1 is C-1,
    index(Grid,Ctr,I),
    write(I),
    write(' '),
    Ctr1 is Ctr+1,
    printCol(C1,Ctr1,Size,Grid).


printGrid1(0,_,_,_,_):-!.
printGrid1(R,C,Ctr,Size,Grid):-
    R1 is R-1,
    printCol(C,Ctr,Size,Grid),
    nl,
	Ctr1 is Ctr+C,
    printGrid1(R1,C,Ctr1,Size,Grid).


createGrid(R,C,Shape,L):-
    S is R*C,
    createGrid1(S,Shape,L2),
    removeFirst(L2,L).
append2([],L2,L2).
append2([H|T],L2,[H|R]):-
    append2(T,L2,R).


createGrid1(0,Shape,[Shape]):-!.

createGrid1(Size,Shape,G):-
	Size1 is Size-1,
    createGrid1(Size1,Shape,G1),
    append2(G1,Shape,G3),
    assign(G3,G).

printClosed(R,C,[]):-!.
printClosed(R,C,[[Grid,Start, End, PC, H, TC]|T]):-
    printGrid(R,C,Grid),nl,
    printClosed(R,C,T),!.












