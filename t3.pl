% initMyData(+NBoxesToPick, -ProgramDataInitial).
initMyData(N, Action):- Action = [N | [none]], assert(mycoords(0,0)), assert(semafor(0)), assert(contor(1)), assert(latura(0)), assert(schimb(0)).

% perform(+ProgramData, +ContainsBox, -Action, -ProgramDataUpdated)

% daca sunt in done, retrag tot
perform([H|_], _, done, _):- H=0,mycoords(0, 0) ,semafor(X), X = 0, retractall(mycoords(_,_)), retractall(semafor(_)), retractall(contor(_)), retractall(latura(_)), retractall(schimb(_)).

% daca am ridicat cutia, si am ajuns in (0, 0) urmatoarea actiune este sa facem deliverBox și plec din starea initiala
perform([H|_], _, deliverBox, [H1|_]):- semafor(X), X = 1, mycoords(0, 0), retractall(semafor(_)), assert(semafor(0)), H1 is H-1, retractall(latura(_)), retractall(contor(_)), retractall(schimb(_)), assert(contor(1)), assert(latura(0)), assert(schimb(0)).

% prima oara plec in nord.
perform([H|_], false, move(north), [H|[move(north)]]):- H > 0, semafor(X), X = 0, mycoords(0, 0), latura(P), P1 is P+2,retractall(contor(_)), retractall(semafor(_)), retractall(mycoords(_, _)),retractall(moveme(_)), retractall(latura(_)), assert(contor(0)), assert(mycoords(0, 1)), assert(semafor(0)), assert(moveme(1)), assert(latura(P1)).

% aici e pasul la care schimb, dupa ce am facut o rotatie completa. merg in nord daca ultima mutare a fost est
perform([H|_], false, move(north), [H|[move(east)]]):- H > 0 , semafor(X), X = 0, moveme(D), D = 2, mycoords(Z, Y), contor(A), A=0, schimb(G), G > 0, M is mod(G, 2), M = 0, latura(P), P1 is P + 2, retractall(moveme(_)), retractall(latura(_)),retractall(mycoords(_, _)), assert(latura(P1)), assert(contor(0)), Y1 = Y +1, assert(mycoords(Z, Y1)), assert(semafor(0)), assert(moveme(1)).

% aici merg in est, daca ultima mea mutare a fost nord
perform([H|_], false, move(east), [H|[move(north)]]):- H > 0, semafor(X), X = 0, moveme(D), D = 1, mycoords(Z, Y), contor(A), A = 0, latura(P), P1 is (P/2 - 1), schimb(I), I1 is I+1, retractall(schimb(_)), assert(schimb(I1)), retractall(contor(_)), retractall(semafor(_)), retractall(mycoords(_, _)), retractall(moveme(_)), assert(moveme(2)), Z1 is Z + 1, assert(mycoords(Z1, Y)), assert(semafor(0)), assert(contor(P1)).


% merg in est daca ultima mea mutare a fost est
perform([H|_], false, move(east), [H|[move(east)]]):- H > 0, semafor(X), X = 0, moveme(D), D = 2, mycoords(Z, Y), contor(A), A > 0, P1 is (A - 1), retractall(contor(_)), retractall(semafor(_)), retractall(mycoords(_, _)), retractall(moveme(_)), assert(moveme(2)), Z1 is Z + 1, assert(mycoords(Z1, Y)), assert(semafor(0)), assert(contor(P1)).


% merg in sud daca ultima mea mutare a fost est
perform([H|_], false, move(south), [H|[move(east)]]):- H > 0, semafor(X), X = 0, moveme(D), D = 2, mycoords(Z, Y), contor(A), A = 0, latura(P), P1 is (P - 1), retractall(contor(_)), retractall(semafor(_)), retractall(mycoords(_, _)), retractall(moveme(_)), assert(moveme(3)), Y1 is Y - 1, assert(mycoords(Z, Y1)), assert(semafor(0)), assert(contor(P1)).

% merg in sud daca ultima mea mutare a fost sud
perform([H|_], false, move(south), [H|[move(south)]]):- H > 0, semafor(X), X = 0, moveme(D), D = 3, mycoords(Z, Y), contor(A), A > 0, P1 is (A - 1), retractall(contor(_)), retractall(semafor(_)), retractall(mycoords(_, _)), retractall(moveme(_)), assert(moveme(3)), Y1 is Y - 1, assert(mycoords(Z, Y1)), assert(semafor(0)), assert(contor(P1)).


% merg in vest daca ultima mutare a fost sud
perform([H|_], false, move(west), [H|[move(south)]]):- H > 0, semafor(X), X = 0, moveme(D), D = 3, mycoords(Z, Y), contor(A), A = 0, latura(P), P1 is (P - 1), retractall(contor(_)), retractall(semafor(_)), retractall(mycoords(_, _)), retractall(moveme(_)), assert(moveme(4)), Z1 is Z - 1, assert(mycoords(Z1, Y)), assert(semafor(0)), assert(contor(P1)).


% merg in vest daca ultima mutare a fost vest
perform([H|_], false, move(west), [H|[move(west)]]):- H > 0, semafor(X), X = 0, moveme(D), D = 4, mycoords(Z, Y), contor(A), A > 0, P1 is (A - 1), retractall(contor(_)), retractall(semafor(_)), retractall(mycoords(_, _)), retractall(moveme(_)), assert(moveme(4)), Z1 is Z - 1, assert(mycoords(Z1, Y)), assert(semafor(0)), assert(contor(P1)).


% merg in nord daca ultima mutare a fost vest
perform([H|_], false, move(north), [H|[move(west)]]):- H > 0, semafor(X), X = 0, moveme(D), D = 4, mycoords(Z, Y), contor(A), A = 0, latura(P), P1 is (P - 1), retractall(contor(_)), retractall(semafor(_)), retractall(mycoords(_, _)), retractall(moveme(_)), assert(moveme(1)), Y1 is Y + 1, assert(mycoords(Z, Y1)), assert(semafor(0)), assert(contor(P1)).

% merg in nord daca ultima mutare a fost nord
perform([H|_], false, move(north), [H|[move(north)]]):- H > 0, semafor(X), X = 0, moveme(D), D = 1, mycoords(Z, Y), contor(A), A > 0, P1 is (A - 1), retractall(contor(_)), retractall(semafor(_)), retractall(mycoords(_, _)), retractall(moveme(_)), assert(moveme(1)), Y1 is Y + 1, assert(mycoords(Z, Y1)), assert(semafor(0)), assert(contor(P1)).


% daca am pickBox, vreau sa merg withBox pana la 0, 0. Diferite cazuri in care verific daca (X, Y) sunt mai mari ca 0. 
perform([H|_], _, moveWithBox(south),[H|_]):- H > 0, semafor(X), X=1, mycoords(Z, Y), Y > 0, Y1 is Y-1, retractall(mycoords(_, _)), assert(mycoords(Z, Y1)).
perform([H|_], _, moveWithBox(north),[H|_]):- H > 0, semafor(X), X=1, mycoords(Z, Y), Y < 0, Y1 is Y+1, retractall(mycoords(_, _)), assert(mycoords(Z, Y1)).
perform([H|_], _, moveWithBox(west),[H|_]):- H > 0, semafor(X), X=1, mycoords(Z, Y), Z > 0, Z1 is Z-1, retractall(mycoords(_, _)), assert(mycoords(Z1, Y)).
perform([H|_], _, moveWithBox(est),[H|_]):- H > 0, semafor(X), X=1, mycoords(Z, Y), Z < 0, Z1 is Z+1, retractall(mycoords(_, _)), assert(mycoords(Z1, Y)).


% daca am box, fac pick la ea. Nu conteaza unde sunt
perform([H|_], true, pickBox, [H|_]):- H > 0, semafor(X), X = 0, mycoords(_, _), retractall(semafor(_)),retractall(moveme(_)), assert(semafor(1)).

% pentru bonus:
% perform(+ProgramData, +ContainsBox, +AvailableDirections,
%                                   -Action, -ProgramDataUpdated)
perform(_, _, _, done, _).
