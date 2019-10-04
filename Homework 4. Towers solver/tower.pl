subset([], _).
subset([E|R], Set) :-
    select(E, Set, L),
    subset(R, L).

perm([],[]).
perm([E|R],L):- select(E,L,T),perm(R,T).

ordered( []      ) .
ordered( [_]     ) .
ordered( [X,Y|Z] ) :- X =< Y , ordered( [Y|Z] ) .

range(L, N):- range1(N, [], L).
range1(0, L, L) :- !.
range1(N, R, L) :- N > 0, N1 is N-1, range1(N1, [N|R], L).

compress([], [], _).
compress(Xs, [X|S], N):- X =< N, compress(Xs, S, N).
compress([X|Xs], [X|S], N):- X > N, compress(Xs, S, X).

fd_compress([], [], _).
fd_compress(Xs, [X|S], N):- X #=< N, fd_compress(Xs, S, N).                                                                       
fd_compress([X|Xs], [X|S], N):- X #> N, fd_compress(Xs, S, X).                                                                    

% return true if L1 is the first ascending sublist with length N
f_a_s([H|T1], [H|T2]) :-
subset(T1,T2),
ordered([H|T1]).

plain_tower_row(L,N,C1,C2):-
range(D,N),perm(L,D),compress(S1,L,0),length(S1,C1),reverse(L,R),compress(S2,R,0),length(S2,C2).

plain_t_rows([],_,[],[]).
plain_t_rows([L|LL],N,[C1|LC],[C2|RC]) :- plain_tower_row(L,N,C1,C2), plain_t_rows(LL,N,LC,RC).

column([],[],[]).
column([[H|T]|TT],[T|RT],[H|C]):- column(TT,RT,C).

cols(_,[],[]).
cols(LL, [_|L],[R|RR]):-  column(LL,RL,R), cols(RL,L,RR).

transpose([],[]).
transpose([L|LL], R) :- cols([L|LL],L,R).

plain_tower(N,T,C):- counts(U, D, L, R) = C, length(U,N),length(D,N),length(L,N),length(R,N),length(T,N),plain_t_rows(T,N,L,R),tr\
anspose(T,TT), plain_t_rows(TT,N,U,D).

tower_row(L,N,C1,C2):-
length(L,N),fd_domain(L,1,N),fd_all_different(L),fd_compress(S1,L,0),length(S1,C1),reverse(L,R),fd_compress(S2,R,0),length(S2,C2)\
,fd_labeling(L).

t_rows([],_,[],[]).
t_rows([L|LL],N,[C1|LC],[C2|RC]) :- tower_row(L,N,C1,C2),t_rows(LL,N,LC,RC).


tower(N,T,C):- counts(U, D, L, R) = C, length(U,N),length(D,N),length(L,N),length(R,N),length(T,N),t_rows(T,N,L,R),transpose(T,TT\
), t_rows(TT,N,U,D).

speedup(RT):-
statistics(cpu_time,[_,_]),
tower(6,[[X11,X12,X13,X14,2,X16],[X21,3,X23,X24,X25,5],X3,[4,5,3,6,1,2],[X51,X52,1|X5],X6],counts([1,3,2,3,2,3],[3,2,3,2,2,1],[1,\
4,3,3,2,4],[5,2,4,2,2,1])),
statistics(cpu_time,[_,T1]),
plain_tower(6,[[Y11,Y12,Y13,Y14,2,Y16],[Y21,3,Y23,Y24,Y25,5],Y3,[4,5,3,6,1,2],[Y51,Y52,1|Y5],Y6],counts([1,3,2,3,2,3],[3,2,3,2,2,\
1],[1,4,3,3,2,4],[5,2,4,2,2,1])),
statistics(cpu_time,[_,T2]),
RT is T2/T1.


ambiguous(N, C, T1, T2):- tower(N,T1,C),tower(N,T2,C),T1\=T2.

