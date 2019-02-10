
%---------------make_column_3---------------%
%Function.  Arguments: your list, your lists, your term, your accumulator.   
%Format: make_column(Column, Rows, N, Accumulator)
%Test Case: make_column(Col,[[2, 3, 4, 5, 1], [5, 4, 1, 3, 2], [4, 1, 5, 2, 3], [1, 2, 3, 4, 5], [3, 5, 2, 1, 4]], 1, []). Should return [2, 5, 4, 1, 3]
%Test Case2: make_column(Col,[[2, 3, 4, 5, 1], [5, 4, 1, 3, 2], [4, 1, 5, 2, 3], [1, 2, 3, 4, 5], [3, 5, 2, 1, 4]], 5, []). Should return [1, 2, 3, 5, 4].
%Notes: Appears to work properly.  Will give you one top column, it appears.  Also, it appears you always need some kind of accumulator.  

make_column(_, _, 0, _).
make_column(Col, [], _, Col). 
make_column(Col, [Row|Rows], N, Acc):-
        N>0,    
	nth(N, Row, E),
	append(Acc, [E], L),%!,
	make_column(Col, Rows, N, L). 

%---------------make_row--------------- %
%Function.  Arguments: your size, your row
%Format: make_row(N, L)
%Test Case: make_row(5, L).  Should return some iteration of 1 2 3 4 5

make_row(N,L):-        
        N>0,
	length(L, N),
	fd_domain(L, 1, N),
	fd_all_different(L).

%---------------make_rows---------------%
%Function.  Arguments: your rows, your list size, your size, Acc  
%Format: make_rows(Rows, Perm, Term, Acc)
%Test Case: make_rows(List, 5, 5, []).


make_rows(Rows, _, 0, Rows).
make_rows(Rows, Perm, N, Acc):-	
	N>0,
 	N1 is N-1, make_row(Perm, L),
	append(Acc, [L], A), 
        make_rows(Rows, Perm, N1, A).

%---------------make_grid---------------%
%Function.  Arguments: Your new grid, your current grid (from make_rows), your perm term, your regular term, and your accumulator
%Format: make_grid(Grid, Rows, Perm, Term, Acc)
%Test Case:

make_grid(Grid, _, _, 0, Grid).
    %:- length(Grid, Perm).
make_grid(Grid, Rows, Perm, N, Acc):-       
    
    N\=0,
    N=<Perm,
	make_column(Col, Rows, N, _),  
        fd_domain(Col, 1, Perm),
%        fd_all_different(Col),
        catch( fd_all_different(Col), _, fail),!,
%        fd_labeling(Col),    
	append(Acc, [Col], A),
	N1 is N-1,
	make_grid(Grid, Rows, Perm, N1, A).  

%---------------the_Grid---------------%
%Wrapper function for make_grid, make_rows, and make_grid.  Arguments: Your new grid, your current grid (from make_rows), your perm 
%Format: the_Grid(Grid, N)

the_Grid(Grid, N):-
	make_rows(Rows, N, N, []),
        length(Grid, N),
    make_grid(Grid, Rows, N, N, []),
    maplist(fd_labeling, Grid).
%    transpose(Grid, Matrix, N, []).

%	fd_labeling(Grid). 




%--------------row_count-------------------%
    

row_count(A, [], _, A).
row_count(A, [X|Z], Y, C):-
	X>Y, C1 is C+1, row_count(A, Z, X, C1).
row_count(A ,[_|Z], Y, C):-
	row_count(A, Z, Y, C).


%---------------edge_count---------------%
%Function: Arguments: your count, your Rows, your accumulator
%Test Case: left(Count, [[2,3,4,5,1], [5,4,1,3,2], [4,1,5,2,3],[1,2,3,4,5],[3,5,2,1,4]], []).  Should return: [2,3,2,1,4]

edge_count(Count, [], Count).
edge_count(Count, [Row|Rows], Acc):-
	row_count(X, Row, 0, 0),
	append(Acc, [X], A),
    edge_count(Count, Rows, A).

%-------------- reverse_count---------------%
reverse_count(Reversed, [], Reversed).
    reverse_count(Reversed, [Row|Rows], Acc):-
	reverse(Row, R1),
	row_count(X, R1, 0, 0),
	append(Acc, [X], A),
	reverse_count(Reversed, Rows, A).
%-------------- transpose---------------%
transpose(Cols, _, 0, Cols).
transpose(Cols, Rows, N, Acc):-
	make_column(Col, Rows, N, []),
	append([Col], Acc, A),
	N1 is N-1,
	transpose(Cols, Rows, N1, A).

 %-------------- total_edge---------------%
total_edge(N, Matrix, counts(R1, R2, R3, R4)):-
	edge_count(R3, Matrix, Acc),
	reverse_count(R4, Matrix, Acc),
	transpose(Reverse, Matrix, N, Acc),
	edge_count(R1, Reverse, Acc),
	reverse_count(R2, Reverse, Acc),!.

%-------------- tower ---------------%
tower(N, T, C):-
	the_Grid(T, N),
	total_edge(N, T, Temp), 
C=Temp.  


/*%-----------------------RE_WRITE------------------------%
  
%------------make_rows-------------%
% Function: makes your grid (no column checking yet, follows essentially same rules as plain).  Format: (Rows, N, Accumulator)

make_rows(Rows, Max, 0, Rows):.
make_rows(Rows, Max, N, Acc):-
	length(List, Max),
	fd_domain(List, 1, Max),
        fd_all_different(List),
 	append(Acc, [List], A),
        N1 is N-1,
        make_rows(Rows,Max, N1, A)

.

%------------grid_check-------------%
% Function: Essentially checks to see if your entire grid is valid.  Format: (Iterator, N, Grid)

grid_check(_, 0, _,_).
grid_check(Iter, N, Grid, New_Grid):-
        N>0,
	make_column(List, Grid, Iter, []),
	fd_all_different(List),
        fd_labeling(List),
     
        Iter1 is Iter+1,
        N1 is N-1,    
	grid_check(Iter1, N1, Grid, New_Grid).

%------------tower-------------%
% Function: duh
tower(N, T, C):-
        make_rows(T,N,  N, []),
%	grid_check(1, N, T),!,
%	fd_labeling(T),
 %       catch(fd_labeling(T),_, fail),
        total_edge(N, T, C).
	
  */  

 %-----------------------PLAIN-------------------%

%------------domain-------------%
%Function: should output a list of distinct numbers in your domain
%domain(List, 5, 1, []).


domain(Dom, 0, _, Dom):-!.
domain(List, N, I, Acc):-
	append(Acc, [I], A),
	N1 is N-1,
	I1 is I+1,
	domain(List, N1, I1, A).

%------------column_check-------------%

column_check([], _, _, _).
column_check([H|T], Iter, N, Prev):-
        make_column(List, Prev, Iter, []),
        \+(memberchk(H, List)),
        Iter1 is Iter+1,
        column_check(T, Iter1, N, Prev).


 
%------------plain_rows-------------%

% Arguments: N, Matrix
    %Format: plain_rows(Grid, N, Incrementor, Max, AccGrid, Term, Incrementor, Accumulator
    % Perm: a permutation.
    
plain_rows(Grid, _, 0, _, Grid).
plain_rows(Grid, Max, N, List, Acc):-
	permutation(List, Perm),
	column_check(Perm, 1, Max, Acc),
	append(Acc, [Perm], A),
	N1 is N-1,		
	plain_rows(Grid, Max, N1, List, A). 
  
%------------plain_grid-------------%

plain_grid(Grid, N):-
	domain(List, N, 1, []), !,
			plain_rows(Grid, N, N, List, []).
			
%------------plain-------------%
plain_tower(N, T, C):-
	plain_grid(T, N),
	total_edge(N, T, Temp),
C=Temp.
			
%--------------------------AMBIGUOUS-----------------------------%
%ambiguous(N, C, T1, T2):-

ambiguous(N, C, T1, T2):-
        tower(N, T1, C),			
	tower(N, T2, C),
	T1\=T2,!.

			
%-----------speedup--------------%
speedup(R):-
			godspeed(A), godspeed(B), godspeed(C), godspeed(D), godspeed(E), godspeed(F), godspeed(G),
			swiftplain(H), 	swiftplain(I), 	swiftplain(J), 	swiftplain(K), 	swiftplain(L), 	swiftplain(M), 	swiftplain(N),
			Tower is (A + B + C + D + E + F + G)/7,
			Plain is (H + I + J + K + L + M + N)/7,
			R is Plain/Tower, !.


	
godspeed(T):-
        statistics(cpu_time, [A,_]),
        tower(7, _, _),
        statistics(cpu_time, [B,_]),
	T is B-A.

swiftplain(P):-
	statistics(cpu_time, [A,_]),
	plain_tower(7, _, _),
        statistics(cpu_time, [B,_]),
        P is B-A.


		
