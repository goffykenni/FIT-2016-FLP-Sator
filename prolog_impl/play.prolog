% transposed(+A, ?B) iff matrix B is transposed matrix A
transposed(A, B) :- transposed(A, [], B).
transposed(M, X, X) :- empty(M), !.
transposed(M, A, X) :- columns(M, Hs, Ts), transposed(Ts, [Hs|A], X).

% empty(+A) iff A is empty list or a list of empty lists
empty([[]|A]) :- empty(A).
empty([]).

% columns(+M, ?Hs, ?Ts) iff Hs is the first column
%   of matrix M and Ts is the rest of matrix M
columns([[Rh|Rt]|Rs], [Rh|Hs], [Rt|Ts]) :- columns(Rs, Hs, Ts).
columns([[]], [], []).
columns([], [], []).