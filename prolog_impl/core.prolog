%=== i_sqrt(+N, ?K), true if K * K = N for positive integers N, K.
i_sqrt(N, Root) :- 
  number(N),
  0 < N,
  i_sqrt(N, 1, 1, Root).

i_sqrt(N, N, Root, Root) :- !.
i_sqrt(N, C, RootAcc, Root) :-
  C < N,
  RootAcc1 is RootAcc + 1,
  C1 is RootAcc1 * RootAcc1,
  i_sqrt(N, C1, RootAcc1, Root).


%*************************List operations*************
%=== lst_eql(?L, ?R), true if L and R has the same length
% and items on respective positions can be unified.
lst_eql([], []).
lst_eql([H | T1], [H | T2]) :- lst_eql(T1, T2).

%=== reverse(+L, -R), reverses an input list R into output list L

% The reverse process will use an accumulator
reverse(L,R) :- reverse(L,[],R).
reverse([],X,X).
% Move element from input list to accumulator
reverse([X|Y],Z,T) :- reverse(Y,[X|Z],T).

%=== pal(+L), determines whether the input list is a palindrome.

pal(L) :- reverse(L, L).

%=== reverse_c(+L, -R, -N), reverses and input list R into output list L
% and piggybacks the length N of the input list.

% Use two accumulators, one for the list, one for counting
reverse_c(L, R, N) :- reverse_c(L, [], R, 0, N).
reverse_c([], R, R, N, N).
% Move element from input list to acc and increase counter
reverse_c([H | T], A, R, C, N) :-
  C1 is C + 1,
  reverse_c(T, [H | A], R, C1, N).
  
%=== pal_c(+L, -N), determines whether the input list is a palindrome
% and piggybacks its length N.
pal_c(L, N) :- reverse_c(L, L, N).

%=== flatten_rev(+L, ?R), true if R is a reversed flatenned list L
flatten_rev(L, R) :- flatten_rev(L, [], R).
flatten_rev([], R, R).

% H is a list
flatten_rev([H | T], A, R) :-
  is_list(H),
  flatten_rev(H, A, R1),
  flatten_rev(T, R1, R), !.
  
% H is not a list
flatten_rev([H | T], A, R) :-
  flatten_rev(T, [H | A], R).


%***************Matrix operations***************

%=== to_matrix(+L, +N, -M), Constructs a matrix M from the input list L,
% that will have N columns. If length of L is not divisible by N,
% the last row will be left incomplete.

to_matrix([], _, []).

to_matrix(L, N, [Row | TailMatrix]) :-
  strip_row(L, N, Row, ListTail),
  to_matrix(ListTail, N, TailMatrix), !.
  

%=== strip_row(+L, +N, ?Row, ?Tail), Constructs a list Row containing
% first N elements from L and a list Tail containing the rest of the
% elements.

strip_row(L, 0, [], L).
% If the length of L is less than N, this allows to "return"
% Row as L and Tail as [].
strip_row([], N, [], []) :- N > 0.

strip_row([H | ListTail], N, [H | RowTail], L) :-
  N > 0,
  N1 is N - 1,
  strip_row(ListTail, N1, RowTail, L).
  
  
%=== transpose_rev(+M, ?R), true if and only if matrix R is transposed
% reversed matrix M.
% [[1,2,3],[4,5,6],[7,8,9]] -> [[9,6,3], [8,5,2], [7,4,1]]

transpose_rev(M, R) :- transpose_rev(M, [], R). % use accumulator
transpose_rev(M, R, R) :- empty(M), !.
transpose_rev(M, A, R) :-
  strip_revcol(M, [], FirstCol, RestCol),
  transpose_rev(RestCol, [FirstCol | A], R).
  
%=== empty(+M), true if and only if M is [] or a list
% of empty lists
empty([]).
empty([[] | RestRows]) :- empty(RestRows).

%=== strip_revcol(+M, ?FirstCol, ?RestCols), true if and only if
% FirstCol is the reversed first column of matrix M and RestCols is
% M stripped from the first column.

strip_revcol(M, FirstCol, RestCol) :- strip_revcol(M, [], FirstCol, RestCol).
strip_revcol([], A, A, []).
strip_revcol([[]], A, A, []).
% First item in the row goes to the first column, rest of the row 
% goes to the rest of the matrix
strip_revcol([[HRow | TRow] | RestRows], A, FirstCol, [TRow | RestCols]) :-
  strip_revcol(RestRows, [HRow | A], FirstCol, RestCols).
  
%****************Front End
%sator(L, ListLen, GridSize, Grid, TransposedGrid, TransFlattened) :-
%  pal_c(L, ListLen),
%  i_sqrt(ListLen, GridSize),
%  to_matrix(L, GridSize, Grid),
%  transpose_rev(Grid, TransposedGrid),
%  flatten_rev(TransposedGrid, TransFlattened),
%  lst_eql(L, TransFlattened).

sator(L, GridSize) :-
  pal_c(L, ListLen), % L must be a palindrom
  i_sqrt(ListLen, GridSize), % its length must be square
  to_matrix(L, GridSize, Grid),
  transpose_rev(Grid, TransposedGrid),
  flatten_rev(TransposedGrid, TransFlattened),
  lst_eql(L, TransFlattened). % Transposed grid must read the same way as original grid
  
%******************Input filtering*****************
%Forbidden characters
forbidden_ch(' ').
forbidden_ch(',').
forbidden_ch('.').
forbidden_ch('!').
forbidden_ch('(').
forbidden_ch(')').

forbidden_code(Code) :-
  char_code(Char, Code),
  forbidden_ch(Char).
  
%=== filter_coded_input(+L, ?R) true, if and only if R is
% filtered list of character codes L.
filter_coded_input([], []).
filter_coded_input([H | T], R) :- 
  forbidden_code(H),
  filter_coded_input(T, R).
filter_coded_input([H | T], [H | T1]) :- filter_coded_input(T, T1).
  
%=== filter_char_input(+L, ?R)
filter_char_input([], []).
filter_char_input([H | T], R) :-
  forbidden_ch(H),
  filter_char_input(T, R), !.
filter_char_input([H | T], [H | T1]) :- filter_char_input(T, T1).
  
  
%******************Input reading*************************
% Query main. will "start" the execution. It opens the input file
% and processes it.
main :-
    open('input.txt', read, Str),
    read_line_count(Str, N),
    read_lines(Str, N),
    close(Str).

%=== read_line_count(+Stream, -Count)
%read_line_count(Stream, Count) :-
%  read(Stream, Count),
%  number(Count),
%  Count > 0.

read_line_count(Stream, Count) :-
  read_line_to_codes(Stream, R),
  number_codes(Count, R),
  Count > 0.

%=== read_lines(+Stream, +LineCount)
read_lines(Stream, LineCount) :- read_lines(Stream, 0, LineCount).
read_lines(_, N, N).
read_lines(Stream, _, _) :- at_end_of_stream(Stream).

read_lines(Stream, K, Count) :-
  K < Count,
  K1 is K + 1,
  read_line_to_codes(Stream, Line), % Every line will be read as a list of character codes
  filter_coded_input(Line, FilteredLine), % Filter the line
  write('Test '), write(K), write(': '),
  process_current_input(FilteredLine), nl, % Process the line
  read_lines(Stream, K1, Count), !.
  
%=== process_current_input(+Line)
process_current_input(Line) :-
  sator(Line, GridSize),
  write(GridSize).

process_current_input(_) :-
  write(0).