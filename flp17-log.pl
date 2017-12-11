%FLP 2nd Project
%Jordan Jarolim, xjarol03, FIT VUTBR
%1. 5. 2017
%flp17-log.pl

% Main taking care about averything
main :-
	prompt(_, ''),
  read_file(user_input, Matrix),
  getDimensions(Matrix, Rows, Cols),
  % Replace hole
  atom_concat('A', Rows, Hole),
  putHole(Matrix, Rows, Replaced, '**', Hole),
  list_butlast(_, Replaced),
  % Define Model to be reached
  defModel(Rows, Cols, Goal),!,

  % getMaxPermut can raise in int_overflow -> manually selected 15:/
  %getMaxPermut(Rows, Cols, Permut),write(Permut),nl,!,
  numlist(1, 15, MaxDepthList), !, % "Breakpoint" is needed here

  % Generate depths
  nth1(_, MaxDepthList, MaxDepth),

  % Start backtracking with defined depth
  backtrackSolution(Replaced, 0, MaxDepth, Goal, Moves, []),

  % Print results
  putHole(Replaced, Rows, Solved, Hole, '**'),
  printMatrix(Solved),
  trackPath(Replaced, Moves).

% Read file line by line
%http://stackoverflow.com/questions/4805601/read-a-file-line-by-line-in-prolog
%http://stackoverflow.com/questions/23411139/prolog-unexpected-end-of-file
read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream,Codes),
    atom_chars(Atoms, Codes),
    %http://stackoverflow.com/questions/15436464/split-string-in-items
    atomic_list_concat(X,' ', Atoms), 
    read_file(Stream,L), !.

%Determine matrix dimension
getDimensions(Matrix, Rows, Cols) :- 
  length(Matrix, Rows),
  nth1(1, Matrix, Row), !,
  length(Row, Cols).

% Find hole
getHolePosition(Matrix, PosRow, PosCol, ToFind) :-
  nth1(PosRow, Matrix, Row),
  nth1(PosCol, Row, ToFind).

% Define correct matrix
defModel(Rows,Cols,Goal) :-
  numlist(1,Rows,NumberedRows),%1,2,3,4
  Max is Cols+65,
  numlist(65, Max, NumberedCols),%A,B,C,D
  buildModel(NumberedRows, NumberedCols, Goal),
  list_butlast(_, Goal).

%Build matrix rows
buildRows(_, [], _).
buildRows(A,[C|D], [E|F]):-
  atom_chars(MyChar, [C]),
  atom_number(MyNum, A),
  atom_concat(MyChar, MyNum, E),
  buildRows(A, D, F).

%Build final correct matrix
buildModel([],_, _).
buildModel([A|B], Cols, [X|Y]):-
  buildRows(A, Cols, ModelRows),
  list_butlast(ModelRows, X),
  buildModel(B, Cols, Y).

% Put AX or ** to the matrix according to its parameters
putHole([], _, _, _, _).
putHole([A|B], Rows, [X|Y], In, Out):-
  replace(In, Out,  A, X),
  putHole(B, Rows, Y, In, Out).

% Print matrix
printMatrix([]).
printMatrix([Row|T]) :-
  printRow(Row),nl,
  printMatrix(T).

% Print row of matrix
printRow([]).
printRow([A|B]):-
  write(A),
  write(' '),
  printRow(B).

% Experimental - get number of all possible combinations
getMaxPermut(Rows, Cols, Result):-
  factorial(Rows*Cols, Value1),
  factorial(Rows, Value2),
  Result is Value1/Value2.

% Experimental - get factorial
factorial( 0, 1 ).
factorial( N, Value ) :-
     N > 0,
     Prev is N - 1,
     factorial( Prev, Prevfact ),
     Value is Prevfact * N.


%%%%%%%%% Backtracking %%%%%%%%%

% Reached the solution
backtrackSolution(ActualTower, _ ,_ , ActualTower, _, _).

% Cannot solve
backtrackSolution(_, Depth, MaxDepth, _ ,_ ,_ ) :-
  Depth >= MaxDepth,
  !,
  fail.

% Backtrack until the max depth of rotations is reached
backtrackSolution(ActualTower, Depth, MaxDepth, Model,[Move|Moves], Changes) :-
  % Increase Depth
  Depth1 is Depth + 1,
  % Generate Move
  (
    tryRotation(ActualTower, NewTower, Move);
    changeHole(ActualTower, NewTower, Move)
  ),
  % Check if it is a new move
  not(member(NewTower, Changes)),
  % Check if its correct solution or go deeper
  backtrackSolution(NewTower, Depth1, MaxDepth, Model, Moves, [ActualTower|Changes]).


% Rotate row - rotates row and save its index to Move Array -> Important for replay -> 
% nth1(?Index, ?List, ?Elem) - Is true when Elem is the Index'th element of List. Counting starts at 1.
% http://www.swi-prolog.org/pldoc/man?predicate=nth1/3
tryRotation(Matrix, RotatedMatrix, Index) :- 
  % Rows stores all possible rows (if fail it just takes another), Index stores index of a row in matrix 
  nth1(Index, Matrix, Rows),
  % Rotate - see def of Rotate() below and stackoverflow link
  rotate(Rows, RotatedRow),
  % Place rotated row in matrix
  % http://www.swi-prolog.org/pldoc/man?predicate=select/4
  % replace() defined below (from stack overflow) doesnt work, I dont know why :/
  select(Rows, Matrix, RotatedRow, RotatedMatrix).

% Rotate list
% http://stackoverflow.com/questions/10255703/how-to-rotate-lists-in-prolog
rotate([H|T],R) :- append(T,[H],R).

changeHole(Matrix, Result, Move):-
  % Define type of move for replay
  Move is -1,
  % Get matrix dimensions
  getDimensions(Matrix, Rows, _),

  % Define hole appereance
  atom_concat('A', Rows, ToFind),
  % Find hole position
  getHolePosition(Matrix, PosRow, PosCol, ToFind), 
  % Define direction of swap
  (Rows > PosRow ->
    ToBeSwapped is PosRow + 1
  ;
    ToBeSwapped is 1
  ),
  % Get hole value
  nth1(PosRow, Matrix, RowHole),
  nth1(PosCol, RowHole, HoleValue),

  % Get value to be swapped
  nth1(ToBeSwapped, Matrix, RowSwap),
  nth1(PosCol, RowSwap, NewValue),

  % Swap in rows - replace defined below doesnt work :/
  select(HoleValue, RowHole, NewValue, NewHoleRow),
  select(NewValue, RowSwap, HoleValue, NewSwapRow),
  
  % Put rows back
  select(RowHole, Matrix, NewHoleRow, FirstSwap),
  select(RowSwap, FirstSwap, NewSwapRow, Result).

% Print the path 
trackPath(_, []) :- !.
trackPath(ActualTower, [Move|Moves]) :-
  (
    tryRotation(ActualTower, NewTower, Move);
    changeHole(ActualTower, NewTower, Move)
  ),
  getDimensions(ActualTower, Rows, _),
  atom_concat('A', Rows, Hole),
  putHole(NewTower, Rows, Solved, Hole, '**'),
  nl,
  printMatrix(Solved), !,
  trackPath(NewTower, Moves).


%%%%%%%%% Helpers %%%%%%%%%

% Delete last element in a list
% In my case some pointer to a memory - just some trash, I dont know how to solve it more elegant
%http://stackoverflow.com/questions/16174681/how-to-delete-the-last-element-from-a-list-in-prolog
list_butlast([X|Xs], Ys) :-                 % use auxiliary predicate ...
   list_butlast_prev(Xs, Ys, X).            % ... which lags behind by one item

list_butlast_prev([], [], _).
list_butlast_prev([X1|Xs], [X0|Ys], X0) :-  
   list_butlast_prev(Xs, Ys, X1).           % lag behind by one

% Replace element in a list
%http://stackoverflow.com/questions/5850937/prolog-element-in-lists-replacement
replace(_, _, [], []).
replace(O, R, [O|T], [R|T2]) :- replace(O, R, T, T2).
replace(O, R, [H|T], [H|T2]) :- H \= O, replace(O, R, T, T2).



