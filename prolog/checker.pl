%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checker.pl
% VERSION: 0.3
% Write in swipl:
%    ?- [your_code, checker].
%    ?- check_solution.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [testy]. % Use name of the file with downloaded tests

check_solution :-
  write('START TESTING'), nl,
  statistics(cputime, T0),
  performTests,
  statistics(cputime, T1),
  DT is T1 - T0,
  write('DONE IN '),
  write(DT),
  write('s'),
  nl.

performTests :-
  takeTest ,nl,
  fail.
performTests.

takeTest :-
  taskArity(N),  % Predicate defined in testy_N, number of input arguments
  length(Args, N),
  (Name = simple_test; Name = student_simple_test; Name = count_test; Name = student_count_test),
  append([Name|Args], [Correct], L),
  GetTest =.. L,
  GetTest,
  append([solve|Args], [Result], L2),
  Task =.. L2,
  (member(Name, [simple_test, student_simple_test]) ->
      trySimpleTest(Name, Task, Correct, Result);
      tryCountTest(Name, Task, Correct, Result)
  ). 
 

trySimpleTest(Name, Test, Correct, Result) :-
  Test,
  testSimpleSolution(Correct, Result), !,  % Predicate defined in testy_N
  write(Name), write(' [ok]').

trySimpleTest(Name, _, _, _) :-
  write(Name), write(' [failed]').

tryCountTest(Name, Test, Correct, Result) :-
  findall( Result, Test, Found),
  length(Found, N),
  write(Name),
  (N == Correct -> write(' [ok]'); write( ' [failed]')).
