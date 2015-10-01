% Pawel Jankowski
% Zad nr. 1 "Kwadraty"
% Celem programu jest wypisanie kwadratow ktore posiadaja w jednym z ich
% rogow cyfre, rowna ilosci liczb ktore ten kwadrat obejmuje. Boki
% kwadratow i rogi nie moga się stykac/pokrywac.
%
% W projekcie staralem sie nazwy predykatow zlozone z wielu slow
% zapisywac z pomoca podkreslenia, poza tym korzystalem z CamelCase.
%
% Czasem pisze przed predykatem bind jest to poprostu takie ulatwienie,
% ktore wykorzystuje najczesciej predykat once.
%
% Dolaczone sa testy studenta, na moim
% komputerze po uruchomieniu predykatu check_solution czas wynosil ok
% 0.19 sekund.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Algorytm:
%
% Algorytm polega na tym ze generuje kwadraty tak jakby ich cyfra byla
% wpisana w lewy-gorny, lewy-dolny, prawy-gorny, prawy-dolny rog,
% oczywiscie sprawdzajac kolizje z cyframi wpisanymi w plansze i ile
% cyfr ich obwod obejmuje. Dalej generuje rozwiazanie biarac pod uwage
% kolizje z innymi kwadratami. A w ostatnim kroku (gdzie sprawdzam
% dlugosci list) potrzebne mi jest to do filtracji ze "smieci".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




% Pierwszy parametr (krotka, postaci "(A, B, C)" jest kwadratem w LT
% (o poczatku w lewym gornym rogu [left-top]) (to ma byc wierzcholek
% kwadratu). Predykat ustala ile liczb jest w srodku tego kwadratu (ale
% nie na obwodzie) i unifukuje ta liczbe z X. L podana to lista punktow
% na rysunku (ta podana dla solve).
%
% Oznaczenia:
%
% -D to dlugosc boku kwadratu
% -RowRB i ColRB to odpowiednio wiersz i kolumna prawego dolnego boku
% kwadratu, natomiast LT (lewego gornego).
bind_digits_in_square((A, B, C), L,  X) :-
	once(digits_in_square((A, B, C), L, X)).
digits_in_square((_, _, _), [], 0).
digits_in_square((Row, Col, D), [(A, B, _)|T], Number) :-
	RowLT is Row + 1, ColLT is Col + 1,
	RowRB is Row + D - 1, ColRB is Col + D - 1,
	between(RowLT, RowRB, A),
	between(ColLT, ColRB, B),
	digits_in_square((Row, Col, D), T, Number1),
	Number is Number1 + 1.
digits_in_square((Row, Col, D), [(_, _, _)|T], Number) :-
	digits_in_square((Row, Col, D), T, Number).





% gen_sqs_LT - "generate squares", wersja LT
% Predykat do generowania listy mozliwych kwadratow, gdzie cyfra
% znajduje sie w ich lewym gornym rogu.
% Jak sie nie daje znalesc zwracana jest lista pusta :)
% Predykat sprawdza przed dalaczeniem ile kwadrat zawiera cyfr oraz czy
% przypadkiem nie ma cyfry na boku wygenerowanego kwadratu.
%
% Oznaczenia :
%
% -M, N rozmiar planszy
% -Board lista punktow na rysunku(ta z solva),
% -(R, C, V) to (Wiersz, Kolumna, Ilosc objetych cyfr)
% -RNext, CNext, RNextNext, CNextNext to wiersze i kolumny
% inkrementowane/dekrementowane
% -Sub to dlugosc kwadratu
% -HM to zmienna stworzona tylko na potrzeby porownania, ktore sie tam
% znajduje
bind_gen_sqs_LT(M, N, Board, (R, C, V), List) :-
	once(gen_sqs_LT(M, N, Board, (R, C, V), List)).
gen_sqs_LT(M, N, Board, (R, C, V), List) :-
	gen_sqs_LT(M, N, Board, (R, C, V), R, C, List, []).
gen_sqs_LT(M, N, Board, (R, C, V), RNext, CNext, List, Acc) :-
	RNextNext is RNext + 1, CNextNext is CNext + 1,
	RNextNext =<  M, CNextNext =< N,
	Sub is RNextNext - R,
	bind_digits_in_square((R, C, Sub), Board, HM),
	HM == V,
	is_not_digit_in_district_of_sq(Board, (R, C, Sub), (R,C,V)),
	append(Acc, [(R,C,Sub)], Acc1),
	gen_sqs_LT(M, N, Board, (R, C, V), RNextNext, CNextNext, List, Acc1).
gen_sqs_LT(M, N, Board, (R, C, V), RNext, CNext, List, Acc) :-
	RNextNext is RNext + 1, CNextNext is CNext + 1,
	RNextNext =< M, CNextNext =< N,
	gen_sqs_LT(M, N, Board, (R, C, V), RNextNext, CNextNext, List, Acc).
gen_sqs_LT(_, _, _,(_,_, _), _, _, List, List).





% gen_sqs_RT - "generate squares" wersja RT
% Predykat do generowania listy mozliwych kwadratow, gdzie cyfra
% znajduje sie w ich prawym gornym rogu.
% Jak sie nie daje znalesc zwracana jest lista pusta :),
% Ale jak sie da, to bedzie nia lista gdzie wiersz, kolumna i dlugosc
% boku kwadratu beda w notacji lewego gornego rogu.
% Predykat sprawdza przed dalaczeniem ile kwadrat zawiera cyfr oraz czy
% przypadkiem nie ma cyfry na boku wygenerowanego kwadratu.
%
% Oznaczenia :
%
% -M, N rozmiar planszy
% -Board lista punktow na rysunku(ta z solva),
% -(R, C, V) to (Wiersz, Kolumna, Ilosc objetych cyfr)
% -RNext, CNext, RNextNext, CNextNext,
%  RBefore, CBefore, RBeforeBefore, RBeforeBefore to wiersze i kolumny
%  inkrementowane/dekrementowane
% -Sub to dlugosc kwadratu
% -HM to zmienna stworzona tylko na potrzeby porownania, ktore sie tam
% znajduje
% - RLT, CLT to wiersz i kolumna w notacji lewego gornego rogu
bind_gen_sqs_RT(M, N, Board, (R, C, V), List) :-
	once(gen_sqs_RT(M, N, Board, (R, C, V), List)).
gen_sqs_RT(M, N, Board, (R, C, V), List) :-
	gen_sqs_RT(M, N, Board, (R, C, V), R, C, List, []).
gen_sqs_RT(M, N, Board, (R, C, V), RNext, CBefore, List, Acc) :-
	RNextNext is RNext + 1, CBeforeBefore is CBefore - 1,
	RNextNext =< M, CBeforeBefore >= 1,
	Sub is RNextNext - R,
	RLT is R,
	CLT is C - Sub,
	bind_digits_in_square((RLT, CLT, Sub), Board, HM),
	HM == V,
	is_not_digit_in_district_of_sq(Board, (RLT, CLT, Sub), (R,C,V)),
	append(Acc, [(RLT,CLT,Sub)], Acc1),
	gen_sqs_RT(M, N, Board, (R, C, V), RNextNext, CBeforeBefore, List, Acc1).
gen_sqs_RT(M, N, Board, (R, C, V), RNext, CBefore, List, Acc) :-
	RNextNext is RNext + 1, CBeforeBefore is CBefore - 1,
	RNextNext =< M, CBeforeBefore >= 1,
	gen_sqs_RT(M, N, Board, (R, C, V), RNextNext, CBeforeBefore, List, Acc).
gen_sqs_RT(_, _, _,(_,_,_),_,_,List,List).





% gen_sqs_LB - "generate squares" wersja LB
% Predykat do generowania listy mozliwych kwadratow, gdzie cyfra
% znajduje sie w ich lewym dolnym rogu.
% Jak sie nie daje znalesc zwracana jest lista pusta :),
% Ale jak sie da, to bedzie nia lista gdzie wiersz, kolumna i dlugosc
% boku kwadratu beda w notacji lewego gornego rogu.
% Predykat sprawdza przed dalaczeniem ile kwadrat zawiera cyfr oraz czy
% przypadkiem nie ma cyfry na boku wygenerowanego kwadratu.
%
% Oznaczenia :
%
% -M, N rozmiar planszy
% -Board lista punktow na rysunku(ta z solva),
% -(R, C, V) to (Wiersz, Kolumna, Ilosc objetych cyfr)
% -RNext, CNext, RNextNext, CNextNext,
%  RBefore, CBefore, RBeforeBefore, RBeforeBefore to wiersze i kolumny
%  inkrementowane/dekrementowane
% -Sub to dlugosc kwadratu
% -HM to zmienna stworzona tylko na potrzeby porownania, ktore sie tam
% znajduje
% - RLT, CLT to wiersz i kolumna w notacji lewego gornego rogu
bind_gen_sqs_LB(M, N, Board, (R, C, V), List) :-
	once(gen_sqs_LB(M, N, Board, (R, C, V), List)).
gen_sqs_LB(M, N, Board, (R, C, V), List) :-
	gen_sqs_LB(M, N, Board, (R, C, V), R, C, List, []).
gen_sqs_LB(M, N, Board, (R, C, V), RBefore, CNext, List, Acc) :-
	RBeforeBefore is RBefore - 1, CNextNext is CNext + 1,
	RBeforeBefore >= 1, CNextNext =< N,
	Sub is CNextNext - C,
	RLT is R - Sub,
	CLT is C,
	bind_digits_in_square((RLT, CLT, Sub), Board, HM),
	HM == V,
	is_not_digit_in_district_of_sq(Board, (RLT, CLT, Sub), (R,C,V)),
	append(Acc, [(RLT, CLT, Sub)], Acc1),
	gen_sqs_LB(M, N, Board, (R, C, V), RBeforeBefore, CNextNext, List, Acc1).
gen_sqs_LB(M, N, Board, (R, C, V), RBefore, CNext, List, Acc) :-
	RBeforeBefore is RBefore - 1, CNextNext is CNext + 1,
	RBeforeBefore >= 1, CNextNext =< N,
	gen_sqs_LB(M, N, Board, (R, C, V), RBeforeBefore, CNextNext, List, Acc).
gen_sqs_LB(_, _, _,(_,_,_),_,_,List, List).





% gen_sqs_RB - "generate squares" wersja RB
% Predykat do generowania listy mozliwych kwadratow, gdzie cyfra
% znajduje sie w ich prawym dolnym rogu.
% Jak sie nie daje znalesc zwracana jest lista pusta :),
% Ale jak sie da, to bedzie nia lista gdzie wiersz, kolumna i dlugosc
% boku kwadratu beda w notacji lewego gornego rogu.
% Predykat sprawdza przed dalaczeniem ile kwadrat zawiera cyfr oraz czy
% przypadkiem nie ma cyfry na boku wygenerowanego kwadratu.
%
% Oznaczenia :
%
% -M, N rozmiar planszy
% -Board lista punktow na rysunku(ta z solva),
% -(R, C, V) to (Wiersz, Kolumna, Ilosc objetych cyfr)
% -RNext, CNext, RNextNext, CNextNext,
%  RBefore, CBefore, RBeforeBefore, RBeforeBefore to wiersze i kolumny
% odpowiednio inkrementowane/dekrementowane
% -Sub to dlugosc kwadratu
% -HM to zmienna stworzona tylko na potrzeby porownania, ktore sie tam
% znajduje
% - RLT, CLT to wiersz i kolumna w notacji lewego gornego rogu
bind_gen_sqs_RB(M, N, Board, (R, C, V), List) :-
	once(gen_sqs_RB(M, N, Board, (R, C, V), List)).
gen_sqs_RB(M, N, Board, (R, C, V), List) :-
	gen_sqs_RB(M, N, Board, (R, C, V), R, C, List, []).
gen_sqs_RB(M, N, Board, (R, C, V), RBefore, CBefore, List, Acc) :-
	RBeforeBefore is RBefore - 1, CBeforeBefore is CBefore - 1,
	RBeforeBefore >= 1, CBeforeBefore >= 1,
	Sub is C - CBeforeBefore,
	RLT is R - Sub,
	CLT is C - Sub,
	bind_digits_in_square((RLT, CLT, Sub), Board, HM),
	HM == V,
	is_not_digit_in_district_of_sq(Board, (RLT, CLT, Sub), (R, C, V)),
	append(Acc, [(RLT, CLT, Sub)], Acc1),
	gen_sqs_RB(M, N, Board, (R, C, V), RBeforeBefore, CBeforeBefore, List, Acc1).
gen_sqs_RB(M, N, Board, (R, C, V), RBefore, CBefore, List, Acc) :-
	RBeforeBefore is RBefore - 1, CBeforeBefore is CBefore - 1,
	RBeforeBefore >= 1, CBeforeBefore >= 1,
	gen_sqs_RB(M, N, Board, (R, C, V), RBeforeBefore, CBeforeBefore, List, Acc).
gen_sqs_RB(_, _, _, (_, _, _), _, _, List, List).





% Predykad laczacy 4 listy razem (na potrzeby gen_sqs_XX).
join_four_lists(X, Y, W, Z, Result) :-
	append(X, Y, A),
	append(A, W, B),
	append(B, Z,Result).





% Predykat ktory pod List zunifikuje liste wszelkich mozliwych
% kwadratow, po podaniu danego punktu na planszy.
simulate(M, N, (R, C, V), Board, List) :-
	bind_gen_sqs_LT(M, N, Board, (R, C, V), A1),
	bind_gen_sqs_RT(M, N, Board, (R, C, V), B1),
	bind_gen_sqs_LB(M, N, Board, (R, C, V), C1),
	bind_gen_sqs_RB(M, N, Board, (R, C, V), D1),
	join_four_lists(A1, B1, C1, D1, List).





% Predykat pozwala stwierdzic, czy jest cyfra na ktoryms z bokow w
% kwadracie, oczywiscie procz tego z ktorego rozpoczynamy. Ten
% problem/predykat podzielilem na kilka mniejszych,
% is_digit_in_district_of_sq_final to ostateczna wersja, gdzie
% WhichToExclude to krotka z ktorej rozpoczynamy szukanie i trzeba ja
% wykluczyc,
% - Board - to lista punktow na planszy
% - (R, C, D) to wiersz ,kolumna i dlugosc boku
is_digit_in_district_of_sq_final(Board, (R, C, D), WhichToExclude) :-
	once(is_digit_in_district_of_sq_without(Board, (R, C, D), WhichToExclude)).


% is_digit_in_district_of_sq_without realizuje wykluczenie za pomoca
% select
is_digit_in_district_of_sq_without(Board, (R, C, D), WhichToExclude) :-
        select(WhichToExclude, Board, BoardW),
	!,
	once(is_digit_in_district_of_sq(BoardW, (R, C, D))).
is_digit_in_district_of_sq_without(Board, (R, C, D), _) :-
	once(is_digit_in_district_of_sq(Board, (R, C, D))).

% is_digit_in_district_of_sq glowny predykat sprawdzajacy czy jest cyfra
% na boku kwadratu
%
%  Oznaczenia:
%  - RB czyli wiersz elementu z planszy
%  - CB czyli kolumna elementu z planszy
%  - (R, C, D) czyli odpowiednio wiersz, kolumna i dlugosc boku kwadratu
%  - RD, CD, wspolrzedne kwadratu prawego dolnego rogu
is_digit_in_district_of_sq([(RB, CB, _)|_], (R, C, D)) :-
	RD is R + D,
	CD is C + D,
	((between(R, RD, RB), (between(CB, CB, C);between(CB,CB,CD))) ; (between(C, CD, CB), (between(RB, RB, R);between(RB,RB,RD)))),
	true.
is_digit_in_district_of_sq([(_, _, _)|TBoard], (R,C,D)) :-
	is_digit_in_district_of_sq(TBoard, (R, C, D)).

% Potrzebna jest mi ta negacja, ten wlasnie predykat wykorzystuje w
% generowaniu odpowiednich kwadratow.
is_not_digit_in_district_of_sq(Board, (R, C, D), WhichToExclude) :-
        \+ is_digit_in_district_of_sq_final(Board, (R, C, D), WhichToExclude).





%% is_collision predykat sprawdzajacy czy czasami boki sie nie
%% pokrywaja (stykaja).
%
% Oznaczenia:
%  - List to lista kwadratow
%  - (R, C, D) to wiersz, kolumna, dlugosc analizowanego kwadratu
%  - RA, CA, DA to wiersz, kolumna, dlugosc kwadratu z ktorym sprawdzamy
%  - Te powyzsze zmienne po dodaniu literki D na koncu np. RAD, RD itp.
%    oznaczaja wspolrzedne prawego dolnego rogu kwadratu.
bind_is_collision((R,C,D), List) :-
	once(is_collision((R, C, D), List)).

is_collision((R, C, D), [HAlready|_]) :-
	HAlready = (RA, CA, DA),
	RAD is RA + DA,
	CAD is CA + DA,
	RD is R + D,
	CD is C + D,
(
	( (R == RA ; RD == RA ; R == RAD ; RD == RAD) ,
	  (between(CA, CAD, C) ; between(CA, CAD, CD) ;
	  between(C, CD, CA) ; between(C, CD, CAD)) );
	( (C == CA ; CD == CA ; C == CAD ; CD == CAD) ,
	  (between(RA, RAD, R) ; between(RA, RAD, RD) ;
	  between(R, RD, RA) ; between(R, RD, RAD)) )
), true.
is_collision((R, C, D), [_|TAlready]) :-
	is_collision((R, C, D), TAlready).

% Glowny predykat wykorzystywany (negacja powyzszego).
is_not_collision((R, C, D), List) :-
	\+ bind_is_collision((R, C, D), List).





%daj rozwiazanie, ktore jednak wygeneruje tez rozne smieci,
%
% Oznaczenia:
%
%  - Board to lista punktow na planszy
%  - (R, C, V) krotka oznaczajaca wiersz, kolumne, i ilosc liczb
%  - M, N rozmiarem planszy
%  - Mess to lista wygenerowanych kwadratow z danego punktu
%  - RCandidate itp. zmiennymi krotki kwadratow na kandydatow na bycie w
%  rozwiazaniu.
generate_solution(M, N, [HBoard|TBoard], Board, List) :-
	generate_solution(M, N, [HBoard|TBoard], Board, List, []).
generate_solution(M, N, [HBoard|TBoard], Board, List, Acc) :-
	HBoard = (R, C, V),
	simulate(M, N, (R, C, V), Board, Mess),
	select(X, Mess, _),
	X = (RCandidate, CCandidate, DCandidate),
	is_not_collision((RCandidate, CCandidate, DCandidate), Acc),
	append(Acc, [X], Acc1),
	generate_solution(M, N, TBoard, Board, List, Acc1).
generate_solution(_, _, _,_, List, List).





% Zauwazam ze zeby pozbyc sie smieci z predykatu generate_solution, musi
% byc tyle kwadratow wygenerowanych, ile bylo podanych punktow na
% planszy.
solve(M, N, Board, Solutions) :-
	generate_solution(M, N, Board, Board, Solutions),
	length(Board, BoardLenght),
	length(Solutions, SoulutionsLength),
	BoardLenght == SoulutionsLength.


%testy
student_count_test(5,5, [(1,1,1),(2,2,0)], 6).
student_count_test(9,5, [(2,2,1),(3,3,1),(5,5,1),(6,4,0)],6).
student_count_test(3,10,[(1,1,0),(1,3,0),(2,5,0),(1,7,0),(2,8,0)],0).
student_count_test(9, 12, [(3,10,1), (4,8,1), (7,4,0), (7,8,0), (8,3,3)], 11).



student_simple_test(14,14,
	[(1,1,6),(1,12,0),(2,2,0),(2,4,1),(2,8,0),(3,5,1),(4,2,0),(4,12,0),
	(6,7,1),(7,14,1),(9,12,0),(10,3,2),(11,9,0),(11,12,1),(12,4,0),
	(12,5,0),(12,9,0),(12,11,0)],
	[ (1,1,8),(1,12,2),(2,2,1),(2,4,3),(2,8,3),(3,5,5),(4,2,1),(4,12,2),
	(6,2,5),(7,11,3),(8,12,1),(10,3,3),(10,8,1),(11,10,2),(12,2,2),
	(12,5,2),(12,8,1),(12,11,2)]).
student_simple_test(20, 28, [(3,3,4), (3,20,1), (3,26,0), (4,18,1), (7,22,1), (9,22,0), (11,18,2), (16,13,1), (18,18,0), (20,15,1)], [ (3, 3, 16), (3, 20, 5), (3, 26, 1), (1, 18, 3), (1, 22, 6), (9, 22, 6), (5, 18, 6), (9, 13, 7), (18, 18, 2), (6, 1, 14)]).



:-[checker].
