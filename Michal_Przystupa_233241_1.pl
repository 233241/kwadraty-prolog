% Michal Przystupa 233241 zagadka nr 1: kwadraty

kwadraty(Nazwa,Rozwiazanie):-
    open(Nazwa, read, _, [alias(params)]),
    read(params, Xmax), 
    read(params, Ymax), 
    read(params, Punkty), 
    close(params),
	generator(Xmax,Ymax,Punkty,Mozliwosci),                                   % generuje poczatkowa liste list mozliwosci, ktora bedzie pozniej ograniczana
	petla(Mozliwosci,[],Rozwiazanie).                                      % przystepuje do eliminacji niektorych mozliwosci
	
	
generator(Xmax,Ymax,Punkty,Mozliwosci):-                                        % 2 zagniezdzone findall'e generuja liste zlozona z list
    findall(Lista,
        (
        select(Punkt,Punkty,Pozostale),                                         % wybiera punkt dla ktorego zostanie wygenerowana lista mozliwosci i usuwa go z listy punktow
        findall((M1,M2,Dlugosc),                                                % pierwszy findall generuje liste rozwiazen dla pojedynczego punktu
            (
            numlist(1,Xmax,NList),                                              % tworzy liste mozliwych wartosci X
            member(X,NList),                                                    % i wybiera jedna z nich
            Punkt=(A,B,Ilosc),                                                  % dopiero tutaj potrzebujemy wiedziec jaka strukture Punkt
            X\=A,                                                               % oczywiscie kwadrat ((A,B),(X,Y)) nie moze miec boku dlugosci 0
            (Y is X-A+B;Y is A+B-X),                                            % albo (Y-X == B-A) albo (X+Y==A+B) czyli sprawdzamy po przekatnych
            0<Y,Y=<Ymax,                                                        % punkt (X,Y) nie moze lezec poza plansza
            porzadek(A,X,M1,W1),                                                % znajdujemy gorny lewy (M1,M2) i dolny prawy (W1,W2) rog kwadratu
            porzadek(B,Y,M2,W2),
            na_krawedzi(M1,M2,W1,W2,Pozostale),                                 % zaden z punktow poczatkowych nie moze lezec na krawedzi kwadratu
            ile(M1,M2,W1,W2,Pozostale,0,Ilosc),                                 % ilosc punktow wewnatrz kwadratu musi sie zgadzac
            Dlugosc is W1-M1                                                    % wyznaczamy dlugosc boku kwadratu
            ),
        Lista)
        ),
    Mozliwosci).

petla([],Wynik,Wynik).                                                     % symuluje petle usuwajaca niepoprawne mozliwosci z list
petla([H|T],Rozw,Akk):-
    najkrotsza(T,H,Min,Reszta),                                            % wez nakrotsza liste (najwydajniejsza opcja)
    member(Elem,Min),                                                      % wez z niej dowolny element (jakis kwadrat)
    roz_filtr(Reszta,Elem,Po_filtrze),                                     % usun wszystkie mozliwosci wchodzace w konflikt z tym elementem
    dodaj_lex(Elem,Rozw,New_Rozw),                                         % dodaj Elem do listy dotychczasowych rozwiazan
    petla(Po_filtrze,New_Rozw,Akk).

filtr([],_,[]).
filtr([(C,D,Dl_2)|T],(A,B,Dl_1),[(C,D,Dl_2)|R]):-                          % usuwa z listy [(C,D,Dl_2)|T] elementy wchodzace w konflikt z kwadratem (A,B,Dl_1)
    W1 is A+Dl_1,
    W2 is B+Dl_1,
    W3 is C+Dl_2,
    W4 is D+Dl_2,
    \+ pokrywaja(A,B,W1,W2,C,D,W3,W4),!,
    filtr(T,(A,B,Dl_1),R).
filtr([_|T],X,R):-
    filtr(T,X,R).	
	
roz_filtr([],_,[]).                                                        % rozszerza filtr do predykatu usuwajacego z listy list (H i HX to listy)
roz_filtr([H|T],X,[HX|TX]):-                                               
    filtr(H,X,HX),
    roz_filtr(T,X,TX).

dodaj_lex(Elem,[],[Elem]).                                                 % dodaje element do listy zachowujac porzadek leksykograficzny
dodaj_lex((A,B,I),[(C,D,J)|T],[(A,B,I),(C,D,J)|T]):-
    (A<C;(A==C,B=<D)),!.
dodaj_lex(X,[Y|T],[Y|T1]):-
    dodaj_lex(X,T,T1).

najkrotsza([],Min,Min,[]).                                                 % wybiera najkrotsza liste i zwraca reszte
najkrotsza([H|T],Obecnie,Min,[Obecnie|R]):-
    length(H,D1),
    length(Obecnie,D2),
    D1<D2,!,
    najkrotsza(T,H,Min,R).
najkrotsza([H|T],Obecnie,Min,[H|R]):-
    najkrotsza(T,Obecnie,Min,R).
	
nachodza(A,B,C,D):-	                                                       % sprawdza czy odcinki (A,B) i (C,D) nachodz¹ na siebie
    (A=<C,B>=C);
    (A=<D,B>=D);
    (A=<C,B>=D);
    (A>=C,B=<D).
	
pokrywaja(A,B,C,D,E,F,G,H):-                                               % sprawdza czy kwadraty((A,B),(C,D)) ((E,F),(G,H)) pokrywaja sie w odpowiedni sposob
    ((B=F;B=H;D=F;D=H),nachodza(A,C,E,G));
    ((A=E;A=G;C=E;C=G),nachodza(B,D,F,H)).

ile(_,_,_,_,[],W,W).                                                       % sprawdza ile punktow lezy wewnatrz kwadratu ((A,B),(C,D))
ile(A,B,C,D,[(X,Y,_)|T],AKK,W):-
    zaw_m(A,B,C,D,X,Y),!,
    AKK1 is AKK +1,
    ile(A,B,C,D,T,AKK1,W).
ile(A,B,C,D,[_|T],AKK,W):-
    ile(A,B,C,D,T,AKK,W).		

zaw_m(A,B,C,D,X,Y):-                                                       % sprawdza czy (X,Y) zawiera sie w ((A,B),(C,D)) nie wliczajac krawedzi 
    A<X,
    X<C,
    B<Y,
    Y<D.
	
zaw_w(A,B,C,D,X,Y):-                                                       % sprawdza czy (X,Y) zawiera sie w ((A,B),(C,D)) wliczajac krawedzie
    A=<X,
    X=<C,
    B=<Y,
    Y=<D.

na_krawedzi(_,_,_,_,[]).
na_krawedzi(A,B,C,D,[(X,Y,_)|T]):-                                         % sprawdza czy zaden z punktow (poczatkowych) (X,Y) NIE lezy na krawedzi kwadratu ((A,B),(C,D))
    (\+ zaw_w(A,B,C,D,X,Y);zaw_m(A,B,C,D,X,Y)),
    na_krawedzi(A,B,C,D,T).

porzadek(X,Y,X,Y):-                                                        % ustala ktora liczba jest mniejsza
    X<Y,!.
porzadek(X,Y,Y,X).