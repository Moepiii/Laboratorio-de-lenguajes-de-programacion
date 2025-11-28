% Rafael y baulio
% Esta mensaje es para ti cuando lo veas bau, luego lo quitas
% si va a agregar un moustruo debes hacerlo con "asi" ej "Dragon" ( una sola comilla simple nose porque se ponen 2)
% esto porque si pones el nombre y ya lo guarda como variable y luego tira es una direccion como _1222 coasa asi del resto pruebalo 

:- dynamic mostro/4.

% Parte1
% BASE DE CONOCIMIENTO INICIAL
% mostro(nombre, nivel, atributo, poder).

mostro(mostroUno, 5, luz, 2100).
mostro(mostroDos, 7, luz, 2400).
mostro(mostroTres, 7, viento, 2500).


% Cosas que necesitamos

% Verifica si dos valores son iguales. Retorna 1 si son iguales, 0 si no.

es_igual(Val1, Val2, 1) :- Val1 == Val2, !.
es_igual(_, _, 0).

% Calcula cuántas características comparten dos mostros dados sus nombres.

% Compara Nivel, Atributo y Poder.
contar_coincidencias(Nombre1, Nombre2, Total) :-
    mostro(Nombre1, Nivel1, Attr1, Poder1),
    mostro(Nombre2, Nivel2, Attr2, Poder2),
    es_igual(Nivel1, Nivel2, C1),
    es_igual(Attr1, Attr2, C2),
    es_igual(Poder1, Poder2, C3),
    Total is C1 + C2 + C3.

% Verifica la condición de "Mundo Chiquito": compartir EXACTAMENTE UNA característica.

cumple_condicion(M1, M2) :-
    mostro(M1, _, _, _),
    mostro(M2, _, _, _),
    contar_coincidencias(M1, M2, 1).

% Parte2
% PREDICADOS PRINCIPALES DEL PROYECTO

% 1. ternaMundoChiquito(X, Y, Z)
% Evalúa verdadero para las ternas que satisfacen: Mano -> Puente -> Mazo
% X: Carta revelada de la mano.
% Y: Carta revelada del mazo (Puente).
% Z: Carta agregada a la mano.
% [cite: 28, 29, 30]


ternaMundoChiquito(X, Y, Z) :-
    mostro(X, _, _, _),  % X debe ser un mostro existente
    mostro(Y, _, _, _),  % Y debe ser un mostro existente
    mostro(Z, _, _, _),  % Z debe ser un mostro existente
    cumple_condicion(X, Y), % X y Y comparten exactamente 1 característica
    cumple_condicion(Y, Z). % Y y Z comparten exactamente 1 característica
    % Nota: No agregamos X \= Z explícitamente porque el PDF permite ciclos como
    % mostroUno -> mostroDos -> mostroUno[cite: 33].
    % Sin embargo, 'cumple_condicion' implícitamente asegura que X \= Y y Y \= Z,
    % porque un mostro consigo mismo comparte 3 características, no 1.


% 2. mundoChiquito/0 tuneado para que nos de como el pdf

mundoChiquito :-
    ternaMundoChiquito(X, Y, Z),
    X \= Z,  % Condición: Inicio y Fin son diferentes
    write(X), write(' '), write(Y), write(' '), write(Z), nl,
    fail.

mundoChiquito :-
    ternaMundoChiquito(X, Y, Z),
    X == Z,  % Condición: Inicio y Fin son el mismo
    write(X), write(' '), write(Y), write(' '), write(Z), nl,
    fail.

% Parte3
% Lee información por consola y agrega un nuevo mostro a la base de conocimiento.

agregarMostro :-
    write('--- Agregar Nuevo Mostro ---'), nl,
    write('Instrucciones: Ingrese los datos seguidos de un punto (.) al final.'), nl,
    
    write('Nombre (atom, ej: dragon.): '), 
    read(Nombre),
    
    write('Nivel (entero 1-12, ej: 4.): '), 
    read(Nivel),
    
    write('Atributo (atom, ej: fuego.): '), 
    read(Atributo),
    
    write('Poder (entero multiplo de 50, ej: 1500.): '), 
    read(Poder),
    
    % Validar y agregar usando assertz
    (   integer(Nivel), integer(Poder)
    ->  assertz(mostro(Nombre, Nivel, Atributo, Poder)),
        write('Mostro agregado exitosamente a la base de conocimiento.'), nl
    ;   write('Error: Nivel y Poder deben ser numeros enteros.'), nl
    ).