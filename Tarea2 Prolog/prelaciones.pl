% prelaciones.pl
% prela(A, B) significa A es prelación de B

prela(ci2691, ci3661).
prela(ci2525, ci2691).
prela(ci2691, ci3641).
prela(ci3641, ci3725).
prela(ci3725, ci3825).
prela(ma1111, ci2525).

% Intento de reglas para ver si sirve

% 1. prelacion_directa(A, B): Es verdadero si A es una prelación directa de B. [cite: 21]
% Esto es simplemente una reformulación de nuestro hecho 'prela'.
prelacion_directa(A, B) :-
    prela(A, B).

% 2. prelacion_total(A, B): Es verdadero si A es una prelación de B, ya sea directa o indirecta 

% Caso Base: A es una prelación directa de B.
prelacion_total(A, B) :-
    prelacion_directa(A, B),
    !.

% Caso Recursivo: A es prelación de B si A prela a una materia intermedia C, Y C es prelación total de B.
prelacion_total(A, B) :-
    prelacion_directa(A, Z),
    prelacion_total(Z, B).
