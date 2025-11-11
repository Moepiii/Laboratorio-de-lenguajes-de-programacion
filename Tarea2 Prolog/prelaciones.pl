% --- INICIO: prelaciones.pl ---

% Base de Conocimiento (Hechos) [cite: 8]
% prela(A, B)
% Basado en los ejemplos de consulta [cite: 24, 25] y el comentario[cite: 11],
% interpretamos que prela(A, B) significa "A es prelación directa de B".
% Esto contradice la definición de texto en la línea [cite: 8] ("Prelacion es una prelación directa de Materia"),
% pero es la única forma en que las consultas de ejemplo [cite: 24, 25] funcionan.

prela(ci2691, ci3661). % [cite: 10, 11]
prela(ci2525, ci2691). % [cite: 12, 13]
prela(ci2691, ci3641). % [cite: 14, 15]
prela(ci3641, ci3725). % [cite: 16, 17]
prela(ci3725, ci3825). % [cite: 18]

% Nota: El hecho [cite: 19] dice 'mal111', pero el ejemplo de consulta [cite: 25]
% usa 'ma1111' y espera 'true'. Usamos 'ma1111' para que el ejemplo funcione.
prela(ma1111, ci2525).

% --- Predicados a Implementar ---

% 1. prelacion_directa(A, B): Es verdadero si A es una prelación directa de B. [cite: 21]
% Esto es simplemente una reformulación de nuestro hecho 'prela'.
prelacion_directa(A, B) :-
    prela(A, B).

% 2. prelacion_total(A, B): Es verdadero si A es una prelación de B,
%    ya sea directa o indirecta (recursivamente). [cite: 22]

% Caso Base: A es una prelación directa de B.
prelacion_total(A, B) :-
    prelacion_directa(A, B).

% Caso Recursivo: A es prelación de B si A prela a una materia intermedia Z,
% y Z (a su vez) es prelación total de B.
prelacion_total(A, B) :-
    prelacion_directa(A, Z),
    prelacion_total(Z, B).

% --- FIN: prelaciones.pl ---