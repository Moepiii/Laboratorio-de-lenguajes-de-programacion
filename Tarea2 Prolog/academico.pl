% --- INICIO: academico.pl ---

% Base de Conocimiento (Hechos) [cite: 31]

% dicta(Profesor, Materia) [cite: 32]
dicta(leonardo, ci3661). % [cite: 33]
dicta(maria, ci3725).    % [cite: 34]
dicta(pedro, ci2691).    % [cite: 35]

% cursa(Estudiante, Materia) [cite: 36]
cursa(ana, ci3661).     % [cite: 37]
cursa(ana, ci3725).     % [cite: 38]
cursa(juan, ci3661).    % [cite: 39]
cursa(elena, ci2691).   % [cite: 40]
cursa(elena, ci3725).   % [cite: 41]

% nota(Estudiante, Materia, Nota) [cite: 42]
nota(ana, ci3661, 88).    % [cite: 43]
nota(ana, ci3725, 70).    % [cite: 44]
nota(juan, ci3661, 40).   % [cite: 45]
nota(elena, ci2691, 51).  % [cite: 46]
% (elena no tiene nota en ci3725) [cite: 47]

% --- Predicados a Implementar --- [cite: 48]

% 1. profesor_de(Prof, Est): Verdadero si Prof le da clases a Est. [cite: 49]
%    Esto ocurre si el profesor dicta una materia Y el estudiante cursa esa misma materia.
profesor_de(Prof, Est) :-
    dicta(Prof, Materia),
    cursa(Est, Materia).

% 2. aprobado(Est, Mat): Verdadero si Est cursó Mat y obtuvo 50 o más. [cite: 50]
%    Buscamos una nota para el estudiante en esa materia, y verificamos que sea >= 50.
aprobado(Est, Mat) :-
    nota(Est, Mat, Nota),
    Nota >= 50.

% 3. aplazado(Est): Verdadero si Est ha aplazado (menos de 50) al menos una materia. [cite: 51]
%    Buscamos CUALQUIER nota del estudiante (usamos '_' para la materia) y verificamos si es < 50.
aplazado(Est) :-
    nota(Est, _, Nota),
    Nota < 50.

% 4. cursando_sin_nota(Est, Mat): Verdadero si Est cursa Mat, pero no tiene nota registrada. [cite: 52]
%    Esto requiere dos condiciones:
%    1. El estudiante 'cursa' la materia.
%    2. NO debe existir (usando 'not' o '\+') un hecho 'nota' para ese estudiante y materia.
cursando_sin_nota(Est, Mat) :-
    cursa(Est, Mat),
    not(nota(Est, Mat, _)).

% --- FIN: academico.pl ---