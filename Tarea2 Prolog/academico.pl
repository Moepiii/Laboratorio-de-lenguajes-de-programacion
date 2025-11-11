% academico.pl

% datos y demas

% dicta(Profesor, Materia) 
dicta(leonardo, ci3661).
dicta(maria, ci3725). 
dicta(pedro, ci2691). 

% cursa(Estudiante, Materia) 
cursa(ana, ci3661).
cursa(ana, ci3725).
cursa(juan, ci3661).
cursa(elena, ci2691).
cursa(elena, ci3725).

% nota(Estudiante, Materia, Nota)
nota(ana, ci3661, 88).
nota(ana, ci3725, 70). 
nota(juan, ci3661, 40). 
nota(elena, ci2691, 51).
% (elena no tiene nota en ci3725)


% Predicados 
% 1. profesor_de(Prof, Est): Verdadero si Prof le da clases a Est. Esto ocurre si el profesor dicta una materia Y el estudiante cursa esa misma materia.
profesor_de(Prof, Est) :-
    dicta(Prof, Materia),
    cursa(Est, Materia).

% 2. aprobado(Est, Mat): Verdadero si Est cursó Mat y obtuvo 50 o más. Buscamos una nota para el estudiante en esa materia, y verificamos que sea >= 50.
aprobado(Est, Mat) :-
    nota(Est, Mat, Nota),
    Nota >= 50.

% 3. aplazado(Est): Verdadero si Est ha aplazado (menos de 50) al menos una materia. Buscamos CUALQUIER nota del estudiante (usamos '_' para la materia) y verificamos si es < 50.
aplazado(Est) :-
    nota(Est, _, Nota),
    Nota < 50.

% 4. cursando_sin_nota(Est, Mat): Verdadero si Est cursa Mat, pero no tiene nota registrada.
%    Esto requiere dos condiciones:
%    1. El estudiante 'cursa' la materia.
%    2. NO debe existir (usando 'not' o '\+') un hecho 'nota' para ese estudiante y materia.
cursando_sin_nota(Est, Mat) :-
    cursa(Est, Mat),
    not(nota(Est, Mat, _)).
