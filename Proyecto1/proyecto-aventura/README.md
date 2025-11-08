# Proyecto 1 - Motor de Aventura de Texto

## Integrantes
- Baudilio Velasquez (carnet 18-10665)
- Rafael Valera (carnet 16-11202)

## Resumen
Este repositorio contiene el motor de aventura de texto solicitado en el Proyecto 1 del curso CI-3661. El objetivo es ofrecer una base reutilizable que cargue mundos desde archivos, parsee comandos en espanol y mantenga el estado del juego de forma funcional.

## Requisitos
- Stack 2.x
- GHC 9.4.7 (Stack lo instala automaticamente con el resolver configurado)

## Compilar y ejecutar
```bash
stack build
stack run
```
El ejecutable lee el archivo `mundo.txt` incluido en la raiz del proyecto. Asegurate de mantener el archivo en el mismo directorio desde el que se ejecuta el binario.

## Pruebas automatizadas
```bash
stack test
```
La suite usa `hspec` y cubre el parsing de comandos y la validacion del archivo de mundo.

## Decisiones de diseno
- **Map para salas, items y salidas:** Usamos `Data.Map` porque nos permite realizar busquedas por nombre o direccion en tiempo logaritmico y evita duplicados. Esto facilita validar referencias cruzadas y garantiza consistencia del mundo.
- **Listas para inventario del jugador:** El inventario generalmente es pequeno y la operacion principal es agregar al frente. Mantenerlo como lista simplifica la serializacion y la presentacion en pantalla, pero validamos la existencia de objetos durante la carga para evitar referencias invalidas.
- **Parsers basados en Either:** `Engine.Parser` y `Engine.Persistence` retornan `Either String ...`, lo que nos permite propagar mensajes de error descriptivos hasta la interfaz de usuario sin lanzar excepciones.

## Separacion de logica pura y efectos
- `Engine.Types`, `Engine.Parser`, `Engine.Core` y `Engine.Persistence` contienen exclusivamente logica pura. `Engine.Core.processCommand` actualiza el estado del juego sin realizar IO, mientras que `Engine.Persistence.parseWorldContent` transforma texto en estructuras puras.
- `app/Main.hs` maneja los efectos: lectura del archivo de mundo, ciclo de entrada/salida y mensajes al usuario. De esta manera, el motor puede reutilizarse en otros front-ends (por ejemplo, pruebas automatizadas o interfaces graficas) sin modificaciones en la logica central.


¡Cualquier inconsistencia (objeto inexistente, direccion invalida, sala duplicada) hace que la carga falle con un mensaje concreto, garantizando que el juego solo arranque con mundos validos!0
