# Proyecto 3: Piedra, Papel, Tijera, Lagarto, Spock

## Integrantes
- **Baudilio Velasquez** (Carnet: 18-10665)
- **Rafael Valera** (Carnet: 16-11202)

## Descripción
Implementación modular del juego extendido utilizando Ruby y la gema Shoes 4 para la interfaz gráfica.
Se han separado las responsabilidades en dos archivos principales:
- `RPTLS.rb`: Contiene el modelo y la lógica (Clases Jugada, Estrategia, Partida).
- `main.rb`: Contiene la vista y la ejecución (Interfaz Shoes).

## Requisitos
- Ruby instalado, JRuby (No la ultima version si no la anterior o le dara problemas) recomendado para Shoes 4.
- Gema `shoes` instalada.

## Estructura de Archivos
El proyecto debe contener los siguientes archivos en la misma carpeta:
1. `main.rb` (Ejecutable)
2. `RPTLS.rb` (Lógica)
3. Imágenes: `Piedra.png`, `Papel.png`, `Tijera.png`, `Lagarto.png`, `Spock.png`
4. `README.md`

## cosas a tomar en cuenta

- Cuando ejecuta el juego se le abre una ventana pidiendo el nombre del jugador, luego otra para la cpu
- Luego le pedira una estrategia para la cpu, debe escribirlo bien o se pone una por defecto (para sesgada viene por defecto piedra debe cambiar el codigo si quiere otra a favor)
- Luego modo de juego
- Luego otra ventana que le pedira informacion acorde a lo seleccionado (debe ser un numero)
- Luego se inicia al juego.
- No hay boton de continuar una vez
- para jugar seleccione alguna figura, el juego le dice si gano empato o perdio en colores verde,azul y rojo, Acto seguido puede elegir otra para la siguiente ronda y se repite el bucle
- Reiniciar solo lanza un mensaje con lo que debe hacer para reiniciar

## Cómo ejecutar el proyecto
Abra la terminal en la carpeta del proyecto y ejecute:

```bash
shoes main.rb
