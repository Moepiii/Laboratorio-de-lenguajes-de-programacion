#Baudilio Velasquez (18-10665)
#Rafael Valera (16-11202)

require_relative 'RPTLS' 

Shoes.app(title: "Proyecto 3: RPTLS - Ruby", width: 950, height: 750, resizable: false) do
  
  background "#1A1A1A" 

  
  # A. pide nombres
  n_user = ask("1. Introduce TU nombre:")
  nombre_u = (n_user.nil? || n_user.strip.empty?) ? "Jugador" : n_user.strip

  n_cpu = ask("2. Introduce el nombre de la CPU:")
  nombre_c = (n_cpu.nil? || n_cpu.strip.empty?) ? "CPU" : n_cpu.strip

  # B. estrategia para la cpu
  strat_input = ask("3. Escribe la Estrategia de la CPU:\n\nOpciones disponibles:\n- Uniforme\n- Sesgada\n- Copiar\n- Pensar\n\n(Si lo dejas vacío será Uniforme)")
  
  # Lógica para crear la estrategia 
  estrategia_obj = nil
  opcion_s = strat_input.to_s.strip.capitalize

  case opcion_s
  when "Sesgada"
    # Usamos unos pesos por defecto para que no explote pidiendo más datos
    pesos_default = {:Piedra => 50, :Papel => 10, :Tijera => 10, :Lagarto => 10, :Spock => 10}
    estrategia_obj = Sesgada.new(pesos_default)
    alert("Has elegido SESGADA.\n(Pesos por defecto: Piedra=50, otros=10)")
  when "Copiar"
    estrategia_obj = Copiar.new
  when "Pensar"
    estrategia_obj = Pensar.new
  else
    # Uniforme es el default si escribe mal o lo deja vacío
    estrategia_obj = Uniforme.new([:Piedra, :Papel, :Tijera, :Lagarto, :Spock])
    alert("Estrategia configurada: UNIFORME") unless opcion_s == "Uniforme"
  end

  # C. Modo de Juego
  modo_input = ask("4. MODO DE JUEGO:\nEscribe 'A' para 'Alcanzar Puntos'\nEscribe 'R' para 'Rondas'")
  modo_juego = (modo_input && modo_input.upcase.include?('R')) ? :rondas : :alcanzar

  # D. Valor de N
  val_n = ask("5. Introduce el valor de N (Puntos o Rondas):")
  meta_n = val_n.to_i
  meta_n = 5 if meta_n <= 0

 # 2 si inicias partida
  
  @partida = Partida.new({
    :nombre_j1 => nombre_u, 
    :nombre_j2 => nombre_c, 
    :estrategia_cpu => estrategia_obj,
    :modo => modo_juego,
    :meta => meta_n
  })

  #interfaz

  # Cabecera
  stack align: "center", margin_top: 10 do
    @header_titulo = title "RPTLS - Ruby", stroke: white, size: 16
    
    info_texto = (@partida.modo == :rondas) ? "Ronda 0/#{@partida.meta}" : "Meta: #{@partida.meta} Pts"
    @header_info = para "Modo: #{info_texto} | CPU: #{estrategia_obj.nombre}", stroke: gray
    
    @score_label = title "0 | 0", stroke: "#40E0D0", size: 34, margin_top: 10
  end
  
  # Área de juego
  para "Elige tu jugada:", stroke: "#AAA", align: "center", margin_top: 20
  
  @botones_flow = flow margin_left: 60, margin_top: 10 do
    ["Piedra", "Papel", "Tijera", "Lagarto", "Spock"].each do |op|
      stack width: 130, align: "center" do
        image("#{op}.png", width: 70, height: 70).click { procesar_jugada(op) }
        para op, stroke: white, size: 10, align: "center"
      end
    end
  end

  # Resultados
  stack margin_top: 20, align: "center" do
    para "--- RESULTADO ---", stroke: gray
    @resultado_texto = title "...", stroke: white, size: 20, align: "center"
    
    flow margin_top: 20 do
      stack width: "50%", align: "center" do
        @lbl_j1 = para @partida.nombre_j1, stroke: "#40E0D0", size: 16
        @img_j1 = image "Spock.png", width: 100, height: 100
        @img_j1.hide
      end
      stack width: "50%", align: "center" do
        @lbl_j2 = para @partida.nombre_j2, stroke: "#FF6347", size: 16
        @img_j2 = image "Spock.png", width: 100, height: 100
        @img_j2.hide
      end
    end
  end
  
  # Botón Reiniciar (vuelve a pedir datos)
  stack align: "center", margin_top: 20 do
    button "Reiniciar (F5)", width: 200 do
      alert("Para reiniciar con nuevos nombres/estrategia, cierra y abre el programa.")
    end
  end

  # 4 logica de actualizacion de UI

  def procesar_jugada(op)
    res = @partida.jugar_ronda(op)
    
    if res.nil?
      alert("¡La partida ya terminó!")
      return
    end

    j1, j2, puntos = res
    @img_j1.path = "#{j1}.png"; @img_j1.show
    @img_j2.path = "#{j2}.png"; @img_j2.show

    msg = ""
    col = white
    if puntos == [1, 0]
      msg = "#{j1} #{j1.accion_victoria(j2)} #{j2}"; col = "#00FF00"
    elsif puntos == [0, 1]
      msg = "#{j2} #{j2.accion_victoria(j1)} #{j1}"; col = "#FF0000"
    else
      msg = "Empate"; col = "#FFFF00"
    end

    fin, fin_msg, fin_col = @partida.verificar_victoria
    if fin
      @resultado_texto.text = fin_msg
      @resultado_texto.stroke = fin_col
      alert(fin_msg) # Aviso final extra
    else
      @resultado_texto.text = msg
      @resultado_texto.stroke = col
    end
    actualizar_marcador
  end

  def actualizar_marcador
    info = (@partida.modo == :rondas) ? "Ronda #{@partida.rondas_jugadas}/#{@partida.meta}" : "Meta: #{@partida.meta}"
    @score_label.text = "#{@partida.puntos_j1}  |  #{@partida.puntos_j2}"
    @header_info.text = "Modo: #{info} | CPU: #{@partida.estrategia_cpu.nombre}"
  end

end