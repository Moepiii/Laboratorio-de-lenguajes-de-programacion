#Baudilio Velasquez (18-10665)
#Rafael Valera (16-11202)

# jerarquia de jugadas

class Jugada
  attr_reader :nombre
  def to_s; @nombre; end

  def puntos(contrincante)
    return [0, 0] if self.class == contrincante.class
    le_gana_a?(contrincante) ? [1, 0] : [0, 1]
  end

  def le_gana_a?(otra); raise NotImplementedError; end
  def accion_victoria(otra); "gana a"; end
end

class Piedra < Jugada
  def initialize; @nombre = "Piedra"; end
  def le_gana_a?(otra); otra.is_a?(Lagarto) || otra.is_a?(Tijera); end
  def accion_victoria(otra)
    return "aplasta a" if otra.is_a?(Lagarto); return "aplasta a" if otra.is_a?(Tijera); "gana a"
  end
end

class Papel < Jugada
  def initialize; @nombre = "Papel"; end
  def le_gana_a?(otra); otra.is_a?(Piedra) || otra.is_a?(Spock); end
  def accion_victoria(otra)
    return "tapa a" if otra.is_a?(Piedra); return "desautoriza a" if otra.is_a?(Spock); "gana a"
  end
end

class Tijera < Jugada
  def initialize; @nombre = "Tijera"; end
  def le_gana_a?(otra); otra.is_a?(Papel) || otra.is_a?(Lagarto); end
  def accion_victoria(otra)
    return "corta a" if otra.is_a?(Papel); return "decapita a" if otra.is_a?(Lagarto); "gana a"
  end
end

class Lagarto < Jugada
  def initialize; @nombre = "Lagarto"; end
  def le_gana_a?(otra); otra.is_a?(Spock) || otra.is_a?(Papel); end
  def accion_victoria(otra)
    return "envenena a" if otra.is_a?(Spock); return "devora a" if otra.is_a?(Papel); "gana a"
  end
end

class Spock < Jugada
  def initialize; @nombre = "Spock"; end
  def le_gana_a?(otra); otra.is_a?(Tijera) || otra.is_a?(Piedra); end
  def accion_victoria(otra)
    return "rompe a" if otra.is_a?(Tijera); return "vaporiza a" if otra.is_a?(Piedra); "gana a"
  end
end

# jerarquia de estrategia

class Estrategia
  attr_accessor :nombre
  def initialize(nombre); @nombre = nombre; end
  def prox(historial); raise NotImplementedError; end
  
  def self.crear_jugada(simbolo)
    case simbolo.to_s
    when "Piedra" then Piedra.new
    when "Papel" then Papel.new
    when "Tijera" then Tijera.new
    when "Lagarto" then Lagarto.new
    when "Spock" then Spock.new
    end
  end
end

class Manual < Estrategia
  def initialize; super("Manual"); end
  def prox(historial); nil; end
end

class Uniforme < Estrategia
  def initialize(lista_posibles)
    super("Uniforme")
    @lista = lista_posibles.uniq
  end
  def prox(historial)
    eleccion = @lista.sample
    Estrategia.crear_jugada(eleccion)
  end
end

class Sesgada < Estrategia
  def initialize(mapa_pesos)
    super("Sesgada")
    @opciones = []
    mapa_pesos.each do |simbolo, peso|
      peso.times { @opciones << simbolo }
    end
  end
  def prox(historial)
    eleccion = @opciones.sample
    Estrategia.crear_jugada(eleccion)
  end
end

class Copiar < Estrategia
  def initialize; super("Copiar"); end
  def prox(historial_oponente)
    return Uniforme.new([:Piedra, :Papel, :Tijera, :Lagarto, :Spock]).prox(nil) if historial_oponente.nil? || historial_oponente.empty?
    historial_oponente.last.class.new 
  end
end

class Pensar < Estrategia
  def initialize; super("Pensar"); end
  def prox(historial_oponente)
    return Uniforme.new([:Piedra, :Papel, :Tijera, :Lagarto, :Spock]).prox(nil) if historial_oponente.nil? || historial_oponente.empty?
    frecuencias = Hash.new(0)
    historial_oponente.each { |j| frecuencias[j.class] += 1 }
    clase_mas_probable = frecuencias.max_by { |k, v| v }[0]
    jugada_esperada = clase_mas_probable.new
    posibles = [Piedra, Papel, Tijera, Lagarto, Spock]
    ganadoras = posibles.select { |c| c.new.le_gana_a?(jugada_esperada) }
    ganadoras.sample.new
  end
end

# clase padre o partida

class Partida
  attr_reader :puntos_j1, :puntos_j2, :rondas_jugadas, :nombre_j1, :nombre_j2, :modo, :meta
  attr_accessor :estrategia_cpu

  def initialize(config)
    @nombre_j1 = config[:nombre_j1] || "Jugador"
    @nombre_j2 = config[:nombre_j2] || "CPU"
    @estrategia_cpu = config[:estrategia_cpu]
    @modo = config[:modo]
    @meta = config[:meta].to_i
    @puntos_j1 = 0; @puntos_j2 = 0; @rondas_jugadas = 0
    @historial_j1 = []; @historial_j2 = []
    @terminada = false
  end

  def jugar_ronda(eleccion_usuario_str)
    return nil if @terminada
    j1_move = Estrategia.crear_jugada(eleccion_usuario_str)
    j2_move = @estrategia_cpu.prox(@historial_j1)
    @historial_j1 << j1_move
    @historial_j2 << j2_move
    res = j1_move.puntos(j2_move)
    @puntos_j1 += res[0]
    @puntos_j2 += res[1]
    @rondas_jugadas += 1
    return [j1_move, j2_move, res]
  end

  def verificar_victoria
    mensaje = nil; color = nil; fin = false
    if @modo == :alcanzar
      if @puntos_j1 >= @meta
        mensaje = "¡#{@nombre_j1.upcase} GANA! (#{@meta} pts)"; color = "#00FF00"; fin = true
      elsif @puntos_j2 >= @meta
        mensaje = "¡#{@nombre_j2.upcase} GANA! (#{@meta} pts)"; color = "#FF0000"; fin = true
      end
    elsif @modo == :rondas
      if @rondas_jugadas >= @meta
        fin = true
        if @puntos_j1 > @puntos_j2; mensaje = "¡FIN! GANA: #{@nombre_j1.upcase}"; color = "#00FF00"
        elsif @puntos_j2 > @puntos_j1; mensaje = "¡FIN! GANA: #{@nombre_j2.upcase}"; color = "#FF0000"
        else; mensaje = "¡FIN! EMPATE"; color = "#FFFF00"; end
      end
    end
    @terminada = true if fin
    return [fin, mensaje, color]
  end

  def reiniciar
    @puntos_j1 = 0; @puntos_j2 = 0; @rondas_jugadas = 0
    @historial_j1 = []; @historial_j2 = []
    @terminada = false
  end
end