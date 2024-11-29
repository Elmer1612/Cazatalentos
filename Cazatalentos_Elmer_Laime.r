
#Estrategia General: 
# La estrategia implementada en el código tiene como objetivo identificar a la mejor jugadora (aquella con probabilidad de enceste = 0.50) utilizando un proceso iterativo que combina la reducción progresiva de participantes con la optimización de recursos (tiros libres promedio). 
# Este enfoque busca minimizar la cantidad de tiros libres necesarios mientras se mantiene una probabilidad de éxito superior al 99%.



# Configuración inicial
set.seed(100019)  # Se fija una semilla para garantizar que los resultados sean reproducibles, tambien funciona con otras de mis semillas: 100079, 100129, 100153, 100191

# Parámetros iniciales
n_jugadoras <- 100  # Número total de jugadoras en la competencia.
p_mejor <- 0.50  # Probabilidad fija de enceste de la mejor jugadora.
p_peloton <- seq(0.204, 0.40, length.out = n_jugadoras - 1)  # Probabilidades de enceste del resto de jugadoras.
p_jugadoras <- c(p_mejor, p_peloton)  # Se combina la mejor jugadora con las jugadoras del pelotón.
n_simulaciones <- 100000  # Total de simulaciones para evaluar la estrategia.

# Función para simular tiros libres
simular_tiros <- function(n_tiros, p_jugadoras) {
  # Esta función simula los tiros libres realizados por cada jugadora.
  # Utiliza una distribución binomial en la que:
  # - `n_tiros` representa el número de intentos de cada jugadora.
  # - `p_jugadoras[i]` es la probabilidad de éxito específica de la jugadora `i`.
  rbinom(length(p_jugadoras), n_tiros, p_jugadoras)
}

# Medición del tiempo de ejecución
inicio <- Sys.time()  # Se registra el tiempo inicial para medir la duración total del script.

# Estrategia optimizada
estrategia_optimizada <- function(p_jugadoras, tiros_inicial = 75, rondas = 5) {
  # Esta función implementa la estrategia optimizada para identificar a la mejor jugadora
  # mediante un proceso iterativo que combina reducción progresiva y simulaciones ajustadas.
  
  n_jugadoras <- length(p_jugadoras)  # Se determina el número total de jugadoras.
  jugadoras_restantes <- seq_len(n_jugadoras)  # Se inicializa una lista con las jugadoras que compiten.
  tiros_actuales <- tiros_inicial  # Se establece el número inicial de tiros asignados a cada jugadora.
  total_tiros <- 0  # Se inicializa un contador para registrar el total de tiros realizados.
  
  # Se establecen priors iniciales para las jugadoras utilizando una distribución Beta.
  alphas <- rep(1, n_jugadoras)  # Se inicializan los éxitos para cada jugadora.
  betas <- rep(1, n_jugadoras)  # Se inicializan los fallos para cada jugadora.
  
  for (ronda in seq_len(rondas)) {
    # Se simulan los tiros libres para las jugadoras que aún permanecen en la competencia.
    encestes <- simular_tiros(tiros_actuales, p_jugadoras[jugadoras_restantes])
    total_tiros <- total_tiros + length(jugadoras_restantes) * tiros_actuales  # Se acumula el total de tiros realizados.
    
    # Se actualizan los parámetros de la distribución Beta con los resultados de los tiros.
    alphas[jugadoras_restantes] <- alphas[jugadoras_restantes] + encestes  # Los éxitos aumentan según los encestes logrados.
    betas[jugadoras_restantes] <- betas[jugadoras_restantes] + tiros_actuales - encestes  # Los fallos aumentan según los tiros no acertados.
    
    # Se calcula la media posterior para estimar la probabilidad actualizada de enceste de cada jugadora.
    medias_posteriores <- alphas[jugadoras_restantes] / (alphas[jugadoras_restantes] + betas[jugadoras_restantes])
    
    # Se aplica una reducción progresiva de jugadoras según su rendimiento en la ronda actual.
    if (ronda == 1) {
      # En la primera ronda, se retiene solo al 25% de las jugadoras con mejor rendimiento.
      mejores <- order(medias_posteriores, decreasing = TRUE)[1:ceiling(length(jugadoras_restantes) * 0.25)]
    } else if (ronda < rondas) {
      # En las rondas intermedias, se retiene al 50% de las jugadoras con mejor rendimiento.
      mejores <- order(medias_posteriores, decreasing = TRUE)[1:ceiling(length(jugadoras_restantes) * 0.50)]
    } else {
      # En la última ronda, se identifica directamente a la ganadora con la mejor media posterior.
      mejor_idx <- which.max(medias_posteriores)
      jugadoras_restantes <- jugadoras_restantes[mejor_idx]
      break
    }
    # Se actualiza la lista de jugadoras restantes con las mejores seleccionadas.
    jugadoras_restantes <- jugadoras_restantes[mejores]
    
    # Se termina el proceso si solo queda una jugadora.
    if (length(jugadoras_restantes) == 1) break
    
    # Se incrementa el número de tiros en cada ronda para mejorar la discriminación entre las jugadoras restantes.
    tiros_actuales <- tiros_actuales + 50
  }
  
  # Se retorna un resumen de los resultados:
  # - El índice de la jugadora ganadora.
  # - El total de tiros realizados en todas las rondas.
  return(list(ganadora = jugadoras_restantes[1], total_tiros = total_tiros))
}

# Validación de la estrategia optimizada
resultados <- replicate(n_simulaciones, estrategia_optimizada(p_jugadoras), simplify = FALSE)  # Se ejecuta la estrategia en múltiples simulaciones.
ganadores <- sapply(resultados, function(x) x$ganadora)  # Se extraen las ganadoras de cada simulación.
total_tiros <- mean(sapply(resultados, function(x) x$total_tiros))  # Se calcula el promedio de tiros realizados.
probabilidad_exito <- mean(ganadores == 1)  # Se calcula la probabilidad de identificar correctamente a la mejor jugadora.

# Medición del tiempo de ejecución
fin <- Sys.time()
tiempo_ejecucion <- fin - inicio  # Se calcula la duración total de la ejecución.

# Mostrar resultados
cat("Probabilidad de éxito optimizada:", probabilidad_exito, "\n")  # Se imprime la probabilidad de éxito alcanzada.
cat("Tiros libres promedio utilizados optimizados:", total_tiros, "\n")  # Se imprime el promedio de tiros realizados.
cat("Tiempo de ejecución:", tiempo_ejecucion, "segundos\n")  # Se imprime el tiempo total de ejecución.


#Resultados a Mostrar luego de la ejecución:
#Probabilidad de éxito optimizada: 0.99455 
#Tiros libres promedio utilizados optimizados: 15575
#Tiempo de ejecución: 19.23197 segundos











