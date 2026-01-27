# ============================================================================
# EJEMPLO INICIAL: ALEATORIZACIÓN Y BALANCE
# Conexión con Clase 1 - Demostración de por qué la aleatorización funciona
# ============================================================================

# Limpiar workspace
rm(list = ls())

set.seed(2026)  # Para reproducibilidad

cat("\n============================================================\n")
cat("DEMOSTRACIÓN: ¿POR QUÉ FUNCIONA LA ALEATORIZACIÓN?\n")
cat("============================================================\n\n")

# ============================================================================
# SIMULAR UNA POBLACIÓN DE ESTUDIANTES
# ============================================================================

n <- 1000  # 1000 estudiantes

cat("Generando población de", n, "estudiantes...\n\n")

# Características pre-tratamiento (covariables)
ingreso <- exp(rnorm(n, mean = 9, sd = 0.8))
motivacion <- rnorm(n, mean = 50, sd = 15)
motivacion <- pmin(pmax(motivacion, 0), 100)
habilidad <- rnorm(n, mean = 70, sd = 12)
habilidad <- pmin(pmax(habilidad, 30), 100)

# Crear data frame
poblacion <- data.frame(
  id = 1:n,
  ingreso = round(ingreso, 2),
  motivacion = round(motivacion, 1),
  habilidad = round(habilidad, 1)
)

# Resultado potencial SIN tratamiento: Y(0)
# Depende de habilidad y motivación
poblacion$Y0 <- 40 + 
  0.3 * poblacion$habilidad + 
  0.2 * poblacion$motivacion + 
  rnorm(n, 0, 8)
poblacion$Y0 <- pmin(pmax(poblacion$Y0, 30), 100)

# Efecto causal VERDADERO (heterogéneo)
# El programa ayuda más a estudiantes de menor ingreso
efecto_verdadero <- 15 - 0.001 * poblacion$ingreso +
  0.05 * (poblacion$habilidad - mean(poblacion$habilidad))

# Resultado potencial CON tratamiento: Y(1)
poblacion$Y1 <- poblacion$Y0 + efecto_verdadero

# ATE verdadero en la población
ATE_verdadero <- mean(poblacion$Y1 - poblacion$Y0)

cat("=== VERDAD EN LA POBLACIÓN (que NO observamos) ===\n")
cat("ATE verdadero:", round(ATE_verdadero, 2), "puntos\n\n")

# ============================================================================
# ESCENARIO 1: ASIGNACIÓN ALEATORIA (EXPERIMENTO)
# ============================================================================

cat("\n--- ESCENARIO 1: ALEATORIZACIÓN ---\n\n")

# Asignar tratamiento ALEATORIAMENTE a la mitad
n_tratados <- n / 2
tratamiento_aleatorio <- sample(c(rep(1, n_tratados), rep(0, n - n_tratados)))

poblacion$tratamiento_aleatorio <- tratamiento_aleatorio

# Observar resultados (solo vemos uno de los dos para cada estudiante)
poblacion$Y_observado_aleatorio <- ifelse(
  poblacion$tratamiento_aleatorio == 1,
  poblacion$Y1,
  poblacion$Y0
)

# Calcular diferencia observada
Y1_obs_alea <- mean(poblacion$Y_observado_aleatorio[poblacion$tratamiento_aleatorio == 1])
Y0_obs_alea <- mean(poblacion$Y_observado_aleatorio[poblacion$tratamiento_aleatorio == 0])
ATE_estimado_alea <- Y1_obs_alea - Y0_obs_alea

cat("Estimación con aleatorización:\n")
cat("  Promedio tratados:", round(Y1_obs_alea, 2), "\n")
cat("  Promedio controles:", round(Y0_obs_alea, 2), "\n")
cat("  ATE estimado:", round(ATE_estimado_alea, 2), "\n")
cat("  Error de estimación:", round(ATE_estimado_alea - ATE_verdadero, 2), "\n\n")

# Verificar balance en covariables
cat("Balance en covariables (diferencias entre tratados y controles):\n")
cat("  Ingreso: ", 
    round(mean(poblacion$ingreso[poblacion$tratamiento_aleatorio == 1]) - 
          mean(poblacion$ingreso[poblacion$tratamiento_aleatorio == 0]), 2), "\n")
cat("  Motivación: ", 
    round(mean(poblacion$motivacion[poblacion$tratamiento_aleatorio == 1]) - 
          mean(poblacion$motivacion[poblacion$tratamiento_aleatorio == 0]), 2), "\n")
cat("  Habilidad: ", 
    round(mean(poblacion$habilidad[poblacion$tratamiento_aleatorio == 1]) - 
          mean(poblacion$habilidad[poblacion$tratamiento_aleatorio == 0]), 2), "\n\n")

cat("CONCLUSIÓN: Con aleatorización, los grupos son COMPARABLES\n")
cat("           (diferencias pequeñas, solo por azar)\n\n")

# ============================================================================
# ESCENARIO 2: ASIGNACIÓN NO ALEATORIA (OBSERVACIONAL)
# ============================================================================

cat("\n--- ESCENARIO 2: SIN ALEATORIZACIÓN (auto-selección) ---\n\n")

# Probabilidad de participar depende de motivación e ingreso
# Más motivados y más pobres tienden a participar más
prob_participar <- plogis(-2 + 
                           0.05 * poblacion$motivacion + 
                           -0.0005 * poblacion$ingreso)

tratamiento_sesgado <- rbinom(n, 1, prob_participar)
poblacion$tratamiento_sesgado <- tratamiento_sesgado

cat("Proporción que recibe tratamiento:", 
    round(mean(tratamiento_sesgado), 3), "\n\n")

# Observar resultados
poblacion$Y_observado_sesgado <- ifelse(
  poblacion$tratamiento_sesgado == 1,
  poblacion$Y1,
  poblacion$Y0
)

# Calcular diferencia observada
Y1_obs_sesgo <- mean(poblacion$Y_observado_sesgado[poblacion$tratamiento_sesgado == 1])
Y0_obs_sesgo <- mean(poblacion$Y_observado_sesgado[poblacion$tratamiento_sesgado == 0])
ATE_estimado_sesgo <- Y1_obs_sesgo - Y0_obs_sesgo

cat("Estimación sin aleatorización:\n")
cat("  Promedio tratados:", round(Y1_obs_sesgo, 2), "\n")
cat("  Promedio controles:", round(Y0_obs_sesgo, 2), "\n")
cat("  ATE estimado (SESGADO):", round(ATE_estimado_sesgo, 2), "\n")
cat("  Error de estimación:", round(ATE_estimado_sesgo - ATE_verdadero, 2), "\n\n")

# Verificar balance en covariables
cat("Balance en covariables (diferencias entre tratados y controles):\n")
cat("  Ingreso: ", 
    round(mean(poblacion$ingreso[poblacion$tratamiento_sesgado == 1]) - 
          mean(poblacion$ingreso[poblacion$tratamiento_sesgado == 0]), 2), "\n")
cat("  Motivación: ", 
    round(mean(poblacion$motivacion[poblacion$tratamiento_sesgado == 1]) - 
          mean(poblacion$motivacion[poblacion$tratamiento_sesgado == 0]), 2), "\n")
cat("  Habilidad: ", 
    round(mean(poblacion$habilidad[poblacion$tratamiento_sesgado == 1]) - 
          mean(poblacion$habilidad[poblacion$tratamiento_sesgado == 0]), 2), "\n\n")

cat("PROBLEMA: Sin aleatorización, los grupos NO son comparables\n")
cat("          (diferencias grandes en covariables importantes)\n\n")

# ============================================================================
# DESCOMPONER EL SESGO
# ============================================================================

cat("\n--- DESCOMPOSICIÓN DEL SESGO ---\n\n")

# Diferencia observada = Efecto causal + Sesgo de selección
sesgo_seleccion <- (mean(poblacion$Y0[poblacion$tratamiento_sesgado == 1]) - 
                    mean(poblacion$Y0[poblacion$tratamiento_sesgado == 0]))

cat("Diferencia observada (sin aleatorización):", 
    round(ATE_estimado_sesgo, 2), "\n")
cat("  = Efecto causal verdadero:", round(ATE_verdadero, 2), "\n")
cat("  + Sesgo de selección:", round(sesgo_seleccion, 2), "\n\n")

cat("Verificación:", 
    round(ATE_verdadero + sesgo_seleccion, 2), 
    "≈", round(ATE_estimado_sesgo, 2), "\n\n")

# ============================================================================
# VISUALIZACIÓN
# ============================================================================

cat("\n--- Generando visualizaciones ---\n")

# Gráfico 1: Distribución de propensity score (solo para observacional)
png("ejemplo_aleatorio_vs_observacional.png", width = 1200, height = 500)
par(mfrow = c(1, 2))

# Panel izquierdo: Aleatorio
hist(poblacion$motivacion[poblacion$tratamiento_aleatorio == 1],
     col = rgb(1, 0, 0, 0.5), breaks = 20,
     xlim = c(0, 100), ylim = c(0, 100),
     main = "Aleatorización: Grupos Comparables",
     xlab = "Motivación (variable pre-tratamiento)",
     ylab = "Frecuencia")
hist(poblacion$motivacion[poblacion$tratamiento_aleatorio == 0],
     col = rgb(0, 0, 1, 0.5), breaks = 20,
     add = TRUE)
legend("topright", 
       legend = c("Tratados", "Controles"),
       fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)))

# Panel derecho: Observacional
hist(poblacion$motivacion[poblacion$tratamiento_sesgado == 1],
     col = rgb(1, 0, 0, 0.5), breaks = 20,
     xlim = c(0, 100), ylim = c(0, 100),
     main = "Sin Aleatorización: Grupos NO Comparables",
     xlab = "Motivación (variable pre-tratamiento)",
     ylab = "Frecuencia")
hist(poblacion$motivacion[poblacion$tratamiento_sesgado == 0],
     col = rgb(0, 0, 1, 0.5), breaks = 20,
     add = TRUE)
legend("topright", 
       legend = c("Tratados", "Controles"),
       fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)))

dev.off()

cat("Gráfico guardado: ejemplo_aleatorio_vs_observacional.png\n\n")

# ============================================================================
# RESUMEN Y LECCIONES
# ============================================================================

cat("\n============================================================\n")
cat("RESUMEN Y LECCIONES CLAVE\n")
cat("============================================================\n\n")

cat("1. ATE VERDADERO (desconocido en práctica):\n")
cat("   ", round(ATE_verdadero, 2), "puntos\n\n")

cat("2. CON ALEATORIZACIÓN:\n")
cat("   - Estimación:", round(ATE_estimado_alea, 2), "\n")
cat("   - Error:", round(abs(ATE_estimado_alea - ATE_verdadero), 2), 
    "(pequeño, solo por azar)\n")
cat("   - Los grupos son COMPARABLES\n\n")

cat("3. SIN ALEATORIZACIÓN:\n")
cat("   - Estimación:", round(ATE_estimado_sesgo, 2), "\n")
cat("   - Error:", round(abs(ATE_estimado_sesgo - ATE_verdadero), 2), 
    "(grande, sesgo de selección)\n")
cat("   - Los grupos NO son comparables\n\n")

cat("LECCIÓN PRINCIPAL:\n")
cat("La aleatorización GARANTIZA que los grupos son comparables\n")
cat("en TODAS las variables (observadas y no observadas).\n\n")

cat("PERO... ¿Qué hacemos cuando NO podemos aleatorizar?\n")
cat("→ Ahí es donde entran los métodos cuasi-experimentales:\n")
cat("  - Matching (hoy)\n")
cat("  - Difference-in-Differences (mañana)\n")
cat("  - Regression Discontinuity (siguiente clase)\n")
cat("  - Instrumental Variables (última clase)\n\n")

cat("Estos métodos intentan APROXIMAR el experimento aleatorio\n")
cat("usando supuestos más fuertes y datos observacionales.\n\n")

cat("============================================================\n\n")
