# ============================================================================
# EJERCICIO PRACTICO: DIFFERENCE-IN-DIFFERENCES
# Tiempo estimado: 20 minutos
# ============================================================================

# CONTEXTO:
# Ya vimos el efecto del Seguro Popular en ACCESO a salud.
# Ahora vamos a analizar el efecto en GASTO DE BOLSILLO.

# Hipotesis: El Seguro Popular deberia REDUCIR el gasto de bolsillo
# porque las familias ya no tienen que pagar por servicios basicos.

# ============================================================================
# TU TAREA:
# ============================================================================
# 1. Calcular DiD manualmente (tabla 2x2)
# 2. Crear grafico de tendencias
# 3. Estimar modelo DiD en R
# 4. Interpretar el efecto
# 5. BONUS: Verificar tendencias paralelas

# ============================================================================
# CODIGO INICIAL (COMPLETA LOS ESPACIOS)
# ============================================================================

# Cargar datos
datos <- read.csv("datos_seguro_popular.csv")

# Explorar la variable de outcome
cat("Variable: gasto_bolsillo (miles de pesos anuales)\n")
summary(datos$gasto_bolsillo)

# ============================================================================
# PASO 1: CALCULAR DiD MANUALMENTE
# ============================================================================

# Pre-tratamiento (2002-2003)
pre_tratados_gasto <- # TU CODIGO AQUI
pre_control_gasto <- # TU CODIGO AQUI

# Post-tratamiento (2004-2008)  
post_tratados_gasto <- # TU CODIGO AQUI
post_control_gasto <- # TU CODIGO AQUI

# Calcular cambios
cambio_tratados_gasto <- # TU CODIGO AQUI
cambio_control_gasto <- # TU CODIGO AQUI

# DiD manual
did_gasto <- # TU CODIGO AQUI

cat("\n=== TABLA RESUMEN DiD ===\n")
cat("                       Pre        Post      Cambio\n")
cat("--------------------------------------------------\n")
cat(sprintf("Tratados           %7.2f    %7.2f    %7.2f\n", 
            pre_tratados_gasto, post_tratados_gasto, cambio_tratados_gasto))
cat(sprintf("Control            %7.2f    %7.2f    %7.2f\n",
            pre_control_gasto, post_control_gasto, cambio_control_gasto))
cat("--------------------------------------------------\n")
cat(sprintf("DiD                                       %7.2f\n", did_gasto))
cat("\n")

# ============================================================================
# PASO 2: CREAR GRAFICO DE TENDENCIAS
# ============================================================================

# Calcular promedios por grupo y anio
promedios_tratados_gasto <- tapply(datos$gasto_bolsillo[datos$tratado], 
                                    datos$anio[datos$tratado], mean)
promedios_control_gasto <- tapply(datos$gasto_bolsillo[!datos$tratado], 
                                  datos$anio[!datos$tratado], mean)

anios_unicos <- sort(unique(datos$anio))

# Crear grafico
# TU CODIGO AQUI
# Pistas:
# - Usa plot() con type = "b"
# - Pon linea vertical en 2003.5
# - Añade leyenda
# - Guarda como PNG

# ============================================================================
# PASO 3: ESTIMACION FORMAL
# ============================================================================

# Modelo DiD basico
modelo_did_gasto <- # TU CODIGO AQUI
# Pista: lm(gasto_bolsillo ~ tratado + post + treat_post, data = datos)

# Ver resumen
summary(modelo_did_gasto)

# Extraer coeficiente clave
efecto_did <- coef(modelo_did_gasto)["treat_postTRUE"]

cat("\n=== RESULTADO CLAVE ===\n")
cat("Efecto DiD en gasto de bolsillo:", round(efecto_did, 2), "mil pesos\n")

# ============================================================================
# PASO 4: INTERPRETACION
# ============================================================================

cat("\n=== INTERPRETACION ===\n")
cat("El coeficiente", round(efecto_did, 2), "significa que...\n")
# TU INTERPRETACION AQUI
# ¿El Seguro Popular redujo el gasto de bolsillo?
# ¿En cuanto?
# ¿Es estadisticamente significativo?

# ============================================================================
# BONUS: VERIFICAR TENDENCIAS PARALELAS
# ============================================================================

# Calcular diferencia pre-tratamiento
dif_pre <- pre_tratados_gasto - pre_control_gasto

cat("\n=== VERIFICACION DE TENDENCIAS PARALELAS ===\n")
cat("Diferencia pre-tratamiento:", round(dif_pre, 2), "\n")
cat("Interpretacion: Si es cercano a 0, tendencias paralelas son plausibles\n")

# ============================================================================
# PREGUNTAS PARA DISCUTIR
# ============================================================================

# 1. ¿El efecto en gasto de bolsillo es consistente con el efecto
#    en acceso a salud que vimos antes?

# 2. ¿Que magnitud tiene el efecto? ¿Es sustancialmente importante?

# 3. ¿Que supuestos necesitamos para interpretar esto causalmente?

# 4. ¿Que amenazas podrian invalidar el diseño?
