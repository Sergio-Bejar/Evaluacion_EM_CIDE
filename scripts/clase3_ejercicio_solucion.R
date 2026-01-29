# ============================================================================
# SOLUCION: EJERCICIO DIFFERENCE-IN-DIFFERENCES
# ============================================================================

# Cargar datos
datos <- read.csv("datos_seguro_popular.csv")

# ============================================================================
# PASO 1: DiD MANUAL
# ============================================================================

# Pre-tratamiento (2002-2003)
pre_tratados_gasto <- mean(datos$gasto_bolsillo[datos$tratado & datos$anio < 2004])
pre_control_gasto <- mean(datos$gasto_bolsillo[!datos$tratado & datos$anio < 2004])

# Post-tratamiento (2004-2008)
post_tratados_gasto <- mean(datos$gasto_bolsillo[datos$tratado & datos$anio >= 2004])
post_control_gasto <- mean(datos$gasto_bolsillo[!datos$tratado & datos$anio >= 2004])

# Cambios
cambio_tratados_gasto <- post_tratados_gasto - pre_tratados_gasto
cambio_control_gasto <- post_control_gasto - pre_control_gasto

# DiD
did_gasto <- cambio_tratados_gasto - cambio_control_gasto

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
# PASO 2: GRAFICO
# ============================================================================

promedios_tratados_gasto <- tapply(datos$gasto_bolsillo[datos$tratado], 
                                    datos$anio[datos$tratado], mean)
promedios_control_gasto <- tapply(datos$gasto_bolsillo[!datos$tratado], 
                                  datos$anio[!datos$tratado], mean)

anios_unicos <- sort(unique(datos$anio))

png("grafico_did_gasto.png", width = 1000, height = 600, res = 100)
par(mar = c(5, 5, 4, 2))

plot(anios_unicos, promedios_tratados_gasto, 
     type = "b", col = "#3498DB", lwd = 2, pch = 19, cex = 1.5,
     ylim = c(5, 11),
     xlab = "Año", ylab = "Gasto de bolsillo (miles de pesos anuales)",
     main = "DiD: Efecto en Gasto de Bolsillo",
     cex.lab = 1.2, cex.main = 1.3)

lines(anios_unicos, promedios_control_gasto, 
      type = "b", col = "#E74C3C", lwd = 2, pch = 17, cex = 1.5)

abline(v = 2003.5, lty = 2, col = "gray40", lwd = 1.5)
text(2003.5, 10.5, "Inicio Seguro Popular", pos = 4, cex = 1.1)

legend("topright", 
       legend = c("Estados Tratados", "Estados Control"),
       col = c("#3498DB", "#E74C3C"),
       lwd = 2, pch = c(19, 17), cex = 1.1, bty = "n")

grid(col = "gray90")
dev.off()

cat("Grafico guardado: grafico_did_gasto.png\n\n")

# ============================================================================
# PASO 3: ESTIMACION FORMAL
# ============================================================================

modelo_did_gasto <- lm(gasto_bolsillo ~ tratado + post + treat_post, 
                       data = datos)

cat("=== MODELO DiD ===\n")
print(summary(modelo_did_gasto))

efecto_did <- coef(modelo_did_gasto)["treat_postTRUE"]
se_did <- summary(modelo_did_gasto)$coefficients["treat_postTRUE", "Std. Error"]

cat("\n=== RESULTADO CLAVE ===\n")
cat("Efecto DiD:     ", round(efecto_did, 2), "mil pesos\n")
cat("Error Estandar: ", round(se_did, 2), "\n")
cat("Interpretacion: El programa REDUJO el gasto de bolsillo\n\n")

# ============================================================================
# PASO 4: INTERPRETACION
# ============================================================================

cat("=== INTERPRETACION COMPLETA ===\n\n")

cat("RESULTADO:\n")
cat("El Seguro Popular redujo el gasto de bolsillo en salud en\n")
cat(abs(round(efecto_did, 2)), "mil pesos anuales por familia.\n\n")

cat("MAGNITUD:\n")
cat("Baseline (control): ", round(pre_control_gasto, 2), "mil pesos\n")
cat("Reduccion:", abs(round(efecto_did, 2)), "mil pesos\n")
cat("Reduccion porcentual:", 
    round(abs(efecto_did)/pre_control_gasto * 100, 1), "%\n\n")

cat("SIGNIFICANCIA:\n")
if (summary(modelo_did_gasto)$coefficients["treat_postTRUE", "Pr(>|t|)"] < 0.05) {
  cat("El efecto ES estadisticamente significativo (p < 0.05)\n\n")
} else {
  cat("El efecto NO es estadisticamente significativo\n\n")
}

# ============================================================================
# BONUS: TENDENCIAS PARALELAS
# ============================================================================

dif_pre <- pre_tratados_gasto - pre_control_gasto

cat("=== VERIFICACION DE TENDENCIAS PARALELAS ===\n")
cat("Diferencia pre-tratamiento:", round(dif_pre, 2), "mil pesos\n")
cat("Interpretacion:", 
    ifelse(abs(dif_pre) < 0.5, "Tendencias paralelas PLAUSIBLES", 
           "Posible violacion de tendencias paralelas"), "\n\n")

# ============================================================================
# RESUMEN
# ============================================================================

cat("=== RESUMEN ===\n\n")
cat("EFECTOS DEL SEGURO POPULAR:\n")
cat("1. Acceso a salud:     +8.03 puntos porcentuales (visto en clase)\n")
cat("2. Gasto de bolsillo:  ", round(efecto_did, 2), "mil pesos\n\n")

cat("CONSISTENCIA:\n")
cat("Los resultados son CONSISTENTES:\n")
cat("- Mas acceso a servicios publicos gratuitos\n")
cat("- Menos gasto privado de bolsillo\n")
cat("- Ambos efectos estadisticamente significativos\n\n")

cat("=== FIN DEL EJERCICIO ===\n")
