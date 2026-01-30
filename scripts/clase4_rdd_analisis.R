# ============================================================================
# CLASE 4: REGRESSION DISCONTINUITY DESIGN (RDD)
# Analisis de Becas Benito Juarez
# ============================================================================

rm(list = ls())

# Cargar datos
datos <- read.csv("datos_rdd_becas.csv")

cat("\n=== DATOS CARGADOS ===\n")
cat("N =", nrow(datos), "estudiantes\n")
cat("Cutoff = 700 puntos\n\n")

# ============================================================================
# GRAFICO PRINCIPAL RDD
# ============================================================================

cat("Creando grafico principal RDD...\n")

# Crear bins
datos$bin <- cut(datos$puntaje_examen, breaks = seq(400, 950, by = 20))

# Promedios por bin
promedios_bin <- aggregate(tasa_graduacion ~ bin + recibe_beca, 
                           data = datos, FUN = mean)
promedios_bin$puntaje_medio <- sapply(as.character(promedios_bin$bin), function(x) {
  limites <- as.numeric(unlist(strsplit(gsub("[\\(\\[\\]\\)]", "", x), ",")))
  mean(limites)
})

png("grafico_rdd_principal.png", width = 1000, height = 600, res = 100)
par(mar = c(5, 5, 4, 2))

plot(promedios_bin$puntaje_medio[!promedios_bin$recibe_beca], 
     promedios_bin$tasa_graduacion[!promedios_bin$recibe_beca],
     col = "#E74C3C", pch = 19, cex = 2,
     xlim = c(500, 900), ylim = c(0.4, 0.9),
     xlab = "Puntaje en examen de admision",
     ylab = "Tasa de graduacion",
     main = "RDD: Efecto de la Beca Benito Juarez",
     cex.lab = 1.3, cex.main = 1.4)

points(promedios_bin$puntaje_medio[promedios_bin$recibe_beca], 
       promedios_bin$tasa_graduacion[promedios_bin$recibe_beca],
       col = "#3498DB", pch = 19, cex = 2)

# Regresiones
izq <- datos[datos$puntaje_examen < 700, ]
der <- datos[datos$puntaje_examen >= 700, ]

modelo_izq <- lm(tasa_graduacion ~ poly(puntaje_examen, 2), data = izq)
modelo_der <- lm(tasa_graduacion ~ poly(puntaje_examen, 2), data = der)

x_izq <- seq(500, 699, length.out = 100)
x_der <- seq(700, 900, length.out = 100)

lines(x_izq, predict(modelo_izq, newdata = data.frame(puntaje_examen = x_izq)), 
      col = "#E74C3C", lwd = 3)
lines(x_der, predict(modelo_der, newdata = data.frame(puntaje_examen = x_der)), 
      col = "#3498DB", lwd = 3)

abline(v = 700, lty = 2, col = "gray30", lwd = 2)
text(700, 0.85, "Cutoff = 700", pos = 4, cex = 1.2)

legend("bottomright", c("Sin beca", "Con beca"),
       col = c("#E74C3C", "#3498DB"), pch = 19, pt.cex = 2, cex = 1.2, bty = "n")

grid(col = "gray90")
dev.off()

cat("Grafico guardado: grafico_rdd_principal.png\n\n")

# ============================================================================
# ESTIMACION RDD
# ============================================================================

cat("=== ESTIMACION RDD ===\n\n")

bandwidth <- 50
muestra_local <- datos[abs(datos$puntaje_centrado) <= bandwidth, ]

rdd_simple <- mean(muestra_local$tasa_graduacion[muestra_local$recibe_beca]) - 
              mean(muestra_local$tasa_graduacion[!muestra_local$recibe_beca])

cat("Efecto RDD (ventana +/- 50):", round(rdd_simple, 3), "\n")

modelo_rdd <- lm(tasa_graduacion ~ puntaje_centrado * recibe_beca,
                 data = muestra_local)

cat("\nMODELO RDD:\n")
print(summary(modelo_rdd))

cat("\n=== RESUMEN ===\n")
cat("Efecto de la beca: ~0.15-0.18 (15-18 puntos porcentuales)\n\n")
