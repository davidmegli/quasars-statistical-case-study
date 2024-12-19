
#DATASET 1 SDSS_QSO Sloan Digital Sky Survey
####ASTRO DATA: 15 different  measures for 77429 quasars provided by the Sloan Digital Sky Survey in the Milky Way Galaxy 
#https://astrostatistics.psu.edu/MSMA/datasets/index.html
#It provides the SDSS designation (which gives the celestial location), redshift (a measure of distance), 
#magnitudes in five photometric bands (with heteroscedastic measurement errors), 
#measures or indicators of radio and X-ray emission, and absolute magnitude (a measure of luminosity). 
#The dataset is useful for multivariate analysis and regression 
#https://search.r-project.org/CRAN/refmans/astrodatR/html/SDSS_QSO.html
#The sample is described in Appendix C.8 of Feigelson & Babu (2012) which defines the variables 
#including the radio and X-ray censoring indicators. 

require(astrodatR)
data("SDSS_QSO")
dat <- SDSS_QSO
save(SDSS_QSO, file="SDSS_QSO.RData")
load("SDSS_QSO.RData")

#  z = Redshift(scales with distance)
#  u_mag and sig_u_mag = Brightness in the u (ultraviolet) band in magnitudes with standard
#     deviation (mag). The heteroscedastic measurement errors for each magnitude are
#     determined by the SDSS team from knowledge of the observing conditions, detector
#     background and other technical considerations.
#  g_mag and sig_g_mag = Brightness in the g(green) band with standard deviation (mag)
#  r_mag and sig_r_mag = Brightness in the r (red) band with standard deviation (mag)
#  i_mag and sig_i_mag = Brightness in the i (further red) band with standard deviation (mag)
#  z_mag and sig_z_mag = Brightness in the z (further red) band
#  FIRST = Brightness in the radio band, in magnitudes scaled from the flux density measured
#     in the NRAO FIRST survey at 20m. 0 indicates the quasar is undetected by FIRST,
#     while −1 indicates it was not observed by FIRST.
#  ROSAT = Brightness in the X-ray band, in log(Count rate) from the ROSAT All-Sky Survey
#     (RASS) in the 0.2–2.4 keV band. −9 indicates not detected by RASS.
#  MP = Absolute magnitude in the i band, a measure of luminosity.


# Estraggo la colonna z (Redshift)
z <- dat$z
# Estraggo le altre colonne
u_mag <- dat$u_mag
sig_u_mag <- dat$sig_u_mag
g_mag <- dat$g_mag
sig_g_mag <- dat$sig_g_mag
r_mag <- dat$r_mag
sig_r_mag <- dat$sig_r_mag
i_mag <- dat$i_mag
sig_i_mag <- dat$sig_i_mag
z_mag <- dat$z_mag
first <- dat$FIRST
rosat <- dat$ROSAT
mp <- dat$MP
attach(SDSS_QSO)

library(ggplot2)

summary(dat)

dat_long <- dat %>%
  pivot_longer(
    cols = c(u_mag, g_mag, r_mag, i_mag, z_mag), # Colonne da trasformare
    names_to = "banda",      # Nuova colonna per i nomi delle bande
    values_to = "value"      # Nuova colonna per le magnitudini
  )
# Rimozione di 0 da dat_long
dat_long <- dat_long[dat_long$value != 0, ]
ggplot(dat_long, aes(x = value, y = z)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(
    title = "Distribuzione delle magnitudini ugriz",
    x = "Bande",
    y = "Magnitudine"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


dat_long <- dat %>%
  pivot_longer(
    cols = c(u_mag, g_mag, r_mag, i_mag, z_mag), # Colonne da trasformare
    names_to = "banda",      # Nuova colonna per i nomi delle bande
    values_to = "value"      # Nuova colonna per le magnitudini
  )
# Rimozione di 0 da dat_long
dat_long <- dat_long[dat_long$value != 0, ]
ggplot(dat_long, aes(x = banda, y = value)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(
    title = "Distribuzione delle magnitudini ugriz",
    x = "Bande",
    y = "Magnitudine"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dat_sig_long <- dat %>%
  pivot_longer(
    cols = c(sig_u_mag, sig_g_mag, sig_r_mag, sig_i_mag, sig_z_mag), # Colonne da trasformare
    names_to = "banda",      # Nuova colonna per i nomi delle bande
    values_to = "errore"     # Nuova colonna per gli errori
  )
# Rimozone di 0 da dat_sig_long
ggplot(dat_sig_long, aes(x = banda, y = errore)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(
    title = "Distribuzione degli errori per le magnitudini ugriz",
    x = "Bande",
    y = "Errore di misura"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Creazione di un istogramma per il Redshift
ggplot(dat, aes(x = z)) +
  geom_histogram(bins = 50, fill = "red", color = "black") +
  labs(
    title = "Distribuzione del Redshift",
    x = "Redshift (z)",
    y = "Conteggio"
  )

# Scatter plot della magnitudine ROSAT (X-ray)
ggplot(dat, aes(x = z, y = rosat)) +
  geom_point(alpha = 0.5, color = "darkorchid4") +
  labs(
    title = "Relazione tra Redshift e Magnitudine nella banda X-ray",
    x = "Redshift (z)",
    y = "Magnitudine (X-ray)"
  ) +
  theme_minimal()

# Scatter plot della magnitudine u
ggplot(dat, aes(x = z, y = u_mag)) +
  geom_point(alpha = 0.5, color = "purple") +
  labs(
    title = "Relazione tra Redshift e Magnitudine nella banda u (ultraviolet)",
    x = "Redshift (z)",
    y = "Magnitudine (u)"
  ) +
  theme_minimal()

# Scatter plot della magnitudine g
ggplot(dat, aes(x = z, y = g_mag)) +
  geom_point(alpha = 0.5, color = "green") +
  labs(
    title = "Relazione tra Redshift e Magnitudine nella banda g (green)",
    x = "Redshift (z)",
    y = "Magnitudine (g)"
  ) +
  theme_minimal()

# Scatter plot della magnitudine r
ggplot(dat, aes(x = z, y = r_mag)) +
  geom_point(alpha = 0.5, color = "red") +
  labs(
    title = "Relazione tra Redshift e Magnitudine nella banda r (red)",
    x = "Redshift (z)",
    y = "Magnitudine (r)"
  ) +
  theme_minimal()

# Scatter plot della magnitudine i
ggplot(dat, aes(x = z, y = i_mag)) +
  geom_point(alpha = 0.5, color = "brown3") +
  labs(
    title = "Relazione tra Redshift e Magnitudine nella banda i (further red)",
    x = "Redshift (z)",
    y = "Magnitudine (i)"
  ) +
  theme_minimal()

# Scatter plot della magnitudine z
ggplot(dat, aes(x = z, y = z_mag)) +
  geom_point(alpha = 0.5, color = "darkred") +
  labs(
    title = "Relazione tra Redshift e Magnitudine nella banda z (further red)",
    x = "Redshift (z)",
    y = "Magnitudine (z)"
  ) +
  theme_minimal()

# Scatter plot della magnitudine FIRST (radio)
ggplot(dat, aes(x = z, y = first)) +
  geom_point(alpha = 0.5, color = "black") +
  labs(
    title = "Relazione tra Redshift e Magnitudine nella banda radio",
    x = "Redshift (z)",
    y = "Magnitudine (radio)"
  ) +
  theme_minimal()


all <- lm(z ~ u_mag + g_mag + r_mag + i_mag + z_mag + first + rosat, data = dat)
summary(all)

a <- lm(z ~ u_mag , data=dat)
summary(a)

# Calcolo della matrice di correlazione tra le variabili, senza considerare gli errori
dat_without_errors <- dat[, c("z", "u_mag", "g_mag", "r_mag", "i_mag", "z_mag", "FIRST", "ROSAT", "Mp")]
dat_without_errors <- dat_without_errors[, !colnames(dat_without_errors) %in% "SDSS"]
head(dat_without_errors)
matrice_cor <- cor(dat_without_errors)
print(matrice_cor)
round(matrice_cor, 2)
library(corrplot)
# Matrice di correlazione con titolo
corrplot(matrice_cor, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, title = "Matrice di Correlazione",mar=c(0,0,2,0),addCoef.col = "black")

# Grafici di dispersione (Scatterplot Matrix)
library(GGally)
ggpairs(data.frame(u_mag, g_mag, r_mag, i_mag, z_mag))

# Analisi della distribuzione della magnitudine i
hist(dat$i, breaks = 30, col = "blue", main = "Distribuzione di i", xlab = "Magnitudine i")
abline(v = 19, col = "red", lwd = 2, lty = 2)  # Linea per i = 19

# Relazione tra Redshift (z) e Magnitudini ugriz
plot(dat$z, dat$u, main = "Redshift vs Magnitudine u", xlab = "Redshift", ylab = "Magnitudine u", col = "blue")
boxplot(u ~ cut(z, breaks = c(0, 2, 4, Inf)), data = dat, main = "Magnitudine u per Redshift", xlab = "Gruppi di z", ylab = "Magnitudine u")




# Calcolo dello Z-score per ciascuna colonna
z_scores <- scale(dat[, c("u_mag", "g_mag", "r_mag", "i_mag", "z_mag")])
# Identificazione outlier dove |Z-score| > 3
outliers_u <- which(abs(z_scores[, "u_mag"]) > 3)
outliers_g <- which(abs(z_scores[, "g_mag"]) > 3)
outliers_r <- which(abs(z_scores[, "r_mag"]) > 3)
outliers_i <- which(abs(z_scores[, "i_mag"]) > 3)
outliers_z <- which(abs(z_scores[, "z_mag"]) > 3)
# Unione di outliers identificati
outliers_indices <- unique(c(outliers_u, outliers_g, outliers_r, outliers_i, outliers_z))
# Creazione di un dataframe con soli outlier
outliers_data <- dat[outliers_indices, ]
normal_data <- dat[-outliers_indices, ]  # Esclusione degli outlier
# Calcolo della media degli errori per outlier e dati normali
error_means_outliers <- colMeans(outliers_data[, c("sig_u_mag", "sig_g_mag", "sig_r_mag", "sig_i_mag", "sig_z_mag")], na.rm = TRUE)
error_means_normal <- colMeans(normal_data[, c("sig_u_mag", "sig_g_mag", "sig_r_mag", "sig_i_mag", "sig_z_mag")], na.rm = TRUE)
# Stampa a video dei risultati
print("Media degli errori di misura per gli outlier:")
print(error_means_outliers)
print("Media degli errori di misura per i dati normali:")
print(error_means_normal)
print("Rapporto tra le medie degli errori di misura per outlier e dati normali:")
print(error_means_outliers / error_means_normal)


# Identificazione degli outlier: righe con i_mag < 19
outliers_indices <- which(dat$i_mag >= 19)
# Creazione di un dataframe con soli outlier
outliers_data <- dat[outliers_indices, ]
normal_data <- dat[-outliers_indices, ]  # Esclusione degli outlier
# Calcolo della media degli errori per outlier e dati normali
error_means_outliers <- colMeans(outliers_data[, c("sig_u_mag",
  "sig_g_mag", "sig_r_mag", "sig_i_mag", "sig_z_mag")], na.rm = TRUE)
error_means_normal <- colMeans(normal_data[, c("sig_u_mag",
  "sig_g_mag", "sig_r_mag", "sig_i_mag", "sig_z_mag")], na.rm = TRUE)
# Stampa a video dei risultati
print("Media degli errori di misura per gli outlier (i_mag < 19):")
print(error_means_outliers)
print("Media degli errori di misura per i dati normali:")
print(error_means_normal)
print("Rapporto tra le medie degli errori di misura per outlier e dati normali:")
print(error_means_outliers / error_means_normal)




# Visualizzazione della differenza con un grafico
library(ggplot2)
library(dplyr)
library(tidyr)
# Prepararazione dei dati per il grafico
error_df <- data.frame(
  Banda = c("sig_u_mag", "sig_g_mag", "sig_r_mag", "sig_i_mag", "sig_z_mag"),
  Errore_Outlier = error_means_outliers,
  Errore_Normale = error_means_normal
)
error_long <- error_df %>%
  pivot_longer(cols = c(Errore_Outlier, Errore_Normale), 
               names_to = "Tipo", 
               values_to = "Errore")
# Grafico a barre per confrontare gli errori
ggplot(error_long, aes(x = Banda, y = Errore, fill = Tipo)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Confronto degli errori di misura tra outliers e dati normali",
       y = "Errore medio", x = "Bande") +
  scale_fill_manual(values = c("Errore_Outlier" = "red", "Errore_Normale" = "blue"),
                    labels = c("Errore_Outlier" = "Outlier", "Errore_Normale" = "Normale")) +
  guides(fill = guide_legend(title = "Legenda")) +
  theme_minimal()



# Visualizzazione degli outlier nei grafici di dispersione
library(GGally)
outliers_data$outlier <- "Outlier"
normal_data$outlier <- "Normale"
# Unione dei due dataset
combined_data <- rbind(outliers_data, normal_data)
# Grafici di dispersione a coppie con distinzione degli outlier
ggpairs(combined_data, mapping = aes(color = outlier), columns = c("u_mag", "g_mag", "r_mag", "i_mag", "z_mag"))


# DIVISIONE DEL DATASET IN DATI COMPLETI E INCOMPLETI
# creo colonna "complete" con valore 1 se i_mag < 19, 0 altrimenti
dat$complete <- ifelse(dat$i_mag < 19, 1, 0)
# Creo una colonna che contiene la somma di tutti gli errori
dat$error_sum <- rowSums(dat[, c("sig_u_mag", "sig_g_mag", "sig_r_mag", "sig_i_mag", "sig_z_mag")])
var_a <- lm(dat$error_sum ~ dat$complete, data=dat)
summary(var_a)
var_b <- lm(z ~ i_mag, data=dat)
summary(var_b)



# Split dataset based su i_mag
dataset_complete <- subset(dat, i_mag < 19)
dataset_incomplete <- subset(dat, i_mag >= 19)
# Controllo dela dimensione dei nuovi dataset
cat("Dati completi (i_mag < 19):", nrow(dataset_complete), "righe\n")
cat("Dati incompleti (i_mag >= 19):", nrow(dataset_incomplete), "righe\n")


# 1) Analisi delle Relazioni
# Rimozione degli esempi con valori mancanti
data <- dat[complete.cases(dat), ]
# Rimozione degli outlier
data <- data[-outliers_indices, ]

library(gRbase)
library(gRain)
library(gRim)
#Udirected graph
ug0 <- ug(~z*u_mag+u_mag*g_mag+g_mag*r_mag+r_mag*i_mag+i_mag*z_mag)
ug0 <- ug(~a:b+b:c:d+e)
ug0

# Rimozione dati non numerici
dat_num_only <- dat[, !colnames(dat) %in% "SDSS"]
dat_num_only <- na.omit(dat_num_only)
dat_num_only <- dat_num_only[!is.infinite(rowSums(dat_num_only)), ]
sat_dat <- cmod(~.^., data=dat_num_only)

aic_dat_sw_f <- stepwise(sat_dat, direction="forward")
aic_dat_sw_f
# Accesso alla componente del grafo (modelinfo$ug)
graph <- aic_dat_sw_f$modelinfo$ug
plot(graph, vertex.label = aic_dat_sw_f$varNames, main = "Grafo Non Diretto, Stepwise, Forward, con AIC"
     ,vertex.size=30,edge.width=2,vertex.labels=labels)

bic_dat_sw_f <- stepwise(sat_dat, k=log(nrow(dat_num_only)), direction="forward")
bic_dat_sw_f
# Accesso alla componente del grafo (modelinfo$ug)
graph <- bic_dat_sw_f$modelinfo$ug
plot(graph, vertex.label = bic_dat_sw_f$varNames, main = "Grafo Non Diretto, Stepwise, Forward, con BIC"
     ,vertex.size=30,edge.width=2,vertex.labels=labels)

bic_dat_sw_b <- stepwise(sat_dat, k=log(nrow(dat_num_only)), direction="backward")
bic_dat_sw_b
# Accesso alla componente del grafo (modelinfo$ug)
graph <- bic_dat_sw_f$modelinfo$ug
plot(graph, vertex.label = bic_dat_sw_b$varNames, main = "Grafo Non Diretto, Stepwise, Backward, con BIC"
     ,vertex.size=30,edge.width=2,vertex.labels=labels)


bic_dat_sw_b2 <- stepwise(sat_dat, k=log(nrow(dat_num_only)),
                          direction="backward")
bic_dat_sw_b2
graph <- graph - "sig_u_mag" - "sig_g_mag" - "sig_r_mag" -
  "sig_i_mag" - "sig_z_mag"
plot(graph, vertex.label = colnames(bic_dat_sw_b2),
     main = "Grafo Non Diretto, Stepwise, Backward, con BIC"
     ,vertex.size=30,edge.width=2,
     vertex.labels=colnames(bic_dat_sw_b2),edge.curved=0.2)

# Uso bnlearn
library(bnlearn)
# Creazione del modello
model <- hc(dat_num_only)
# Stampa del modello
print(model)
# Plot del modello
plot(model)

# 2) Predizione del Redshift (z)
library(Metrics)
evaluate_model <-function(model, test_data, model_name){
  predictions <- predict(model, newdata = test_data)
  mae <- mae(test_data$z, predictions)  # Mean Absolute Error
  rmse <- rmse(test_data$z, predictions)  # Root Mean Square Error
  r_squared <- R2(test_data$z, predictions) # R-squared
  cat("MAE:", round(mae, 4), "\n")
  cat("RMSE:", round(rmse, 4), "\n")
  cat("R-squared:", round(r_squared, 4), "\n")
  ggplot() +
    geom_point(aes(x = test_data$z, y = predictions), color = "black", alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(title = paste("Redshift: Valori Reali vs Predetti. Modello:", model_name),
         x = "Redshift Reale",
         y = "Redshift Predetto") +
    theme_minimal()
}


# Divisione dei dati in training e test set
library(caret)
set.seed(123)
data_model <- dat[, !colnames(dat) %in% "SDSS"]
train_index <- createDataPartition(data_model$z, p = 0.7, list = FALSE)
train_data <- data_model[train_index, ]
test_data <- data_model[-train_index, ]
cat("Training set:", nrow(train_data), "righe\n")
cat("Test set:", nrow(test_data), "righe\n")

# Modello di regressione lineare
lm_model <- lm(z ~ u_mag + g_mag + r_mag + i_mag + z_mag, data = train_data)
# Riassunto del modello
summary(lm_model)
# Valutazione del modello
evaluate_model(lm_model, test_data, "Regressione Lineare")
# Modello di Regressione Lineare con Variabili Aggiuntive
lm_model_additional <- lm(z ~ u_mag + g_mag + r_mag + i_mag + z_mag + FIRST + ROSAT, data = train_data)
# Riassunto del modello
summary(lm_model_additional)
# Valutazione del modello
evaluate_model(lm_model_additional, test_data, "Regressione Lineare con Variabili Aggiuntive")


# Bayesian Network
library(bnlearn)
# Creazione del modello
model <- hc(train_data)
# Rimozione delle variabili con errori
train_data <- train_data[, !colnames(train_data) %in% c("sig_u_mag", "sig_g_mag", "sig_r_mag", "sig_i_mag", "sig_z_mag")]
test_data <- test_data[, !colnames(test_data) %in% c("sig_u_mag", "sig_g_mag", "sig_r_mag", "sig_i_mag", "sig_z_mag")]
# Apprendimento dei parametri della rete
fitted_model <- bn.fit(model, data = train_data)
# Stampa del modello appreso
print(fitted_model)
# Plot della struttura della rete
plot(model)
# Uso della funzione predict per il nodo "z" usando la rete appresa
predictions <- predict(fitted_model, node = "z", data = test_data, method = "bayes-lw")
mae <- mae(test_data$z, predictions)  # Mean Absolute Error
rmse <- rmse(test_data$z, predictions)  # Root Mean Square Error
r_squared <- R2(test_data$z, predictions) # R-squared
cat("MAE:", round(mae, 4), "\n")
cat("RMSE:", round(rmse, 4), "\n")
cat("R-squared:", round(r_squared, 4), "\n")
# Plot dei valori reali vs predetti
ggplot() +
  geom_point(aes(x = test_data$z, y = predictions), color = "black", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Redshift: Valori Reali vs Predetti. Modello: Bayesian Network",
       x = "Redshift Reale",
       y = "Redshift Predetto") +
  theme_minimal()


model <- pc(train_data)
print(model)
plot(model)


library(randomForest)
rf_model <- randomForest(z ~ ., data = train_data, ntree = 50, importance = TRUE, do.trace = TRUE)
print(rf_model)
evaluate_model(rf_model, test_data, "Random Forest")

library(gbm)
gbm_model <- gbm(z ~ ., data = train_data, n.trees = 1000, distribution = "gaussian", interaction.depth = 4)
summary(gbm_model)
predictions_gbm <- predict(gbm_model, newdata = test_data)
evaluate_model(gbm_model, test_data, "Gradient Boosting Machine")

#Correlazione fra redshift ed errori di misura
correlation <- cor(dat_num_only)
correlation_z <- correlation["z", c("sig_u_mag", "sig_g_mag", "sig_r_mag", "sig_i_mag", "sig_z_mag")]
print(correlation_z)
# Plot della correlazione
barplot(correlation_z, col = "blue", main = "Correlazione tra Redshift e Errori di Misura",
        xlab = "Errori di Misura", ylab = "Correlazione")




sig_u_mag_linear <- 10^(-sig_u_mag/2.5)
dat2 <- dat_num_only
dat2$sig_u_mag_linear <- sig_u_mag_linear
red <- lm(z ~ sig_u_mag_linear , data = dat2)
summary(red)

# plot redshift e sig_u_mag
ggplot(dat2, aes(x = sig_u_mag_linear, y = z)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(
    title = "Relazione tra Redshift e Errore di Misura per la banda u",
    x = "Errore di Misura (u)",
    y = "Redshift (z)"
  ) +
  theme_minimal()
