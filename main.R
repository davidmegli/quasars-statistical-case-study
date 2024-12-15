
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

