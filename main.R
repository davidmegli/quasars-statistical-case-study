
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

