# load libraries
library(dplyr)
library(readxl)
library(car)
library(sandwich)
library(lmtest)
library(stargazer)

# set working dictory depending on the user
user <- "CP"

if (user == "CP") {
  setwd("C:/Users/crist/OneDrive/Documentos/GitHub/Performance-Chilean-Pension-Funds")
} else if (user == "JL") {
  setwd("C:/Users/JL/Documents/Proyectos/2019/2019-05-01 - Retorno Fondos Brutos")
} else if (user == "AP") {
  setwd("C:/Users/JG/Documents/Proyectos/2019/2019-05-01 - Retorno Fondos Brutos")
}

# load data
df_retornos_fondos_brutos <- read_excel("2-raw/retorno-mensuales-fondos-brutos.xlsx")
df_retornos_indices <- read_excel("2-raw/retorno-mensual-indices.xlsx")

# create df with returns
df_retornos <- left_join(df_retornos_fondos_brutos, df_retornos_indices, by = "Fecha")


###################################$$######################
############### --- MODELO ESTATICO --- ###################
###########################################################

#########################
##- model for fondo A -##
#########################

# model estimation
modelo_fondo_a <- lm(fondo_a ~ RVN + RVE + RFN + RFE, data = df_retornos)

# test to check heteroscedasticity
hete_modelo_fondo_a <- bptest(modelo_fondo_a, ~ RVN + RVE + RFN + RFE, data = df_retornos)

# check how many lags we need to include in the test for autocorrelation


# test to check autocorrelation
bgtest(modelo_fondo_a, order = 9)

# resume of model
stargazer(modelo_fondo_a, type = "text", se = list(NeweyWest(modelo_fondo_a)))


#########################
##- model for fondo B -##
#########################

# model estimation
modelo_fondo_b <- lm(fondo_b ~ RVN + RVE + RFN + RFE, data = df_retornos)

# test to check heteroscedasticity
hete_modelo_fondo_b <- bptest(modelo_fondo_b, ~ RVN + RVE + RFN + RFE, data = df_retornos)

# check how many lags we need to include in the test for autocorrelation


# test to check autocorrelation
bgtest(modelo_fondo_b, order = 9)

# resume of model
