# Limpeza de variaveis antigas
rm(list = ls())

# biblioteca utilizadas
library(gmm)
library(readxl)
library(dplyr)

EW.dados <- read_excel("dataset/dadosHansenSingleton.xlsx", sheet = "EW.data")
VW.dados <- read_excel("dataset/dadosHansenSingleton.xlsx", sheet = "VW.data")

EW.dados$Const = 1
VW.dados$Const = 1

EW.dados = mutate(EW.dados, "Ct_1" = lag(Ct, 1), "Rt_1" = lag(Rt, 1))
EW.dados = mutate(EW.dados, "Ct_2" = lag(Ct, 2), "Rt_2" = lag(Rt, 2))
EW.dados = mutate(EW.dados, "Ct_3" = lag(Ct, 3), "Rt_3" = lag(Rt, 3))
EW.dados = mutate(EW.dados, "Ct_4" = lag(Ct, 4), "Rt_4" = lag(Rt, 4))
EW.dados = mutate(EW.dados, "Ct_5" = lag(Ct, 5), "Rt_5" = lag(Rt, 5))
EW.dados = mutate(EW.dados, "Ct_6" = lag(Ct, 6), "Rt_6" = lag(Rt, 6))

VW.dados = mutate(VW.dados, "Ct_1" = lag(Ct, 1), "Rt_1" = lag(Rt, 1))
VW.dados = mutate(VW.dados, "Ct_2" = lag(Ct, 2), "Rt_2" = lag(Rt, 2))
VW.dados = mutate(VW.dados, "Ct_3" = lag(Ct, 3), "Rt_3" = lag(Rt, 3))
VW.dados = mutate(VW.dados, "Ct_4" = lag(Ct, 4), "Rt_4" = lag(Rt, 4))
VW.dados = mutate(VW.dados, "Ct_5" = lag(Ct, 5), "Rt_5" = lag(Rt, 5))
VW.dados = mutate(VW.dados, "Ct_6" = lag(Ct, 6), "Rt_6" = lag(Rt, 6))

head(EW.dados[-c(1:6),])
head(VW.dados)

################    Fim tratamento de bases   ##############

###### Contrucao do GMM ##########
# A function of the form g(θ,x) and which returns a n \times q matrix with typical element g_i(θ,x_t)
# for i=1,...q and t=1,...,n.
# This matrix is then used to build the q sample moment conditions.
GMatrix = function(param, database){
  beta  = param[1]
  alpha = param[2]

  C = database$Ct
  R = database$Rt
  
  Z = database[,3:ncol(database)]

  g = (beta*(C^(alpha))*R-1)*(as.matrix(Z))

  return(g)
}

######## Tabela I ##################

 ########### Base EWR  ##############
# Estimacao com um LAG 
EWR1 = gmm(GMatrix, x = EW.dados[-c(1:6),1:5] , t0=c(beta = 0, alpha = -1) )

EWR2 = gmm(GMatrix, x = EW.dados[-c(1:6),1:7] , t0=c(beta = 0, alpha = -1)	)

EWR4 = gmm(GMatrix, x = EW.dados[-c(1:6),1:11], t0=c(beta = 0, alpha = -1) )

EWR6 = gmm(GMatrix, x = EW.dados[-c(1:6),1:15], t0=c(beta = 0, alpha = -1) )

 ########### Base VWR  ##############
# Estimacao com um LAG
VWR1 = gmm(GMatrix, x = VW.dados[-c(1:6),1:5] , t0=c(beta = 0, alpha = -1) )

VWR2 = gmm(GMatrix, x = VW.dados[-c(1:6),1:7] , t0=c(beta = 0, alpha = -1) )

VWR4 = gmm(GMatrix, x = VW.dados[-c(1:6),1:11] , t0=c(beta = 0, alpha = -1) )

VWR6 = gmm(GMatrix, x = VW.dados[-c(1:6),1:15] , t0=c(beta = 0, alpha = -1) )


# Resultados #
rbind(
EWR1=EWR1$coefficients,
EWR2=EWR2$coefficients,
VWR1=VWR1$coefficients,
VWR2=VWR2$coefficients,
EWRVWR1=EWRVWR1$coefficients,
EWRVWR2=EWRVWR2$coefficients)

result = tibble(Return = c(rep("EWR", 4), rep("VWR", 4)))
result[1 ,"NLAG"] = 1
result[1 ,"alpha"] = EWR1$coefficients["alpha"]
result[1 ,"alpha.se"] = EWR1$vcov[2,2]^0.5
result[1 ,"beta"] = EWR1$coefficients["beta"]
result[1 ,"beta.se"] = EWR1$vcov[1,1]^0.5

result[2 ,"NLAG"] = 2
result[2 ,"alpha"] = EWR2$coefficients["alpha"]
result[2 ,"alpha.se"] = EWR2$vcov[2,2]^0.5
result[2 ,"beta"] = EWR2$coefficients["beta"]
result[2 ,"beta.se"] = EWR2$vcov[1,1]^0.5

result[3 ,"NLAG"] = 4
result[3 ,"alpha"] = EWR4$coefficients["alpha"]
result[3 ,"alpha.se"] = EWR4$vcov[2,2]^0.5
result[3 ,"beta"] = EWR4$coefficients["beta"]
result[3 ,"beta.se"] = EWR4$vcov[1,1]^0.5

result[4 ,"NLAG"] = 1
result[4 ,"alpha"] = EWR6$coefficients["alpha"]
result[4 ,"alpha.se"] = EWR6$vcov[2,2]^0.5
result[4 ,"beta"] = EWR6$coefficients["beta"]
result[4 ,"beta.se"] = EWR6$vcov[1,1]^0.5


result[5 ,"NLAG"] = 1
result[5 ,"alpha"] = VWR1$coefficients["alpha"]
result[5 ,"alpha.se"] = VWR1$vcov[2,2]^0.5
result[5 ,"beta"] = VWR1$coefficients["beta"]
result[5 ,"beta.se"] = VWR1$vcov[1,1]^0.5

result[6 ,"NLAG"] = 2
result[6 ,"alpha"] = VWR2$coefficients["alpha"]
result[6 ,"alpha.se"] = VWR2$vcov[2,2]^0.5
result[6 ,"beta"] = VWR2$coefficients["beta"]
result[6 ,"beta.se"] = VWR2$vcov[1,1]^0.5

result[7 ,"NLAG"] = 4
result[7 ,"alpha"] = VWR4$coefficients["alpha"]
result[7 ,"alpha.se"] = VWR4$vcov[2,2]^0.5
result[7 ,"beta"] = VWR4$coefficients["beta"]
result[7 ,"beta.se"] = VWR4$vcov[1,1]^0.5

result[8 ,"NLAG"] = 6
result[8 ,"alpha"] = VWR6$coefficients["alpha"]
result[8 ,"alpha.se"] = VWR6$vcov[2,2]^0.5
result[8 ,"beta"] = VWR6$coefficients["beta"]
result[8 ,"beta.se"] = VWR6$vcov[1,1]^0.5

