# Limpeza de variaveis antigas
rm(list = ls())

# biblioteca utilizadas
library(gmm)
library(readxl)
library(dplyr)

EW.dados <- read_excel("dataset/dadosHansenSingleton.xlsx", sheet = "EW.data")
VW.dados <- read_excel("dataset/dadosHansenSingleton.xlsx", sheet = "VW.data")

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

EW.dados$Const = 1
VW.dados$Const = 1

head(EW.dados)
head(VW.dados)

# # analisando a base
# head(cbind(cbapmewrdata,cbapmewrinstr)[,c("Const" , "C" ,"C-1", "C-2" , "R" , "R-1" , "R-2")], 5)
# head(cbind(cbapmvwrdata,cbapmvwrinstr)[,c("Const" , "C" ,"C-1", "C-2" , "R" , "R-1" , "R-2")], 5)

# # note que as bases sao mensais!! 
# sapply(cbapmewrdata , mean)**12
# sapply(cbapmewrinstr , mean)**12
# 
# sapply(cbapmvwrdata , mean)**12
# sapply(cbapmvwrinstr , mean)**12
# 
# dim(cbapmvwrinstr)
# dim(cbapmvwrdata)

# observacao 2: todas as bases tem mesmo numero de observacoes. OK!

################    Fim tratamento de bases   ##############

###### Contrucao do GMM ##########
# A function of the form g(θ,x) and which returns a n \times q matrix with typical element g_i(θ,x_t)
# for i=1,...q and t=1,...,n.
# This matrix is then used to build the q sample moment conditions.
GMatrix = function(param, base){
  beta  = param[1]
  gamma = param[2]

  C = base$Ct
  R = base$Rt
  
  Z = base[,3:ncol(base)]
      
  g = (beta*(C^(gamma))*R-1)*(as.matrix(Z))

  return(g) #matrix (NxM), conforme exigido pela funcao GMM do R
}

######## Tabela I ##################

 ########### Base EWR  ##############
# Estima??o com um LAG 
EWR1 = gmm(   J
	,x = cbind(cbapmewrdata,(cbapmewrinstr))[,-c(5,7)]
	,t0=c(beta = 0, gamma = -1)
	,type="twoStep"
	,crit=1e-6     # 10e-7
	,tol = 1e-10   # 1e-7
	,itermax=400   # 100
	,vcov="HAC"
	,kernel ="Bartlett"
	)

# Estimacao com DOIS LAG 
EWR2 = gmm(   J
	,x = cbind(cbapmewrdata,(cbapmewrinstr))
	,t0=c(beta = 0, gamma = -1)
	,type="twoStep"
	,crit=1e-6
	,tol = 1e-10
	,itermax=400
	,vcov="HAC"
	,kernel ="Bartlett"
	)

 ########### Base VWR  ##############
# Estimacao com um LAG 
VWR1 = gmm(   J
	,x = cbind(cbapmvwrdata,(cbapmvwrinstr))[,-c(5,7)]
	,t0=c(beta = 0, gamma = -1)
	,type="twoStep"
	,crit=1e-6
	,tol = 1e-10
	,itermax=400
	,vcov="HAC"
	,kernel ="Bartlett"
	)

# Estimacao com DOIS LAGs 
VWR2 = gmm(   J
	,x = cbind(cbapmvwrdata,(cbapmvwrinstr))
	,t0=c(beta = 0, gamma = -1)
	,type="twoStep"
	,crit=1e-6
	,tol = 1e-10
	,itermax=400
	,vcov="HAC"
	,kernel ="Bartlett"
	)

######## Tabela 3 #########

# Estimacao com um LAG 
EWRVWR1 = gmm(   J
	,x = cbind(rbind(cbapmewrdata,cbapmvwrdata),rbind(cbapmewrinstr,cbapmvwrinstr))[,-c(5,7)]
	,t0=c(beta = 0, gamma = -1)
	,type="twoStep"
	,crit=1e-6
	,tol = 1e-10
	,itermax=400
	,vcov="HAC"
	,kernel ="Bartlett"
	)

# Estimacao com DOIS LAGs 
EWRVWR2 = gmm(   J
	,x = cbind(rbind(cbapmewrdata,cbapmvwrdata),rbind(cbapmewrinstr,cbapmvwrinstr))
	,t0=c(beta = 0, gamma = -1)
	,type="twoStep"
	,crit=1e-6
	,tol = 1e-10
	,itermax=400
	,vcov="HAC"
	,kernel ="Bartlett"
	)

# Resultados #
rbind(
EWR1=EWR1$coefficients,
EWR2=EWR2$coefficients,
VWR1=VWR1$coefficients,
VWR2=VWR2$coefficients,
EWRVWR1=EWRVWR1$coefficients,
EWRVWR2=EWRVWR2$coefficients)





