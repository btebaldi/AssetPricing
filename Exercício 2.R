# Limpeza de variaveis antigas
rm(list = ls())

# biblioteca utilizadas
library(gmm)

# Leitura de base de dados #
cbapmewrdata  = read.table("dataset/cbapmewrdata.dat")
cbapmewrinstr = read.table("dataset/cbapmewrinstr.dat")
cbapmvwrdata  = read.table("dataset/cbapmvwrdata.dat")
cbapmvwrinstr = read.table("dataset/cbapmvwrinstr.dat")

# atribuindo nomes (verificar! Nomes determinados olhando as m?dias) (palpite!) 

names(cbapmewrdata) = c("C" , "R")
names(cbapmvwrdata) = c("C" , "R")

names(cbapmewrinstr) = c("Const" , "C-1", "C-2" , "R-1" , "R-2")
names(cbapmvwrinstr) = c("Const" , "C-1", "C-2" , "R-1" , "R-2")

# analisando a base
head(cbind(cbapmewrdata,cbapmewrinstr)[,c("Const" , "C" ,"C-1", "C-2" , "R" , "R-1" , "R-2")], 5)
head(cbind(cbapmvwrdata,cbapmvwrinstr)[,c("Const" , "C" ,"C-1", "C-2" , "R" , "R-1" , "R-2")], 5)

# note que as bases s?o mensais!! 
sapply(cbapmewrdata , mean)**12
sapply(cbapmewrinstr , mean)**12

sapply(cbapmvwrdata , mean)**12
sapply(cbapmvwrinstr , mean)**12

dim(cbapmvwrinstr)
dim(cbapmvwrdata)

# observa??o 2: todas as bases tem mesmo n?mero de observa??es. OK!

################    Fim tratamento de bases   ##############

###### Contru??o do GMM ##########
J = function(param , base){

  beta  = param[1]
  gamma = param[2]

  C = base$C
  R = base$R
  Z = base[,3:ncol(base)]
      
  g = (beta*(C^(gamma))*R-1)*(as.matrix(Z))

  return(g) #matrix (NxM), conforme exigido pela func?o GMM do R
}

######## Tabela I ##################

 ########### Base EWR  ##############
# Estima??o com um LAG 
EWR1 = gmm(   J
	,x = cbind(cbapmewrdata,(cbapmewrinstr))[,-c(5,7)]
	,t0=c(beta = 0, gamma = -1)
	,type="twoStep"
	,crit=1e-6
	,tol = 1e-10
	,itermax=400
	,vcov="HAC"
	,kernel ="Bartlett"
	)

# Estima??o com DOIS LAG 
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
# Estima??o com um LAG 
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

# Estima??o com DOIS LAGs 
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

# Estima??o com um LAG 
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

# Estima??o com DOIS LAGs 
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





