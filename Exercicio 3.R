op <- par(mfrow = c(2, 2))

# Função lambda: Sensitivity function #

lambda =   function(S,sigma, gamma, phi){
#     Sb = sigma*sqrt(gamma/(1 - phi))
    Sb = 0.057    
    
    Smax = exp(log(Sb) + 0.5*(1-Sb^2))
    LAMBDA = vector()
    LAMBDA[log(S) <= log(Smax)] = (1/Sb)*sqrt(1-2*log(S[log(S) <= log(Smax)]/Sb))-1
    LAMBDA[log(S) > log(Smax)] = 0
    
    return(LAMBDA)
    }

# Função derivada dX/dc #
dX =   function(S,sigma, gamma, phi){
  1 - lambda(S)/((S)**(-1))-1)
}

#### Gráficos ####

curve(lambda(x,sigma = 1, gamma = 2, phi = .13)
      ,.001
      ,.1
      ,xlab = ("Surplus consumption ratio S = (C-X)/C")
      ,ylab = expression(paste(lambda, "(s)"))
      ,add = F
      ,col = 1
      ,lty = 1
      ,lwd = 3
      ,xlim = c(0,.1)
      ,ylim = c(0,60))
abline(v = 0.057 , lwd = 3)
abline(v = exp(log(0.057) + 0.5*(1-0.057^2)), lty = 3, lwd = 3)
abline(h = 0)
abline(v = 0)

curve(dX(x,sigma = 1, gamma = 2, phi = .13)
      ,.001
      ,.1
      ,xlab = ("Surplus consumption ratio S = (C-X)/C")
      ,ylab = "dx/dc"
      ,add = F
      ,col = 1
      ,lwd = 2
      ,xlim = c(0,.1)
      ,ylim = c(0,1)
      )
abline(v = 0.057 , lwd = 3)
abline(v = exp(log(0.057) + 0.5*(1-0.057^2))
       ,lwd = 1
       ,lty = 3)
abline(h = 0)
abline(v = 0)
par(op)

### Daqui para baixo deu tudo errado! A função explode!

Gera_hist = function(S_0 , Sb, nper , phi , gamma , sigma, g){

  S = c(rep(NA),nper)
  erro = rnorm(nper, g , sigma)
  
  erro[1] = 0
  S[1] = S_0
  
  for (i in 2:nper){

    S[i] = exp((1-0.9)*(log(0.057)) + phi*log(S[eval(i-1)]) + lambda(S[eval(i-1)])*erro[i])
    
  }
    
  saida = cbind(S)
  return(saida)
}



vetor = Gera_hist(S_0 = 0.05
                  ,Sb= 0.057
                  ,nper = 10000
                  ,phi = 0.87**(1/12)
                  ,gamma =  2
                  ,sigma = 0/sqrt(12)
                  ,g = 1.89/1200)

plot(vetor[1:20] , type = "l")

plot(vetor[,2])
head(vetor, 20)


hist(exp(vetor))
plot(vetor[,2])
max(vetor)
summary(exp(vetor))

