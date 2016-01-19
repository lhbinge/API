#==============================#
# Bubbles: Explosive Behaviour
#==============================#


y <- hedonic_indices[,2]

# Check explosivity of time series
library(tseries)
adf.test(y, alternative = c("stationary", "explosive"),k = trunc((length(x)-1)^(1/3)))

#Loop oor dievolgende toets (vir die window size en die veskillende indekse)
toets1 <- adf.test(y, alternative = "explosive", k=4)
bubble <- toets1$statistic

bubble$p <- toets$p.value  #maar wat is die critical value? Simulate self


#OF:
library(urca)
ur.df(y, type = c("none", "drift", "trend"), lags = 1,
      selectlags = c("Fixed", "AIC", "BIC"))

toets2 <- ur.df(y, c("none", "drift", "trend"), lags = 4, selectlags = c("AIC"))
bubble2 <- toets2@teststat

summary(toets2)


y <- hedonic_indices[,2]
toets <- ur.df(y, type= "none", lags = 4, selectlags = c("AIC"))
toets <- ur.df(y, type= "drift", lags = 4, selectlags = c("AIC"))
toets <- ur.df(y, type= "trend", lags = 4, selectlags = c("AIC"))
summary(toets)

#indicate dat daar nie 'n drift of trend component hoef te wees nie.

y_indices <- all_indices[,-6]
bubble.nc <- list()
for(i in 1:ncol(y_indices)) {
    #bubble1 <- numeric()
    bubble <- numeric()
    for(j in 12:62) {
        y <- y_indices[1:j,i]
        #toets1 <- adf.test(y, alternative = "explosive", k=4)
        #bubble1 <- rbind(bubble1,toets1$statistic)
        toets <- ur.df(y, type= "none", lags = 4, selectlags = c("AIC"))
        #toets <- ur.df(y, type= "drift", lags = 4, selectlags = c("AIC"))
        #toets <- ur.df(y, type= "trend", lags = 4, selectlags = c("AIC"))
        bubble <- rbind(bubble,toets@teststat)
    
    }
    bubble.nc[[i]] <- bubble
}

y_indices <- all_indices[,-6]
bubble.c <- list()
for(i in 1:ncol(y_indices)) {
    #bubble1 <- numeric()
    bubble <- numeric()
    for(j in 12:62) {
        y <- y_indices[1:j,i]
        #toets1 <- adf.test(y, alternative = "explosive", k=4)
        #bubble1 <- rbind(bubble1,toets1$statistic)
        #toets <- ur.df(y, type= "none", lags = 4, selectlags = c("AIC"))
        toets <- ur.df(y, type= "drift", lags = 4, selectlags = c("AIC"))
        #toets <- ur.df(y, type= "trend", lags = 4, selectlags = c("AIC"))
        bubble <- rbind(bubble,toets@teststat)
        
    }
    bubble.c[[i]] <- bubble
}


y_indices <- all_indices[,-6]
bubble.ct <- list()
for(i in 1:ncol(y_indices)) {
    #bubble1 <- numeric()
    bubble <- numeric()
    for(j in 12:62) {
        y <- y_indices[1:j,i]
        #toets1 <- adf.test(y, alternative = "explosive", k=4)
        #bubble1 <- rbind(bubble1,toets1$statistic)
        #toets <- ur.df(y, type= "none", lags = 4, selectlags = c("AIC"))
        #toets <- ur.df(y, type= "drift", lags = 4, selectlags = c("AIC"))
        toets <- ur.df(y, type= "trend", lags = 4, selectlags = c("AIC"))
        bubble <- rbind(bubble,toets@teststat)
        
    }
    bubble.ct[[i]] <- bubble
}
#---------------------------------------------------------------------------

library(dyn)
yt <- ts(y)
xt <- ts(x)
dyn$lm(yt ~ xt + lag(yt, -1))


##--------------------------------------------------------------------------
K1 <- numeric()
K2 <- numeric()
K3 <- numeric()

for(j in 12:62) {

    set.seed(123)                           #for replicability
    reps <- 2000                            #Monte Carlo replications
    burn <- 50                              #burn in periods: first generate a T+B sample
                                            #To make "sure" that influence of initial values has faded
    #obs <- 62                              #ultimate sample size
    obs <- j

    tstat.nc <- numeric()
    tstat.c <- numeric()
    tstat.ct <- numeric()

    for(i in 1:reps) {     
        e <- rnorm(obs+burn)
        e[1] <- 0
        Y1 <- cumsum(e)
        DY1 <- diff(Y1)

        y1 <- Y1[(burn+1):(obs+burn)]               #trim off burn period
        dy1 <- DY1[(burn+1):(obs+burn)]             
        ly1 <- Y1[burn:(obs+burn-1)] 
        trend <- 1:obs
    
        EQ1 <- lm(dy1 ~ 0 + ly1)       
        tstat.nc <- rbind(tstat.nc,summary(EQ1)$coefficients[1,3]) 
        EQ2 <- lm(dy1 ~ ly1)            
        tstat.c <- rbind(tstat.c,summary(EQ2)$coefficients[2,3])  
        EQ3 <- lm(dy1 ~ lag(y1) + trend)    
        tstat.ct <- rbind(tstat.ct,summary(EQ3)$coefficients[2,3]) 
    }                                       

    #hist(tstat.nc)
    #K1 <- quantile(tstat.nc, probs=c(0.9,0.95,0.99)) 
    #K2 <- quantile(tstat.c, probs=c(0.9,0.95,0.99))
    #K3 <- quantile(tstat.ct, probs= c(0.9,0.95,0.99))

    K1 <- rbind(K1,quantile(tstat.nc, probs=c(0.9,0.95,0.99)))
    K2 <- rbind(K2,quantile(tstat.c, probs=c(0.9,0.95,0.99)))
    K3 <- rbind(K3,quantile(tstat.ct, probs= c(0.9,0.95,0.99)))
}


#return(K1,K2,K3)

#REPEAT HIERDIE EXERCISE VIR 1-62 OBSERVATIONS?
#- DIT GEE 'N VECTOR VAN 62 CRITICAL VALUES

#crit. <- data.frame()
#for(j in 10:62) { 
#    monte.carlo(j)
#}

##---------------------------------------------------------------------------











##---------------------------------------------------------------



E=rnorm(240)
X=cumsum(E)
plot(X,type="l")

lags=0
z=diff(X)
n=length(z)
z.diff=embed(z, lags+1)[,1]
z.lag.1=X[(lags+1):n]
summary(lm(z.diff~0+z.lag.1 ))
summary(lm(z.diff~0+z.lag.1 ))$coefficients[1,3]

library(urca)
df=ur.df(X,type="none",lags=0)
df
summary(df)

library(tseries)
adf.test(X,k=0)


lags=1
z=diff(X)
n=length(z)
z.diff=embed(z, lags+1)[,1]
z.lag.1=X[(lags+1):n]
k=lags+1
z.diff.lag = embed(z, lags+1)[, 2:k]
summary(lm(z.diff~0+z.lag.1+z.diff.lag ))
summary(lm(z.diff~0+z.lag.1+z.diff.lag ))$coefficients[1,3]

df=ur.df(X,type="none",lags=1)
summary(df)

adf.test(X,k=1)

summary(lm(z.diff~1+z.lag.1+z.diff.lag ))
summary(lm(z.diff~1+z.lag.1+z.diff.lag ))$coefficients[2,3]
anova(lm(z.diff ~ z.lag.1 + 1 + z.diff.lag),lm(z.diff ~ 0 + z.diff.lag))$F[2]

df=ur.df(X,type="drift",lags=1)
summary(df)

temps=(lags+1):n
summary(lm(z.diff~1+temps+z.lag.1+z.diff.lag ))

summary(lm(z.diff~1+temps+z.lag.1+z.diff.lag ))$coefficients[3,3]
anova(lm(z.diff ~ z.lag.1 + 1 + temps+ z.diff.lag),lm(z.diff ~ 1+ z.diff.lag))$F[2]

df=ur.df(X,type="trend",lags=1)
summary(df)








