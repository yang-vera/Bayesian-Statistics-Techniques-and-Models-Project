train <- read.csv('train.csv',header=TRUE)

str(train)

temp <- sapply(train, function(x)  sum(is.na(x)) )
miss <- sort(temp, decreasing=T)
miss[miss>0]
temp2 = miss[miss>0]
Drop <- names(train) %in% c("PoolQC","MiscFeature","Alley","Fence","FireplaceQu","LotFrontage", "GarageType", "GarageFinish", "GarageQual", "GarageCond", "BsmtExposure", "BsmtFinType2", "BsmtQual", "BsmtCond", "BsmtFinType1", "MasVnrType", "MasVnrArea", "Electrical")
train <- train[!Drop]

dat = na.omit(train)

train$Utilities <- NULL
dat = na.omit(train)

dat$Id<- NULL
dat$MoSold<-NULL

chosen<- names(dat) %in% c("SalePrice","MSSubClass","MSZoning","LotArea","BldgType","SaleCondition","YrSold", "YearRemodAdd", "OverallQual", "ExterQual")
dat <- dat[chosen]
str(dat)
dat$age = dat$YrSold-dat$YearRemodAdd
str(dat)

library("corrplot")
Cor = cor(dat)
corrplot(Cor, type="upper", method="ellipse", tl.pos="d")
corrplot(Cor, type="lower", method="number", col="black", 
         add=TRUE, diag=FALSE, tl.pos="n", cl.pos="n")
pairs(dat)
dat$YrSold<-NULL
dat$YearRemodAdd<-NULL

dat$MSZoning = as.numeric(dat$MSZoning)
dat$BldgType = as.numeric(dat$BldgType)
dat$ExterQual = as.numeric(dat$ExterQual)
dat$SaleCondition = as.numeric(dat$SaleCondition)

dat$SaleCondition = as.factor(dat$SaleCondition)
dat$BldgType = as.factor(dat$BldgType)
X = model.matrix( ~ BldgType + SaleCondition, data=dat)
hist(dat$OverallQual)



library("rjags")

mod_string = " model {
  for (i in 1:length(y)) {
    y[i] ~ dnorm(mu[i], prec)
    mu[i] = a[zone[i]] + b[1]*log_class[i] + b[2]*log_area[i]+b[3]*isBld2[i] + b[4]*isBld3[i] + b[5]*isBld4[i]+b[6]*isBld5[i] + b[7]*iscon2[i] + b[8]*iscon3[i]+b[9]*iscon4[i]+b[10]*iscon5[i]+b[11]*iscon6[i] + b[12]*age[i] +b[13]*overall[i]
  }
  
  for (j in 1:max(zone)) {
    a[j] ~ dnorm(a0, prec_a)
  }
  
  a0 ~ dnorm(0.0, 1.0/1.0e6)
  prec_a ~ dgamma(1/2.0, 1*10.0/2.0)
  tau = sqrt( 1.0 / prec_a )
  
  for (j in 1:13) {
    b[j] ~ dnorm(0.0, 1.0/1.0e6)
  }
  
  prec ~ dgamma(5/2.0, 5*10.0/2.0)
  sig = sqrt( 1.0 / prec )
} "

set.seed(116)
data_jags = list(y=dat$logSalePrice, zone = dat$MSZoning, log_class=dat$logMSSubClass, log_area=dat$logLotArea, isBld2=X[,"BldgType2"], isBld3=X[,"BldgType3"], isBld4=X[,"BldgType4"],isBld5=X[,"BldgType5"],iscon2=X[,"SaleCondition2"], iscon3=X[,"SaleCondition3"],iscon4=X[,"SaleCondition4"],iscon5=X[,"SaleCondition5"], iscon6=X[,"SaleCondition6"], age=dat$age, overall=dat$OverallQual)

params = c("a0", "a", "b", "sig", "tau")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=10)
update(mod, 5e3) # burn-in

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=1.5e4)

mod_csim = as.mcmc(do.call(rbind, mod_sim)) # combine multiple chains

## convergence diagnostics
plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
effectiveSize(mod_sim)
dic.samples(mod, n.iter=1e3)


mod_string2 = " model {
  for (i in 1:length(y)) {
    y[i] ~ dnorm(mu[i], prec)
    mu[i] = int  + b[1]*log_class[i] + b[2]*log_area[i]+b[3]*isBld2[i] + b[4]*isBld3[i] + b[5]*isBld4[i]+b[6]*isBld5[i] + b[7]*iscon2[i] + b[8]*iscon3[i]+b[9]*iscon4[i]+b[10]*iscon5[i]+b[11]*iscon6[i] + b[12]*age[i] +b[13]*overall[i] + b[14]*zone2[i] + b[15]*zone3[i] + b[16]*zone4[i]+b[17]*zone5[i]
  }
  int ~ dnorm(0.0, 1.0/1.0e6)

  for (j in 1:17) {
    b[j] ~ dnorm(0.0, 1.0/1.0e6)
  }
  
  prec ~ dgamma(5/2.0, 5*10.0/2.0)
  sig = sqrt( 1.0 / prec )
} "

set.seed(116)
data_jags2 = list(y=dat$logSalePrice, log_class=dat$logMSSubClass, log_area=dat$logLotArea, isBld2=X[,"BldgType2"], isBld3=X[,"BldgType3"], isBld4=X[,"BldgType4"],isBld5=X[,"BldgType5"],iscon2=X[,"SaleCondition2"], iscon3=X[,"SaleCondition3"],iscon4=X[,"SaleCondition4"],iscon5=X[,"SaleCondition5"], iscon6=X[,"SaleCondition6"], age=dat$age, overall=dat$OverallQual, zone2 = X2[, "MSZoning2"], zone3 = X2[, "MSZoning3"], zone4 = X2[, "MSZoning4"], zone5 = X2[, "MSZoning5"])

params2 = c("int", "b", "sig")

mod2 = jags.model(textConnection(mod_string2), data=data_jags2, n.chains=5)
update(mod2, 2e3) # burn-in

mod_sim2 = coda.samples(model=mod2,
                        variable.names=params2,
                        n.iter=1e4)

mod_csim2 = as.mcmc(do.call(rbind, mod_sim2)) # combine multiple chains


dic.samples(mod2, n.iter=1e3)
params = colMeans(mod_csim)

a = params[dat$MSZoning]
a = as.numeric(a)

y_hat = a + params["b[1]"]*dat$logMSSubClass + params["b[2]"]*dat$logLotArea + X[,c(2,3,4,5)] %*% params[9:12] + X[,c(6,7,8,9,10)] %*% params[13:17] + params["b[12]"]*dat$age + params["b[13]"]*dat$OverallQual  

residual = dat$logSalePrice - y_hat
plot(residual)
plot(y_hat, residual)
gelman.diag(mod_sim)
