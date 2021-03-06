# A three-state progressive survival model: exploring msm and ELECT a bit more
# Example with toy data 

# Ardo, UCL 2016

# Requirements:
# - R package msm should be installed
# - Working directory contains the files <dataEx1.RData> and <ELECT.r>
#   (You can set the working directory in R by using the pull-down menu)

###############################
# SHEET 4
# Q1:

# Preliminaries:
library(msm)
# Define rounding in output:
digits <- 3

# Load data <dta>: 
load("dataEx1.RData")

# Load newest version of ELECT
source("ELECT_WIP.r")

# Data info:
subjects <- as.numeric(names(table(dta$id)))
N <- length(subjects)
cat("\nSample size:",N,"\n")
cat("\nFrequencies observed state:"); print(table(dta$state))
cat("\nFrequencies number of observations:"); print(table(table(dta$id)))
cat("\nState table:"); print(statetable.msm(state,id,data=dta))


# Choose model:
Model <- 2
covariates <- as.formula("~age+x")
# Generator matrix Q:
q <- 0.01; Q <- rbind(c(0,q,q), c(q,0,q),c(0,0,0))
qnames <- c("q12","q21","q13","q23")
# Optimisation method ("BFGS" or "Nelder-Mead"):
method <- "BFGS"

# Model fit using msm:
if(0){
model <- msm(state~age, subject=id, data=dta, center=FALSE, 
             qmatrix=Q, death=TRUE, covariates=covariates,
             censor= -2, censor.states=c(1,2), method=method,
             control=list(trace=0,REPORT=1,maxit=1000,fnscale=10000))
}

# Generate output:
cat("\n---------------------------------------\n")
cat("\nModel",Model," with covariates: "); print(covariates)
cat("Convergence code =", model$opt$convergence,"\n")
minus2LL <-  model$minus2loglik
cat("-2loglik =", minus2LL,"\n")
p <- model$estimates
p.se <- sqrt(diag(model$covmat))
# Results to screen:
cat("\nParameter estimats and SEs:\n")
print(cbind(q=qnames,p=round(p,digits),se=round(p.se,digits)),quote=FALSE)
cat("\n---------------------------------------\n")

###############################
# SHEET 4
# Qs 2 3 and 4:

#############################################################
# Life expectancies for Model 2:
sddata <- dta[dta$state%in%c(1,2),]
age0 <- 65
age.max <- 115
x0 <- 0
h <- 0.25
LEs.pnt <- elect(model=model, b.covariates=list(age=age0,x=x0),
                 statedistdata=sddata, time.scale.msm="years",
                 h=h, age.max=age.max, S=0)
summary.elect(LEs.pnt, digits=4)

# Explore different integration methods:
LEs.pnt <- elect(model=model, b.covariates=list(age=age0,x=x0),
                 statedistdata=sddata, time.scale.msm="years",
                 h=h, age.max=age.max, S=0, method="Simpson")
summary.elect(LEs.pnt, digits=4)
LEs.pnt <- elect(model=model, b.covariates=list(age=age0,x=x0),
                 statedistdata=sddata, time.scale.msm="years",
                 h=h, age.max=age.max, S=0, method="MiddleRiemann")
summary.elect(LEs.pnt, digits=4)

# Explore simulation:
LEs.pnt <- elect(model=model, b.covariates=list(age=age0,x=x0),
                 statedistdata=sddata, time.scale.msm="years",
                 h=h, age.max=age.max, S=500, method="Simpson")
plot.elect(LEs.pnt, col="green",lwd=3)
