library(micEconCES)

a<-data("MishraCES")
a
colMeans(MishraCES)
b<-data("GermanIndustry")

data <- read.delim(("~/R/ekonometrijaPROJ/data.txt"))
attach(data)

data_ol <- read.delim("https://raw.githubusercontent.com/Rokis1990/KlydziaThesis/master/data/data.txt") # tik pademonstravimui kaip ištraukti online txt failus. Githube reik eiti į raw direktoriją.
y0 <- ts(log(GVA_LT)*100, start=c(1999, 1), frequency=4)
y <- y0 - decompose(y0)$seasonal
l0 <- ts(log(SEMPh_LT)*100, start=c(1999, 1), frequency=4)
l <- l0 - decompose(l0)$seasonal
k0 <- ts(log(GFCF_LT)*100, start=c(1999, 1), frequency=4)
k <- k0 - decompose(k0)$seasonal
dy <- diff(y)
dl <- diff(l)
dk <- diff(k)
plot.ts(dy)
plot.ts(dl)
plot.ts(dk)
??MicEconCES


set.seed( 123 )
cesData <- data.frame(x1 = rchisq(200, 10), x2 = rchisq(200, 10), x3 = rchisq(200, 10), x4 = rchisq(200, 10) )
cesData$y2 <- cesCalc( xNames = c( "x1", "x2" ), data = cesData, coef = c( gamma = 1, delta = 0.6, rho = 0.5, nu = 1.1 ) )


cesData$y2 <- cesData$y2 + 2.5 * rnorm( 200 )
cesData$y3 <- cesCalc(xNames = c("x1", "x2", "x3"), data = cesData,
                      coef = c( gamma = 1, delta_1 = 0.7, delta = 0.6, rho_1 = 0.3, rho = 0.5,
                                nu = 1.1), nested = TRUE )
cesData$y3 <- cesData$y3 + 1.5 * rnorm(200)
cesData$y4 <- cesCalc(xNames = c("x1", "x2", "x3", "x4"), data = cesData,
                      coef = c(gamma = 1, delta_1 = 0.7, delta_2 = 0.6, delta = 0.5,
                               rho_1 = 0.3, rho_2 = 0.4, rho = 0.5, nu = 1.1), nested = TRUE )
cesData$y4 <- cesData$y4 + 1.5 * rnorm(200)


cesNls <- nls( y2 ~ gamma * ( delta * x1^(-rho) + (1 - delta) * x2^(-rho) )^(-phi / rho),
               data = cesData, start = c( gamma = 0.5, delta = 0.5, rho = 0.25, phi = 1 ) )
print( cesNls )

attach(cesData)
plot.ts(x1)
plot.ts(x2)
plot.ts(x3)
plot.ts(x4)
plot.ts(y2)
plot.ts(y3)
plot.ts(y4)

cesKmenta <- cesEst( yName = "y2", xNames = c( "x1", "x2" ), data = cesData,
                     method = "Kmenta", vrs = TRUE )
summary(cesKmenta)
acf(residuals(cesKmenta))
coef( summary( cesKmenta$kmenta ) )
# b12=-b11=-b22 turi labai skirtis nuo 0 kad gamybos funkcija nebutu CD


#lyginam Kmentos estimated CES su realiu Y
library( "miscTools" )
compPlot ( cesData$y2, fitted( cesKmenta ), xlab = "actual values",
           ylab = "fitted values" )
# Kmenta aproximation tiksliau veikia kai gamybos funkcija yra realiai artima CD



#3.2. Gradient-based optimisation algorithms
#Levenberg-Marquardt
#maximum neighbourhood method which performs an optimum
#interpolation between a rst-order Taylor series approximation (Gauss-Newton method) and a
#steepest-descend method (gradient method) (Marquardt 1963)

#Levenberg-Marquardt algorithm outperforms
#the other methods and gives the best estimates of the CES parameters. However, the
#Levenberg-Marquardt algorithm performs as poorly as the other methods in estimating the
#elasticity of substitution

#Although the Levenberg-Marquardt algorithm does not live up to modern standards, we include
#it for reasons of completeness, as it is has proven to be a standard method for estimating
#CES functions

# dviems inputams
cesLm2 <- cesEst( "y2", c( "x1", "x2" ), cesData, vrs = TRUE )
summary( cesLm2 )

# trims inputams
cesLm3 <- cesEst( "y3", c( "x1", "x2", "x3" ), cesData, vrs = TRUE )
summary( cesLm3 )

# HM - direct elasticity of substitution
# AU - Allen elasticity of substitution

# keturi? input? GF
cesLm4 <- cesEst( "y4", c( "x1", "x2", "x3", "x4" ), cesData, vrs = TRUE )
summary( cesLm4 )


# palyginam estimates su real data

compPlot ( cesData$y2, fitted( cesLm2 ), xlab = "actual values",
           ylab = "fitted values", main = "two-input CES" )
compPlot ( cesData$y3, fitted( cesLm3 ), xlab = "actual values",
           ylab = "fitted values", main = "three-input nested CES" )
compPlot ( cesData$y4, fitted( cesLm4 ), xlab = "actual values",
           ylab = "fitted values", main = "four-input nested CES" )



#Conjugate Gradients
#One of the gradient-based optimisation algorithms that can be used by cesEst is the \Conjugate
#Gradients"method based on Fletcher and Reeves (1964). This iterative method is mostly
#applied to optimisation problems with many parameters and a large and possibly sparse Hessian
#matrix, because this algorithm does not require that the Hessian matrix is stored or
#inverted. The \Conjugated Gradient" method works best for objective functions that are
#approximately quadratic and it is sensitive to objective functions that are not well-behaved
#and have a non-positive semi-denite Hessian, i.e., convergence within the given number of
#iterations is less likely the more the level surface of the objective function diers from spherical
#(Kelley 1999). Given that the CES function has only few parameters and the objective
#function is not approximately quadratic and shows a tendency to \
#at surfaces" around the
#minimum, the \Conjugated Gradient"method is probably less suitable than other algorithms
#for estimating a CES function. Setting argument method of cesEst to "CG" selects the \Conjugate
#Gradients" method for estimating the CES function by non-linear least-squares.

cesCg <- cesEst( "y2", c( "x1", "x2" ), cesData, vrs = TRUE, method = "CG" )
summary( cesCg )
compPlot ( cesData$y2, fitted( cesCg ), xlab = "actual values", ylab = "fitted values", main = "Conjugated Gradient two-input CES" )

#Although the estimated parameters are similar to the estimates from the Levenberg-Marquardt
#algorithm, the \Conjugated Gradient" algorithm reports that it did not converge. Increasing
#the maximum number of iterations and the tolerance level leads to convergence. This conrms
#a slow convergence of the \Conjugate Gradients" algorithm for estimating the CES function.

cesCg2 <- cesEst( "y2", c( "x1", "x2" ), cesData, vrs = TRUE, method = "CG", control = list( maxit = 1000, reltol = 1e-5 ) )
summary(cesCg2)      
compPlot ( cesData$y2, fitted( cesCg2 ), xlab = "actual values", ylab = "fitted values", main = "Conjugated Gradient 2 two-input CES" )

#Newton
#Another algorithm supported by cesEst that is probably more suitable for estimating a CES
#function is an improved Newton-type method. As with the original Newton method, this
#algorithm uses rst and second derivatives of the objective function to determine the direction
#of the shift vector and searches for a stationary point until the gradients are (almost) zero.
#However, in contrast to the original Newton method, this algorithm does a line search at each
#iteration to determine the optimal length of the shift vector (step size) as described in Dennis
#and Schnabel (1983) and Schnabel, Koontz, and Weiss (1985). Setting argument method of
#cesEst to "Newton" selects this improved Newton-type method. The user can modify a few
#details of this algorithm (e.g., the maximum step length) by adding further arguments that
#are described in the documentation of the R function nlm. The following commands estimate
#a CES function by non-linear least-squares using this algorithm and print summary results.

cesNewton <- cesEst( "y2", c( "x1", "x2" ), cesData, vrs = TRUE, method = "Newton" )
summary( cesNewton )
compPlot ( cesData$y2, fitted( cesNewton ), xlab = "actual values", ylab = "fitted values", main = "Newton two-input CES" )


#Broyden-Fletcher-Goldfarb-Shanno
#Furthermore, a quasi-Newton method developed independently by Broyden (1970), Fletcher
#(1970), Goldfarb (1970), and Shanno (1970) can be used by cesEst. This so-called BFGS
#algorithm also uses rst and second derivatives and searches for a stationary point of the
#objective function where the gradients are (almost) zero. In contrast to the original Newton
#method, the BFGS method does a line search for the best step size and uses a special procedure
#to approximate and update the Hessian matrix in every iteration. The problem with
#BFGS can be that although the current parameters are close to the minimum, the algorithm
#does not converge because the Hessian matrix at the current parameters is not close to the
#Hessian matrix at the minimum. However, in practice, BFGS proves robust convergence (often
#                                                                                     superlinear) (Kelley 1999). If argument method of cesEst is "BFGS", the BFGS algorithm
#is used for the estimation. The user can modify a few details of the BFGS algorithm (e.g.,
#                                                                                     the convergence tolerance level) by adding the further argument control as described in the
#Details" section of the documentation of the R function optim.

cesBfgs <- cesEst( "y2", c( "x1", "x2" ), cesData, vrs = TRUE, method = "BFGS" )
summary( cesBfgs )
compPlot ( cesData$y2, fitted( cesBfgs ), xlab = "actual values", ylab = "fitted values", main = "BFGS or quasi-Newton two-input CES" )


# cesData panaudoju is random generated example, o y, l ir k is realiu LT duomenu
cesDataLT <- data.frame(x1 = l, x2 = k)
ProdLT_CES <- cesEst(dy, c(dl, dk), cesDataLT, vrs= TRUE, method ="BFGS")
# bbs neveikia. su cesData reik kazka padarryt. be to, del ko tie cesEst argumentai kabutese??
