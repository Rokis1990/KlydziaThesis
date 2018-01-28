#ą Cobb-Douglas bandymas

data <- read.delim("~/R/ekonometrijaPROJ/Data.txt")
attach(data)
prod <- GVA_LT
lab <- SEMPh_LT
cap <- GFCF_LT

cd   <- data.frame( prod = prod, lab = lab, cap = cap )

row.names( cd ) <- 1:75

cd.lm <-
  lm(
    formula = I( log( prod ) - log( cap ) ) ~ I( log( lab ) - log( cap ) ),
    data    = cd )
summary( cd.lm )

get( getOption( "device" ) )()

interval <- 10

x <-
  seq(
    from = floor( min( cd$lab ) / interval ) * interval,
    to   = ceiling( max( cd$lab ) / interval ) * interval,
    by   = interval )

y <-
  seq(
    from = floor( min( cd$cap ) / interval ) * interval,
    to   = ceiling( max( cd$cap ) / interval ) * interval,
    by   = interval )


f <- function( x, y )
{
  exp( coef( cd.lm )[ 1 ] ) * ( x ^ coef( cd.lm )[ 2 ] ) *
    ( y ^ ( 1 - coef( cd.lm )[ 2 ] ) )
}

z <- outer( x, y, f )

##  Calculate the plotting limits for the z coordinate.

zlim <- c( floor( min( z ) ), ceiling( max( z ) ) )

cd$prod.hat <- f( x = cd$lab, y = cd$cap )

theta <- -60
phi <- 10

cd.persp <-
  persp(
    x        = x,
    y        = y,
    z        = z,
    theta    = theta,
    phi      = phi,
    col      = "lightblue",
    ltheta   = -135,
    shade    = 0.5,
    ticktype = "detailed",
    main     = "Cobb-Douglas  gamybos funkcija 1999K1-2017K3",
    xlab     = "Darbas (SEMPh_LT)",
    ylab     = "Kapitalas (GFCF_LT)",
    zlab     = "Gamyba (GVA_LT)",
    zlim     = zlim,
    scale    = FALSE,
    border   = NA,
    nticks   = 4 )

cd.below <- subset( cd, cd$prod < cd$prod.hat )

cd.below.trans3d <-
  trans3d(
    x    = cd.below$lab,
    y    = cd.below$cap,
    z    = cd.below$prod,
    pmat = cd.persp )

points( cd.below.trans3d, col = "gray32", pch = 16 )

##  Convert the coordinates of the fitted values.

cd.below.hat.trans3d <-
  trans3d(
    x    = cd.below$lab,
    y    = cd.below$cap,
    z    = cd.below$prod.hat,
    pmat = cd.persp )

##  Draw lines from the observed points to the fitted points.

segments(
  x0  = cd.below.trans3d$x,
  y0  = cd.below.trans3d$y,
  x1  = cd.below.hat.trans3d$x,
  y1  = cd.below.hat.trans3d$y,
  lty = "solid",
  col = "gray32" )

##  Second, plot the data points that are above the surface.

cd.above <- subset( cd, cd$prod >= cd$prod.hat )

cd.above.trans3d <-
  trans3d(
    x    = cd.above$lab,
    y    = cd.above$cap,
    z    = cd.above$prod,
    pmat = cd.persp )

points( cd.above.trans3d, col = "black", pch = 16 )

cd.above.hat.trans3d <-
  trans3d(
    x    = cd.above$lab,
    y    = cd.above$cap,
    z    = cd.above$prod.hat,
    pmat = cd.persp )

segments(
  x0  = cd.above.trans3d$x,
  y0  = cd.above.trans3d$y,
  x1  = cd.above.hat.trans3d$x,
  y1  = cd.above.hat.trans3d$y,
  lty = "solid",
  col = "black" )

