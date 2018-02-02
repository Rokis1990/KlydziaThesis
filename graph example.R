# taken from https://economics.stackexchange.com/questions/8873/ces-production-function-with-rho1

remove(list = ls())

y1 <- 5
y2 <- 7

curve(sqrt(y1^2-x^2), from=0, to=10, ylim=c(0,10),type = "l", xlab = expression(x[1]), ylab = expression(x[2]), xaxs="i", yaxs="i",axes = TRUE, lwd = 2, bty="n")
curve(sqrt(y2^2-x^2), from=0, to=10, ylim=c(0,10),type = "l", xlab = "x_1", ylab = "x_2", xaxs="i", yaxs="i", lwd = 2, bty="n", add = TRUE)

segments(x0 = 0, y = 5, x1 = 4, y1 = 0, col = 1, lwd = 2,lty = 2)

points(0, 5, type="p", pch=19, col="black", cex=2)
text(0.5, 5.5, "A",cex = 1.3)
text(4.1, 4.1, expression("y=5"),cex = 1.3)
text(5.6, 5.6, expression("y=7"),cex = 1.3)
