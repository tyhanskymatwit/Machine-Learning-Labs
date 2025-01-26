#Q:1a
x1 <- seq(-5, 5, by = 0.1)
x2 <- 1 + 3 * x1

plot(x1, x2, type = "l", col = "blue", lwd = 2, xlab = "x1", ylab = "x2", xlim = c(-5, 5), ylim = c(-10, 20))

polygon(c(x1, rev(x1)), c(x2, rep(20, length(x1))), col = rgb(1, 0, 0, 0.3), border = NA)
polygon(c(x1, rev(x1)), c(x2, rep(-10, length(x1))), col = rgb(0, 0, 1, 0.3), border = NA)
title("Plot of the hyperplane 1 + 3x1 - x2 = 0")

#Q:1b
x1 <- seq(-5, 5, by = 0.1)
x2 <- (2 - x1) / 2

plot(x1, x2, type = "l", col = "green", lwd = 2, xlab = "x1", ylab = "x2", xlim = c(-5, 5), ylim = c(-5, 5))

polygon(c(x1, rev(x1)), c(x2, rep(5, length(x1))), col = rgb(1, 0, 0, 0.3), border = NA)
polygon(c(x1, rev(x1)), c(x2, rep(-5, length(x1))), col = rgb(0, 0, 1, 0.3), border = NA)

title("Plot of the hyperplane -2 + x1 + 2x2 = 0")