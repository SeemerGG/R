Time <- double(10)
Res <- double(10)
Num <- integer(10)
#Test_KurVA <- data.frame(Time, Res, Num)

t0 <- Sys.time()
xA <- seq(100, 200, 5)
Res[1] <- sum(xA)
t1 <- Sys.time()
Time[1] <- t1 - t0

Res[2] <- length(xA)
t2 <- Sys.time()
Time[2] <- t2 - t1


Res[3] <- mean(xA)
t3 <- Sys.time()
Time[3] <- t3 - t2

Res[4] <- round(sd(rnorm(Res[2]+7, mean = 5)), 0)
t4 <- Sys.time()
Time[4] <- t4 - t3

arr <- array(xA, dim = c(5, length(xA)/5))
Res[5] <- round(sum(sin(arr)), 4)
t5 <- Sys.time()
Time[5] <- t5 - t4


matr <- matrix(sample(xA, 5*length(xA), replace = TRUE), 5, length(xA))

