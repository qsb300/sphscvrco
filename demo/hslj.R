require("sphscvrco", quietly = TRUE)

# default for demo
sigma <- 1
temp <- 1
radcut <- 2.5
lower <- 0
upper <- Inf

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) {
    sigma <- as.numeric(args[1])
}
if (length(args) >= 2) {
    temp <- as.numeric(args[2])
}
beta <- 1/temp
if (length(args) >= 3) {
    radcut <- as.numeric(args[3])
}
if (length(args) >= 4) {
    lower = as.numeric(args[4])
    cat("Modified lower:", lower, "\n")
}
if (length(args) >= 5) {
    upper = as.numeric(args[5])
    cat("Modified upper:", upper, "\n")
}

HSB2 <- SphB22(hardsphere, beta, 1, lower = lower, upper = upper)
cat("HS B2:", HSB2, "\n")

LJB2 <- SphB22(lj, beta, 1, sigma, lower = lower, upper = upper)
cat("LJ B2:", LJB2, "T:", temp, "\n")

LJcutB2 <- SphB22(LJCut, beta, 1, sigma, radcut, lower = lower, upper = upper)
cat("LJcut B2:", LJcutB2, "T:", temp, "Rc:", radcut, "\n")

LJShfB2 <- SphB22(LJShf, beta, 1, sigma, radcut, lower = lower, upper = upper)
cat("LJShf B2:", LJShfB2, "T:", temp, "Rc:", radcut, "\n")

