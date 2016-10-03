#' Convert u into Partition for Integration
#'
#' Convert u into Partition for Integration, (exp(-u)*b-1)*x^2, use closures
#' following http://adv-r.had.co.nz/Functional-programming.html
#' @param pot potential function in form of pot(r, ...)
#' @param beta kB/T
#' @export
#' @examples
#' partHS <- u2pc(hardsphere)
#' partHS(0.5, 1.0, 1)
#' partHS(1.5, 1.0, 1)
u2pc <- function(pot) {
    function(x, beta, ...) {
        u <- pot(x, ...)
        (exp(-u * beta) - 1) * x^2
    }
}

#' B22 for sphere with r depended potential, J Biol Phys 2015 41:85-97 eq.3.
#' @param pot potential function in form of pot(r, ...)
#' @param beta kB/T
#' @param lower lower limit for integration
#' @param upper upper limit for integration
#' @export
#' @examples
#' HSB2 <- SphB22(hardsphere, 1, 1, lower = 0, upper = Inf)
#' cat('HS B2:', HSB2, '\n')
SphB22 <- function(pot, beta, ..., lower = 0, upper = Inf) {
    # myfc <- function(x,eps,sig,rcut,beta) (exp(-pot(x,eps,sig,rcut)*beta)-1)*x^2 ##
    # works
    myfc <- u2pc(pot)
    s <- integrate(Vectorize(myfc), beta = beta, ... = ..., lower = lower, upper = upper)
    # s<-integrate(Vectorize(fx2), lower = 0, upper = Inf, sig=sig, beta=beta,
    # rcut=rcut, myf=pot) ## not work; possible myf is a function; error: object of
    # type 'closure' is not subsettable.
    -2 * pi * s$value
}

