#' Potential Function for Lennard–Jones
#' 
#' @param r distance from center
#' @param eps epsilon, strength
#' @param sig sigma, size of sphere
#' @keywords Lennard-Jones lj
#' @export
#' @examples
#' lj(0.5, 1.0, 1.0)
#' lj(1.5, 1.0, 1.0)
lj <- function(r, eps, sig) {
    4 * eps * ((sig/r)^12 - (sig/r)^6)
}

#' Force Function for Lennard–Jones
#'
#' @param r distance from center
#' @param eps epsilon, strength
#' @param sig sigma, size of sphere
#' @export
ljforce <- function(r, eps, sig) {
    sr <- sig/r
    sr2 <- sr * sr
    sr6 <- sr2 * sr2 * sr2
    -48 * eps * sr6 * (sr6 - 0.5)/r
}

#' Wrapper of Potential Function for Lennard–Jones
#'
#' This is a wrapper over lj to called by SphB22
#' @param r distance from center
#' @param eps epsilon, strength
#' @param sig sigma, size of sphere
#' @param rcut cutoff, ignored
#' @keywords Lennard-Jones lj
#' @export
#' @examples
#' LJ(0.5, 1.0, 1.0, 2.5)
#' LJ(1.5, 1.0, 1.0, 2.5)
LJ <- function(r, eps, sig, rcut) {
    lj(r, eps, sig)
}

#' Potential Function for Lennard–Jones with cutoff
#' 
#' @param r distance from center
#' @param eps epsilon, strength
#' @param sig sigma, size of sphere
#' @param rcut cutoff
#' @keywords Lennard-Jones lj
#' @export
#' @examples
#' LJCut(0.5, 1.0, 1.0, 2.5)
#' LJCut(1.5, 1.0, 1.0, 2.5)
LJCut <- function(r, eps, sig, rcut) {
    if (r < rcut) {
        lj(r, eps, sig)
    } else {
        0
    }
}

#' Potential Function for Lennard–Jones with Linear-Force shifted
#' 
#' @param r distance from center
#' @param eps epsilon, strength
#' @param sig sigma, size of sphere
#' @param rcut cutoff
#' @keywords Lennard-Jones lj
#' @export
#' @examples
#' LJShf(0.5, 1.0, 1.0, 2.5)
#' LJShf(1.5, 1.0, 1.0, 2.5)
LJShf <- function(r, eps, sig, rcut) {
    vRc <- lj(rcut, eps, sig)
    fRc <- ljforce(rcut, eps, sig)
    if (r < rcut) {
        lj(r, eps, sig) - vRc - fRc * (r - rcut)
    } else {
        0
    }
}

