#' Potential Function for Generized Lennard–Jones
#' 
#' @param r distance from center
#' @param eps epsilon, strength
#' @param sig sigma, size of sphere
#' @param offset, offset of sphere center
#' @keywords Generized Lennard-Jones glj
#' @export
#' @examples
#' glj(0.5, 1.0, 1.0, 0.1)
#' glj(1.5, 1.0, 1.0, 0.1)
glj <- function(r, eps, sig, offset) {
    x <- r - offset
    lj(x, eps, sig)
}

#' Force Function for Generized Lennard–Jones
#'
#' @param r distance from center
#' @param eps epsilon, strength
#' @param sig sigma, size of sphere
#' @param offset, offset of sphere center
gljforce <- function(r, eps, sig, offset) {
    x <- r - offset
    ljforce(x, eps, sig)
}

#' Potential Function for Generized Lennard–Jones with cutoff
#' 
#' @param r distance from center
#' @param eps epsilon, strength
#' @param sig sigma, size of sphere
#' @param offset, offset of sphere center
#' @param rcut cutoff
#' @keywords Generized Lennard-Jones glj
#' @export
#' @examples
#' gLJCut(0.5, 1.0, 1.0, 0.1, 2.5)
#' gLJCut(1.5, 1.0, 1.0, 0.1, 2.5)
gLJCut <- function(r, eps, sig, offset, rcut) {
    if (r < rcut) {
        glj(r, eps, sig, offset)
    } else {
        0
    }
}

#' Potential Function for Generized Lennard–Jones with Linear-Force shifted
#' 
#' @param r distance from center
#' @param eps epsilon, strength
#' @param sig sigma, size of sphere
#' @param rcut cutoff
#' @keywords Generized Lennard-Jones glj
#' @export
#' @examples
#' gLJShf(0.5, 1.0, 1.0, 0.1, 2.5)
#' gLJShf(1.5, 1.0, 1.0, 0.1, 2.5)
gLJShf <- function(r, eps, sig, offset, rcut) {
    vRc <- glj(rcut, eps, sig, offset)
    fRc <- gljforce(rcut, eps, sig, offset)
    if (r < rcut) {
        glj(r, eps, sig, offset) - vRc - fRc * (r - offset - rcut)
    } else {
        0
    }
}
