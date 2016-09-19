#' Potential Function for Hard Sphere
#' 
#' @param r distance from center
#' @param sig sigma, size of sphere
#' @keywords hardsphere
#' @export
#' @examples
#' hardsphere(0.5, 1.0)
#' hardsphere(1.5, 1.0)
hardsphere <- function(r, sig) {
    if (r < sig) {
        Inf
    } else {
        0
    }
}

#' Wrapper of Potential Function for Hard Sphere
#'
#' This is a wrapper over hardsphere to called by SphB22
#' @param r distance from center
#' @param eps epsilon, ignored
#' @param sig sigma, size of sphere
#' @param rcut cutoff, ignored
#' @param pot potential
#' @keywords hardsphere
#' @export
#' @examples
#' HardSph(0.5, 1.0, 1.0, 100)
#' HardSph(1.5, 1.0, 1.0, 100)
HardSph <- function(r, eps, sig, rcut) {
    hardsphere(r, sig)
}

