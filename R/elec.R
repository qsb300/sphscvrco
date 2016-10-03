#' Potential Function for electrostatics in form of Debye–Huckel
#' 
#' @param r distance from center
#' @param q1 charges of A
#' @param q2 charges of B
#' @param kap kappa, 1/lamda, the Debye length
#' @export
#' @examples
#' hardsphere(0.5, 1.0)
#' hardsphere(1.5, 1.0)
DebyeHuckel <- function(r, q1, q2, kap) {
    q1 * q2 * exp(-kap * r)/r
}
