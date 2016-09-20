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

