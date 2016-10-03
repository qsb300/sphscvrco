#' Convert mol/angstrom to Standard Unit M
#' 
#' C0, The standard concentration, ~ 1/1660 Å^−3 
#' 6.022140857(74)×10^23 mol^−1 http://physics.nist.gov/cgi-bin/cuu/Value?na retrived 10/03/2016
#' M = mol/L = Na/cm^3 = 6.022140857e23/(1e27Å^3) = 1/1660.539
#' @param va3 in vol in angstrom for each molecule
#' @return value in Standard Unit, M
#' @export
#' @examples
#' mola2std(1.0) # ~ 1/1660==0.0006024096 
mola2std <- function(va3) {
    na <- 6.022140857e+23
    vL <- 1e+27
    va3 * na/vL
}

#' Convert Boltzmann Integartion into KA
#' 
#' @param blz sum from Boltzmann integartion
#' @param vol volume of whole integration space
#' @param dx gridspacing
#' @export
blz2KAMayerdx <- function(blz, vol, dx) {
    va3 <- (blz - vol) * dx^3
    mola2std(va3)
}

#' B22 in Standard Unit M
#' 
#' @param blz sum from Boltzmann integartion
#' @param vol volume of whole integration space
#' @param dx gridspacing
#' @export
#' @examples
#' blz2b22M(0.0,32/3*pi*27.241659^3,1.0) #203.9859, BSA, rg=27.241659^3
blz2b22M <- function(blz, vol, dx) {
    -0.5 * blz2KAMayerdx(blz, vol, dx)
}

# B22 in Experimental Unit

#' B22 in Experimental Unit, 10e-4 mol.ml/g/g
#' @param blz sum from Boltzmann integartion
#' @param vol volume of whole integration space
#' @param dx gridspacing
#' @param mass mol mass
#' @export
#' @examples
#' blz2b22(0.0,32/3*pi*27.241659^3,1.0,66391.13)  #0.4627858 BSA, rg=27.241659^3, mass=66391.13
blz2b22 <- function(blz, vol, dx, mass) {
    b22M <- blz2b22M(blz, vol, dx)
    # 1e3 for cm^3 and 1e4 for Bxe-4
    b22M/mass^2 * 1000 * 10000
}
