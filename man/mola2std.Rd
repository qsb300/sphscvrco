% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blz2b22.R
\name{mola2std}
\alias{mola2std}
\title{Convert mol/angstrom to Standard Unit M}
\usage{
mola2std(va3)
}
\arguments{
\item{va3}{in vol in angstrom for each molecule}
}
\value{
value in Standard Unit, M
}
\description{
C0, The standard concentration, ~ 1/1660 Å^−3 
6.022140857(74)×10^23 mol^−1 http://physics.nist.gov/cgi-bin/cuu/Value?na retrived 10/03/2016
M = mol/L = Na/cm^3 = 6.022140857e23/(1e27Å^3) = 1/1660.539
}
\examples{
mola2std(1.0) # ~ 1/1660==0.0006024096 
}

