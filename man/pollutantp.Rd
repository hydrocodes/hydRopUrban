\name{pollutantp}
\alias{pollutantp}
\title{pollutantp: Empirical pollutant discharge by wash-off from pervious surfaces}
\usage{
pollutantp(int30, A, K, L, S, C, Pf)
}
\description{
Estimation of pollutant discharge by wash-off from pervious surfaces.
}
\arguments{
int30: A vector: Input 30-min intensities in mm/hr
A: A numeric value: Unit drainage area in km²
K: A numeric value: Soil erodibility factor in tonnes/acre or tons/ha
L: A numeric value: Flow length in m
S: A numeric value: Slope in the flow direction in m/m
C: A numeric value: Cropping management factor
Pf: A numeric value: Erosion control practice factor
}
\examples{
pollutantp(int30, A=0.0121, K=0.27, L=122, S=0.001, C=0.003, Pf=1)
}
