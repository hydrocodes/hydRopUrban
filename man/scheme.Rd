\name{scheme}
\alias{scheme}
\title{scheme: Basin drainage scheme}
\usage{
scheme(crunoff, intensity, area, type, S, n, B, z1, z2, D)
}
\description{
Basin drainage scheme by rational method and uniform flow in open channels
}
\arguments{
crunoff: A numeric value of Runoff coefficient
intensity: A numeric value of intensity in mm/hr
area: A numeric value of area in km2
type: A character: select method "rtg" for rectangular section, "tpz" for trapezoidal section, "trg" for triangular section, "crc" for circular section
S A numeric value of channel or ditch slope in m/m
n A numeric value of Manning rugosity of channel or ditch
B A numeric value of channel or ditch width in m
z1 A numeric value of cut slope of channel or ditch section 1V:(z1)H
z2 A numeric value of cut slope of channel or ditch section 1V:(z2)H
D A numeric value of circular section or pipe diameter in m
}
\examples{
scheme(area=0.1, crunoff=0.7, intensity=10, type="rt", S=0.01, n=0.012, B=0.5, z1=1, z2=1, D=0.5)
}
