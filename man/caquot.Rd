\name{caquot}
\alias{caquot}
\title{caquot: Method of Caquot for parallel and serial drainage units}
\usage{
caquot(crunoff, slope, area, longitud, a, b, method, path)
}
\description{
Maximal discharge by Caquot's method for parallel drainage units and equivalent values for unit drainage resulting.
}
\arguments{
crunoff: A vector containing drainage units parameters: Runoff coefficient (c)
slope: A vector containing drainage units parameters: slope (S)
area: A vector containing drainage units parameters: area in Ha (A)
longitud: A vector containing drainage units parameters: travel length in m (L)
a: A numeric value: Exponent from Montana IDF equation a*D^b
b: A numeric value: Exponent from Montana IDF equation a*D^b
method: A character: select method "parallel", "serial"
path: An option: path
}
\examples{
caquot(crunoff = 0.82, slope = 0.02, area = 200.21, longitud = 1300, a = 5, b = -0.6, method = 'parallel', path = NA)
}
