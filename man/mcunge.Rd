\name{mcunge}
\alias{mcunge}
\title{mcunge: Method of Muskingum-Cunge for urban channel routing}
\usage{
mcunge(inflow, Qo, To, Vo, L, m, dt, init)
}
\description{
Outflow discharge through a channel routing by the method of Muskingum-Cunge.
}
\arguments{
inflow: A vector: Input hydrograph in m3/s

Qo: A numeric value: Reference discharge in m3/s

To: A numeric value: Reference open width in m

Vo: A numeric value: Reference velocity in m/s

L: A numeric value: Travel length in m

m: A numeric value: Q-A relationship exponent of the hydraulic section Q=e*A^m

dt: A numeric value: Time interval same as inflow in hours

init: A numeric value: Initial outflow discharge in m3/s
}
\examples{
mcunge(inflow=database3, Qo, To, Vo, L, m=1.33, dt=0.05, init)
}
