\name{convex}
\alias{convex}
\title{convex: Empirical hydrograph routing by the Convex method}
\usage{
convex(inflow, Qo, B, L, S, n, dt)
}
\description{
Estimation of routed hydrograph and peak flows by the Convex method.
}
\arguments{
inflow: A vector: Input hydrograph in m3/s

Qo: A numeric value: initial output discharge in m3/s

B: A numeric value: channel width in m

L: A numeric value: channel length in m

S: A numeric value: channel slope in m/m

n: A numeric value: Manning's roughness coefficient

dt: A numeric value: Time interval in hours
}
\examples{
convex(inflow=database4, Qo=0, B, L, S, n=0.035, dt=0.1)
}
