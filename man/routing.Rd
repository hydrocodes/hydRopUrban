\name{routing}
\alias{routing}
\title{routing: Empirical hydrograph routing by the Convex and MCunge method}
\usage{
routing(inflow, Qo, B, L, S, n, dt)
}
\description{
Estimation of routed hydrograph and peak flows by the Convex and MCunge method.
}
\arguments{
flow.in: A vector: Input hydrograph in m3/s
flow.ref: A numeric value: Reference discharge in m3/s
flow.init: A numeric value: initial discharge in m3/s
channel: A numeric value: Reference open width in m
velocity: A numeric value: Reference velocity in m/s
longitud: A numeric value: Travel length in m
slope: A numeric value: channel slope in m/m
flow.area: A numeric value: Q-A relationship exponent of the hydraulic section Q=e*A^m
cmanning: A numeric value: Manning's roughness coefficient
delta.time: A numeric value: Time interval in hours
method: A character: select method "convex", "mcunge"
path: An option: path
}
\examples{
routing(flow.in = inflow, flow.ref = 1.5, velocity = 2.1, slope = 0.01, channel = 20, longitud =  2500, flow.area = 1.31, flow.init = 0, delta.time = 0.05, method = 'mcunge')
}
