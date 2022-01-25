\name{pollutant}
\alias{pollutant}
\title{pollutant: Empirical pollutant discharge and concentration by wash-off from impervious surfaces}
\usage{
pollutant(inflow, A, w, k, dt)
}
\description{
Estimation of pollutant discharge and concentration by wash-off from impervious surfaces.
}
\arguments{
inflow: A vector: Input hydrograph in m3/s

A: A numeric value: Unit drainage area in km2

w: A numeric value: Initial amount of solids accumulated prior to the rain in kg

k: A numeric value: Wash-off coefficient in 1/mm

dt: A numeric value: Time interval same as inflow in hours
}
\examples{
pollutant(inflow=database4, A, w, k=0.18, dt=0.05)
}
