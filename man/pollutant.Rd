\name{pollutant}
\alias{pollutant}
\title{pollutant: Empirical pollutant discharge and concentration by wash-off from impervious and impervious surfaces}
\usage{
pollutant(flow.in, intensity, area, solids, kc, kt, delta.time, longitud, slope, coefficient, param, method, path)
}
\description{
Estimation of pollutant discharge and concentration by wash-off from impervious and impervious surfaces.
}
\arguments{
flow.in: A vector: Input hydrograph in m3/s
area: A numeric value: Unit drainage area in km²
solids: A numeric value: Initial amount of solids accumulated prior to the rain in kg
kc: A numeric value: Wash-off coefficient in 1/mm
intensity: A vector: Input 30-min intensities in mm/hr
kt: A numeric value: Soil erodibility factor in tonnes/acre or tons/ha
longitud: A numeric value: Flow length in m
slope: A numeric value: Slope in the flow direction in m/m
coefficient: A numeric value: Cropping management factor
param: A numeric value: Erosion control practice factor
delta.time: A numeric value: Time interval in hours
method: A character: select method
path: An option: path
}
\examples{
pollutant(flow.in = inflow, intensity = c(7.62,12.7,33.02,10.16,7.62,5.08), area = 0.2023, solids = 2131.88, kc = 0.1811, kt = 0.27, delta.time = 0.05, longitud=122, slope = 0.001, coefficient=0.003, param = 1, method = 'impervious')
}
