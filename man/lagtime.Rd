\name{lagtime}
\alias{lagtime}
\title{lagtime: Estimation of time concentration and lag time}
\usage{
lagtime(longitud, area, slope, altitudiff, path)
}
\description{
Estimation of Intensity-Duration-Frequency adjustment from SCS storms.
}
\arguments{
longitud A numeric value: Input design longitud in km
area A numeric value: Input design area in km2
slope A numeric value: Input design slope in percentage
altitudiff A numeric value: Input design altitudiff in m
path An option: path
}
\examples{
lagtime(area = 0.107, longitud = 0.115, slope = 7.414, altitudiff = 9.45)
}
