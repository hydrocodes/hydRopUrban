\name{lagtime}
\alias{lagtime}
\title{lagtime: Estimation of time of concentration and lag time}
\usage{
lagtime(longitud, area, slope, altitudiff, path)
}
\description{
Estimation of time of concentration and lag time with 18 empirical methods.
}
\arguments{
area: A numeric value: Input design area in km2

longitud: A numeric value: Input design longitud in km

slope: A numeric value: Input design slope in percentage

altitudiff: A numeric value: Input design altitudiff in m

id: a character value: ID or name subbassin

path: An option: path

load_plot: A logical value: T or F 
}
\examples{
lagtime(area = 0.107, longitud = 0.115, slope = .414, altitudiff = 9.45, id = 1, path = NA, load_plot =T)
}
