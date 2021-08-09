\name{rationalm}
\alias{rationalm}
\title{rationalm: Modified rational method}
\usage{
rationalm(data, D, dt)
}
\description{
Estimation of discharges time series by the modified rational method with a triangular hydrograph with a recession of 1.5xTc, and considering a storm event duration.
}
\arguments{
data: A dataframe: containing drainage units parameters: Runoff coefficient (c), intensity in mm/hr (i), area in km2 (A) and time of concentration in hr (Tc)

D: A numeric value: Event duration in hr

dt: A numeric value: Time interval in hours for plotting (e.g. 3-min as 0.05)
}
\examples{
rationalm(data=database1, D=1, dt=0.05)
}
