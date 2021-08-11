\name{rational}
\alias{rational}
\title{rational: Standard rational method}
\usage{
rational(data, dt)
}
\description{
Estimation of discharges time series by the standard rational method with a triangular hydrograph with a recession of 1xTc, without considering a storm duration
}
\arguments{
data: A dataframe: containing drainage units parameters: Runoff coefficient (c), intensity in mm/hr (i), area in km2 (A) and time of concentration in hr (Tc)

dt: A numeric value: Time interval in hours for plotting (e.g. 3-min as 0.05)
}
\examples{
rational(data=database1, dt=0.05)
}
