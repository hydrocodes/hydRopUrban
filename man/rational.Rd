\name{rational}
\alias{rational}
\title{rational: Standard, modified and universal rational method}
\usage{
rational(crunoff, intensity, area, time.con, duration, method, delta.time, path)
}
\description{
Estimation of discharges time series by the standard, modified and universal rational method with a triangular hydrograph with a recession of 1xTc, without considering a storm duration
}
\arguments{
crunoff: A dataframe containing drainage units parameters: Runoff coefficient (c)
intensity: A dataframe containing drainage units parameters: intensity in mm/hr (i)
area: A dataframe containing drainage units parameters: area in km2 (A)
time.con: A dataframe containing drainage units parameters: time of concentration in hr (Tc)
duration: A numeric value: Event duration in hr
delta.time: A numeric value: Time interval in hours (e.g. 3-min as 0.05)
method: A character: select method "standard", "modified", "universal"
path: An option: path
}
\examples{
rational(crunoff = 0.95, intensity = 2.9, area = 0.57, time.con = 0.91, duration = 2, method = 'standard', delta.time = 0.05, path = NA)
}
