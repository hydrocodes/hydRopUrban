\name{rationalu}
\alias{rationalu}
\title{rationalu: Universal rational method}
\usage{
rationalu(data, dt)
}
\description{
Estimation of discharges time series by the universal rational method with a synthetic hydrograph for urban catchments.
}
\arguments{
data: A dataframe: containing drainage units parameters: Runoff coefficient (c), intensity in mm/hr (i), area in km2 (A) and time of concentration in hr (Tc)

dt: A numeric value: Time interval in hours for plotting (e.g. 3-min as 0.05)
}
\examples{
rationalu(data=database1, dt=0.05)
}
