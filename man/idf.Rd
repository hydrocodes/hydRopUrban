\name{idf}
\alias{idf}
\title{idf: Intensity-Duration-Frequency curve from 24hr SCS storms}
\usage{
idf(precipitation, type, path)
}
\description{
Estimation of Intensity-Duration-Frequency adjustment from SCS storms.
}
\arguments{
precipitation A numeric value: Input design precipitation of 24hr in mm

type: An option: SCS storm type option ("1","1a","2","3")

path An option: path
}
\examples{
idf(precipitation = 48, type = '1')
}
