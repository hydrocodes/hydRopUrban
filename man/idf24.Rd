\name{idf24}
\alias{idf24}
\title{idf24: Intensity-Duration-Frequency curve from 24hr SCS storms}
\usage{
indf24(P24, type)
}
\description{
Estimation of Intensity-Duration-Frequency adjustment from SCS storms.
}
\arguments{
P24 A numeric value: Input design precipitation of 24hr in mm

type: An option: SCS storm type option ("1","1a","2","3")
}
\examples{
idf24(P24, type="2")
}
