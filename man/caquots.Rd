\name{caquots}
\alias{caquots}
\title{caquots: Method of Caquot for serial drainage units}
\usage{
caquots(data, a, b)
}
\description{
Maximal discharge by Caquot's method for serial drainage units and equivalent values for unit drainage resulting.
}
\arguments{
data: A dataframe: containing drainage units parameters: Runoff coefficient (c), slope (S), area in Ha (A) and travel length in m (L)

a: A numeric value: Exponent from Montana IDF equation: a*Duration^b

b: A numeric value: Exponent from Montana IDF equation: a*Duration^b
}
\examples{
caquots(data=database2, a=3.1, b=-0.64)
}
