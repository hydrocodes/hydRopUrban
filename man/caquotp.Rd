\name{caquotp}
\alias{caquotp}
\title{caquotp: Method of Caquot for parallel drainage units}
\usage{
caquotp(data, a, b)
}
\description{
Maximal discharge by Caquot's method for parallel drainage units and equivalent values for unit drainage resulting.
}
\arguments{
data: A dataframe: containing drainage units parameters: Runoff coefficient (c), slope (S), area in Ha (A) and travel length in m (L)

a: A numeric value: Exponent from Montana IDF equation: a*Duration^b

b: A numeric value: Exponent from Montana IDF equation: a*Duration^b
}
\examples{
caquotp(data=database2, a=3.1, b=-0.64)
}
