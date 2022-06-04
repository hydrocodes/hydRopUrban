#' @title rational
#' @description Discharge time series by the rational method
#' @param data A dataframe containing drainage units parameters: Runoff coefficient (c), intensity in mm/hr (i), area in km2 (A) and time of concentration in hr (Tc)
#' @param dt A numeric value: Time interval in hours
#' @return Maximal discharges by drainage unit, hydrograph plots and output file with discharge series in m3/s
#' @examples rational(data, dt)
#' @export
rational <- function(data,dt)
{ Qp <- (data$c)*(data$i)*(data$A)/3.6
Tc <- data$Tc
q1 <- list()
q3 <- list()
qls <- list()
for (h in 1:length(Qp)){
  q1[[h]] <- seq(0,Qp[h],dt*Qp[h]/Tc[h])
  q3[[h]] <- seq(Qp[h]-dt*Qp[h]/(Tc[h]),0,-dt*Qp[h]/(Tc[h]))
  qls[[h]] <- c(q1[[h]],q3[[h]])
}
n.obs <- sapply(qls, length)
seq.max <- seq_len(max(n.obs))
mat <- t(sapply(qls, "[", i = seq.max))
mat[is.na(mat)] <- 0
hydr <- colSums(mat)
x <- seq(0,dt*(length(hydr)-1),dt)
plot(x,hydr, type="l", lwd=2, xlab="Hours", ylab="Discharge (m3/s)")
for (g in 1:length(Qp)){
  lines(x,mat[g,], type="l", lty=2)
  print(max(mat[g,]))
}
df <- data.frame(Hours=x,Discharge=hydr)
write.table(df,file=output, sep = "\t", row.names = FALSE, col.names = TRUE)
sprintf("Qpeak = %f m3/s, Volume = %f m3", max(hydr), sum(hydr)*dt*60*60)
}

#' @title rationalm
#' @description Discharge time series by the modified rational method
#' @param data A dataframe containing drainage units parameters: Runoff coefficient (c), intensity in mm/hr (i), area in km2 (A) and time of concentration in hr (Tc)
#' @param D A numeric value: Event duration in hr
#' @param dt A numeric value: Time interval in hours
#' @return Maximal discharges by drainage unit, hydrograph plots and output file with discharge series in m3/s
#' @examples rationalm(data, D, dt)
#' @export
rationalm <- function(data,D,dt)
{ Qp <- (data$c)*(data$i)*(data$A)/3.6
Tc <- data$Tc
q1 <- list()
q2 <- list()
q3 <- list()
qls <- list()
j <- c()
D <- round(D,1)
for (h in 1:length(Qp)){
  if (D > Tc[h]) {
    q1[[h]] <- seq(0,Qp[h],dt*Qp[h]/Tc[h])
    j[h] <- round((D-Tc[h]),1)/dt
    q2[[h]] <- rep(Qp[h],j[h])
    q3[[h]] <- seq(Qp[h],0,-dt*Qp[h]/(1.5*Tc[h]))
    qls[[h]] <- c(q1[[h]],q2[[h]],q3[[h]])
  } else {
    if (D < Tc[h]){
      q1[[h]] <- seq(0,Qp[h]*D/Tc[h],dt*Qp[h]/Tc[h])
      q3[[h]] <- seq((Qp[h]*D/Tc[h])-dt*Qp[h]/(1.5*Tc[h]),0,-dt*Qp[h]/(1.5*Tc[h]))
      qls[[h]] <- c(q1[[h]],q3[[h]])
    }
    else {
      q1[[h]] <- seq(0,Qp[h],dt*Qp[h]/Tc[h])
      q3[[h]] <- seq(Qp[h]-dt*Qp[h]/(1.5*Tc[h]),0,-dt*Qp[h]/(1.5*Tc[h]))
      qls[[h]] <- c(q1[[h]],q3[[h]])
    }
  }
}
n.obs <- sapply(qls, length)
seq.max <- seq_len(max(n.obs))
mat <- t(sapply(qls, "[", i = seq.max))
mat[is.na(mat)] <- 0
hydr <- colSums(mat)
x <- seq(0,dt*(length(hydr)-1),dt)
plot(x,hydr, type="l", lwd=2, xlab="Hours", ylab="Discharge (m3/s)")
for (g in 1:length(Qp)){
  lines(x,mat[g,], type="l", lty=2)
  print(max(mat[g,]))
}
df <- data.frame(Hours=x,Discharge=hydr)
write.table(df,file=output, sep = "\t", row.names = FALSE, col.names = TRUE)
sprintf("Qpeak = %f m3/s, Volume = %f m3", max(hydr), sum(hydr)*dt*60*60)
}

#' @title rationalu
#' @description Discharge time series by the universal rational method
#' @param data A dataframe containing drainage units parameters: Runoff coefficient (c), intensity in mm/hr (i), area in Km2 (A) and time of concentration in hr (Tc)
#' @param dt A numeric value: Time interval in hours
#' @return Maximal discharges by drainage unit, hydrograph plots and output file with discharge series in m3/s
#' @examples rationalu(data, dt)
#' @export
rationalu <- function(data,dt)
{ Qp <- (data$c)*(data$i)*(data$A)/3.6
Tc <- data$Tc
q1 <- list()
q2 <- list()
q3 <- list()
q4 <- list()
q5 <- list()
q6 <- list()
q7 <- list()
q8 <- list()
q9 <- list()
q10 <- list()
q11 <- list()
qls <- list()
for (h in 1:length(Qp)){
  q1[[h]] <- seq(0,0.21*Qp[h],dt*0.21*Qp[h]/Tc[h])
  q2[[h]] <- seq(0.21*Qp[h]+dt*0.09*Qp[h]/Tc[h],0.3*Qp[h],dt*0.09*Qp[h]/Tc[h])
  q3[[h]] <- seq(0.3*Qp[h]+dt*0.7*Qp[h]/Tc[h],Qp[h],dt*0.7*Qp[h]/Tc[h])
  q4[[h]] <- seq(Qp[h]-dt*0.46*Qp[h]/Tc[h],0.54*Qp[h],-dt*0.46*Qp[h]/Tc[h])
  q5[[h]] <- seq(0.54*Qp[h]-dt*0.15*Qp[h]/Tc[h],0.39*Qp[h],-dt*0.15*Qp[h]/Tc[h])
  q6[[h]] <- seq(0.39*Qp[h]-dt*0.14*Qp[h]/Tc[h],0.25*Qp[h],-dt*0.14*Qp[h]/Tc[h])
  q7[[h]] <- seq(0.25*Qp[h]-dt*0.07*Qp[h]/Tc[h],0.18*Qp[h],-dt*0.07*Qp[h]/Tc[h])
  q8[[h]] <- seq(0.18*Qp[h]-dt*0.03*Qp[h]/Tc[h],0.15*Qp[h],-dt*0.03*Qp[h]/Tc[h])
  q9[[h]] <- seq(0.15*Qp[h]-dt*0.01*Qp[h]/Tc[h],0.14*Qp[h],-dt*0.01*Qp[h]/Tc[h])
  q10[[h]] <- seq(0.14*Qp[h]-dt*0.01*Qp[h]/Tc[h],0.13*Qp[h],-dt*0.01*Qp[h]/Tc[h])
  q11[[h]] <- seq(0.13*Qp[h]-dt*0.13*Qp[h]/Tc[h],0,-dt*0.13*Qp[h]/Tc[h])
  qls[[h]] <- c(q1[[h]],q2[[h]],q3[[h]],q4[[h]],q5[[h]],q6[[h]],q7[[h]],q8[[h]],q9[[h]],q10[[h]],q11[[h]])
}
n.obs <- sapply(qls, length)
seq.max <- seq_len(max(n.obs))
mat <- t(sapply(qls, "[", i = seq.max))
mat[is.na(mat)] <- 0
hydr <- colSums(mat)
x <- seq(0,dt*(length(hydr)-1),dt)
plot(x,hydr, type="l", lwd=2, xlab="Hours", ylab="Discharge (m3/s)")
for (g in 1:length(Qp)){
  lines(x,mat[g,], type="l", lty=2)
  print(max(mat[g,]))
}
df <- data.frame(Hours=x,Discharge=hydr)
write.table(df,file=output, sep = "\t", row.names = FALSE, col.names = TRUE)
sprintf("Qpeak = %f m3/s, Volume = %f m3", max(hydr), sum(hydr)*dt*60*60)
}

#' @title caquots
#' @description Maximal discharge by Caquot for serial drainage units
#' @param data A dataframe containing drainage units parameters: Runoff coefficient (c), slope (S), area in Ha (A) and travel length in m (L)
#' @param a A numeric value: Exponent from Montana IDF equation a*D^b
#' @param b A numeric value: Exponent from Montana IDF equation a*D^b
#' @return Maximal discharge by drainage unit, total discharge and equivalent values
#' @examples caquots(data,a,b)
#' @export
caquots <- function(data, a, b)
{ k <- (0.5^b)*a/6.6
u <- 1 + 0.287*b
v <- -0.41*b
w <- 0.95 + 0.507*b
E <- (data$L)/sqrt(data$A*10000)
m <- (E/2)^(0.7*b)
Qp <- m*(k^(1/u))*((data$S)^(v/u))*((data$c)^(1/u))*((data$A)^(w/u))
A <- sum(data$A)
c <- sum((data$c)*(data$A))/sum(data$A)
S <- (sum(data$L)/sum((data$L)/sqrt(data$S)))^2
Es <- sum(data$L)/sqrt(sum(data$A*10000))
ms <- (Es/2)^(0.7*b)
Qps <- ms*(k^(1/u))*(S^(v/u))*(c^(1/u))*(A^(w/u))
df <- data.frame(c,S,A,L=max(data$L))
print(Qp)
print(df)
sprintf("Qpeak = %f m3/s", Qps)
}

#' @title caquotp
#' @description Maximal discharge by Caquot for parallel drainage units
#' @param data A dataframe containing drainage units parameters: Runoff coefficient (c), slope (S), area in Ha (A) and travel length in m (L)
#' @param a A numeric value: Exponent from Montana IDF equation a*D^b
#' @param b A numeric value: Exponent from Montana IDF equation a*D^b
#' @return Maximal discharge by drainage unit, total discharge and equivalent values
#' @examples caquotp(data,a,b)
#' @export
caquotp <- function(data, a, b)
{ k <- (0.5^b)*a/6.6
u <- 1 + 0.287*b
v <- -0.41*b
w <- 0.95 + 0.507*b
E <- (data$L)/sqrt(data$A*10000)
m <- (E/2)^(0.7*b)
Qp <- m*(k^(1/u))*((data$S)^(v/u))*((data$c)^(1/u))*((data$A)^(w/u))
A <- sum(data$A)
c <- sum((data$c)*(data$A))/sum(data$A)
S <- sum((data$S)*Qp)/sum(Qp)
df1 <- data.frame(L=data$L,Qp)
Es <- df1$L[df1$Qp==max(Qp)]/sqrt(sum(data$A*10000))
ms <- (Es/2)^(0.7*b)
Qps <- ms*(k^(1/u))*(S^(v/u))*(c^(1/u))*(A^(w/u))
df <- data.frame(c,S,A,L=sum(data$L))
print(Qp)
print(df)
sprintf("Qpeak = %f m3/s", Qps)
}

#' @title mcunge
#' @description Hydrograph routing through an open urban drainage channel
#' @param inflow A vector: Input hydrograph in m3/s
#' @param Qo A numeric value: Reference discharge in m3/s
#' @param To A numeric value: Reference open width in m
#' @param Vo A numeric value: Reference velocity in m/s
#' @param L A numeric value: Travel length in m
#' @param m A numeric value: Q-A relationship exponent of the hydraulic section Q=e*A^m
#' @param dt A numeric value: Time interval in hours
#' @param init A numeric value: Initial outflow discharge
#' @return Routed hydrograph and discharge time series
#' @examples mcunge(inflow, Qo, To, Vo, L, m, dt, init)
#' @export
mcunge <- function(inflow, Qo, To, Vo, L, m, dt, init){
  x <- 0.5*(1-(Qo/To)/(So*m*Vo*L))
  K <- L/(m*Vo)
  c0 <- (-K*x + 0.5*dt*3600)/(K - K*x + 0.5*dt*3600)
  c1 <- (K*x + 0.5*dt*3600)/(K - K*x + 0.5*dt*3600)
  c2 <- (K - K*x - 0.5*dt*3600)/(K - K*x + 0.5*dt*3600)
  outflow <- rep(0, length(inflow))
  outflow[1] <- init
  for (i in 2:length(inflow)){
    outflow[i] <- c0*inflow[i] + c1*inflow[i-1] + c2*outflow[i-1]
  }
  time <- seq(0,(length(inflow)-1)*dt,dt)
  df <- data.frame(Hours=time, Outflow=outflow)
  write.table(df,file=output, sep = "\t", row.names = FALSE, col.names = TRUE)
  plot(time,inflow,type="l", lwd=2, xlab="Hours", ylab="Discharge (m3/s)")
  lines(time,outflow,type="l", lwd=2, lty=3)
  sprintf("Qpeak_in = %f m3/s, Qpeak_out = %f m3/s", max(inflow), max(outflow))
}

#' @title convex
#' @description Hydrograph routing through a wide open channel
#' @param inflow A vector: Input hydrograph in m3/s
#' @param Qo A numeric value: Output initial discharge in m3/s
#' @param B A numeric value: channel width in m
#' @param L A numeric value: channel length in m
#' @param S A numeric value: channel slope in m/m
#' @param n A numeric value: Manning's roughness coefficient
#' @param dt A numeric value: Time interval in hours
#' @return Routed hydrograph and discharge time series
#' @examples convex(inflow, Qo, B, L, S, n, dt)
#' @export
convex <- function (inflow, Qo, B, L, S, n, dt)
{ Qpk <- max(inflow)
  Q34 <- 3*Qpk/4
  y34 <- (Q34*n)/(B*S^0.5)^0.6
  CX <- 1.5*y34^(2/3)*S^0.5*dt*60*60/(n*L)
  outflow <- rep(0, length(inflow))
  outflow[1] <- Qo
  for (i in 2:length(inflow)){
    outflow[i] <- CX*inflow[i-1] + (1-CX)*outflow[i-1]
  }
  time <- seq(0,(length(inflow)-1)*dt,dt)
  df <- data.frame(Hours=time, Outflow=outflow)
  write.table(df,file=output, sep = "\t", row.names = FALSE, col.names = TRUE)
  plot(time,inflow,type="l", lwd=2, xlab="Hours", ylab="Discharge (m3/s)")
  lines(time,outflow,type="l", lwd=2, lty=3)
  sprintf("Qpeak_in = %f m3/s, Qpeak_out = %f m3/s", max(inflow), max(outflow))
}

#' @title pollutant
#' @description Estimation of pollutant discharge and concentration by wash-off from impervious areas.
#' @param inflow A vector: Input hydrograph in m3/s
#' @param A A numeric value: Unit drainage area in km²
#' @param w A numeric value: Initial amount of solids accumulated prior to the rain in kg
#' @param k A numeric value: Wash-off coefficient in 1/mm
#' @param dt A numeric value: Time interval in hours
#' @return Maximal pollutant discharge and minimal pollutant concentration, pollutegraphs and output file with time series in m3/s
#' @examples pollutant(inflow, A, w, k, dt)
#' @export
pollutant <- function(inflow, A, w, k, dt)
{ x <- seq(0,dt*(length(inflow)-1),dt)
  r1 <- inflow*1000*3600/(A*1000000)
  ra <- c()
  deltaV <- c()
  p1 <- c()
  p1[1] <- w
  deltaP <- c()
  for (i in 1:length(inflow)) {
    ra[i] <- (r1[i]+r1[i+1])/2
    ra[length(inflow)] <- r1[length(inflow)]/2
    deltaV[i] <- (inflow[i]+inflow[i+1])*dt*3600/2
    deltaV[length(inflow)] <- inflow[length(inflow)]*dt*3600/2
    p1[i+1] <- p1[i]*exp(-k*ra[i]*dt)
    deltaP[i] <- p1[i]-p1[i+1]
  }
  C <- deltaP*1000/deltaV
  W <- deltaP/dt
  layout(matrix(c(1,2), nrow = 1), widths = c(1, 1), )
  par(mar=c(4,4,2,4))
  print(plot(x,W, type="l", lwd=2, xlab=" ", ylab=" ", col="black"))
  axis(side=2, col="black")
  mtext("Pollutant discharge (kg/hr)",side=2, col="black", line=2.5, font=2)
  mtext("Hours", side=1, line=2)
  par(new=TRUE)
  print(plot(x,inflow, type="l", axes=FALSE, lwd=1, lty=3, xlab=" ", ylab=" "))
  axis(4, col="black" )
  mtext("Discharge (m3/s)", side=4, line=2)

  print(plot(x,C, type="l", lwd=2, xlab=" ", ylab=" ", col="black"))
  axis(side=2, col="black")
  mtext("Pollutant concentration (mg/l)",side=2, col="black", line=2.5, font=2)
  mtext("Hours", side=1, line=2)
  par(new=TRUE)
  print(plot(x,inflow, type="l", axes=FALSE, lwd=1, lty=3, xlab=" ", ylab=" "))
  axis(4, col="black" )
  mtext("Discharge (m3/s)", side=4, line=2)

  df <- data.frame(Hours=x, Discharge=inflow, Pollutant_discharge=W, Pollutant_concentration=C)
  write.table(df,file=output, sep = "\t", row.names = FALSE, col.names = TRUE)
  sprintf("Ppeak = %f kg/hr, Cmin = %f mg/l", max(W), min(C))
}

#' @title pollutantp
#' @description Estimation of pollutant discharge and concentration by wash-off from impervious areas.
#' @param int30 A vector: Input 30-min intensities in mm/hr
#' @param A A numeric value: Unit drainage area in km²
#' @param K A numeric value: Soil erodibility factor in tonnes/acre or tons/ha
#' @param L A numeric value: Flow length in m
#' @param S A numeric value: Slope in the flow direction in m/m
#' @param C A numeric value: Cropping management factor
#' @param Pf A numeric value: Erosion control practice factor
#' @return Maximal pollutant discharge and total load, pollutegraph and output file with time series in kg/hr
#' @examples pollutantp(int30, A, K, L, S, C, Pf)
#' @export
pollutantp <- function(int30, A, K, L, S, C, Pf)
{ x <- seq(0,0.5*(length(int30)-1),0.5)
  E <- (9.16+3.31*log10(int30/25.4))*(int30/25.4)*0.5
  i30 <- max(int30/25.4)
  R <- E*i30
  Ls <- (L*3.281)^0.5*(0.0076+0.53*S+0.76*S^2)
  deltaP <- (2000/2.205)*(A*247.105)*R*K*Ls*C*Pf
  W <- deltaP/0.5
  par(mar=c(4,4,2,4))
  print(plot(x,W, ylim = range(2*W), type="l", lwd=2, xlab=" ", ylab=" ", col="black"))
  par(new = TRUE)
  print(plot(x,int30, ylim = rev(range(2*int30)), type="h", axes=FALSE, lwd=1, xlab=" ", ylab=" ", col="black"))
  axis(side=4)
  mtext("Pollutant discharge (kg/hr)",side=2, col="black", line=2.5, font=2)
  mtext("Intensity (mm/hr)",side=4, col="black", line=2.5, font=1)
  mtext("Hours", side=1, line=2)

  df <- data.frame(Hours=x, Pollutant_discharge=W)
  write.table(df,file=output, sep = "\t", row.names = FALSE, col.names = TRUE)
  sprintf("Ppeak = %f kg/hr, Total load = %f kg", max(W), sum(W)*0.5)
}

#' @title idf24
#' @description Estimation of Intensity-Duration-Frequency adjustment from SCS storms
#' @param P24 A numeric value: Input design precipitation of 24hr in mm
#' @param type An option: SCS storm type option (1,1a,2,3)
#' @return Montana Intensity-Duration-Frequency equation and curve
#' @examples idf24(P24, type)
#' @export
idf24 <- function(P24, type)
{ scs1 <- c(0,0.0175,0.035,0.0555,0.076,0.1005,
            0.125,0.156,0.194,0.254,0.515,0.624,
            0.682,0.727,0.767,0.7985,0.83,0.854,
            0.878,0.902,0.926,0.9445,0.963,0.9815,1)
  scs1a <- c(0,0.025,0.05,0.083,0.116,0.161,0.206,
           0.268,0.425,0.52,0.577,0.624,0.664,
           0.701,0.736,0.768,0.8,0.8265,0.853,
           0.8795,0.906,0.9295,0.953,0.9765,1)
  scs2 <- c(0,0.011,0.022,0.035,0.048,0.064,0.08,
          0.098,0.12,0.147,0.181,0.235,0.663,
          0.772,0.82,0.85,0.88,0.898,0.916,0.934,
          0.952,0.964,0.976,0.988,1)
  scs3 <- c(0,0.01,0.02,0.0315,0.043,0.0575,0.072,
          0.089,0.115,0.148,0.189,0.25,0.5,0.751,
          0.811,0.8485,0.886,0.90375,0.9215,0.93925,
          0.957,0.96775,0.9785,0.98925,1)
if(type=="1") {
  pacm <- P24*scs1
  p <- c()
  int2 <- c()
  int3 <- c()
  int4 <- c()
  for (i in 1:24) {
    p[i] <- pacm[i+1]-pacm[i]
    int2[i] <- (p[i+2]+p[i+1])/2
    int3[i] <- (p[i+3]+p[i+2]+p[i+1])/3
    int4[i] <- (p[i+4]+p[i+3]+p[i+2]+p[i+1])/4
  }
}
else if(type=="1a") {
  pacm <- P24*scs1a
  p <- c()
  int2 <- c()
  int3 <- c()
  int4 <- c()
  for (i in 1:24) {
    p[i] <- pacm[i+1]-pacm[i]
    int2[i] <- (p[i+2]+p[i+1])/2
    int3[i] <- (p[i+3]+p[i+2]+p[i+1])/3
    int4[i] <- (p[i+4]+p[i+3]+p[i+2]+p[i+1])/4
  }
}
else if(type=="2"){
  pacm <- P24*scs2
  p <- c()
  int2 <- c()
  int3 <- c()
  int4 <- c()
  for (i in 1:24) {
    p[i] <- pacm[i+1]-pacm[i]
    int2[i] <- (p[i+2]+p[i+1])/2
    int3[i] <- (p[i+3]+p[i+2]+p[i+1])/3
    int4[i] <- (p[i+4]+p[i+3]+p[i+2]+p[i+1])/4
  }
}
else if(type=="3"){
  pacm <- P24*scs3
  p <- c()
  int2 <- c()
  int3 <- c()
  int4 <- c()
  for (i in 1:24) {
    p[i] <- pacm[i+1]-pacm[i]
    int2[i] <- (p[i+2]+p[i+1])/2
    int3[i] <- (p[i+3]+p[i+2]+p[i+1])/3
    int4[i] <- (p[i+4]+p[i+3]+p[i+2]+p[i+1])/4
  }
}
else
stop(sQuote(x), " not implemented")
ps <- max(p[!is.na(p)])
int2s <- suppressWarnings(max(int2[!is.na(int2)]))
int3s <- suppressWarnings(max(int3[!is.na(int3)]))
int4s <- suppressWarnings(max(int4[!is.na(int4)]))
int24 <- P24/24
int24max <- c(ps, int2s, int3s, int4s, int24)
D <- c(1,2,3,4,24)
model <- suppressWarnings(lm(log(int24max) ~ log(D)))
a <- exp(model$coefficients[1])
b <- model$coefficients[2]
x <- c(1:24)
y <- a*x^b
plot(x,y,type="l", lwd=2, xlab="Duration (hr)", ylab="Intensity (mm/hr)")
sprintf("Intensity = a*Duration^b: a = %f, b = %f", a, b)
}
