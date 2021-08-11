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
