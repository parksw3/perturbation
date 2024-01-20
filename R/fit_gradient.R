sus_reconstruct <- function(cases,
                            birth) {
  regfit <- loess(cumsum(birth)~cumsum(cases))
  
  Z <- regfit$residuals
  
  Z
}

fit_gradient <- function(cases,
                         birth,
                         week,
                         N,
                         cutoff,
                         nstep=10) {
  time <- 1:length(cases)
  
  dd <- data.frame(
    cases=cases,
    birth=birth,
    time=time,
    week=week
  )
  
  Z <- sus_reconstruct(cases, birth)
  rho <- coef(lm(cumsum(birth)~cumsum(cases)))[[2]]
  
  logI <- log(dd$cases)
  
  gfit <- lm(log(cases)~ns(time, knots=seq(1, nrow(dd), by=6)), data=dd)
  
  S0vec <- seq(0.001, 0.02, by=0.00025)
  llvec <- rep(NA, length(S0vec))
  # llvec2 <- rep(NA, length(S0vec))
  # S0estvec <- rep(NA, length(S0vec))
  
  for (j in 1:length(S0vec)) {
    fitdata <- data.frame(
      grad=(predict(gfit, newdata = data.frame(time=1:(nrow(dd))+0.1))-predict(gfit))/0.1+1+birth/N,
      logS=log(S0vec[j]*N + Z),
      week=dd$week,
      time=1:nrow(dd),
      change=as.factor((time-1)/52+2013<cutoff)
    )
    
    if (any(!is.finite(fitdata$logS))) {
      llvec[j] <- -Inf
      # llvec2[j] <- Inf
    } else {
      
      tempdata <- fitdata[fitdata$week==52,]
      tempdata$week <- 0
      
      fitdata <- rbind(fitdata, tempdata)
      
      fitdata$offterm <- fitdata$logS - log(N)
      
      lfit <- gam(grad ~ change + s(week, bs=c("cc"), by=change) + offset(offterm), data=fitdata,
                  family = gaussian("log"))
      
      beta <- exp(predict(lfit, newdata=data.frame(week=week, change=(time-1)/52+2013<2016, offterm=0)))
      
      # betafun <- approxfun(dd$time, beta)
      # mufun <- approxfun(dd$time, dd$birth/N)
      
      # S0ini <- (S0vec[j]*N + Z)[1]
      
      # oo <- optim(S0ini, function(x) {
      #   deout <- runsir(gamma=1, N=N, S0=x, I0=cases[1]*rho, betafun=betafun, mufun=mufun)
      #   
      #   sum((log(deout$I/rho)-log(cases))^2, na.rm=TRUE)
      # }, method="Brent", lower=0.5*S0ini, upper=2*S0ini)
      
      # S0estvec[j] <- oo$par
      
      # deout <- runsir(gamma=1, N=N, S0=oo$par, I0=cases[1]*rho, betafun=betafun, mufun=mufun)
      
      llvec[j] <- logLik(lfit)
      # llvec2[j] <- sum((log(deout$I/rho)-log(cases))^2, na.rm=TRUE)
    }
  }
  
  j <- which.max(llvec)
  
  fitdata <- data.frame(
    grad=(predict(gfit, newdata = data.frame(time=1:(nrow(dd))+0.1))-predict(gfit))/0.1+1+birth/N,
    logS=log(S0vec[j]*N + Z),
    week=dd$week,
    time=1:nrow(dd),
    change=as.factor((time-1)/52+2013<cutoff)
  )
  
  tempdata <- fitdata[fitdata$week==52,]
  tempdata$week <- 0
  
  fitdata <- rbind(fitdata, tempdata)
  
  fitdata$offterm <- fitdata$logS - log(N)
  
  lfit <- gam(grad ~ change + s(week, bs=c("cc"), by=change) + offset(offterm), data=fitdata,
              family = gaussian("log"))
  
  beta <- exp(predict(lfit, newdata=data.frame(week=week, change=(time-1)/52+2013<2016, offterm=0)))
  
  betafun <- approxfun(dd$time, beta)
  mufun <- approxfun(dd$time, dd$birth/N)
  
  S0ini <- (S0vec[j]*N + Z)[1]
  
  oo <- optim(S0ini, function(x) {
    deout <- runsir(gamma=1, N=N, S0=x, I0=cases[1]*rho, betafun=betafun, mufun=mufun)
    
    sum((log(deout$I/rho)-log(cases))^2, na.rm=TRUE)
  }, method="Brent", lower=0.5*S0ini, upper=2*S0ini)
  
  # deout <- runsir(gamma=1, N=N, S0=oo$par, I0=cases[1]*rho, betafun=betafun, mufun=mufun)
  
  llvec_iter <- rep(NA, nstep)
  S0_iter <- rep(NA, nstep)
  
  ## iterative step
  for (i in 1:nstep) {
    fitdata <- data.frame(
      grad=(predict(gfit, newdata = data.frame(time=1:(nrow(dd))+0.1))-predict(gfit))/0.1+1+birth/N,
      logS=log(oo$par + Z),
      week=dd$week,
      time=1:nrow(dd),
      change=as.factor((time-1)/52+2013<cutoff)
    )
    
    tempdata <- fitdata[fitdata$week==52,]
    tempdata$week <- 0
    
    fitdata <- rbind(fitdata, tempdata)
    
    fitdata$offterm <- fitdata$logS - log(N)
    
    lfit <- gam(grad ~ change + s(week, bs=c("cc"), by=change) + offset(offterm), data=fitdata,
                family = gaussian("log"))
    
    beta <- exp(predict(lfit, newdata=data.frame(week=week, change=(time-1)/52+2013<2016, offterm=0)))
    
    betafun <- approxfun(dd$time, beta)
    mufun <- approxfun(dd$time, dd$birth/N)
    
    S0ini <- oo$par
    
    oo <- optim(S0ini, function(x) {
      deout <- runsir(gamma=1, N=N, S0=x, I0=cases[1]*rho, betafun=betafun, mufun=mufun)
      
      sum((log(deout$I/rho)-log(cases))^2, na.rm=TRUE)
    }, method="Brent", lower=0.5*oo$par, upper=2*oo$par)
    
    llvec_iter[i] <- oo$value
    S0_iter[i] <- oo$par
  }
  
  if (which.min(llvec_iter) > 1) {
    fitdata <- data.frame(
      grad=(predict(gfit, newdata = data.frame(time=1:(nrow(dd))+0.1))-predict(gfit))/0.1+1+birth/N,
      logS=log(S0_iter[which.min(llvec_iter)-1] + Z),
      week=dd$week,
      time=1:nrow(dd),
      change=as.factor((time-1)/52+2013<cutoff)
    )  
    
    S0ini <- S0_iter[which.min(llvec_iter)-1]
    
  } else {
    fitdata <- data.frame(
      grad=(predict(gfit, newdata = data.frame(time=1:(nrow(dd))+0.1))-predict(gfit))/0.1+1+birth/N,
      logS=log(S0vec[j]*N + Z),
      week=dd$week,
      time=1:nrow(dd),
      change=as.factor((time-1)/52+2013<cutoff)
    )
    
    S0ini <- (S0vec[j]*N + Z)[1]
  }
  
  tempdata <- fitdata[fitdata$week==52,]
  tempdata$week <- 0
  
  fitdata <- rbind(fitdata, tempdata)
  
  fitdata$offterm <- fitdata$logS - log(N)
  
  lfit <- gam(grad ~ change + s(week, bs=c("cc"), by=change) + offset(offterm), data=fitdata,
              family = gaussian("log"))
  
  beta <- exp(predict(lfit, newdata=data.frame(week=week, change=(time-1)/52+2013<2016, offterm=0)))
  
  betafun <- approxfun(dd$time, beta)
  mufun <- approxfun(dd$time, dd$birth/N)
  
  oo <- optim(S0ini, function(x) {
    deout <- runsir(gamma=1, N=N, S0=x, I0=cases[1]*rho, betafun=betafun, mufun=mufun)
    
    sum((log(deout$I/rho)-log(cases))^2, na.rm=TRUE)
  }, method="Brent", lower=0.5*S0ini, upper=2*S0ini)
  
  deout <- runsir(gamma=1, N=N, S0=oo$par, I0=cases[1]*rho, betafun=betafun, mufun=mufun)
  
  list(
    Z=Z,
    j=j,
    S0vec=S0vec,
    S0=oo$par,
    llvec=llvec,
    llvec_iter,
    S0_iter,
    lfit=lfit,
    beta=beta,
    betafun=betafun,
    mufun=mufun,
    rho=rho,
    deout=deout
  )
}

sir <- function(t, y, par) {
  with(as.list(c(y, par)), {
    beta <- betafun(t)
    mu <- mufun(t)
    
    dS <- mu * N - beta * S * I/N - mu * S
    dI <- beta * S * I/N - (gamma + mu) * I
    
    list(c(dS, dI),
         Reff=beta * S/N)
  })
}

runsir <- function(gamma=1,
                   N=N,
                   S0,
                   I0,
                   betafun,
                   mufun) {
  param <- list(N=N, gamma=gamma, mufun=mufun, betafun=betafun)
  yini <- c(S=unname(S0), I=I0)
  
  deout <- as.data.frame(deSolve::ode(yini, 1:364, sir, param))
  
  deout
}
