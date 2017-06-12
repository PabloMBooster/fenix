
# dinamics population -----------------------------------------------------

# stock - reclutamiento

"SR.Fun" <- function (rs = rs, st = st, type = c("Ricker","BevertonHolt"))#, param = 1, msg = FALSE) 
{
  type <- match.arg(type)
  switch(type, Ricker= {
    
    fit=lm(log(rt/st)~st)
    a <- fit$coef[1]
    b <- fit$coef[2]
    St <- seq(0,max(st),by=(max(st)/(length(st)-1)))
    Rt = exp(a)*St*exp(b*St)
    #Rmax <- D(exp(a)*St*exp(b*St),"St")
    par(mfrow=c(1,2))
    plot(st,log(rt/st),xlab="Stock Desovante",ylab="Log(Rt/St)",pch=16,col=1)
    points(st,fitted(fit),pch="O",col=3)
    text(st,log(rt/st),yr,cex=0.6,pos=4)
    #png("grafica.png",width=600,height=600)
    plot(st,rt,xlab="Stock Desovante",ylab="Reclutamiento",pch=16,col=1,cex=1.2,cex.lab=1,cex.axis=1)
    lines(St,Rt,lwd=3,col=2)
    #text(3200,13000,expression(R == alpha * SSB * e^(beta * SSB)),cex=1)
    #dev.off()
    #text(st,rt,yr,cex=0.6,pos=1)#
    #print(Rt)#
    return(list(fit=summary(fit), aic=AIC(fit)))#,Rmax=Rmax))
    
  }, BevertonHolt = {
    
    #fit=lm(log(rt/st)~log(a)-log(1+b*st),start=list(a=1,b=1),model=T,
    # control=list(maxiter=1000))
    fit=nls(log(rt/st)~log(a)-log(1+b*st),start=list(a=2,b=0.0001),model=T,
            control=list(maxiter=1000))
    a <- coefficients(fit)[1]
    b <- coefficients(fit)[2]
    
    St <- seq(0,max(st),by=(max(st)/(length(st)-1)))
    Rt=a*St/(1+b*St)
    par(mfrow=c(1,2))
    plot(st,log(rt/st),xlab="Stock Desovante",ylab="Log(Rt/St)",pch=16,col=1)
    points(st,fitted(fit),pch="O",col=3)
    text(st,log(rt/st),yr,cex=0.6,pos=4)
    plot(st,rt,xlab="Stock Desovante",ylab="Reclutamiento",pch=16,col=1)
    lines(St,Rt,type="l",lwd=2,col=2)
    text(st,rt,yr,cex=0.6,pos=1)
    #text(3200,13000,expression(R == alpha * SSB * e^(beta * SSB)),cex=1)
    return(list(fit=summary(fit), aic=AIC(fit)))
  })
}

## production model

schaefer = function(theta)
{
  bt=vector()  #vector of population biomass
  with(as.list(theta),{
    r=exp(r); k=exp(k); q=exp(q)
    bt[1]=k  #assumption that starting biomass is at carrying capacity.
    for(i in 1:length(yr))
    {
      bt[i+1]=bt[i]+r*bt[i]*(1-bt[i]/k)-ct[i]  #assuming no process errors
    }
    #print("bt")
    #print(bt)
    epsilon = log(yt)-log(q*bt[1:length(yt)])
    nloglike = -sum(dnorm(epsilon,0,0.2,log=T))
    #print(yt/exp(epsilon))
    #print(q)
    #print(r)
    #print(k)
    #it is the predicted cpue  [cpue=q*bt*exp(epsilon),  where q*bt=it]
    return(list(nloglike=nloglike,bt=bt,r=r,k=k,epsilon=epsilon,it=yt/exp(epsilon)))
  })
}

solver = function(pars, fn, methods = "BFGS", hess=FALSE, maxit = 100000)
{
  #fit = optim(pars,fn,method="BFGS",hessian=T)
  fit = optim(pars, fn, method = methods,#lower=log(c(kmin,rmin,qmin)),
              #upper=log(c(kmax,rmax,qmax))
              control = list(maxit = maxit), hessian = T)
  if(hess){	fit$V = solve(fit$hessian)  			#Variance-Covariance
  fit$S = sqrt(diag(fit$V))				#Standard deviations
  fit$R = fit$V/(fit$S %o% fit$S)}	#Parameter correlation
  return(fit)
}

# fit=solver(theta,fn=function(theta) schaefer(theta)$nloglike,hess=T)
# A=schaefer(fit$par)

