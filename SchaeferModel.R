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
	
solver = function(pars,fn,hess=FALSE)
	{
		#fit = optim(pars,fn,method="BFGS",hessian=T)
    fit = optim(pars,fn,method="L-BFGS-B",lower=log(c(kmin,rmin,qmin)),
                upper=log(c(kmax,rmax,qmax)),control = list(maxit=100000),hessian=T)
		if(hess){	fit$V = solve(fit$hessian)  			#Variance-Covariance
				      fit$S = sqrt(diag(fit$V))				#Standard deviations
				      fit$R = fit$V/(fit$S %o% fit$S)}	#Parameter correlation
		return(fit)
	}

fit=solver(theta,fn=function(theta) schaefer(theta)$nloglike,hess=T)
A=schaefer(fit$par)
