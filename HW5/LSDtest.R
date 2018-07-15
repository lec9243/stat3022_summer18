LSDtest=function(y,trt,alpha=0.05,...)
{
  if (missing(...)) m=aov(y~trt)##trt and other factors
  else m=aov(y~trt+...)##trt and other factors
  tab=summary(m)[[1]]##anova table
  dfE=tab$Df[length(tab$Df)]##degrees of freedom for errors
  MSE=tab$`Mean Sq`[length(tab$`Mean Sq`)]##mean squared errors
  crit=qt(1-alpha/2,dfE)##critical value
  n=tapply(y,trt,length)##group sizes
  mu=tapply(y,trt,mean)##group means
  K=length(mu)##number of groups
  L=choose(K,2)##number of pairwise comparisons
  pair=rep(0,L)##pair names
  est=rep(0,L)
  lwr=upr=rep(0,L)
  
  l=1
  for(i in 1:(K-1)){
    for(j in (i+1):K){
      LSD=crit*sqrt(MSE*(1/n[i]+1/n[j]))
      pair[l]=paste(names(mu)[i],"~",names(mu)[j],sep="")
      est[l]=mu[i]-mu[j]
      lwr[l]=est[l]-LSD
      upr[l]=est[l]+LSD
      l=l+1
    }
  }
  compare=data.frame(pair=pair,est=est,lwr=lwr,upr=upr)
  return(compare)
}