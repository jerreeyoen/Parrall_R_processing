#parallel 
library(parallel)
## Mixture data
y = mixture.dat$y


## INITIAL VALUES
n = 10000
x.val1 = NULL
x.val2=NULL
x.val3=NULL
burn.in=201
set.seed(0)
#set Proposal variables
#first proposal
PV1.x1<-1
PV1.x2<-1
#second proposal
PV2.x1<-2
PV2.x2<-10
#third
PV3.x1<-1.5
PV3.x2<-8
## FUNCTIONS
f = function(x){prod(x*dnorm(y,7,0.5) + (1-x)*dnorm(y,10,0.5))}
R = function(xt,x,x1,x2){f(x)*g(xt,x1,x2)/(f(xt)*g(x,x1,x2))}
MHMC<-function(x1,x2,x.val){
  xt = x.val
  x = rbeta(1,x1,x2)
  p = min(R(xt,x,x1,x2),1)
  d = rbinom(1,1,p)
  out = x*d + xt*(1-d)
  return(out)
}
## MAIN
# BETA(1,1) PROPOSAL DENSITY
start.time<-proc.time()
g = function(x,x1,x2){dbeta(x,x1,x2)}
x.val1[1] = rbeta(1,PV1.x1,PV1.x2)

for(i in 1:n){
  x.val1[i+1] <- MHMC(PV1.x1,PV1.x2,x.val1[i])
}
mean(x.val1[(burn.in+1):(n+1)])
par(mfrow=c(2,2))
plot(x.val1[(burn.in+1):(n+1)],ylim=c(0,1),type="l",ylab="delta",xlab="t")
title(paste("Sample path for Beta(",PV1.x1,",",PV1.x2,") Proposal Dist."))
hist(x.val1[(burn.in+1):(n+1)],breaks=20,xlab="delta",
     main=paste("Hist. for Beta(",PV1.x1,",",PV1.x2,") Proposal Dist."))

# BETA(2,10) PROPOSAL DENSITY
x.val2[1] = rbeta(1,PV2.x1,PV2.x2)
for(i in 1:n){
  x.val2[i+1] <- MHMC(PV2.x1,PV2.x2,x.val2[i])
}
mean(x.val2[(burn.in+1):(n+1)])
plot(x.val2[(burn.in+1):(n+1)],ylim=c(0,1),type="l",ylab="delta",xlab="t")
title(paste("Sample path for Beta(",PV2.x1,",",PV2.x2,") Proposal Dist."))
hist(x.val2[(burn.in+1):(n+1)],breaks=20,xlab="delta",
     main=paste("Hist. for Beta(",PV2.x1,",",PV2.x2,") Proposal Dist."))


#x.val3[1] = rbeta(1,PV3.x1,PV3.x2)
#for(i in 1:n){
#  x.val3[i+1] <- MHMC(PV3.x1,PV3.x2,x.val3[i])
#}
end.time<-proc.time()
last.singlecore<-(end.time[3]-start.time[3])/60

marking<-NULL
no.cores<-detectCores()
for(token in 1:no.cores){
clust <-makeCluster(token)
start.time<-proc.time()
clusterExport(clust,c("MHMC","PV1.x1","PV1.x2","PV2.x1","PV2.x2","n","x.val1","x.val2","R","f","g","y","PV3.x1","PV3.x2","x.val3"))
#clusterEvalQ(clust,"MHMC")
parSapply(clust,1:n,function(i) c(x.val1[i+1] <- MHMC(PV1.x1,PV1.x2,x.val1[i]),x.val2[i+1] <- MHMC(PV2.x1,PV2.x2,x.val2[i])))
par(mfrow=c(2,2))
plot(x.val1[(burn.in+1):(n+1)],ylim=c(0,1),type="l",ylab="delta",xlab="t")
title(paste("Sample path for Beta(",PV1.x1,",",PV1.x2,") Proposal Dist."))
hist(x.val1[(burn.in+1):(n+1)],breaks=20,xlab="delta",
     main=paste("Hist. for Beta(",PV1.x1,",",PV1.x2,") Proposal Dist."))
plot(x.val2[(burn.in+1):(n+1)],ylim=c(0,1),type="l",ylab="delta",xlab="t")
title(paste("Sample path for Beta(",PV2.x1,",",PV2.x2,") Proposal Dist."))
hist(x.val2[(burn.in+1):(n+1)],breaks=20,xlab="delta",
     main=paste("Hist. for Beta(",PV2.x1,",",PV2.x2,") Proposal Dist."))
end.time<-proc.time()
stopCluster(clust)
last.mutiplecores<-(end.time[3]-start.time[3])/60

print(paste("#core",token," time",last.mutiplecores))
marking[token]<-last.mutiplecores
}
par(mfrow=c(1,1))
plot(1:no.cores,marking,type="l",xlab = "#cores", ylab="time used")
print(paste("#core",token," time",last.singlecore))
