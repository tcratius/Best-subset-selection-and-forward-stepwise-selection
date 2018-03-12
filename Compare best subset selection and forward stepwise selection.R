##install.packages("leaps")
##install.packages("lars")
##install.packages("wle")

require(leaps)
require(wle)
t1<-c()
t2<-c()
di1<-c()
di2<-c()
num1=0
num2=0
for(m in 1:1000){
  a <- c(1:5)
  x<- rnorm(1000)
  X<- matrix(x, 100, 10)
  X1<- matrix(rep(1, 100), 100, 1)
  XX<-cbind(X1, X)
  B<- c(1, 5, 4, 3, 2, 1, rep(0, 5))
  e<-rnorm(100, sd=0.1)
  Y<-XX%*%B+e
  
  ##Best subset selection##
  ptm <- proc.time()
  L = leaps(X, Y)
  ind = which.min(L$Cp)
  LL <- L$which[ind,] 
  Best <- which(LL==TRUE)
  di1[m]<-length(c(setdiff(a, Best), setdiff(Best, a)))
  if(di1[m]==0) num1=num1+1
  t1[m]<-proc.time()[3] - ptm[3]
  
  ##Forward stepwise selection##
  ptm <- proc.time()
  fit = lm(Y ~ X[,1]+X[,2]+X[,3]+X[,4]+X[,5]+X[,6]+X[,7]+X[,8]+X[,9]+X[,10])
  step <- mle.stepwise(fit, type="Forward")
  result <- as.matrix(step$step)
  dim<-dim(result) 
  result1 <- result[dim[1], -c(1,dim[2])]
  GG <- which(result1!=0)
  di2[m]<-length(c(setdiff(a, GG), setdiff(GG,a)))
  if(di2[m]==0) num2=num2+1
  t2[m]<-proc.time()[3] - ptm[3]
}
tt1<-sum(t1)
tt2<-sum(t2)
ddi1<-mean(di1)
ddi2<-mean(di2)
num1<-num1/1000
num2<-num2/1000
cat("For 1000 times simulation, The total time for best subset selection method is", tt1)
cat("For 1000 times simulation, The total time for forward stepwise selection method is", tt2)
cat("For 1000 times simulation, The average difference for best subset selection method is", ddi1)
cat("For 1000 times simulation, The average difference for forward stepwise selection method is", ddi2)
cat("For 1000 times simulation, The accuracy rate of best subset selection method is", num1)
cat("For 1000 times simulation, The accuracy rate of forward stepwise selection method is", num2)

##Firstly, as for computational speed, we simulate the problem for 1000 times, the
##total time for best subset selection method is around 6.0 seconds, and the total 
##time for forward stepwise selection method is also around 5.7 seconds, so the speeds 
##of the two methods are similar, and best subset selection method is a little 
##slower than forward stepwise selection method.

##Secondly, as for accuracy, the average diiference of variable numbers between true 
##model and best subset selection estimator is about 0.80, but the average 
##difference of variable numbers between true model and forward stepwise selection
##estimator is about 0.25, so forward stepwise selection method is more accurate
##than best subset selection method.

##Additionally, accuracy rate for best subset selection method is about
##0.43, but accuracy rate for forward stepwise selection method is around 0.78, so
##the accuracy rate of forward stepwise selection method is higher than that of 
##best subset selection method.
