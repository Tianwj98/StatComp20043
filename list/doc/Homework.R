## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
fit<-lm(weight~height,data=women)
plot(women$height,women$weight,xlab="Height",ylab="Weight")
abline(fit)

## -----------------------------------------------------------------------------
knitr::kable(head(women))

## -----------------------------------------------------------------------------
set.seed(2)
n<-1000
u<-runif(n)
x<-2/(1-u)^(1/2)
hist(x,prob=TRUE,main = expression(f(x)==8*x^-4))
y <- seq(2,100, .01)
lines(y, 8*y^(-4))

## -----------------------------------------------------------------------------
set.seed(3)
n<-1000
u<-runif(n)
r<-1
q<-4*u-2
theta<-acos(-q/(2*r))/3
x<-2*cos(theta+4*pi/3)
hist(x,prob=TRUE)


## -----------------------------------------------------------------------------
set.seed(3)
n<-1000
u0<-c(1:1000)
u1<-runif(n,-1,1)
u2<-runif(n,-1,1)
u3<-runif(n,-1,1)
for(i in 1:1000){
  if(abs(u3[i])>=abs(u2[i]) & abs(u3[i])>=abs(u1[i])){
  u0[i]=u2[i]
 }else
 {u0[i]=u3[i]
 }
}
hist(u0,prob=TRUE,main = expression(f(x)==(3/4)*(1-x^2)))
y <- seq(-1,1, .01)
lines(y, (3/4)*(1-y^2))

## -----------------------------------------------------------------------------
set.seed(4)
n<-1000
u<-runif(n)
r<-4
beta<-2
y<-beta/(1-u)^(1/r)-beta
hist(y,prob=TRUE,main = expression(f(y)==64/(2+y)^5))
z<-seq(0,10, .01)
lines(z,r*beta^r/(beta+z)^(r+1))

## -----------------------------------------------------------------------------
set.seed(11)
m<-10000
t<-runif(m,min=0,max=pi/3)
theta.hat<-mean(sin(t))*(pi/3)
print(theta.hat)
print(-cos(pi/3)+cos(0))

## -----------------------------------------------------------------------------
set.seed(111)
n<-10000
u<-runif(n)
T1<-exp(u) #simple MC
theta1<-mean(T1)
s1<-var(T1)/n
n1<-n/2
u1<-u[1:n1]
y1<-exp(u1)
y2<-exp(1-u1)
y<-(y1+y2)/2
theta2<-mean(y) #antithetic variables
s2<-var(y)/n1
theta1
theta2
(s1-s2)/s1


## -----------------------------------------------------------------------------
e<-exp(1)
v1=(-1/2)*e^2+2*e-3/2
v2=(-3/4)*e^2+5*e/2-5/4
(v1-2*v2)/v1


## -----------------------------------------------------------------------------
x<-seq(1,10,.01)
w<-2
f1<-2*exp(-x^2/2)/sqrt(2*pi)
f2<-x*exp(-x^2/2)
g<-x^2*exp(-x^2/2)/sqrt(2*pi)
plot(x,g,type="l",main="",ylab="",lwd=w)
lines(x,f1,lty=2,lwd=w)
lines(x,f2,lty=3,lwd=w)
legend("topright",legend=c("g",1:2),lty=1:3,lwd=w,inset=0.02)

plot(x,g/f1,type="l",main="",ylab="",ylim=c(0,4),lwd=w,lty=2)
lines(x,g/f2,lty=3,lwd=w)
legend("topright",legend=c(1:2),lty=2:3,lwd=w,inset=0.02)


## -----------------------------------------------------------------------------
set.seed(111)
m<-10000
theta.hat<-s<-numeric(5)
g<-function(x){
  exp(-x-log(1+x^2))*(x>0)*(x<1)
}
u1<-runif(m/5,0,1/5)
x1<--log(1-u1*(1-exp(-1/5)))
fg1<-g(x1)/(exp(-x1)/(1-exp(-1/5)))
theta.hat[1]<-mean(fg1)
s[1]<-var(fg1)

u2<-runif(m/5,1/5,2/5)
x2<--log(exp(-1/5)-u2*(exp(-1/5)-exp(-2/5)))
fg2<-g(x2)/(exp(-x2)/(exp(-1/5)-exp(-2/5)))
theta.hat[2]<-mean(fg2)
s[2]<-var(fg2)

u3<-runif(m/5,2/5,3/5)
x3<--log(exp(-2/5)-u3*(exp(-2/5)-exp(-3/5)))
fg3<-g(x3)/(exp(-x3)/(exp(-2/5)-exp(-3/5)))
theta.hat[3]<-mean(fg3)
s[3]<-var(fg3)

u4<-runif(m/5,3/5,4/5)
x4<--log(exp(-3/5)-u4*(exp(-3/5)-exp(-4/5)))
fg4<-g(x4)/(exp(-x4)/(exp(-3/5)-exp(-4/5)))
theta.hat[4]<-mean(fg4)
s[4]<-var(fg4)

u5<-runif(m/5,4/5,5/5)
x5<--log(exp(-4/5)-u5*(exp(-4/5)-exp(-5/5)))
fg5<-g(x5)/(exp(-x5)/(exp(-4/5)-exp(-5/5)))
theta.hat[5]<-mean(fg5)
s[5]<-var(fg5)

sum(theta.hat)
mean(s)

## -----------------------------------------------------------------------------
set.seed(100)
n<-20
alpha<-0.05
UCL<-replicate(1000,expr={
  x<-rlnorm(n,mean=0,sd=2)
  mean(log(x))-sd(log(x))*qt(alpha/2,df=n-1)/sqrt(n)
})
LCL<-replicate(1000,expr={
  x<-rlnorm(n,mean=0,sd=2)
  mean(log(x))+sd(log(x))*qt(alpha/2,df=n-1)/sqrt(n)
})
sum(UCL>0 & LCL<0)
mean(UCL>0 & LCL<0)

## -----------------------------------------------------------------------------
set.seed(99)
n<-20
alpha<-0.05
UCL<-replicate(1000,expr={
  x<-rchisq(n,df=2)
  mean(x)-sd(x)*qt(alpha/2,df=n-1)/sqrt(n)
})
LCL<-replicate(1000,expr={
  x<-rchisq(n,df=2)
  mean(x)+sd(x)*qt(alpha/2,df=n-1)/sqrt(n)
})
sum(UCL>2 & LCL<2)
mean(UCL>2 & LCL<2)


## -----------------------------------------------------------------------------
set.seed(102)
sk<-function(x){
  xbar<-mean(x)
  m3<-mean((x-xbar)^3)
  m2<-mean((x-xbar)^2)
  return(m3/m2^1.5)
}
alpha<-0.05
n<-30
m<-10000
a<-c(seq(5,100,5))
N<-length(a)
pwr<-numeric(N)
cv<-qnorm(1-alpha/2,0,sqrt(6*(n-2)/((n+1)*(n+3))))
for(j in 1:N){
  a1<-a[j]
  sktests<-numeric(m)
  for(i in 1:m){
    x<-rbeta(n,a1,a1)
    sktests[i]<-as.integer(abs(sk(x))>=cv)
  }
  pwr[j]<-mean(sktests)
}
plot(a,pwr,xlab=bquote(a),ylim=c(0,0.2))
se<-sqrt(pwr*(1-pwr)/m)
lines(a,pwr+se,lty=3)
lines(a,pwr-se,lty=3)

## -----------------------------------------------------------------------------
set.seed(103)
count5test<-function(x,y){
  X<-x-mean(x)
  Y<-y-mean(y)
  outx<-sum(X>max(Y))+sum(X<min(Y))
  outy<-sum(Y>max(X))+sum(Y<min(X))
  return(as.integer(max(c(outx,outy))>5))
}
m<-10000
sigma1<-1
sigma2<-1.5
n<-c(20,500,10000)
CFpower<-numeric(length(n))
Fpower<-numeric(length(n))

for(i in 1:length(n)){
  CFpower[i]<-mean(replicate(m,expr={
  x<-rnorm(n[i],0,sigma1)
  y<-rnorm(n[i],0,sigma2)
  count5test(x,y)
}))
  
  pvalues<-replicate(m,expr={
  x<-rnorm(n[i],0,sigma1)
  y<-rnorm(n[i],0,sigma2)
  Ftest<-var.test(x,y,alternative="two.sided")
  Ftest$p.value
})

  Fpower[i]<-mean(pvalues<=0.055)
}

print(CFpower)
print(Fpower)


## -----------------------------------------------------------------------------
set.seed(104)
sk<-function(x){
  m1<-x
  inv_sigma<-solve(cov(t(x)))
  m1[1,]<-m1[1,]-mean(m1[1,])
  m1[2,]<-m1[2,]-mean(m1[2,])
  m3<-(t(m1)%*%inv_sigma%*%m1)^3
  return(mean(m3))
}
d<-2
n<-c(10,20,30,50,100,500)
cv1<-qchisq(0.975,d*(d+1)*(d+2)/6)
cv2<-qchisq(0.025,d*(d+1)*(d+2)/6)
p.reject<-numeric(length(n))
m<-10000
for(i in 1:length(n)){
  sktests<-numeric(m)
  for(j in 1:m){
    x<-rnorm(n[i])
    y<-rnorm(n[i])
    x0<-rbind(x,y)
    sktests[j]<-as.integer((n[i]*sk(x0)/6)>=cv1 | (n[i]*sk(x0)/6)<=cv2)
  }
  p.reject[i]<-mean(sktests)
}
p.reject


## -----------------------------------------------------------------------------
data(law,package="bootstrap")
n<-nrow(law)
LSAT<-law$LSAT
GPA<-law$GPA
theta.hat<-cor(LSAT,GPA)


theta.jack<-numeric(n)
for(i in 1:n){
  theta.jack[i]<-cor(LSAT[-i],GPA[-i])
}
bias<-(n-1)*(mean(theta.jack)-theta.hat)
print(bias)

se<-sqrt((n-1)*mean((theta.jack-mean(theta.jack))^2))
print(se)

round(c(bias=bias,se=se),8)

## -----------------------------------------------------------------------------
library(fitdistrplus)
library(boot)
set.seed(123)

theta.boot<-function(dat,i){
  lambda<-fitdist(dat[i],distr="exp",method="mle")$estimate
  1/lambda
}
data(aircondit,package = "boot")
dat<-aircondit$hours
boot.obj<-boot(dat,statistic=theta.boot,R=2000)
print(boot.ci(boot.obj,type=c("basic","norm","perc","bca")))

## -----------------------------------------------------------------------------
data(scor,package="bootstrap")
n<-nrow(scor)
scor.cov<-cov(scor)
ev<-eigen(scor.cov)
val<-ev$values
theta.hat<-val[1]/sum(val)

theta.jack<-numeric(n)
for(i in 1:n){
  scor.cov.jack<-cov(scor[-i,])
  ev.jack<-eigen(scor.cov.jack)
  val.jack<-ev.jack$values
  theta.jack[i]<-val.jack[1]/sum(val.jack)
}
bias<-(n-1)*(mean(theta.jack)-theta.hat)
print(bias)

se<-sqrt((n-1)*mean((theta.jack-mean(theta.jack))^2))
print(se)

round(c(bias=bias,se=se),8)

## -----------------------------------------------------------------------------
library(DAAG)
attach(ironslag)
n<-length(magnetic)
e1<-e2<-e3<-e4<-matrix(nrow=n,ncol=n-1)

for(i in 1:n){
  y0<-magnetic[-i]
  x0<-chemical[-i]
  for(k in 1:(n-1)){
    y<-y0[-k]
    x<-x0[-k]
    
    J1 <- lm(y ~ x)
    yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k]
    e1[i,k] <- magnetic[k] - yhat1
    
    J2 <- lm(y ~ x + I(x^2))
    yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k] +
      J2$coef[3] * chemical[k]^2
    e2[i,k] <- magnetic[k] - yhat2
    
    J3 <- lm(log(y) ~ x)
    logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k]
    yhat3 <- exp(logyhat3)
    e3[i,k] <- magnetic[k] - yhat3
    
    J4 <- lm(log(y) ~ log(x))
    logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[k])
    yhat4 <- exp(logyhat4)
    e4[i,k] <- magnetic[k] - yhat4
  }
}



## -----------------------------------------------------------------------------
c(mean(e1^2),mean(e2^2),mean(e3^2),mean(e4^2))

## -----------------------------------------------------------------------------
a <- seq(10, 40, .1)
L2 <- lm(magnetic ~ chemical + I(chemical^2))
plot(chemical, magnetic, main="Quadratic", pch=16)
yhat2 <- L2$coef[1] + L2$coef[2] * a + L2$coef[3] * a^2
lines(a, yhat2, lwd=2)
L2

## -----------------------------------------------------------------------------
set.seed(123)
maxout<-function(x,y) {
 X<-x-mean(x)
 Y<-y-mean(y)
 outx<-sum(X > max(Y)) + sum(X < min(Y))
 outy<-sum(Y > max(X)) + sum(Y < min(X))
 return(max(c(outx, outy)))
}

R<-999
n1<-20
n2<-30
mu1<-mu2<-0
sigma1<-sigma2<-1
x<-rnorm(n1,mu1,sigma1)
y<-rnorm(n2,mu2,sigma2)
z<-c(x,y)
K<-1:50
reps<-numeric(R)
t0<-maxout(x,y)

for(i in 1:R){
  k<-sample(K,size=20,replace=FALSE)
  x1<-z[k]
  y1<-z[-k]
  reps[i]<-maxout(x1,y1)
}
p<-mean(c(t0,reps)>5)
p
  

## -----------------------------------------------------------------------------
library(RANN)
library(boot)
library(energy)
library(Ball)

Tn <- function(z, ix, sizes,k) {
  n1 <- sizes[1]; n2 <- sizes[2]; n <- n1 + n2
  if(is.vector(z)) z <- data.frame(z,0);
  z <- z[ix, ];
  NN <- nn2(data=z, k=k+1)
  block1 <- NN$nn.idx[1:n1,-1]
  block2 <- NN$nn.idx[(n1+1):n,-1]
  i1 <- sum(block1 < n1 + .5); i2 <- sum(block2 > n1+.5)
  (i1 + i2) / (k * n)
}

m <- 1e3; k<-3; p<-2; set.seed(12345)
n1 <- n2 <- 30; R<-999; n <- n1+n2; N = c(n1,n2)
eqdist.nn <- function(z,sizes,k){
  boot.obj <- boot(data=z,statistic=Tn,R=R,
                   sim = "permutation", sizes = sizes,k=k)
  ts <- c(boot.obj$t0,boot.obj$t)
  p.value <- mean(ts>=ts[1])
  list(statistic=ts[1],p.value=p.value)
}
p.values <- matrix(NA,m,3)

## -----------------------------------------------------------------------------
for(i in 1:m){
  x <- matrix(rnorm(n1*p,0,1),ncol=p)
  y <- matrix(rnorm(n2*p,0,1.5),ncol=p)
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,num.permutations=999,seed=i*12345)$p.value
}
alpha <- 0.1;
pow <- colMeans(p.values<alpha)
round(c(NN=pow[1],Energy=pow[2],Ball=pow[3]),3)

## -----------------------------------------------------------------------------
for(i in 1:m){
  x <- matrix(rnorm(n1*p,0,1),ncol=p)
  y <- matrix(rnorm(n2*p,0.3,1.5),ncol=p)
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,num.permutations=999,seed=i*12345)$p.value
}
alpha <- 0.1;
pow <- colMeans(p.values<alpha)
round(c(NN=pow[1],Energy=pow[2],Ball=pow[3]),3)

## -----------------------------------------------------------------------------
for(i in 1:m){
  x <- matrix(rt(n1*p,1),ncol=p)
  y <- cbind(rnorm(n2),rnorm(n2,0.5))
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,num.permutations=999,seed=i*12345)$p.value
}
alpha <- 0.1;
pow <- colMeans(p.values<alpha)
round(c(NN=pow[1],Energy=pow[2],Ball=pow[3]),3)

## -----------------------------------------------------------------------------
set.seed(12345)
f<-function(x){
  return(exp(-abs(x))/2)
}

rw.Metropolis <- function(sigma, x0, N) {
x <- numeric(N)
x[1] <- x0
u <- runif(N)
k <- 0
for (i in 2:N) {
y <- rnorm(1, x[i-1], sigma)
if (u[i] <= (f(y) / f(x[i-1])))
x[i] <- y else {
x[i] <- x[i-1]
k <- k + 1
} }
return(list(x=x, k=k))
}


## -----------------------------------------------------------------------------
N <- 2000
sigma <- c(.5, 1, 2, 4)
round(c(sigma1=sigma[1],sigma2=sigma[2],sigma3=sigma[3],sigma4=sigma[4]),2)
x0 <- 25
rw1 <- rw.Metropolis(sigma[1], x0, N)
rw2 <- rw.Metropolis(sigma[2], x0, N)
rw3 <- rw.Metropolis(sigma[3], x0, N)
rw4 <- rw.Metropolis(sigma[4], x0, N)

#number of candidate points rejected
print(c(rw1$k, rw2$k, rw3$k, rw4$k))

## -----------------------------------------------------------------------------
round(c(sigma1=1-rw1$k/N,sigma2=1-rw2$k/N,sigma3=1-rw3$k/N,sigma4=1-rw4$k/N),4)

## -----------------------------------------------------------------------------
set.seed(12345)
Gelman.Rubin <- function(psi) {

psi <- as.matrix(psi)
n <- ncol(psi)
k <- nrow(psi)
psi.means <- rowMeans(psi) 
B <- n * var(psi.means)
psi.w <- apply(psi, 1, "var") 
W <- mean(psi.w)
v.hat <- W*(n-1)/n + (B/n)
r.hat <- v.hat / W
return(r.hat)
}

k <- 4
n <- 15000
b <- 1000
X <- matrix(0, nrow=k, ncol=n)
for (i in 1:k)
  X[i, ] <-rw.Metropolis(sigma[i], x0, n)$x

psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
  psi[i,] <- psi[i,] / (1:ncol(psi))
print(Gelman.Rubin(psi))


for (i in 1:k)
  plot(psi[i, (b+1):n], type="l",
       xlab=i, ylab=bquote(psi))


rhat <- rep(0, n)
for (j in (b+1):n)
rhat[j] <- Gelman.Rubin(psi[,1:j])
plot(rhat[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
S<-function(k,a){
  return(1-pt(sqrt((a^2)*k/(k+1-a^2)),k))
}
f<-function(a){
  S(k-1,a)-S(k,a)
}


## -----------------------------------------------------------------------------
k=25
solution <- uniroot(f,c(0.01,2))
unlist(solution)

## -----------------------------------------------------------------------------
k=100
solution <- uniroot(f,c(0.01,2))
unlist(solution)

## -----------------------------------------------------------------------------
k=500
solution <- uniroot(f,c(0.01,2))
unlist(solution)

## -----------------------------------------------------------------------------
k=1000
solution <- uniroot(f,c(0.01,2))
unlist(solution)

## -----------------------------------------------------------------------------
library(nloptr)

eval_f0 = function(x,x1,n.A=444,n.B=132,nOO=361,nAB=63) {
  
  r1 = 1-sum(x1)
  nAA = n.A*x1[1]^2/(x1[1]^2+2*x1[1]*r1)
  nBB = n.B*x1[2]^2/(x1[2]^2+2*x1[2]*r1)
  r = 1-sum(x)
  return(-2*nAA*log(x[1])-2*nBB*log(x[2])-2*nOO*log(r)-
           (n.A-nAA)*log(2*x[1]*r)-(n.B-nBB)*log(2*x[2]*r)-nAB*log(2*x[1]*x[2]))
}



eval_g0 = function(x,x1,n.A=444,n.B=132,nOO=361,nAB=63) {
  return(sum(x)-0.999999)
}

opts = list("algorithm"="NLOPT_LN_COBYLA",
             "xtol_rel"=1.0e-8)
mle = NULL
r = matrix(0,1,2)
r = rbind(r,c(0.2,0.35))# the beginning value of p0 and q0
j = 2
while (sum(abs(r[j,]-r[j-1,]))>1e-8) {
res = nloptr( x0=c(0.2,0.25),
               eval_f=eval_f0,
               lb = c(0,0), ub = c(1,1), 
               eval_g_ineq = eval_g0, 
               opts = opts, x1=r[j,],n.A=444,n.B=132,nOO=361,nAB=63 )
j = j+1
r = rbind(r,res$solution)
mle = c(mle,eval_f0(x=r[j,],x1=r[j-1,]))
}

r 
#the max likelihood values
plot(-mle,type = 'l')

## -----------------------------------------------------------------------------
formulas <- list(
mpg ~ disp,
mpg ~ I(1 / disp),
mpg ~ disp + wt,
mpg ~ I(1 / disp) + wt
)

l<-formulas

for(i in 1:4){
  l[[i]]<-lm(formulas[[i]],mtcars)
}

l

## -----------------------------------------------------------------------------
formulas <- list(
mpg ~ disp,
mpg ~ I(1 / disp),
mpg ~ disp + wt,
mpg ~ I(1 / disp) + wt
)

lapply(formulas,function(formula) lm(formula,mtcars))

## -----------------------------------------------------------------------------
set.seed(12345)
trials <- replicate(100, t.test(rpois(10, 10), rpois(7, 10)),
simplify = FALSE
)
sapply(trials,function(trial) trial$p.value)

## -----------------------------------------------------------------------------
x<-matrix(1:10,nrow = 2)
x
vapply(x,function(x) as.vector(x),c(1))

## -----------------------------------------------------------------------------
library(Rcpp)

## ----eval=FALSE---------------------------------------------------------------
#  #include <Rcpp.h>
#  using namespace Rcpp;
#  
#  double lap_f(double x){
#    return 0.5 * exp(-abs(x));
#  }
#  
#  
#  // [[Rcpp::export]]
#  
#  NumericVector MetropolisC(double sigma, double x0, int N){
#    NumericVector x(N);
#    x[0] = x0;
#    NumericVector u(N);
#    u = runif(N);
#    int k = 0;
#    for (int i = 1; i < N; ++i){
#      NumericVector y(1);
#      y = rnorm(1, x[i-1],sigma);
#      if ( u[i] <= lap_f(y[0])/lap_f(x[i-1]))
#            x[i] = y[0] ;
#      else{
#            x[i] = x[i-1];
#            k = k+1;
#      }
#    }
#    return x;
#  }
#  

## -----------------------------------------------------------------------------
set.seed(12345)
lap_f = function(x) exp(-abs(x))

rw.Metropolis = function(sigma, x0, N){
 x = numeric(N)
 x[1] = x0
 u = runif(N)
 k = 0
 for (i in 2:N) {
  y = rnorm(1, x[i-1], sigma)
  if (u[i] <= (lap_f(y) / lap_f(x[i-1]))) x[i] = y 
  else {
  x[i] = x[i-1]
  k = k+1
  }
 }
 return(list(x = x, k = k))
}


dir_cpp <- '../src/'

sourceCpp(paste0(dir_cpp,"Homework.cpp")) 

n<-4
N<-2000
sigma<-c(0.05,0.5,2,16)

x0<-25
rw1c<-MetropolisC(sigma[1],x0,N)
rw2c<-MetropolisC(sigma[2],x0,N)
rw3c<-MetropolisC(sigma[3],x0,N)
rw4c<-MetropolisC(sigma[4],x0,N)
rw1<-rw.Metropolis(sigma[1],x0,N)
rw2<-rw.Metropolis(sigma[2],x0,N)
rw3<-rw.Metropolis(sigma[3],x0,N)
rw4<-rw.Metropolis(sigma[4],x0,N)


## -----------------------------------------------------------------------------
plot(rw1$x,type="l",xlab=bquote(sigma==0.05),ylab="X",main="R function")
plot(rw1c,type="l",xlab=bquote(sigma==0.05),ylab="X",main="Rcpp function")
plot(rw2$x,type="l",xlab=bquote(sigma==0.5),ylab="X",main="R function")
plot(rw2c,type="l",xlab=bquote(sigma==0.5),ylab="X",main="Rcpp function")
plot(rw3$x,type="l",xlab=bquote(sigma==2),ylab="X",main="R function")
plot(rw3c,type="l",xlab=bquote(sigma==2),ylab="X",main="Rcpp function")
plot(rw4$x,type="l",xlab=bquote(sigma==16),ylab="X",main="R function")
plot(rw4c,type="l",xlab=bquote(sigma==16),ylab="X",main="Rcpp function")

## -----------------------------------------------------------------------------
library(microbenchmark)
ts1 <- microbenchmark(rw1R=rw.Metropolis(sigma[1],x0,N),rw1C=MetropolisC(sigma[1],x0,N))
summary(ts1)[,c(1,3,5,6)]

ts2 <- microbenchmark(rw2R=rw.Metropolis(sigma[2],x0,N),rw2C=MetropolisC(sigma[2],x0,N))
summary(ts2)[,c(1,3,5,6)]

ts3 <- microbenchmark(rw3R=rw.Metropolis(sigma[3],x0,N),rw3C=MetropolisC(sigma[3],x0,N))
summary(ts3)[,c(1,3,5,6)]

ts4 <- microbenchmark(rw4R=rw.Metropolis(sigma[4],x0,N),rw4C=MetropolisC(sigma[4],x0,N))
summary(ts4)[,c(1,3,5,6)]

