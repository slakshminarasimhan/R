source('D:/Users/Lakshmi Narasimhan/PROJECTS/code.r', echo=TRUE)

library(dplyr)
dat <- read.csv("D:\\Users\\Lakshmi Narasimhan\\PROJECTS\\femaleMiceWeights.csv")

dat[12, 2]

dat$Bodyweight[11]

length(dat$Diet)

hf <- filter(dat, Diet=="hf")

mean(hf$Bodyweight)

hf <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist
class(hf)
mean(hf)

chow <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
class(chow)
mean(chow)

obsdiff <- mean(hf) - mean(chow)
print(obsdiff)

set.seed(1)

i <- sample( 13:24, 1)
dat$Bodyweight[i]

#https://rstudio-pubs-static.s3.amazonaws.com/155770_0c286e39b48b4670ab7ababb103a7448.html


library(downloader)
url="https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- basename(url)
download(url,filename)



set.seed(1)
population <- read.csv("femaleControlsPopulation.csv")
population <- unlist(population) 
mean(population)
control <- sample(population,5)
abs(mean(control)-mean(population))

set.seed(1)
population <- read.csv("femaleControlsPopulation.csv")
population <- unlist(population) 
mean(population)

n <- 1000
null <- vector("numeric",n)
for (i in 1:n) {
  control <- sample(population, 50)
  null[i] <- mean(control)
}

hist(null)

mean( abs( null - mean(population)) >1 )






install.packages("gapminder")
library(gapminder)
data(gapminder)
head(gapminder)
class(gapminder)

x<-filter(gapminder, year==1952) %>% select(lifeExp) %>% unlist

###OR

dat1952 <- gapminder[gapminder$year==1952, ]
y = dat1952$lifeExp

mean(x<=40)
mean(x<=60) - mean(x<=40)


setwd("D:\\Users\\Lakshmi Narasimhan\\PROJECTS")
x<-unlist(read.csv("femaleControlsPopulation.csv"))



#random sample of 5 mice 1000 times
set.seed(1)
n <- 1000
fivemice1000times <- vector("numeric",n)
for (i in 1:n) {
  control <- sample(x, 5)
  fivemice1000times[i] <- mean(control)
}
mean(fivemice1000times)

#random sample of 50 mice 1000 times
set.seed(1)
n<-1000
fiftymice1000times <- vector("numeric", n)
for (i in 1:n) {
  control <- sample(x, 50)
  fiftymice1000times[i] <- mean(control)
}
hist(fiftymice1000times)
mean(fiftymice1000times)

install.packages("rafalib")
library(rafalib)
mypar(1,2)
hist(fivemice1000times)
hist(fiftymice1000times)
hist(fivemice1000times, xlim = c(18, 30))
hist(fiftymice1000times, xlim = c(18, 30))


mean(fiftymice1000times<25) - mean(fiftymice1000times<23)
mean(fiftymice1000times > 23 & fiftymice1000times < 25)

pnorm(25,23.9,0.43)-pnorm(23,23.9,0.43)
#or
pnorm( (25-23.9) / 0.43)  - pnorm( (23-23.9) / 0.43) 

dat <- read.csv("mice_pheno.csv") 
dat <- na.omit(dat)

controlPopulation <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
mean(controlPopulation)

hfPopulation <- filter(dat, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist
length(hfPopulation)

popsd(controlPopulation)

set.seed(1)
x<-sample(controlPopulation, 25)
mean(x)

mean(hfPopulation)

popsd(hfPopulation)

set.seed(1)
y<-sample(hfPopulation, 25)
mean(y)

(mean(hfPopulation)-mean(controlPopulation)) - (mean(y)-mean(x))

#same exercise for female population
library(dplyr)
dat <- read.csv("mice_pheno.csv") 
dat <- na.omit(dat)

set.seed(1)
controlPopulation <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
mean(controlPopulation)

set.seed(1)
hfPopulation <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
length(hfPopulation)
mean(hfPopulation)

popsd(controlPopulation)

set.seed(1)
x<-sample(controlPopulation, 25)
mean(x)

mean(hfPopulation)

popsd(hfPopulation)

set.seed(1)
y<-sample(hfPopulation, 25)
mean(y)

abs((mean(hfPopulation)-mean(controlPopulation)) - (mean(y)-mean(x)))

pnorm(1)-pnorm(-1)
pnorm(2)-pnorm(-2)
pnorm(3)-pnorm(-3)

set.seed(1)
controlPopulation <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- (controlPopulation - mean(controlPopulation)) / popsd(controlPopulation)
mean(abs(z) <=1)
mean(abs(z) <=2)
mean(abs(z) <=3)

#comparison of quantiles

mypar(1,1)
qqnorm(z)
abline(0, 1)


#comparison of quantiles of male, female mice

mypar(2,2)
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)


#central limit theorem - distribution of random variables
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)

mean(avgs)
popsd(avgs)



library(dplyr)
dat <- read.csv("D:\\Users\\Lakshmi Narasimhan\\PROJECTS\\femaleMiceWeights.csv")

set.seed(1)
n <- 100
sides <- 6
p <- 1/sides
zs <- replicate(10000,{
  x <- sample(1:sides,n,replace=TRUE)
  (mean(x==6) - p) / sqrt(p*(1-p)/n)
}) 
qqnorm(zs)
abline(0,1)#confirm it's well approximated with normal distribution
mean(abs(zs) > 2)


#for various p and n values, run it in one loop

ps <- c(0.5,0.5,0.01,0.01)
ns <- c(5,30,30,100)
library(rafalib)
mypar(4,2)
for(i in 1:4){
  p <- ps[i]
  sides <- 1/p
  n <- ns[i]
  zs <- replicate(10000,{
    x <- sample(1:sides,n,replace=TRUE)
    (mean(x==1) - p) / sqrt(p*(1-p)/n)
  }) 
  hist(zs,nclass=7)
  qqnorm(zs)
  abline(0,1)
}

X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist
str(X)
mean(X)
sd(X)


2 * ( 1-pnorm(2/sd(X) * sqrt(12) ) )


sqrt( sd(X)^2/12 + sd(Y)^2/12 )
( mean(Y) - mean(X) ) / sqrt( var(X)/12 + var(Y)/12)

1 - pt(3,df=3)
1 - pt(3,df=15)
1 - pt(3,df=30)
1 - pnorm(3)
Z <- ( mean(Y) - mean(X) ) / sqrt( var(X)/12 + var(Y)/12)
2*( 1-pnorm(Z)) 

t.test(Y,X)$p.value

#inferences

babies <- read.table("D:\\Users\\Lakshmi Narasimhan\\PROJECTS\\babies.txt", header = TRUE)
head(babies)
bwt.nonsmoke <- filter(babies, smoke == 0) %>% select(bwt) %>% unlist
bwt.smoke <- filter(babies, smoke == 1) %>% select(bwt) %>% unlist
#population differences in birth weights
mean(bwt.nonsmoke) - mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

#T test
n=25
set.seed(1)
dat.ns <- sample(bwt.nonsmoke, n)
dat.s <- sample(bwt.smoke, n)
tval<- t.test(dat.ns, dat.s)$statistic
tval

#OR ALTERNATIVELY

n<-25
set.seed(1)

#instead of CLT, we use the t-distribution approximation, what do we add and subtract to obtain a 99% confidence interval (use 2*N-2 degrees of freedom)?


dat.ns<- sample(bwt.nonsmoke, n)
dat.s<- sample(bwt.smoke, n)

X.ns <- mean(dat.ns)
sd.ns <- sd(dat.ns)

X.s <- mean(dat.s)
sd.s <- sd(dat.s)

sd.diff <- sqrt(sd.ns^2/n+sd.s^2/n)
tval <- (X.ns - X.s)/sd.diff
tval

#t statistics + or - 1. pnorm computes area under the normal curve
pval <- 1-(pnorm(abs(tval))-pnorm(-abs(tval)))
pval

#OR ALTERNATIVELY
2*(pnorm(-abs(tval)))

# If we use the CLT, what quantity would we add and subtract to this estimate to obtain a 99% confidence interval?
n <- 25
set.seed(1)
dat.ns <- sample(bwt.nonsmoke, n) 
dat.s <- sample(bwt.smoke, n) 
qt(0.995,2*n-2)*sqrt( sd( dat.ns)^2/n + sd( dat.s)^2/n )

#just to cross verify
#qnorm(0.995)*sqrt(sd(dat.ns)^2/n + sd(dat.s)^2/n)

##note that if you define dat.s before dat.ns, you get a different answer
##due to sampling randomness
##tolerance is set to accept both answers

#Why are the values from T-test Exercises #3 and Confidence Intervals Exercises #1 so similar?


N=5
set.seed(1)
dat.ns <- sample(bwt.nonsmoke , N) 
dat.s <- sample(bwt.smoke , N) 
t.test(dat.s, dat.ns)$p.value


N=5
set.seed(1)
dat.ns <- sample(bwt.nonsmoke , N) 
dat.s <- sample(bwt.smoke , N) 
t.test(dat.s, dat.ns)$p.value>0.05

#Set the seed at 1, then use the replicate function to repeat the code used in the exercise above 10,000 times. What proportion of the time do we reject at the 0.05 level?

#set the alpha level at 0.05, calculate power
N=5
set.seed(1)
rejects <- replicate(10000,{
  dat.ns <- sample(bwt.nonsmoke , N)
  dat.s <- sample(bwt.smoke , N)
  t.test(dat.s, dat.ns)$p.value < 0.05
})
mean(rejects)

N=30
set.seed(1)
rejects <- replicate(10000,{
  dat.ns <- sample(bwt.nonsmoke , N)
  dat.s <- sample(bwt.smoke , N)
  t.test(dat.s, dat.ns)$p.value < 0.05
})
mean(rejects)

N=60
set.seed(1)
rejects <- replicate(10000,{
  dat.ns <- sample(bwt.nonsmoke , N)
  dat.s <- sample(bwt.smoke , N)
  t.test(dat.s, dat.ns)$p.value < 0.05
})
mean(rejects)

N=90
set.seed(1)
rejects <- replicate(10000,{
  dat.ns <- sample(bwt.nonsmoke , N)
  dat.s <- sample(bwt.smoke , N)
  t.test(dat.s, dat.ns)$p.value < 0.05
})
mean(rejects)

N=120
set.seed(1)
rejects <- replicate(10000,{
  dat.ns <- sample(bwt.nonsmoke , N)
  dat.s <- sample(bwt.smoke , N)
  t.test(dat.s, dat.ns)$p.value < 0.05
})
mean(rejects)

#changing the alpha level, calculate power
N=30
set.seed(1)
rejects <- replicate(10000,{
  dat.ns <- sample(bwt.nonsmoke , N)
  dat.s <- sample(bwt.smoke , N)
  t.test(dat.s, dat.ns)$p.value < 0.01
})
mean(rejects)

N=60
set.seed(1)
rejects <- replicate(10000,{
  dat.ns <- sample(bwt.nonsmoke , N)
  dat.s <- sample(bwt.smoke , N)
  t.test(dat.s, dat.ns)$p.value < 0.01
})
mean(rejects)

N=90
set.seed(1)
rejects <- replicate(10000,{
  dat.ns <- sample(bwt.nonsmoke , N)
  dat.s <- sample(bwt.smoke , N)
  t.test(dat.s, dat.ns)$p.value < 0.01
})
mean(rejects)

N=120
set.seed(1)
rejects <- replicate(10000,{
  dat.ns <- sample(bwt.nonsmoke , N)
  dat.s <- sample(bwt.smoke , N)
  t.test(dat.s, dat.ns)$p.value < 0.01
})
mean(rejects)

#refactored code for the 4 blocks of code above

N=5
set.seed(1)
rejects <- replicate(10000,{
  dat.ns <- sample(bwt.nonsmoke , N)
  dat.s <- sample(bwt.smoke , N)
  t.test(dat.s, dat.ns)$p.value < 0.05
})
mean(rejects)

#or 

set.seed(1)
N <- 5
alpha <- 0.05
reject <- function(N, alpha=0.05){
  dat.ns <- sample(bwt.nonsmoke , N) 
  dat.s <- sample(bwt.smoke , N) 
  pval <- t.test(dat.s,dat.ns )$p.value
  pval < alpha
}
reject(N)

B <- 10000
rejections <- replicate(B,reject(N))
mean(rejections) 

Ns=c(10,60,90,120)
res <- sapply(Ns, function(N){
  set.seed(1)
  rejects <- replicate(10000,{
    dat.ns <- sample(bwt.nonsmoke , N)
    dat.s <- sample(bwt.smoke , N)
    t.test(dat.s, dat.ns)$p.value < 0.05
  })
  mean(rejects)
})
Ns[ which.min( abs( res - .8) ) ] 

#or

Ns <- c(30, 60, 90,120)
power <- sapply(Ns,function(N){
  rejections <- replicate(B, reject(N))
  mean(rejections)
})
power 

Ns=c(10,60,90,120)
res <- sapply(Ns, function(N){
  set.seed(1)
  rejects <- replicate(10000,{
    dat.ns <- sample(bwt.nonsmoke , N)
    dat.s <- sample(bwt.smoke , N)
    t.test(dat.s, dat.ns)$p.value < 0.01
  })
  mean(rejects)
})
Ns[ which.min( abs( res - .8) ) ]


#Monte carlo simulations
#rnorm random number generator

set.seed(1)
X<-rnorm(5)
t<-(sqrt(5)*mean(X))/sd(X)
t


#You have just performed a Monte Carlo simulation using rnorm , 
# a random number generator for normally distributed data. 
# Gosset’s mathematical calculation tells us that the t-statistic
# defined in the previous exercises, a random variable, follows a 
# t-distribution with N−1 degrees of freedom. 
# Monte Carlo simulations can be used to check the theory: 
#   we generate many outcomes and compare them to the theoretical result. 
# Set the seed to 1, generate B=1000 t-statistics as done in exercise 1. 
# What proportion is larger than 2?

set.seed(1)
ttestgenerator <- function(n) {
  X <- rnorm(n)
  tstat <- (sqrt(n)*mean(X))/sd(X) 
  return(tstat)
}

ttests <- replicate(1000, ttestgenerator(5))
mean(ttests>=2)

#or
set.seed(1)
N <- 5
B<- 1000

tstats <- replicate(B,{
  X <- rnorm(N)
  sqrt(N)*mean(X)/sd(X)
})
mean(tstats>2)

1-pt(2,df=4)

#Check For which sample sizes does the approximation best work?

library(rafalib)
mypar(3,2)

Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
  ts <- replicate(B, {
    X <- rnorm(N)
    sqrt(N)*mean(X)/sd(X)
  })
  ps <- seq(1/(B+1),1-1/(B+1),len=B)
  qqplot(qt(ps,df=N-1),ts,main=N,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
} 

Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
  ts <- replicate(B,{
    x <- rnorm(N)
    y <- rnorm(N)
    t.test(x,y, var.equal = TRUE)$stat
  })
  ps <- seq(1/(B+1),1-1/(B+1),len=B)
  qqplot(qt(ps,df=2*N-2),ts,main=N,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
}  


#to test how many degrees of freedom
#The population data is not normal thus the theory does not apply.
#We check with a Monte Carlo simulation. The qqplot shows a large tail. 
#Note that there is a small but positive chance that all the X are the same.
##In this case the denominator is 0 and the t-statistics is not defined
set.seed(1)
N <- 15
B <- 10000
tstats <- replicate(B,{
  X <- sample(c(-1,1), N, replace=TRUE)
  sqrt(N)*mean(X)/sd(X)
})
ps=seq(1/(B+1), 1-1/(B+1), len=B) 
qqplot(qt(ps,N-1), tstats, xlim=range(tstats))
abline(0,1)

set.seed(1)
N <- 1000
B <- 10000
tstats <- replicate(B,{
  X <-  sample(c(-1,1), N, replace=TRUE)
  sqrt(N)*mean(X)/sd(X)
})
qqnorm(tstats)
abline(0,1)

#With N=1000, CLT kicks in and the t-statistic is approximated with normal 0,1
##Furthermore, t-distribution with df=999 and normal are practically the same.

set.seed(1)
Ns <- seq(5,45,5)
library(rafalib)
mypar(3,3)
for(N in Ns){
  medians <- replicate(10000, median ( rnorm(N) ) )
  title <- paste("N=",N,", avg=",round( mean(medians), 2) , ", sd*sqrt(N)=", round( sd(medians)*sqrt(N),2) )
  qqnorm(medians, main = title )
  qqline(medians)
}

##there is an asymptotic result that says SD is sqrt(N*4*dnorm(0)^2)



N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obs <- mean(smokers) - mean(nonsmokers)

#The question is whether this observed difference is statistically 
# significant. We do not want to rely on the assumptions needed for 
# the normal or t-distribution approximations to hold, so instead 
# we will use permutations. We will reshuffle the data and recompute
# the mean. We can create one permuted sample with the following code:
  
dat <- c(smokers,nonsmokers)
shuffle <- sample( dat )
smokersstar <- shuffle[1:N]
nonsmokersstar <- shuffle[(N+1):(2*N)]
mean(smokersstar)-mean(nonsmokersstar)

#The last value is one observation from the null distribution we will
# construct. Set the seed at 1, and then repeat the permutation 1,000 times
# to create a null distribution. What is the permutation derived p-value for
# our observation?
set.seed(1)
null <- replicate(1000, {
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  mean(smokersstar)-mean(nonsmokersstar)
})
( sum( abs(null) >= abs(obs)) +1 ) / ( length(null)+1 ) 

#if we used median instead of mean
N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obs <- median(smokers) - median(nonsmokers)

set.seed(1)
null <- replicate(1000, {
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  median(smokersstar)-median(nonsmokersstar)
})
( sum( abs(null) >= abs(obs)) +1 ) / ( length(null)+1 ) 

