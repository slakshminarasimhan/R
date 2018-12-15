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
