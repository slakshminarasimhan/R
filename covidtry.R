library("googleVis")

D=read.csv("D:\\Users\\Lakshmi Narasimhan\\PROJECTS\\WHO-COVID-19-global-data.csv", header=TRUE)

D$day <- as.Date(D$day, fomat="%Y/%m/%d")

m <- gvisMotionChart(D, idvar="Country", timevar="day", xvar = "Confirmed", yvar = "Deaths",  date.format="%Y/%m/%d")

plot(m)
