setwd('/home/mfc/cwd/EDA/data')
require("RDS")


NEI <- readRDS('summarySCC_PM25.rds')
SCC <- readRDS('Source_Classification_Code.rds')

total <- data.frame(year = seq(1999,2008,3), pol = rep(0,4))
n=1
for ( i in seq(1999,2008,3)){
        a <- (sum(NEI$Emissions[NEI$year == i]))
        total$pol[n] =a
        n = n+1

}

lm.fit <- lm(pol~., data =total)

plot(total$year,total$pol,main = "plot1",type= "b")
abline(lm.fit,col='red')
savePlot("plot1.png")

