setwd('/home/mfc/cwd/EDA/data')
require("RDS")
require(ggplot2)

NEI <- readRDS('summarySCC_PM25.rds')
SCC <- readRDS('Source_Classification_Code.rds')

indx <- grep("Vehicle",SCC$EI.Sector)
NEI.balt <- NEI[(NEI$SCC %in% SCC$SCC[indx]),]

balt <- NEI.balt[NEI.balt$fips == "24510",]


plt <- ggplot(data = balt, aes(x = year, y = Emissions ) ) + geom_point() +
        #stat_summary(fun.y = mean, geom="line") +
        geom_smooth(aes(x=year,y=Emissions),data=balt ,method ="lm")

print(plt)




total <- data.frame(year = seq(1999,2008,3), pol = rep(0,4))
n=1
for ( i in seq(1999,2008,3)){
        a <- (sum(NEI.balt$Emissions[NEI.balt$year == i]))
        total$pol[n] =a
        n = n+1

}

lm.fit <- lm(pol~., data =total)

plot(total$year,total$pol,main = "plot4",type= "b")
abline(lm.fit,col='red')
