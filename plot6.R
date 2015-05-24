setwd('/home/mfc/cwd/EDA/data')
require("RDS")
require(ggplot2)

NEI <- readRDS('summarySCC_PM25.rds')
SCC <- readRDS('Source_Classification_Code.rds')

indx <- grep("Vehicle",SCC$EI.Sector)
NEI.balt <- NEI[(NEI$SCC %in% SCC$SCC[indx]),]

balt <- NEI.balt[NEI.balt$fips == "24510",]







total <- data.frame(year = seq(1999,2008,3), pol = rep(0,4))
n=1
for ( i in seq(1999,2008,3)){
        a <- (sum(balt$Emissions[balt$year == i]))
        total$pol[n] =a
        n = n+1

}


lm.fit <- lm(pol~., data =total)

plot(total$year,total$pol,main = "plot4",type= "b")
abline(lm.fit,col='red')


LA <- NEI.balt[NEI.balt$fips == "06037",]


total.la <- data.frame(year = seq(1999,2008,3), pol = rep(0,4))
n=1
for ( i in seq(1999,2008,3)){
        a <- (sum(LA$Emissions[LA$year == i]))
        total.la$pol[n] =a
        n = n+1

}

lm.fit <- lm(pol~., data =total.la)

plot(total.la$year,total.la$pol,main = "plot4",type= "b")
abline(lm.fit,col='red')


plt <- ggplot(data = balt, aes(x = year, y = Emissions ) ) + #geom_point(col='blue') +
        geom_smooth(aes(x=year,y=Emissions),data=balt ,method ="lm",col = 'blue')+
       # geom_point(data= LA ,aes(x = year, y =Emissions),col='red') +
        geom_smooth(aes(x=year,y=Emissions),data=LA,method = 'lm',col = 'red')

print(plt)



plt2 <- ggplot(data = total, aes(x = year, y = pol ) ) + #geom_point(col='blue') +
        geom_smooth(aes(x=year,y=pol),data=total ,method ="lm",col = 'blue')+
        geom_point(data= total.la ,aes(x = year, y =pol),col='red') +
        geom_smooth(aes(x=year,y=pol), data=total.la ,method = 'lm',col = 'red')
print(plt2)


ggsave("../plot6.png",plt2)
