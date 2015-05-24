setwd('/home/mfc/cwd/EDA/data')
require("RDS")
require(ggplot2)

NEI <- readRDS('summarySCC_PM25.rds')
SCC <- readRDS('Source_Classification_Code.rds')

balt <- NEI[NEI$fips == "24510",]

plt <- ggplot(data = balt, aes(x = year, y = Emissions ) ) + #geom_point() +
         facet_grid(.~type) +  stat_summary(fun.y = mean, geom="line") +
        geom_smooth(aes(x=year,y=Emissions),data=balt ,method = "lm")

print(plt)

ggsave("../plot3.png")
