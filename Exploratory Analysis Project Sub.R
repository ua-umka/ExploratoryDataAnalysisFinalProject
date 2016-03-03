install.packages("dplyr")
library("dplyr") # for group_by function and pipeline operator (%>%)
setwd("C:/Users/a/Desktop/DS/4 Exploratory Data Analysis/Practice/Exploratory Data Analysis")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## 1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
## Using the base plotting system, make a plot showing the total PM2.5 emission from all sources 
## for each of the years 1999, 2002, 2005, and 2008.

graphData <- NEI %>% 
      group_by(year) %>%
      summarise(total = sum(Emissions))
png("ExplProjPlot1.png", width = 480, height = 480)
barplot(graphData$total/1e6, main = "Total PM2.5 emission from all sources by years", 
        col = "black", xlab="Year", ylab = "PM2.5 (million tons)",
        names.arg = graphData$year)
dev.off()

## 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008?
## Use the base plotting system to make a plot answering this question.
baltimore <- NEI %>%
      subset(fips == "24510") %>%
      group_by(year) %>%
      summarise(total = sum(Emissions))
png("ExplProjPlot2.png", width = 480, height = 480)
barplot(baltimore$total, main = "Total PM2.5 emission in the Baltimore City by years", 
        col = "black", xlab="Year", ylab = "PM2.5 (tons)",
        names.arg = graphData$year)
dev.off()

## 3. Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
## which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
## Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make 
## a plot answer this question.
baltimore2 <- NEI %>% subset(fips == "24510")
baltimorByType <- aggregate(Emissions ~ type + year, data = baltimore2, sum)
install.packages("ggplot2")
library(ggplot2)
png("ExplProjPlot3.png", width = 480, height = 480)
qplot(year, Emissions, data = baltimorByType, col = type, geom = c("point", "smooth")) + 
      labs(title = "Total PM2.5 emission in the Baltimore City by types")
dev.off()

## 4. Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
subSCC <- SCC[grep("Coal", SCC$EI.Sector),c("SCC","EI.Sector")]
mergedData <- merge(NEI,subSCC, by.x="SCC",by.y="SCC",all = F)
aggMrgData <- aggregate(Emissions ~ year, data = mergedData, sum)
png("ExplProjPlot4.png", height = 480, width = 480)
barplot(aggMrgData$Emissions/1e5, main = "PM2.5 coal emission by years in the United States", 
        xlab="Year", ylab = "PM2.5 (1e+05 tons)",
        names.arg = graphData$year)
dev.off()

## 5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
baltimore3 <- baltimore2 %>% subset(type == "ON-ROAD")
baltimorAgg <- aggregate(Emissions ~ year, data = baltimore3, sum)
png("ExplProjPlot5.png", width = 480, height = 480)
qplot(year, Emissions, data = baltimorAgg,  geom = c("point", "smooth")) +
      labs(title = "PM2.5 emission from vehicle in the Baltimore City")
dev.off()

## 6. Compare emissions from motor vehicle sources in Baltimore City with emissions 
## from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
## Which city has seen greater changes over time in motor vehicle emissions?
LB <- NEI %>%
      subset(fips %in% c("06037", "24510") & type == "ON-ROAD") %>%
      aggregate(Emissions ~ year + fips, data = ., FUN = .%>% sum)
LB[LB$fips == "06037",]$fips <- "Los Angeles"
LB[LB$fips == "24510",]$fips <- "Baltimore"
png("ExplProjPlot6.png", width = 480, height = 480)
qplot(year, Emissions,data = LB, geom = "smooth", col = fips) +
      labs(title = "PM2.5 emission from vehicle in the Baltimore City and Los Angeles") +
      xlab("Year") + ylab("PM2.5 (tons)")
dev.off()