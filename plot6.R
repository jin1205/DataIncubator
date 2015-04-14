library(ggplot2)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
NEI_Baltimore <- NEI[(NEI$fips == "24510"),]
NEI_LA <- NEI[(NEI$fips == "06037"),]

index <- grep("[Mm]otor", SCC$Short.Name)
Motor <- SCC[index,]
SCC_Motor <- Motor$SCC

NEI_Bal_Motor <- NULL
for( i in 1:length(SCC_Motor)) {
        subset <- NEI_Baltimore[(NEI_Baltimore$SCC == SCC_Motor[i]),]
        NEI_Bal_Motor <- rbind(NEI_Bal_Motor, subset)
}

s <- split(NEI_Bal_Motor, NEI_Bal_Motor$year)
Motor_Emi <- sapply(s, function(x) sum(x[,"Emissions"]))
Motor_Emi<- data.frame(names(Motor_Emi), unlist(Motor_Emi))
Motor_Emi <- cbind(Motor_Emi, "Baltimore City")
names(Motor_Emi) <- c("Year","Emissions", "City")
rownames(Motor_Emi) <- 1:nrow(Motor_Emi)

NEI_LA_Motor <- NULL
for( i in 1:length(SCC_Motor)) {
        subset <- NEI_LA[(NEI_LA$SCC == SCC_Motor[i]),]
        NEI_LA_Motor <- rbind(NEI_LA_Motor, subset)
}

s2<- split(NEI_LA_Motor, NEI_LA_Motor$year)
Motor_Emi2 <- sapply(s2, function(x) sum(x[,"Emissions"]))
Motor_Emi2<- data.frame(names(Motor_Emi2), unlist(Motor_Emi2))
Motor_Emi2 <- cbind(Motor_Emi2, "Los Angeles County")
names(Motor_Emi2) <- c("Year","Emissions", "City")
rownames(Motor_Emi2) <- 1:nrow(Motor_Emi2)

dataset<- rbind(Motor_Emi, Motor_Emi2)

qplot(Year,Emissions, data=dataset, col = City, geom = "point", main = "Emissions from motor 
      vehicle sources in Baltimore and Los Angeles")
dev.copy(png, file="plot6.png")
dev.off()