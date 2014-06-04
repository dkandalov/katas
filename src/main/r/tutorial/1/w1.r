oz <- read.table("~/IdeaProjects/katas/src/main/r/tutorial/1/data.csv", header = T, sep=",")
oz[1:2,] # first 2 rows
nrow(oz) # number of rows
oz[(nrow(oz)-1):nrow(oz),] # last 2 rows
oz[47,]$Ozone
nrow(subset(oz, is.na(Ozone))) # amount of rows where Ozone is NA
mean(subset(oz, !is.na(Ozone))$Ozone)
mean(subset(oz, Ozone > 31 & Temp > 90)[["Solar.R"]])
mean(subset(oz, Month == 6)$Temp)
max(subset(oz, Month == 5 & !is.na(Ozone))$Ozone)



