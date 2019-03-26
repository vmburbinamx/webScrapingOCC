#options(error = browser())
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(googlesheets)

#Load function
source("getDataFromOddsChecker.R")

#Load World Cup Winner table
worldCupWinner <- oddschecker("football/world-cup/brazil-v-mexico/correct-score")
#Set row name as an additional column
worldCupWinner <- add_rownames(worldCupWinner)
names(worldCupWinner)[1] <- "Country"
#Transpose data
worldCupWinner <- worldCupWinner %>% gather("Source", "Bet", 2:ncol(worldCupWinner))
#remove empty values
worldCupWinner <- worldCupWinner %>% filter(Bet != "")
#Get first element into a separate column
numbersInSeparateColumns <- str_split(worldCupWinner$Bet, "/")
firstElement <- c()
for (listItem in seq_along(numbersInSeparateColumns)) {
  listItemValue <- numbersInSeparateColumns[[listItem]][[1]]
  firstElement <- c(firstElement, listItemValue)
}
firstElement <- as.numeric(firstElement)
firstElement <- as_data_frame(firstElement)
worldCupWinner <- bind_cols(worldCupWinner, firstElement)
names(worldCupWinner)[4] <- "First"
remove(firstElement, listItem, listItemValue)

#Get second element into a separate column
secondElement <- c()
for (listItemTwo in seq_along(numbersInSeparateColumns)) {
  listItemValue <- numbersInSeparateColumns[[listItemTwo]][[2]]
  if (listItemValue == "") {
    listItemValue <- NA
  }
  secondElement <- c(secondElement,listItemValue)
}
secondElement <- as.numeric(secondElement)
secondElement <- as_data_frame(secondElement)
worldCupWinner <- bind_cols(worldCupWinner, secondElement)
names(worldCupWinner)[5] <- "Second"
remove(secondElement, listItemTwo, listItemValue, numbersInSeparateColumns)

#Get factor
# worldCupWinner$Factor <- worldCupWinner$First/worldCupWinner$Second
worldCupWinner <- worldCupWinner %>% mutate(Factor = First/Second)
worldCupWinner <- worldCupWinner %>% mutate(RevFactor = 1/ Factor)

selection <- worldCupWinner %>% group_by(Country) %>% summarise(total = n())
selection <- selection %>% filter(total > 25) %>% select(Country)

#Create a chart
# theGroup <- c("Saudi Arabia", "Tunisia", "Costa Rica", "Morocco")
theGroup <- c("Draw 1-1")
# theGroup <- c("France 5-1")
theChartData <- worldCupWinner %>% filter(Country %in% selection$Country)
theChart <- ggplot(theChartData, aes(x = Country, y = RevFactor, Color = Country)) +
  geom_boxplot() +
  scale_x_discrete() + 
  geom_jitter() +
  #ggtitle(paste("¿Qué dicen las apuestas? Última consulta:", Sys.time())) +
  ggtitle(paste("El grupo de México:", Sys.Date())) +
  theme_wsj()
print(theChart)
max_Values <- theChartData %>% filter(Country != "Each-way terms") %>% group_by(Country) %>% summarise(max(RevFactor))
#View(max_Values)
max_Values <- max_Values %>% mutate(theDate = Sys.time(), global = "Global", color = `max(RevFactor)`) %>% top_n(20)
# Send to google sheets

#Start a new session
#gs_auth(new_user = TRUE)

#Set parameters
theSourceGoogleSheet <- gs_key("1zXDNMZ9E4HsklJVVBDnXJ0rPSqYOJfV9vxpJq1xAZTo", lookup = FALSE, visibility = "private")

#Create a copy from an existing google sheet (name = "Copy of thiIsATest")
myCopy <- gs_edit_cells(ss = theSourceGoogleSheet, ws = "data", input = max_Values, anchor = "A3", col_names = FALSE)

#rm(list = ls())
