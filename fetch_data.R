library("dplyr")
library("reshape2")
library("ggplot2")

sessionInfo()

if (!exists("storm.data")) {
  if (!file.exists("./data/StormData.csv.bz2")) {
    dir.create("./data")
    dataSrcUri <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    download.file(dataSrcUri, "./data/StormData.csv.bz2", "curl")
    
    conn <- file("./data/data_fetch_timestamp.txt")
    writeLines(paste("StormData downloaded from", dataSrcUri, "at", Sys.time()), conn)
    close(conn)
  }
  
  storm.data <- tbl_df(read.csv(bzfile("./data/StormData.csv.bz2"), stringsAsFactors = FALSE, na.strings = ""))
  
  format.exponent <- function(exp) {
    exp[is.na(exp)] = "0"
    exp <- gsub("h|H", "2", exp)
    exp <- gsub("k|K", "3", exp)
    exp <- gsub("m|M", "6", exp)
    exp <- gsub("b|B", "9", exp)
    exp <- gsub("\\+|\\?|\\-", "0", exp)
    as.numeric(exp)
  }
  
  cleaned.storm.data <- storm.data %>% 
    mutate(prop.dmg.exp = sapply(PROPDMGEXP, format.exponent), 
           prop.dmg = PROPDMG * (10^prop.dmg.exp), 
           crop.dmg.exp = sapply(CROPDMGEXP, format.exponent), 
           crop.dmg = CROPDMG * (10^crop.dmg.exp)) %>% 
    select(event.type = EVTYPE, fatalities = FATALITIES, injuries = INJURIES, prop.dmg, crop.dmg) %>% 
    group_by(event.type)
}

harmful.events <- cleaned.storm.data %>% 
  summarize(Fatalities = sum(fatalities), 
            Injuries = sum(injuries)) %>% 
  arrange(desc(Fatalities, Injuries))

harmful.events <- melt(head(harmful.events), id = "event.type", variable.name = "Severity")

harmful.events.plot <- ggplot(harmful.events, aes(reorder(event.type, -value), y = value, fill = Severity)) + 
  geom_bar(stat = "identity", position = "stack") + 
  scale_fill_manual(values = c("green3", "forestgreen")) + 
  xlab("Weather Event") + ylab("Total Occurences") + 
  ggtitle("Most Harmful US Weather Events (1950 - 2011)")

print(harmful.events.plot)



expensive.events <- cleaned.storm.data %>% 
  summarize(Property = sum(prop.dmg)/10^9, Agriculture = sum(crop.dmg)/10^9) %>% 
  arrange(desc(Agriculture, Property))

expensive.events <- melt(head(expensive.events), id.vars = "event.type", variable.name = "Category")

expensive.events.plot <- ggplot(expensive.events, aes(reorder(event.type, -value), y = value, fill = Category)) + 
  geom_bar(stat = "identity", position = "stack") + 
  scale_fill_manual(values = c("green3", "forestgreen")) + 
  xlab("Weather Event") + 
  ylab("Total Damage (USD Billions)") + 
  ggtitle("Most Expensive US Weather Events (1950 - 2011)")

print(expensive.events.plot) 
