## Summary of Escapement counts and ASL data
## author: "Jorge F. Cornejo-Donoso"
## Date: Jan 04, 2018
## Goal: Produce a table with a summary of escapement counts by
##      Species, SASAP.Region, LocationID and Year.


rm(list = ls())
require(dplyr)
library(dtplyr)
library(data.table)
library(ggplot2)
require("scales")

sp <- c("sockeye")#, "pink", "coho", "chinook", "chum")

## Using the ASL data on KNB
###  Jeanette Clark, Rich Brenner, and Bert Lewis. 2018. Compiled age, sex, and length data for Alaskan salmon, 1922-2017. 
###  nowledge Network for Biocomplexity. urn:uuid:b10119dc-1e75-45fa-9efb-cc8f7f406b1d.
file = 'https://knb.ecoinformatics.org/knb/d1/mn/v2/object/urn%3Auuid%3A4c06a320-3087-4828-926b-123f06c51cf3'
asl <- fread(file, na.strings = c('', 'NA'), stringsAsFactors = F )

## Only sockeye from Bristol Bay
asl <- filter(asl, Species %in% sp, SASAP.Region == "Bristol Bay", is.na(Flag), !is.na(yday), !is.na(Salt.Water.Age),
              Length.Measurement.Type == "mid-eye to fork of tail")

asl <- mutate(asl, year=year(sampleDate), yday = yday(sampleDate), 
              totalAge = 1 + Salt.Water.Age + Fresh.Water.Age)

sockeyeBB <- filter(asl, Species == "sockeye", Sex %in% c("male", "female"), !is.na(Length)) %>%
                select(year, Length, Sex, Salt.Water.Age)

ggplot(data = sockeyeBB, aes(x=Length)) +
    geom_histogram() + facet_grid(Salt.Water.Age ~ Sex, scales = "free_y")

meansd <- summarize(group_by(sockeyeBB, Sex, Salt.Water.Age), n = n(), meanLength=mean(Length, na.rm = T), sdLength=sd(Length, na.rm = T))

write.csv(meansd, file="data/MeanSD_BB.csv")
write.csv(sockeyeBB, file="data/sockeyeBB_fishes.csv")
