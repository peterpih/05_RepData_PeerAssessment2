#
x <- read.csv("data/repdata-data-StormData.csv.bz2")
#
names(x)
#
# Subset FATALITIES > 0
#
fatal <- subset(x, x$FATALITIES > 0)
injury <- subset(x, x$INJURIES > 0)
fatal_injury <- subset(x, x$FATALITIES > 0 | x$INJURIES > 0)
#
# get the frequency of fatalities
#
n_fatalities = unique(fatalities$FATALITIES)
fatal_result <- data.frame()
for (i in n_fatalities){
    fatal_result <- rbind(fatal_result, data.frame(n=i, count=sum(fatalities$FATALITIES == i)))
}
fatal_result
#
# sort descending
#
fatal_result <- fatal_result[with(fatal_result, order(-fatal_result$n)),]
n_injuries = unique(injuries$INJURIES)
injury_result <- data.frame()
for (i in n_injuries){
    injury_result <- rbind(injury_result, data.frame(n=i, count=sum(injuries$INJURIES == i)))
}
injury_result
#
# sort descending
#
injury_result <- injury_result[with(injury_result, order(-injury_result$n)),]
#
# Get the top 20 reasons
#
fatalities_top30 <- data.frame()
for (i in 1:30){
    fatalities_top30 <- rbind(fatalities_top10, subset(fatalities, fatalities$FATALITIES == fatal_result$n[i]))
}

fatalities_top30[c("EVTYPE", "BGN_DATE", "END_DATE")]

top30_proportion = sum(fatalities_top30)/sum(fatalities) * 100


heat <- subset(fatalities, grepl("HEAT", fatalities$EVTYPE))
cold <- subset(fatalities, grepl("COLD", fatalities$EVTYPE))
t.test(heat$FATALITIES, cold$FATALITIES)$p.value

fatalities_big <- subset(fatalities, fatalities$FATALITIES> 500)

#
# Look at duration of event
#

sum(fatalities$FATALITIES)
sum(fatalities_top10$FATALITIES)

cold <- subset(fatalities, grepl("COLD", fatalities$EVTYPE))

#
# Fatalities by EVTYPE
#


result$EVTYPE <- as.character(result$EVTYPE)

evtype_reclassify <- function(x) {
    x$EVTYPE[grepl("HEAT", x$EVTYPE, ignore.case=TRUE)] <-"HEAT"
    x$EVTYPE[grepl("WARM", x$EVTYPE, ignore.case=TRUE)] <-"HEAT"
    x$EVTYPE[grepl("COLD", x$EVTYPE, ignore.case=TRUE)] <-"COLD"
    x$EVTYPE[grepl("LOW TEMPERATURE", x$EVTYPE, ignore.case=TRUE)] <-"COLD"
    x$EVTYPE[grepl("WIND", x$EVTYPE, ignore.case=TRUE)] <-"WIND"
    x$EVTYPE[grepl("FLOOD", x$EVTYPE, ignore.case=TRUE)] <-"FLOOD"
    x$EVTYPE[grepl("FLD", x$EVTYPE, ignore.case=TRUE)] <-"FLOOD"
    x$EVTYPE[grepl("SNOW", x$EVTYPE, ignore.case=TRUE)] <- "PRECIP"
    x$EVTYPE[grepl("RAIN", x$EVTYPE, ignore.case=TRUE)] <-"PRECIP"
    x$EVTYPE[grepl("STORM", x$EVTYPE, ignore.case=TRUE)] <-"PRECIP"
    x$EVTYPE[grepl("HAIL", x$EVTYPE, ignore.case=TRUE)] <-"PRECIP"
    x$EVTYPE[grepl("BLIZZARD", x$EVTYPE, ignore.case=TRUE)] <-"PRECIP"
    x$EVTYPE[grepl("WINTER", x$EVTYPE, ignore.case=TRUE)] <-"PRECIP"
    x$EVTYPE[grepl("SLEET", x$EVTYPE, ignore.case=TRUE)] <-"PRECIP"
    x$EVTYPE[grepl("WINTRY MIX", x$EVTYPE, ignore.case=TRUE)] <-"PRECIP"
    x$EVTYPE[grepl("DRIZZLE", x$EVTYPE, ignore.case=TRUE)] <-"PRECIP"
    x$EVTYPE[grepl("HURRICANE", x$EVTYPE, ignore.case=TRUE)] <-"HURRICANE"
    return(x)
}

#
# aggregate
#
result2 <- data.frame()
types <- unique(result$type)
for (i in types){
  type_subset <- subset(result, result$type == i)
  result2 <- rbind(result2, data.frame(type=i, n=sum(type_subset$n), fatal=sum(type_subset$fatal)))
}
result2 <- result2[with(result2, order(-result2$fatal)), ]

#
# Property Damage
#
paste(fatalities_top30$EVTYPE, fatalities_top30$PROPDMG, fatalities_top30$PROPDMGEXP)
#
# EXP is the scale to use
#