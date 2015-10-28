###############################################################################
# Calendar plot                                                               #
# This script will create a timeseries heatmap, plotted on a calendar         #
#                                                                             #
# Expected values:                                                            #
# Dataframe name = df                                                         #
# Column containing date field = $date                                        #
# Column containing the variable plotted = $freq                              #
#                                                                             #
# You can of course easily find -> replace any of the above                   #
###############################################################################

# Load libraries

library(zoo) # for the yearmon() function
library(ggplot2) # our plotting engine
library(scales) # extra for ggplot2
library(RColorBrewer) # nice palettes for our plot
library(lubridate) # better date handling
library(plyr)

# Clean the data of duplicate dates
df <- df %>% distinct(date)
# Get the month numeric value from the date field
df$month <- as.numeric(as.POSIXlt(df$date)$mon+1)

# Turn months into ordered factors to control the appearance/ordering
# in the presentation

df$monthf<-factor(df$month,
  levels = as.character(1:12), labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"), ordered = TRUE)

# Turn days into ordered factors to control the appearance/ordering
# in the presentation
df$weekday = as.POSIXlt(df$date)$wday
df$weekday[df$weekday==0] <- 7
df$weekdayf<-factor(df$weekday, levels = 1:7, labels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), ordered = TRUE)

# Calculate which week of the month a date falls into.
# First we create a factor, which cuts the data frame into month chunks

df$yearmonth<-as.yearmon(df$date)
df$yearmonthf<-factor(df$yearmonth)

# Find the "week of year" for each day
df$week <- as.numeric(format(df$date,"%W"))

# and now for each monthblock we normalize the week to start at 1
df<-ddply(df,.(yearmonthf),transform,monthweek=1+week-min(week))

# create ggplot object
p <- ggplot(data=df, aes(x=weekdayf, y=monthweek, fill=freq)) + geom_tile(color="black", guide=FALSE) + geom_text(aes(label = day(date))) + coord_fixed(ratio=1) + facet_wrap(~ monthf, ncol=4) + scale_fill_distiller(palette = "Spectral", trans="reverse") + scale_y_reverse(breaks=c(1:6,1), expand=c(0,0)) +
theme_bw() +
ylab("Week of the month") +
xlab("Day of the week") +
ggtitle("Conversion rate") +
theme(legend.position="bottom") +
labs(fill="Conversion rate")
