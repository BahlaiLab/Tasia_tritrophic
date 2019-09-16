#load bad breakup script
source_github <- function(u) {
  # load package
  require(RCurl)
  
  # read script lines from website
  script <- getURL(u, ssl.verifypeer = FALSE)
  
  # parase lines and evaluate in the global environment
  eval(parse(text = script))
}

source("https://raw.githubusercontent.com/BahlaiLab/bad_breakup_2/master/R_model/bad_breakup_script.R")

#let's clean all these data up!
#general procedure- import, check, fix obvious errors
# then get summaries of our metrics of interest, with subsets on treatments which would
#affect the dependant variables.

library(plyr)
# Konza prairie- let's import the data

#import data, assuming both blanks and periods are null values
grassmass<-read.csv(file="https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-knz.72.9&entityid=d7d500227665f76533332ebade88deeb", 
                    header=T, na.strings=c("",".","NA"))


#do some checks to see if R read the data correctly
summary(grassmass)
#we need to correct the transect values, in certain years, 'ni' was used as a treatment name instead of 'c' for control
grassmass$TRANSECT<-as.factor(gsub("ni", "c", grassmass$TRANSECT))

#we're interested in the LIVEGRASS and FORBS data. We need to turn each of these into a yearly metric
#we want a metric by plot, year and TRANSECT because we expect irrigation treatment will probably
#be very important re: plant productivity

summary.grassmass<-ddply(grassmass, c("RECYEAR", "PLOT", "TRANSECT"), summarise,
                               avg.LIVEGRASS=mean(LIVEGRASS), avg.FORBS=mean(FORBS))

#let's create subsets on transect, and then on the response variables we're interested in

grassmass.control<-summary.grassmass[which(summary.grassmass$TRANSECT=="c"),]
grassmass.irrigated<-summary.grassmass[which(summary.grassmass$TRANSECT=="i"),]

#get rid of the response variables we don't need- for bad breakup we need it stripped to 
#year, response

#control grass
grassmass.control.grass<-grassmass.control
grassmass.control.grass$avg.FORBS<-NULL
grassmass.control.grass$PLOT<-NULL
grassmass.control.grass$TRANSECT<-NULL

#control forbs
grassmass.control.forbs<-grassmass.control
grassmass.control.forbs$avg.LIVEGRASS<-NULL
grassmass.control.forbs$PLOT<-NULL
grassmass.control.forbs$TRANSECT<-NULL

#irrigated grass
grassmass.irrigated.grass<-grassmass.irrigated
grassmass.irrigated.grass$avg.FORBS<-NULL
grassmass.irrigated.grass$PLOT<-NULL
grassmass.irrigated.grass$TRANSECT<-NULL

#irrigated forbs
grassmass.irrigated.forbs<-grassmass.irrigated
grassmass.irrigated.forbs$avg.LIVEGRASS<-NULL
grassmass.irrigated.forbs$PLOT<-NULL
grassmass.irrigated.forbs$TRANSECT<-NULL

pyramid_plot(grassmass.irrigated.forbs)
