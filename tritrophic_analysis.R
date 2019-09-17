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

#now let's write the intermediate cleaned data products into a folder

#we want to encode the information about site and trophic level into the file name
write.csv(grassmass.control.grass, file="cleaned_data/Konza_producer_control_grass.csv")
write.csv(grassmass.control.forbs, file="cleaned_data/Konza_producer_control_forbs.csv")
write.csv(grassmass.irrigated.grass, file="cleaned_data/Konza_producer_irrigated_grass.csv")
write.csv(grassmass.irrigated.forbs, file="cleaned_data/Konza_producer_irrigated_forbs.csv")

#ok, let's grasshopper this! the data has a BUNCH of issues we're going to have to address, 
#but let's talk about that later and bring it in first

hoppers<-read.csv(file="https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-knz.29.12&entityid=3fb352e2478f776517f7e880fe31b808", 
                    header=T, na.strings=c("",".","NA"))

summary(hoppers)

#oooh boy. well, our first problem is that the datasheet doesn't record zeros, just blanks
#but we can kind of get around it by assuming that the total column is correct.
#because of the way the data is recorded, we're going to have to make the big, horrible
#assumption that in the unit of 200 sweeps, we always got at least one grasshopper, because the 
#way this is actually set up, if a researcher went out and swept and got nothing, they actually would
#record nothing. I think we should do the top four or so species (let's see what it looks like when
#we actually get counts by species) so we can use evidence that other species were there in a sample
#to essentially generate the absences of the key species we select

#first, let's cull out data from before 1996, because we need a continuous series anyway, so this cuts out the 
#hole in the data

hoppers1<-hoppers[which(hoppers$RECYEAR>1995),]

#let's also cull out the breakdown by units of sweeps
hoppers1$S1<-NULL
hoppers1$S2<-NULL
hoppers1$S3<-NULL
hoppers1$S4<-NULL
hoppers1$S5<-NULL
hoppers1$S6<-NULL
hoppers1$S7<-NULL
hoppers1$S8<-NULL
hoppers1$S9<-NULL
hoppers1$S10<-NULL
#and spcode and soiltype
hoppers1$SPCODE<-NULL
hoppers1$SOILTYPE<-NULL

