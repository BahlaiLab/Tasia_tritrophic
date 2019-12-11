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
#and spcode and soiltype and datacode
hoppers1$SPCODE<-NULL
hoppers1$SOILTYPE<-NULL
hoppers1$DATACODE<-NULL
hoppers1$COMMENTS<-NULL

#so it's apparent now that there's plenty of typoes in species names (MY NEMESIS)
# let's start by making sure they're consistently capitalized
library(Hmisc)
hoppers1$SPECIES<-capitalize(as.character(hoppers1$SPECIES))

#so what does our species list look like now?
species.list<-sort(unique(hoppers1$SPECIES))
species.list

#take care of the typographical errors, multiple ways of saying spp

hoppers1$SPECIES<-gsub("Ageneotett deorum", "Ageneotettix deorum", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Arphia species", "Arphia spp.", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Arphia xanthopterara", "Arphia xanthoptera", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Boopedon auriventr", "Boopedon auriventris", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Arphia xanthopte", "Arphia xanthoptera", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Brachystol magna", "Brachystola magna", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Campylacan olivacea", "Campylacantha olivacea", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Hadrotetti trifascia", "Hadrotettix trifasciatus", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Hesperotet speciosus", "Hesperotettix speciosus", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Hesperotet species", "Hesperotettix spp.", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Hesperotet spp.", "Hesperotettix spp.", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Hesperotet viridis", "Hesperotettix viridis", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Melanoplus bivittatu", "Melanoplus bivittatus", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Melanoplus bivittatuss", "Melanoplus bivittatus", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Melanoplus different", "Melanoplus differentialis", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Melanoplus femurrubr", "Melanoplus femurrubrum", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Melanoplus femurrubrumum", "Melanoplus femurrubrum", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Melanoplus sanguinip", "Melanoplus sanguinipes", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Melanoplus sanguinipeses", "Melanoplus sanguinipes", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Melanoplus species", "Melanoplus spp.", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Mermiria bivitatta", "Mermiria bivittata", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Mermiria species", "Mermiria spp.", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Orphullela speciosa", "Orphulella speciosa", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Paratylotr brunneri", "Paratylotropidia brunneri", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Paratylota brunneri", "Paratylotropidia brunneri", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Pardalopho apiculata", "Pardalophora apiculata", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Pardalopho haldemani", "Pardalophora haldemani", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Pardalopho spp.", "Pardalophora spp.", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Phoetaliot nebrascen", "Phoetaliotes nebrascensis", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Schistocer lineata", "Schistocerca lineata", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Schistocer obscura", "Schistocerca obscura", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Syrbula admirabilisis", "Syrbula admirabilis", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Syrbula admirabil", "Syrbula admirabilis", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Unknown ", "Unknown", hoppers1$SPECIES)

#so what does our species list look like after cleaning?
species.list<-sort(unique(hoppers1$SPECIES))
species.list


#so 58 species. That was a long time to get there, #otherpeoplesdata. Let's use reshape2 to
#find out what our most abundant species are, by year
#but first! R is not seeing the total column as numeric.
hoppers1$TOTAL<-as.numeric(hoppers1$TOTAL)
#and guess what?! site codes switch between capitalization patterns
hoppers1$WATERSHED <- toupper(hoppers1$WATERSHED)
#also, the hoppers data was all about disturbance regimes, but the intermediate disturbances
#ie 2 and 4 year treatments are probably out of sync with anything going on in the primary 
#productivity plots, so let's cut those out, and then divide the data into grazed/ungrazed (by mammmals)
hoppers1<-hoppers1[which(hoppers1$WATERSHED!="002C"),]
hoppers1<-hoppers1[which(hoppers1$WATERSHED!="002D"),]
hoppers1<-hoppers1[which(hoppers1$WATERSHED!="004B"),]
hoppers1<-hoppers1[which(hoppers1$WATERSHED!="004F"),]
hoppers1<-hoppers1[which(hoppers1$WATERSHED!="N04A"),]
hoppers1<-hoppers1[which(hoppers1$WATERSHED!="N04D"),]

summary.hoppers.by.species<-ddply(hoppers1, c("SPECIES"), summarise,
                       TOTAL=sum(TOTAL))
summary.hoppers.by.year<-ddply(hoppers1, c("RECYEAR"), summarise,
                                  TOTAL=sum(TOTAL))
summary.hoppers.by.watershed<-ddply(hoppers1, c("WATERSHED"), summarise,
                               TOTAL=sum(TOTAL))

#sum of all hoppers, by year, watershed
summary.hoppers.total<-ddply(hoppers1, c("RECYEAR", "WATERSHED"), summarise,
                             TOTAL=sum(TOTAL))

#get sum of each species by year, watershed
summary.hoppers<-ddply(hoppers1, c("RECYEAR", "WATERSHED", "SPECIES"), summarise,
                         TOTAL=sum(TOTAL))

#two most common species are: 	Phoetaliotes nebrascensis, Orphulella speciosa

P.nebrascensis<-summary.hoppers[which(summary.hoppers$SPECIES=="Phoetaliotes nebrascensis"),]
P.nebrascensis$SPECIES<-NULL
colnames(P.nebrascensis)[colnames(P.nebrascensis)=="TOTAL"] <- "P.nebrascensis"
O.speciosa<-summary.hoppers[which(summary.hoppers$SPECIES=="Orphulella speciosa"),]
O.speciosa$SPECIES<-NULL
colnames(O.speciosa)[colnames(O.speciosa)=="TOTAL"] <- "O.speciosa"

#merge those data in
summary.hoppers.total<-merge(summary.hoppers.total, P.nebrascensis, by=c("RECYEAR", "WATERSHED"))
summary.hoppers.total<-merge(summary.hoppers.total, O.speciosa, by=c("RECYEAR", "WATERSHED"), all.x = TRUE)

#finally, there's one year that no O.speciosa was recoded in a plot- an implied zero
#let's make it an explicit zero
summary.hoppers.total[is.na(summary.hoppers.total)] <- 0

#now we just need to divide the data into usable sets with year, response
#for each treatment, we have grazed and ungrazed, total, and the two species of grasshoper, 
#so 6 sets
hoppers.grazed<-summary.hoppers.total[which(grepl("N", summary.hoppers.total$WATERSHED)),]
hoppers.ungrazed<-summary.hoppers.total[which(!grepl("N", summary.hoppers.total$WATERSHED)),]

hoppers.grazed$WATERSHED<-NULL
hoppers.ungrazed$WATERSHED<-NULL

hoppers.grazed.total<-hoppers.grazed[1:2]
hoppers.grazed.p.n<-hoppers.grazed[c(1,3)]
hoppers.grazed.o.s<-hoppers.grazed[c(1,4)]

hoppers.ungrazed.total<-hoppers.ungrazed[1:2]
hoppers.ungrazed.p.n<-hoppers.ungrazed[c(1,3)]
hoppers.ungrazed.o.s<-hoppers.ungrazed[c(1,4)]

#all right,here we go, write the data

write.csv(hoppers.grazed.total, file="cleaned_data/Konza_herbivore_grazed_grasshopper_total.csv")
write.csv(hoppers.grazed.p.n, file="cleaned_data/Konza_herbivore_grazed_grasshopper_pn.csv")
write.csv(hoppers.grazed.o.s, file="cleaned_data/Konza_herbivore_grazed_grasshopper_os.csv")
write.csv(hoppers.ungrazed.total, file="cleaned_data/Konza_herbivore_ungrazed_grasshopper_total.csv")
write.csv(hoppers.ungrazed.p.n, file="cleaned_data/Konza_herbivore_ungrazed_grasshopper_pn.csv")
write.csv(hoppers.ungrazed.o.s, file="cleaned_data/Konza_herbivore_ungrazed_grasshopper_os.csv")

#ok,time for the small mammals. This data is in a different format from the grasshoppes but at least
#it seems to mostly be taken in the same spaces.
mammals<-read.csv(file="https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-knz.88.7&entityid=1ced8529601926470f68c1d5eb708350", 
                  header=T, na.strings=c("",".","NA"))
#get totals so we can get rid of the extra columns

mammals$TOTAL<- rowSums(mammals[7:20])

#cull out the columns we don't need: Pm and Pl are our two most abundant species
mammals1<-mammals[c(4,5,6,7,14,21)]

#we want to sum things over the two samplings each year

summary.mammals<-ddply(mammals1, c("RECYEAR", "WATERSHED.LINE"), summarise,
                               TOTAL=sum(TOTAL), Pl=sum(Pl),Pm=sum(Pm))

#cull out treatments with 2 and 4 year fire frequencies
summary.mammals1<-summary.mammals[which(!grepl("2", summary.mammals$WATERSHED.LINE)),]
summary.mammals2<-summary.mammals1[which(!grepl("4", summary.mammals1$WATERSHED.LINE)),]


mammals.grazed<-summary.mammals2[which(grepl("N", summary.mammals2$WATERSHED.LINE)),]
mammals.ungrazed<-summary.mammals2[which(!grepl("N", summary.mammals2$WATERSHED.LINE)),]

mammals.grazed$WATERSHED.LINE<-NULL
mammals.ungrazed$WATERSHED.LINE<-NULL

mammals.grazed.total<-mammals.grazed[1:2]
mammals.grazed.pl<-mammals.grazed[c(1,3)]
mammals.grazed.pm<-mammals.grazed[c(1,4)]

mammals.ungrazed.total<-mammals.ungrazed[1:2]
mammals.ungrazed.pl<-mammals.ungrazed[c(1,3)]
mammals.ungrazed.pm<-mammals.ungrazed[c(1,4)]


#all right,here we go, write the data

write.csv(mammals.grazed.total, file="cleaned_data/Konza_omnivore_grazed_mammal_total.csv")
write.csv(mammals.grazed.pl, file="cleaned_data/Konza_omnivore_grazed_mammal_pl.csv")
write.csv(mammals.grazed.pm, file="cleaned_data/Konza_omnivore_grazed_mammal_pm.csv")
write.csv(mammals.ungrazed.total, file="cleaned_data/Konza_omnivore_ungrazed_mammal_total.csv")
write.csv(mammals.ungrazed.pl, file="cleaned_data/Konza_omnivore_ungrazed_mammal_pl.csv")
write.csv(mammals.ungrazed.pm, file="cleaned_data/Konza_omnivore_ungrazed_mammal_pm.csv")
