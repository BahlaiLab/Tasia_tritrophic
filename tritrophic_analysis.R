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
write.csv(grassmass.control.grass, file="cleaned_data/Konza_producer_control_grass.csv", row.names=FALSE)
write.csv(grassmass.control.forbs, file="cleaned_data/Konza_producer_control_forbs.csv", row.names=FALSE)
write.csv(grassmass.irrigated.grass, file="cleaned_data/Konza_producer_irrigated_grass.csv", row.names=FALSE)
write.csv(grassmass.irrigated.forbs, file="cleaned_data/Konza_producer_irrigated_forbs.csv", row.names=FALSE)

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

write.csv(hoppers.grazed.total, file="cleaned_data/Konza_herbivore_grazed_grasshopper_total.csv", row.names=FALSE)
write.csv(hoppers.grazed.p.n, file="cleaned_data/Konza_herbivore_grazed_grasshopper_pn.csv", row.names=FALSE)
write.csv(hoppers.grazed.o.s, file="cleaned_data/Konza_herbivore_grazed_grasshopper_os.csv", row.names=FALSE)
write.csv(hoppers.ungrazed.total, file="cleaned_data/Konza_herbivore_ungrazed_grasshopper_total.csv", row.names=FALSE)
write.csv(hoppers.ungrazed.p.n, file="cleaned_data/Konza_herbivore_ungrazed_grasshopper_pn.csv", row.names=FALSE)
write.csv(hoppers.ungrazed.o.s, file="cleaned_data/Konza_herbivore_ungrazed_grasshopper_os.csv", row.names=FALSE)

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

write.csv(mammals.grazed.total, file="cleaned_data/Konza_omnivore_grazed_mammal_total.csv", row.names=FALSE)
write.csv(mammals.grazed.pl, file="cleaned_data/Konza_omnivore_grazed_mammal_pl.csv", row.names=FALSE)
write.csv(mammals.grazed.pm, file="cleaned_data/Konza_omnivore_grazed_mammal_pm.csv", row.names=FALSE)
write.csv(mammals.ungrazed.total, file="cleaned_data/Konza_omnivore_ungrazed_mammal_total.csv", row.names=FALSE)
write.csv(mammals.ungrazed.pl, file="cleaned_data/Konza_omnivore_ungrazed_mammal_pl.csv", row.names=FALSE)
write.csv(mammals.ungrazed.pm, file="cleaned_data/Konza_omnivore_ungrazed_mammal_pm.csv", row.names=FALSE)

# okay now the data is clean and in the format we need.  -_- At least for Konza.

# That was somethin'. but- onward! we need to clean three more sites, haha :'S

#######################################
#let's do Hubbard Brook next

#ugh, yeah the tree phenology data isn't going to work, but we could do two tropic levels- 
#leps and birds, and maybe use litter deposition for a proxy for productivity?

hub.leps<-read.csv(file="https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-hbr.82.7&entityid=c32446bad7211a5a1cfabf70c89baec8", 
                    header=T, na.strings=c("",".","NA"))
summary(hub.leps)
#I think what's most relevant here is the number of individuals from all taxa and the biomass.
#there is also four species of tree these data were collected from, so maybe divide up on that.
#looks like there's probably unequal sampling between years so will have to account for that

summary.hub.leps<-ddply(hub.leps, c("Year", "GridLetter", "GridNumber", "TreeSpecies"), summarise,
                       individuals=mean(NumberIndividuals), biomass=mean(biomass))

#looks like there's some missing data for sampling location, so let's ditch that

summary.hub.leps<-summary.hub.leps[complete.cases(summary.hub.leps),]

#strip out unnecessary columns
summary.hub.leps$GridLetter<-NULL
summary.hub.leps$GridNumber<-NULL

#Divide it up by tree species
hub.leps.viburnum<-summary.hub.leps[which(summary.hub.leps$TreeSpecies=="4"),]
hub.leps.st.maple<-summary.hub.leps[which(summary.hub.leps$TreeSpecies=="3"),]
hub.leps.su.maple<-summary.hub.leps[which(summary.hub.leps$TreeSpecies=="2"),]
hub.leps.beech<-summary.hub.leps[which(summary.hub.leps$TreeSpecies=="1"),]

#strip out the tree species column
hub.leps.viburnum$TreeSpecies<-NULL
hub.leps.st.maple$TreeSpecies<-NULL
hub.leps.su.maple$TreeSpecies<-NULL
hub.leps.beech$TreeSpecies<-NULL

#oh my god, there's no data reported for viburnum or striped maple? Ok, let's leave (ha)
#those out :/
#divide data into biomass and abundance

hub.leps.maple.biomass<-hub.leps.su.maple[c(1,3)]
hub.leps.maple.individuals<-hub.leps.su.maple[c(1,2)]
hub.leps.beech.biomass<-hub.leps.beech[c(1,3)]
hub.leps.beech.individuals<-hub.leps.beech[c(1,2)]

#and write it

write.csv(hub.leps.maple.biomass, file="cleaned_data/Hubbard_herbivore_maple_biomass.csv", row.names=FALSE)
write.csv(hub.leps.maple.individuals, file="cleaned_data/Hubbard_herbivore_maple_abundance.csv", row.names=FALSE)
write.csv(hub.leps.beech.biomass, file="cleaned_data/Hubbard_herbivore_beech_biomass.csv", row.names=FALSE)
write.csv(hub.leps.beech.individuals, file="cleaned_data/Hubbard_herbivore_beech_abundance.csv", row.names=FALSE)

#ok, as a proxy for plant productivity, which is scattered accross numerous datasets,
#hbr makes litterfall available, so let's take a look-see at the coverage of these data

hub.litter<-read.csv(file="https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-hbr.49.6&entityid=4f2ea33823ced1a36fb8f18c757aec6e", 
                   header=T, na.strings=c("",".","NA", "-9999", "-9999.99", "-9999.9", "-99.00"))
summary(hub.litter)

#so I was hoping to break it out by species dry mass, but the data coverage is not super, so total dry
#mass may be the thing. Since the lep data is only taken in beech and maple, let's just use hardwood
#forest sites, and sites withour Ca addition

hub.litter1<-hub.litter[which(hub.litter$TRTMT=="noCA"&hub.litter$COMP=="HW"),]
#pull out the columns we need
hub.litter2<- hub.litter1[c(3,4,5,6,9)]

#looks like there's some missing data for sampling location, so let's ditch that

hub.litter2<-hub.litter2[complete.cases(hub.litter2),]

#ok, differing number of samples each year. Sheesh. ok, let's see if the number of samples matters

summary.hub.litter<-ddply(hub.litter2, c("YEAR", "SITE", "ELEV"), summarise,
                        Avlitter=mean(DRY_MASS), totlitter=sum(DRY_MASS), samples=length(DRY_MASS))

#looks like number of samples is strongly positively correlated with total samples but average leaf litter
#is not, so let's use the average, and there's no apparent site or elevation effects so they can be our 
#subsamples, and we'll just have one response measured for this level

hub.litter.mass<-summary.hub.litter[c(1,4)]

#and write it
write.csv(hub.litter.mass, file="cleaned_data/Hubbard_producer_litter_mass.csv", row.names=FALSE)

#all righty- birbs. This table is fairly low complexity, but there are a few things that make 
#you go hmm. First, it's from a usable format, also, there are 't's all theough it
#to indicate trace numbers of birds. hmm. let's fix this up

hub.birb<-read.csv(file="https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-hbr.81.7&entityid=e1b527e8d41b314cb19209d3cf1aeed1", 
                     header=T, na.strings=c(""))
summary(hub.birb)

#whooboy, let's transpose this

hub.birb.trans<-dcast(melt(hub.birb, id="Bird.Species"), variable~Bird.Species)

summary(hub.birb.trans)

#ok, let's clean this up!
#first column is Year- rename it, clear out all the Xs in the year name from the import
colnames(hub.birb.trans)[colnames(hub.birb.trans)=="variable"] <- "Year"
hub.birb.trans$Year<-as.factor(gsub("X", "", hub.birb.trans$Year))

#now let's get rid of all those trace birds

hub.birb.trans[, 2:37] <- apply(hub.birb.trans[, 2:37], 2, 
                                function(x) as.numeric(gsub("t", "0", x)))
#let's also get the NAs- we'll assume if a bird isn't recorded, it wasn't there

hub.birb.trans[is.na(hub.birb.trans)] <- 0
summary(hub.birb.trans)

#let's get totals- lat's find the two most common birds, and the total birds

colSums(hub.birb.trans[2:37])
hub.birb.trans$total<-rowSums(hub.birb.trans[2:37])
#red eyed vireo and american redstart are our guys

hub.birds.total<-hub.birb.trans[c(1,38)]
hub.birds.redstart<-hub.birb.trans[c(1,2)]
hub.birds.vireo<-hub.birb.trans[c(1,23)]

#and write it

write.csv(hub.birds.total, file="cleaned_data/Hubbard_omnivore_bird_total.csv", row.names=FALSE)
write.csv(hub.birds.redstart, file="cleaned_data/Hubbard_omnivore_bird_redstart.csv", row.names=FALSE)
write.csv(hub.birds.vireo, file="cleaned_data/Hubbard_omnivore_bird_vireo.csv", row.names=FALSE)


##########
# and now, North Temperate lakes!

# Phytoplankton data is discontinouous but chlorophyll runs 1984-2007 without gaps and has more
# overlap with zooplankton, and then, I guess we find a fish that likes to eat zooplankton?

#chlorophyll

ntl.chlor<-read.csv(file="https://lter.limnology.wisc.edu/file/11572/download?token=ZQoUgowd1gDw1WvJCddhkiPJ0jkkb4vvWLrODxLygsU", 
                   header=T, na.strings=c("",".","NA"))

summary(ntl.chlor)

#ok, lakes R and L are the only ones that have continuous measuremnts for chlorophyll A over the 1984-2007 period
#so let's pull them out for use

ntl.chlor1<-ntl.chlor[which(ntl.chlor$lakeid=="R"|ntl.chlor$lakeid=="L"),]
#pull out the columns we need
ntl.chlor2<- ntl.chlor1[c(1,3,4,6,11)]

# from looking at a pivot table, it looks like they didn't consistently sample at a depth greater than 6m
# after the early 90s, so let's cull out depths >6m because that would bias the sample 
ntl.chlor3<-ntl.chlor2[which(ntl.chlor2$depth<6.1),]

summary(ntl.chlor3)

#still a bunch of NAs and some negative values for chla- let's cull those out
ntl.chlor4<-ntl.chlor3[which(ntl.chlor3$chla>=0),]

summary(ntl.chlor4)
