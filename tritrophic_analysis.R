################################################################################
#LTER data

#let's clean all these data up!
#general procedure - import, check, fix obvious errors
#then get summaries of our metrics of interest, with subsets on treatments which would
#affect the dependent variables.

library(plyr)

# Konza prairie - let's import the data

#import data, assuming both blanks and periods are null values
#LTER Package ID: knb-lter-knz.72


grassmass<-read.csv(file="https://pasta.lternet.edu/package/data/eml/knb-lter-knz/72/18/d7d500227665f76533332ebade88deeb", 
                    header=T, na.strings=c("",".","NA"))


#do some checks to see if R read the data correctly
str(grassmass)
summary(grassmass)
levels(as.factor((grassmass$TRANSECT)))
#we need to correct the transect values, in certain years, 'ni' was used as a treatment name instead of 'c' for control
grassmass$TRANSECT <- as.factor(gsub("ni", "c", grassmass$TRANSECT))

#we're interested in the LIVEGRASS and FORBS data. We need to turn each of these into a yearly metric
#we want a metric by plot, year and TRANSECT because we expect irrigation treatment will probably
#be very important re: plant productivity

summary.grassmass <- ddply(grassmass, c("RECYEAR", "PLOT", "TRANSECT"), summarise,
                               avg.LIVEGRASS=mean(LIVEGRASS), avg.FORBS=mean(FORBS))

#let's create subsets on transect, and then on the response variables we're interested in

grassmass.control <- summary.grassmass[which(summary.grassmass$TRANSECT=="c"),]
grassmass.irrigated <- summary.grassmass[which(summary.grassmass$TRANSECT=="i"),]

#get rid of the response variables we don't need- for bad breakup we need it stripped to 
#year, response

#control grass
grassmass.control.grass <- grassmass.control
grassmass.control.grass$avg.FORBS <- NULL
grassmass.control.grass$PLOT <- NULL
grassmass.control.grass$TRANSECT <- NULL
summary(grassmass.control.grass)

#there is a missing value in 2017, so let's remove it
grassmass.control.grass <- grassmass.control.grass[complete.cases(grassmass.control.grass),]
summary(grassmass.control.grass)

#control forbs
grassmass.control.forbs <- grassmass.control
grassmass.control.forbs$avg.LIVEGRASS <- NULL
grassmass.control.forbs$PLOT <- NULL
grassmass.control.forbs$TRANSECT <- NULL
summary(grassmass.control.forbs)

#irrigated grass
grassmass.irrigated.grass <- grassmass.irrigated
grassmass.irrigated.grass$avg.FORBS <- NULL
grassmass.irrigated.grass$PLOT <- NULL
grassmass.irrigated.grass$TRANSECT <- NULL
summary(grassmass.irrigated.grass)

#irrigated forbs
grassmass.irrigated.forbs <- grassmass.irrigated
grassmass.irrigated.forbs$avg.LIVEGRASS <- NULL
grassmass.irrigated.forbs$PLOT <- NULL
grassmass.irrigated.forbs$TRANSECT <- NULL
summary(grassmass.irrigated.forbs)

#now let's write the intermediate cleaned data products into a folder

#we want to encode the information about site and trophic level into the file name
write.csv(grassmass.control.grass, file="cleaned_data/Konza_producer_control_grass.csv", row.names=FALSE)
write.csv(grassmass.control.forbs, file="cleaned_data/Konza_producer_control_forbs.csv", row.names=FALSE)
write.csv(grassmass.irrigated.grass, file="cleaned_data/Konza_producer_irrigated_grass.csv", row.names=FALSE)
write.csv(grassmass.irrigated.forbs, file="cleaned_data/Konza_producer_irrigated_forbs.csv", row.names=FALSE)

#ok, let's grasshopper this! the data has a BUNCH of issues we're going to have to address, 
#but let's talk about that later and bring it in first

#LTER Package ID: knb-lter-knz.29

hoppers<-read.csv(file="https://pasta.lternet.edu/package/data/eml/knb-lter-knz/29/19/3fb352e2478f776517f7e880fe31b808", 
                    header=T, na.strings=c("",".","NA"))

summary(hoppers)
str(hoppers)

#oooh boy. well, our first problem is that the datasheet doesn't record zeros, just blanks
#but we can kind of get around it by assuming that the total column is correct.
#because of the way the data are recorded, we're going to have to make the big, horrible
#assumption that in the unit of 200 sweeps, we always got at least one grasshopper, because the 
#way this is actually set up, if a researcher went out and swept and got nothing, they actually would
#record nothing. I think we should do the top four or so species (let's see what it looks like when
#we actually get counts by species) so we can use evidence that other species were there in a sample
#to essentially generate the absences of the key species we select

#first, let's cull out data from before 1996, because we need a continuous series anyway, so this cuts out the 
#hole in the data

levels(as.factor((hoppers$RECYEAR)))

hoppers1 <- hoppers[which(hoppers$RECYEAR>1995),]

#let's also cull out the breakdown by units of sweeps
hoppers1$S1 <- NULL
hoppers1$S2 <- NULL
hoppers1$S3 <- NULL
hoppers1$S4 <- NULL
hoppers1$S5 <- NULL
hoppers1$S6 <- NULL
hoppers1$S7 <- NULL
hoppers1$S8 <- NULL
hoppers1$S9 <- NULL
hoppers1$S10 <- NULL
#and spcode and soiltype and datacode
hoppers1$SPCODE <- NULL
hoppers1$SOILTYPE <- NULL
hoppers1$DATACODE <- NULL
hoppers1$COMMENTS <- NULL

#so it's apparent now that there's plenty of typos in species names (MY NEMESIS)
#let's start by making sure they're consistently capitalized

library(Hmisc)

hoppers1$SPECIES<-capitalize(as.character(hoppers1$SPECIES))

#so what does our species list look like now?
species.list <- sort(unique(hoppers1$SPECIES))
species.list

#take care of the typographical errors, multiple ways of saying spp

hoppers1$SPECIES<-gsub("Ageneotett deorum", "Ageneotettix deorum", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Arphia species", "Arphia spp.", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Arphia xanthopterara", "Arphia xanthoptera", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Boopedon auriventr", "Boopedon auriventris", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Boopedon auriventrisis", "Boopedon auriventris", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Arphia xanthopte", "Arphia xanthoptera", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Brachystol magna", "Brachystola magna", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Campylacan olivacea", "Campylacantha olivacea", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Chortophag viridifas", "Chortophaga viridifasciata", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Encoptolop sordidus", "Encoptolophus sordidus", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Encoptolop subgracil", "Encoptolophus subgracilis", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Encoptolop spp.", "Encoptolphus spp.", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Hadrotetti trifascia", "Hadrotettix trifasciatus", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Hesperotet speciosus", "Hesperotettix speciosus", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Hesperotet species", "Hesperotettix spp.", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Hesperotet spp.", "Hesperotettix spp.", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Hesperotet viridis", "Hesperotettix viridis", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Melanoplus angustipe", "Melanoplus angustipennis", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Melanoplus angustipennisnnis", "Melanoplus angustipennis", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Melanoplus bivittatu", "Melanoplus bivittatus", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Melanoplus bivittatuss", "Melanoplus bivittatus", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Melanoplus different", "Melanoplus differentialis", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Melanoplus differentialisialis", "Melanoplus differentialis", hoppers1$SPECIES)
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
hoppers1$SPECIES<-gsub("Pseudopoma brachypte", "Pseuodopomala brachyptera", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Phoetaliot nebrascen", "Phoetaliotes nebrascensis", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Schistocer lineata", "Schistocerca lineata", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Schistocer obscura", "Schistocerca obscura", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Syrbula admirabilisis", "Syrbula admirabilis", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Syrbula admirabil", "Syrbula admirabilis", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Unknown ", "Unknown", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Xanthippus corallipe", "Xanthippus corallipes", hoppers1$SPECIES)
hoppers1$SPECIES<-gsub("Xanthippus corallipess", "Xanthippus corallipes", hoppers1$SPECIES)

#so what does our species list look like after cleaning?
species.list <- sort(unique(hoppers1$SPECIES))
species.list

##double check there are only 74 species
#so 58 species. That was a long time to get there, #otherpeoplesdata. Let's use reshape2 to
#find out what our most abundant species are, by year
#but first! R is not seeing the total column as numeric.
str(hoppers1)
library(reshape2)

hoppers1$TOTAL <- as.numeric(hoppers1$TOTAL)

#and guess what?! site codes switch between capitalization patterns
hoppers1$WATERSHED <- toupper(hoppers1$WATERSHED)

#also, the hoppers data was all about disturbance regimes, but the intermediate disturbances
#ie 2 and 4 year treatments are probably out of sync with anything going on in the primary 
#productivity plots, so let's cut those out, and then divide the data into grazed/ungrazed (by mammmals)
levels(as.factor((hoppers1$WATERSHED)))

hoppers1<-hoppers1[which(hoppers1$WATERSHED!="002C"),]
hoppers1<-hoppers1[which(hoppers1$WATERSHED!="002D"),]
hoppers1<-hoppers1[which(hoppers1$WATERSHED!="004B"),]
hoppers1<-hoppers1[which(hoppers1$WATERSHED!="004F"),]
hoppers1<-hoppers1[which(hoppers1$WATERSHED!="N04A"),]
hoppers1<-hoppers1[which(hoppers1$WATERSHED!="N04D"),]

summary(hoppers1)

#there is one species (Pardalophora haldemani, 2005) with an NA in the total column - an implied zero
#let's make it an explicit zero
#otherwise total for year 2005 is NA in the summary below
hoppers1[is.na(hoppers1)] <- 0
summary(hoppers1)

summary.hoppers.by.species <- ddply(hoppers1, c("SPECIES"), summarise,
                                    TOTAL=sum(TOTAL))

summary.hoppers.by.year <- ddply(hoppers1, c("RECYEAR"), summarise,
                                 TOTAL=sum(TOTAL))

summary.hoppers.by.watershed <- ddply(hoppers1, c("WATERSHED"), summarise,
                               TOTAL=sum(TOTAL))

#sum of all hoppers, by year, watershed
summary.hoppers.total <- ddply(hoppers1, c("RECYEAR", "WATERSHED"), summarise,
                             TOTAL=sum(TOTAL))

#get sum of each species by year, watershed
summary.hoppers <- ddply(hoppers1, c("RECYEAR", "WATERSHED", "SPECIES"), summarise,
                         TOTAL=sum(TOTAL))

#two most common species are: 	Phoetaliotes nebrascensis, Orphulella speciosa

P.nebrascensis <- summary.hoppers[which(summary.hoppers$SPECIES=="Phoetaliotes nebrascensis"),]
P.nebrascensis$SPECIES <- NULL
colnames(P.nebrascensis)[colnames(P.nebrascensis)=="TOTAL"] <- "P.nebrascensis"

O.speciosa <- summary.hoppers[which(summary.hoppers$SPECIES=="Orphulella speciosa"),]
O.speciosa$SPECIES <- NULL
colnames(O.speciosa)[colnames(O.speciosa)=="TOTAL"] <- "O.speciosa"

#merge those data in
summary.hoppers.total <- merge(summary.hoppers.total, P.nebrascensis, by=c("RECYEAR", "WATERSHED"))
summary.hoppers.total <- merge(summary.hoppers.total, O.speciosa, by=c("RECYEAR", "WATERSHED"), all.x = TRUE)

summary(summary.hoppers.total)

#finally, there's one year that no O.speciosa was recorded in a plot - an implied zero
#let's make it an explicit zero
summary.hoppers.total[is.na(summary.hoppers.total)] <- 0

#now we just need to divide the data into usable sets with year, response
#for each treatment, we have grazed and ungrazed, total, and the two species of grasshoppers, 
#so 6 sets
hoppers.grazed <- summary.hoppers.total[which(grepl("N", summary.hoppers.total$WATERSHED)),]
hoppers.ungrazed <- summary.hoppers.total[which(!grepl("N", summary.hoppers.total$WATERSHED)),]

hoppers.grazed$WATERSHED <- NULL
hoppers.ungrazed$WATERSHED <- NULL

hoppers.grazed.total <- hoppers.grazed[1:2]
hoppers.grazed.p.n <- hoppers.grazed[c(1,3)]
hoppers.grazed.o.s <- hoppers.grazed[c(1,4)]

hoppers.ungrazed.total <- hoppers.ungrazed[1:2]
hoppers.ungrazed.p.n <- hoppers.ungrazed[c(1,3)]
hoppers.ungrazed.o.s <- hoppers.ungrazed[c(1,4)]

#all right,here we go, write the data

write.csv(hoppers.grazed.total, file="cleaned_data/Konza_herbivore_grazed_grasshopper_total.csv", row.names=FALSE)
write.csv(hoppers.grazed.p.n, file="cleaned_data/Konza_herbivore_grazed_grasshopper_pn.csv", row.names=FALSE)
write.csv(hoppers.grazed.o.s, file="cleaned_data/Konza_herbivore_grazed_grasshopper_os.csv", row.names=FALSE)
write.csv(hoppers.ungrazed.total, file="cleaned_data/Konza_herbivore_ungrazed_grasshopper_total.csv", row.names=FALSE)
write.csv(hoppers.ungrazed.p.n, file="cleaned_data/Konza_herbivore_ungrazed_grasshopper_pn.csv", row.names=FALSE)
write.csv(hoppers.ungrazed.o.s, file="cleaned_data/Konza_herbivore_ungrazed_grasshopper_os.csv", row.names=FALSE)

#ok, time for the small mammals. These data are in a different format from the grasshoppers but at least
#it seems to mostly be taken in the same spaces.

#LTER Package ID: knb-lter-knz.88

mammals<-read.csv(file="https://pasta.lternet.edu/package/data/eml/knb-lter-knz/88/8/1ced8529601926470f68c1d5eb708350", 
                  header=T, na.strings=c("",".","NA"))

str(mammals)

#get totals so we can get rid of the extra columns

mammals$TOTAL <- rowSums(mammals[7:20])

#cull out the columns we don't need: Pm and Pl are our two most abundant species
summary(mammals)
mammals1 <- mammals[c(4,5,6,7,14,21)]

#we want to sum things over the two samplings each year

summary.mammals <- ddply(mammals1, c("RECYEAR", "WATERSHED.LINE"), summarise,
                               TOTAL=sum(TOTAL), Pl=sum(Pl),Pm=sum(Pm))

#cull out treatments with 2 and 4 year fire frequencies
levels(as.factor((summary.mammals$WATERSHED.LINE)))
summary.mammals1 <- summary.mammals[which(!grepl("2", summary.mammals$WATERSHED.LINE)),]
summary.mammals2 <- summary.mammals1[which(!grepl("4", summary.mammals1$WATERSHED.LINE)),]


mammals.grazed <- summary.mammals2[which(grepl("N", summary.mammals2$WATERSHED.LINE)),]
mammals.ungrazed <- summary.mammals2[which(!grepl("N", summary.mammals2$WATERSHED.LINE)),]

mammals.grazed$WATERSHED.LINE <- NULL
mammals.ungrazed$WATERSHED.LINE <- NULL

mammals.grazed.total <- mammals.grazed[1:2]
mammals.grazed.pl <- mammals.grazed[c(1,3)]
mammals.grazed.pm <- mammals.grazed[c(1,4)]

mammals.ungrazed.total <- mammals.ungrazed[1:2]
mammals.ungrazed.pl <- mammals.ungrazed[c(1,3)]
mammals.ungrazed.pm <- mammals.ungrazed[c(1,4)]


#all right, here we go, write the data

write.csv(mammals.grazed.total, file="cleaned_data/Konza_omnivore_grazed_mammal_total.csv", row.names=FALSE)
write.csv(mammals.grazed.pl, file="cleaned_data/Konza_omnivore_grazed_mammal_pl.csv", row.names=FALSE)
write.csv(mammals.grazed.pm, file="cleaned_data/Konza_omnivore_grazed_mammal_pm.csv", row.names=FALSE)
write.csv(mammals.ungrazed.total, file="cleaned_data/Konza_omnivore_ungrazed_mammal_total.csv", row.names=FALSE)
write.csv(mammals.ungrazed.pl, file="cleaned_data/Konza_omnivore_ungrazed_mammal_pl.csv", row.names=FALSE)
write.csv(mammals.ungrazed.pm, file="cleaned_data/Konza_omnivore_ungrazed_mammal_pm.csv", row.names=FALSE)

#okay now the data are clean and in the format we need.  -_- At least for Konza.

#That was somethin'. but- onward! we need to clean three more sites, haha :'S

#######################################
#let's do Hubbard Brook next

#ugh, yeah the tree phenology data isn't going to work, but we could do two tropic levels- 
#leps and birds, and maybe use litter deposition as a proxy for productivity?

#LTER Package ID: knb-lter-hbr.82

hub.leps<-read.csv(file="https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/82/8/c32446bad7211a5a1cfabf70c89baec8", 
                   header=T, na.strings=c("",".","NA"))

summary(hub.leps)
#I think what's most relevant here is the number of individuals from all taxa and the biomass.
#there is also four species of tree these data were collected from, so maybe divide up on that.
#looks like there's probably unequal sampling between years so will have to account for that

summary.hub.leps <- ddply(hub.leps, c("Year", "GridLetter", "GridNumber", "TreeSpecies"), summarise,
                       individuals=mean(NumberIndividuals), biomass=mean(biomass))

#looks like there's some missing data for sampling location, so let's ditch that

summary.hub.leps <- summary.hub.leps[complete.cases(summary.hub.leps),]

summary(summary.hub.leps)

#strip out unnecessary columns
summary.hub.leps$GridLetter <- NULL
summary.hub.leps$GridNumber <- NULL

#Divide it up by tree species
hub.leps.viburnum <- summary.hub.leps[which(summary.hub.leps$TreeSpecies=="4"),]
hub.leps.st.maple <- summary.hub.leps[which(summary.hub.leps$TreeSpecies=="3"),]
hub.leps.su.maple <- summary.hub.leps[which(summary.hub.leps$TreeSpecies=="2"),]
hub.leps.beech <- summary.hub.leps[which(summary.hub.leps$TreeSpecies=="1"),]

#strip out the tree species column
hub.leps.viburnum$TreeSpecies <- NULL
hub.leps.st.maple$TreeSpecies <- NULL
hub.leps.su.maple$TreeSpecies <- NULL
hub.leps.beech$TreeSpecies <- NULL

#oh my god, there's no data reported for viburnum or striped maple? Ok, let's leave (ha)
#those out :/
#divide data into biomass and abundance

hub.leps.maple.biomass <- hub.leps.su.maple[c(1,3)]
hub.leps.maple.individuals <- hub.leps.su.maple[c(1,2)]
hub.leps.beech.biomass <- hub.leps.beech[c(1,3)]
hub.leps.beech.individuals <- hub.leps.beech[c(1,2)]

#and write it

write.csv(hub.leps.maple.biomass, file="cleaned_data/Hubbard_herbivore_maple_biomass.csv", row.names=FALSE)
write.csv(hub.leps.maple.individuals, file="cleaned_data/Hubbard_herbivore_maple_abundance.csv", row.names=FALSE)
write.csv(hub.leps.beech.biomass, file="cleaned_data/Hubbard_herbivore_beech_biomass.csv", row.names=FALSE)
write.csv(hub.leps.beech.individuals, file="cleaned_data/Hubbard_herbivore_beech_abundance.csv", row.names=FALSE)

#ok, as a proxy for plant productivity, which is scattered across numerous datasets,
#hbr makes litterfall available, so let's take a look-see at the coverage of these data

#LTER Package ID: knb-lter-hbr.49

hub.litter<-read.csv(file="https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/49/7/e4bf3920eaafe685aa8755828da48770", 
                     header=T, na.strings=c("",".","NA", "-9999", "-9999.99", "-9999.9", "-99.00"))

summary(hub.litter)

#so I was hoping to break it out by species dry mass, but the data coverage is not super, so total dry
#mass may be the thing. Since the lep data are only taken in beech and maple, let's just use hardwood
#forest sites, and sites without Ca addition

levels(as.factor(hub.litter$TRTMT))
levels(as.factor(hub.litter$COMP))

hub.litter1 <- hub.litter[which(hub.litter$TRTMT=="noCA"&hub.litter$COMP=="HW"),]
#pull out the columns we need
hub.litter2<- hub.litter1[c(3,4,5,6,9)]

#looks like there's some missing data for sampling location, so let's ditch that
summary(hub.litter2)

hub.litter2 <- hub.litter2[complete.cases(hub.litter2),]

#ok, differing number of samples each year. Sheesh. ok, let's see if the number of samples matters

summary.hub.litter <- ddply(hub.litter2, c("YEAR", "SITE", "ELEV"), summarise,
                            Avlitter=mean(DRY_MASS), totlitter=sum(DRY_MASS), samples=length(DRY_MASS))

plot(summary.hub.litter$totlitter ~ summary.hub.litter$samples)
plot(summary.hub.litter$Avlitter ~ summary.hub.litter$samples)

#looks like number of samples is strongly positively correlated with total litter but average leaf litter
#is not, so let's use the average, and there's no apparent site or elevation effects so they can be our 
#subsamples, and we'll just have one response measured for this level

hub.litter.mass <- summary.hub.litter[c(1,4)]

#and write it
write.csv(hub.litter.mass, file="cleaned_data/Hubbard_producer_litter_mass.csv", row.names=FALSE)

#all righty- birds. This table is fairly low complexity, but there are a few things that make 
#you go hmm. First, it's from a usable format, also, there are 't's all through it
#to indicate trace numbers of birds. hmm. let's fix this up

#LTER Package ID: knb-lter-hbr.81

hub.bird<-read.csv(file="https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/81/7/e1b527e8d41b314cb19209d3cf1aeed1", 
                   header=T, na.strings=c(""))

summary(hub.bird)

#whooboy, let's transpose this

hub.bird.trans <- dcast(melt(hub.bird, id="Bird.Species"), variable ~ Bird.Species)

summary(hub.bird.trans)

#ok, let's clean this up!
#first column is Year- rename it, clear out all the Xs in the year name from the import
colnames(hub.bird.trans)[colnames(hub.bird.trans)=="variable"] <- "Year"
hub.bird.trans$Year <- as.factor(gsub("X", "", hub.bird.trans$Year))

#now let's get rid of all those trace birds

hub.bird.trans[, 2:37] <- apply(hub.bird.trans[, 2:37], 2, 
                                function(x) as.numeric(gsub("t", "0", x)))

#let's also get the NAs- we'll assume if a bird isn't recorded, it wasn't there

hub.bird.trans[is.na(hub.bird.trans)] <- 0
summary(hub.bird.trans)

#let's get totals- let's find the two most common birds, and the total birds

colSums(hub.bird.trans[2:37])
hub.bird.trans$total <- rowSums(hub.bird.trans[2:37])
#red eyed vireo and american redstart are our guys (or gals)

hub.birds.total <- hub.bird.trans[c(1,38)]
hub.birds.redstart <- hub.bird.trans[c(1,2)]
hub.birds.vireo <- hub.bird.trans[c(1,23)]

#and write it

write.csv(hub.birds.total, file="cleaned_data/Hubbard_omnivore_bird_total.csv", row.names=FALSE)
write.csv(hub.birds.redstart, file="cleaned_data/Hubbard_omnivore_bird_redstart.csv", row.names=FALSE)
write.csv(hub.birds.vireo, file="cleaned_data/Hubbard_omnivore_bird_vireo.csv", row.names=FALSE)


##########
# and now, North Temperate lakes!

# Phytoplankton data is discontinouous but chlorophyll runs 1984-2007 without gaps and has more
# overlap with zooplankton, and then, I guess we find a fish that likes to eat zooplankton?

#chlorophyll

#LTER Package ID: knb-lter-ntl.73

ntl.chlor<-read.csv(file="https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/73/6/af2632acc5f66cfdafc0a470dae4f095", 
                    header=T, na.strings=c("",".","NA"))

summary(ntl.chlor)
str(ntl.chlor)

#ok, lakes R and L are the only ones that have continuous measurements for chlorophyll A over the 1984-2007 period
#so let's pull them out for use

levels(as.factor(ntl.chlor$lakeid))

ntl.chlor1 <- ntl.chlor[which(ntl.chlor$lakeid=="R"|ntl.chlor$lakeid=="L"),]

#pull out the columns we need
ntl.chlor2 <- ntl.chlor1[c(1,3,4,6,11)]
summary(ntl.chlor2)

# from looking at a pivot table, it looks like they didn't consistently sample at a depth greater than 6m
# after the early 90s, so let's cull out depths >6m because that would bias the sample 
ntl.chlor3 <- ntl.chlor2[which(ntl.chlor2$depth<6.1),]

summary(ntl.chlor3)

#still a bunch of NAs and some negative values for chla- let's cull those out
ntl.chlor4 <- ntl.chlor3[which(ntl.chlor3$chla>=0),]

summary(ntl.chlor4)

# Ok, let's think about how we want to divy this up- by each lake? and what sort of yearly metric do we want?
# Average across all depths? Repeated in time across a year?
summary.ntl.chlor <- ddply(ntl.chlor4, c("lakeid", "year4", "daynum"), summarise,
                         avg.chla=mean(chla))

#then we want to get rid of the day column, because we're just treating
#it as reps for this analysis

summary.ntl.chlor$daynum <- NULL

#divide it out by lake ID
ntl.lakeL.chlor <- summary.ntl.chlor[which(summary.ntl.chlor$lakeid=="L"),]
ntl.lakeR.chlor <- summary.ntl.chlor[which(summary.ntl.chlor$lakeid=="R"),]

#remove lakeID from the data frames
ntl.lakeL.chlor$lakeid <- NULL
ntl.lakeR.chlor$lakeid <- NULL

#and write it:

write.csv(ntl.lakeL.chlor, file="cleaned_data/NTL_producer_chlorA_lakeL.csv", row.names=FALSE)
write.csv(ntl.lakeR.chlor, file="cleaned_data/NTL_producer_chlorA_lakeR.csv", row.names=FALSE)

###
#ok, now zooplankton biomass

# LTER Package ID: knb-lter-ntl.355

ntl.zoo <- read.csv(file="https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/355/4/8084d8a30424cbf3feb4f69621e6c0a1", 
                  header=T, na.strings=c("",".","NA"))

summary(ntl.zoo)
levels(as.factor(ntl.zoo$year4))

# it's a bit inelegant and reductive, but let's just use biomass and abundance (number_per_net) as our response variables- totals per day per lake
#for lakes R and L

ntl.zoo1 <- ntl.zoo[which(ntl.zoo$lakeid=="R"|ntl.zoo$lakeid=="L"),]
#pull out the columns we need
ntl.zoo2 <- ntl.zoo1[c(1,3,4,9,16)]

#ok, let's make this repped by day, and take the total abundance and biomass reported within a given day
summary.ntl.zoo <- ddply(ntl.zoo2, c("lakeid", "year4", "daynum"), summarise,
                         tot.abund=sum(number_per_net), tot.mass=sum(biomass))

#then we want to get rid of the day column, because we're just treating
#it as reps for this analysis

summary.ntl.zoo$daynum <- NULL

#divide it out by lake ID
ntl.lakeL.zoo <- summary.ntl.zoo[which(summary.ntl.zoo$lakeid=="L"),]
ntl.lakeR.zoo <- summary.ntl.zoo[which(summary.ntl.zoo$lakeid=="R"),]

#and then snip it into abundance and biomass

ntl.lakeL.zoo.abund <- ntl.lakeL.zoo[c(2,3)]
ntl.lakeL.zoo.biomass <- ntl.lakeL.zoo[c(2,4)]
ntl.lakeR.zoo.abund <- ntl.lakeR.zoo[c(2,3)]
ntl.lakeR.zoo.biomass <- ntl.lakeR.zoo[c(2,4)]

#and write it:

write.csv(ntl.lakeL.zoo.abund, file="cleaned_data/NTL_consumer_zoo_abund_lakeL.csv", row.names=FALSE)
write.csv(ntl.lakeR.zoo.abund, file="cleaned_data/NTL_consumer_zoo_abund_lakeR.csv", row.names=FALSE)
write.csv(ntl.lakeL.zoo.biomass, file="cleaned_data/NTL_consumer_zoo_biomass_lakeL.csv", row.names=FALSE)
write.csv(ntl.lakeR.zoo.biomass, file="cleaned_data/NTL_consumer_zoo_biomass_lakeR.csv", row.names=FALSE)



#Ok, now it's fish time!

#LTER Package ID: knb-lter-ntl.86

ntl.fish <- read.csv(file="https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/86/5/9dc475cd80f45f64fb79a7d6733ee20f", 
                   header=T, na.strings=c("",".","NA"))

summary(ntl.fish)

# it's a bit inelegant and reductive, but let's just use biomass and abundance (number_per_net) as our response variables- totals per day per lake
#for lakes R and L

ntl.fish1 <- ntl.fish[which(ntl.fish$lakename=="PETER"|ntl.fish$lakename=="PAUL"),]

#looks like largemouth bass should be the species of focus because they're most common

ntl.fish2 <- ntl.fish1[which(ntl.fish1$species=="LARGEMOUTHBASS"),]

#now we count up the number of fish per sampling day, year
ntl.fish.count <- count(ntl.fish2, c("lakename", "year4", "daynum"))


#then we want to get rid of the day column, because we're just treating
#it as reps for this analysis

ntl.fish.count$daynum <- NULL

#divide it out by lake ID
ntl.lakeL.fish <- ntl.fish.count[which(ntl.fish.count$lakename=="PAUL"),]
ntl.lakeR.fish <- ntl.fish.count[which(ntl.fish.count$lakename=="PETER"),]

#remove lakename from the data frames
ntl.lakeL.fish$lakename <- NULL
ntl.lakeR.fish$lakename <- NULL


#and write it:

write.csv(ntl.lakeL.fish, file="cleaned_data/NTL_predator_fish_lakeL.csv", row.names=FALSE)
write.csv(ntl.lakeR.fish, file="cleaned_data/NTL_predator_fish_lakeR.csv", row.names=FALSE)


#all righty! We are finally here! the last of the four sites! Santa Barbara Coastal. A nice place go in your imagination
#when it's February and there's an ice storm in Ohio. first dataset actually documents algae and inverts  and fish all at once, so
#that's handy. Let's bring this data in. Notes said it's a big dataset so warning this might be a bit slow

#LTER Package ID: knb-lter-sbc.50

sbc <- read.csv(file="https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/50/10/24d18d9ebe4f6e8b94e222840096963c", 
              header=T, na.strings=c("",".","NA", -99999,-99999.00))

summary(sbc)

#oh, look at that, the authors of these data were so kind as to provide a coarse grouping of each of the taxa. it's like they knew I was coming
#to analyse it that way! So awesome. Ok, let's figure out how we want to aggregate this- let's use DRY_GRM2 as our response variable

#ok, I think it makes sense to do this by site, let's use top-two sampled sites, CARP and NAPL

sbc.1 <- sbc[which(sbc$SITE=="CARP"|sbc$SITE=="NAPL"),]

#ok, let's scale it down to our functional groups, and heck, let's aggregate it by month/transect

summary.sbc <- ddply(sbc.1, c("YEAR", "MONTH", "SITE", "TRANSECT", "COARSE_GROUPING"), summarise,
                         biomass=mean(DRY_GM2))

#ok, ditch the NAs
summary.sbc1 <- summary.sbc[complete.cases(summary.sbc),]

#then we want to get rid of the month and transect columns, for our purposes, these are just reps within a year

summary.sbc1$MONTH <- NULL
summary.sbc1$TRANSECT <- NULL


#divide it out by lake ID
sbc.carp <- summary.sbc1[which(summary.sbc1$SITE=="CARP"),]
sbc.napl <- summary.sbc1[which(summary.sbc1$SITE=="NAPL"),]

#remove SITE from the data frames
sbc.carp$SITE <- NULL
sbc.napl$SITE <- NULL

#now we need to create an object for each of the coarse groupings

sbc.carp.fish <- sbc.carp[which(sbc.carp$COARSE_GROUPING=="FISH"), c(1,3)]
sbc.carp.kelp <- sbc.carp[which(sbc.carp$COARSE_GROUPING=="GIANT KELP"), c(1,3)]
sbc.carp.mob.invt <- sbc.carp[which(sbc.carp$COARSE_GROUPING=="MOBILE INVERT"), c(1,3)]
sbc.carp.ses.invt <- sbc.carp[which(sbc.carp$COARSE_GROUPING=="SESSILE INVERT"), c(1,3)]
sbc.carp.algae <- sbc.carp[which(sbc.carp$COARSE_GROUPING=="UNDERSTORY ALGAE"), c(1,3)]

sbc.napl.fish <- sbc.napl[which(sbc.napl$COARSE_GROUPING=="FISH"), c(1,3)]
sbc.napl.kelp <- sbc.napl[which(sbc.napl$COARSE_GROUPING=="GIANT KELP"), c(1,3)]
sbc.napl.mob.invt <- sbc.napl[which(sbc.napl$COARSE_GROUPING=="MOBILE INVERT"), c(1,3)]
sbc.napl.ses.invt <- sbc.napl[which(sbc.napl$COARSE_GROUPING=="SESSILE INVERT"), c(1,3)]
sbc.napl.algae <- sbc.napl[which(sbc.napl$COARSE_GROUPING=="UNDERSTORY ALGAE"), c(1,3)]


#and now we write all these

write.csv(sbc.carp.fish, file="cleaned_data/SBC_predator_fish_carp.csv", row.names=FALSE)
write.csv(sbc.carp.kelp, file="cleaned_data/SBC_producer_kelp_carp.csv", row.names=FALSE)
write.csv(sbc.carp.mob.invt, file="cleaned_data/SBC_consumer_minvert_carp.csv", row.names=FALSE)
write.csv(sbc.carp.ses.invt, file="cleaned_data/SBC_consumer_sinvert_carp.csv", row.names=FALSE)
write.csv(sbc.carp.algae, file="cleaned_data/SBC_producer_algae_carp.csv", row.names=FALSE)

write.csv(sbc.napl.fish, file="cleaned_data/SBC_predator_fish_napl.csv", row.names=FALSE)
write.csv(sbc.napl.kelp, file="cleaned_data/SBC_producer_kelp_napl.csv", row.names=FALSE)
write.csv(sbc.napl.mob.invt, file="cleaned_data/SBC_consumer_minvert_napl.csv", row.names=FALSE)
write.csv(sbc.napl.ses.invt, file="cleaned_data/SBC_consumer_sinvert_napl.csv", row.names=FALSE)
write.csv(sbc.napl.algae, file="cleaned_data/SBC_producer_algae_napl.csv", row.names=FALSE)


#####################################
#load bad breakup script
source_github <- function(u) {
  # load package
  require(RCurl)
  
  # read script lines from website
  script <- getURL(u, ssl.verifypeer = FALSE)
  
  # parse lines and evaluate in the global environment
  eval(parse(text = script))
}

source("https://raw.githubusercontent.com/BahlaiLab/bad_breakup_2/master/R_model/bad_breakup_script.R")

options(scipen=10)

#Let's start with Konza Prairie

#Plants
grassmass.control.grass <- read.csv(file = "cleaned_data/Konza_producer_control_grass.csv")
grassmass.control.forbs <- read.csv(file = "cleaned_data/Konza_producer_control_forbs.csv")
grassmass.irrigated.grass <- read.csv(file="cleaned_data/Konza_producer_irrigated_grass.csv")
grassmass.irrigated.forbs <- read.csv(file="cleaned_data/Konza_producer_irrigated_forbs.csv")


model.grassmass.control.grass <- multiple_breakups(grassmass.control.grass)
pyramid_plot(model.grassmass.control.grass, title="Grass Biomass", plot_insig = TRUE, significance=0.05, rsq_points =TRUE)
wrongness_plot(model.grassmass.control.grass)
broken_stick_plot(model.grassmass.control.grass, window_length = 4, significance = 0.5)

#add some descriptive columns
model.grassmass.control.grass$site <- rep(c("konza"),each = 406)
model.grassmass.control.grass$trmt <- rep(c("control"),each = 406)
model.grassmass.control.grass$taxa <- rep(c("grass"),each = 406)
model.grassmass.control.grass$trophic_level <- rep(c("producer"),each = 406)

model.grassmass.control.forbs <- multiple_breakups(grassmass.control.forbs)
pyramid_plot(model.grassmass.control.forbs, title="Forb Biomass", plot_insig = TRUE, significance=0.05, rsq_points =TRUE)
wrongness_plot(model.grassmass.control.forbs)
broken_stick_plot(model.grassmass.control.forbs, window_length = 4, significance = 0.5)

model.grassmass.control.forbs$site <- rep(c("konza"),each = 406)
model.grassmass.control.forbs$trmt <- rep(c("control"),each = 406)
model.grassmass.control.forbs$taxa <- rep(c("forbs"),each = 406)
model.grassmass.control.forbs$trophic_level <- rep(c("producer"),each = 406)

model.grassmass.irrigated.grass <- multiple_breakups(grassmass.irrigated.grass)
pyramid_plot(model.grassmass.irrigated.grass, title="Grass Biomass", plot_insig = TRUE, significance=0.05, rsq_points =TRUE)
wrongness_plot(model.grassmass.irrigated.grass)
broken_stick_plot(model.grassmass.irrigated.grass, window_length = 4, significance = 0.5)

model.grassmass.irrigated.grass$site <- rep(c("konza"),each = 300)
model.grassmass.irrigated.grass$trmt <- rep(c("irrigated"),each = 300)
model.grassmass.irrigated.grass$taxa <- rep(c("grass"),each = 300)
model.grassmass.irrigated.grass$trophic_level <- rep(c("producer"),each = 300)

model.grassmass.irrigated.forbs <- multiple_breakups(grassmass.irrigated.forbs)
pyramid_plot(model.grassmass.irrigated.forbs, title="Forb Biomass", plot_insig = TRUE, significance=0.05, rsq_points =TRUE)
wrongness_plot(model.grassmass.irrigated.forbs)
broken_stick_plot(model.grassmass.irrigated.forbs, window_length = 4, significance = 0.5)

model.grassmass.irrigated.forbs$site <- rep(c("konza"),each = 300)
model.grassmass.irrigated.forbs$trmt <- rep(c("irrigated"),each = 300)
model.grassmass.irrigated.forbs$taxa <- rep(c("forbs"),each = 300)
model.grassmass.irrigated.forbs$trophic_level <- rep(c("producer"),each = 300)

#now merge all dataframes together
model.grassmass <- rbind(model.grassmass.control.grass, model.grassmass.control.forbs,
                         model.grassmass.irrigated.grass, model.grassmass.irrigated.forbs)

#Grasshoppers
hoppers.grazed.total <- read.csv(file="cleaned_data/Konza_herbivore_grazed_grasshopper_total.csv")
hoppers.grazed.p.n <- read.csv(file="cleaned_data/Konza_herbivore_grazed_grasshopper_pn.csv")
hoppers.grazed.o.s <- read.csv(file="cleaned_data/Konza_herbivore_grazed_grasshopper_os.csv")
hoppers.ungrazed.total <- read.csv(file="cleaned_data/Konza_herbivore_ungrazed_grasshopper_total.csv")
hoppers.ungrazed.p.n <- read.csv(file="cleaned_data/Konza_herbivore_ungrazed_grasshopper_pn.csv")
hoppers.ungrazed.o.s <- read.csv(file="cleaned_data/Konza_herbivore_ungrazed_grasshopper_os.csv")

summary(hoppers.grazed.total)
model.hoppers.grazed.total <- multiple_breakups(hoppers.grazed.total)
pyramid_plot(model.hoppers.grazed.total, plot_insig = TRUE, significance=0.05, rsq_points =TRUE)
wrongness_plot(model.hoppers.grazed.total)
broken_stick_plot(model.hoppers.grazed.total, window_length = 4, significance = 0.5)

model.hoppers.grazed.total$site <- rep(c("konza"),each = 153)
model.hoppers.grazed.total$trmt <- rep(c("grazed"),each = 153)
model.hoppers.grazed.total$taxa <- rep(c("grasshoppers_total"),each = 153)
model.hoppers.grazed.total$trophic_level <- rep(c("herbivore"),each = 153)

summary(hoppers.grazed.p.n)
model.hoppers.grazed.p.n <- multiple_breakups(hoppers.grazed.p.n)
pyramid_plot(model.hoppers.grazed.p.n, plot_insig = TRUE, significance=0.05, rsq_points =TRUE)
wrongness_plot(model.hoppers.grazed.p.n)
broken_stick_plot(model.hoppers.grazed.p.n, window_length = 4, significance = 0.5)

model.hoppers.grazed.p.n$site <- rep(c("konza"),each = 153)
model.hoppers.grazed.p.n$trmt <- rep(c("grazed"),each = 153)
model.hoppers.grazed.p.n$taxa <- rep(c("grasshoppers_pn"),each = 153)
model.hoppers.grazed.p.n$trophic_level <- rep(c("herbivore"),each = 153)

summary(hoppers.grazed.o.s)
model.hoppers.grazed.o.s <- multiple_breakups(hoppers.grazed.o.s)
pyramid_plot(model.hoppers.grazed.o.s, plot_insig = TRUE, significance=0.05, rsq_points =TRUE)
wrongness_plot(model.hoppers.grazed.o.s)
broken_stick_plot(model.hoppers.grazed.o.s, window_length = 4, significance = 0.5)

model.hoppers.grazed.o.s$site <- rep(c("konza"),each = 153)
model.hoppers.grazed.o.s$trmt <- rep(c("grazed"),each = 153)
model.hoppers.grazed.o.s$taxa <- rep(c("grasshoppers_os"),each = 153)
model.hoppers.grazed.o.s$trophic_level <- rep(c("herbivore"),each = 153)

summary(hoppers.ungrazed.total)
model.hoppers.ungrazed.total <- multiple_breakups(hoppers.ungrazed.total)
pyramid_plot(model.hoppers.ungrazed.total, plot_insig = TRUE, significance=0.05, rsq_points =TRUE)
wrongness_plot(model.hoppers.ungrazed.total)
broken_stick_plot(model.hoppers.ungrazed.total, window_length = 4, significance = 0.5)

model.hoppers.ungrazed.total$site <- rep(c("konza"),each = 276)
model.hoppers.ungrazed.total$trmt <- rep(c("ungrazed"),each = 276)
model.hoppers.ungrazed.total$taxa <- rep(c("grasshoppers_total"),each = 276)
model.hoppers.ungrazed.total$trophic_level <- rep(c("herbivore"),each = 276)

summary(hoppers.ungrazed.p.n)
model.hoppers.ungrazed.p.n <- multiple_breakups(hoppers.ungrazed.p.n)
pyramid_plot(model.hoppers.ungrazed.p.n, plot_insig = TRUE, significance=0.05, rsq_points =TRUE)
wrongness_plot(model.hoppers.ungrazed.p.n)
broken_stick_plot(model.hoppers.ungrazed.p.n, window_length = 4, significance = 0.5)

model.hoppers.ungrazed.p.n$site <- rep(c("konza"),each = 276)
model.hoppers.ungrazed.p.n$trmt <- rep(c("ungrazed"),each = 276)
model.hoppers.ungrazed.p.n$taxa <- rep(c("grasshoppers_pn"),each = 276)
model.hoppers.ungrazed.p.n$trophic_level <- rep(c("herbivore"),each = 276)

summary(hoppers.ungrazed.o.s)
model.hoppers.ungrazed.o.s <- multiple_breakups(hoppers.ungrazed.o.s)
pyramid_plot(model.hoppers.ungrazed.o.s, plot_insig = TRUE, significance=0.05, rsq_points =TRUE)
wrongness_plot(model.hoppers.ungrazed.o.s)
broken_stick_plot(model.hoppers.ungrazed.o.s, window_length = 4, significance = 0.5)

model.hoppers.ungrazed.o.s$site <- rep(c("konza"),each = 276)
model.hoppers.ungrazed.o.s$trmt <- rep(c("ungrazed"),each = 276)
model.hoppers.ungrazed.o.s$taxa <- rep(c("grasshoppers_os"),each = 276)
model.hoppers.ungrazed.o.s$trophic_level <- rep(c("herbivore"),each = 276)

#now merge all dataframes together
model.hoppers <- rbind(model.hoppers.grazed.total, model.hoppers.grazed.p.n, model.hoppers.grazed.o.s,
                       model.hoppers.ungrazed.total, model.hoppers.ungrazed.p.n, model.hoppers.ungrazed.o.s)


#Mammals
mammals.grazed.total <- read.csv(file="cleaned_data/Konza_omnivore_grazed_mammal_total.csv")
mammals.grazed.pl <- read.csv(file="cleaned_data/Konza_omnivore_grazed_mammal_pl.csv")
mammals.grazed.pm <- read.csv(file="cleaned_data/Konza_omnivore_grazed_mammal_pm.csv")
mammals.ungrazed.total <- read.csv(file="cleaned_data/Konza_omnivore_ungrazed_mammal_total.csv")
mammals.ungrazed.pl <- read.csv(file="cleaned_data/Konza_omnivore_ungrazed_mammal_pl.csv")#not enough individuals in ungrazed treatment
mammals.ungrazed.pm <- read.csv(file="cleaned_data/Konza_omnivore_ungrazed_mammal_pm.csv")

summary(mammals.grazed.total)
model.mammals.grazed.total <- multiple_breakups(mammals.grazed.total)
pyramid_plot(model.mammals.grazed.total, plot_insig = TRUE, significance=0.05, rsq_points =TRUE)
wrongness_plot(model.mammals.grazed.total)
broken_stick_plot(model.mammals.grazed.total, window_length = 4, significance = 0.5)

model.mammals.grazed.total$site <- rep(c("konza"),each = 496)
model.mammals.grazed.total$trmt <- rep(c("grazed"),each = 496)
model.mammals.grazed.total$taxa <- rep(c("mammals_total"),each = 496)
model.mammals.grazed.total$trophic_level <- rep(c("omnivore"),each = 496)

#summary(mammals.grazed.pl)
#model.mammals.grazed.pl <- multiple_breakups(mammals.grazed.pl)
#pyramid_plot(model.mammals.grazed.pl, plot_insig = TRUE, significance=0.05, rsq_points =TRUE)
#wrongness_plot(model.mammals.grazed.pl)
#broken_stick_plot(model.mammals.grazed.pl, window_length = 4, significance = 0.5)

summary(mammals.grazed.pm)
model.mammals.grazed.pm <- multiple_breakups(mammals.grazed.pm)
pyramid_plot(model.mammals.grazed.pm, plot_insig = TRUE, significance=0.05, rsq_points =TRUE)
wrongness_plot(model.mammals.grazed.pm)
broken_stick_plot(model.mammals.grazed.pm, window_length = 4, significance = 0.5)

model.mammals.grazed.pm$site <- rep(c("konza"),each = 496)
model.mammals.grazed.pm$trmt <- rep(c("grazed"),each = 496)
model.mammals.grazed.pm$taxa <- rep(c("mammals_pm"),each = 496)
model.mammals.grazed.pm$trophic_level <- rep(c("omnivore"),each = 496)

summary(mammals.ungrazed.total)
model.mammals.ungrazed.total <- multiple_breakups(mammals.ungrazed.total)
pyramid_plot(model.mammals.ungrazed.total, plot_insig = TRUE, significance=0.05, rsq_points =TRUE)
wrongness_plot(model.mammals.ungrazed.total)
broken_stick_plot(model.mammals.ungrazed.total, window_length = 4, significance = 0.5)

model.mammals.ungrazed.total$site <- rep(c("konza"),each = 496)
model.mammals.ungrazed.total$trmt <- rep(c("ungrazed"),each = 496)
model.mammals.ungrazed.total$taxa <- rep(c("mammals_total"),each = 496)
model.mammals.ungrazed.total$trophic_level <- rep(c("omnivore"),each = 496)

#summary(mammals.ungrazed.pl)
#colSums(mammals.ungrazed.pl) #I don't think there are enough individuals in the ungrazed treatment to use this species
#model.mammals.ungrazed.pl <- multiple_breakups(mammals.ungrazed.pl)
#pyramid_plot(model.mammals.ungrazed.pl, plot_insig = TRUE, significance=0.05, rsq_points =TRUE)
#wrongness_plot(model.mammals.ungrazed.pl)
#broken_stick_plot(model.mammals.ungrazed.pl, window_length = 4, significance = 0.5)

summary(mammals.ungrazed.pm)
model.mammals.ungrazed.pm <- multiple_breakups(mammals.ungrazed.pm)
pyramid_plot(model.mammals.ungrazed.pm, plot_insig = TRUE, significance=0.05, rsq_points =TRUE)
wrongness_plot(model.mammals.ungrazed.pm)
broken_stick_plot(model.mammals.ungrazed.pm, window_length = 4, significance = 0.5)

model.mammals.ungrazed.pm$site <- rep(c("konza"),each = 496)
model.mammals.ungrazed.pm$trmt <- rep(c("ungrazed"),each = 496)
model.mammals.ungrazed.pm$taxa <- rep(c("mammals_pm"),each = 496)
model.mammals.ungrazed.pm$trophic_level <- rep(c("omnivore"),each = 496)

#now merge all dataframes together
model.mammals <- rbind(model.mammals.grazed.total, model.mammals.grazed.pm,
                       model.mammals.ungrazed.total, model.mammals.ungrazed.pm)

#now merge all konza dataframes together and save
model.konza <- rbind(model.grassmass, model.hoppers, model.mammals)
write.csv(model.konza, file="model_output/model_konza.csv", row.names=FALSE)


##Hubbard Brook

#Litter
hub.litter.mass <- read.csv(file="cleaned_data/Hubbard_producer_litter_mass.csv")

summary(hub.litter.mass)
model.hub.litter.mass <- multiple_breakups(hub.litter.mass)
pyramid_plot(model.hub.litter.mass, plot_insig = TRUE, significance=0.05, rsq_points =TRUE)
wrongness_plot(model.hub.litter.mass)
broken_stick_plot(model.hub.litter.mass, window_length = 4, significance = 0.5)

model.hub.litter.mass$site <- rep(c("hbrook"),each = 351)
model.hub.litter.mass$trmt <- rep(c("hardwood"),each = 351)
model.hub.litter.mass$taxa <- rep(c("tree"),each = 351)
model.hub.litter.mass$trophic_level <- rep(c("producer"),each = 351)

#Lepidoptera
hub.leps.maple.biomass <- read.csv(file="cleaned_data/Hubbard_herbivore_maple_biomass.csv")
hub.leps.maple.individuals <- read.csv(file="cleaned_data/Hubbard_herbivore_maple_abundance.csv")
hub.leps.beech.biomass <- read.csv(file="cleaned_data/Hubbard_herbivore_beech_biomass.csv")
hub.leps.beech.individuals <- read.csv(file="cleaned_data/Hubbard_herbivore_beech_abundance.csv")

summary(hub.leps.maple.biomass)
model.hub.leps.maple.biomass <- multiple_breakups(hub.leps.maple.biomass)
pyramid_plot(model.hub.leps.maple.biomass, plot_insig = TRUE, significance=0.05, rsq_points =TRUE)
wrongness_plot(model.hub.leps.maple.biomass)
broken_stick_plot(model.hub.leps.maple.biomass, window_length = 4, significance = 0.5)

model.hub.leps.maple.biomass$site <- rep(c("hbrook"),each = 496)
model.hub.leps.maple.biomass$trmt <- rep(c("hardwood"),each = 496)
model.hub.leps.maple.biomass$taxa <- rep(c("lep_biomass_maple"),each = 496)
model.hub.leps.maple.biomass$trophic_level <- rep(c("herbivore"),each = 496)

summary(hub.leps.maple.individuals)
model.hub.leps.maple.individuals <- multiple_breakups(hub.leps.maple.individuals)
pyramid_plot(model.hub.leps.maple.individuals, plot_insig = TRUE, significance=0.05, rsq_points =TRUE)
wrongness_plot(model.hub.leps.maple.individuals)
broken_stick_plot(model.hub.leps.maple.individuals, window_length = 4, significance = 0.5)

model.hub.leps.maple.individuals$site <- rep(c("hbrook"),each = 496)
model.hub.leps.maple.individuals$trmt <- rep(c("hardwood"),each = 496)
model.hub.leps.maple.individuals$taxa <- rep(c("lep_abundance_maple"),each = 496)
model.hub.leps.maple.individuals$trophic_level <- rep(c("herbivore"),each = 496)

summary(hub.leps.beech.biomass)
model.hub.leps.beech.biomass <- multiple_breakups(hub.leps.beech.biomass)
pyramid_plot(model.hub.leps.beech.biomass, plot_insig = TRUE, significance=0.05, rsq_points =TRUE)
wrongness_plot(model.hub.leps.beech.biomass)
broken_stick_plot(model.hub.leps.beech.biomass, window_length = 4, significance = 0.5)

model.hub.leps.beech.biomass$site <- rep(c("hbrook"),each = 496)
model.hub.leps.beech.biomass$trmt <- rep(c("hardwood"),each = 496)
model.hub.leps.beech.biomass$taxa <- rep(c("lep_biomass_beech"),each = 496)
model.hub.leps.beech.biomass$trophic_level <- rep(c("herbivore"),each = 496)

summary(hub.leps.beech.individuals)
model.hub.leps.beech.individuals <- multiple_breakups(hub.leps.beech.individuals)
pyramid_plot(model.hub.leps.beech.individuals, plot_insig = TRUE, significance=0.05, rsq_points =TRUE)
wrongness_plot(model.hub.leps.beech.individuals)
broken_stick_plot(model.hub.leps.beech.individuals, window_length = 4, significance = 0.5)

model.hub.leps.beech.individuals$site <- rep(c("hbrook"),each = 496)
model.hub.leps.beech.individuals$trmt <- rep(c("hardwood"),each = 496)
model.hub.leps.beech.individuals$taxa <- rep(c("lep_abundance_beech"),each = 496)
model.hub.leps.beech.individuals$trophic_level <- rep(c("herbivore"),each = 496)

#now merge all dataframes together
model.leps <- rbind(model.hub.leps.maple.biomass, model.hub.leps.maple.individuals,
                    model.hub.leps.beech.biomass, model.hub.leps.beech.individuals)

#Birds
hub.birds.total <- read.csv(file="cleaned_data/Hubbard_omnivore_bird_total.csv")
hub.birds.redstart <- read.csv(file="cleaned_data/Hubbard_omnivore_bird_redstart.csv")
hub.birds.vireo <- read.csv(file="cleaned_data/Hubbard_omnivore_bird_vireo.csv")

summary(hub.birds.total)
model.hub.birds.total <- multiple_breakups(hub.birds.total)
pyramid_plot(model.hub.birds.total, plot_insig = TRUE, significance=0.05, rsq_points =TRUE)
wrongness_plot(model.hub.birds.total)
broken_stick_plot(model.hub.birds.total, window_length = 4, significance = 0.5)

model.hub.birds.total$site <- rep(c("hbrook"),each = 1035)
model.hub.birds.total$trmt <- rep(c("hardwood"),each = 1035)
model.hub.birds.total$taxa <- rep(c("bird_total"),each = 1035)
model.hub.birds.total$trophic_level <- rep(c("omnivore"),each = 1035)

summary(hub.birds.redstart)
model.hub.birds.redstart <- multiple_breakups(hub.birds.redstart)
pyramid_plot(model.hub.birds.redstart, plot_insig = TRUE, significance=0.05, rsq_points =TRUE)
wrongness_plot(model.hub.birds.redstart)
broken_stick_plot(model.hub.birds.redstart, window_length = 4, significance = 0.5)

model.hub.birds.redstart$site <- rep(c("hbrook"),each = 1035)
model.hub.birds.redstart$trmt <- rep(c("hardwood"),each = 1035)
model.hub.birds.redstart$taxa <- rep(c("bird_redstart"),each = 1035)
model.hub.birds.redstart$trophic_level <- rep(c("omnivore"),each = 1035)

summary(hub.birds.vireo)
model.hub.birds.vireo <- multiple_breakups(hub.birds.vireo)
pyramid_plot(model.hub.birds.vireo, plot_insig = TRUE, significance=0.05, rsq_points =TRUE)
wrongness_plot(model.hub.birds.vireo)
broken_stick_plot(model.hub.birds.vireo, window_length = 4, significance = 0.5)

model.hub.birds.vireo$site <- rep(c("hbrook"),each = 1035)
model.hub.birds.vireo$trmt <- rep(c("hardwood"),each = 1035)
model.hub.birds.vireo$taxa <- rep(c("bird_vireo"),each = 1035)
model.hub.birds.vireo$trophic_level <- rep(c("omnivore"),each = 1035)

#now merge all dataframes together
model.birds <- rbind(model.hub.birds.total, model.hub.birds.redstart,
                     model.hub.birds.vireo)

#now merge all hubbard brook dataframes together and save
model.hbrook <- rbind(model.hub.litter.mass, model.leps, model.birds)
write.csv(model.hbrook, file="model_output/model_hubbard_brook.csv", row.names=FALSE)


##North Temperate Lakes

#Chlorophyll
ntl.lakeL.chlor <- read.csv(file="cleaned_data/NTL_producer_chlorA_lakeL.csv")
ntl.lakeR.chlor <- read.csv(file="cleaned_data/NTL_producer_chlorA_lakeR.csv")

#Zooplankton
ntl.lakeL.zoo.abund <- read.csv(file="cleaned_data/NTL_consumer_zoo_abund_lakeL.csv")
ntl.lakeR.zoo.abund <- read.csv(file="cleaned_data/NTL_consumer_zoo_abund_lakeR.csv")
ntl.lakeL.zoo.biomass <- read.csv(file="cleaned_data/NTL_consumer_zoo_biomass_lakeL.csv")
ntl.lakeR.zoo.biomass <- read.csv(file="cleaned_data/NTL_consumer_zoo_biomass_lakeR.csv")

#Fish
ntl.lakeL.fish <- read.csv(file="cleaned_data/NTL_predator_fish_lakeL.csv")
ntl.lakeR.fish <- read.csv(file="cleaned_data/NTL_predator_fish_lakeR.csv")


##Santa Barbara Coastal

#Kelp,Algae
sbc.carp.kelp <- read.csv(file="cleaned_data/SBC_producer_kelp_carp.csv")
sbc.carp.algae <- read.csv(file="cleaned_data/SBC_producer_algae_carp.csv")
sbc.napl.kelp <- read.csv(file="cleaned_data/SBC_producer_kelp_napl.csv")
sbc.napl.algae <- read.csv(file="cleaned_data/SBC_producer_algae_napl.csv")

#Invertebrates
sbc.carp.mob.invt <- read.csv(file="cleaned_data/SBC_consumer_minvert_carp.csv")
sbc.carp.ses.invt <- read.csv(file="cleaned_data/SBC_consumer_sinvert_carp.csv")
sbc.napl.mob.invt <- read.csv( file="cleaned_data/SBC_consumer_minvert_napl.csv")
sbc.napl.ses.invt <- read.csv(file="cleaned_data/SBC_consumer_sinvert_napl.csv")

#Fish
sbc.carp.fish <- read.csv(file="cleaned_data/SBC_predator_fish_carp.csv")
sbc.napl.fish <- read.csv(file="cleaned_data/SBC_predator_fish_napl.csv")



