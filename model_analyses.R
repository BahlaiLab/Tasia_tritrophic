#####################################
# Exploratory analysis of model outputs for LTER sites

# import the data
mod.lter <- read.csv(file="model_output/model_all_LTER_sites.csv")

library(ggplot2)
library(lattice)


boxplot(slope ~ trophic_level, data = mod.lter)
boxplot(p_value ~ trophic_level, data = mod.lter)
boxplot(slope_se ~ trophic_level, data = mod.lter)
boxplot(r_square ~ trophic_level, data = mod.lter)
boxplot(adj_r_square ~ trophic_level, data = mod.lter)

ggplot(mod.lter, aes(start_year, slope, colour = trophic_level)) + geom_point() + theme_bw()
ggplot(mod.lter, aes(start_year, slope_se, colour = trophic_level)) + geom_point() + theme_bw()
ggplot(mod.lter, aes(start_year, p_value, colour = trophic_level)) + geom_point() + theme_bw()
ggplot(mod.lter, aes(start_year, r_square, colour = trophic_level)) + geom_point() + theme_bw()
ggplot(mod.lter, aes(start_year, adj_r_square, colour = trophic_level)) + geom_point() + theme_bw()

ggplot(mod.lter, aes(N_years, slope, colour = trophic_level)) + geom_point() + theme_bw()
ggplot(mod.lter, aes(N_years, slope_se, colour = trophic_level)) + geom_point() + theme_bw()
ggplot(mod.lter, aes(N_years, p_value, colour = trophic_level)) + geom_point() + theme_bw()
ggplot(mod.lter, aes(N_years, r_square, colour = trophic_level)) + geom_point() + theme_bw()
ggplot(mod.lter, aes(N_years, adj_r_square, colour = trophic_level)) + geom_point() + theme_bw()


## Konza Prairie
mod.konza <- read.csv(file="model_output/model_konza.csv")
options(scipen = 999)
mod.konza$p_value <- round(mod.konza$p_value, digits = 10)
mod.konza$intercept_p_value <- round(mod.konza$intercept_p_value, digits = 10)
cols <- c("black", "red")

str(mod.konza)
mod.konza$trophic_level <- as.factor(mod.konza$trophic_level)
levels(mod.konza$trophic_level)
mod.konza$trophic_level <- factor(mod.konza$trophic_level, levels = c("producer", "herbivore", "omnivore"))
levels(mod.konza$trophic_level)
mod.konza$N_years <- as.numeric(mod.konza$N_years)

png("figures/Konza_Prairie.png", width = 1600, height = 600, pointsize = 20)
xyplot(mod.konza$N_years ~ mod.konza$slope | mod.konza$trophic_level,
       col = ifelse(mod.konza$p_value < 0.05,'red','black'),
       pch = 19, cex = 1,
       strip = strip.custom(bg="lightgrey", par.strip.text=list(col="black", cex = 1, font = 2)),
       xlab = "Slope", ylab = "Number of years",
       abline=c(v=0, lwd = 3))
dev.off()

ggplot(mod.konza, aes(N_years, slope_se, colour = trophic_level)) + geom_point() + theme_classic()
ggplot(mod.konza, aes(N_years, p_value, colour = trophic_level)) + geom_point() + theme_bw()
ggplot(mod.konza, aes(N_years, r_square, colour = trophic_level)) + geom_point() + theme_bw()
ggplot(mod.konza, aes(N_years, adj_r_square, colour = trophic_level)) + geom_point() + theme_bw()


## Hubbard Brook
mod.hbrook <- read.csv(file="model_output/model_hubbard_brook.csv")
options(scipen = 999)
mod.hbrook$p_value <- round(mod.hbrook$p_value, digits = 10)
mod.hbrook$intercept_p_value <- round(mod.hbrook$intercept_p_value, digits = 10)

str(mod.hbrook)
mod.hbrook$trophic_level <- as.factor(mod.hbrook$trophic_level)
levels(mod.hbrook$trophic_level)
mod.hbrook$trophic_level <- factor(mod.hbrook$trophic_level, levels = c("producer", "herbivore", "omnivore"))
levels(mod.hbrook$trophic_level)
mod.hbrook$N_years <- as.numeric(mod.hbrook$N_years)

png("figures/Hubbard_Brook.png", width = 1600, height = 600, pointsize = 20)
xyplot(mod.hbrook$N_years ~ mod.hbrook$slope | mod.hbrook$trophic_level,
       col = ifelse(mod.hbrook$p_value < 0.05,'red','black'),
       pch = 19, cex = 1,
       strip = strip.custom(bg="lightgrey", par.strip.text=list(col="black", cex = 1, font = 2)),
       xlab = "Slope", ylab = "Number of years",
       abline=c(v=0, lwd = 3))
dev.off()


## North Temperate Lakes
mod.ntlakes <- read.csv(file="model_output/model_north_temperate_lakes.csv")
options(scipen = 999)
mod.ntlakes$p_value <- round(mod.ntlakes$p_value, digits = 10)
mod.ntlakes$intercept_p_value <- round(mod.ntlakes$intercept_p_value, digits = 10)

str(mod.ntlakes)
mod.ntlakes$trophic_level <- as.factor(mod.ntlakes$trophic_level)
levels(mod.ntlakes$trophic_level)
mod.ntlakes$trophic_level <- factor(mod.ntlakes$trophic_level, levels = c("producer", "consumer", "predator"))
levels(mod.ntlakes$trophic_level)
mod.ntlakes$N_years <- as.numeric(mod.ntlakes$N_years)

png("figures/North_Temperate_Lakes.png", width = 1600, height = 600, pointsize = 20)
xyplot(mod.ntlakes$N_years ~ mod.ntlakes$slope | mod.ntlakes$trophic_level,
       col = ifelse(mod.ntlakes$p_value < 0.05,'red','black'),
       pch = 19, cex = 1,
       strip = strip.custom(bg="lightgrey", par.strip.text=list(col="black", cex = 1, font = 2)),
       xlab = "Slope", ylab = "Number of years",
       abline=c(v=0, lwd = 3))
dev.off()


## North Temperate Lakes
mod.sbcoastal <- read.csv(file="model_output/model_santa_barbara_coastal.csv")
options(scipen = 999)
mod.sbcoastal$p_value <- round(mod.sbcoastal$p_value, digits = 10)
mod.sbcoastal$intercept_p_value <- round(mod.sbcoastal$intercept_p_value, digits = 10)

str(mod.sbcoastal)
mod.sbcoastal$trophic_level <- as.factor(mod.sbcoastal$trophic_level)
levels(mod.sbcoastal$trophic_level)
mod.sbcoastal$trophic_level <- factor(mod.sbcoastal$trophic_level, levels = c("producer", "consumer", "predator"))
levels(mod.sbcoastal$trophic_level)
mod.sbcoastal$N_years <- as.numeric(mod.sbcoastal$N_years)

png("figures/Santa_Barbara_Coastal.png", width = 1600, height = 600, pointsize = 20)
xyplot(mod.sbcoastal$N_years ~ mod.sbcoastal$slope | mod.sbcoastal$trophic_level,
       col = ifelse(mod.sbcoastal$p_value < 0.05,'red','black'),
       pch = 19, cex = 1,
       strip = strip.custom(bg="lightgrey", par.strip.text=list(col="black", cex = 1, font = 2)),
       xlab = "Slope", ylab = "Number of years",
       abline=c(v=0, lwd = 3))
dev.off()


