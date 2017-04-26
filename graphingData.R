library(ggplot2)
library(readr)
library(maps)
library(mapproj)

#Getting the map information
all_states <- map_data("state")
coloradoPlateauStates <- c("arizona","colorado","new mexico","utah")
substates <- subset(all_states, region %in% coloradoPlateauStates)

#Getting the species information
load("PR_AtriplexConfertifolia.Rdata")
aConfertifolia <- s
load("PR_EphedraTorreyana.Rdata")
eTorreyana <- s
load("PR_HilariaJamesii.Rdata")
hJamesii <- s
load("PR_SarcobatusVermiculatus.Rdata")
sVermiculatus <- s
aConfertifolia["species"] <- "Atriplex Confertifolia - C4 Shrub"
eTorreyana["species"] <- "Ephedra Torreyana - C3 Shrub"
hJamesii["species"] <- "Hilaria Jamesii - C4 Grass"
sVermiculatus["species"] <- "Sarcobatus Vermiculatus - C3 Grass"
#merge all species into one from
allSpecies <- merge(sVermiculatus,merge(hJamesii,merge(aConfertifolia,eTorreyana, all=TRUE), all=TRUE)
              ,all=TRUE)
#plot the occurence data points and label with color by species
ggplot(allSpecies) + 
  geom_polygon(data=all_states, aes(x=long, y=lat, group = group), colour="black", fill="white") + 
  expand_limits(x = all_states$long, y = all_states$lat) + 
  coord_map()+
  geom_point(aes(colour = species,x=allSpecies$lon, y=allSpecies$lat))

