#### -------------------------------------------------------------------------------- ####
#### Cull data to passeriformes with good data coverage
#### -------------------------------------------------------------------------------- ####

# Created by Austin 
# August 30th, 2019

# Set working directory

#setwd("C:/Users/Tingley Lab_2/Dropbox/BBS_Project") #Tingley lab computer
#setwd("/Users/austinspence/Dropbox/BBS_Project")

# Load in the full data set

load("./BBS_Data/full_birds_dataset.RData")  #nope, still doesn't work. full_birds_dataset is already culled
# to 17 columns


# Limit it to Passerines


full_passeriformes <- full_birds[which(full_birds$ORDER == "Passeriformes"),]

full_passeriformes <- droplevels(passeriformes)

## simplify the dataset to only what we need for analysis
passeriformes <- full_passeriformes[,c(1:3,10,11,18,26,299,303,409,411,413:415,417:419)] # simplify dataset

## Remove birds with less than 100 lag observations and less than 20 different sites
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "Abert's Towhee"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "American Dipper"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "American Pipit"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "American Tree Sparrow"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "Bay-breasted Warbler"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "Bell's Sparrow"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "Black-capped Vireo"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "Black-crested Titmouse"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "Blackpoll Warbler"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "Boat-tailed Grackle"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "Bohemian Waxwing"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "Boreal Chickadee"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "Cape May Warbler"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "Cerulean Warbler"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "Chestnut-collared Longspur"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "Common Redpoll"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "Connecticut Warbler"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "Golden-crowned Sparrow"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "Gray-cheeked Thrush"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "Juniper Titmouse"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "Lawrence's Goldfinch"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "Le Conte's Thrasher"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "Lucy's Warbler"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "McCown's Longspur"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "Nelson's Sparrow"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "Northwestern Crow"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "Palm Warbler"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "Pine Grosbeak"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "Rusty Blackbird"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "Sprague's Pipit"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "Swainson's Warbler"),]
passeriformes <- passeriformes[which(passeriformes$English_Common_Name != "unid. California Scrub-Jay / Woodhouse's Scrub-Jay"),]

## Some of the birds are not lumped that should be
passeriformes$aou[passeriformes$aou == 6556] <- 6550 #yellow rumped together
passeriformes$aou[passeriformes$aou == 6560] <- 6550 #yellow rumped together
passeriformes$aou[passeriformes$aou == 5677] <- 5670 #juncos together
passeriformes$aou[passeriformes$aou == 5690] <- 5670 #juncos together
passeriformes$aou[passeriformes$aou == 5660] <- 5670 #juncos together
passeriformes$aou[passeriformes$aou == 5680] <- 5670 #juncos together
passeriformes$aou[passeriformes$aou == 5671] <- 5670 #juncos together

## Refactor birds
passeriformes$aou <- factor(passeriformes$aou)

## Save the data for analysis
save(passeriformes, file = "passeriformes.Rdata")

