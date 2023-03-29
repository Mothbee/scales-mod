setwd("/Users/michellesu/Desktop/SCALE PROJECT/Analysis/R/scales-mod")

########### PERSONAL ACCESS TOKEN: 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

rm(list=ls())

#install.packages("readxl")
#install.packages("geomorph")
#install.packages("tibble")
#install.packages("vctrs")
#install.packages("readr")
library(readxl)
library(plyr)
library(dplyr)
# library(rvest)
library(stringr)
# library(ggplot2)
# library(reshape2)
# library(matrixStats)
library(phytools)
library(geomorph)
library(tibble)
library(vctrs)
library(readr)

##### read in data template (collection info, raw scale csv file names) ####
specimen_management_raw <- read.csv("/Users/michellesu/Desktop/SCALE PROJECT/Analysis/R/michelle_scales_desktop/2.0 Specimen Management with new stuff.csv", 
                                    header = T)
specimen_management_raw$specimen.number <- as.character(specimen_management_raw$specimen.number)



# extract.imageID = function(list.name){
#   imageID=list.name[length(list.name)] # image ID
#   #species=list.name[1] # species name 
#   collection=list.name[length(list.name)-2] # collection name
#   specimen.number=list.name[length(list.name)-1] # spec ID 
#   # note not including species and subspecies ID here... spec ID is a unique ID within collections
#   bound.meta = cbind(imageID, collection, specimen.number)
# }

# head.list = strsplit(specimen_management_raw$head.image, "-")
# head.list.clean = Filter(function(a) any(!is.na(a)), lapply(head.list, extract.imageID))  # remove elements of list where only NA values bc photos not entered yet
# head.image.df = data.frame(do.call(rbind, head.list.clean))
# 
# specimen_management_raw$collection <- head.image.df$collection
# specimen_management_raw$specimen.number <- head.image.df$specimen.number

# make matching variable
specimen_management_raw$matcher <- paste(specimen_management_raw$collection,specimen_management$specimen.number)

# read in most up to date loan specimen csv
# check for a newer one periodically! and save LOCALLY as a csv
mahler_loan_raw <- read.csv("/Users/michellesu/Desktop/SCALE PROJECT/Analysis/R/michelle_scales_desktop/Mahler Lab Active Loan Specimens 2023-03-28.csv")
mahler_loan_trim <- select(mahler_loan_raw, species, subspecies, collection, specimen.number, sex, svl, percentile)
# make matching variable
mahler_loan_trim$matcher <- paste(mahler_loan_trim$collection,mahler_loan_trim$specimen.number)


# combine these 2 data frames but first, remove circular columns from specimen_management_raw
# ie these are columns that will be imported when bind with mahler loan specimen sheet
specimen_management_raw2 <- select(specimen_management_raw, -species, -subspecies, -sex, -svl, -percentile, -svl.6perc, -svl.2perc)   # remove last bunch of columns, will remove Mahler loan (circular) columns

##### change code so don't need to manually remove any columns 
specimen_management <- left_join(specimen_management_raw2, mahler_loan_trim, by=c("matcher"))

# calculating metadata for imageJ: svl 6%, svl 1%, area per point
specimen_management$svl <- as.numeric(specimen_management$svl)
specimen_management$percentile <- as.numeric(specimen_management$percentile)

specimen_management$svl.6perc <- as.numeric(specimen_management$svl)*0.06
specimen_management$svl.2perc <- as.numeric(specimen_management$svl)*0.02

specimen_management$svl.6perc <- round(specimen_management$svl.6perc ,digit=2)
specimen_management$svl.2perc <- round(specimen_management$svl.2perc ,digit=2)


specimen_management <- specimen_management %>% 
  relocate(svl.2perc, .after=hindtoes.image) %>% 
  relocate(svl.6perc, .after=hindtoes.image) %>% 
  relocate(specimen.number, .after=hindtoes.image) %>% 
  relocate(sex, .after=hindtoes.image)

# flag small ones
specimen_management$collection[specimen_management$percentile<0.8] <- "PERCENTILE <0.8"

# make undone ones have date NA
specimen_management$year[is.na(specimen_management$head.count)] <- NA
specimen_management$month[is.na(specimen_management$head.count)] <- NA
specimen_management$day[is.na(specimen_management$head.count)] <- NA



# make blank cells NA
specimen_management[specimen_management == ''] <- NA



###### write csv with circular columns #####
write.csv(specimen_management, 
          file="/Users/michellesu/Desktop/SCALE PROJECT/Analysis/R/michelle_scales_desktop/2.0 Specimen Management.csv", row.names=F)



##### Summary info for myself #####
# so i can see how many i have of each species and sex
summary.m <- specimen_management %>% 
  filter(!is.na(head.count)) %>% 
  filter(percentile>=0.8) %>% 
  filter(sex=="M") %>% 
  group_by(species) %>% 
  #filter(species) %>% 
  count()

summary.f <- specimen_management %>% 
  filter(!is.na(head.count)) %>% 
  filter(percentile>=0.8) %>% 
  filter(sex=="F") %>% 
  group_by(species) %>% 
  #filter(species) %>% 
  count()

# number of inds done
summary.ind <- specimen_management %>% 
  filter(!is.na(head.count)) %>% 
  filter(percentile>=0.8) %>% 
  #filter(species) %>% 
  count()

# # small ones for luke to check
# smalls <- specimen_management %>%
#   filter(!is.na(head.count)) %>% 
#   filter(percentile<0.8) %>% 
#   select(species, specimen.number, sex, svl, percentile)
# 
# smalls <- smalls[order(smalls$species),]
# 
# write.csv(smalls,
#           file="/Users/michellesu/Desktop/SCALE PROJECT/Analysis/R/michelle_scales_desktop/Percentile less than 0.8.csv", row.names=F)




##### cleaning and size-correcting individual data #####

## removing NAs and cleaning up the data
specimen_management_noNA <- specimen_management %>% 
  filter(!is.na(head.count)) %>% # a proxy for unfinished specimens 
  #  filter(!is.na(hindtoes.count.b)) %>% 
  filter(hindtoes.count.b!="check!") # need to get rid of ANY strings in the count columns, this is temporary solution

# ALSO FIX, because of "check!"s, column was "character" not "integer"
specimen_management_noNA$hindtoes.count.b <- as.integer(specimen_management_noNA$hindtoes.count.b)
## same for svl for some reason??
specimen_management_noNA$svl <- as.integer(specimen_management_noNA$svl)

## converting to densities per mm
specimen_management_noNA_permm <- specimen_management_noNA %>%
  mutate(
    across(c("head.count", "midline.count", "dorsal.count", "dorsolateral.count", "chin.count", "ventral.count", "hindtoes.count.t", "hindtoes.count.b"),
           .fns = ~./svl.6perc)) %>% 
  mutate(
    across(c("foretoes.count.t", "foretoes.count.b"),
           .fns = ~./svl.2perc))

## size-correction

# log-transform
specimen_management_noNA_permm_log <- specimen_management_noNA_permm
specimen_management_noNA_permm_log[, c("head.count", "midline.count", "dorsal.count", "dorsolateral.count", "chin.count", "ventral.count", "foretoes.count.t", "foretoes.count.b", "hindtoes.count.t", "hindtoes.count.b"
)] <- log(specimen_management_noNA_permm_log[, c("head.count", "midline.count", "dorsal.count", "dorsolateral.count", "chin.count", "ventral.count", "foretoes.count.t", "foretoes.count.b", "hindtoes.count.t", "hindtoes.count.b"
)])

# size-correcting
specimen_management_noNA_permm_sc <- specimen_management_noNA_permm_log
specimen_management_noNA_permm_sc[, c("head.count", "midline.count", "dorsal.count", "dorsolateral.count", "chin.count", "ventral.count", "foretoes.count.t", "foretoes.count.b", "hindtoes.count.t", "hindtoes.count.b"
)] <- specimen_management_noNA_permm_sc[, c("head.count", "midline.count", "dorsal.count", "dorsolateral.count", "chin.count", "ventral.count", "foretoes.count.t", "foretoes.count.b", "hindtoes.count.t", "hindtoes.count.b"
)] - log(as.numeric(specimen_management_noNA_permm_sc$svl))




#### pairwise plots
# pairs(specimen_management_noNA_permm_sc[,18:27],
#       pch=16, cex=0.4, col=rgb(red=0, green=0, blue=0, alpha=0.4))

# ##takes so damn long to run
# pairs(specimen_management_noNA_permm_sc[,18:27],
#       pch=16, cex=0.4, col=rgb(red=0, green=0, blue=0, alpha=0.4),
#       panel=function(x, y, ...) { points(x, y, ...); 
#         text(x, y, specimen_management_noNA_permm_sc[,"specimen.number"], cex=0.3) })



weirdos_list <- c("29784", "64616", "156786", "317496", "20418", "66370", "S7673", "103456", "122438", "176355", "122438",
                  "149680", "149679", "122438", "166239", "149679", "103450", "41894F", "103948", "20394", "255638", "P747", "103456", "94609")

specimen_management_noNA_permm_sc <- specimen_management_noNA_permm_sc[ ! specimen_management_noNA_permm_sc$specimen.number %in% weirdos_list, ]



# # this worked
# pairwise_m <- specimen_management_noNA_permm_sc[,c("svl", "head.count", "midline.count", "dorsal.count", "dorsolateral.count", "chin.count", "ventral.count", "foretoes.count.t", "foretoes.count.b", "hindtoes.count.t", "hindtoes.count.b")]
# pairs(pairwise_m, labels=c("svl", "head", "midline", "dorsal", "lateral", "chin", "ventral", "toepad fore", "toe base fore", "toepad hind", "toe base hind"),
#       pch=16, cex=0.4, col=rgb(red=0, green=0, blue=0, alpha=0.4))


######## summarizing for species and sex ######
summary.ind <- specimen_management_noNA_permm_sc %>% 
  filter(!is.na(head.count)) %>% 
  filter(percentile>=0.8) %>% 
  filter(sex=="M") %>% 
  count()

## Males
males <- specimen_management_noNA_permm_sc %>% 
  filter(sex=="M") %>% 
  filter(percentile>=0.8) %>% 
  group_by(species) %>% 
  dplyr::summarise(across(c(svl, head.count, midline.count, dorsal.count, dorsolateral.count, chin.count, ventral.count,
                            foretoes.count.t, foretoes.count.b, hindtoes.count.t, hindtoes.count.b), list(mean=mean), .names="{col}_{fn}"))
# the following would add new columns for means, matches to species still but would be duplicate rows for each species
# mutate(across(c(midline.count, dorsal.count, dorsolateral.count, chin.count, ventral.count,
#                           foretoes.count.t, foretoes.count.b, hindtoes.count.t, hindtoes.count.b), list(mean=mean), .names="{col}_{fn}"))

write.csv(males, 
          file="/Users/michellesu/Desktop/SCALE PROJECT/Analysis/R/michelle_scales_desktop/males.csv", row.names=F)


## Females
females <- specimen_management_noNA_permm_sc %>% 
  filter(sex=="F") %>% 
  filter(percentile>=0.8) %>% 
  group_by(species) %>% 
  dplyr::summarise(across(c(svl, head.count, midline.count, dorsal.count, dorsolateral.count, chin.count, ventral.count,
                            foretoes.count.t, foretoes.count.b, hindtoes.count.t, hindtoes.count.b), list(mean=mean), .names="{col}_{fn}"))
write.csv(females, 
          file="/Users/michellesu/Desktop/SCALE PROJECT/Analysis/R/michelle_scales_desktop/females.csv", row.names=F)



##### species missing #######
# all imaged/loaned stuff
all_spp <- read.csv("all species.csv")

# read in tree and make spp list
tree_full <- read.tree("/Users/michellesu/Desktop/SCALE PROJECT/Analysis/R/michelle_scales_desktop/trees_cleaned/Poe.et.al.2017.Timetree_cleaned.tre")
tree_spp <- as.data.frame(tree_full$tip.label)

# spp we have in the tree
in_tree <- all_spp[all_spp$Row.Labels %in% tree_spp$`tree_full$tip.label`, ]

missing<- as.data.frame(setdiff(in_tree$Row.Labels, males$species))



