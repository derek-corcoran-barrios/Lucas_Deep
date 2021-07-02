################################################################################
# Download and select LUCAS images for keras model
# Author: Naia Morueta-Holme
# Date: June 2021

# download table with LULC tags and image references
# select subset of LC tags
# create folder with subfolders labelled with LC tags
# download sample LUCAS images into each subfolder for training
# download separate subset of LUCAS images into test folder

################################################################################

#-----------#
# libraries #
#-----------#
library(dplyr)
library(RCurl)

#-----------------#
# Set directories #
#-----------------#
#input directory
#idir <- "C:/Users/vwn831/OneDrive - Københavns Universitet/GSV_ideas/Data/"
#output directory
odir <- paste0(getwd(),"/lucas01/")
dir.create(odir)


#---------------------------------------#
# Read and check the LUCAS lookup table #
#---------------------------------------#
# Harmonized LUCAS table for 2018 (downloaded from https://data.jrc.ec.europa.eu/dataset/f85907ae-d123-471f-a44a-8cca993485a2/resource/7d391a09-5bf4-4d12-98eb-a2b4b18a0676
# - see d'Andrimont et al 2020 Scientific Data)
dt <- read.csv("lucas_harmo_uf_2018.csv") # 337,854 observations of 117 variables

# Get names of headers in table
cols <- names(dt)

#Why east
# select all points with photo towards East that are classified as woodland
dtsub <- subset(dt, !is.na(file_path_gisco_east) & letter_group=="C") #66,500 observations

# # check for secondary LC
# table(dtsub$lc2_label) # 64,145 out of the 66,500 "not relevant" - 1,731 "grassland without tree/shrub cover"
# ## --> ignore LC2 for now
#
# # distribution of observations over lc1_label
# table(dtsub$lc1_label)
# ## --> 7 classes, 1400-36000 observations each
#
# #distribution of observations over lc1_spec_label
# table(dtsub$lc1_spec_label)
# ##--> 96 classes, 1-9378 observations each (47 classes >100 observations) - NOTE: only counting one picture
# # per site! Can get many more across directions and survey years


#---------------------------#
# Select images to download #
#---------------------------#

## Batch 1 - training of all woodland lc1_labels

# get image urls for random subset of images for each label

n <- 500 #how many samples per label?
testsize <- 10
trainsize <- n-testsize
labels <- unique(dtsub$lc1_label)
direction <- "east"

# tibble of lookup table grouped by lc1_labels with equal sample of size n for each
set.seed(1)
new_df <- dtsub %>%
  group_by(lc1_label) %>%
  sample_n(n)

#

l <- "Broadleaved woodland"
 i <- 1

 for(l in labels[6:7]) {
   for(i in 1:n) {
    obs <- new_df[new_df$lc1_label==l,][i,]

    # check output directories exist
    trainDir <- paste0(odir, "train/", l, "/")
    testDir <- paste0(odir, "test/", l, "/")

    ifelse(!dir.exists(trainDir), dir.create(trainDir, recursive=T), FALSE)
    ifelse(!dir.exists(testDir), dir.create(testDir, recursive=T), FALSE)

    #get image file web url
    url_grid <- as.character(obs[which(colnames(obs)==paste("file_path_gisco",direction,sep="_"))])

    #file name to write to disk (include year)
    yr <- obs$year
    pid <- obs$point_id


    #filename for testing or training?
    if(i <= testsize) {
      ff <- paste0(testDir,pid, "_", direction, "_", yr, ".jpg")
    } else {
      ff <- paste0(trainDir,pid, "_", direction, "_", yr, ".jpg")
    }

    print(c(l,yr,pid,i))
    print(Sys.time())

    #check that file isn't already downloaded
    if(file.exists(ff)) {
      print("file already exists...skipping")
    } else{
       tryCatch(download.file(url_grid, ff, method = "auto", quiet = FALSE, mode = "wb"),
               error = function(e) writeLines(as.character(paste0("in address ", url_grid, "following error ", as.character(e)), outputFile)))

      #This should download the file to the directory you specify above (ff) and skips if url not found
    }

  }

}




## Batch 2 - training of most common woodland lc1_spec_labels

#output directory
odir <- "C:/Users/vwn831/ImageRecognition/lucas02/"

# get image urls for random subset of images for each label

n <- 500 #how many samples per label?
testsize <- 10
trainsize <- n-testsize
direction <- "east"

# work only with labels that have a minimum size
table(table(dtsub$lc1_spec_label)>500) #69 false, 27 true
minsize <- 500


 # tibble of lookup table grouped by lc1_labels with equal sample of size n for each
 set.seed(1)
 new_df <- dtsub %>%
   group_by(lc1_spec_label) %>%
   filter(table(lc1_spec_label) > minsize) %>%
   sample_n(n)

 labels <- unique(new_df$lc1_spec_label) #now using the specific label

 #

 l <- labels[1]
 i <- 1

outputFile <- file("logfile.txt")


for(l in labels) {
  for(i in 1:n) {
     obs <- new_df[new_df$lc1_spec_label==l,][i,]

     # check output directories exist
     trainDir <- paste0(odir, "train/", l, "/")
     testDir <- paste0(odir, "test/", l, "/")

     ifelse(!dir.exists(trainDir), dir.create(trainDir, recursive=T), FALSE)
     ifelse(!dir.exists(testDir), dir.create(testDir, recursive=T), FALSE)

     #get image file web url
     url_grid <- as.character(obs[which(colnames(obs)==paste("file_path_gisco",direction,sep="_"))])

     #file name to write to disk (include year)
     yr <- obs$year
     pid <- obs$point_id


     #filename for testing or training?
     if(i <= testsize) {
       ff <- paste0(testDir,pid, "_", direction, "_", yr, ".jpg")
     } else {
       ff <- paste0(trainDir,pid, "_", direction, "_", yr, ".jpg")
     }

     print(c(l,yr,pid,i))
     print(Sys.time())

     #check that file isn't already downloaded
     if(file.exists(ff)) {
       print("file already exists...skipping")
     } else{
       tryCatch(download.file(url_grid, ff, method = "auto", quiet = FALSE, mode = "wb"),
                error = function(e) writeLines(as.character(paste0("in address ", url_grid, "following error ", as.character(e)), outputFile)))

      }
       #This should download the file to the directory you specify above (ff)
   }
}
close(outputFile)

 # sink(type="message")
 # close(zz)
 #
 # readLines("logfile.Rout")

##############################################
#          Quality checks                    #
##############################################

# #checked a few of the labels for photos that seem to be taken off the point
#
# # read "off" photos
# off <- read.csv2(paste0(odir, "Photos_off_label2_checks.csv"))
# off$point_id <- as.numeric(sapply(strsplit(off$File_name, "_"), "[",1))
#
# test_cases <- dtsub %>%
#   filter(point_id%in%off$point_id) %>%
#   select(point_id,obs_dist,obs_direct,lu1_label,cprn_lc_label,cprn_urban,th_gps_dist) %>%
#   full_join(off)
#
#

