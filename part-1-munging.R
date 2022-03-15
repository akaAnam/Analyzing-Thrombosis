# libraries 
library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(cowplot)

# READING in data 
Athrom <- read.csv("part-1-data/TSUMOTO_A.csv", fileEncoding = "ISO-8859-1")
Bthrom <- read.csv("part-1-data/TSUMOTO_B.csv")
# for reading in non-utf-8 text formats (special characters)
Cthrom <- read.csv("part-1-data/TSUMOTO_C.csv", fileEncoding = "ISO-8859-1")

# ------- TIDY FORMAT ---------
  # Each variable must have its own column
  # Each observation must have its own row
  # Each type of observational unit forms a table (patient visit)

# ----------------------
# Thrombosis - A: CLEANING 
# ----------------------
lapply(Athrom, table)

# change all date columns to have the same format (YYYY/M/D)
Athrom$Description <- as.character(Athrom$Description)
Athrom$Description <- str_replace_all(Athrom$Description, "[.]", "/")

Athrom$First.Date <- str_replace_all(as.character(Athrom$First.Date), "[.]", "/")

# remove 19 from DOB 
# lowest dob year is 1912 sthe assumption that all DOBs are in the 1900s is fine
Athrom$Birthday <- substring(Athrom$Birthday,3)

# Column names 
Athrom_colnames <- c("id", "sex", "dob", "recorded_date", "first_date", "admitted", "diagnosis")
colnames(Athrom) <- Athrom_colnames

# Change admitted values + = TRUE, - = FALSE
Athrom$admitted <- str_replace_all(as.character(Athrom$admitted), "[+]", "T")
Athrom$admitted <- str_replace_all(as.character(Athrom$admitted), "[-]", "F")

# getting rid of all special characters/weird stuff in admitted column 
# define a function for "not in" 
'%!in%' <- Negate('%in%')

# replace all vals that arent TRUE or FALSE w/ empty space 
Athrom$admitted[Athrom$admitted %!in% c("T", "F")] <- ""

# DIAGNOSIS COL CLEANING AND FORMATTING
# --------------------------------------

# count number of columns and get max to figure out how many new "diagnosis_x" cols to add
diagnosis_commas <- str_count(Athrom$diagnosis, ",")

max(diagnosis_commas) # max number of other diseases a person has is 4 (3 + 1)

# removing special characters from diagnosis column 

Athrom <- separate(Athrom, col = diagnosis,
         c("diagnosis1","diagnosis2","diagnosis3","diagnosis4"),
         sep = ",")

# there are still some diagnosis columns with multiple diagnoses in them (ie. not comma seperated)
# getting tables to quickly find the multi-diagnosis values 
lapply(Athrom, table)

# Select Rows with Partial String Match to consolidate all of the SAME types of diagnosis 
# like SJS suspected will just become SJS etc.

## DIAGNOSIS 1
Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "BEHCET") ] <- "BEHCET"
Athrom$diagnosis1[Athrom$diagnosis1 == "ANA (+)"] <- "ANA"
Athrom$diagnosis1[Athrom$diagnosis1 == "AORTITIS susp"] <- "AORTITIS"
Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "APS") ] <- "APS"
Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "collagen") ] <- "collagen disease"
Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "purpura") ] <- "Henoch-Schoelein purpura"
# grouping Scleroderma and CREST - same thing 
Athrom$diagnosis1[Athrom$diagnosis1 == "CREST"] <- "Scleroderma"
Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "CPK") ] <- "CPK"
Athrom$diagnosis2[Athrom$diagnosis1 == "PM/DM"] <- substr(Athrom$diagnosis1[Athrom$diagnosis1 == "PM/DM"], start = 1, stop = 2)
Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "DM") ] <- "DM"
Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "FUO") ] <- "FUO"

Athrom$diagnosis2[Athrom$diagnosis1 == "SJS\035MCTD"] <- substr(Athrom$diagnosis1[Athrom$diagnosis1 == "SJS\035MCTD"], start = 1, stop = 3)
Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "MCTD") ] <- "MCTD"
Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "MRA") ] <- "Magnetic Resonance Angiogram"

Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "PMR") ] <- "Polymyalgia rheumatica" 
Athrom$diagnosis2[Athrom$diagnosis1 == "PM\035PSS\035RA"] <- "PSS"
Athrom$diagnosis3[Athrom$diagnosis1 == "PM\035PSS\035RA"] <- "RA"
Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "PM") ] <- "PM"
Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "Polymyalgia rheumatica" ) ] <- "PMR"

Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "PNc") ] <- "Polyneuritis cranialis"
Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "PN") ] <- "PN"
Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "Polyneuritis cranialis")] <- "PNc"
Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "Psoriatic")] <- "Psoriatic Arthritis"

Athrom[Athrom$diagnosis1 == "PSS/CREST"] <- "CREST"
Athrom$diagnosis2[Athrom$diagnosis1 == "PSS(CREST)"] <- "CREST"
Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "PSS")] <- "PSS"

Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "JRA")] <- "jra"
Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "ANAPHYLACTOID PURPURA NEPHRITIS")] <- "Anaphylactoid Purpura Nephritis"

Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "RA")] <- "RA"
Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "jra")] <- "JRA"
Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "Magnetic Resonance Angiogram")] <- "MRA"

Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "Raynaud")] <- "Raynaud"
Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "relapsing")] <- "relapsing polychondritis" 
Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "Sarcoidosis")] <- "Sarcoidosis"

# SJS 
Athrom$diagnosis2[Athrom$diagnosis1 == "SJS\035LUPOID HEPATITIS"] <- "Lupoid Hepatitis"
Athrom$diagnosis2[Athrom$diagnosis1 == "SLE\035SJS"] <- "SLE"
Athrom$diagnosis2[Athrom$diagnosis1 == "SLE  SJS susp"] <- "SLE"
Athrom$diagnosis2[Athrom$diagnosis1 == "WG\035SJS"] <- "WG"
Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "SJS")] <- "SJS"
Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "SLE")] <- "SLE"
Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "asculitis")] <- "Vasculitis"
Athrom$diagnosis1[str_detect(Athrom$diagnosis1, "WG")] <- "WG"
Athrom$diagnosis1[Athrom$diagnosis1 == "arthritis (r/o Behcets)"] <- "arthritis"

## DIAGNOSIS 2
# replacing NAs with empty strings 
Athrom$diagnosis2[is.na(Athrom$diagnosis2)] <- ""
Athrom$diagnosis2[str_detect(Athrom$diagnosis2, "SJS")] <- "SJS"
Athrom$diagnosis2[str_detect(Athrom$diagnosis2, "APS")] <- "APS"
Athrom$diagnosis2[str_detect(Athrom$diagnosis2, "Chronic EB")] <- "Chronic EB"
Athrom$diagnosis2[str_detect(Athrom$diagnosis2, "CREST")] <- "Scleroderma" 
Athrom$diagnosis2[str_detect(Athrom$diagnosis2, "MCTD")] <- "MCTD"
Athrom$diagnosis2[str_detect(Athrom$diagnosis2, "PM")] <- "PM"
Athrom$diagnosis2[str_detect(Athrom$diagnosis2, "PSS")] <- "PSS"
Athrom$diagnosis2[str_detect(Athrom$diagnosis2, "RA")] <- "RA"
Athrom$diagnosis2[str_detect(Athrom$diagnosis2, "SLE")] <- "SLE"

## DIAGNOSIS 3
# replacing NAs with empty strings
Athrom$diagnosis3[is.na(Athrom$diagnosis3)] <- ""
Athrom$diagnosis3[str_detect(Athrom$diagnosis3, "SLE")] <- "SLE"
Athrom$diagnosis3[str_detect(Athrom$diagnosis3, "SJS")] <- "SJS"
Athrom$diagnosis3[str_detect(Athrom$diagnosis3, "RA")] <- "RA"

# DIAGNOSIS 4 
Athrom$diagnosis4[is.na(Athrom$diagnosis4)] <- ""
Athrom$diagnosis4[str_detect(Athrom$diagnosis4, "RA")] <- "RA"
lapply(Athrom, table)

# ------------------------
# Thrombosis - B: CLEANING 
# ------------------------

# column names
bthrom_colnames <- c("id", "exam_date", "aCL_IgG", "aCL_IgM", "ANA", "ANA_pattern", "aCL_IgA", "diagnosis", "KCT", "RVVT", "LAC", "symptoms","thrombosis")
colnames(Bthrom) <- bthrom_colnames
# same date format as table A
Bthrom$exam_date <- substring(Bthrom$exam_date,3)

# count number of columns and get max to figure out how many new "diagnosis_x" cols to add
diagnosis_commas <- str_count(Bthrom$diagnosis, ",")

max(diagnosis_commas) # max number of other diseases a person has is 4 (3 + 1)

# removing special characters from diagnosis column 

Bthrom <- separate(Bthrom, col = diagnosis,
                   c("diagnosis1b","diagnosis2b","diagnosis3b","diagnosis4b"),
                   sep = ",")

# there are still some diagnosis columns with multiple diagnoses in them (ie. not comma seperated)
# getting tables to quickly find the multi-diagnosis values 

# DIAGNOSIS 1 
Bthrom$diagnosis1b[str_detect(Bthrom$diagnosis1b, "Adult Still") ] <- "Adult Still"
Bthrom$diagnosis1b[str_detect(Bthrom$diagnosis1b, "ANA") ] <- "ANA"
Bthrom$diagnosis1b[str_detect(Bthrom$diagnosis1b, "APS") ] <- "APS"
Bthrom$diagnosis1b[str_detect(Bthrom$diagnosis1b, "Behcet") ] <- "BEHCET"
Bthrom$diagnosis1b[str_detect(Bthrom$diagnosis1b, "collagen") ] <- "collagen disease"
Bthrom$diagnosis1b[str_detect(Bthrom$diagnosis1b, "Collagen") ] <- "collagen disease"
Bthrom$diagnosis1b[str_detect(Bthrom$diagnosis1b, "FUO") ] <- "FUO"
Bthrom$diagnosis1b[str_detect(Bthrom$diagnosis1b, "JRA") ] <- "JRA"
Bthrom$diagnosis1b[str_detect(Bthrom$diagnosis1b, "MCTD") ] <- "MCTD"
Bthrom$diagnosis1b[str_detect(Bthrom$diagnosis1b, "PN")] <- "PN"
Bthrom$diagnosis1b[str_detect(Bthrom$diagnosis1b, "PSS") ] <- "PSS"
Bthrom$diagnosis1b[str_detect(Bthrom$diagnosis1b, "RA") ] <- "RA"
Bthrom$diagnosis1b[str_detect(Bthrom$diagnosis1b, "SjS") ] <- "SJS"
Bthrom$diagnosis1b[str_detect(Bthrom$diagnosis1b, "SLE") ] <- "SLE"
Bthrom$diagnosis1b[str_detect(Bthrom$diagnosis1b, "Vasculitis") ] <- "vasculitis"

# DIAGNOSIS 2
# replacing NAs with empty space 
Bthrom$diagnosis2b[is.na(Bthrom$diagnosis2b)] <- ""

Bthrom$diagnosis2b[str_detect(Bthrom$diagnosis2b, "abortion") ] <- "Abortion"
Bthrom$diagnosis2b[str_detect(Bthrom$diagnosis2b, "APS") ] <- "APS"
Bthrom$diagnosis2b[str_detect(Bthrom$diagnosis2b, "CNS") ] <- "CNS"
Bthrom$diagnosis2b[str_detect(Bthrom$diagnosis2b, "depression") ] <- "Depression"
Bthrom$diagnosis2b[Bthrom$diagnosis2b == "paniculitis"] <- "Paniculitis"
Bthrom$diagnosis2b[str_detect(Bthrom$diagnosis2b, "SjS") ] <- "SJS"
Bthrom$diagnosis2b[str_detect(Bthrom$diagnosis2b, "SLE") ] <- "SLE"

# DIAGNOSIS 3
# replacing NAs with empty space 
Bthrom$diagnosis3b[is.na(Bthrom$diagnosis3b)] <- ""
Bthrom$diagnosis3b[str_detect(Bthrom$diagnosis3b, "preg") ] <- "Pregnant"
Bthrom$diagnosis3b[str_detect(Bthrom$diagnosis3b, "Preg") ] <- "Pregnant"
Bthrom$diagnosis3b[str_detect(Bthrom$diagnosis3b, "SjS") ] <- "SJS"

# Change KCT, RVVT, & LAC values + = TRUE, - = FALSE
Bthrom$KCT <- str_replace_all(as.character(Bthrom$KCT), "[+]", "T")
Bthrom$KCT <- str_replace_all(as.character(Bthrom$KCT), "[-]", "F")

Bthrom$RVVT <- str_replace_all(as.character(Bthrom$RVVT), "[+]", "T")
Bthrom$RVVT <- str_replace_all(as.character(Bthrom$RVVT), "[-]", "F")

Bthrom$LAC <- str_replace_all(as.character(Bthrom$LAC), "[+]", "T")
Bthrom$LAC <- str_replace_all(as.character(Bthrom$LAC), "[-]", "F")

lapply(Bthrom, table)

# dropping columns not needed: Symptoms - 11, diagnosis 4 - 15
Bthrom <- Bthrom[-c(11,15)]

# ------------------------
# JOINING A & B + CLEANING 
# ------------------------

# left join on Athrom & Bthrom 
Athrom = Athrom %>% left_join(Bthrom, by='id')

# find and drop duplicate diagnosis for each row 
athrom.test <- as.data.frame(Athrom)

athrom.test$diagnosis1[athrom.test$diagnosis1 == ""] <- NA
athrom.test$diagnosis2[athrom.test$diagnosis2 == ""] <- NA
athrom.test$diagnosis3[athrom.test$diagnosis3 == ""] <- NA
athrom.test$diagnosis4[athrom.test$diagnosis4 == ""] <- NA
athrom.test$diagnosis4[athrom.test$diagnosis1b == ""] <- NA
athrom.test$diagnosis4[athrom.test$diagnosis2b == ""] <- NA
athrom.test$diagnosis4[athrom.test$diagnosis3b == ""] <- NA

# reorder DF
reorderCols <- c(1:10,17:19,11:16,20:23)
athrom.test <- athrom.test[,reorderCols]

# replace duplicates with NA 
for (row in 1:1240){
  ind <- which(duplicated(as.list(athrom.test[row,c(7:13)]), incomparables = c(NA, "")))
  ind <- ind + 6
  athrom.test[row,ind] <- NA
}

# shift diagnosis to the left most columns 
# https://stackoverflow.com/questions/23285215/shifting-non-na-cells-to-the-left
athrom.test[,c(7:13)] <- as.data.frame(t(apply(athrom.test[,c(7:13)], 1, function(x) x[order(is.na(x))])))
athrom.test$diagnosis1[athrom.test$diagnosis1 == ""] <- NA
athrom.test$diagnosis2[athrom.test$diagnosis2 == ""] <- NA
athrom.test$diagnosis3[athrom.test$diagnosis3 == ""] <- NA
athrom.test$diagnosis4[athrom.test$diagnosis4 == ""] <- NA
athrom.test$diagnosis4[athrom.test$diagnosis1b == ""] <- NA
athrom.test$diagnosis4[athrom.test$diagnosis2b == ""] <- NA
athrom.test$diagnosis4[athrom.test$diagnosis3b == ""] <- NA

# dropping diagnosis1.y, diagnosis2.y, diagnosis3.y - they only contain dupes
nrow(athrom.test)
sum(is.na(athrom.test$diagnosis3b))
athrom.test <- athrom.test[-c(11:13)]

# still a few dupes in diagnosis 4 - cleaning that up 
athrom.test$diagnosis4[259] <- NA
athrom.test$diagnosis4[193] <- NA
athrom.test$diagnosis4[1027] <- NA

# changing column names 
Athrom <- athrom.test

Athrom[is.na(Athrom)] <- ""

# ------------------------
# Thrombosis - C: CLEANING 
# ------------------------
ncol(Cthrom)
Cthrom[Cthrom == ""] <- NA

# columns with more than 50% NAs 
# https://stackoverflow.com/questions/31848156/delete-columns-rows-with-more-than-x-missing
which(colMeans(!is.na(Cthrom)) > 0.5)

# dropping columns
Cthrom <- Cthrom[-c(2,7,9,10,12,14,15,25,26,27,32,43,44)]
Cthrom[is.na(Cthrom)] <- ""

# ------------------------
# JOINING A & C + CLEANING 
# ------------------------

names(Cthrom)[1] <- "id"
Cthrom[1] <- lapply(Cthrom[1], as.numeric)

# left join on Athrom & Cthrom 
Athrom = Athrom %>% left_join(Cthrom, by='id')

# getting rid of bad data 
Athrom$thrombosis[Athrom$thrombosis == 3] <- ""
Athrom <- Athrom[-5]
# removing all rows where the thrombosis column is NA since this info is central to our questions
Athrom$thrombosis[Athrom$thrombosis == ""] <- NA
sum(is.na(Athrom$thrombosis))
Athrom <- Athrom[!is.na(Athrom$thrombosis),]

# 412 patients with thrombosis 
length(unique(Athrom$id))

# melt diagnosis 
meltAthrom <- Athrom %>% pivot_longer(cols="diagnosis1":"diagnosis4", names_to = "diagnosis_num", values_to="diagnosis")
meltAthrom$diagnosis[meltAthrom$diagnosis == ""] <- NA
meltAthrom$diagnosis[str_detect(meltAthrom$diagnosis, "preg") ] <- "Pregnant"
meltAthrom$diagnosis[str_detect(meltAthrom$diagnosis, "abortion") ] <- "Abortion"
meltAthrom$diagnosis[str_detect(meltAthrom$diagnosis, "Abortion") ] <- "Abortion"
meltAthrom$diagnosis[str_detect(meltAthrom$diagnosis, "APS") ] <- "APS"

 
meltAthrom <- meltAthrom[-c(16,17,32,36:39)]
# remove NA in diagnosis columns
meltAthrom$diagnosis[is.na(meltAthrom$diagnosis)] <- ""

# write to csv
write.csv(meltAthrom, "thrombosis.csv",row.names = F)

# Notes for me -- 
# SLE is an autoimmune disease in which the immune system attacks its own tissues, 
# causing widespread inflammation and tissue damage in the affected organs. 
# It can affect the joints, skin, brain, lungs, kidneys, and blood vessels.
  # how does SLE effect blood and blood vessels.Lupus may lead to blood problems??
  # https://www.mayoclinic.org/diseases-conditions/lupus/symptoms-causes/syc-20365789
  # including a reduced number of healthy red blood cells (anemia) and an increased
  # risk of bleeding or blood clotting

# APS - increased risk of bloodclots 

# (MCTD) is a rare autoimmune disorder that is characterized by features commonly. Is seen in Lupus
# seen in three different connective tissue disorders: systemic lupus erythematosus, 
# scleroderma, and polymyositis. Some affected people may also have symptoms of rheumatoid arthritis
# high blood pressure in lungs - cholestrol check 

# brain infarction -  It is caused by disrupted blood supply (ischemia) and restricted oxygen supply (hypoxia)

# LDH - High levels of LDH indicate some form of tissue damage
# ALP enzyme found throughout the body mostly in the liver, bones, kidneys, and digestive system
# Albumin is a protein made by your liver. Albumin helps keep fluid in your bloodstream so it doesn't leak into other tissues. 
# prothrombin time - measures how long it takes for a clot to form in a blood sample
# APTT - helps evaluate a person's ability to appropriately form blood clots
# Fibrinogen is a protein produced by the liver. This protein helps stop bleeding by helping blood clots to form

# Ig G - antibody most common in blood circulation 
# Ig A - antibody blood protein that's part of your immune system
# Ig M-  blood and lymph fluid, this is the first antibody the body makes when it fights a new infection
# all other antibodies minus DNA ones