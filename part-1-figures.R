# libraries 
library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2) 
library(cowplot)
library(ggalluvial)
library(corrplot)
library(ggcorrplot)

# read in data
meltAthrom <- read.csv("thrombosis.csv")

# Note to TA: 
#   many of the columns in my thrombosis.csv dataset werent used
#   as i did EDA it became more clear what i wanted to use and what was irrelevent

# figure1.png - Top 20 diseases
# figure2.png - Thrombosis by Sex

# -------------
# EDA - Phase 1
# -------------

# Plugged in my variables of interest into the below plots 
# variables of interest according to the main diagnoses - so like blood stuff, & antibodies

# we have some crazy outliers for Immunoglobins 
boxplot(meltAthrom$aCL_IgG, meltAthrom$aCL_IgM, meltAthrom$aCL_IgA)
summary(meltAthrom$aCL_IgG) # max is 1502
summary(meltAthrom$aCL_IgM)  # max - 187122
summary(meltAthrom$aCL_IgA) # max - 48547

# Other antibodies
# white blood cells 
plot(meltAthrom$WBC, 
     ylim = c(0,1200))

meltAthrom$WBC <- lapply(meltAthrom$WBC, as.double)
summary(as.double(meltAthrom$WBC))
summary(as.double(meltAthrom$FG))

# density plot of _____ by thrombosis
ggplot(meltAthrom, aes(APTT, group=as.factor(thrombosis), fill=as.factor(thrombosis))) + 
  geom_density(alpha=.4)

# another denisty test
meltAthrom$FG[meltAthrom$FG == ""] <- NA
ggplot(meltAthrom, aes(as.double(FG), group=as.factor(thrombosis), fill=as.factor(thrombosis))) + 
  geom_density(alpha=.4)

# distribution of WBC by thrombosis 
ggplot(meltAthrom, aes(as.double(RBC),fill=as.factor(thrombosis))) + 
  geom_histogram(stat="count",
                 bins = 50,
                 binwidth = .1) 

ggplot(meltAthrom, aes(as.double(FG),fill=as.factor(thrombosis))) + 
  geom_histogram(stat="count",
                 bins = 50,
                 binwidth = .1) + 
  scale_y_continuous(limits = c(0,100))

ggplot(dummyAthrom, aes(as.double(WBC),fill=as.factor(thrombosis))) + 
  geom_histogram(stat="count",
                 bins = 50,
                 binwidth = .1) +
  scale_y_continuous(limits = c(0,300))

ggplot(meltAthrom, aes(as.factor(thrombosis), as.double(T.CHO), fill = as.factor(thrombosis)))+
  geom_boxplot(varwidth = T)

# boxplot with jitter
ggplot(meltAthrom, aes(as.factor(thrombosis), as.double(APTT), fill = as.factor(thrombosis)))+
  geom_boxplot(varwidth = T)+
  geom_jitter(size=0.4, alpha = 0.5)+
  theme_classic()

dummyAthrom <- meltAthrom[meltAthrom$thrombosis != 0,]

table(meltAthrom$APTT)

# Findings: 
# APTT is important - density, hist, boxplot. Could explain APS jumping up to #2 spot  in bar pot 
# PT as well has some bearing 
# LDH mean > 500 for no thrombosis and lower for thrombosis 1 but highest for thrombosis 2

# correlations with between blood indicators 
miniBlood <- meltAthrom[c(15,21:29)]
miniBlood$thrombosis[miniBlood$thrombosis == ""] <- NA
miniBlood$GLU[miniBlood$GLU == ""] <- NA
miniBlood$WBC[miniBlood$WBC == ""] <- NA
miniBlood$HCT[miniBlood$HCT == ""] <- NA
miniBlood$PLT[miniBlood$PLT == ""] <- NA
miniBlood$FG[miniBlood$FG == ""] <- NA

# get data into acceptable format for cor()
miniBlood <- lapply(miniBlood, as.double)
miniBlood <- as.data.frame(miniBlood)

# create corr matrix (ignoring NAs)
M <- cor(miniBlood, use = "complete.obs")

# corrplot
corrplot(M, method = "number")



# show.diag = F
# -------------
# EDA - Phase 2 
# -------------

# DIAGNOSIS PROBLEM - figure1
# -----------------
meltAthrom$diagnosis[meltAthrom$diagnosis == ""] <- NA

# subset columns we need into miniDiagnosis
miniDiagnosis <- meltAthrom %>%
  group_by(id, thrombosis) %>%
  distinct(diagnosis)
# sort all the diagnoses in descending order to get the top 15 - 20
miniDiagtable <- lapply(miniDiagnosis, table)
miniDiagtable <- miniDiagtable$diagnosis
# get descending order 
miniDiagtable <- miniDiagtable[order(-miniDiagtable)]
head(miniDiagtable, n=20)
# ANA is in the top 20 at a 4 way tie for 14th spot  

# list of top 20 diseases 
top20diagn <- names(head(miniDiagtable, n=20))
# get only the rows where diagnosis is in our top 20
miniDiagnosis <- filter(miniDiagnosis, diagnosis %in% top20diagn)


# steps below are so that we can order the bars in bar chart in descending order
counts <- miniDiagnosis %>%
  group_by(diagnosis) %>%
  tally()
colnames(counts)[1] <- "diagnosis"

miniDiagnosis <- miniDiagnosis %>% left_join(counts, by="diagnosis")

str(miniDiagnosis)

# plot
diagn_all <- ggplot(miniDiagnosis) + 
  geom_bar(mapping = aes(x= reorder(diagnosis, n), fill=as.factor(thrombosis)))+
  coord_flip()+
  scale_fill_manual(name="Thrombosis", 
                    labels = c("Negative", "Positive", "Positive - very severe"),
                    values = c("#83bb7f","#c0848c", "#8e2930"))+
  theme_minimal()+
  theme(legend.position = c(0.23, 0.2),
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid", colour = "grey"),
        axis.text.y = element_text(hjust = 1, vjust = 0.5),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0, size = 13)) + 
  scale_x_discrete(position="top") +
  scale_y_reverse()+
  ggtitle("Top 20 Diseases in All Patients")+
  ylab("Patients with Disease")


# plotting only people who have thrombosis 
# ----------------------------------------
# subset columns we need into miniDiagnosis
miniDiagnosis2 <- meltAthrom %>%
  group_by(id, thrombosis) %>%
  distinct(diagnosis)
# get only people with thrombosis
miniDiagnosis2 <- miniDiagnosis2[miniDiagnosis2[2] != 0,]
# Drop NAs 
miniDiagnosis2 <- na.omit(miniDiagnosis2)

# sort all the diagnoses in descending order to get the top 15 - 20
miniDiagtable <- lapply(miniDiagnosis2, table)
miniDiagtable <- miniDiagtable$diagnosis
# get descending order 
miniDiagtable <- miniDiagtable[order(-miniDiagtable)]
head(miniDiagtable, n=20)
# ANA is in the top 20 at a 4 way tie for 14th spot  

# list of top 20 diseases 
top20diagn <- names(head(miniDiagtable, n=20))
# get only the rows where diagnosis is in our top 20
miniDiagnosis2 <- filter(miniDiagnosis2, diagnosis %in% top20diagn)

# steps below are so that we can order the bars in bar chart in descending order
counts <- miniDiagnosis2 %>%
  group_by(diagnosis) %>%
  tally()
colnames(counts)[1] <- "diagnosis"

miniDiagnosis2 <- miniDiagnosis2 %>% left_join(counts, by="diagnosis")
miniDiagnosis2$diagnosis[str_detect(miniDiagnosis2$diagnosis, "Abortion") ] <- "Abortion"

# plot
diag_pos <- ggplot(miniDiagnosis2) + 
  geom_bar(mapping = aes(x= reorder(diagnosis, n), fill=as.factor(thrombosis)))+
  coord_flip()+
  xlab("Top 20 \n Diagnoses")+
  ylab("Patients with Disease")+
  scale_fill_manual(name="Thrombosis", labels = c("Positive", "Positive - very severe"),values = c("#c0848c", "#8e2930"))+
  theme_minimal()+
  ggtitle("Top 20 Diseases in Thrombosis Patients") +
  theme(legend.position = c(0.7, 0.16),
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid", colour = "grey"),
        axis.title.y =element_text(angle=0, vjust = 0.5, face="bold", color= "#a2a2a2"),
        plot.title = element_text(hjust = 1, size = 13))

# back to back bar chart: 
# https://stackoverflow.com/questions/54855767/back-to-back-bar-chart-with-one-common-y-axis-and-two-independent-x-values
plot_grid(diagn_all, diag_pos)
# -------------------


# BLOOD PROBLEM - figure 5
# -------------------

# Phase 1 EDA contained me exploring the correlations. Below is a better vis for it 

# correlation matrix to include factor data
# https://stackoverflow.com/questions/52554336/plot-the-equivalent-of-correlation-matrix-for-factors-categorical-data-and-mi

corrplot <- model.matrix(~0+., data=miniBlood) %>%
  cor(use = "complete.obs") %>%
  ggcorrplot(type = "lower",
             outline.color = "white",
             lab=T, show.diag = F, lab_size = 2.5,
             colors = c("#B53038", "white","#83bb7f"), title = "Correlation Matrix of Blood Indicators")

# changing size of axis tick marks
corrplot + theme(axis.text.x = element_text(size = 10),
                 axis.text.y = element_text(size=10))
# -------------------

# # BLOOD PROBLEM - figure 6, 7, & 8
# -------------------

# from EDA Phase 1 but modified

# density plot of APTT by thrombosis
ggplot(meltAthrom, aes(APTT, group=as.factor(thrombosis), fill=as.factor(thrombosis))) + 
  geom_density(alpha=.5)+
  scale_fill_manual(name="Thrombosis",
                    labels = c("Negative", "Positive", "Positive - very severe"),
                    values = c("#83bb7f","#A7191B","#F9A03F"))+
  theme_minimal()+
  geom_vline(xintercept = 45, linetype = "dashed", color = "gray5", lwd=.4)+
  ggtitle("APTT Distribution by Thrombosis")+ 
  ylab("Density") + 
  labs(caption = "Normal range for APTT: N < 45")+
  theme(plot.caption = element_text(size = 8, face = "italic"))

# maybe - # 

# density plot of FG by thrombosis
meltAthrom$FG[meltAthrom$FG == ""] <- NA
ggplot(meltAthrom, aes(as.double(FG), group=as.factor(thrombosis), fill=as.factor(thrombosis))) + 
  geom_density(alpha=.5)+
  scale_fill_manual(name="Thrombosis",
                    labels = c("Negative", "Positive", "Positive - very severe"),
                    values = c("#83bb7f","#A7191B","#F9A03F"))+
  theme_minimal()+
  geom_vline(xintercept = c(150,450), linetype = "dashed", color = "gray5", lwd=.4)+
  ggtitle("FG Distribution by Thrombosis")+ 
  ylab("Density") + 
  xlab("FG")+
  labs(caption = "Normal range for FG: 150 < N < 450")+
  theme(plot.caption = element_text(size = 8, face = "italic"))

# Total FG Distribution
ggplot(meltAthrom, aes(as.double(FG)))+
  geom_density(alpha=0.4, fill = "#127475")+
  geom_vline(xintercept = c(150,450), linetype = "dashed", color = "gray5", lwd=.4)+
  theme_minimal()+
  ggtitle("FG Distribution")+ 
  ylab("Density") + 
  xlab("FG")+
  labs(caption = "Normal range for FG: 150 < N < 450")+
  theme(plot.caption = element_text(size = 8, face = "italic"))




# -------------------

# GENDER PROBLEM - figure 2
# -------------------

# get relevant demographic data 
demographic <- meltAthrom %>%
  group_by(sex, thrombosis) %>%
  summarise(count=n())
demographic <- demographic[-1,]
# make type suitable for manipulation 
demographic$sex <- lapply(demographic$sex, as.character)
demographic$thrombosis <- lapply(demographic$thrombosis, as.character)
# change to correct lables - this is for correct labeling when using ggplot
demographic$sex[demographic$sex == "F"] <- "Female" 
demographic$sex[demographic$sex == "M"] <- "Male" 
demographic$thrombosis[demographic$thrombosis == "0"] <- "No Thrombosis" 
demographic$thrombosis[demographic$thrombosis == "1"] <- "Thrombosis" 
demographic$thrombosis[demographic$thrombosis == "2"] <- "Severe Thrombosis" 
# unlist the df
# https://stackoverflow.com/questions/14521492/weird-behaviour-by-ordering-a-data-frame
demographic <- as.data.frame(lapply(demographic, unlist))

# PLOT (alluvium)
ggplot(data = demographic,
       aes(axis1 = as.factor(sex), axis2 = as.factor(thrombosis), y=count))+
  geom_alluvium(aes(fill=as.factor(thrombosis)), alpha=0.85)+
  geom_stratum()+
  geom_text(stat="stratum",
            aes(label=after_stat(stratum)), size = 3)+
  scale_x_discrete(limits = c("sex", "thrombosis"),
                   expand = c(.09, .09)) + 
  scale_fill_manual(values = c("#83bb7f","#8e2930", "#d3a9af"))+
  theme_void()+
  guides(fill = guide_legend(title = "Thrombosis")) + 
  ggtitle("Thrombosis Frequency by Gender")


# -------------------------------


# ANTIBODY/IMMUNOGLOBIN PROBLEM - figure 3 & 4 
# -------------------------------
# Immunoglobins only 

# Below line is from EDA Phase 1 
boxplot(meltAthrom$aCL_IgG, meltAthrom$aCL_IgM, meltAthrom$aCL_IgA, meltAthrom$ANA)
# normalizing the data because original boxplots looked bad
boxplot(log(meltAthrom$aCL_IgG), log(meltAthrom$aCL_IgM), log(meltAthrom$aCL_IgA),log(meltAthrom$ANA))

# subset to keep only Immunoglobins, ANA and thromb
# edit - taking out ANA because there are only 6 possible outcomes. not good for boxplot
miniAnti <- meltAthrom[,c(1,7:8,11,15)]
# remove duplicate rows (again 412 patients)
miniAnti <- miniAnti[!duplicated(miniAnti),]
# normalize antibody data 
miniAnti[2:4] <- lapply(miniAnti[2:5], log)

# convert our mini Anitbody DF to long format
names(miniAnti)
miniAnti <- miniAnti %>%
  pivot_longer(cols=c("aCL_IgG","aCL_IgM","aCL_IgA"), names_to = "antibody", values_to="value")

miniAnti$value[is.infinite(miniAnti$value)] <- NA

ggplot(miniAnti, 
       aes(antibody, y=value))+
  geom_boxplot(aes(fill = antibody), 
               show.legend = F, 
               outlier.size = 0.8, outlier.alpha = 0.8,outlier.color = "black", outlier.shape = 1)+
  geom_point(position = position_jitterdodge(jitter.width = 1.5, dodge.width = 0), # for the jitter within box plot
             aes(colour=factor(thrombosis)), size=2, alpha=0.7)+
  scale_fill_manual(values = c("#FFFFFF","#FFFFFF", "#FFFFFF"))+
  scale_color_manual(values = c("#9cc999", "#8E2930", "#F0803C"), 
                     name="Thrombosis", 
                     labels = c("Negative", "Positive", "Positive - very severe"))+
  theme_minimal()+
  ylab("Value")+
  xlab("Antibody (immunoglobin)") + 
  scale_x_discrete(labels = c("aCl IgA", "aCl IgG", "aCl IgM"))+
  ggtitle("Distribution of Anticardiolipin Antibodies for All Patients")

# WITHOUT NEGATIVE cases
miniAntiThrom <- miniAnti[miniAnti$thrombosis != 0,]
miniAntiThrom$value[is.infinite(miniAntiThrom$value)] <- NA

ggplot(miniAntiThrom, 
       aes(antibody, y=value))+
  geom_boxplot(aes(fill = antibody), 
               show.legend = F, 
               outlier.size = 0.8, outlier.alpha = 0.8,outlier.color = "black", outlier.shape = 1)+
  geom_point(position = position_jitterdodge(jitter.width = 1.5, dodge.width = 0), # for the jitter within box plot
             aes(colour=factor(thrombosis)), size=2, alpha=0.7)+
  scale_fill_manual(values = c("#FFFFFF","#FFFFFF", "#FFFFFF"))+
  scale_color_manual(values = c("#8E2930", "#F0803C"), 
                     name="Thrombosis", 
                     labels = c("Positive", "Positive - very severe"))+
  theme_minimal()+
  ylab("Value")+
  xlab("Antibody (immunoglobin)") + 
  scale_x_discrete(labels = c("aCl IgA", "aCl IgG", "aCl IgM"))+
  ggtitle("Distribution of Anticardiolipin Antibodies for Thrombosis Patients")



