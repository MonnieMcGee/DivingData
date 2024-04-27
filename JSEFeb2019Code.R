# Code for Analysis of Diving Data 
# JSE Data Sets and Stories article
# February 2019

# Packages needed
library(gdata)
library(tidyverse)
library(reshape2)
library(xtable)

## Data manipulation
## Diver's names were substituted with letters 
## using the code below. Original outcome is in another file.
## Girls are A - M. Boys are N - X.
# sample(as.character(unique(dive_girls$Diver)),13)
# sample(as.character(unique(dive_boys$Diver)),12)

## The redacted data doesn't have names or schools affiliation for the divers.
## "NA"s were used for data that was not gathered during the competition.
## '0' means a failed dive.

# Enter the data
## Download "DivingData2018Redacted.xlsx" 
## Place the data in a directory called "DivingData" 
## In the Documents folder on your home computer. 
## In R, type: setwd("/Documents/DivingData") or whatever
## directory path is appropriate
girls <- read.xls("DivingScoresRedacted.xlsx",sheet=1)
boys <- read.xls("DivingScoresRedacted.xlsx",sheet=2)

################## Data Manipulation #############

## Adding a column for the Round scores and the Running Total
## The round score is the 20% trimmed mean for the judges' scores
## multiplied by the DD of the dive for that round for each diver.
## Cumulative round scores are equal to the cumulative sum 
## of each round score for each diver.

dg <- girls %>% rowwise() %>% mutate(MeanScore = mean(c(Judge1,Judge2,Judge3,Judge4,Judge5),trim=0.2))
dg2 <- dg %>% rowwise() %>% mutate(RoundScore = round(MeanScore*DD*3,3))
dg2 <- as.data.frame(dg2)
dive_girls <- dg2 %>% group_by(Diver) %>% mutate(CumScore = cumsum(RoundScore))

db <- boys %>% rowwise() %>% mutate(MeanScore = mean(c(Judge1,Judge2,Judge3,Judge4,Judge5),trim=0.2))
db2 <- db %>% rowwise() %>% mutate(RoundScore = round(MeanScore*DD*3,3))
db2 <- as.data.frame(db2)
dive_boys <- db2 %>% group_by(Diver) %>% mutate(CumScore = cumsum(RoundScore))

## Create categorical variable for "difficult" dives
## If DD <= 1.7, DDfactor is low, if DD > 1.7, DDfactor is high.
dive_girls$DDfactor <- as.factor(ifelse(dive_girls$DD > 1.7, "High","Low"))
dive_boys$DDfactor <- as.factor(ifelse(dive_boys$DD > 1.7, "High","Low"))

# Put boys and girls scores together
allDives <- bind_rows("Girls"=dive_girls,"Boys"=dive_boys,.id="Gender")
# Coerce Gender into a factor because it appears as class character
allDives$Gender <- as.factor(allDives$Gender)
allDives$Diver <- as.factor(allDives$Diver)
# Check to see that all variables have the expected R class 
str(allDives)

###### "Melting" the data into long format ########
## Also in paper
dive_girls_melt <- melt(dive_girls,measure.vars=c("Judge1","Judge2","Judge3","Judge4","Judge5"),variable.name="Judge",value.name="Award")
dive_boys_melt <- melt(dive_boys,measure.vars=c("Judge1","Judge2","Judge3","Judge4","Judge5"),variable.name="Judge",value.name="Award")
allDives_melt <- melt(allDives,measure.vars=c("Judge1","Judge2","Judge3","Judge4","Judge5"),variable.name="Judge",value.name="Award")

######## "Casting" the data into wide format #########
## Place on rows with DD on columns
dcast(dive_girls,Place ~ DD, mean, value.var="RoundScore")
## Diver by Round for the first Judge
dcast(dive_girls,Diver ~ Round, value.var="Judge1")
## Diver on rows now
dcast(dive_girls,Diver ~ DD, mean, value.var="RoundScore")
## All of the data with Judge's Scores instead of round scores
dcast(allDives_melt, Round + Diver + Dive + Position + Approach ~ Judge,mean,value.var="Award")
# Awards for each Diver by Round and Judge
dcast(allDives_melt,Diver +Round ~ Judge,mean,value.var="Award")

######### Table of Descriptive Statistics##############
## Descriptive statistics

# Produce Table 1 in the paper
xtable(summary(allDives))
# Latex code edited to exclude factor variables, since their summaries are 
# simply a tabulation of the number of observations in each category.

######### Simple Graphics ##############
# Histogram of meet scores using base R
hist(allDives$MeetScore, prob = T, xlab="Final Meet Score", ylab="Percentage", main="Distribution of Final Score for All Divers", col = "mistyrose")

# Using ggplot2. More painful for now
ggplot(data=allDives,aes(MeetScore)) + geom_histogram(bins=10)
# Make facets for males and females - easier with ggplot2
ggplot(data=allDives,aes(MeetScore)) + geom_histogram(bins=10) + facet_grid(~Gender)
# Make it one column and 2 rows
ggplot(data=allDives,aes(MeetScore)) + geom_histogram(bins=10) + facet_wrap(~Gender, ncol=1)

# Ugly boxplot
ggplot(data=allDives,aes(x=Gender, y = MeetScore)) + geom_boxplot()

# Distribution of judge scores for each Judge by Gender
## Boxplot of judges scores
pdf(file="JudgeBoxplot.pdf")
ggplot(data=allDives_melt,aes(x=Judge,y=Award,fill=Gender)) + geom_boxplot() + theme(legend.position="top") + labs(title="Scores for Each Judge by Gender of Diver")
dev.off()

## Trajectory of round scores for male and female divers on the same graph
pdf(file="SpaghettiPlot.pdf")
ggall <- ggplot(allDives, aes(x=Round,y=RoundScore,colour=Diver)) + geom_line(aes(group=Diver))
ggall + labs(title="Scores by Round for each Diver",x="Round",y="Round Score") + theme(legend.position="bottom") 
dev.off()

## Trajectory of round scores with facets for male and female divers
pdf(file="SpaghettiFacets.pdf")
ggfacet <-  ggplot(allDives, aes(x=Round,y=RoundScore,colour=Diver)) + geom_line(aes(group=Diver))+facet_grid(Gender~.) 
ggfacet + labs(title="Scores by Round for each Diver",x="Round",y="Round Score") + theme(legend.position="bottom")
dev.off()


## Visualizations for DD and placement

# Score by Place with coloring for DD
pdf(file="GirlsPlaceDDrev.pdf")
gg <- ggplot(dive_girls_melt, aes(x=as.factor(Place),y=Award,colour=DDfactor)) + geom_jitter(position=position_jitter(width=0.1,height=0.1)) + theme(legend.position="bottom") 
gg + labs(title="Score by Place - Girls",x="Place",y="Score per Dive") 
dev.off()

# Same for boys
pdf(file="BoysPlaceDDrev.pdf")
gg <- ggplot(dive_boys_melt, aes(x=as.factor(Place),y=Award,colour=DDfactor) + geom_jitter(position=position_jitter(width=0.1,height=0.1)) + theme(legend.position="bottom") 
             gg + labs(title="Score by Place - Boys",x="Place",y="Score per Dive") 
             dev.off()
             
             # Add average of three middle scores for each dive.
             # Leaving out DD and using mean of the three middle scores
             
             pdf(file="GirlsRoundAvgDD.pdf")
             ggg <- ggplot(dive_girls, aes(x=as.factor(Place),y=MeanScore,colour=DDfactor,shape=DDfactor)) + geom_jitter(position=position_jitter(width=0.1,height=0.1)) + labs(title="Average Rating by Place and DD - Girls",x="Final Diver Rank",y="Average of 3 Middle Judges' Scores") 
             ggg + theme(legend.position="top") + coord_cartesian(ylim = c(0, 8)) + scale_colour_manual(values=c("#E69F00", "#56B4E9"), 
                                                                                                        name="Degree of Difficulty",
                                                                                                        breaks=c("High", "Low"),
                                                                                                        labels=c("> 1.7","<= 1.7")) +
               scale_shape_manual(name="Degree of Difficulty",values = c(17,19),labels=c("> 1.7","<= 1.7"))
             dev.off()
             
             # adjust y scale with + coord_cartesian(ylim = c(0, 8)) 
             
             pdf(file="BoysRoundAvgDD.pdf")
             ggb <- ggplot(dive_boys, aes(x=as.factor(Place),y=MeanScore,colour=DDfactor)) + geom_jitter(position=position_jitter(width=0.1,height=0.1)) + theme(legend.position="top") + coord_cartesian(ylim = c(0, 8)) 
             ggb + labs(title="Average Rating by Place and DD - Boys",x="Final Diver Rank",y="Average of 3 Middle Judges' Scores") + scale_colour_manual(values=c("#E69F00", "#56B4E9"), 
                                                                                                                                                         name="Degree of Difficulty",
                                                                                                                                                         breaks=c("High", "Low"),
                                                                                                                                                         labels=c("> 1.7","<= 1.7")) +
               scale_shape_manual(name="Degree of Difficulty",values = c(17,19),labels=c("> 1.7","<= 1.7"))
             dev.off()
             
             ## Regression and Correlation
             ## Pearson and Spearman correlation between final score and DD
             
             # Correlation with all DD for girls
             cor(dive_girls$DD,dive_girls$MeanScore) #  -0.311171
             cor(dive_girls$DD,dive_girls$MeanScore, method="s") # -0.2895066
             ## Negative indicates that as DD increases, score decreases
             
             # Correlation with all DD for boys
             cor(dive_boys$DD,dive_boys$MeanScore) # -0.002008412
             cor(dive_boys$DD,dive_boys$MeanScore,method="s") # -0.04404707
             
             # Correlation with average DD 
             DDavg <- dive_girls %>% group_by(Diver) %>% mutate(AvgDD = mean(DD))
             avgDDgirls <- DDavg$AvgDD[c(rep(FALSE,10),TRUE)]
             scoreGirls <- DDavg$MeanScore[c(rep(FALSE, 10),TRUE)]
             cor(avgDDgirls,scoreGirls) # -0.2612614
             cor(avgDDgirls,scoreGirls,method="s") # -0.4005854
             
             DDavgB <- dive_boys %>% group_by(Diver) %>% mutate(AvgDD = mean(DD))
             avgDDboys <- DDavgB$AvgDD[c(rep(FALSE, 10),TRUE)]
             scoreBoys <- DDavgB$MeanScore[c(rep(FALSE, 10),TRUE)]
             cor(avgDDboys,scoreBoys) # 0.6694188
             cor(avgDDboys,scoreBoys, method="s") # 0.7641035
             
             # Correlation with median DD
             DDmed <- dive_girls %>% group_by(Diver) %>% mutate(MedDD = median(DD))
             medDDgirls <- DDmed$MedDD[c(rep(FALSE,10),TRUE)]
             cor(medDDgirls,scoreGirls) #-0.4608702
             cor(medDDgirls,scoreGirls, method="s") #-0.5427868
             
             DDmedB <- dive_boys %>% group_by(Diver) %>% mutate(MedDD = median(DD))
             medDDboys <- DDmedB$MedDD[c(rep(FALSE,10),TRUE)]
             cor(medDDboys,scoreBoys) # 0.7739377
             cor(medDDboys,scoreBoys,method="s") # 0.7813481
             
             # scatterplot matrix
             avgScores <- data.frame(Avg=c(avgDDgirls,avgDDboys),Med=c(medDDgirls,medDDboys),Score=c(scoreGirls,scoreBoys),Gender=c(rep("Girls",13),rep("Boys",12)))
             
             pdf(file="DDscoreMatrix.pdf")
             pairs(avgScores[,1:3],col=ifelse(avgScores$Gender=='Boys','cyan4','coral'),pch=ifelse(avgScores$Gender=='Boys',15,19),cex=1.5,labels=c("Average DD","Median DD", "Mean Score"))
             dev.off()
             
             ## Round off error
             scoreMat <- matrix(c(dive_girls$CumScore,dive_boys$CumScore),byrow=TRUE, nrow=25)
             meetMat <- matrix(c(dive_girls$MeetScore,dive_boys$MeetScore),byrow=TRUE, nrow=25)
             scoreDF <- data.frame(MeetScore=meetMat[,11],calcScore = scoreMat[,11],Gender=as.factor(c(rep("Female",13),rep("Male",12))))
             scoreDF <- scoreDF  %>% arrange(desc(MeetScore)) %>% arrange(Gender)
             scoreDFfem <- subset(scoreDF,Gender=='Female')
             scoreDFfem$calcPlace <- order(scoreDFfem$calcScore,decreasing=TRUE)
             scoreDFmale <- subset(scoreDF,Gender=='Male')
             scoreDFmale$calcPlace <- order(scoreDFmale$calcScore,decreasing=TRUE)
             scoreDF <- bind_rows(scoreDFfem,scoreDFmale)
             scoreDF$Change <- ifelse(scoreDF$Place != scoreDF$calcPlace,"Yes","No")
             ggplot(scoreDF, aes(x=MeetScore,y=calcScore,colour=Gender))+geom_point()
             scoreDFmelt <- melt(scoreDF, measure.vars=c("MeetScore","calcScore"),variable.name="ScoreType",value.name="Score")
             pdf(file="RoundOff.pdf")
             ggplot(scoreDFmelt,aes(x=ScoreType,y=Score,colour=Change)) + geom_point() + geom_line(aes(group=Place)) + facet_grid(cols=vars(Gender)) + theme(legend.position="bottom")
             dev.off()
             
             ## Interrater agreement
             # Jitter plot of Award by Judge with colors for divers
             ggplot(dive_girls_melt, aes(x=Judge,y=Award,colour=Diver)) + geom_jitter(position=position_jitter(width=0.1,height=0.1))
             
             # Jitter plot of Award by Diver with colors for dives
             # Not all divers do all dives
             # Each dot is associated with a rating and ratings are specific to judges
             ggplot(dive_girls_melt,aes(x=Diver,y=Award,colour=Dive)) + geom_jitter(position=position_jitter(width=0.1,height=0.1))
             
             ggplot(dive_boys_melt,aes(x=Diver,y=Award,colour=Dive)) + geom_jitter(position=position_jitter(width=0.1,height=0.1))
             
             ## Jitter plot of Award by Diver with color for Judge
             pdf(file="GirlsDivingScoresPlace.pdf")
             gg1 <- ggplot(dive_girls_melt, aes(x=as.factor(Place),y=Award,colour=Judge)) + geom_jitter(position=position_jitter(width=0.1,height=0.1)) + 
               coord_cartesian(ylim = c(0, 8))  + theme(legend.position="bottom")
             gg1 + labs(title="Round Score by Diver - Girls",x="Place",y="Dive Score")
             dev.off()
             
             # Same for boys
             ## Jitter plot of Award by Dive with colours for Judge
             pdf(file="BoysDivingScoresPlace.pdf")
             gg2 <- ggplot(dive_boys_melt, aes(x=as.factor(Place),y=Award,colour=Judge)) + geom_jitter(position=position_jitter(width=0.1,height=0.1)) +
               theme(legend.position="bottom") 
             gg2 + labs(title="Round Score by Diver - Boys",x="Place",y="Dive Score")
             dev.off()
             
             ###### Calculation of Fleiss's kappa and ICC requires libraries below
             require(irr)
             require(psych)
             # Fleiss's kappa is not correct. Does not account for multiple ratings per subject.
             # Need to obtain an n subjects by m raters matrix 
             # Will have to do 11 matrices - one for each round.
             # Divers do not do the same dives in the same order.
             # Should we do this by round or by dive? 
             
             # Exact round 1 data (Round), Judges 1 - 5 scores, for all subjects
             # Girls
             girls_r1 <- subset(dive_girls, Round == 1, select = c("Judge1","Judge2","Judge3","Judge4","Judge5"))
             girls_fk1 <- kappam.fleiss(girls_r1)
             girls_r2 <- subset(dive_girls, Round == 2, select = c("Judge1","Judge2","Judge3","Judge4","Judge5"))
             girls_fk2 <- kappam.fleiss(girls_r2)
             girls_r3 <- subset(dive_girls, Round == 3, select = c("Judge1","Judge2","Judge3","Judge4","Judge5"))
             girls_fk3 <- kappam.fleiss(girls_r3)
             girls_r4 <- subset(dive_girls, Round == 4, select = c("Judge1","Judge2","Judge3","Judge4","Judge5"))
             girls_fk4 <- kappam.fleiss(girls_r4)
             girls_r5 <- subset(dive_girls, Round == 5, select = c("Judge1","Judge2","Judge3","Judge4","Judge5"))
             girls_fk5 <- kappam.fleiss(girls_r5)
             girls_r6 <- subset(dive_girls, Round == 6, select = c("Judge1","Judge2","Judge3","Judge4","Judge5"))
             girls_fk6 <- kappam.fleiss(girls_r6)
             girls_r7 <- subset(dive_girls, Round == 7, select = c("Judge1","Judge2","Judge3","Judge4","Judge5"))
             girls_fk7 <- kappam.fleiss(girls_r7)
             girls_r8 <- subset(dive_girls, Round == 8, select = c("Judge1","Judge2","Judge3","Judge4","Judge5"))
             girls_fk8 <- kappam.fleiss(girls_r8)
             girls_r9 <- subset(dive_girls, Round == 9, select = c("Judge1","Judge2","Judge3","Judge4","Judge5"))
             girls_fk9 <- kappam.fleiss(girls_r9)
             girls_r10 <- subset(dive_girls, Round == 10, select = c("Judge1","Judge2","Judge3","Judge4","Judge5"))
             girls_fk10 <- kappam.fleiss(girls_r10)
             girls_r11 <- subset(dive_girls, Round == 11, select = c("Judge1","Judge2","Judge3","Judge4","Judge5"))
             girls_fk11 <- kappam.fleiss(girls_r11)
             
             kappa_girls <- c(girls_fk1$value, girls_fk2$value, girls_fk3$value, girls_fk4$value, girls_fk5$value, girls_fk6$value, girls_fk7$value, girls_fk8$value, girls_fk9$value, girls_fk10$value, girls_fk11$value)
             
             pval_girls <- c(girls_fk1$p.value, girls_fk2$p.value, girls_fk3$p.value, girls_fk4$p.value, girls_fk5$p.value, girls_fk6$p.value, girls_fk7$p.value, girls_fk8$p.value, girls_fk9$p.value, girls_fk10$p.value, girls_fk11$p.value)
             
             
             # Boys
             boys_r1 <- subset(dive_boys, Round == 1, select = c("Judge1","Judge2","Judge3","Judge4","Judge5"))
             boys_fk1 <- kappam.fleiss(boys_r1)
             boys_r2 <- subset(dive_boys, Round == 2, select = c("Judge1","Judge2","Judge3","Judge4","Judge5"))
             boys_fk2 <- kappam.fleiss(boys_r2)
             boys_r3 <- subset(dive_boys, Round == 3, select = c("Judge1","Judge2","Judge3","Judge4","Judge5"))
             boys_fk3 <- kappam.fleiss(boys_r3)
             boys_r4 <- subset(dive_boys, Round == 4, select = c("Judge1","Judge2","Judge3","Judge4","Judge5"))
             boys_fk4 <- kappam.fleiss(boys_r4)
             boys_r5 <- subset(dive_boys, Round == 5, select = c("Judge1","Judge2","Judge3","Judge4","Judge5"))
             boys_fk5 <- kappam.fleiss(boys_r5)
             boys_r6 <- subset(dive_boys, Round == 6, select = c("Judge1","Judge2","Judge3","Judge4","Judge5"))
             boys_fk6 <- kappam.fleiss(boys_r6)
             boys_r7 <- subset(dive_boys, Round == 7, select = c("Judge1","Judge2","Judge3","Judge4","Judge5"))
             boys_fk7 <- kappam.fleiss(boys_r7)
             boys_r8 <- subset(dive_boys, Round == 8, select = c("Judge1","Judge2","Judge3","Judge4","Judge5"))
             boys_fk8 <- kappam.fleiss(boys_r8)
             boys_r9 <- subset(dive_boys, Round == 9, select = c("Judge1","Judge2","Judge3","Judge4","Judge5"))
             boys_fk9 <- kappam.fleiss(boys_r9)
             boys_r10 <- subset(dive_boys, Round == 10, select = c("Judge1","Judge2","Judge3","Judge4","Judge5"))
             boys_fk10 <- kappam.fleiss(boys_r10)
             boys_r11 <- subset(dive_boys, Round == 11, select = c("Judge1","Judge2","Judge3","Judge4","Judge5"))
             boys_fk11 <- kappam.fleiss(boys_r11)
             
             kappa_boys <- c(boys_fk1$value, boys_fk2$value, boys_fk3$value, boys_fk4$value, boys_fk5$value, boys_fk6$value, boys_fk7$value, boys_fk8$value, boys_fk9$value, boys_fk10$value, boys_fk11$value)
             
             pval_boys <- c(boys_fk1$p.value, boys_fk2$p.value, boys_fk3$p.value, boys_fk4$p.value, boys_fk5$p.value, boys_fk6$p.value, boys_fk7$p.value, boys_fk8$p.value, boys_fk9$p.value, boys_fk10$p.value, boys_fk11$p.value)
             
             kappa <- c(kappa_girls,kappa_boys)
             round <- rep(1:11,2)
             gender <- c(rep("Female",11),rep("Male",11))
             
             df_fleiss <- data.frame(Kappa = kappa, Round = round, Gender=gender)
             
             pdf(file="fleissKappa.pdf")
             ggplot(df_fleiss, aes(x=as.factor(Round),y=Kappa, group=Gender)) +  geom_line(aes(linetype=Gender, colour=Gender), size=2) + geom_point(aes(color=Gender))+labs(title="Fleiss's Kappa for 5 Judges and 11 Dives",x="Round",y="Fleiss's Kappa") + theme(legend.position="bottom") + scale_colour_manual(values=c(Male="#339999",Female="#663399"))
             dev.off()
             
             # Problems with Fleiss's kappa
             # the ratings for the diving data are ordinal, not categorical
             # must be done for each round separately
             # The number of judges has an effect on the value
             
             ## Intra class correlation
             # Again, must do this for each round
             
             girls_icc1<- ICC(girls_r1,lmer=FALSE)$results$ICC[6]
             girls_icc2<- ICC(girls_r2,lmer=FALSE)$results$ICC[6]
             girls_icc3<- ICC(girls_r3,lmer=FALSE)$results$ICC[6]
             girls_icc4<- ICC(girls_r4,lmer=FALSE)$results$ICC[6]
             girls_icc5<- ICC(girls_r5,lmer=FALSE)$results$ICC[6]
             girls_icc6<- ICC(girls_r6,lmer=FALSE)$results$ICC[6]
             girls_icc7<- ICC(girls_r7,lmer=FALSE)$results$ICC[6]
             girls_icc8<- ICC(girls_r8,lmer=FALSE)$results$ICC[6]
             girls_icc9<- ICC(girls_r9,lmer=FALSE)$results$ICC[6]
             girls_icc10<- ICC(girls_r10,lmer=FALSE)$results$ICC[6]
             girls_icc11<- ICC(girls_r11,lmer=FALSE)$results$ICC[6]
             
             icc_girls <- c(girls_icc1, girls_icc2, girls_icc3, girls_icc4, girls_icc5, girls_icc6, girls_icc7, girls_icc8, girls_icc9, girls_icc10, girls_icc11)
             
             
             # For boys
             boys_icc1<- ICC(boys_r1,lmer=FALSE, missing=FALSE)$results$ICC[6]
             boys_icc2<- ICC(boys_r2,lmer=FALSE)$results$ICC[6]
             boys_icc3<- ICC(boys_r3,lmer=FALSE)$results$ICC[6]
             boys_icc4<- ICC(boys_r4,lmer=FALSE)$results$ICC[6]
             boys_icc5<- ICC(boys_r5,lmer=FALSE)$results$ICC[6]
             boys_icc6<- ICC(boys_r6,lmer=FALSE)$results$ICC[6]
             boys_icc7<- ICC(boys_r7,lmer=FALSE)$results$ICC[6]
             boys_icc8<- ICC(boys_r8,lmer=FALSE)$results$ICC[6]
             boys_icc9<- ICC(boys_r9,lmer=FALSE)$results$ICC[6]
             boys_icc10<- ICC(boys_r10,lmer=FALSE)$results$ICC[6]
             boys_icc11<- ICC(boys_r11,lmer=FALSE)$results$ICC[6]
             
             # Relevant ICC's are ICC3 and ICC3k.
             icc_boys <- c(boys_icc1, boys_icc2, boys_icc3, boys_icc4, boys_icc5, boys_icc6, boys_icc7, boys_icc8, boys_icc9, boys_icc10, boys_icc11)
             
             icc3k <- c(icc_girls,icc_boys)
             round <- rep(1:11,2)
             gender <- c(rep("Female",11),rep("Male",11))
             
             df_icc3k <- data.frame(ICC = icc3k, Round = round, Gender=gender)
             
             # Graphics
             pdf(file="ICC3k.pdf")
             ggplot(df_icc3k, aes(x=as.factor(Round),y=ICC, group=Gender)) +  geom_line(aes(linetype=Gender, colour=Gender), size=2) + geom_point(aes(color=Gender)) + labs(title="ICC 3k for 5 Judges and 11 Dives",x="Round",y="ICC") + theme(legend.position="bottom") + scale_colour_manual(values=c(Male="#339999",Female="#663399"))
             dev.off()
             
             ######## End of Examples ##############