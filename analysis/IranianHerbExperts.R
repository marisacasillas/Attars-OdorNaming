rm(list=ls())
library(moments)
library(reshape)
source("0-utils.R")

# Set paths
data.path <- "inputData/"
proc.data.path <- "processedData/"
plot.path <- "plots/"

# Read in experiment and participant data
naming <- read.csv(paste(
	data.path,"NamingTaskData.csv", sep=""))

odaware <- read.csv(paste(
	data.path,"OdorAwarenessTaskData.csv", sep=""))

stim.info <- read.csv(paste(
	data.path,"StimulusInfo.csv", sep=""))

stim.dict <- read.csv(paste(
	data.path,"HerbDictionary.csv", sep=""))

demo.info <- read.csv(paste(
	data.path,"DemographicInfo-anon.csv", sep=""))

ptcp.info <- read.csv(paste(
	data.path,"ParticipantInfo.csv", sep=""))

# Merge participant info sources together
ptcp.info <- merge(ptcp.info, demo.info, by="ShortID")

############################################################
# ANALYSES
############################################################

##### Data preparation #####
# Merge participant and stimulus data into naming data
naming.all <- merge(naming, ptcp.info, by="ShortID")
naming.all <- merge(naming.all, stim.info, by="StimulusID")

# Collapse non-responses and incorrect responses
# Naming 1
naming.all$CorrectNaming1Collapsed <-
	naming.all$CorrectNaming1
nonresp.cn1 <- which(
	naming.all$CorrectNaming1Collapsed == 999)
naming.all$CorrectNaming1Collapsed[nonresp.cn1] <- 0
# Naming 2
naming.all$CorrectNaming2Collapsed <-
	naming.all$CorrectNaming2
nonresp.cn2 <- which(
	naming.all$CorrectNaming2Collapsed == 999)
naming.all$CorrectNaming2Collapsed[nonresp.cn2] <- 0

# Remove the "expert" who has only been trading
# spices for 1 year and who does it part-time (P09)
naming.all <- subset(naming.all, ShortID != "P09")

# Collapse the two illness columns
naming.all$Ill <- ifelse(naming.all$HasCold == "Yes" |
	naming.all$HasOtherIllness == "Yes","Yes","No")

# Center Age
naming.all$AgeC <- naming.all$Age - mean(naming.all$Age)

# Change the group names
naming.all$Group <- factor(naming.all$Group,
	labels=c("Laypeople", "Cooks", "Attars"))

# Create re-leveled group factors for pairwise
# comparisons in the statistical models
naming.all$GroupExp <- factor(naming.all$Group,
	levels=c("Attars", "Cooks", "Laypeople"))
naming.all$GroupCoo <- factor(naming.all$Group,
	levels=c("Cooks", "Laypeople", "Attars"))
naming.all$GroupCon <- factor(naming.all$Group,
	levels=c("Laypeople", "Cooks", "Attars"))
naming.all$StimType2 <- factor(naming.all$StimulusType,
	levels=c("Medicinal", "Culinary"))

# Add English odor names
naming.all <- merge(naming.all, stim.dict, by.x="StimulusID", by.y="Persian")
p.names <- which(names(naming.all) == "StimulusID")
e.names <- which(names(naming.all) == "English")
names(naming.all)[p.names] <- "PersianStimID"
names(naming.all)[e.names] <- "StimulusID"

# Prepare data frame for looking at response likelihood
naming.all.tr1 <- naming.all
naming.all.tr1$trial <- 1
naming.all.tr1$respgiven <- ifelse(
	naming.all.tr1$CorrectNaming1 == 999, 0, 1)
naming.all.tr2 <- naming.all
naming.all.tr2$trial <- 2
naming.all.tr2$respgiven <- ifelse(
	naming.all.tr2$CorrectNaming2 == 999, 0, 1)
naming.all.2trs <- rbind(naming.all.tr1, naming.all.tr2)

# Create figures for the descriptive analysis
# of the ratings (see sourced script below)
source("0-RatingsFigs.R")

# Create figures for the descriptive analysis
# of correctness (see sourced script below)
source("0-CorrectnessFigs.R")

# Create figures for the descriptive analysis
# of consistency (see sourced script below)
source("0-ConsistencyFigs.R")

# Figures for primary findings
source("0-FindingsFigs.R")

##### Models for correctness #############################
naming.all$GroupAtt <- relevel(naming.all$Group, ref = "Attars")

# Frequency as a substitute for experience
corr.0 <- glmer(CorrectNaming1Collapsed ~
	GroupAtt +
	(1|ShortID) + (1 + GroupAtt|StimulusID), data=naming.all,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))

# Frequency as a substitute for experience
corr.fre00 <- glmer(CorrectNaming1Collapsed ~
	GroupAtt + Frequency +
	(1|ShortID) + (1 + GroupAtt|StimulusID), data=naming.all,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))

# Familiarity as a substitute for experience
corr.fam00 <- glmer(CorrectNaming1Collapsed ~
	GroupAtt + Familiarity +
	(1|ShortID) + (1 + GroupAtt|StimulusID), data=naming.all,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))

anova(corr.0, corr.fre00)
anova(corr.0, corr.fam00)

# Do they work together?
corr.famfre00 <- glmer(CorrectNaming1Collapsed ~
	GroupAtt + Familiarity + Frequency +
	(1|ShortID) + (1 + GroupAtt|StimulusID), data=naming.all,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))

anova(corr.fam00, corr.famfre00)

# Check for a smoking effect
corr.fam1.1 <- glmer(CorrectNaming1Collapsed ~
	GroupAtt + Familiarity + 	Frequency +
	Smoker +
	(1|ShortID) + (1 + GroupAtt|StimulusID), data=naming.all,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))
anova(corr.famfre00, corr.fam1.1)
# Smoker status doesn't add anything

# Note: We chose to ignore illness since it wasn't
# systematically enough defined during data collection

# Check for an age effect
corr.fam1.2 <- glmer(CorrectNaming1Collapsed ~
	GroupAtt + Familiarity + 	Frequency +
	AgeC +
	(1|ShortID) + (1 + GroupAtt|StimulusID), data=naming.all,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))
anova(corr.famfre00, corr.fam1.2)
# Age doesn't add anything

# Note: didn't use an age * group effect because, though
# it shows initial signs of an effect, there is a large
# gap on the upper end of the range for experts

# Check for intensity
corr.fam1.3 <- glmer(CorrectNaming1Collapsed ~
	GroupAtt + Familiarity + 	Frequency +
	Intensity +
	(1|ShortID) + (1 + GroupAtt|StimulusID), data=naming.all,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))
anova(corr.famfre00, corr.fam1.3)
# Intensity only adds marginal improvement (p = 0.07431)

# Best model: fam1
corr.con <- glmer(CorrectNaming1Collapsed ~
	GroupCon + Familiarity + Frequency +
	(1|ShortID) + (1 + GroupCon|StimulusID), data=naming.all,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))

corr.coo <- glmer(CorrectNaming1Collapsed ~
	GroupCoo + Familiarity + Frequency +
	(1|ShortID) + (1 + GroupCoo|StimulusID), data=naming.all,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))

corr.exp <- glmer(CorrectNaming1Collapsed ~
	GroupExp + Familiarity + Frequency +
	(1|ShortID) + (1 + GroupExp|StimulusID), data=naming.all,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))

# Parallel model with odor type
corr.odt.con <- glmer(CorrectNaming1Collapsed ~
	GroupCon + StimType2 +
	(1|ShortID) + (1 + GroupCon|StimulusID), data=naming.all,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))

corr.odt.coo <- glmer(CorrectNaming1Collapsed ~
	GroupCoo + StimType2 +
	(1|ShortID) + (1 + GroupCoo|StimulusID), data=naming.all,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))

# reported as matched model in supp mat
corr.odt.exp <- glmer(CorrectNaming1Collapsed ~
	GroupExp + StimType2 +
	(1|ShortID) + (1 + GroupExp|StimulusID), data=naming.all,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))

##### Control model: Correctness #######################
corr.control.naming <- subset(naming.all,
	Group == "Laypeople")

# version with ratings (reported in supp mat)
corr.cm0 <- glmer(CorrectNaming1Collapsed ~
	Gender * Familiarity + Gender * Frequency +
	(1|ShortID) + (1|StimulusID),
	data=corr.control.naming,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))
# No effect of gender

# version with stim type (reported in supp mat)
corr.cm1 <- glmer(CorrectNaming1Collapsed ~
	Gender * StimType2 +
	(1|ShortID) + (1|StimulusID),
	data=corr.control.naming,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))
# No effect of gender

##### Models for response likelihood #############################
naming.all.2trs$GroupAtt <- relevel(
  naming.all.2trs$Group, ref = "Attars")

gvrsp.0 <- glmer(respgiven ~
	GroupAtt + trial +
	(1|ShortID) + (1 + GroupAtt|StimulusID), data=naming.all.2trs,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))

gvrsp.fam0 <- glmer(respgiven ~
	GroupAtt + Familiarity + trial +
	(1|ShortID) + (1 + GroupAtt|StimulusID), data=naming.all.2trs,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))
anova(gvrsp.0, gvrsp.fam0)

gvrsp.fre0 <- glmer(respgiven ~
	GroupAtt + Frequency + trial +
	(1|ShortID) + (1 + GroupAtt|StimulusID), data=naming.all.2trs,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))
anova(gvrsp.0, gvrsp.fre0)

gvrsp.famfre0 <- glmer(respgiven ~
	GroupAtt + Familiarity + Frequency + trial +
	(1|ShortID) + (1 + GroupAtt|StimulusID), data=naming.all.2trs,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))
anova(gvrsp.fam0, gvrsp.famfre0)

gvrsp.famfre.int <- glmer(respgiven ~
	GroupAtt * Familiarity + Familiarity + trial +
	(1|ShortID) + (1 + GroupAtt|StimulusID), data=naming.all.2trs,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))
anova(gvrsp.famfre0, gvrsp.famfre.int)
# not improved

gvrsp.famfre.int2 <- glmer(respgiven ~
	GroupAtt + Familiarity + GroupAtt * Familiarity + trial +
	(1|ShortID) + (1 + GroupAtt|StimulusID), data=naming.all.2trs,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))
anova(gvrsp.famfre0, gvrsp.famfre.int2)
# not improved

# age?
gvrsp.fam.fre0.age <- glmer(respgiven ~
	GroupAtt + Familiarity + Frequency + trial + AgeC +
	(1|ShortID) + (1 + GroupAtt|StimulusID), data=naming.all.2trs,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))
anova(gvrsp.famfre0, gvrsp.fam.fre0.age) # not improved

# intensity?
gvrsp.fam.fre0.int <- glmer(respgiven ~
	GroupAtt + Familiarity + Frequency + trial + Intensity +
	(1|ShortID) + (1 + GroupAtt|StimulusID), data=naming.all.2trs,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))
anova(gvrsp.famfre0, gvrsp.fam.fre0.int) # not improved

# Best model: gvrsp.fam.fre0
# this one reported in the main text
gvrsp.fam.fre.con <- glmer(respgiven ~
	GroupCon + Familiarity + Frequency + trial +
	(1|ShortID) + (1 + GroupCon|StimulusID), data=naming.all.2trs,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))

gvrsp.fam.fre.coo <- glmer(respgiven ~
	GroupCoo + Familiarity + Frequency + trial +
	(1|ShortID) + (1 + GroupCoo|StimulusID), data=naming.all.2trs,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))

gvrsp.fam.fre.exp <- glmer(respgiven ~
	GroupExp + Familiarity + Frequency + trial +
	(1|ShortID) + (1 + GroupExp|StimulusID), data=naming.all.2trs,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))

# version with stimulus type
gvrsp.stimtype.con <- glmer(respgiven ~
	GroupCon + StimType2 + trial +
	(1|ShortID) + (1 + GroupCon|StimulusID), data=naming.all.2trs,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))

gvrsp.stimtype.coo <- glmer(respgiven ~
	GroupCoo + StimType2 + trial +
	(1|ShortID) + (1 + GroupCoo|StimulusID), data=naming.all.2trs,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))

gvrsp.stimtype.exp <- glmer(respgiven ~
	GroupExp + StimType2 + trial +
	(1|ShortID) + (1 + GroupExp|StimulusID), data=naming.all.2trs,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))


##### Control model: Response Likelihood #######################
gvrsp.control.naming <- subset(naming.all.2trs,
	Group == "Laypeople")

# version with ratings (reported in supp mat)
gvrsp.fam.fre.con.con <- glmer(respgiven ~
	Gender + Familiarity + Frequency + trial +
	(1|ShortID) + (1|StimulusID), data=gvrsp.control.naming,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))
# No effect of gender

# version with stim type (reported in supp mat)
gvrsp.stimtype.con.con <- glmer(respgiven ~
	Gender + StimType2 + trial +
	(1|ShortID) + (1|StimulusID), data= gvrsp.control.naming,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))
# No effect of gender


##### Models for consistency #############################
# Take the subset of datapoints that can be used
# for a consistency analysis
# Make subset of answers with consistency scores
naming.tworespons <- subset(naming.all,
	Consistency != "N/A")

# Ensure consistency ratings are numeric
naming.tworespons$Consistency <-
	ifelse(naming.tworespons$Consistency == "1",
	1, 0)

naming.tworespons$GroupAtt <- relevel(
  naming.tworespons$Group, ref = "Attars")

con.0 <- glmer(Consistency ~
	GroupAtt +
	(1|ShortID) + (1 + GroupAtt|StimulusID),
	data=naming.tworespons,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))

con.fre00 <- glmer(Consistency ~
	GroupAtt + Frequency +
	(1|ShortID) + (1 + GroupAtt|StimulusID),
	data=naming.tworespons,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))
anova(con.0, con.fre00)
# only marginally improved (p = 0.06541)

con.fam00 <- glmer(Consistency ~
	GroupAtt + Familiarity +
	(1|ShortID) + (1 + GroupAtt|StimulusID),
	data=naming.tworespons,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))
anova(con.0, con.fam00)
# improved

con.fam0 <- glmer(Consistency ~
	GroupAtt * Familiarity +
	(1|ShortID) + (1 + GroupAtt|StimulusID),
	data=naming.tworespons,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))
anova(con.fam00, con.fam0)
# no improvement

con.famfre0 <- glmer(Consistency ~
	GroupAtt + Familiarity + Frequency +
	(1|ShortID) + (1 + GroupAtt|StimulusID),
	data=naming.tworespons,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))
anova(con.fam00, con.famfre0)
# no improvement


# Check for a smoking effect
con.fam0.1 <- glmer(Consistency ~
	GroupAtt + Familiarity +
	Smoker +
	(1|ShortID) + (1 + GroupAtt|StimulusID),
	data=naming.tworespons,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))
anova(con.fam00, con.fam0.1)
# Smoker status doesn't add anything

# Note: We chose to ignore illness since it wasn't
# systematically enough defined during data collection

# Check for an age effect
con.fam0.2 <- glmer(Consistency ~
	GroupAtt + Familiarity +
	AgeC +
	(1|ShortID) + (1 + GroupAtt|StimulusID),
	data=naming.tworespons,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))
anova(con.fam00, con.fam0.2)
# Age doesn't add anything

# Note: didn't use an age * group effect because, though
# it shows initial signs of an effect, there is a large
# gap on the upper end of the range for experts

# Check for intensity
con.fam0.3 <- glmer(Consistency ~
	GroupAtt + Familiarity +
	Intensity +
	(1|ShortID) + (1 + GroupAtt|StimulusID),
	data=naming.tworespons,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))
anova(con.fam00, con.fam0.3)
# Intensity doesn't add anything

# Best model: fam0
# reported in the main paper
con.con <- glmer(Consistency ~
	GroupCon + Familiarity +
	(1|ShortID) + (1 + GroupCon|StimulusID),
	data=naming.tworespons,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))
	
con.coo <- glmer(Consistency ~
	GroupCoo + Familiarity +
	(1|ShortID) + (1 + GroupCoo|StimulusID),
	data=naming.tworespons,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))

con.exp <- glmer(Consistency ~
	GroupExp + Familiarity +
	(1|ShortID) + (1 + GroupExp|StimulusID),
	data=naming.tworespons,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))

# matched model with stim type
con.odt.con <- glmer(Consistency ~
	GroupCon + StimType2 +
	(1|ShortID) + (1 + GroupCon|StimulusID),
	data=naming.tworespons,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))

con.odt.coo <- glmer(Consistency ~
	GroupCoo + StimType2 +
	(1|ShortID) + (1 + GroupCoo|StimulusID),
	data=naming.tworespons,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))
# doesn't converge

con.odt.exp <- glmer(Consistency ~
	GroupExp + StimType2 +
	(1|ShortID) + (1 + GroupExp|StimulusID),
	data=naming.tworespons,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))

##### Control model: Consistency #######################
control.naming.con <- subset(naming.tworespons,
	Group == "Laypeople")

# version with ratings (reported in supp mat)
con.cm0 <- glmer(Consistency ~
	Gender * Familiarity + Gender * Frequency +
	(1|ShortID) + (1|StimulusID),
	data=control.naming.con,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))
# No effect of gender

# version with stim type (reported in supp mat)
con.cm1 <- glmer(Consistency ~
	Gender * StimType2 +
	(1|ShortID) + (1|StimulusID),
	data=control.naming.con,
	family=binomial,
	control=glmerControl(optimizer="bobyqa",
	optCtrl=list(maxfun=50000)))
# No effect of gender

##### Models for ratings #############################
# Familiarity
m.fam.max <- lmer(Familiarity^2 ~
	GroupExp * StimType2 + AgeC +
	(1|ShortID) + (1 + GroupExp|StimulusID),
	naming.all)
# summary(m.fam.max)
# m.fam.max.resid <- residuals(m.fam.max)
# qqnorm(m.fam.max.resid)
# qqline(m.fam.max.resid)
# fam.resids <- data.frame(resid = m.fam.max.resid)
# ggplot(fam.resids, aes(x=resid)) +
	# geom_histogram(aes(y=..density..),
		# fill="white", color="black", binwidth = 10) +
	# geom_density(color="firebrick3", lwd=2)
m.fam.max.coo <- lmer(Familiarity^2 ~
	GroupCoo * StimType2 + AgeC +
	(1|ShortID) + (1 + GroupCoo|StimulusID),
	naming.all)
	
# Frequency
m.fre.max <- lmer(Frequency^2 ~
	GroupExp * StimType2 + AgeC +
	(1|ShortID) + (1 + GroupExp|StimulusID),
	naming.all)
# summary(m.fre.max)
# slope for stimulus
# m.fre.max.resid <- residuals(m.fre.max)
# qqnorm(m.fre.max.resid)
# qqline(m.fre.max.resid)
# fre.resids <- data.frame(resid = m.fre.max.resid)
# ggplot(fre.resids, aes(x=resid)) +
	# geom_histogram(aes(y=..density..),
		# fill="white", color="black", binwidth = 10) +
	# geom_density(color="firebrick3", lwd=2)
m.fre.max.coo <- lmer(Frequency^2 ~
	GroupCoo * StimType2 + AgeC +
	(1|ShortID) + (1 + GroupCoo|StimulusID),
	naming.all)

# Edibility
m.edi.max <- lmer(Edibility^2 ~
	GroupExp * StimType2 + AgeC +
	(1|ShortID) + (1|StimulusID),
	naming.all)
# summary(m.edi.max)
# won't converge with random group slope
# m.edi.max.resid <- residuals(m.edi.max)
# qqnorm(m.edi.max.resid)
# qqline(m.edi.max.resid)
# edi.resids <- data.frame(resid = m.edi.max.resid)
# ggplot(edi.resids, aes(x=resid)) +
	# geom_histogram(aes(y=..density..),
		# fill="white", color="black", binwidth = 10) +
	# geom_density(color="firebrick3", lwd=2)
m.edi.max.coo <- lmer(Edibility^2 ~
	GroupCoo * StimType2 + AgeC +
	(1|ShortID) + (1|StimulusID),
	naming.all)

# Medicinalness
m.med.max <- lmer(Medicinal^2 ~
	GroupExp * StimType2 + AgeC +
	(1|ShortID) + (1 + GroupExp|StimulusID),
	naming.all)
# summary(m.med.max)
# m.med.max.resid <- residuals(m.med.max)
# qqnorm(m.med.max.resid)
# qqline(m.med.max.resid)
# med.resids <- data.frame(resid = m.med.max.resid)
# ggplot(med.resids, aes(x=resid)) +
	# geom_histogram(aes(y=..density..),
		# fill="white", color="black", binwidth = 10) +
	# geom_density(color="firebrick3", lwd=2)
m.med.max.coo <- lmer(Medicinal^2 ~
	GroupCoo * StimType2 + AgeC +
	(1|ShortID) + (1 + GroupCoo|StimulusID),
	naming.all)
# convergence warning

# Intensity
m.int.max <- lmer(Intensity^2 ~
	GroupExp * StimType2 + AgeC +
	(1|ShortID) + (1 + GroupExp|StimulusID),
	naming.all)
# summary(m.int.max)
# m.int.max.resid <- residuals(m.int.max)
# qqnorm(m.int.max.resid)
# qqline(m.int.max.resid)
# int.resids <- data.frame(resid = m.int.max.resid)
# ggplot(int.resids, aes(x=resid)) +
	# geom_histogram(aes(y=..density..),
		# fill="white", color="black", binwidth = 10) +
	# geom_density(color="firebrick3", lwd=2)
m.int.max.coo <- lmer(Intensity^2 ~
	GroupCoo * StimType2 + AgeC +
	(1|ShortID) + (1 + GroupCoo|StimulusID),
	naming.all)

# Pleasantness
m.ple.max <- lmer(Pleasantness^2 ~
	GroupExp * StimType2 + AgeC +
	(1|ShortID) + (1 + GroupExp|StimulusID),
	naming.all)
# summary(m.ple.max)
# m.ple.max.resid <- residuals(m.ple.max)
# qqnorm(m.ple.max.resid)
# qqline(m.ple.max.resid)
# ple.resids <- data.frame(resid = m.ple.max.resid)
# ggplot(ple.resids, aes(x=resid)) +
	# geom_histogram(aes(y=..density..),
		# fill="white", color="black", binwidth = 10) +
	# geom_density(color="firebrick3", lwd=2)
m.ple.max.coo <- lmer(Pleasantness^2 ~
	GroupCoo * StimType2 + AgeC +
	(1|ShortID) + (1 + GroupCoo|StimulusID),
	naming.all)


#source("0-RatingsExtraModels.R")

##### Models for odor awareness ########################
# Read in the odor awareness results from scratch
odaware <- read.csv(paste(
	data.path,"OdorAwarenessTaskData.csv", sep=""))
# Add in participant info
odaware <- merge(odaware, ptcp.info[,1:4], by="ShortID")
# Exclude participant 9
odaware <- subset(odaware, ShortID != "P09")
# Center Age
odaware$AgeC <- odaware$Age - mean(odaware$Age)
# Melt odaware into a modelable df
oa.melt <- melt(odaware, id=c("ShortID", "Group",
	"Gender", "AgeC", "Age"))
# Model
m.oa <- lmer(value ~
	Group + AgeC +
	(1|ShortID) + (1+Group|variable), oa.melt)
# summary(m.oa)
# m.oa.resid <- residuals(m.oa)
# qqnorm(m.oa.resid)
# qqline(m.oa.resid)
# oa.resids <- data.frame(resid = m.oa.resid)
# ggplot(oa.resids, aes(x=resid)) +
	# geom_histogram(aes(y=..density..),
		# fill="white", color="black", binwidth = 10) +
	# geom_density(color="firebrick3", lwd=2)
oa.melt$Group2 <- factor(oa.melt$Group,
	levels=c("Expert", "Cook", "Control"))
m.oa2 <- lmer(value ~
	Group2 + AgeC +
	(1|ShortID) + (1+Group2|variable), oa.melt)


# Control model (max interactions, though
# none contribute significantly)
oa.melt.con <- subset(oa.melt, Group == "Control")
m.oa.con <- lmer(value ~
	Gender * AgeC +
	(1|ShortID) + (1|variable), oa.melt.con)
# summary(m.oa.con)
# m.oa.con.resid <- residuals(m.oa.con)
# qqnorm(m.oa.resid.con)
# qqline(m.oa.resid.con)
# oa.con.resids <- data.frame(resid = m.oa.resid.con)
# ggplot(oa.con.resids, aes(x=resid)) +
	# geom_histogram(aes(y=..density..),
		# fill="white", color="black", binwidth = 10) +
	# geom_density(color="firebrick3", lwd=2)

