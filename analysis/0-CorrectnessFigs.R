plot.path <- "plots/correctness-descriptive/"

# By-item variance in correctness
item.avgs <- aggregate(CorrectNaming1Collapsed ~
	StimulusID + StimulusType,
	naming.all, mean)
item.ses <- aggregate(CorrectNaming1Collapsed ~
	StimulusID + StimulusType,
	naming.all, sem)$CorrectNaming1Collapsed
item.effects <- data.frame(
	Stimulus = item.avgs$StimulusID,
	OdorType = item.avgs$StimulusType,
	m = item.avgs$CorrectNaming1Collapsed,
	se = item.ses,
	stringsAsFactors = F)
orderedms <- as.character(item.effects[order(
	item.effects$m),]$Stimulus)
# save for use in later plots
correctness.order <- as.character(item.effects[order(
	item.effects$m),]$Stimulus)
item.effects$Stimulus <- factor(item.effects$Stimulus,
	levels=orderedms)
item.effects$OdorType <- factor(item.effects$OdorType,
	levels=c("Medicinal", "Culinary"))
limits <- aes(ymax = m + se, ymin= m - se)
p <- ggplot(item.effects, aes(x=Stimulus, y=m)) +
	facet_grid(. ~ OdorType, scales = "free",
	space = "free") +
	ylab("Mean accuracy") +
	xlab("Stimulus") +
	geom_pointrange(limits, size=1.5) +
	basic.theme
png(paste(plot.path,
	"COR-item-overall-averages.png", sep=""),
    width=1000,height=600,units="px",
    bg = "transparent")
print(p)
dev.off() 

# By-item variance between groups in correctness
item.group.avgs <- aggregate(
	CorrectNaming1Collapsed ~
	StimulusID + StimulusType + Group,
	naming.all, mean)
item.group.ses <- aggregate(
	CorrectNaming1Collapsed ~
	StimulusID + StimulusType + Group,
	naming.all, sem)$CorrectNaming1Collapsed
item.group.effects <- data.frame(
	Stimulus = item.group.avgs$StimulusID,
	OdorType = item.group.avgs$StimulusType,
	Group = item.group.avgs$Group,
	m = item.group.avgs$CorrectNaming1Collapsed,
	se = item.group.ses)
item.group.effects$Stimulus <- factor(
	item.group.effects$Stimulus, levels=orderedms)
item.group.effects$OdorType <- factor(
	item.group.effects$OdorType,
	levels=c("Medicinal", "Culinary"))
item.group.effects$Group <- factor(
	item.group.effects$Group,
	levels=c("Attars", "Cooks", "Laypeople"))
limits <- aes(ymax = m + se, ymin= m - se)
p <- ggplot(item.group.effects,
	aes(x=Stimulus, y=m, color=Group)) +
	facet_grid(. ~ OdorType, scales = "free",
	space = "free") +
	geom_pointrange(limits, size=1.5) +
	scale_color_manual(name="Group",
		values=rev(c("darkseagreen1",
	"aquamarine3", "aquamarine4"))) +
	basic.theme
png(paste(plot.path,
	"COR-item-group-averages.png", sep=""),
    width=1000,height=600,units="px",
    bg = "transparent")
print(p)
dev.off()

# By-item variance between gender in correctness
# Control group only (others are ~categorical gender)
naming.all.controls <- subset(naming.all,
	Group == "Laypeople")
item.gend.avgs <- aggregate(CorrectNaming1Collapsed ~
	StimulusID + StimulusType + Gender,
	naming.all.controls, mean)
item.gend.ses <- aggregate(CorrectNaming1Collapsed ~
	StimulusID + StimulusType + Gender,
	naming.all.controls, sem)$CorrectNaming1Collapsed
item.gend.effects <- data.frame(
	Stimulus = item.gend.avgs$StimulusID,
	OdorType = item.gend.avgs$StimulusType,
	Gender = item.gend.avgs$Gender,
	m = item.gend.avgs$CorrectNaming1Collapsed,
	se = item.gend.ses)
item.gend.effects$Stimulus <- factor(
	item.gend.effects$Stimulus, levels=orderedms)
item.gend.effects$OdorType <- factor(
	item.gend.effects$OdorType,
	levels=c("Medicinal", "Culinary"))
limits <- aes(ymax = m + se, ymin= m - se)
p <- ggplot(item.gend.effects,
	aes(x=Stimulus, y=m, color=Gender)) +
	facet_grid(. ~ OdorType, scales = "free",
	space = "free") +
	geom_pointrange(limits, size=1.5) +
	basic.theme
png(paste(plot.path,
	"COR-item-gender-averages.png", sep=""),
    width=1000,height=600,units="px",
    bg = "transparent")
print(p)
dev.off()

# By-item variance between smokers/nonsmokers
# in correctness
# No cooks are smokers; Controls and attars only
naming.all.smks <- subset(naming.all, Group == "Laypeople" |
	Group == "Attars")
item.smk.avgs <- aggregate(CorrectNaming1Collapsed ~
	StimulusID + StimulusType + Smoker + Group,
	naming.all.smks, mean)
item.smk.ses <- aggregate(CorrectNaming1Collapsed ~
	StimulusID + StimulusType + Smoker + Group,
	naming.all.smks, sem)$CorrectNaming1Collapsed
item.smk.effects <- data.frame(
	Stimulus = item.smk.avgs$StimulusID,
	OdorType = item.smk.avgs$StimulusType,
	Group = item.smk.avgs$Group,
	Smoker = item.smk.avgs$Smoker,
	m = item.smk.avgs$CorrectNaming1Collapsed,
	se = item.smk.ses)
item.smk.effects$Stimulus <- factor(
	item.smk.effects $Stimulus, levels=orderedms)
item.smk.effects$OdorType <- factor(
	item.smk.effects$OdorType,
	levels=c("Medicinal", "Culinary"))
limits <- aes(ymax = m + se, ymin= m - se)
p <- ggplot(item.smk.effects,
	aes(x=Stimulus, y=m, color=Smoker)) +
	facet_grid(Group ~ OdorType, scales = "free",
		space = "free") +
	geom_pointrange(limits, size=1.5) +
	basic.theme
png(paste(plot.path,
	"COR-item-smoker-averages.png", sep=""),
    width=1000,height=600,units="px",
    bg = "transparent")
print(p)
dev.off()

# By-item variance between ill/not-ill people
# between groups in correctness
# Attars only (cooks and controls are almost
# completely not ill)
naming.all.exp <- subset(naming.all, Group == "Attars")
item.ill.avgs <- aggregate(CorrectNaming1Collapsed ~
	StimulusID + StimulusType + Ill,
	naming.all.exp, mean)
item.ill.ses <- aggregate(CorrectNaming1Collapsed ~
	StimulusID + StimulusType + Ill,
	naming.all.exp, sem)$CorrectNaming1Collapsed
item.ill.effects <- data.frame(
	Stimulus = item.ill.avgs$StimulusID,
	OdorType = item.ill.avgs$StimulusType,
	Sick = item.ill.avgs$Ill,
	m = item.ill.avgs$CorrectNaming1Collapsed,
	se = item.ill.ses)
item.ill.effects$Stimulus <- factor(
	item.ill.effects $Stimulus, levels=orderedms)
item.ill.effects$OdorType <- factor(
	item.ill.effects$OdorType,
	levels=c("Medicinal", "Culinary"))
limits <- aes(ymax = m + se, ymin= m - se)
p <- ggplot(item.ill.effects,
	aes(x=Stimulus, y=m, color=Sick)) +
	facet_grid(. ~ OdorType, scales = "free",
		space = "free") +
	geom_pointrange(limits, size=1.5) +
	basic.theme
png(paste(plot.path,
	"COR-item-illness-averages.png", sep=""),
    width=1000,height=600,units="px",
    bg = "transparent")
print(p)
dev.off()

# By-item variance among ill people in correctness
ill.people <- subset(naming.all, Ill == "Yes")
ill.people.avgs <- aggregate(CorrectNaming1Collapsed ~
	StimulusID + StimulusType + HasOtherIllness,
	ill.people, mean)
ill.people.ses <- aggregate(CorrectNaming1Collapsed ~
	StimulusID + StimulusType + HasOtherIllness,
	ill.people, sem)$CorrectNaming1Collapsed
ill.people.effects <- data.frame(
	Stimulus = ill.people.avgs$StimulusID,
	OdorType = ill.people.avgs$StimulusType,
	NonCold = ill.people.avgs$HasOtherIllness,
	m = ill.people.avgs$CorrectNaming1Collapsed,
	se = ill.people.ses)
ill.people.effects$OdorType <- factor(
	ill.people.effects$OdorType,
	levels=c("Medicinal", "Culinary"))
limits <- aes(ymax = m + se, ymin= m - se)
p <- ggplot(ill.people.effects,
	aes(x=Stimulus, y=m, color=NonCold)) +
	facet_grid(. ~ OdorType, scales = "free",
		space = "free") +
	geom_pointrange(limits, size=1.5) +
	basic.theme
png(paste(plot.path,
	"COR-item-illnesstypes-averages.png", sep=""),
    width=1000,height=600,units="px",
    bg = "transparent")
print(p)
dev.off()


# By-itemtype variance across age and groups
# in correctness
item.age.avgs <- aggregate(CorrectNaming1Collapsed ~
	StimulusType + Age + Group,
	naming.all, mean)
item.age.ses <- aggregate(CorrectNaming1Collapsed ~
	StimulusType + Age + Group,
	naming.all, sem)$CorrectNaming1Collapsed
item.age.effects <- data.frame(
	OdorType = item.age.avgs$StimulusType,
	Age = item.age.avgs$Age,
	Group = item.age.avgs$Group,
	m = item.age.avgs$CorrectNaming1Collapsed,
	se = item.age.ses)
item.age.effects$OdorType <- factor(
	item.age.effects$OdorType,
	levels=c("Medicinal", "Culinary"))
limits <- aes(ymax = m + se, ymin= m - se)
p <- ggplot(item.age.effects,
	aes(x=Age, y=m, color=OdorType)) +
	facet_grid(. ~ Group) +
	geom_pointrange(limits, size=1.5) +
	geom_line(size=1.5) +
	basic.theme
png(paste(plot.path,
	"COR-itemtype-age-averages.png", sep=""),
    width=1600,height=600,units="px",
    bg = "transparent")
print(p)
dev.off()

# By-itemtype variance across education in correctness
edu.avgs <- aggregate(CorrectNaming1Collapsed ~
	ShortID + StimulusType + Education + Group,
	naming.all, mean)
edu.ses <- aggregate(CorrectNaming1Collapsed ~
	ShortID + StimulusType + Education + Group,
	naming.all, sem)$CorrectNaming1Collapsed
edu.effects <- data.frame(
	Participant = edu.avgs$ShortID,
	OdorType = edu.avgs$StimulusType,
	Education = edu.avgs$Education,
	Group = edu.avgs$Group,
	m = edu.avgs$CorrectNaming1Collapsed,
	se = edu.ses)
edu.effects$OdorType <- factor(
	edu.effects$OdorType,
	levels=c("Medicinal", "Culinary"))
# edu.effects$Education <- factor(edu.effects$Education,
# 	levels=c(
# 	"School Diploma", "High School Diploma", "BA",
# 	"Associate Degree", "MA", "PhD"))
# edu.effects$Education <- factor(edu.effects$Education,
# 	labels=c(
# 	"School", "High School", "BA",
# 	"Assoc. Degree", "MA", "PhD"))
p <- ggplot(edu.effects,
	aes(x=Group, y=m, fill=OdorType)) +
	facet_grid(. ~ Education) +
	facet_wrap(~ Education, ncol = 3) +
	geom_violin(position=position_dodge(1), trim=F) +
	geom_dotplot(position=position_dodge(1),
		binaxis="y", binwidth=0.03,
		stackdir="center", size=1) +
	geom_boxplot(position=position_dodge(1), width=0.1) +
	basic.theme
png(paste(plot.path,
	"COR-itemtype-education-averages.png", sep=""),
    width=1000,height=800,units="px",
    bg = "transparent")
print(p)
dev.off()

plot.path <- "plots/"