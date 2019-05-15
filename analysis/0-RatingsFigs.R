plot.path <- "plots/ratings-descriptive/"

# By-item variance in intensity by group
inten.avgs <- aggregate(Intensity ~
	StimulusID + Group + StimulusType,
	naming.all, mean)
inten.ses <- aggregate(Intensity ~
	StimulusID + Group + StimulusType,
	naming.all, sem)$Intensity
inten.effects <- data.frame(
	Group = inten.avgs$Group,
	Odor = inten.avgs$StimulusID,
	OdorType = inten.avgs$StimulusType,
	m = inten.avgs$Intensity,
	se = inten.ses,
	stringsAsFactors = F)
# Order items by overall intensity (most to least)
inten.avgs.overall <- aggregate(Intensity ~
	StimulusID, naming.all, mean)
orderedms.intens <- as.character(inten.avgs.overall
	[order(inten.avgs.overall$Intensity),]$StimulusID)
inten.effects$Odor <- factor(inten.effects$Odor,
	levels=orderedms.intens)
inten.effects$OdorType <- factor(inten.effects$OdorType,
	levels=c("Medicinal", "Culinary"))
limits <- aes(ymax = m + se, ymin= m - se)
p <- ggplot(inten.effects,
	aes(x=Odor, y=m, color=Group)) +
	facet_grid(. ~ OdorType, scales = "free",
	space = "free") +
	geom_pointrange(limits, size=1.5) +
	basic.theme
png(paste(plot.path,
	"RAT-intensity-item-overall-averages.png", sep=""),
    width=1000,height=600,units="px",
    bg = "transparent")
print(p)
dev.off() 

# By-item variance in familiarity by group
famil.avgs <- aggregate(Familiarity ~
	StimulusID + Group + StimulusType,
	naming.all, mean)
famil.ses <- aggregate(Familiarity ~
	StimulusID + Group + StimulusType,
	naming.all, sem)$Familiarity
famil.effects <- data.frame(
	Group = famil.avgs$Group,
	Odor = famil.avgs$StimulusID,
	OdorType = famil.avgs$StimulusType,
	m = famil.avgs$Familiarity,
	se = famil.ses,
	stringsAsFactors = F)
# Order items by overall familiarity (most to least)
famil.avgs.overall <- aggregate(Familiarity ~
	StimulusID, naming.all, mean)
orderedms.famil <- as.character(famil.avgs.overall
	[order(famil.avgs.overall$Familiarity),]$StimulusID)
famil.effects$Odor <- factor(famil.effects$Odor,
	levels=orderedms.famil)
famil.effects$OdorType <- factor(famil.effects$OdorType,
	levels=c("Medicinal", "Culinary"))
limits <- aes(ymax = m + se, ymin= m - se)
p <- ggplot(famil.effects,
	aes(x=Odor, y=m, color=Group)) +
	facet_grid(. ~ OdorType, scales = "free",
	space = "free") +
	geom_pointrange(limits, size=1.5) +
	basic.theme
png(paste(plot.path,
	"RAT-familiarity-item-overall-averages.png", sep=""),
    width=1000,height=600,units="px",
    bg = "transparent")
print(p)
dev.off()

# By-item variance in frequency by group
frequ.avgs <- aggregate(Frequency ~
	StimulusID + Group + StimulusType,
	naming.all, mean)
frequ.ses <- aggregate(Frequency ~
	StimulusID + Group + StimulusType,
	naming.all, sem)$Frequency
frequ.effects <- data.frame(
	Group = frequ.avgs$Group,
	Odor = frequ.avgs$StimulusID,
	OdorType = frequ.avgs$StimulusType,
	m = frequ.avgs$Frequency,
	se = frequ.ses,
	stringsAsFactors = F)
# Order items by overall frequency (most to least)
frequ.avgs.overall <- aggregate(Frequency ~
	StimulusID, naming.all, mean)
orderedms.frequ <- as.character(frequ.avgs.overall
	[order(frequ.avgs.overall$Frequency),]$StimulusID)
frequ.effects$Odor <- factor(frequ.effects$Odor,
	levels=orderedms.frequ)
frequ.effects$OdorType <- factor(frequ.effects$OdorType,
	levels=c("Medicinal", "Culinary"))
limits <- aes(ymax = m + se, ymin= m - se)
p <- ggplot(frequ.effects,
	aes(x=Odor, y=m, color=Group)) +
	facet_grid(. ~ OdorType, scales = "free",
	space = "free") +
	geom_pointrange(limits, size=1.5) +
	basic.theme
png(paste(plot.path,
	"RAT-frequency-item-overall-averages.png", sep=""),
    width=1000,height=600,units="px",
    bg = "transparent")
print(p)
dev.off()

# By-item variance in pleasantness by group
pleas.avgs <- aggregate(Pleasantness ~
	StimulusID + Group + StimulusType,
	naming.all, mean)
pleas.ses <- aggregate(Pleasantness ~
	StimulusID + Group + StimulusType,
	naming.all, sem)$Pleasantness
pleas.effects <- data.frame(
	Group = pleas.avgs$Group,
	Odor = pleas.avgs$StimulusID,
	OdorType = pleas.avgs$StimulusType,
	m = pleas.avgs$Pleasantness,
	se = pleas.ses,
	stringsAsFactors = F)
# Order items by overall pleasantness (most to least)
pleas.avgs.overall <- aggregate(Pleasantness ~
	StimulusID, naming.all, mean)
orderedms.pleas <- as.character(pleas.avgs.overall
	[order(pleas.avgs.overall$Pleasantness),]$StimulusID)
pleas.effects$Odor <- factor(pleas.effects$Odor,
	levels=orderedms.pleas)
pleas.effects$OdorType <- factor(pleas.effects$OdorType,
	levels=c("Medicinal", "Culinary"))
limits <- aes(ymax = m + se, ymin= m - se)
p <- ggplot(pleas.effects,
	aes(x=Odor, y=m, color=Group)) +
	facet_grid(. ~ OdorType, scales = "free",
	space = "free") +
	geom_pointrange(limits, size=1.5) +
	basic.theme
png(paste(plot.path,
	"RAT-pleasantness-item-overall-averages.png", sep=""),
    width=1000,height=600,units="px",
    bg = "transparent")
print(p)
dev.off()

# By-item variance in edibility by group
edibi.avgs <- aggregate(Edibility ~
	StimulusID + Group + StimulusType,
	naming.all, mean)
edibi.ses <- aggregate(Edibility ~
	StimulusID + Group + StimulusType,
	naming.all, sem)$Edibility
edibi.effects <- data.frame(
	Group = edibi.avgs$Group,
	Odor = edibi.avgs$StimulusID,
	OdorType = edibi.avgs$StimulusType,
	m = edibi.avgs$Edibility,
	se = edibi.ses,
	stringsAsFactors = F)
# Order items by overall edibility (most to least)
edibi.avgs.overall <- aggregate(Edibility ~
	StimulusID, naming.all, mean)
orderedms.edibi <- as.character(edibi.avgs.overall
	[order(edibi.avgs.overall$Edibility),]$StimulusID)
edibi.effects$Odor <- factor(edibi.effects$Odor,
	levels=orderedms.edibi)
edibi.effects$OdorType <- factor(edibi.effects$OdorType,
	levels=c("Medicinal", "Culinary"))
limits <- aes(ymax = m + se, ymin= m - se)
p <- ggplot(edibi.effects,
	aes(x=Odor, y=m, color=Group)) +
	facet_grid(. ~ OdorType, scales = "free",
	space = "free") +
	geom_pointrange(limits, size=1.5) +
	basic.theme
png(paste(plot.path,
	"RAT-edibility-item-overall-averages.png", sep=""),
    width=1000,height=600,units="px",
    bg = "transparent")
print(p)
dev.off()

# By-item variance in medicinal-ness by group
medic.avgs <- aggregate(Medicinal ~
	StimulusID + Group + StimulusType,
	naming.all, mean)
medic.ses <- aggregate(Medicinal ~
	StimulusID + Group + StimulusType,
	naming.all, sem)$Medicinal
medic.effects <- data.frame(
	Group = medic.avgs$Group,
	Odor = medic.avgs$StimulusID,
	OdorType = medic.avgs$StimulusType,
	m = medic.avgs$Medicinal,
	se = medic.ses,
	stringsAsFactors = F)
# Order items by overall medicinal (most to least)
medic.avgs.overall <- aggregate(Medicinal ~
	StimulusID, naming.all, mean)
orderedms.medic <- as.character(medic.avgs.overall
	[order(medic.avgs.overall$Medicinal),]$StimulusID)
medic.effects$Odor <- factor(medic.effects$Odor,
	levels=orderedms.medic)
medic.effects$OdorType <- factor(medic.effects$OdorType,
	levels=c("Medicinal", "Culinary"))
limits <- aes(ymax = m + se, ymin= m - se)
p <- ggplot(medic.effects,
	aes(x=Odor, y=m, color=Group)) +
	facet_grid(. ~ OdorType, scales = "free",
	space = "free") +
	geom_pointrange(limits, size=1.5) +
	basic.theme
png(paste(plot.path,
	"RAT-medicinal-item-overall-averages.png", sep=""),
    width=1000,height=600,units="px",
    bg = "transparent")
print(p)
dev.off()

plot.path <- "plots/"