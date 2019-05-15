plot.path <- "plots/consistency-descriptive/"

# Plot the available consistency cases
naming.all$TwoResp <- ifelse(
	naming.all$Consistency == "N/A", 0, 1)
consist.cases <- aggregate(TwoResp ~
	StimulusID + Group + StimulusType, naming.all, mean)
consist.cases$Group <- factor(consist.cases$Group,
	levels=c("Attars", "Cooks", "Laypeople"))
consist.cases$StimulusID <- factor(
	consist.cases$StimulusID,
	levels=correctness.order)
consist.cases$StimulusType <- factor(
	consist.cases$StimulusType,
	levels=c("Medicinal", "Culinary"))
names(consist.cases) <- c(
	"Herb", "Group", "HerbType", "LabeledTwice")
consist.cases.med <- subset(consist.cases,
	HerbType == "Medicinal")
consist.cases.cul <- subset(consist.cases,
	HerbType == "Culinary")
p.med <- ggplot(consist.cases.med,
	aes(x=factor(Herb), y=LabeledTwice)) +
	ylab("% labeled twice") + xlab("Herb") +
	ylim(0,1) +
	facet_grid(. ~ Group, scales = "free",
	space = "free") +
	geom_bar(stat = "identity") +
	basic.theme
p.cul <- ggplot(consist.cases.cul,
	aes(x=factor(Herb), y=LabeledTwice)) +
	ylab("% labeled twice") + xlab("Herb") +
	ylim(0,1) +
	facet_grid(. ~ Group, scales = "free",
	space = "free") +
	geom_bar(stat = "identity") +
	basic.theme
png(paste(plot.path,
	"CON-labeled-twice_item-by-group.png", sep=""),
    width=1000,height=1000,units="px",
    bg = "transparent")
print(multiplot(p.med, p.cul))
dev.off()

# Make subset of answers with consistency scores
naming.tworespons <- subset(naming.all,
	Consistency != "N/A")

# Ensure consistency ratings are numeric
naming.tworespons$Consistency <-
	ifelse(naming.tworespons$Consistency == "1",
	1, 0)

# Order items by number of consistency scores
item.score.freq <- data.frame(
	table(naming.tworespons$StimulusID))
names(item.score.freq) <- c("StimulusID", "NumScores")
item.score.freq <- item.score.freq[order(
	item.score.freq$NumScores),]
score.order <- item.score.freq$StimulusID

# Plot the distribution of consistency scores
# across groups by item and by odor type with
# number of consistency scores indexed by order
item.group.avgs <- aggregate(Consistency ~
	StimulusID + StimulusType + Group,
	naming.tworespons, mean)
item.group.ses <- aggregate(Consistency ~
	StimulusID + StimulusType + Group,
	naming.tworespons, sem)$Consistency
item.group.effects <- data.frame(
	Stimulus = item.group.avgs$StimulusID,
	OdorType = item.group.avgs$StimulusType,
	Group = item.group.avgs$Group,
	m = item.group.avgs$Consistency,
	se = item.group.ses,
	stringsAsFactors = F)
orderedms <- as.character(score.order)
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
	basic.theme
png(paste(plot.path,
	"CON-item-group-averages.png", sep=""),
    width=1000,height=600,units="px",
    bg = "transparent")
print(p)
dev.off()

# Now across groups overall and by odor type with
# number of consistency scores indexed by order
item.group.avgs <- aggregate(Consistency ~
	StimulusType + Group,
	naming.tworespons, mean)
item.group.ses <- aggregate(Consistency ~
	StimulusType + Group,
	naming.tworespons, sem)$Consistency
item.group.effects <- data.frame(
	OdorType = item.group.avgs$StimulusType,
	Group = item.group.avgs$Group,
	m = item.group.avgs$Consistency,
	se = item.group.ses,
	stringsAsFactors = F)
orderedms <- as.character(score.order)
item.group.effects$OdorType <- factor(
	item.group.effects$OdorType,
	levels=c("Medicinal", "Culinary"))
item.group.effects$Group <- factor(
	item.group.effects$Group,
	levels=c("Attars", "Cooks", "Laypeople"))
limits <- aes(ymax = m + se, ymin= m - se)
p <- ggplot(item.group.effects,
	aes(OdorType, y=m, color=Group)) +
	geom_pointrange(limits, size=1.5) +
	basic.theme
png(paste(plot.path,
	"CON-group-averages.png", sep=""),
    width=1000,height=600,units="px",
    bg = "transparent")
print(p)
dev.off()

plot.path <- "plots/"