plot.path <- "plots/findings/"

# Cell-based ratings summary
groupcolors <- c("darkseagreen1",
	"aquamarine3", "aquamarine4")
groupcolors.d <- c("darkseagreen4",
	"darkcyan", "darkgreen")
naming.all$StimulusType <- factor(naming.all$StimulusType,
	levels=c("Medicinal", "Culinary"))
naming.all$Group2 <- naming.all$Group
# naming.all$Group2 <- factor(naming.all$Group,
	# labels=c("Laypeople", "Cooks", "Attars"))
# naming.all$Group2 <- factor(naming.all$Group2,
	# levels=c("Laypeople", "Cooks", "Attars"))
int.cell <- ggplot(naming.all,
	aes(x=Group2, y=Intensity, fill=Group2)) +
	scale_y_continuous(limits=c(1,7),
		breaks=seq(1,7,1)) +
	geom_boxplot(outlier.shape = NA) +
#	geom_jitter(color="gray50") +
	coord_flip() + ggtitle("Intensity\n") +
	facet_grid(. ~ StimulusType) +
	scale_fill_manual(name="Group",
		values=groupcolors) +
	basic.theme +
	theme(legend.position="none",
#		axis.ticks.y = element_blank(),
#		axis.text.y = element_blank(),
		title = element_text(size=26),
		axis.title.y = element_blank(),
		axis.title.x = element_blank(),
		axis.text.x = element_text(size=30, angle=0, hjust=0.5),
		plot.margin = unit(c(0,1.5,1.5,0), "cm"))
fam.cell <- ggplot(naming.all,
	aes(x=Group2, y=Familiarity, fill=Group2)) +
	scale_y_continuous(limits=c(1,7),
		breaks=seq(1,7,1)) +
	geom_boxplot(outlier.shape = NA) +
#	geom_jitter(color="gray50") +
	coord_flip() + ggtitle("Familiarity\n") +
	facet_grid(. ~ StimulusType) +
	scale_fill_manual(name="Group",
		values=groupcolors) +
	basic.theme +
	theme(legend.position="none",
#		axis.ticks.y = element_blank(),
#		axis.text.y = element_blank(),
		title = element_text(size=26),
		axis.title.y = element_blank(),
		axis.title.x = element_blank(),
		axis.text.x = element_text(size=30, angle=0, hjust=0.5),
		plot.margin = unit(c(0,1.5,1.5,0), "cm"))
fre.cell <- ggplot(naming.all,
	aes(x=Group2, y=Frequency, fill=Group2)) +
	scale_y_continuous(limits=c(1,7),
		breaks=seq(1,7,1)) +
	geom_boxplot(outlier.shape = NA) +
#	geom_jitter(color="gray50") +
	coord_flip() + ggtitle("Frequency\n") +
	facet_grid(. ~ StimulusType) +
	scale_fill_manual(name="Group",
		values=groupcolors) +
	basic.theme +
	theme(legend.position="none",
#		axis.ticks.y = element_blank(),
#		axis.text.y = element_blank(),
		title = element_text(size=26),
		axis.title.y = element_blank(),
		axis.title.x = element_blank(),
		axis.text.x = element_text(size=30, angle=0, hjust=0.5),
		plot.margin = unit(c(0,1.5,1.5,0), "cm"))
ple.cell <- ggplot(naming.all,
	aes(x=Group2, y=Pleasantness, fill=Group2)) +
	scale_y_continuous(limits=c(1,7),
		breaks=seq(1,7,1)) +
	geom_boxplot(outlier.shape = NA) +
#	geom_jitter(color="gray50") +
	coord_flip() + ggtitle("Pleasantness\n") +
	facet_grid(. ~ StimulusType) +
	scale_fill_manual(name="Group",
		values=groupcolors) +
	basic.theme +
	theme(legend.position="none",
#		axis.ticks.y = element_blank(),
#		axis.text.y = element_blank(),
		title = element_text(size=26),
		axis.title.y = element_blank(),
		axis.title.x = element_blank(),
		axis.text.x = element_text(size=30, angle=0, hjust=0.5),
		plot.margin = unit(c(0,1.5,1.5,0), "cm"))
edi.cell <- ggplot(naming.all,
	aes(x=Group2, y=Edibility, fill=Group2)) +
	scale_y_continuous(limits=c(1,7),
		breaks=seq(1,7,1)) +
	geom_boxplot(outlier.shape = NA) +
#	geom_jitter(color="gray50") +
	coord_flip() + ggtitle("Edibility\n") +
	facet_grid(. ~ StimulusType) +
	scale_fill_manual(name="Group",
		values=groupcolors) +
	basic.theme +
	theme(legend.position="none",
#		axis.ticks.y = element_blank(),
#		axis.text.y = element_blank(),
		title = element_text(size=26),
		axis.title.y = element_blank(),
		axis.title.x = element_blank(),
		axis.text.x = element_text(size=30, angle=0, hjust=0.5),
		plot.margin = unit(c(0,1.5,1.5,0), "cm"))
med.cell <- ggplot(naming.all,
	aes(x=Group2, y=Medicinal, fill=Group2)) +
	scale_y_continuous(limits=c(1,7),
		breaks=seq(1,7,1)) +
	geom_boxplot(outlier.shape = NA) +
#	geom_jitter(color="gray50") +
	coord_flip() + ggtitle("Medicinalness\n") +
	facet_grid(. ~ StimulusType) +
	scale_fill_manual(name="Group",
		values=groupcolors) +
	basic.theme +
	theme(legend.position="none",
#		axis.ticks.y = element_blank(),
#		axis.text.y = element_blank(),
		title = element_text(size=26),
		axis.title.y = element_blank(),
		axis.title.x = element_blank(),
		axis.text.x = element_text(size=30, angle=0, hjust=0.5),
		plot.margin = unit(c(0,1.5,1.5,0), "cm"))
png(paste(plot.path,
	"FIN-RAT-cell-summary.png", sep=""),
    width=1400,height=1000,units="px",
    bg = "transparent")
print(multiplot(fam.cell, edi.cell, ple.cell,
	fre.cell, med.cell, int.cell, cols=2))
dev.off()

# Change levels back to original
naming.all$StimulusType <- factor(naming.all$StimulusType,
	levels=c("Culinary","Medicinal"))
naming.all$Group2 <- factor(naming.all$Group2,
	levels=c("Attars", "Cooks", "Laypeople"))

# Raster plot of correctness
corr.data <- aggregate(CorrectNaming1Collapsed ~
	Frequency + Familiarity + Group2,
	naming.all, mean)
corr.raster <- ggplot(corr.data,
	aes(Familiarity, Frequency)) +
	geom_raster(aes(fill=CorrectNaming1Collapsed)) +
	guides(fill = guide_legend(title = "Naming\naccuracy\n",
		reverse = TRUE)) +
	facet_grid(. ~ Group2) +
  scale_fill_gradientn(colours = rev(grey.colors(10))) +
  scale_y_continuous(limits=c(1,7),
		breaks=seq(1,7,1)) +
	scale_x_continuous(limits=c(1,7),
		breaks=seq(1,7,1)) +
	basic.theme +
	labs(x="\nFamiliarity rating", y="Frequency rating\n") +
	theme(axis.text.x =
			element_text(size=30, angle=0, hjust=0.5),
		legend.title =
			element_text(size=22))
png(paste(plot.path,
	"FIN-COR-famfre-raster.png", sep=""),
    width=1000,height=350,units="px",
    bg = "transparent")
print(corr.raster)
dev.off()
# Add correctness by group
corr.data.indiv <- aggregate(CorrectNaming1Collapsed ~
	Group2 + ShortID,
	naming.all, mean)
corr.data.grp <- ggplot(corr.data.indiv,
  aes(x = Group2, y = CorrectNaming1Collapsed)) +
  geom_pirate(aes(colour = Group2, fill = Group2),
              alpha_bars = 0.5) +
	scale_alpha(range = c(0.2, 0.2)) +
  scale_fill_manual(name="Group",
		values=groupcolors) +
	scale_color_manual(name="Group",
		values=groupcolors.d) +
	guides(fill = guide_legend(title =
		"Naming\naccuracy\n",
		reverse = TRUE)) +
	scale_y_continuous(limits=c(0,1),
		breaks=seq(0,1,0.25)) +
	basic.theme +
	labs(x="\nParticipant group", y="Accuracy\n") +
	theme(axis.text.x =
			element_text(size=20, angle=0, hjust=0.5),
		legend.title =
			element_text(size=22))
png(paste(plot.path,
	"FIN-COR-grpavgs.png", sep=""),
    width=400,height=350,units="px",
    bg = "transparent")
print(corr.data.grp)
dev.off()

	
# Raster plot of responding
naming.all.2trs$Group2 <- factor(naming.all.2trs$Group,
	levels=c("Attars", "Cooks", "Laypeople"))
resp.data <- aggregate(respgiven ~
	Frequency + Familiarity + Group2,
	naming.all.2trs, mean)
resp.raster <- ggplot(resp.data,
	aes(Familiarity, Frequency)) +
	geom_raster(aes(fill=respgiven)) +
	guides(fill = guide_legend(title =
		"Proportion\nof trials with\nresponses\n",
		reverse = TRUE)) +
	facet_grid(. ~ Group2) +
  scale_fill_gradientn(colours = rev(grey.colors(10))) +
	scale_y_continuous(limits=c(1,7),
		breaks=seq(1,7,1)) +
	scale_x_continuous(limits=c(1,7),
		breaks=seq(1,7,1)) +
	basic.theme +
	labs(x="\nFamiliarity rating", y="Frequency rating\n") +
	theme(axis.text.x =
			element_text(size=30, angle=0, hjust=0.5),
		legend.title =
			element_text(size=22))
png(paste(plot.path,
	"FIN-RES-famfre-raster.png", sep=""),
    width=1000,height=350,units="px",
    bg = "transparent")
print(resp.raster)
dev.off()
# Add responsiveness by group
resp.data.indiv <- aggregate(respgiven ~
	Group2 + ShortID,
	naming.all.2trs, mean)
resp.data.grp <- ggplot(resp.data.indiv,
  aes(x = Group2, y = respgiven)) +
  geom_pirate(aes(colour = Group2, fill = Group2),
              alpha_bars = 0.5) +
	scale_alpha(range = c(0.2, 0.2)) +
  scale_fill_manual(name="Group",
		values=groupcolors) +
	scale_color_manual(name="Group",
		values=groupcolors.d) +
	guides(fill = guide_legend(title =
		"Responses\ngiven\n",
		reverse = TRUE)) +
	scale_y_continuous(limits=c(0,1),
		breaks=seq(0,1,0.25)) +
	basic.theme +
	labs(x="\nParticipant group", y="Responses\n") +
	theme(axis.text.x =
			element_text(size=20, angle=0, hjust=0.5),
		legend.title =
			element_text(size=22))
png(paste(plot.path,
	"FIN-RES-grpavgs.png", sep=""),
    width=400,height=350,units="px",
    bg = "transparent")
print(resp.data.grp)
dev.off()

# Raster plot of consistency
# Make subset of answers with consistency scores
naming.tworespons <- subset(naming.all,
	Consistency != "N/A")

# Ensure consistency ratings are numeric
naming.tworespons$Consistency <-
	ifelse(naming.tworespons$Consistency == "1",
	1, 0)

naming.tworespons$Group2 <- factor(naming.tworespons$Group,
	levels=c("Attars", "Cooks", "Laypeople"))
cons.data <- aggregate(Consistency ~
	Frequency + Familiarity + Group2,
	naming.tworespons, mean)
cons.raster <- ggplot(cons.data,
	aes(Familiarity, Frequency)) +
	geom_raster(aes(fill=Consistency)) +
	guides(fill = guide_legend(title =
		"Proportion\nconsistent\nresponses\n",
		reverse = TRUE)) +
	facet_grid(. ~ Group2) +
  scale_fill_gradientn(colours = rev(grey.colors(10))) +
	scale_y_continuous(limits=c(1,7),
		breaks=seq(1,7,1)) +
	scale_x_continuous(limits=c(1,7),
		breaks=seq(1,7,1)) +
	basic.theme +
	labs(x="\nFamiliarity rating", y="Frequency rating\n") +
	theme(axis.text.x =
			element_text(size=30, angle=0, hjust=0.5),
		legend.title =
			element_text(size=22))
png(paste(plot.path,
	"FIN-CON-famfre-raster.png", sep=""),
    width=1000,height=350,units="px",
    bg = "transparent")
print(cons.raster)
dev.off()
# Add consistency by group
cons.data.indiv <- aggregate(Consistency ~
	Group2 + ShortID,
	naming.tworespons, mean)
cons.data.grp <- ggplot(cons.data.indiv,
  aes(x = Group2, y = Consistency)) +
  geom_pirate(aes(colour = Group2, fill = Group2),
              alpha_bars = 0.5) +
	scale_alpha(range = c(0.2, 0.2)) +
  scale_fill_manual(name="Group",
		values=groupcolors) +
	scale_color_manual(name="Group",
		values=groupcolors.d) +
	guides(fill = guide_legend(title =
		"Consistency\n",
		reverse = TRUE)) +
	scale_y_continuous(limits=c(0,1),
		breaks=seq(0,1,0.25)) +
	basic.theme +
	labs(x="\nParticipant group", y="Consistency\n") +
	theme(axis.text.x =
			element_text(size=20, angle=0, hjust=0.5),
		legend.title =
			element_text(size=22))
png(paste(plot.path,
	"FIN-CON-grpavgs.png", sep=""),
    width=400,height=350,units="px",
    bg = "transparent")
print(cons.data.grp)
dev.off()

##### Summary of odor awareness ########################
odaware <- merge(odaware, ptcp.info, by="ShortID")

# Remove the "expert" who has only been trading
# spices for 1 year and who does it part-time (P09)
odaware <- subset(odaware, ShortID != "P09")

# Change the group names
odaware$Group <- factor(odaware$Group,
	labels=c("Laypeople", "Cooks", "Attars"))
odaware$Group <- factor(odaware$Group,
	levels=c("Attars", "Cooks", "Laypeople"))

# create an summary of ratings by group
odaware$mn.rtg <- rowMeans(odaware[,2:11])
summ.oa.df <- data.frame(
	m = aggregate(mn.rtg ~
		Group, odaware, mean)$mn.rtg,
	sem = aggregate(mn.rtg ~
		Group, odaware, sem)$mn.rtg,
	Group = aggregate(mn.rtg ~
		Group, odaware, mean)$Group)
summ.oa.df$Group <- as.factor(summ.oa.df$Group)
summ.odaware.rtgs <- ggplot(summ.oa.df,
	aes(x=factor(Group), y=m,
		group=factor(Group), fill=factor(Group))) +
	geom_bar(stat="identity",
		position=position_dodge(), color="black") +
	geom_errorbar(aes(ymin=m-sem, ymax=m+sem),
		width=.2, position=position_dodge(.9)) +
	scale_fill_manual(name="Group",
		values=rev(groupcolors)) +
	coord_cartesian(ylim=c(1,5)) +
	xlab("\nGroup") +
	ylab("Average rating (1-5)\n") +
	basic.theme + theme(
	axis.text.x = element_blank(),
	axis.ticks.x = element_blank())
png(paste(plot.path,
	"FIN-ODA-item-summary-overall.png", sep=""),
    width=600,height=500,units="px",
    bg = "transparent")
print(summ.odaware.rtgs)
dev.off()

# Create overview of individual questions by group
# longer titles for questions
long.qs <- c(
	rep("1. sniff at a new book?", 3),
	rep("2. feel cheerful when detecting pleasant odors?", 3),
	rep("3. notice people's perfume/deodorant?", 3),
	rep("4. get vivid memories from odors?", 3),
	rep("5. think odors are important in
		your everyday life?", 3),
	rep("6. notice the smells of others' houses?", 3),
	rep("7. smell gas before everyone else?", 3),
	rep("8. smell fire before everyone else?", 3),
	rep("9. smell spoilt food before everyone else?", 3),
	rep("10. feel sensitive to/affected by odors?", 3))
long.qs <- factor(long.qs, levels=c(
	"1. sniff at a new book?",
	"2. feel cheerful when detecting pleasant odors?",
	"3. notice people's perfume/deodorant?",
	"4. get vivid memories from odors?",
	"5. think odors are important in your everyday life?",
	"6. notice the smells of others' houses?",
	"7. smell gas before everyone else?",
	"8. smell fire before everyone else?",
	"9. smell spoilt food before everyone else?",
	"10. feel sensitive to/affected by odors?"))
# shorter titles for questions
short.qs <- c(
	rep("1. new book", 3),
	rep("2. pleasant odors", 3),
	rep("3. perfume/deodorant", 3),
	rep("4. vivid memories", 3),
	rep("5. everyday importance", 3),
	rep("6. others' house smells", 3),
	rep("7. smell gas first", 3),
	rep("8. smell fire first", 3),
	rep("9. smell spoilt food first", 3),
	rep("10. odor sensitivity", 3))
short.qs <- factor(short.qs, levels=c(
	"1. new book",
	"2. pleasant odors",
	"3. perfume/deodorant",
	"4. vivid memories",
	"5. everyday importance",
	"6. others' house smells",
	"7. smell gas first",
	"8. smell fire first",
	"9. smell spoilt food first",
	"10. odor sensitivity"))

oa.df <- data.frame(
	Group = rep(c("Attars", "Cooks", "Laypeople"), 10),
	rating = short.qs,
	mean = c(
		aggregate(OA1 ~ Group, odaware, mean)$OA1,
		aggregate(OA2 ~ Group, odaware, mean)$OA2,
		aggregate(OA3 ~ Group, odaware, mean)$OA3,
		aggregate(OA4 ~ Group, odaware, mean)$OA4,
		aggregate(OA5 ~ Group, odaware, mean)$OA5,
		aggregate(OA6 ~ Group, odaware, mean)$OA6,
		aggregate(OA7 ~ Group, odaware, mean)$OA7,
		aggregate(OA8 ~ Group, odaware, mean)$OA8,
		aggregate(OA9 ~ Group, odaware, mean)$OA9,
		aggregate(OA10 ~ Group, odaware, mean)$OA10),
	sem = c(
		aggregate(OA1 ~ Group, odaware, sem)$OA1,
		aggregate(OA2 ~ Group, odaware, sem)$OA2,
		aggregate(OA3 ~ Group, odaware, sem)$OA3,
		aggregate(OA4 ~ Group, odaware, sem)$OA4,
		aggregate(OA5 ~ Group, odaware, sem)$OA5,
		aggregate(OA6 ~ Group, odaware, sem)$OA6,
		aggregate(OA7 ~ Group, odaware, sem)$OA7,
		aggregate(OA8 ~ Group, odaware, sem)$OA8,
		aggregate(OA9 ~ Group, odaware, sem)$OA9,
		aggregate(OA10 ~ Group, odaware, sem)$OA10),
	stringsAsFactors = FALSE)
oa.df$Group <- as.factor(oa.df$Group)	
odaware.rtgs <- ggplot(oa.df,
	aes(x=factor(rating), y=mean,
		group=factor(Group), fill=factor(Group))) +
	geom_bar(stat="identity",
		position=position_dodge(), color="black") +
	geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),
		width=.2, position=position_dodge(.9)) +
	scale_fill_manual(name="Group",
		values=rev(groupcolors)) +
	coord_cartesian(ylim=c(1,5)) +
	xlab("\nOdor Awareness Questions") +
	ylab("Rating (1-5)\n") +
	basic.theme + theme(
	axis.text.x = element_text(size=22))
png(paste(plot.path,
	"FIN-ODA-item-summary.png", sep=""),
    width=1500,height=600,units="px",
    bg = "transparent")
print(odaware.rtgs)
dev.off()

# odor awareness by gender (control group only)
control.oa <- subset(odaware, Group == "Laypeople")
# shorter titles for questions
short.qs.2 <- c(
	rep("1. new book", 2),
	rep("2. pleasant odors", 2),
	rep("3. perfume/deodorant", 2),
	rep("4. vivid memories", 2),
	rep("5. everyday importance", 2),
	rep("6. others' house smells", 2),
	rep("7. smell gas first", 2),
	rep("8. smell fire first", 2),
	rep("9. smell spoilt food first", 2),
	rep("10. odor sensitivity", 2))
short.qs.2 <- factor(short.qs.2, levels=c(
	"1. new book",
	"2. pleasant odors",
	"3. perfume/deodorant",
	"4. vivid memories",
	"5. everyday importance",
	"6. others' house smells",
	"7. smell gas first",
	"8. smell fire first",
	"9. smell spoilt food first",
	"10. odor sensitivity"))
oa.df.con <- data.frame(
	Gender = rep(c("Female", "Male"), 10),
	rating = short.qs.2,
	mean = c(
		aggregate(OA1 ~ Gender, control.oa, mean)$OA1,
		aggregate(OA2 ~ Gender, control.oa, mean)$OA2,
		aggregate(OA3 ~ Gender, control.oa, mean)$OA3,
		aggregate(OA4 ~ Gender, control.oa, mean)$OA4,
		aggregate(OA5 ~ Gender, control.oa, mean)$OA5,
		aggregate(OA6 ~ Gender, control.oa, mean)$OA6,
		aggregate(OA7 ~ Gender, control.oa, mean)$OA7,
		aggregate(OA8 ~ Gender, control.oa, mean)$OA8,
		aggregate(OA9 ~ Gender, control.oa, mean)$OA9,
		aggregate(OA10 ~ Gender, control.oa, mean)$OA10),
	sem = c(
		aggregate(OA1 ~ Gender, control.oa, sem)$OA1,
		aggregate(OA2 ~ Gender, control.oa, sem)$OA2,
		aggregate(OA3 ~ Gender, control.oa, sem)$OA3,
		aggregate(OA4 ~ Gender, control.oa, sem)$OA4,
		aggregate(OA5 ~ Gender, control.oa, sem)$OA5,
		aggregate(OA6 ~ Gender, control.oa, sem)$OA6,
		aggregate(OA7 ~ Gender, control.oa, sem)$OA7,
		aggregate(OA8 ~ Gender, control.oa, sem)$OA8,
		aggregate(OA9 ~ Gender, control.oa, sem)$OA9,
		aggregate(OA10 ~ Gender, control.oa, sem)$OA10),
	stringsAsFactors = FALSE)
odaware.rtgs.con <- ggplot(oa.df.con,
	aes(x=factor(rating), y=mean,
		group=factor(Gender), fill=factor(Gender))) +
	geom_bar(stat="identity",
		position=position_dodge(), color="black") +
	geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),
		width=.2, position=position_dodge(.9)) +
	scale_fill_manual(name="Gender",
		values=c("lightcoral", "lightblue2")) +
	coord_cartesian(ylim=c(1,5)) +
	xlab("\nOdor Awareness Questions") +
	ylab("Rating (1-5)\n") +
	basic.theme + theme(
	axis.text.x = element_text(size=22))
png(paste(plot.path,
	"FIN-ODA-item-summary-gender-controls.png", sep=""),
    width=1500,height=600,units="px",
    bg = "transparent")
print(odaware.rtgs.con)
dev.off()

# odor awareness by age
# shorter titles for questions
short.qs.3 <- c(
	rep("1. new book", 24),
	rep("2. pleasant odors", 24),
	rep("3. perfume/deodorant", 24),
	rep("4. vivid memories", 24),
	rep("5. everyday importance", 24),
	rep("6. others' house smells", 24),
	rep("7. smell gas first", 24),
	rep("8. smell fire first", 24),
	rep("9. smell spoilt food first", 24),
	rep("10. odor sensitivity", 24))
short.qs.3 <- factor(short.qs.3, levels=c(
	"1. new book",
	"2. pleasant odors",
	"3. perfume/deodorant",
	"4. vivid memories",
	"5. everyday importance",
	"6. others' house smells",
	"7. smell gas first",
	"8. smell fire first",
	"9. smell spoilt food first",
	"10. odor sensitivity"))
oa.df.age <- data.frame(
	Age = rep(sort(unique(odaware$Age)), 10),
	rating = short.qs.3,
	mean = c(
		aggregate(OA1 ~ Age, odaware, mean)$OA1,
		aggregate(OA2 ~ Age, odaware, mean)$OA2,
		aggregate(OA3 ~ Age, odaware, mean)$OA3,
		aggregate(OA4 ~ Age, odaware, mean)$OA4,
		aggregate(OA5 ~ Age, odaware, mean)$OA5,
		aggregate(OA6 ~ Age, odaware, mean)$OA6,
		aggregate(OA7 ~ Age, odaware, mean)$OA7,
		aggregate(OA8 ~ Age, odaware, mean)$OA8,
		aggregate(OA9 ~ Age, odaware, mean)$OA9,
		aggregate(OA10 ~ Age, odaware, mean)$OA10),
	sem = c(
		aggregate(OA1 ~ Age, odaware, sem)$OA1,
		aggregate(OA2 ~ Age, odaware, sem)$OA2,
		aggregate(OA3 ~ Age, odaware, sem)$OA3,
		aggregate(OA4 ~ Age, odaware, sem)$OA4,
		aggregate(OA5 ~ Age, odaware, sem)$OA5,
		aggregate(OA6 ~ Age, odaware, sem)$OA6,
		aggregate(OA7 ~ Age, odaware, sem)$OA7,
		aggregate(OA8 ~ Age, odaware, sem)$OA8,
		aggregate(OA9 ~ Age, odaware, sem)$OA9,
		aggregate(OA10 ~ Age, odaware, sem)$OA10),
	stringsAsFactors = FALSE)
odaware.rtgs.age <- ggplot(oa.df.age,
	aes(x=Age, y=mean,
		group=factor(rating), color=factor(rating))) +
	geom_point() + geom_line() +
	scale_y_continuous(limits=c(1,5),
		breaks=seq(1,5,1)) +
	facet_wrap(~ rating, ncol=2) +
	ylab("Rating (1-5)\n") +
	basic.theme + theme(
	strip.text.x = element_text(size=20),
	legend.position = "none")
png(paste(plot.path,
	"FIN-ODA-item-summary-age.png", sep=""),
    width=800,height=1000,units="px",
    bg = "transparent")
print(odaware.rtgs.age)
dev.off()



### DRAFTS! NOT USED IN MANUSCRIPT ############################
# Overview of ratings
int.agg <- aggregate(Intensity ~
	StimulusID + Group + StimulusType, naming.all, mean)
names(int.agg)[4] <- "Rating"
int.agg$RatingType <- "Intensity"
fam.agg <- aggregate(Familiarity ~
	StimulusID + Group + StimulusType, naming.all, mean)
names(fam.agg)[4] <- "Rating"
fam.agg$RatingType <- "Familiarity"
fre.agg <- aggregate(Frequency ~
	StimulusID + Group + StimulusType, naming.all, mean)
names(fre.agg)[4] <- "Rating"
fre.agg$RatingType <- "Frequency"
ple.agg <- aggregate(Pleasantness ~
	StimulusID + Group + StimulusType, naming.all, mean)
names(ple.agg)[4] <- "Rating"
ple.agg$RatingType <- "Pleasantness"
edi.agg <- aggregate(Edibility ~
	StimulusID + Group + StimulusType, naming.all, mean)
names(edi.agg)[4] <- "Rating"
edi.agg$RatingType <- "Edibility"
med.agg <- aggregate(Medicinal ~
	StimulusID + Group + StimulusType, naming.all, mean)
names(med.agg)[4] <- "Rating"
med.agg$RatingType <- "Medicinal"
rating.agg <- rbind(int.agg, fam.agg, fre.agg,
	ple.agg, edi.agg, med.agg)
rating.agg$StimulusType <- factor(rating.agg$StimulusType,
	levels=c("Medicinal", "Culinary"))
rating.agg$StimulusID <- factor(rating.agg$StimulusID,
	levels=correctness.order)
rating.agg$RatingTypeAB <- factor(rating.agg$RatingType,
	labels=c("ED", "FA", "FR", "IN", "ME", "PL"))
med.sub <- subset(rating.agg, StimulusType == "Medicinal")
cul.sub <- subset(rating.agg, StimulusType == "Culinary")
uniqueAbrevs <- c("IN", "FA", "FR", "PL", "ED", "ME")
abrevShapes <- unlist(lapply(uniqueAbrevs, utf8ToInt))
p.cul <- ggplot(cul.sub,
	aes(x=StimulusID, y=Rating, label=RatingTypeAB)) +
	facet_grid(. ~ Group) +
	scale_y_continuous(breaks=c(1:7)) + expand_limits(y=c(1,7)) +
	geom_text() +
	scale_shape_manual(values = abrevShapes) +
	theme(axis.text.x = element_text(size=20, angle=45)) +
	basic.theme + theme(
		strip.text = element_text(size=20),
		plot.margin=unit(c(1,0,1,1.5), "cm"))
p.med <- ggplot(med.sub,
	aes(x=StimulusID, y=Rating, label=RatingTypeAB)) +
	facet_grid(. ~ Group) +
	scale_y_continuous(breaks=c(1:7)) + expand_limits(y=c(1,7)) +
	geom_text() +
	scale_shape_manual(values = abrevShapes) +
	theme(axis.text.x = element_text(size=20, angle=45)) +
	basic.theme + theme(
		strip.text = element_text(size=20),
		plot.margin=unit(c(1,0,1,1.5), "cm"))
png(paste(plot.path,
	"FIN-RAT-summary.png", sep=""),
    width=1000,height=1500,units="px",
    bg = "transparent")
print(multiplot(p.cul, p.med))
dev.off()

# Intensity decreases with age
age.i <- aggregate(Intensity^2 ~ Age, naming.all, mean)
age.ses.i <- aggregate(Intensity^2 ~ Age, naming.all, sem)$Intensity
age.sum.i <- data.frame(
	Age = age.i$Age,
	m = age.i$Intensity,
	se = age.ses.i,
	stringsAsFactors=F)
p <- ggplot(age.sum.i,
	aes(x=Age, y=m)) +
	geom_pointrange(limits, size=1.5) +
	basic.theme
png(paste(plot.path,
	"FIN-RAT-age-by-intensity.png", sep=""),
    width=1000,height=600,units="px",
    bg = "transparent")
print(p)
dev.off()

# Culinary odors are rated as more familiar
# Experts rate medicinal odors as more familiar than others
p <- ggplot(naming.all,
	aes(x=StimulusType, y=Familiarity)) +
	facet_grid(. ~ Group) +
	geom_violin(position=position_dodge(1), trim=F) +
	geom_boxplot(position=position_dodge(1), width=0.1) +
	basic.theme + theme(strip.text = element_text(size=20))
png(paste(plot.path,
	"FIN-RAT-familiarity_odortype-by-group.png", sep=""),
    width=1000,height=600,units="px",
    bg = "transparent")
print(p)
dev.off()

# Experts generally rate odors as more frequent than controls
p <- ggplot(naming.all,
	aes(x=Group, y=Frequency)) +
	geom_violin(position=position_dodge(1), trim=F) +
	geom_boxplot(position=position_dodge(1), width=0.1) +
	basic.theme + theme(strip.text = element_text(size=20))
png(paste(plot.path,
	"FIN-RAT-frequency_group.png", sep=""),
    width=1000,height=600,units="px",
    bg = "transparent")
print(p)
dev.off()

# Culinary odors are rated as more frequent
# Experts rate medicinal odors as more frequent than others
p <- ggplot(naming.all,
	aes(x=StimulusType, y=Frequency)) +
	facet_grid(. ~ Group) +
	geom_violin(position=position_dodge(1), trim=F) +
	geom_boxplot(position=position_dodge(1), width=0.1) +
	basic.theme + theme(strip.text = element_text(size=20))
png(paste(plot.path,
	"FIN-RAT-frequency_odortype-by-group.png", sep=""),
    width=1000,height=600,units="px",
    bg = "transparent")
print(p)
dev.off()

# Culinary odors are rated as more pleasant
# Experts rate medicinal odors as more pleasant than others
p <- ggplot(naming.all,
	aes(x=StimulusType, y=Pleasantness)) +
	facet_grid(. ~ Group) +
	geom_violin(position=position_dodge(1), trim=F) +
	geom_boxplot(position=position_dodge(1), width=0.1) +
	basic.theme + theme(strip.text = element_text(size=20))
png(paste(plot.path,
	"FIN-RAT-pleasantness_odortype-by-group.png", sep=""),
    width=1000,height=600,units="px",
    bg = "transparent")
print(p)
dev.off()

# Experts generally rate odors as more edible than controls
p <- ggplot(naming.all,
	aes(x=Group, y=Edibility)) +
	geom_violin(position=position_dodge(1), trim=F) +
	geom_boxplot(position=position_dodge(1), width=0.1) +
	basic.theme + theme(strip.text = element_text(size=20))
png(paste(plot.path,
	"FIN-RAT-edibility_group.png", sep=""),
    width=1000,height=600,units="px",
    bg = "transparent")
print(p)
dev.off()

# Culinary odors are rated as more edible
p <- ggplot(naming.all,
	aes(x=StimulusType, y=Edibility)) +
	geom_violin(position=position_dodge(1), trim=F) +
	geom_boxplot(position=position_dodge(1), width=0.1) +
	basic.theme + theme(strip.text = element_text(size=20))
png(paste(plot.path,
	"FIN-RAT-edibility_odortype.png", sep=""),
    width=1000,height=600,units="px",
    bg = "transparent")
print(p)
dev.off()

# Experts rate medicinal odors as more medicinal than others
p <- ggplot(naming.all,
	aes(x=StimulusType, y=Medicinal)) +
	facet_grid(. ~ Group) +
	geom_violin(position=position_dodge(1), trim=F) +
	geom_boxplot(position=position_dodge(1), width=0.1) +
	basic.theme + theme(strip.text = element_text(size=20))
png(paste(plot.path,
	"FIN-RAT-medicinal_odortype-by-group.png", sep=""),
    width=1000,height=600,units="px",
    bg = "transparent")
print(p)
dev.off()

plot.path <- "plots/"