########## Models without age
# Intensity
m.int <- lmer(Intensity^2 ~
	Group * StimulusType +
	(1|ShortID) + (1|StimulusID),
	naming.all)
# Model won't converge with random Group
# slope for stimulus
summary(m.int)
m.int.resid <- residuals(m.int)
qqnorm(m.int.resid)
qqline(m.int.resid)

# Familiarity
m.fam <- lmer(Familiarity^2 ~
	Group * StimulusType +
	(1|ShortID) + (1 + Group|StimulusID),
	naming.all)
summary(m.fam)
m.fam.resid <- residuals(m.fam)
qqnorm(m.fam.resid)
qqline(m.fam.resid)

# Frequency
m.fre <- lmer(Frequency^2 ~
	Group * StimulusType +
	(1|ShortID) + (1|StimulusID),
	naming.all)
summary(m.fre)
# Model won't converge with random Group
# slope for stimulus
m.fre.resid <- residuals(m.fre)
qqnorm(m.fre.resid)
qqline(m.fre.resid)

# Pleasantness
m.ple <- lmer(Pleasantness^2 ~
	Group * StimulusType +
	(1|ShortID) + (1 + Group|StimulusID),
	naming.all)
summary(m.ple)
m.ple.resid <- residuals(m.ple)
qqnorm(m.ple.resid)
qqline(m.ple.resid)

# Edibility
m.edi <- lmer(Edibility^2 ~
	Group * StimulusType +
	(1|ShortID) + (1 + Group|StimulusID),
	naming.all)
summary(m.edi)
m.edi.resid <- residuals(m.edi)
qqnorm(m.edi.resid)
qqline(m.edi.resid)

# Medicinality
m.med <- lmer(Medicinal^2 ~
	Group * StimulusType +
	(1|ShortID) + (1|StimulusID),
	naming.all)
# Model won't converge with random Group
# slope for stimulus
summary(m.med)
m.med.resid <- residuals(m.med)
qqnorm(m.med.resid)
qqline(m.med.resid)

# Comparisons with max models
anova(m.int, m.int.max) # better with age *** but random slope diff
summary(m.int)
summary(m.int.max)

anova(m.fam, m.fam.max) # not sign/qual different
summary(m.fam)
summary(m.fam.max)

anova(m.fre, m.fre.max) # not sign/qual different
summary(m.fre)
summary(m.fre.max)

anova(m.ple, m.ple.max) # not sign/qual different
summary(m.ple)
summary(m.ple.max)

anova(m.edi, m.edi.max) # not sign/qual different
summary(m.edi)
summary(m.edi.max)

anova(m.med, m.med.max) # not sign/qual different
summary(m.med)
summary(m.med.max)

########## Models releveled for group effects
# Familiarity
m.fam.max2 <- lmer(Familiarity^2 ~
	Group2 * StimulusType + AgeC +
	(1|ShortID) + (1 + Group|StimulusID),
	naming.all)
summary(m.fam.max2)

m.fam.max3 <- lmer(Familiarity^2 ~
	Group3 * StimulusType + AgeC +
	(1|ShortID) + (1 + Group|StimulusID),
	naming.all)
summary(m.fam.max3)

# Frequency
m.fre.max2 <- lmer(Frequency^2 ~
	Group2 * StimulusType + AgeC +
	(1|ShortID) + (1|StimulusID),
	naming.all)
summary(m.fre.max2)

m.fre.max3 <- lmer(Frequency^2 ~
	Group3 * StimulusType + AgeC +
	(1|ShortID) + (1|StimulusID),
	naming.all)
summary(m.fre.max3)

# Pleasantness
m.ple.max2 <- lmer(Pleasantness^2 ~
	Group2 * StimulusType + AgeC +
	(1|ShortID) + (1 + Group|StimulusID),
	naming.all)
summary(m.ple.max2)

m.ple.max3 <- lmer(Pleasantness^2 ~
	Group3 * StimulusType + AgeC +
	(1|ShortID) + (1 + Group|StimulusID),
	naming.all)
summary(m.ple.max3)

# Edibility
m.edi.max2 <- lmer(Edibility^2 ~
	Group2 * StimulusType + AgeC +
	(1|ShortID) + (1 + Group|StimulusID),
	naming.all)
summary(m.edi.max2)

m.edi.max3 <- lmer(Edibility^2 ~
	Group3 * StimulusType + AgeC +
	(1|ShortID) + (1 + Group|StimulusID),
	naming.all)
summary(m.edi.max3)

# Medicinalness
m.med.max2 <- lmer(Medicinal^2 ~
	Group2 * StimulusType + AgeC +
	(1|ShortID) + (1|StimulusID),
	naming.all)
summary(m.med.max2)

m.med.max3 <- lmer(Medicinal^2 ~
	Group3 * StimulusType + AgeC +
	(1|ShortID) + (1|StimulusID),
	naming.all)
summary(m.med.max3)
