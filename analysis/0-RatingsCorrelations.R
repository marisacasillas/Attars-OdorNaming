##### Individual ratings level #######################
# Intensity
cor.test(naming.all$Intensity, naming.all$Familiarity)
# t = 17.8324, df = 702, p-value < 2.2e-16
# 95 percent confidence interval: 0.5053108 0.6071949
# cor: 0.5583546

cor.test(naming.all$Intensity, naming.all$Frequency)
# t = 8.3146, df = 702, p-value = 4.441e-16
# 95 percent confidence interval: 0.2306288 0.3652295
# cor 0.2994181 

cor.test(naming.all$Intensity, naming.all$Pleasantness)
# t = 12.759, df = 702, p-value < 2.2e-16
# 95 percent confidence interval: 0.3719035 0.4919911
# cor: 0.4338723

cor.test(naming.all$Intensity, naming.all$Edibility)
# t = 8.7157, df = 702, p-value < 2.2e-16
# 95 percent confidence interval: 0.2442281 0.3776528
# cor: 0.3124808 

cor.test(naming.all$Intensity, naming.all$Medicinal)
# t = 4.4881, df = 702, p-value = 8.397e-06
# 95 percent confidence interval: 0.09428427 0.23796778
# cor: 0.1670126

# Familiarity
cor.test(naming.all$Familiarity, naming.all$Frequency)
# t = 18.6715, df = 702, p-value < 2.2e-16
# 95 percent confidence interval: 0.5244760 0.6234005
# cor: 0.5760436

cor.test(naming.all$Familiarity, naming.all$Pleasantness)
# t = 20.4259, df = 702, p-value < 2.2e-16
# 95 percent confidence interval: 0.5620184 0.6549008
# cor: 0.6105548 

cor.test(naming.all$Familiarity, naming.all$Edibility)
# t = 15.7947, df = 702, p-value < 2.2e-16
# 95 percent confidence interval: 0.4553894 0.5645812
# cor: 0.512051

cor.test(naming.all$Familiarity, naming.all$Medicinal)
# t = 3.199, df = 702, p-value = 0.001441
# 95 percent confidence interval: 0.04638761 0.19205940
# cor: 0.1198686


# Frequency
cor.test(naming.all$Frequency, naming.all$Pleasantness)
# t = 15.9421, df = 702, p-value < 2.2e-16
# 95 percent confidence interval: 0.4591649 0.5678245
# cor: 0.5155644

cor.test(naming.all$Frequency, naming.all$Edibility)
# t = 19.1231, df = 702, p-value < 2.2e-16
# 95 percent confidence interval: 0.5344630 0.6318117
# cor: 0.5852423

cor.test(naming.all$Frequency, naming.all$Medicinal)
# t = 3.3236, df = 702, p-value = 0.0009348
# 95 percent confidence interval: 0.05104277 0.19654946
# cor: 0.1244652


# Pleasantness
cor.test(naming.all$Pleasantness, naming.all$Edibility)
# t = 22.2003, df = 702, p-value < 2.2e-16
# 95 percent confidence interval: 0.5966702 0.6836921
# cor: 0.6422461

cor.test(naming.all$Pleasantness, naming.all$Medicinal)
# t = 4.3255, df = 702, p-value = 1.743e-05
# 95 percent confidence interval: 0.08828104 0.23224872
# cor: 0.1611219


# Edibility
cor.test(naming.all$Edibility, naming.all$Medicinal)
# t = 1.757, df = 702, p-value = 0.07935
# 95 percent confidence interval: -0.007761342  0.139378923
# cor: 0.0661685

##### Item average ratings level #######################
item.avgs <- data.frame(
	Intensity = aggregate(Intensity ~
		StimulusID, naming.all, mean)$Intensity,
	Familiarity = aggregate(Familiarity ~
		StimulusID, naming.all, mean)$Familiarity,
	Frequency = aggregate(Frequency ~
		StimulusID, naming.all, mean)$Frequency,
	Pleasantness = aggregate(Pleasantness ~
		StimulusID, naming.all, mean)$Pleasantness,
	Edibility = aggregate(Edibility ~
		StimulusID, naming.all, mean)$Edibility,
	Medicinal = aggregate(Medicinal ~
		StimulusID, naming.all, mean)$Medicinal)

# Intensity
cor.test(item.avgs$Intensity, item.avgs$Familiarity)
# t = 4.503, df = 14, p-value = 0.0004964
# 95 percent confidence interval: 0.4419062 0.9157099
# cor: 0.7691296

cor.test(item.avgs$Intensity, item.avgs$Frequency)
# t = 1.7105, df = 14, p-value = 0.1092
# 95 percent confidence interval: -0.1007016  0.7557152
# cor 0.4157577 

cor.test(item.avgs$Intensity, item.avgs$Pleasantness)
# t = 3.4225, df = 14, p-value = 0.004124
# 95 percent confidence interval: 0.2693487 0.8771687
# cor: 0.6749379

cor.test(item.avgs$Intensity, item.avgs$Edibility)
# t = 2.2428, df = 14, p-value = 0.04162
# 0.0247106 0.8047358
# cor: 0.5141184

cor.test(item.avgs$Intensity, item.avgs$Medicinal)
# t = 1.1656, df = 14, p-value = 0.2633
# 95 percent confidence interval: -0.2325822  0.6912130
# cor: 0.2974125 

# Familiarity
cor.test(item.avgs$Familiarity, item.avgs$Frequency)
# t = 6.7485, df = 14, p-value = 9.342e-06
# 95 percent confidence interval: 0.6688165 0.9558757
# cor: 0.8745703

cor.test(item.avgs$Familiarity, item.avgs$Pleasantness)
# t = 5.2474, df = 14, p-value = 0.0001234
# 95 percent confidence interval: 0.5340548 0.9332489
# cor: 0.814211

cor.test(item.avgs$Familiarity, item.avgs$Edibility)
# t = 6.0373, df = 14, p-value = 3.053e-05
# 95 percent confidence interval: 0.6122635 0.9467777
# cor: 0.8499938

cor.test(item.avgs$Familiarity, item.avgs$Medicinal)
# t = 0.1297, df = 14, p-value = 0.8987
# 95 percent confidence interval: -0.4691278  0.5213858
# cor: 0.03463092 


# Frequency
cor.test(item.avgs$Frequency, item.avgs$Pleasantness)
# t = 3.6185, df = 14, p-value = 0.002794
# 95 percent confidence interval: 0.3043484 0.8856749
# cor: 0.6951747

cor.test(item.avgs$Frequency, item.avgs$Edibility)
# t = 8.0719, df = 14, p-value = 1.234e-06
# 95 percent confidence interval: 0.7479364 0.9677426
# cor: 0.907267

cor.test(item.avgs$Frequency, item.avgs$Medicinal)
# t = -0.7347, df = 14, p-value = 0.4746
# 95 percent confidence interval: -0.6283698  0.3350221
# cor: -0.1926824


# Pleasantness
cor.test(item.avgs$Pleasantness, item.avgs$Edibility)
# t = 6.396, df = 14, p-value = 1.666e-05
# 95 percent confidence interval: 0.6422418 0.9516680
# cor: 0.8631518 

cor.test(item.avgs$Pleasantness, item.avgs$Medicinal)
# t = 0.8944, df = 14, p-value = 0.3862
# 95 percent confidence interval: -0.2974942  0.6529501
# cor: 0.2324974


# Edibility
cor.test(item.avgs$Edibility, item.avgs$Medicinal)
# t = -0.2868, df = 14, p-value = 0.7785
# 95 percent confidence interval: -0.5512472  0.4357894
# cor: -0.07642562