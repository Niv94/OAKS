#First check for factors
is.numeric(`Height(cm) (03/2022)`)
is.factor(treatment)
treatment=as.factor(treatment)
is.factor(Species)
Species=as.factor(Species)
is.factor(FileName)
FileName=as.factor(FileName)
#######################
####### 
#Box plots
boxplot(`Height(cm) (03/2022)`~ Species, xlab="Species ", ylab="Height total (mm)") 

#reordering
Colororder=factor(Color, levels=c("Green","Greenish yellow","Yellow","Yellowish orange","Orangy red","Red"))
levels(Colororder)
boxplot(Uv_meanintensity~Colororder, xlab="Color", ylab="UV emission intensity", col= c("lawngreen","yellowgreen","yellow","darkorange","orangered","red"))

#then normality

shapiro.test(Uv_meanintensity) 


##Now test assumptions for D vs Sex

# data appear to follow a normal distribution, but you can use Shapiro for hypothesis test of this
shapiro.test(Uv_meanintensity[Sex=="Female"])
shapiro.test(Uv_meanintensity [Sex=="Male"])

#data are normal, now do test of homoscedascity using Bartlett test
bartlett.test(`Height(cm) (03/2022)`~ treatment)#test for homoscedascity for levels of sex
#data has equal variances
bartlett.test(`Height(cm) (03/2022)`~ Table)
install.packages("car")
library(car)
leveneTest(`Height(cm) (03/2022)`,Table)
leveneTest(Uv_meanintensity,Color)



#now anova for D
model<-aov(`Height(cm) (03/2022)`~treatment+Table+Table:treatment)
summary(model)#this prints the ANOVA table ; shows both sex and color are significant
TukeyHSD(model) #shows male and female differ in diameter; 


######

#simpler bar plots without shading
library(ggplot2)
#Color#
#gives mean, sd, and n for different levels of factors 
anovan <- aggregate(`Height(cm) (03/2022)`,
                    by = list(sal = Table), FUN = function(x) c(mean = mean(x), sd = sd(x), n = length(x)))

#above data frame gives a matrices but we want it in vector form:                                                                                                                 
anovan <-do.call(data.frame,anovan) #puts it in vector form

#calculate st error and add it to our data.frame
anovan$se<-anovan$x.sd/sqrt(anovan$x.n)

#makes it look nicer
colnames(anovan) <- c("table","Mean","sd","n","se")
anovan #lookin good

#dodge I think gives bin width, but I'm not sure
#limits gives error bars
dodge <- position_dodge(width = 1)
limits <- aes(ymax = anovan$Mean + anovan$se,
              ymin = anovan$Mean - anovan$se)

#fill is the color coding of the bars
p <- ggplot(data = anovan, aes(x = Color, y = Mean, fill = Color))

#not sure what stat="identity" does but yeah, this should give us the bar plot with error bars
p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.x=element_blank()) +
  labs(x = "Color",y="Mean diameter (mm)") + scale_fill_manual(values=c("lawngreen",
                                                                        "yellowgreen",
                                                                        "orangered",
                                                                        "red1","yellow","tan1"))


