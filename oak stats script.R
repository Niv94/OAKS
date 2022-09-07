install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))
install.packages(c("multcompView"))

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(dplyr)
library(multcompView)

agrifolia_H <- read.csv("oaks-diameter.csv")
agrifolia_H <- read.csv("agrifolia_height.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "numeric"))
head(agrifolia_H)
str(agrifolia_H)
summary(agrifolia_H)

###To delete unnecessary columns /not necessary if manually deleting 
Oaks <- agrifolia_H%>% select(-(X:X.11))
str(Oaks)

summary(Oaks)


one.way <- aov(`height (cm) 09/06` ~ Locality, data = agrifolia_H)

summary(one.way)



##Mean of height by locality
nivi%>%                                        # Specify data frame
  group_by(Locality) %>%                         # Specify group indicator
  summarise(
    count_Height = n(),
    Height_mean = mean(`height (cm) 09/06`, na.rm = TRUE),
    sd_Height = sd(`height (cm) 09/06`, na.rm = TRUE)
  )


##To plot means of height by locality
nivi%>%                                        # Specify data frame
  group_by(Locality) %>%                         # Specify group indicator
  summarise(mean_Height = mean(`height (cm) 09/06`))%>%
  ggplot(aes(x = Locality, y = mean_Height, fill = Locality)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    x = "Locality",
    y = "Height",
    title = paste(
      "Height by Locality"
    )
  )

## To plot the Height data 

ggplot(agrifolia_H, aes(x = Locality, y = Average_diameter, fill = Species)) +
  geom_boxplot() +
  geom_jitter(shape = 1,
              color = "brown",
              position = position_jitter(0.21)) +
  theme_classic()

## ANOVA oneway
aov_H <- aov(Average_diameter ~ Locality, data = agrifolia_H)

summary(aov_H)



##Tukey analysis
TUKEY4 <- TukeyHSD(aov_H)
TUKEY4
plot(TUKEY4)
plot (TUKEY4, las=1, col="brown")



c2<-tapply(agrifolia_H$Average_diameter, agrifolia_H$Locality,mean, na.rm=TRUE)
c2
barplot(c2, col = "pink", axis.lty = 1,xlab="Locality", ylab = "Diameter")


###Calculate standard deviation

d<-tapply(agrifolia_H$Average_diameter, agrifolia_H$Locality, sd, na.rm=TRUE)

d


##creating a table with factors

randon_summary <- group_by(agrifolia_H, Locality)%>%
  summarise(mean=mean(Average_diameter, na.rm = TRUE), sd=sd(Average_diameter, na.rm = TRUE), n=n())
View(randon_summary)


##compact letter display of tukey
exp_tukey <- TukeyHSD(exp_aov <- aov(Average_diameter ~ Locality, data = agrifolia_H))
multcompLetters4(exp_aov, exp_tukey)

cld <- multcompLetters4(aov_H, TUKEY4)
print(cld)

##Table with factors and 3rd quantile
Tk <- group_by(agrifolia_H, Locality)%>%
  summarise(mean=mean(Average_diameter, na.rm = TRUE), quant=quantile(Average_diameter, probs=0.75, na.rm = TRUE)) %>%
  arrange(desc(mean))

## extracting the compact letter display and adding to the Tk table


cld<-as.data.frame.list(cld$Locality)
Tk$cld<-cld$Letters
print(Tk)

## boxplot
ggplot(agrifolia_H, aes(Locality, Average_diameter))+
  geom_boxplot()+
  labs(x="Locality", y="Height")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_text(data=Tk, aes(label=cld, x=Locality, y=quant))


##boxplot nicer
ggplot(agrifolia_H, aes(x = Locality, y = Average_diameter, fill = Locality)) +
  geom_boxplot() +
  geom_jitter(shape = 1,
              color = "mediumorchid1",
              position = position_jitter(0.21)) +
  theme_classic()+
  labs(x="Locality", y="Height")+
  geom_text(data=Tk, aes(label=cld, x=Locality, y=quant), size = 6, vjust=-10, hjust =0) 
