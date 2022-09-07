###Chi square  test to see if sex and anther color or flower and anther color are related

Locality=as.factor(Locality)
`mortality march`=as.factor(`mortality march`)


##gives counts of different colors in male and female
sj=table(Locality, `mortality march`)
sj
###chi sq test
chisq.test(Locality, `mortality march`)



library(ggplot2)

#Creating plots 

hhhh=as.data.frame(sj) ##converts table to dataframe
hhhh

ggplot(hhhh, aes(fill=mortality.march, y=`Freq`, x=Locality)) + 
  geom_bar(position="stack", stat="identity")








