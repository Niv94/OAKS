###Chi square  test to see if sex and anther color or flower and anther color are related

Locality=as.factor(Locality)
`mortality march`=as.factor(`mortality march`)



##gives counts of different colors in male and female
sj=table(Locality, `mortality march`)
sj
###chi sq test
chisq=chisq.test(Locality, `mortality march`)
chisq
##Note that, Chi-square test should only be applied when the expected frequency of any cell is at least 5
# Expected counts
round(chisq$expected,2)

library(ggplot2)

#Creating plots 

hhhh=as.data.frame(sj) ##converts table to dataframe
hhhh

ggplot(hhhh, aes(fill=mortality.march, y=`Freq`, x=Locality)) + 
  geom_bar(position="stack", stat="identity")




####FISHER TEST - for small sample size

###as for the Chi-square test of independence, the observations must be independent in order for the Fisher’s exact test to be valid. 

dat = table(`Flower color`,`Anther color`)

test <- fisher.test(dat,simulate.p.value=TRUE)
test ###p-value = 0.0004998

##create data frame from count table

x <- c()
for (row in rownames(dat)) {
  for (col in colnames(dat)) {
    x <- rbind(x, matrix(rep(c(row, col), dat[row, col]), ncol = 2, byrow = TRUE))
  }
}
df <- as.data.frame(x)
colnames(df) <- c("Flower color", "Filament color")
df

## Fisher's exact test with raw data
test <- fisher.test(table(df), simulate.p.value = TRUE)

# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  df, `Flower color`, 'Filament color',
  results.subtitle = FALSE,
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.001, "< 0.001", round(test$p.value, 3)))
  +  scale_fill_manual(values = c( "orange2", "yellow1" ,"red", "orangered","greenyellow","green"))  )




