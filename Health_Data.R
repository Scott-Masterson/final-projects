setwd("~/Documents/QTM150-151")
library(tidyverse)
library(Hmisc)   
library(SASxport)   

demo <- read.xport ("DEMO_J.XPT.txt")

mental <- read.xport ("DPQ_J.XPT.txt")

## Is there a significant difference in the rates of depression among people of different ethnicities? 

# get a feel for data
  
summary(demo) # using RIDRETH3
summary(mental) # using DPQ020

demo$RIDRETH3
mental$DPQ020

demo_mental <- inner_join(mental, demo, by = "SEQN")

demo_mental


# change names of observations within the variables

eth <- as.numeric(demo_mental$RIDRETH3)

eth1 <- recode(eth, "1" = "Mexican American", 
       "2" = "Other Hispanic", 
       "3" = "Non-Hispanic White",
       "4" = "Non-Hispanic Black",
       "6" = "Non-Hispanic Asian",
       "7" = "Other Race - Including Multi-Racial") 

eth1


ment <- as.numeric(demo_mental$DPQ020)

ment1 <- recode(ment, "0" = "Not at all", 
                   "1" = "Several days", 
                   "2" = "More than half the days",
                   "3" = "Nearly every day",
                   "7" = "Refused",
                   "9" = "Don't Know")


ment1

joined <- data.frame(eth1, ment1)
joined

# ggplot by ment1, faceted by eth1

ment_eth_plot <- joined %>%
  ggplot(aes(x = ment1, fill = ment1)) +
  geom_histogram(stat = "count") +
  facet_wrap(~eth1) +
  labs(title = "Rates of Depression by Ethnicity") +
  xlab("Amount of days Depressed per Week") +
  ylab("Count") +
  labs(fill = 'Days Depressed')

ment_eth_plot

# the ggplot suggests that African Americans are less depressed than Caucasians  



