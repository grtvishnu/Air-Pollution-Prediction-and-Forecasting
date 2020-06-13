library(tidyverse)
library(caret)


# read data -----------------------------------------------------------------------------------

kozhi <- read_csv("kozhikodu.csv")
kollam <- read_csv("kollam.csv")
kochi <- read_csv("kochi.csv")
kannur <- read_csv("kannur.csv")

head(kozhi)

kozhi<- kozhi[ ,-1]


org_kozhi<- kozhi %>% 
  spread(parameter,value)

rm(kozhi)
