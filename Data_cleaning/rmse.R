names <- c("CatBoost","RandomForest","Deeplearning(MLP)","XGBoost")
values <- c(0.892051368899712,0.941187,1.091259,1.156075)

new <- cbind(names,values)
new <- as.data.frame(new)
new$values <- as.numeric(new$values)
library(tidyverse)

ggplot(new,aes(reorder(names, values), values))+
  geom_bar(stat = "identity", fill = '#0099CC')+
  ggtitle("RMSE of Models", subtitle = "lower is better")
