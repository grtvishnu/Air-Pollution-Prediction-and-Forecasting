# Visualize Accuracy
names <- c("Catboost", "Random Forest", "XGBoost", "DNN(MLP)")
data <- c(0.8453924, 1.92769, 2.089297, 4.365164)
df <- cbind(names, data)
df <- as.tibble(df)
df$data <- as.double(df$data)

ggplot(df, aes(x = reorder(names, data), y = data))+
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4)+
  xlab("Models")+
  ylab("RMSE")

str(df)

ggplot(data = df, aes(x=reorder(names, data), y=data)) +
  geom_segment( aes(xend=names, yend=0)) +
  geom_point( size=4, color="orange")+
  xlab("Models")+
  ylab("RMSE")

ggplot(df,aes(x= names, y=data))+
  geom_boxplot()

