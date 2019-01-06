library(tidyverse)
names(diamonds)
dim(diamonds)
diamonds_df <- as.data.frame(diamonds)

sort(diamonds_df[,"price"],decreasing = TRUE)[1:10]
arrange(select(diamonds,price),price)

diamonds_exp = diamonds_df[diamonds_df["price"]==18823,]

diamonds_prem = diamonds_df[diamonds_df["cut"]=="Premium",]
length(diamonds_prem)



library(ggplot2)
ggplot(data = diamonds) + geom_point(mapping = aes(x = carat, y = price))
ggplot(data = diamonds) + geom_smooth(mapping = aes(x = carat, y = price,color = color))
ggplot(data = diamonds) + geom_smooth(mapping = aes(x = carat, y = price,color = clarity))
ggplot(data = diamonds) + geom_smooth(mapping = aes(x = carat, y = price, color = cut))
