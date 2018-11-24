library(ggplot2)
df <- data.frame(
 region = c('Northeast', 'Southeast', 'Central', 'West'),
 sales = c(1200, 800, 450, 900),
 avg_margin = c(3.2, -1.4, 0.1, 2.1)
 )
ggplot(df, aes(
     x = region,
     color = region,
     fill = region,
     y = avg_margin,
     width = sales
  )) +
  geom_barmekko() +
  labs(title = "Margins by Region")
