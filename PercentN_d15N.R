
setwd("/Users/lisagarcia/Documents/Paleomidden project/Final R code/percent_delta")

library(ggplot2)
library(readr)

d15N_percentN <- read_csv("Paleo_modern_seuss.csv")

ggplot(d15N_percentN, aes(x = Averaged_percentN, 
                          y = averaged_d15N, 
                          color = Category, 
                          shape = Category)) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("Modern" = "darkorchid1", 
                                "Paleo" = "purple4")) +
  scale_shape_manual(values = c("Modern" = 16, "Paleo" = 18)) +
  
  ylab(expression({delta}^15 * N ~ '\u2030')) +
  xlab("%N") +
  
  # Custom clean theme
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),        # remove gridlines
    axis.line = element_line(color = "black", linewidth = 0.8),  # add axis lines
    axis.ticks = element_line(color = "black"),                  # keep ticks
    panel.border = element_blank()
  )
model <- lm(averaged_d15N ~ Averaged_percentN * Category, data = d15N_percentN)
summary(model)

modern_lm <- lm(averaged_d15N ~ Averaged_percentN,
                data = subset(d15N_percentN, Category == "Modern"))

paleo_lm <- lm(averaged_d15N ~ Averaged_percentN,
               data = subset(d15N_percentN, Category == "Paleo"))

summary(modern_lm)
summary(paleo_lm)
