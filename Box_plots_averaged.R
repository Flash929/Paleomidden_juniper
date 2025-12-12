setwd("/Users/lisagarcia/Documents/Paleomidden project/Final R code/box plots")
library(readxl)
Paleo_modern_CN <- read_csv("Paleo_modern_box.csv")
library(tidyverse)

##Carbon
ggplot(Paleo_modern_CN, aes(x=factor(Category, levels = c("Paleo", "Modern")), y=averaged_C13, fill = Category))+
  geom_boxplot(alpha=0.7, width=0.4)+
  theme_classic()+
  theme(axis.text.x=element_text(family = 'sans' ,size=20, angle = 0, hjust=0.5), 
        axis.text.y =element_text(family = 'sans', size=16, face='bold'),
        axis.title.y = element_text(family='sans', size=20, face='bold'),
        axis.title.x =element_blank(),
        legend.position = 'none')+
  scale_fill_manual(values=c('Modern'='darkgreen', 'Paleo'='limegreen'))+
  ylab(expression({delta}^13*C~'\u2030'))+
  ylim(-25,-19)
  
  

##Nitrogen
ggplot(Paleo_modern_CN, aes(x=factor(Category, levels = c("Paleo", "Modern")), y=averaged_d15N, fill = Category))+
         geom_boxplot(alpha=0.7, width=0.4)+
  theme_classic()+
theme(axis.text.x=element_text(family = 'sans' ,size=20, angle = 0, hjust=0.5), 
      axis.text.y =element_text(family = 'sans', size=16, face='bold'),
      axis.title.y = element_text(family='sans', size=20, face='bold'),
      axis.title.x =element_blank(),
      legend.position = 'none')+
  scale_fill_manual(values=c('Modern'='purple', 'Paleo'='purple4'))+
  ylab(expression({delta}^15*N~'\u2030'))
  
   