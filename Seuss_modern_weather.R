setwd("/Users/lisagarcia/Documents/Paleomidden project/Final R code/historical_modern")
# load in all CSV files
library(readr)
averaged_modern_seuss<- read_csv("Averaged_Seuss_modern.csv")

library(ggplot2)

######### Max Temp ###################

# carbon plot----
p1 <- ggplot(averaged_modern_seuss, aes(x = avg_maxtemp, y = averaged_C13)) +
  geom_point(colour="darkgreen", size=5, shape=18)+### create a scatter plot, you can modify shape, size and color
  #geom_smooth(method = lm, se = TRUE, alpha=0.5,fullrange= TRUE)+
  theme_classic()+ ### Modify the overall look of the graph
  theme(axis.text=element_text(family = 'sans', size = 14), 
        axis.title = element_text(family='sans', size=16))+
  xlab("Max Temperature \u00b0C")+ ### Modify the x axis label
  ylab(expression({delta}^13*C~'\u2030'))+
  geom_smooth(method = lm, se = FALSE, alpha=0.5, colour="darkgreen", fullrange= TRUE)+
  ylim(-25,-19)
  ###linear regression fit and confidence bands

p1

# p value carbon
linear_model <- lm(averaged_C13~avg_maxtemp,data=averaged_modern_seuss)
model_summary <- summary(linear_model)
model_summary
#Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]
r_squared <- summary(linear_model)$r.squared
pval <- summary(linear_model)$coefficients[,4]
print(pval)
df_model <- model_summary$df[1]  # Degrees of freedom for the model (number of parameters)
df_residual <- model_summary$df[2]  # Degrees of freedom for residuals (number of data points - parameters)
df_model ##This is the model parameters
df_residual ###this is reported degrees of freedom (number of residuals-model df) --> 19-2
# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)



# Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]

# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)

## nitrogen plot----

p2 <- ggplot(averaged_modern_seuss, aes(x = avg_maxtemp, y = averaged_d15N))+
  geom_point(colour="purple3", size=5, shape=20)+### create a scatter plot, you can modify shape, size and color
  theme_classic()+ ### Modify the overall look of the graph
  theme(axis.text=element_text(family = 'sans', size = 14), 
        axis.title = element_text(family='sans', size=16))+
  xlab("Max Temperature \u00b0C")+ ### Modify the x axis label
  ylab(expression({delta}^15*N~'\u2030'))+ ### Modify the y axis label
  geom_smooth(method = lm, se = FALSE, alpha=0.5, colour="purple3", fullrange= TRUE)+
  ylim(-10,15)
  ###linear regression fit and confidence bands

p2

print(p2)

# p value nitrogen
linear_model <- lm(averaged_d15N~avg_maxtemp,data=averaged_modern_seuss)
model_summary <- summary(linear_model)
model_summary
#Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]
r_squared <- summary(linear_model)$r.squared
pval <- summary(linear_model)$coefficients[,4]
print(pval)
df_model <- model_summary$df[1]  # Degrees of freedom for the model (number of parameters)
df_residual <- model_summary$df[2]  # Degrees of freedom for residuals (number of data points - parameters)
df_model ##This is the model parameters
df_residual ###this is reported degrees of freedom (number of residuals-model df) --> 19-2
# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)



# Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]

# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)


################ Min Temperature ############

# carbon plot----
p1 <- ggplot(averaged_modern_seuss, aes(x = Avg_mintemp, y = averaged_C13))+
  geom_point(colour="darkgreen", size=5, shape=18)+### create a scatter plot, you can modify shape, size and color
  theme_classic()+ ### Modify the overall look of the graph
  theme(axis.text=element_text(family = 'sans', size = 14), 
        axis.title = element_text(family='sans', size=16))+
  xlab("Min Temperature \u00b0C")+ ### Modify the x axis label
  ylab(expression({delta}^13*C~'\u2030'))+
  #scale_y_(limits= c(-22,-28))+### Modify the y axis label
  #geom_smooth(method = lm, se = TRUE, alpha=0.5, colour="darkseagreen3", fullrange= TRUE)+
  ylim(-30,-15)###linear regression fit and confidence bands

p1

# p value carbon
linear_model <- lm(averaged_C13~Avg_mintemp,data=averaged_modern_seuss)
model_summary <- summary(linear_model)
model_summary
#Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]
r_squared <- summary(linear_model)$r.squared
pval <- summary(linear_model)$coefficients[,4]
print(pval)
df_model <- model_summary$df[1]  # Degrees of freedom for the model (number of parameters)
df_residual <- model_summary$df[2]  # Degrees of freedom for residuals (number of data points - parameters)
df_model ##This is the model parameters
df_residual ###this is reported degrees of freedom (number of residuals-model df) --> 19-2
# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)



# Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]

# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)

## min temp nitrogen plot----

p2 <- ggplot(averaged_modern_seuss, aes(x = Avg_mintemp, y = averaged_d15N))+
  geom_point(colour="purple3", size=5, shape=20)+### create a scatter plot, you can modify shape, size and color
  theme_classic()+ ### Modify the overall look of the graph
  theme(axis.text=element_text(family = 'sans', size = 14), 
        axis.title = element_text(family='sans', size=16))+
  xlab("Min Temperature \u00b0C")+ ### Modify the x axis label
  ylab(expression({delta}^15*N~'\u2030'))+ ### Modify the y axis label
  geom_smooth(method = lm, se = FALSE, alpha=0.5, colour="purple3", fullrange= TRUE)+
  ylim(-10,15)

p2

# p value nitrogen
linear_model <- lm(averaged_d15N~Avg_mintemp,data=averaged_modern_seuss)
model_summary <- summary(linear_model)
model_summary
#Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]
r_squared <- summary(linear_model)$r.squared
pval <- summary(linear_model)$coefficients[,4]
print(pval)
df_model <- model_summary$df[1]  # Degrees of freedom for the model (number of parameters)
df_residual <- model_summary$df[2]  # Degrees of freedom for residuals (number of data points - parameters)
df_model ##This is the model parameters
df_residual ###this is reported degrees of freedom (number of residuals-model df) --> 19-2
# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)



# Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]

# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)

################ Mean Temperature ############

# carbon plot----
p1 <- ggplot(averaged_modern_seuss, aes(x = avg_meantemp, y = averaged_C13))+
  geom_point(colour="darkgreen", size=5, shape=18)+### create a scatter plot, you can modify shape, size and color
  theme_classic()+ ### Modify the overall look of the graph
  theme(axis.text=element_text(family = 'sans', size = 14), 
        axis.title = element_text(family='sans', size=16))+
  xlab("Mean Annual Temperature \u00b0C")+ ### Modify the x axis label
  ylab(expression({delta}^13*C~'\u2030'))+
  #scale_y_(limits= c(-22,-28))+### Modify the y axis label
  geom_smooth(method = lm, se = FALSE, alpha=0.5, colour="darkgreen", fullrange= TRUE)+
  ylim(-25,-19)###linear regression fit and confidence bands

p1

# p value carbon
linear_model <- lm(averaged_C13~avg_meantemp,data=averaged_modern_seuss)
model_summary <- summary(linear_model)
model_summary
#Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]
r_squared <- summary(linear_model)$r.squared
pval <- summary(linear_model)$coefficients[,4]
print(pval)
df_model <- model_summary$df[1]  # Degrees of freedom for the model (number of parameters)
df_residual <- model_summary$df[2]  # Degrees of freedom for residuals (number of data points - parameters)
df_model ##This is the model parameters
df_residual ###this is reported degrees of freedom (number of residuals-model df) --> 19-2
# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)



# Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]

# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)

## nitrogen plot----

p2 <- ggplot(averaged_modern_seuss, aes(x = avg_meantemp, y = averaged_d15N))+
  geom_point(colour="purple3", size=5, shape=20)+### create a scatter plot, you can modify shape, size and color
  theme_classic()+ ### Modify the overall look of the graph
  theme(axis.text=element_text(family = 'sans', size = 14), 
        axis.title = element_text(family='sans', size=16))+
  xlab("Mean Annual Temperature \u00b0C")+ ### Modify the x axis label
  ylab(expression({delta}^15*N~'\u2030'))+ ### Modify the y axis label
  geom_smooth(method = lm, se = FALSE, alpha=0.5, colour="purple3", fullrange= TRUE)+
  ylim(-10,15)

p2

# p value nitrogen
linear_model <- lm(averaged_d15N~avg_meantemp,data=averaged_modern_seuss)
model_summary <- summary(linear_model)
model_summary
#Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]
r_squared <- summary(linear_model)$r.squared
pval <- summary(linear_model)$coefficients[,4]
print(pval)
df_model <- model_summary$df[1]  # Degrees of freedom for the model (number of parameters)
df_residual <- model_summary$df[2]  # Degrees of freedom for residuals (number of data points - parameters)
df_model ##This is the model parameters
df_residual ###this is reported degrees of freedom (number of residuals-model df) --> 19-2
# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)



# Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]

# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)

####### Precipitation ###########

# carbon plot----
p1 <- ggplot(averaged_modern_seuss, aes(x = Avg_ppt, y = averaged_C13))+
  geom_point(colour="darkgreen", size=5, shape=18)+### create a scatter plot, you can modify shape, size and color
  theme_classic()+ ### Modify the overall look of the graph
  theme(axis.text=element_text(family = 'sans', size = 14), 
        axis.title = element_text(family='sans', size=16))+
  xlab("Mean Annual Precipitation (mm)")+ ### Modify the x axis label
  ylab(expression({delta}^13*C~'\u2030'))+
  #scale_y_(limits= c(-22,-28))+### Modify the y axis label
 geom_smooth(method = lm, se = FALSE, alpha=0.5, colour="darkgreen", fullrange= TRUE)+
  ylim(-25,-19)###linear regression fit and confidence bands

p1

#PValue
linear_model <- lm(averaged_C13~Avg_ppt,data=averaged_modern_seuss)
model_summary <- summary(linear_model)
model_summary
#Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]
r_squared <- summary(linear_model)$r.squared
pval <- summary(linear_model)$coefficients[,4]
print(pval)
df_model <- model_summary$df[1]  # Degrees of freedom for the model (number of parameters)
df_residual <- model_summary$df[2]  # Degrees of freedom for residuals (number of data points - parameters)
df_model ##This is the model parameters
df_residual ###this is reported degrees of freedom (number of residuals-model df) --> 19-2
# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)



# Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]

# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)


## nitrogen plot----

p2 <- ggplot(averaged_modern_seuss, aes(x = Avg_ppt, y = averaged_d15N))+
  geom_point(colour="purple3", size=5, shape=20)+### create a scatter plot, you can modify shape, size and color
  theme_classic()+ ### Modify the overall look of the graph
  theme(axis.text=element_text(family = 'sans', size = 14), 
        axis.title = element_text(family='sans', size=16))+
  xlab("Mean Annual Precipitation (mm)")+ ### Modify the x axis label
  ylab(expression({delta}^15*N~'\u2030'))+ ### Modify the y axis label
  geom_smooth(method = lm, se = FALSE, alpha=0.5, colour="purple3", fullrange= TRUE)+
  ylim(-10,15)

p2

#pvalue
linear_model <- lm(averaged_d15N~Avg_ppt,data=averaged_modern_seuss)
model_summary <- summary(linear_model)
model_summary
#Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]
r_squared <- summary(linear_model)$r.squared
pval <- summary(linear_model)$coefficients[,4]
print(pval)
df_model <- model_summary$df[1]  # Degrees of freedom for the model (number of parameters)
df_residual <- model_summary$df[2]  # Degrees of freedom for residuals (number of data points - parameters)
df_model ##This is the model parameters
df_residual ###this is reported degrees of freedom (number of residuals-model df) --> 19-2
# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)



# Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]

# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)

############################## SPEI ##############################
#### carbon plot----
p1 <- ggplot(averaged_modern_seuss, aes(x = SPEI, y = averaged_C13)) +
  geom_point(colour="darkgreen", size=5, shape=18)+### create a scatter plot, you can modify shape, size and color
  #geom_smooth(method = lm, se = TRUE, alpha=0.5,fullrange= TRUE)+
  theme_classic()+ ### Modify the overall look of the graph
  theme(axis.text=element_text(family = 'sans', size = 14), 
        axis.title = element_text(family='sans', size=16))+
  xlab("SPEI")+ ### Modify the x axis label
  ylab(expression({delta}^13*C~'\u2030'))+
  #scale_y_(limits= c(-22,-28))+### Modify the y axis label
  ylim(-30,-15)###linear regression fit and confidence bands

p1

# p value carbon
linear_model <- lm(averaged_C13~SPEI,data=averaged_modern_seuss)
model_summary <- summary(linear_model)
model_summary
#Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]
r_squared <- summary(linear_model)$r.squared
pval <- summary(linear_model)$coefficients[,4]
print(pval)
df_model <- model_summary$df[1]  # Degrees of freedom for the model (number of parameters)
df_residual <- model_summary$df[2]  # Degrees of freedom for residuals (number of data points - parameters)
df_model ##This is the model parameters
df_residual ###this is reported degrees of freedom (number of residuals-model df) --> 19-2
# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)



# Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]

# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)

## nitrogen plot---- #############

p2 <- ggplot(averaged_modern_seuss, aes(x = SPEI, y = averaged_d15N))+
  geom_point(colour="purple3", size=5, shape=20)+### create a scatter plot, you can modify shape, size and color
  theme_classic()+ ### Modify the overall look of the graph
  theme(axis.text=element_text(family = 'sans', size = 14), 
        axis.title = element_text(family='sans', size=16))+
  xlab("SPEI")+ ### Modify the x axis label
  ylab(expression({delta}^15*N~'\u2030'))+ ### Modify the y axis label
  #geom_smooth(method = lm, se = TRUE, alpha=0.5, colour="darkseagreen3", fullrange= TRUE)+
  ylim(-10,15)

p2

#pvalue
linear_model <- lm(averaged_d15N~SPEI,data=averaged_modern_seuss)
model_summary <- summary(linear_model)
model_summary
#Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]
r_squared <- summary(linear_model)$r.squared
pval <- summary(linear_model)$coefficients[,4]
print(pval)
df_model <- model_summary$df[1]  # Degrees of freedom for the model (number of parameters)
df_residual <- model_summary$df[2]  # Degrees of freedom for residuals (number of data points - parameters)
df_model ##This is the model parameters
df_residual ###this is reported degrees of freedom (number of residuals-model df) --> 19-2
# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)



# Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]

# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)

############## aridity ########################
# carbon plot----
p1 <- ggplot(averaged_modern_seuss, aes(x = aridity, y = averaged_C13)) +
  geom_point(colour="darkgreen", size=5, shape=18)+### create a scatter plot, you can modify shape, size and color
  #geom_smooth(method = lm, se = TRUE, alpha=0.5,fullrange= TRUE)+
  theme_classic()+ ### Modify the overall look of the graph
  theme(axis.text=element_text(family = 'sans', size = 14), 
        axis.title = element_text(family='sans', size=16))+
  xlab("Aridity")+ ### Modify the x axis label
  ylab(expression({delta}^13*C~'\u2030'))+
  #scale_y_(limits= c(-22,-28))+### Modify the y axis label
  ylim(-30,-15)###linear regression fit and confidence bands

p1

# p value carbon
linear_model <- lm(averaged_C13~aridity,data=averaged_modern_seuss)
model_summary <- summary(linear_model)
model_summary
#Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]
r_squared <- summary(linear_model)$r.squared
pval <- summary(linear_model)$coefficients[,4]
print(pval)
df_model <- model_summary$df[1]  # Degrees of freedom for the model (number of parameters)
df_residual <- model_summary$df[2]  # Degrees of freedom for residuals (number of data points - parameters)
df_model ##This is the model parameters
df_residual ###this is reported degrees of freedom (number of residuals-model df) --> 19-2
# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)



# Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]

# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)

## nitrogen plot---- #############

p2 <- ggplot(averaged_modern_seuss, aes(x = aridity, y = averaged_d15N))+
  geom_point(colour="purple3", size=5, shape=20)+### create a scatter plot, you can modify shape, size and color
  theme_classic()+ ### Modify the overall look of the graph
  theme(axis.text=element_text(family = 'sans', size = 14), 
        axis.title = element_text(family='sans', size=16))+
  xlab("Aridity")+ ### Modify the x axis label
  ylab(expression({delta}^15*N~'\u2030'))+ ### Modify the y axis label
  #geom_smooth(method = lm, se = TRUE, alpha=0.5, colour="darkseagreen3", fullrange= TRUE)+
  ylim(-10,15)

p2

#pvalue
linear_model <- lm(averaged_d15N~aridity,data=averaged_modern_seuss)
model_summary <- summary(linear_model)
model_summary
#Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]
r_squared <- summary(linear_model)$r.squared
pval <- summary(linear_model)$coefficients[,4]
print(pval)
df_model <- model_summary$df[1]  # Degrees of freedom for the model (number of parameters)
df_residual <- model_summary$df[2]  # Degrees of freedom for residuals (number of data points - parameters)
df_model ##This is the model parameters
df_residual ###this is reported degrees of freedom (number of residuals-model df) --> 19-2
# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)



# Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]

# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)

############## aridity_2 ########################
# carbon plot----
p1 <- ggplot(averaged_modern_seuss, aes(x = aridity_2, y = averaged_C13)) +
  geom_point(colour="darkgreen", size=5, shape=18)+### create a scatter plot, you can modify shape, size and color
  #geom_smooth(method = lm, se = TRUE, alpha=0.5,fullrange= TRUE)+
  theme_classic()+ ### Modify the overall look of the graph
  theme(axis.text=element_text(family = 'sans', size = 14), 
        axis.title = element_text(family='sans', size=16))+
  xlab("Aridity")+ ### Modify the x axis label
  ylab(expression({delta}^13*C~'\u2030'))+
  geom_smooth(method = lm, se = FALSE, alpha=0.5, colour="darkgreen", fullrange= TRUE)+
  #scale_y_(limits= c(-22,-28))+### Modify the y axis label
  ylim(-25,-19)###linear regression fit and confidence bands

p1

# p value carbon
linear_model <- lm(averaged_C13~aridity_2,data=averaged_modern_seuss)
model_summary <- summary(linear_model)
model_summary
#Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]
r_squared <- summary(linear_model)$r.squared
pval <- summary(linear_model)$coefficients[,4]
print(pval)
df_model <- model_summary$df[1]  # Degrees of freedom for the model (number of parameters)
df_residual <- model_summary$df[2]  # Degrees of freedom for residuals (number of data points - parameters)
df_model ##This is the model parameters
df_residual ###this is reported degrees of freedom (number of residuals-model df) --> 19-2
# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)



# Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]

# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)

## nitrogen plot---- #############

p2 <- ggplot(averaged_modern_seuss, aes(x = aridity_2, y = averaged_d15N))+
  geom_point(colour="purple3", size=5, shape=20)+### create a scatter plot, you can modify shape, size and color
  theme_classic()+ ### Modify the overall look of the graph
  theme(axis.text=element_text(family = 'sans', size = 14), 
        axis.title = element_text(family='sans', size=16))+
  xlab("Aridity")+ ### Modify the x axis label
  ylab(expression({delta}^15*N~'\u2030'))+ ### Modify the y axis label
  geom_smooth(method = lm, se = FALSE, alpha=0.5, colour="purple3", fullrange= TRUE)+
  ylim(-10,15)

p2

#pvalue
linear_model <- lm(averaged_d15N~aridity_2,data=averaged_modern_seuss)
model_summary <- summary(linear_model)
model_summary
#Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]
r_squared <- summary(linear_model)$r.squared
pval <- summary(linear_model)$coefficients[,4]
print(pval)
df_model <- model_summary$df[1]  # Degrees of freedom for the model (number of parameters)
df_residual <- model_summary$df[2]  # Degrees of freedom for residuals (number of data points - parameters)
df_model ##This is the model parameters
df_residual ###this is reported degrees of freedom (number of residuals-model df) --> 19-2
# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)



# Extract the coefficients (intercept and slope)
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]

# Print the linear equation
equation <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x", sep = "")
print(equation)
