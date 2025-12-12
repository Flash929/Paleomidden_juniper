setwd("/Users/lisagarcia/Documents/Paleomidden project/Final R code/Paleo")
# load in all CSV files
library(readr)
New_Paleojuniper_temp <- read_csv("new_averaged_paleo.csv")
library(ggplot2)


# carbon plots

p1 <- ggplot(New_Paleojuniper_temp, aes(x = Year_dated_1, y = d13C))+
  geom_point(colour="darkgreen", size=7, shape=18)+### create a scatter plot, you can modify shape, size and color
  scale_x_reverse(limits= c(35000, 0), breaks= c(35000, 30000, 25000, 20000, 15000, 10000, 5000, 0))+
  theme_classic()+ ### Modify the overall look of the graph
  theme(axis.text=element_text(family = 'sans', size = 14), 
        axis.title = element_text(family='sans', size=16))+
  xlab("Years before present")+ ### Modify the x axis label
 
  ylab(expression({delta}^13*C~'\u2030'))+ ### Modify the y axis label
  #geom_smooth(method = lm, se = TRUE, alpha=0.5, colour="darkseagreen3", fullrange= TRUE)+
ylim(-30,-15)

p1

#PValue
linear_model <- lm(d13C~Year_dated_1,data=New_Paleojuniper_temp)
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


p2 <- ggplot(New_Paleojuniper_temp, aes(x = Year_dated_1, y = d15N))+
  geom_point(colour="purple3", size=7, shape=20)+### create a scatter plot, you can modify shape, size and color
  scale_x_reverse(limits= c(35000, 0), breaks= c(35000, 30000, 25000, 20000, 15000, 10000, 5000, 0))+
  theme_classic()+ ### Modify the overall look of the graph
  theme(axis.text=element_text(family = 'sans', size = 14), 
        axis.title = element_text(family='sans', size=16))+
  xlab("Years before present")+ 
   ### Modify the x axis label
  ylab(expression({delta}^15*N~'\u2030'))+ ### Modify the y axis label
  #geom_smooth(method = lm, se = TRUE, alpha=0.5, colour="darkseagreen3", fullrange= TRUE)+
  ylim(-10,15) ###linear regression fit and confidence bands

p2

#PValue
linear_model <- lm(d15N~Year_dated_1,data=New_Paleojuniper_temp)
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


############# Woodrat mean_size Temp################
# carbon plots

p1 <- ggplot(New_Paleojuniper_temp, aes(x = max_temp_mean_size, y = d13C))+
  geom_point(colour="darkgreen", size=5, shape=18)+### create a scatter plot, you can modify shape, size and color
  theme_classic()+ ### Modify the overall look of the graph
  theme(axis.text=element_text(family = 'sans', size = 14), 
        axis.title = element_text(family='sans', size=16))+
  xlab("Max Temperature \u00b0C")+ ### Modify the x axis label
  
  ylab(expression({delta}^13*C~'\u2030'))+ ### Modify the y axis label
  #geom_smooth(method = lm, se = TRUE, alpha=0.5, colour="darkseagreen3", fullrange= TRUE)+
  ylim(-30,-15)

p1

#PValue
linear_model <- lm(d13C~max_temp_mean_size,data=New_Paleojuniper_temp)
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


p2 <- ggplot(New_Paleojuniper_temp, aes(x = max_temp_mean_size, y = d15N))+
  geom_point(colour="purple3", size=5, shape=20)+### create a scatter plot, you can modify shape, size and color
  theme_classic()+ ### Modify the overall look of the graph
  theme(axis.text=element_text(family = 'sans', size = 14), 
        axis.title = element_text(family='sans', size=16))+
  xlab("Max Temperature \u00b0C")+ ### Modify the x axis label
  ylab(expression({delta}^15*N~'\u2030'))+ ### Modify the y axis label
  #geom_smooth(method = lm, se = TRUE, alpha=0.5, colour="darkseagreen3", fullrange= TRUE)+
  ylim(-10,15) ###linear regression fit and confidence bands

p2

#PValue
linear_model <- lm(d15N~max_temp_mean_size,data=New_Paleojuniper_temp)
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

############# Woodrat max_size Temp################
# temperature with time

ggplot(New_Paleojuniper_temp, aes(x = Year_dated_1, y = max_temp_max_size))+
  geom_point(colour="grey30", size=7, shape=18)+### create a scatter plot, you can modify shape, size and color
  scale_x_reverse(limits= c(35000, 0), breaks= c(35000, 30000, 25000, 20000, 15000, 10000, 5000, 0))+
  theme_classic()+ ### Modify the overall look of the graph
  theme(axis.text=element_text(family = 'sans', size = 14), 
        axis.title = element_text(family='sans', size=16))+
  xlab("Years before present")+ ### Modify the x axis label
  ylab("Max Temperature \u00b0C")
  ### Modify the y axis label
  #geom_smooth(method = lm, se = TRUE, alpha=0.5, colour="darkseagreen3", fullrange= TRUE)+
  



#PValue
linear_model <- lm(d13C~Year_dated_1,data=New_Paleojuniper_temp)
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


# carbon plots

p1 <- ggplot(New_Paleojuniper_temp, aes(x = max_temp_max_size, y = d13C))+
  geom_point(colour="darkgreen", size=5, shape=18)+### create a scatter plot, you can modify shape, size and color
  theme_classic()+ ### Modify the overall look of the graph
  theme(axis.text=element_text(family = 'sans', size = 14), 
        axis.title = element_text(family='sans', size=16))+
  xlab("Max Temperature \u00b0C")+ ### Modify the x axis label
  
  ylab(expression({delta}^13*C~'\u2030'))+ ### Modify the y axis label
  #geom_smooth(method = lm, se = TRUE, alpha=0.5, colour="darkseagreen3", fullrange= TRUE)+
  ylim(-30,-15)

p1

#PValue
linear_model <- lm(d13C~max_temp_max_size,data=New_Paleojuniper_temp)
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


p2 <- ggplot(New_Paleojuniper_temp, aes(x = max_temp_max_size, y = d15N))+
  geom_point(colour="purple3", size=5, shape=20)+### create a scatter plot, you can modify shape, size and color
  #scale_x_reverse(limits= c(35000, 0), breaks= c(35000, 30000, 25000, 20000, 15000, 10000, 5000, 0))+
  theme_classic()+ ### Modify the overall look of the graph
  theme(axis.text=element_text(family = 'sans', size = 14), 
        axis.title = element_text(family='sans', size=16))+
  xlab("Years before present")+ 
  ### Modify the x axis label
  ylab(expression({delta}^15*N~'\u2030'))+ ### Modify the y axis label
  #geom_smooth(method = lm, se = TRUE, alpha=0.5, colour="darkseagreen3", fullrange= TRUE)+
  ylim(-10,15) ###linear regression fit and confidence bands

p2

#PValue
linear_model <- lm(d15N~max_temp_max_size,data=New_Paleojuniper_temp)
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

## nitrogen plot differences----


p2 <- ggplot(Paleo_differences, aes(x = Year_dated_1, y = differences))+
  geom_point(colour="purple3", size=5, shape=20)+### create a scatter plot, you can modify shape, size and color
  scale_x_reverse(limits= c(35000, 0), breaks= c(35000, 30000, 25000, 20000, 15000, 10000, 5000, 0))+
  theme_classic()+ ### Modify the overall look of the graph
  theme(axis.text=element_text(family = 'sans', size = 14), 
        axis.title = element_text(family='sans', size=16))+
  xlab("Years before present")+ 
  ### Modify the x axis label
  ylab(expression({delta}^15*N~'\u2030'))+ ### Modify the y axis label
  #geom_smooth(method = lm, se = TRUE, alpha=0.5, colour="darkseagreen3", fullrange= TRUE)+
  ylim(-10,15) ###linear regression fit and confidence bands

p2

#PValue
linear_model <- lm(differences~Year_dated_1,data=Paleo_differences)
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
