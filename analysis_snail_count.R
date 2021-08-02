

# setup -------------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(patchwork)
library(MASS)
library(PerformanceAnalytics)
library(jtools)
source("format_snail_data.R")


# Analysis -------------------------------------------------------------------

#' # Correlation plot - target environmental variables and count data
  chart.Correlation(df_snail[, c(22, 3:4, 13:19, 20)], method="spearman", histogram=TRUE, cex = 10) # all possible variable
  chart.Correlation(df_snail[, c(22, 3:4, 20)], method="spearman", histogram=TRUE, cex = 10) # we do not need to worry about multicolinearity.

  
#' # Linear model
#'- Simple linear model
  mod01 <- lm(Count ~ scale(Depth) + scale(Velocity) + scale(coarseness), data= df_snail)
  summary(mod01)

#'- Poisson GLM
  mod02 <- glm(Count ~ scale(Depth) + scale(Velocity) + scale(coarseness), data= df_snail, family= "poisson")
  summary(mod02) # heavy overdispersion

 #'- Check the frequency of count data
  ggplot(data= df_snail, aes(Count)) + geom_histogram(binwidth = 5)

#'- Negative binomial GLM
  mod03 <-glm.nb(Count ~ scale(Depth) + scale(Velocity) + scale(coarseness), data= df_snail)
  summary(mod03) # overdispersion fixed
  
 # Plot
  effect_plot(model = mod03, pred = Depth, interval = TRUE,int.type = "confidence", int.width = .95, plot.points=TRUE, 
              data = df_snail, x.label= "Depth (cm)", y.label= "Count")
  
  effect_plot(model = mod03, pred = coarseness, interval = TRUE,int.type = "confidence", int.width = .95, plot.points=TRUE, 
              data = df_snail, x.label= "Coarseness", y.label= "Count")
 
  