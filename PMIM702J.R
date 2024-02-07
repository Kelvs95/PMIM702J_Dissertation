########### Installing necessary packages #################

#install.packages("esc")
#install.packages("meta")
#install.packages("tidyverse")
#remotes::install_github("MathiasHarrer/dmetar")
#install.packages("webshot")
#install.packages("magick")
#install.packages(c("grid", "gridExtra"))

############### IMPORTING LIBRARIES ######################

library(tidyverse)
library(dmetar)
library(esc)
library(meta)
library(knitr)
library(webshot)
library(magick)
library(grid)
library(gridExtra)


######################## general ##################


data2 <- read_csv("dissertation_2.csv", show_col_types = FALSE)
glimpse(data2)


data2 <- data2 %>%
  mutate(logOR = log10(ODDS_RATIO)) %>%
  mutate(SElogOR = (log10(CI_2) - log10(CI_1)) / (2 * qnorm(0.975)))


############# 
# Row index 4 of the dissertation_data has a standard error of 0 due to
# no difference between the confidence intervals.
# Adjustment was made by adding a very negligible value to the standard error 
# value of 0 so as not to affect the result of the meta-analysis 

# Specify the row index where you want to add the negligible value
row_index <- 2  

# negligible value
value <- 0.000000001

# Updating the data table
data2 <- data2 %>%
  mutate(SElogOR = ifelse(row_number() == row_index, 
                          SElogOR + value, SElogOR))

############### META-ANALYSIS ######################

data2.gen <- metagen(TE = logOR,
                    seTE = SElogOR,
                    studlab = AUTHOR,
                    data = data2,
                    sm = "OR",
                    fixed = FALSE,
                    random = TRUE,
                    method.tau = "REML", #Restricted Maximum Likelihood 
                    hakn = FALSE,
                    title = "DISSERTATION")


summary(data2.gen)

############### FOREST PLOT  ######################

forest.meta(data2.gen, 
            sortvar = TE,
            prediction = TRUE, 
            print.tau2 = FALSE,
            leftlabs = c("AUTHOR", "g", "SE"))


forest.meta(data2.gen, layout = "RevMan5")

############# Funnel Plot ##################
par(mfrow=c(1,1))

# Define fill colors for contour
contour <- c(0.9, 0.95, 0.99)
col.contour <- c("gray75", "gray85", "gray95")
ld <- c("p<0.1", "p<0.05", "p<0.01")


funnel.meta(data2.gen,
            xlim = c(0.5, 2.5), contour = contour,
            col.contour = col.contour)
legend(x = 0.5, y = 0.01, 
       legend = ld, fill = col.contour)

# Add title
title("(a) - Funnel Plot (Level of Bias)")

eggers.test(data2.gen)


#### USING Trim and Fill method ############
# With all studies included
tf <- trimfill(data2.gen)
summary(tf)
eggers.test(tf)

# Contour-enhanced funnel plot (full data)
funnel.meta(tf, 
            xlim = c(0.5, 2.5), contour = contour,
            col.contour = col.contour)
legend(x = 0.5, y = 0.00, 
       legend = ld, fill = col.contour)
title("(b) - Funnel Plot (Trim & Fill Method)")

############### FOREST PLOT trim and fill  ######################

forest.meta(tf, 
            sortvar = TE,
            prediction = TRUE, 
            print.tau2 = FALSE,
            leftlabs = c("AUTHOR", "g", "SE"))


forest.meta(tf, layout = "RevMan5")

######### SUBGROUP ANALYSES ##############

############### GESTATIONAL DIABETES MELLITUS  ######################

#import dataset
gdm_data <- read_csv("gdm_data.csv")
glimpse(gdm_data)

# CALC log_odds and SE 
gdm_data <- gdm_data %>%
  mutate(logOR = log10(Odds_Ratio)) %>%
  mutate(SElogOR = (log10(CI_2) - log10(CI_1)) / (2 * qnorm(0.975)))

#META-ANALYSIS 
gdm_data.gen <- metagen(TE = logOR,
                        seTE = SElogOR,
                        studlab = Author,
                        data = gdm_data,
                        sm = "OR",
                        fixed = FALSE,
                        random = TRUE,
                        method.tau = "REML", #Restricted Maximum Likelihood 
                        hakn = FALSE,
                        title = "Gestational Diabetes Mellitus")


summary(gdm_data.gen)

#Prediction 
gdm_data.gen <- update.meta(gdm_data.gen, prediction = TRUE)

summary(gdm_data.gen)

#FOREST PLOT  

forest.meta(gdm_data.gen, 
            sortvar = TE,
            prediction = TRUE, 
            print.tau2 = FALSE,
            leftlabs = c("Author", "g", "SE"))


forest.meta(gdm_data.gen, layout = "RevMan5")

grid.text("Gestational Diabetes Mellitus", x = 0.5,
          y = unit(1, "npc") - unit(1, "lines"), just = "center",
          gp = gpar(fontsize = 14))

############### Eclampsia/Preeclampsia  ######################

eclampsia_preeclamsia_dataset <- read_csv("eclampsia_preeclamsia_dataset.csv")
glimpse(eclampsia_preeclamsia_dataset)

# CALC log_odds and SE 
eclampsia_preeclamsia_data <- eclampsia_preeclamsia_dataset %>%
  mutate(logOR = log10(odds_ratio)) %>%
  mutate(SElogOR = (log10(CI_2) - log10(CI_1)) / (2 * qnorm(0.975)))

#META-ANALYSIS 
eclampsia_preeclamsia_data.gen <- metagen(TE = logOR,
                                          seTE = SElogOR,
                                          studlab = Author,
                                          data = eclampsia_preeclamsia_data,
                                          sm = "OR",
                                          fixed = FALSE,
                                          random = TRUE,
                                          method.tau = "REML", #Restricted Maximum Likelihood 
                                          hakn = FALSE,
                                          title = "Eclampsia/Preeclampsia")


summary(eclampsia_preeclamsia_data.gen)

#Prediction 
eclampsia_preeclamsia_data.gen <- update.meta(eclampsia_preeclamsia_data.gen, 
                                              prediction = TRUE)

summary(eclampsia_preeclamsia_data.gen)

#FOREST PLOT  

forest.meta(eclampsia_preeclamsia_data.gen, 
            sortvar = TE,
            prediction = TRUE, 
            print.tau2 = FALSE,
            leftlabs = c("Author", "g", "SE"))


forest.meta(eclampsia_preeclamsia_data.gen, layout = "RevMan5")
grid.text("Eclampsia/Preeclampsia", x = 0.5,
          y = unit(1, "npc") - unit(1, "lines"), just = "center",
          gp = gpar(fontsize = 14))

############### GESTATIONAL HYPERTENSION  ######################

#import dataset
hdp_dataset <- read_csv("hdp_dataset.csv")
glimpse(hdp_dataset)

# CALC log_odds and SE 
hdp_dataset <- hdp_dataset %>%
  mutate(logOR = log10(Odds_ratio)) %>%
  mutate(SElogOR = (log10(CI_2) - log10(CI_1)) / (2 * qnorm(0.975)))

#META-ANALYSIS 
hdp_dataset.gen <- metagen(TE = logOR,
                           seTE = SElogOR,
                           studlab = Author,
                           data = hdp_dataset,
                           sm = "OR",
                           fixed = FALSE,
                           random = TRUE,
                           method.tau = "REML", #Restricted Maximum Likelihood 
                           hakn = FALSE,
                           title = "Gestational Hypertension")


summary(hdp_dataset.gen)

#Prediction 
hdp_dataset.gen <- update.meta(hdp_dataset.gen, prediction = TRUE)

summary(hdp_dataset.gen)

#FOREST PLOT  

forest.meta(hdp_dataset.gen, 
            sortvar = TE,
            prediction = TRUE, 
            print.tau2 = FALSE,
            leftlabs = c("Author", "g", "SE"))


forest.meta(hdp_dataset.gen, layout = "RevMan5")
grid.text("Gestational Hypertension", x = 0.5,
          y = unit(1, "npc") - unit(1, "lines"), just = "center",
          gp = gpar(fontsize = 14))
############### STILLBIRTH ######################

#import dataset
stillbirth_dataset <- read_csv("stillbirth_dataset.csv")
glimpse(stillbirth_dataset)

# CALC log_odds and SE 
stillbirth_dataset <- stillbirth_dataset %>%
  mutate(logOR = log10(odds_ratio)) %>%
  mutate(SElogOR = (log10(CI_2) - log10(CI_1)) / (2 * qnorm(0.975)))

#META-ANALYSIS 
stillbirth_dataset.gen <- metagen(TE = logOR,
                                  seTE = SElogOR,
                                  studlab = Author,
                                  data = stillbirth_dataset,
                                  sm = "OR",
                                  fixed = FALSE,
                                  random = TRUE,
                                  method.tau = "REML", #Restricted Maximum Likelihood 
                                  hakn = FALSE,
                                  title = "STILLBIRTH")


summary(stillbirth_dataset.gen)

#Prediction 
stillbirth_dataset.gen <- update.meta(stillbirth_dataset.gen, prediction = TRUE)

summary(stillbirth_dataset.gen)

#FOREST PLOT  

forest.meta(stillbirth_dataset.gen, 
            sortvar = TE,
            prediction = TRUE, 
            print.tau2 = FALSE,
            leftlabs = c("Author", "g", "SE"))


forest.meta(stillbirth_dataset.gen, layout = "RevMan5")

grid.text("Stillbirth", x = 0.5,
          y = unit(1, "npc") - unit(1, "lines"), just = "center",
          gp = gpar(fontsize = 14))


############# Preterm Birth ##########################
#import dataset
pretermmm <- read_csv("pretermmm.csv")
glimpse(pretermmm)

# CALC log_odds and SE 
pretermmm <- pretermmm %>%
  mutate(logOR = log10(Odds_Ratio)) %>%
  mutate(SElogOR = (log10(CI_2) - log10(CI_1)) / (2 * qnorm(0.975)))

# Specify the row index where you want to add the negligible value
row_index <- 1  

# negligible value
value <- 0.000000001

# Updating the data table
pretermmm <- pretermmm %>%
  mutate(SElogOR = ifelse(row_number() == row_index, 
                          SElogOR + value, SElogOR))

#META-ANALYSIS 
pretermmm.gen <- metagen(TE = logOR,
                         seTE = SElogOR,
                         studlab = Author,
                         data = pretermmm,
                         sm = "OR",
                         fixed = FALSE,
                         random = TRUE,
                         method.tau = "REML", #Restricted Maximum Likelihood 
                         hakn = FALSE,
                         title = "PRETERM BIRTH")


summary(pretermmm.gen)

#Prediction 
pretermmm.gen <- update.meta(pretermmm.gen, prediction = TRUE)

summary(pretermmm.gen)

#FOREST PLOT  

forest.meta(pretermmm.gen, 
            sortvar = TE,
            prediction = TRUE, 
            print.tau2 = FALSE,
            leftlabs = c("Author", "g", "SE"))


forest.meta(pretermmm.gen, layout = "RevMan5")

grid.text("Preterm birth", x = 0.5,
          y = unit(1, "npc") - unit(1, "lines"), just = "center",
          gp = gpar(fontsize = 14))



####### Pie chart ##############

countries <- read_csv("countries.csv")
View(countries)

# Create a pie chart with labels using ggplot2
ggplot(countries, aes(x = "", y = no_of_studies, fill = country)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(aes(label = no_of_studies), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "                            Distribution of studies by Country", 
       fill = "Country") +
  theme_minimal()



