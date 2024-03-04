################################################################################
##           Scriptfile for the Bachelor thesis of Philipp Shaha:             ##       
##           overweight and the consumption of medical services               ##
################################################################################

################################################################################
#                 Preparatory work to obtain the final dataset                 #
################################################################################

# Load the necessary packages:
library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(haven)
library(labelled)
library(data.table)
library(stringr)
library(estimatr)
library(car)
library(xtable)
library(stargazer)
library(fixest)
library(ivreg)
library(fastDummies)
library(texreg)
library(ggdag)
library(dagitty)
library(ggtext)
library(pxR)

# Set working directory:
setwd("C:/Users/phili/OneDrive/Desktop/Bachelor thesis/swissubase_932_13_0/Data_STATA/Data_STATA/SHP-Data-W1-W23-STATA")

# Import SHP dataset from the years between 2004 and 2019:
shp04_personal <- read_dta(file = 'W6_2004/shp04_p_user.dta')
shp04_household <- read_dta(file = 'W6_2004/shp04_h_user.dta')
shp05_personal <- read_dta(file = 'W7_2005/shp05_p_user.dta')
shp05_household <- read_dta(file = 'W7_2005/shp05_h_user.dta')
shp06_personal <- read_dta(file = 'W8_2006/shp06_p_user.dta')
shp06_household <- read_dta(file = 'W8_2006/shp06_h_user.dta')
shp07_personal <- read_dta(file = 'W9_2007/shp07_p_user.dta')
shp07_household <- read_dta(file = 'W9_2007/shp07_h_user.dta')
shp08_personal <- read_dta(file = 'W10_2008/shp08_p_user.dta')
shp08_household <- read_dta(file = 'W10_2008/shp08_h_user.dta')
shp09_personal <- read_dta(file = 'W11_2009/shp09_p_user.dta')
shp09_household <- read_dta(file = 'W11_2009/shp09_h_user.dta')
shp10_personal <- read_dta(file = 'W12_2010/shp10_p_user.dta')
shp10_household <- read_dta(file = 'W12_2010/shp10_h_user.dta')
shp11_personal <- read_dta(file = 'W13_2011/shp11_p_user.dta')
shp11_household <- read_dta(file = 'W13_2011/shp11_h_user.dta')
shp12_personal <- read_dta(file = 'W14_2012/shp12_p_user.dta')
shp12_household <- read_dta(file = 'W14_2012/shp12_h_user.dta')
shp13_personal <- read_dta(file = 'W15_2013/shp13_p_user.dta')
shp13_household <- read_dta(file = 'W15_2013/shp13_h_user.dta')
shp14_personal <- read_dta(file = 'W16_2014/shp14_p_user.dta')
shp14_household <- read_dta(file = 'W16_2014/shp14_h_user.dta')
shp15_personal <- read_dta(file = 'W17_2015/shp15_p_user.dta')
shp15_household <- read_dta(file = 'W17_2015/shp15_h_user.dta')
shp16_personal <- read_dta(file = 'W18_2016/shp16_p_user.dta')
shp16_household <- read_dta(file = 'W18_2016/shp16_h_user.dta')
shp17_personal <- read_dta(file = 'W19_2017/shp17_p_user.dta')
shp17_household <- read_dta(file = 'W19_2017/shp17_h_user.dta')
shp18_personal <- read_dta(file = 'W20_2018/shp18_p_user.dta')
shp18_household <- read_dta(file = 'W20_2018/shp18_h_user.dta')
shp19_personal <- read_dta(file = 'W21_2019/shp19_p_user.dta')
shp19_household <- read_dta(file = 'W21_2019/shp19_h_user.dta')

# Change working directory to read master dataset
setwd("C:/Users/phili/OneDrive/Desktop/Bachelor thesis/swissubase_932_13_0/Data_STATA/Data_STATA/SHP-Data-WA-STATA")
shp_master_individual <- read_dta(file = "shp_mp.dta")
shp_master_household <- read_dta(file = "shp_mh.dta")

## Obtain information on the BMI of relatives and merge all datasets: 
years <- list("04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19")

for (year in years) {
  # Load yearly dataset
  yearly_data <- get(paste0("shp", year, "_personal"))
  
  # Merge with yearly dataset for fathers
  merged_data <- merge(shp_master_individual, yearly_data[c("idpers", paste0("p", year, "c45"), paste0("p", year, "c46"))], by.x = "idfath__", by.y = "idpers")
  
  # Merge with yearly dataset for mothers
  merged_data <- merge(merged_data, yearly_data[c("idpers", paste0("p", year, "c45"), paste0("p", year, "c46"))], by.x = "idmoth__", by.y = "idpers")
  
  # Rename columns
  names(merged_data)[names(merged_data) == paste0("p", year, "c45.x")] <- "height_father"
  names(merged_data)[names(merged_data) == paste0("p", year, "c46.x")] <- "weight_father"
  names(merged_data)[names(merged_data) == paste0("p", year, "c45.y")] <- "height_mother"
  names(merged_data)[names(merged_data) == paste0("p", year, "c46.y")] <- "weight_mother"
  
  # Replace negative values with NA
  merged_data[merged_data < 0] = NA
  
  # Calculate fathers and mothers BMI and average BMI
  merged_data <- merged_data %>%
    mutate(
      bmi_father = weight_father / ((height_father / 100) ^ 2),
      bmi_mother = weight_mother / ((height_mother / 100) ^ 2),
      average_bmi = (bmi_father + bmi_mother) / 2
    )
  
  # Merge the data with the personal information of every observation
  merged_year <- merge(get(paste0("shp", year, "_personal")), merged_data, by.x = "idpers", by.y = "idpers")
  
  # Merge the data with household characteristics for later analysis
  merged_year <- merge(get(paste0("shp", year, "_household")), merged_year, by.x = paste0("idhous", year), by.y = paste0("idhous", year, ".x"))
  
  # Store the result in the global environment
  assign(paste0("merged_20", year), merged_year, envir = .GlobalEnv)
}

# Add year dummies to the datasets using a for loop: 
for (year in 2004:2019) {
  dataset_name <- paste0("merged_", year)
  
  # Create a vector with the year values
  year_vector <- rep(year, nrow(get(dataset_name)))
  
  # Assign the year dummy column to the dataset
  assign(dataset_name, cbind(get(dataset_name), year_dummy = year_vector), envir = .GlobalEnv)
}

# Write a function to obtain datasets that require less memory storage by reducing the dataset on the variables needed

# Relevant variables are: 
# ID of person = idpers
# gender = sex$$
# age = age$$ 
# physician visits = p$$c15
# hospitalizations = p$$c41a
# height = p$$c45
# weight = p$$c46
# mean BMI of parents = average_bmi
# activity_weekly = p$$a04
# sleeping problems = p$$c06a 
# backpain = p$$c04a
# headache = p$$c07a
# chronic = p$$c19a
# newborn child = nbb$$
# nationality = nat_1_$$ & nat_2_$$ & nat_3_$$ 
# education = edyear$$ 
# working status = wstat$$ 
# degree of urbanity = com2_$$ 
# canton fixed effects = canton$$ 
# year fixed effects = year_dummy

# Define the function
extract_relevant_columns <- function(changeable_vars_list_type1, changeable_vars_list_type2, unchangeable_vars_list) {
  for (year in years) {
    
    # Get the dataset
    dataset_name <- paste0("merged_20", year)
    dataset <- get(dataset_name)
    
    # Print processing information
    cat("Processing year:", year, "\n")
    
    # Initialize extracted_data with unchangeable variables
    extracted_data <- dataset[, unlist(unchangeable_vars_list), drop = FALSE]
    
    # Extract changeable variables of type 1 for the current year
    for (changeable_vars_type1 in changeable_vars_list_type1) {
      
      # Create variable names
      year_vars_type1 <- paste0(changeable_vars_type1[1], year, changeable_vars_type1[2])
      
      # Combine it with extracted_data
      if (year_vars_type1 %in% colnames(dataset)) {
        extracted_data <- cbind(extracted_data, dataset[, year_vars_type1, drop = FALSE])
      } else {
        cat("Column not found:", year_vars_type1, "for year", year, "\n")
      }
    }
    
    # Extract changeable variables of type 1 for the current year
    for (changeable_vars_type2 in changeable_vars_list_type2) {
      
      # Create variable names
      year_vars_type2 <- paste0(changeable_vars_type2[1], year)
      
      # Combine it with extracted_data
      if (year_vars_type2 %in% colnames(dataset)) {
        extracted_data <- cbind(extracted_data, dataset[, year_vars_type2, drop = FALSE])
      } else {
        cat("Column not found:", year_vars_type2, "for year", year, "\n")
      }
    }
    
    # Create a unique name for the extracted data set
    extracted_name <- paste0("extracted_", dataset_name)
    
    # Assign the extracted data set to the global environment
    assign(extracted_name, extracted_data, envir = .GlobalEnv)
    
    print(paste("Extracted data set:", extracted_name, "\n"))
  }
}

# Define function inputs
vars_change_list_type1 <- list(
  c("p", "c15"), # physician visits
  c("p", "c41a"), # hospitalisations
  c("p", "c45"), # height
  c("p", "c46"), # weight
  c("p", "a04"), # activity weekly
  c("p", "c06a"), # sleeping problems over the last four weeks
  c("p", "c04a"), # back problems over the last four weeks
  c("p", "c07a"), # headache over the last four weeks
  c("p", "c19a") # enduring illness or health problem
)
vars_change_list_type2 <- list(
  c("sex"), # gender
  c("age"), # age
  c("edyear"), # education
  c("wstat"), # working status
  c("com2_"), # urban
  c("canton"), # canton
  c("nbb_"), # newborn baby in household
  c("civsta"), # civil status
  c("nat_1_"), # first nationality
  c("nat_2_"), # second nationality
  c("nat_3_") # third nationality
)
vars_unchange_list <- list(
  c("idpers"), 
  c("year_dummy"),
  c("average_bmi")
)

# Call the function
extract_relevant_columns(vars_change_list_type1, vars_change_list_type2, vars_unchange_list)

# Rename variables in all shorter data sets
for (year in 2004:2019) {
  
  # Get the data set
  current_df <- get(paste0("extracted_merged_", year))
  
  # Rename the variables
  colnames(current_df) <- c("ID", "year", "mean_bmi_parents", "physician_visits", "hospitalisations", "height", "weight", 
                            "activity_weekly", "sleep", "backpain", "headache", "chronic", "sex", "age", "edu",
                            "wstat", "urban", "canton", "newborn", "civsta", "nat1", "nat2", "nat3")
  
  # Store the result in the global environment
  assign(paste0("extracted_merged_", year), current_df, envir = .GlobalEnv)
}

# Prepare variable "swiss" already now, to avoid the removal of "inapplicable" (in second and third nationality) in the next two steps.
for (year in 2004:2019) {
  current_df <- get(paste0("extracted_merged_", year))
  current_df <- current_df %>%
    mutate(swiss = ifelse(nat1 == 8100|nat1 == 8100|nat3 == 8100, 1, 0)) %>% 
    select(-nat1, -nat2, -nat3)
  # Store the result in the local environment
  assign(paste0("extracted_merged_", year), current_df, envir = .GlobalEnv)
}

# Combine all yearly data sets 
combined_data <- bind_rows(
  extracted_merged_2004,
  extracted_merged_2005,
  extracted_merged_2006,
  extracted_merged_2007,
  extracted_merged_2008,
  extracted_merged_2009,
  extracted_merged_2010,
  extracted_merged_2011,
  extracted_merged_2012,
  extracted_merged_2013,
  extracted_merged_2014,
  extracted_merged_2015,
  extracted_merged_2016,
  extracted_merged_2017,
  extracted_merged_2018,
  extracted_merged_2019
)

# Prevent the exclusion of observations with missing values in comorbidities as they are not important for later analysis (setting missing observations to 100)
combined_data <- combined_data %>% 
  mutate(backpain = ifelse(backpain < 0, 100, backpain),
         headache = ifelse(headache < 0, 100, headache),
         chronic = ifelse(chronic < 0, 100, chronic))

# Replace negative values with NA
combined_data[combined_data < 0] = NA
combined_data <- na.omit(combined_data)

# Prepare variable BMI
combined_data <- combined_data %>% mutate(BMI = weight / ((height / 100) ^ 2))

# Prepare variable female
combined_data <- combined_data %>%
  mutate(female = ifelse(sex == 2, 1, 0)) %>%
  select(-sex)  # Drop the original 'sex' column

# Prepare variable canton
combined_data$canton <- factor(combined_data$canton, levels = 1:26, labels = c("AG  Argovia", 
                                                                               "AI  Appenzell Inner-Rhodes", 
                                                                               "AR  Appenzell Outer-Rhodes",
                                                                               "BE  Berne",
                                                                               "BS  Basle-Town",
                                                                               "BL  Basle-Country",
                                                                               "FR  Fribourg",
                                                                               "GE  Geneva",
                                                                               "GL  Glarus",
                                                                               "GR  Grisons",
                                                                               "JU  Jura",
                                                                               "LU  Lucerne",
                                                                               "NE  Neuchatel",
                                                                               "NW  Nidwalden",
                                                                               "OW  Obwalden",
                                                                               "SG  St. Gall",
                                                                               "SH  Schaffhausen",
                                                                               "SO  Solothurn",
                                                                               "SZ  Schwyz",
                                                                               "TG Thurgovia",
                                                                               "TI  Ticino",
                                                                               "UR  Uri",
                                                                               "VD  Vaud",
                                                                               "VS  Valais",
                                                                               "ZG  Zug",
                                                                               "ZH  Zurich"))

# Prepare variable civsta
combined_data <- combined_data %>%
  mutate(civsta = factor(case_when(
    civsta %in% c(3, 4, 5, 7) ~ "separated",
    civsta %in% c(2, 6) ~ "married",
    civsta %in% c(1) ~ "single"
  )))

# Relevel civsta to make "single" the reference category
combined_data$civsta <- relevel(combined_data$civsta, ref = "single")

# Prepare variable newborn
combined_data <- combined_data %>%
  mutate(newborn = ifelse(female == 1 & age >= 16 & newborn == 1, 1, 0)) 

# Prepare variable wstat
combined_data$wstat <- factor(combined_data$wstat, levels = c(1, 2, 3), labels = c("employed", "unemployed", "nonworking"))

# Prepare variable urban
combined_data <- combined_data %>%
  mutate(urban = factor(case_when(
    urban %in% c(1, 3, 4) ~ "urban",
    urban %in% c(2, 5, 6) ~ "suburban",
    urban %in% c(7, 8, 9) ~ "rural"
  )))

# Prepare variable sleep
combined_data <- combined_data %>%
  mutate(sleep = ifelse(sleep == 2|sleep == 3, 1, 0)) 

# Define NAs for the three comorbidities variables
combined_data <- combined_data %>%
  mutate(backpain = ifelse(backpain == 100, NA, backpain),
         headache = ifelse(headache == 100, NA, headache),
         chronic = ifelse(chronic == 100, NA, chronic))

# Prepare variable backpain
combined_data <- combined_data %>%
  mutate(backpain = ifelse(backpain == 2|backpain == 3, 1, 0)) 

# Prepare variable headache
combined_data <- combined_data %>%
  mutate(headache = ifelse(headache == 2|headache == 3, 1, 0))

# Prepare variable chronic
combined_data <- combined_data %>%
  mutate(chronic = ifelse(chronic == 1, 1, 0)) 

################################################################################
#                            Descriptive Statistics                            #
################################################################################

nrow(combined_data %>% filter(BMI <= 60)) # Compute N after filtering

# Create the combined kernel density plot for gender comparison regarding BMI
bmi_plot_m_f <- combined_data %>% 
  filter(BMI <= 60) %>%
  ggplot(aes(x = BMI, fill = factor(female))) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = c(18.5, 25, 30), color = "black", linetype = "dashed") + 
  labs(title = "Distribution of BMI by gender",
       caption = "Source: Swiss Household Panel (SHP). N = 7,420.",
       x = "BMI",
       y = "Density") +
  theme_minimal() +
  annotate("text", # Set labels
           x = c(12, 19, 25.5, 30.5),
           y = c(0.167, 0.167, 0.167, 0.167),  
           label = c("Underweight", "Normalweight", "Overweight", "Obese"), 
           color = "black",
           size = 2,
           hjust = 0,
           vjust = 0,
           angle = 0
  ) +
  scale_fill_manual(values = c("0" = "#003f5c", "1" = "#bc5090"), labels = c("Male", "Female")) +  # Adjust colors 
  guides(fill = guide_legend(title = NULL)) + # Remove legend title
  coord_cartesian(ylim = c(0, 0.168)) # Set y-axis limits
bmi_plot_m_f
ggsave("bmi_m_f.png", plot = bmi_plot_m_f, width = 6, height = 4, dpi = 300)

# Calculate percentages for each gender and all BMI categories
counts <- table(combined_data$female, cut(combined_data$BMI, c(-Inf, 18.5, 25, 30, Inf)))

# Calculate percentages
percentage_summary <- prop.table(counts, margin = 1) * 100

# Print the summary
print(percentage_summary)

# Exclude observations with BMI > 60 from the dataset (above 40 is already class 3 obesity)
combined_data <- combined_data %>% filter(BMI <= 60)

# Create the combined kernel density plot for children and their parents regarding BMI
bmi_plot_c_p <- ggplot(combined_data, aes(x = BMI)) +
  geom_density(aes(fill = "BMI"), alpha = 0.5, color = "black") +
  geom_density(aes(x = mean_bmi_parents, fill = "Parents BMI"), alpha = 0.5) +
  labs(title = "Distribution of BMI and parental BMI",
       caption = "Source: Swiss Household Panel (SHP). N = 7,420.",
       x = "BMI",
       y = "Density") +
  scale_fill_manual(values = c("BMI" = "#6aa84f", "Parents BMI" = "#f1c232")) +
  geom_vline(xintercept = c(18.5, 25, 30), color = "black", linetype = "dashed") +
  annotate("text", # Set labels
           x = c(12, 19, 25.5, 30.5),
           y = c(0.167, 0.167, 0.167, 0.167),  
           label = c("Underweight", "Normalweight", "Overweight", "Obese"),
           color = "black",
           size = 2,
           hjust = 0,
           vjust = 0,
           angle = 0
  ) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 0.168)) + # Set y-axis limits
  guides(fill = guide_legend(title = NULL)) # Remove the legend title
bmi_plot_c_p
ggsave("bmi_c_p.png", plot = bmi_plot_c_p, width = 6, height = 4, dpi = 300)

# Visualize physician visits
head(sort(combined_data$physician_visits, decreasing = TRUE)) # Maximum of 200 -> exclude outliers

# Compute the number of observations for the plot
nrow(combined_data %>% filter(physician_visits <= quantile(physician_visits,0.99, na.rm = TRUE))) 

# Create histogram for the number of physician visits
physician_visits_plot <- combined_data %>% filter(physician_visits <= quantile(physician_visits,0.99)) %>% 
  ggplot(aes(x = physician_visits)) +
  geom_histogram(fill = "#4e79a7", color = "black", bins = 26) +
  labs(title = "Distribution of physician visits",
       caption = expression("Source: Swiss Household Panel (SHP). " * italic("n"[P]) * " = 7,351."),
       x = "Number of physician visits a year",
       y = "Frequency") +
  theme_minimal()
physician_visits_plot
ggsave("physician_visits.png", plot = physician_visits_plot, width = 6, height = 4, dpi = 300)

# Compute average number of physician visits for males
mean(combined_data$physician_visits[combined_data$female == 0 & combined_data$physician_visits < quantile(combined_data$physician_visits, 0.99)], na.rm = TRUE)
# Compute average number of physician visits for females
mean(combined_data$physician_visits[combined_data$female == 1 & combined_data$physician_visits < quantile(combined_data$physician_visits, 0.99)], na.rm = TRUE)

# Visualize hospitalisations
head(sort(combined_data$hospitalisations, decreasing = TRUE)) # Maximum of 177 -> exclude outliers

# Compute N
nrow(combined_data %>% filter(hospitalisations <= quantile(hospitalisations,0.99))) # Compute N

# Create histogram for the number of days in hospitalisation
hospitalisation_plot <- combined_data %>% filter(hospitalisations <= quantile(hospitalisations,0.99) & female == 1) %>% 
  ggplot(aes(x = hospitalisations)) +
  geom_histogram(fill = "lightblue", color = "blue", bins = 10) +
  labs(title = "Histogram of hospitalisations",
       caption = expression("Source: Swiss Household Panel (SHP). " * italic("n"[H]) * " = 7,348."),
       x = "Number of days in hospital a year",
       y = "Frequency") +
  theme_minimal() +
  scale_x_continuous(
    breaks = 0:9,  # Set the breaks for x-axis
    labels = 0:9   # Set the labels for x-axis
  ) + theme(
    panel.grid.major.x = element_line(color = alpha("gray", 0.5)), # Adjust grid lines
    panel.grid.minor.x = element_blank()
  )
hospitalisation_plot
ggsave("hospitalisations.png", plot = hospitalisation_plot, width = 6, height = 4, dpi = 300)

# Compute the fraction of people that were actually hospitalized
sum(combined_data$hospitalisations > 0 & combined_data$hospitalisations <= quantile(combined_data$hospitalisations, 0.99), na.rm = TRUE) / nrow(combined_data)

# Compute the fraction of males that were actually hospitalized
sum(combined_data$hospitalisations > 0 & combined_data$hospitalisations <= quantile(combined_data$hospitalisations, 0.99) & combined_data$female == 0, na.rm = TRUE) / sum(combined_data$female == 0)

# Compute the fraction of females that were actually hospitalized
sum(combined_data$hospitalisations > 0 & combined_data$hospitalisations <= quantile(combined_data$hospitalisations, 0.99) & combined_data$female == 1, na.rm = TRUE) / sum(combined_data$female == 1)

## Create descriptive statistics of all variables
# Add column for the values of physician visits without outliers
top_percentile_threshold_physician_visits <- quantile(combined_data$physician_visits, 0.99) # Remove top percentile of physician visits for the descriptive statistics (as for the regression)
combined_data$physician_visits_99 <- ifelse(combined_data$physician_visits <= top_percentile_threshold_physician_visits, combined_data$physician_visits, NA) 

# Add column for the values of physician visits without outliers
top_percentile_threshold_hospitalisations <- quantile(combined_data$hospitalisations, 0.99) # Remove top percentile of hospitalisations for the descriptive statistics (as for the regression):
combined_data$hospitalisations_99 <- ifelse(combined_data$hospitalisations <= top_percentile_threshold_hospitalisations, combined_data$hospitalisations, NA) 

# Build dummies for categorical variables
combined_data <- fastDummies::dummy_cols(combined_data, select_columns = c("wstat", "civsta", "urban"))

# Specify the relevant columns
columns_to_summarize <- c("BMI", "physician_visits_99", "hospitalisations_99", "mean_bmi_parents", "female", "age", "edu", "wstat_employed", "wstat_unemployed", "wstat_nonworking", "civsta_single", "civsta_married", "civsta_separated", "activity_weekly", "newborn", "sleep", "swiss", "urban_rural", "urban_suburban", "urban_urban") 

# Create the data frame
descriptive_stats <- data.frame(
  Mean = sapply(combined_data[, columns_to_summarize], function(x) mean(x, na.rm = TRUE)),
  Standard_Deviation = sapply(combined_data[, columns_to_summarize], function(x) sd(x, na.rm = TRUE)),
  Minimum = sapply(combined_data[, columns_to_summarize], function(x) min(x, na.rm = TRUE)),
  Maximum = sapply(combined_data[, columns_to_summarize], function(x) max(x, na.rm = TRUE))
)

# Round the data frame
descriptive_stats <- round(descriptive_stats, digits = 2) # Round on two digits

# Export for LaTeX
table_object <- xtable(descriptive_stats) 
print.xtable(table_object, file = "table_descr_stats.tex")

################################################################################
#                             Regression analysis                              #
################################################################################

############################ For physician visits ##############################

# IV estimation
# Without control variables
physician_visits_iv_uncontrolled <- iv_robust(physician_visits ~ BMI | mean_bmi_parents, data = combined_data %>% filter(physician_visits <= quantile(physician_visits, 0.99)), fixed_effects = ~ canton + year)
summary(physician_visits_iv_uncontrolled)
# Extract first stage results
physician_visits_iv_first_stage_uncontrolled = lm_robust(BMI ~ mean_bmi_parents, fixed_effects = ~ canton + year, data = combined_data %>% filter(physician_visits <= quantile(physician_visits, 0.99)))
summary(physician_visits_iv_first_stage_uncontrolled)

# With control variables
physician_visits_iv <- iv_robust(physician_visits ~ BMI + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + swiss + urban | mean_bmi_parents + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + swiss + urban, data = combined_data %>% filter(physician_visits <= quantile(physician_visits, 0.99)), fixed_effects = ~ canton + year)
summary(physician_visits_iv)
# Extract first stage results
physician_visits_iv_first_stage = lm_robust(BMI ~ mean_bmi_parents + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + swiss + urban, fixed_effects = ~ canton + year, data = combined_data %>% filter(physician_visits <= quantile(physician_visits, 0.99)))
summary(physician_visits_iv_first_stage)

### Perform analogue to Breusch-Pagan test to find out, whether adjusting for heteroskedasticity is necessary
fitteds_secondstage_physician_visits <- physician_visits_iv$fitted.values 
# Filter rows with non-NA values in physician_visits_99
combined_data_physician_visits <- combined_data %>% filter(!is.na(physician_visits_99))
# Create a new data frame by adding the fitteds_secondstage_physician_visits vector
breusch_pagan_physician_visits <- cbind(combined_data_physician_visits, fitteds_secondstage_physician_visits)
# Add the squared residuals to the dataset
breusch_pagan_physician_visits["squared_residuals_2sls"] <- (breusch_pagan_physician_visits$fitteds_secondstage_physician_visits - breusch_pagan_physician_visits$physician_visits)^2
# Conduct the statistical test
breusch_pagan__physician_visits_test <- lm(squared_residuals_2sls ~ mean_bmi_parents + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + swiss + urban + canton + year, data = breusch_pagan_physician_visits)
summary(breusch_pagan__physician_visits_test) # As the p-value of the F-statistic is below 0.05 with 9.828e-09, I will use robust standard errors.

# OLS estimation
physician_visits_ols <- lm_robust(physician_visits ~ BMI + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + swiss + urban, fixed_effects = ~ year + canton, data = combined_data %>% filter(physician_visits <= quantile(physician_visits, 0.99)))
summary(physician_visits_ols)

# Hausman test for endogeneity
fitteds_firststage_physician_visits <- physician_visits_iv_first_stage$fitted.values
# Create a new data frame by adding the fitteds_firststage_physician_visits vector
hausman_physician_visits <- cbind(combined_data_physician_visits, fitteds_firststage_physician_visits)
# Conduct the statistical test
hausman_physician_visits_test <- lm_robust(physician_visits ~ BMI + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + swiss + urban + fitteds_firststage_physician_visits, data = hausman_physician_visits, fixed_effects = ~ canton + year)
summary(hausman_physician_visits_test) # Yes, the differences are statistically different (p-value = 3.402e-02)

# With control for comorbidities
# IV estimation
physician_visits_iv_w_comorbidities <- iv_robust(physician_visits ~ BMI + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + backpain + headache + chronic + swiss + urban | mean_bmi_parents + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + backpain + headache + chronic + swiss + urban, data = combined_data %>% filter(physician_visits <= quantile(physician_visits, 0.99)), fixed_effects = ~ canton + year)
summary(physician_visits_iv_w_comorbidities)
# Extract first stage results
physician_visits_iv_first_stage_w_comorbidities = lm_robust(BMI ~ mean_bmi_parents + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + backpain + headache + chronic + swiss + urban, fixed_effects = ~ canton + year, data = combined_data %>% filter(physician_visits <= quantile(physician_visits, 0.99)))
summary(physician_visits_iv_first_stage_w_comorbidities)

# OLS estimation
physician_visits_ols_w_comorbidities <- lm_robust(physician_visits ~ BMI + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + backpain + headache + chronic + swiss + urban, fixed_effects = ~ year + canton, data = combined_data %>% filter(physician_visits <= quantile(physician_visits, 0.99)))
summary(physician_visits_ols_w_comorbidities)

# IV estimation without exclusion of outliers
physician_visits_iv_no_exclusion <- iv_robust(physician_visits ~ BMI + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + swiss + urban | mean_bmi_parents + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + swiss + urban, data = combined_data, fixed_effects = ~ canton + year)
summary(physician_visits_iv_no_exclusion)

# IV estimation with exclusion of the highest 5%
physician_visits_iv_5_exclusion <- iv_robust(physician_visits ~ BMI + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + swiss + urban | mean_bmi_parents + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + swiss + urban, data = combined_data %>% filter(physician_visits <= quantile(physician_visits, 0.95)), fixed_effects = ~ canton + year)
summary(physician_visits_iv_5_exclusion)

############################ For hospitalizations ##############################
# IV estimation
# Without control variables
hospitalisations_iv_uncontrolled <- iv_robust(hospitalisations ~ BMI | mean_bmi_parents, data = combined_data %>% filter(hospitalisations <= quantile(hospitalisations, 0.99)), fixed_effects = ~ canton + year)
summary(hospitalisations_iv_uncontrolled)
# Extract first stage results
hospitalisations_iv_first_stage_uncontrolled = lm_robust(BMI ~ mean_bmi_parents, fixed_effects = ~ canton + year, data = combined_data %>% filter(hospitalisations <= quantile(hospitalisations, 0.99)))
summary(hospitalisations_iv_first_stage_uncontrolled)

# With control variables
hospitalisations_iv <- iv_robust(hospitalisations ~ BMI + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + swiss + urban | mean_bmi_parents + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + swiss + urban, data = combined_data %>% filter(hospitalisations <= quantile(hospitalisations, 0.99)), fixed_effects = ~ canton + year)
summary(hospitalisations_iv)
# Extract first stage results
hospitalisations_iv_first_stage = lm_robust(BMI ~ mean_bmi_parents + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + swiss + urban, fixed_effects = ~ canton + year, data = combined_data %>% filter(hospitalisations <= quantile(hospitalisations, 0.99)))
summary(hospitalisations_iv_first_stage)

### Perform analogue to Breusch-Pagan test to find out, whether adjusting for heteroskedasticity is necessary
fitteds_secondstage_hospitalisations <- hospitalisations_iv$fitted.values 
# Filter rows with non-NA values in hospitalisations_99
combined_data_hospitalisations <- combined_data %>% filter(!is.na(hospitalisations_99))
# Create a new data frame by adding the fitteds_secondstage_hospitalisations vector
breusch_pagan_hospitalisations <- cbind(combined_data_hospitalisations, fitteds_secondstage_hospitalisations)
# Add the squared residuals to the dataset
breusch_pagan_hospitalisations["squared_residuals_2sls"] <- (breusch_pagan_hospitalisations$fitteds_secondstage_hospitalisations - breusch_pagan_hospitalisations$hospitalisations)^2
# Conduct the statistical test
breusch_pagan_hospitalisations_test <- lm(squared_residuals_2sls ~ mean_bmi_parents + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + swiss + urban + canton + year, data = breusch_pagan_hospitalisations)
summary(breusch_pagan_hospitalisations_test) # As the p-value of the F-statistic is below 0.05 with 5.164e-05, I will use robust standard errors.

# OLS estimation
hospitalisations_ols <- lm_robust(hospitalisations ~ BMI + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + swiss + urban, fixed_effects = ~ year + canton, data = combined_data %>% filter(hospitalisations <= quantile(hospitalisations, 0.99)))
summary(hospitalisations_ols)

# Hausman test for endogeneity
fitteds_firststage_hospitalisations <- hospitalisations_iv_first_stage$fitted.values
# Create a new data frame by adding the fitteds_firststage vector
hausman_hospitalisations <- cbind(combined_data_hospitalisations, fitteds_firststage_hospitalisations)
# Conduct the statistical test
hausman_hospitalisations_test <- lm_robust(hospitalisations ~ BMI + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + swiss + urban + fitteds_firststage_hospitalisations, data = hausman_hospitalisations, fixed_effects = ~ canton + year)
summary(hausman_hospitalisations_test) # The differences are only significant at the 10% significance level (p = 6.248e-02)

# With control for comorbidities
# IV estimation
hospitalisations_iv_w_comorbidities <- iv_robust(hospitalisations ~ BMI + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + backpain + headache + chronic + swiss + urban | mean_bmi_parents + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + backpain + headache + chronic + swiss + urban, data = combined_data %>% filter(hospitalisations <= quantile(hospitalisations, 0.99)), fixed_effects = ~ canton + year)
summary(hospitalisations_iv_w_comorbidities)
# Extract first stage results
hospitalisations_iv_first_stage_w_comorbidities = lm_robust(BMI ~ mean_bmi_parents + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + backpain + headache + chronic + swiss + urban, fixed_effects = ~ canton + year, data = combined_data %>% filter(hospitalisations <= quantile(hospitalisations, 0.99)))
summary(hospitalisations_iv_first_stage_w_comorbidities)

# OLS estimation
hospitalisations_ols_w_comorbidities <- lm_robust(hospitalisations ~ BMI + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + backpain + headache + chronic + swiss + urban, fixed_effects = ~ year + canton, data = combined_data %>% filter(hospitalisations <= quantile(hospitalisations, 0.99)))
summary(hospitalisations_ols_w_comorbidities)

# IV estimation without exclusion of outliers
hospitalisations_iv_no_exclusion <- iv_robust(hospitalisations ~ BMI + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + swiss + urban | mean_bmi_parents + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + swiss + urban, data = combined_data, fixed_effects = ~ canton + year)
summary(hospitalisations_iv_no_exclusion)

# IV estimation with exclusion of the highest 5%
hospitalisations_iv_5_exclusion <- iv_robust(hospitalisations ~ BMI + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + swiss + urban | mean_bmi_parents + female + age + edu + wstat + civsta + activity_weekly + newborn + sleep + swiss + urban, data = combined_data %>% filter(hospitalisations <= quantile(hospitalisations, 0.95)), fixed_effects = ~ canton + year)
summary(hospitalisations_iv_5_exclusion)

################################################################################
##                             Code for the DAGs                              ##
################################################################################
# Change the working directory
setwd("C:/Users/phili/OneDrive/Desktop/Bachelor thesis")

############################# DAG without the IV ###############################
# Specify the coordinates of the nodes
coords_wo_iv <- list(
  x = c(y1 = 0, x1 = -2, u_c = -2, o_c = 0, m = -1, g_p = 0),
  y = c(y1 = 0, x1 = 0, u_c = -1.5, o_c = -1.5, m = 1.5, g_p = 1.5)
)

# Create the DAG
dag_wo_iv <- dagify(y1 ~ o_c + u_c + m + g_p,
                   x1 ~ o_c + u_c,
                   m ~ x1 + g_p,
                   y1 ~ ~x1,
                   o_c ~ u_c,
                   exposure = "y1",
                   outcome = "x1",
                   latent = c("u_c", "g_p"),
                   coords = coords_wo_iv)

# Specify x and y axis limits
x_limits_wo_iv <- c(-3, 2)  
y_limits_wo_iv <- c(-3, 2)  

# Display the DAG and add labels
dag_wo_iv <- ggdag_status(dag_wo_iv, 
                     text = FALSE, stylized = TRUE,
                     check_overlap = FALSE) +
  guides(fill = FALSE, color = FALSE) + # Disable the legend
  geom_richtext(x=0,y=0,hjust=0,vjust=0.5,label="_**health care use**_<br>physician visits<br>hospitalisations",size=3,
                label.padding = unit(c(0.25,0.25,0.25,0.25), "lines"), label.color = "black", color = "black") +
  geom_richtext(x=-2,y=0,hjust=1,vjust=0.5,label="BMI",size=3,
                label.padding = unit(c(0.25,0.25,0.25,0.25), "lines"), label.color = "black", color = "black") +
  geom_richtext(x=-2.05,y=-1.5,hjust=1,vjust=0.2,label="_**unobserved confounders**_<br>smoker<br>time preferences",size=3,
                label.padding = unit(c(0.25,0.25,0.25,0.25), "lines"), label.color = "black", color = "black") +
  geom_richtext(x=0,y=-1.5,hjust=0,vjust=0.9,label="_**observed confounders**_<br>gender<br>age<br>education<br>working status<br>civil status<br>activity<br>newborn child<br>sleeping problems<br>nationality<br>urbanity",size=3,
                label.padding = unit(c(0.25,0.25,0.25,0.25), "lines"), label.color = "black", color = "black") +
  geom_richtext(x=-1,y=1.5,hjust=1,vjust=0.2,label="_**comorbidities**_<br>chronic health problems<br>back pain<br>headache",size=3,
                label.padding = unit(c(0.25,0.25,0.25,0.25), "lines"), label.color = "black", color = "black") +
  geom_richtext(x=0,y=1.5,hjust=0,vjust=0.5,label="genetic predisposition<br>for diseases",size=3,
                label.padding = unit(c(0.25,0.25,0.25,0.25), "lines"), label.color = "black", color = "black") +
  xlim(x_limits_wo_iv) +
  ylim(y_limits_wo_iv) +
  theme_dag()

# Display the DAG
dag_wo_iv

############################## DAG with the IV #################################
# Specify the coordinates of the nodes
coords_w_iv <- list(
  x = c(y1 = 0, x1 = -2, u_c = -2, o_c = 0, m = -1, g_p = 0, z = -2.8),
  y = c(y1 = 0, x1 = 0, u_c = -1.5, o_c = -1.5, m = 1.5, g_p = 1.5, z = -1.5)
)

# Create the DAG
dag_w_iv <- dagify(y1 ~ o_c + u_c + m + g_p,
                   x1 ~ o_c + u_c + z,
                   m ~ x1 + g_p,
                   y1 ~ ~x1,
                   o_c ~ u_c,
                   exposure = "y1",
                   outcome = "x1",
                   latent = c("u_c", "g_p"),
                   coords = coords_w_iv)

# Specify x and y axis limits
x_limits_w_iv <- c(-4, 2)  
y_limits_w_iv <- c(-3, 2)   

# Display the DAG and add labels
dag_w_iv <- ggdag_status(dag_w_iv, 
                     text = FALSE, stylized = TRUE,
                     check_overlap = FALSE) +
  guides(fill = FALSE, color = FALSE) + # Disable the legend
  geom_richtext(x=0,y=0,hjust=0,vjust=0.5,label="_**health care use**_<br>physician visits<br>hospitalisations",size=3,
                label.padding = unit(c(0.25,0.25,0.25,0.25), "lines"), label.color = "black", color = "black") +
  geom_richtext(x=-2,y=0,hjust=1,vjust=0.5,label="BMI",size=3,
                label.padding = unit(c(0.25,0.25,0.25,0.25), "lines"), label.color = "black", color = "black") +
  geom_richtext(x=-2.05,y=-1.5,hjust=0,vjust=1.1,label="_**unobserved confounders**_<br>smoker<br>time preferences",size=3,
                label.padding = unit(c(0.25,0.25,0.25,0.25), "lines"), label.color = "black", color = "black") +
  geom_richtext(x=0,y=-1.5,hjust=0,vjust=0.9,label="_**observed confounders**_<br>gender<br>age<br>education<br>working status<br>civil status<br>activity<br>newborn child<br>sleeping problems<br>nationality<br>urbanity",size=3,
                label.padding = unit(c(0.25,0.25,0.25,0.25), "lines"), label.color = "black", color = "black") +
  geom_richtext(x=-1,y=1.5,hjust=1,vjust=0.2,label="_**comorbidities**_<br>chronic health problems<br>back pain<br>headache",size=3,
                label.padding = unit(c(0.25,0.25,0.25,0.25), "lines"), label.color = "black", color = "black") +
  geom_richtext(x=-2.8,y=-1.5,hjust=1,vjust=0.5,label="BMI parents",size=3,
                label.padding = unit(c(0.25,0.25,0.25,0.25), "lines"), label.color = "black", color = "black") +
  geom_richtext(x=0,y=1.5,hjust=0,vjust=0.5,label="genetic predisposition<br>for diseases",size=3,
                label.padding = unit(c(0.25,0.25,0.25,0.25), "lines"), label.color = "black", color = "black") +
  xlim(x_limits_w_iv) +
  ylim(y_limits_w_iv) +
  theme_dag()

# Display the DAG
dag_w_iv

################################################################################
##      Code for the FSO data on the cost of medical services in 2019         ##                      ##
################################################################################

# Load the data set
fso <- read.px("px-x-1405000000_101.px", encoding = NULL,
               na.strings = c('"."', '".."', '"..."', '"...."', '"....."', '"......"',
                              '":"'))

# Convert it to a data frame
bfs <- as.data.frame(fso)

# Filter the total cost of hospitalisations for the year 2019
filtered_hospitalisation_costs <- fso %>% filter(Jahr == 2019 & Leistungserbringer == "- P.1 Krankenhäuser" & Finanzierungsregime == "Finanzierungsregime - Total", Leistung == "Leistung - Total", Art.der.Leistungserbringung == "Art der Leistungserbringung - Total")
filtered_hospitalisation_costs

# Filter the total cost of physician visits for the year 2019
filtered_physician_visits_costs <- fso %>% filter(Jahr == 2019 & Leistungserbringer == ">> P.3.1 Arztpraxen" & Finanzierungsregime == "Finanzierungsregime - Total", Leistung == "Leistung - Total", Art.der.Leistungserbringung == "Art der Leistungserbringung - Total")
filtered_physician_visits_costs


