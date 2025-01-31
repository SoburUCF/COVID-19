# Load the libraries
library(tidyverse)
library(tidyr)
library(readxl)


######################################################
################ Flu Case Rate #######################
######################################################


flu_cases <- read.csv("data/casesSummarized.csv")  

flu_cases <- flu_cases %>%
  filter(Flu_Season %in% c("2018-2019Flu Season", "2019-2020Flu Season", 
                                   "2020-2021Flu Season", "2021-2022Flu Season", 
                                   "2022-2023Flu Season", "2023-2024Flu Season"))


# Extract only the first four digits (year) and filter
flu_cases <- flu_cases %>%
  mutate(Year = substr(Flu_Season, 1, 4)) %>%  # Extract first 4 characters (year)
  filter(Year %in% c("2018", "2019", "2020", "2021", "2022", "2023"))  # Keep only specified years

# Import the population data

population <- read_excel("data/USpopulation18-23.xlsx")
names(population)
sort(population$NAME) == sort(unique(flu_cases$NAME))

# Reshape population data from wide to long format
population_long <- population %>%
  pivot_longer(cols = -NAME, names_to = "Year", values_to = "Population") %>%
  mutate(Year = as.character(Year))  # Ensure Year is a character to match flu_cases


# Merge with flu_cases based on NAME (state) and Year
flu_cases <- flu_cases %>%
  left_join(population_long, by = c("NAME" = "NAME", "Year" = "Year"))


# Create a new column for case rate per 100,000 population
flu_cases <- flu_cases %>%
  mutate(CaseRate = (Totalcases / Population) * 100000)



# Flu cases plot
# Create the corrected box plot with jitter and trend lines
ggplot(flu_cases, aes(x = factor(Year), y = CaseRate)) +
  #geom_violin( alpha = 0.7) +  # One box plot per season
  geom_boxplot(fill = "grey",outlier.shape = NA, width = 0.6) +
  geom_jitter( width = 0.2, alpha = 0.5, size = 2) +  # Small, scattered dots
  labs(y = "Flu Case Rate") +
  theme_classic() +
  theme(
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.text.x = element_blank()   # Remove x-axis text
  )


### **Paired T-Test for 2019-2020 and 2020-2021**
# Subset data for paired comparison
data_paired_c <- flu_cases %>%
  filter(Year %in% c("2019", "2020")) %>%
  select(NAME, Year, CaseRate)

# Reshape data into wide format for pairing by state
data_wide_c <- data_paired_c %>%
  pivot_wider(names_from = Year, values_from = CaseRate)

# Remove rows with NA values (in case some states are missing data)
data_wide_c <- drop_na(data_wide_c)

# Perform paired t-test
t.test(data_wide_c$`2019`, data_wide_c$`2020`, paired = TRUE)




# Calculate the differences
differences <- data_wide_c$`2020` - data_wide_c$`2019`

# Shapiro-Wilk test for normality
shapiro.test(differences)

# Histogram to visualize normality
hist(differences, main = "Distribution of Differences", xlab = "Case Rate Difference", breaks = 10)


wilcox.test(data_wide_c$`2019`, data_wide_c$`2020`, paired = TRUE)
boxplot(differences, main = "Boxplot of Differences", ylab = "Difference in Case Rate")



### **Paired T-Test for 2019-2020 and 2020-2021**
# Subset data for paired comparison
data_paired_c2 <- flu_cases %>%
  filter(Year %in% c("2020", "2021")) %>%
  select(NAME, Year, CaseRate)

# Reshape data into wide format for pairing by state
data_wide_c2 <- data_paired_c2 %>%
  pivot_wider(names_from = Year, values_from = CaseRate)

# Remove rows with NA values (in case some states are missing data)
data_wide_c2 <- drop_na(data_wide_c2)

# Perform paired t-test
t.test(data_wide_c2$`2020`, data_wide_c2$`2021`, paired = TRUE)


# Calculate the differences
differences <- data_wide_c2$`2021` - data_wide_c2$`2020`

# Shapiro-Wilk test for normality
shapiro.test(differences)

# Histogram to visualize normality
hist(differences, main = "Distribution of Differences", xlab = "Case Rate Difference", breaks = 10)


wilcox.test(data_wide_c2$`2020`, data_wide_c2$`2021`, paired = TRUE)
boxplot(differences, main = "Boxplot of Differences", ylab = "Difference in Case Rate")




####################################################################
################# Flu Death Rate       #########################
####################################################################

flu_deaths <- read.csv("data/DeathsSummarized.csv")  

flu_deaths <- flu_deaths %>%
  filter(Flu_Season %in% c("2018-2019Flu Season", "2019-2020Flu Season", 
                           "2020-2021Flu Season", "2021-2022Flu Season", 
                           "2022-2023Flu Season", "2023-2024Flu Season"))


# Extract only the first four digits (year) and filter
flu_deaths <- flu_deaths %>%
  mutate(Year = substr(Flu_Season, 1, 4)) %>%  # Extract first 4 characters (year)
  filter(Year %in% c("2018", "2019", "2021", "2022"))  # Keep only specified years



# Merge with flu_cases based on NAME (state) and Year
flu_deaths <- flu_deaths %>%
  left_join(population_long, by = c("NAME" = "NAME", "Year" = "Year"))

# Create a new column for case rate per 100,000 population
flu_deaths <- flu_deaths %>%
  mutate(DeathRate = (TotalDeaths / Population) * 1000000)


flu_deaths <- flu_deaths %>% select(-Year)

write.csv(flu_deaths, "data/flu_death_rate.csv")

# Flu cases plot
# Create the corrected box plot with jitter and trend lines
ggplot(flu_deaths, aes(x = factor(Year), y = DeathRate)) +
  #geom_violin( alpha = 0.7) +  # One box plot per season
  geom_boxplot(fill = "grey",outlier.shape = NA, width = 0.6) +
  geom_jitter( width = 0.2, alpha = 0.5, size = 2) +  # Small, scattered dots
  labs(y = "Flu Death Rate") +
  theme_classic() +
  theme(
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.text.x = element_blank()   # Remove x-axis text
  )


### **Paired T-Test for 2019-2020 and 2020-2021**
# Subset data for paired comparison
data_paired_c <- flu_cases %>%
  filter(Year %in% c("2019", "2020")) %>%
  select(NAME, Year, CaseRate)

# Reshape data into wide format for pairing by state
data_wide_c <- data_paired_c %>%
  pivot_wider(names_from = Year, values_from = CaseRate)

# Remove rows with NA values (in case some states are missing data)
data_wide_c <- drop_na(data_wide_c)

# Perform paired t-test
t_test_result <- t.test(data_wide_c$`2019`, data_wide_c$`2020`, paired = TRUE)

# Print the results
print(t_test_result)


# Calculate the differences
differences <- data_wide_c$`2020` - data_wide_c$`2019`

# Shapiro-Wilk test for normality
shapiro.test(differences)

# Histogram to visualize normality
hist(differences, main = "Distribution of Differences", xlab = "Case Rate Difference", breaks = 10)


wilcox.test(data_wide_c$`2019`, data_wide_c$`2020`, paired = TRUE)
boxplot(differences, main = "Boxplot of Differences", ylab = "Difference in Case Rate")



### **Paired T-Test for 2019-2020 and 2020-2021**
# Subset data for paired comparison
data_paired_c2 <- flu_cases %>%
  filter(Year %in% c("2020", "2021")) %>%
  select(NAME, Year, CaseRate)

# Reshape data into wide format for pairing by state
data_wide_c2 <- data_paired_c2 %>%
  pivot_wider(names_from = Year, values_from = CaseRate)

# Remove rows with NA values (in case some states are missing data)
data_wide_c2 <- drop_na(data_wide_c2)

# Perform paired t-test
t.test(data_wide_c2$`2020`, data_wide_c2$`2021`, paired = TRUE)


# Calculate the differences
differences <- data_wide_c2$`2021` - data_wide_c2$`2020`

# Shapiro-Wilk test for normality
shapiro.test(differences)

# Histogram to visualize normality
hist(differences, main = "Distribution of Differences", xlab = "Case Rate Difference", breaks = 10)


wilcox.test(data_wide_c2$`2020`, data_wide_c2$`2021`, paired = TRUE)
boxplot(differences, main = "Boxplot of Differences", ylab = "Difference in Case Rate")



####################################################################
################# Flu Vaccination Coverage #########################
####################################################################

# Import Influenza vaccine Data 
vaccine_flu <- read.csv("data/vaccineSummarized.csv")  
names(vaccine_flu)


# Filter the dataset for the last six seasons (2018-19 and onward)
vaccine_flu <- vaccine_flu %>%
  filter(Season.Survey.Year %in% c("2018-19", "2019-20", "2020-21", 
                                   "2021-22", "2022-23", "2023-24"))

# Create the corrected box plot with jitter and trend lines
ggplot(vaccine_flu, aes(x = factor(Season.Survey.Year), y = MaxCoverage)) +
  #geom_violin( alpha = 0.7) +  # One box plot per season
  geom_boxplot(fill = "grey",outlier.shape = NA, width = 0.6) +
  geom_jitter( width = 0.2, alpha = 0.5, size = 2) +  # Small, scattered dots
  labs(x = "Flu Seasons",
       y = "Flu Vaccine Coverage (%)") +
  theme_classic() 



###  Season from "2019-20" to "2020-21"
# Subset data for paired comparison
data_paired <- vaccine_flu[vaccine_flu$Season.Survey.Year %in% c("2019-20", "2020-21"), ]

# Reshape data into wide format for pairing by state
data_wide <- pivot_wider(data_paired, names_from = Season.Survey.Year, values_from = MaxCoverage)

# Perform paired t-test
t.test(data_wide$`2019-20`, data_wide$`2020-21`, paired = TRUE)


###  Season from "2020-21" to "2021-22"
# Subset data for paired comparison
data_paired2 <- vaccine_flu[vaccine_flu$Season.Survey.Year %in% c("2020-21", "2021-22"), ]

# Reshape data into wide format for pairing by state
data_wide2 <- pivot_wider(data_paired2, names_from = Season.Survey.Year, values_from = MaxCoverage)

# Perform paired t-test
t.test(data_wide2$`2020-21`, data_wide2$`2021-22`, paired = TRUE)





