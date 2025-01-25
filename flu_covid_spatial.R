# Required libraries
install.packages(c("sp", "sf", "spdep"))


library(tidyverse)
library(lubridate)
library(sp)
library(sf)
library(spdep)


#################################
## Data Preparation ############
################################


flucase <- read.csv("data/casesSummarized.csv")
url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/refs/heads/master/rolling-averages/us-states.csv"
destfile <- "C:/Users/md525251/OneDrive - University of Central Florida/Planned_papers/Flu_Ele/data/covid_state.csv"
download.file(url,destfile)
covid <- read.csv("data/covid_state.csv")
covid <- covid %>% select(date, state, cases, deaths)

# Filter out the states in covid that are not in flucase
covid <- covid[covid$state %in% unique(flucase$NAME), ]

# Rename the 'state' column to 'NAME'
colnames(covid)[colnames(covid) == "state"] <- "NAME"


##############################################
####### Flu Season 2020-21 ###################
##############################################

# For flu
fsea20_21 <- filter(flucase, Flu_Season == "2020-2021Flu Season")
fsea20_21 <- select(fsea20_21, - Flu_Season)


#For COVID
covid$date <- as.Date(covid$date)

# Filter data within the date range and sum cases by state
csea20_21 <- covid %>%
  filter(date >= "2020-08-30" & date <= "2021-08-28") %>%
  group_by(NAME) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE))

# combined flu and covid cases
season20_21 <- merge(fsea20_21, csea20_21, by = "NAME", all = TRUE)


# Calculate correlation
cor.test(log10(season20_21$Totalcases), log10(season20_21$total_cases))

# Extract R and p-value
R_value <- round(correlation$estimate, 2)


# Create the plot with annotations
ggplot(season20_21, aes(x = log10(Totalcases), y = log10(total_cases))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Flu Season 2020-21",
       x = "log10(Flu Cases)",
       y = "log10(COVID-19 Cases)") +
  theme_classic() +
  annotate("text", x = min(log10(season20_21$Totalcases)) + 0.2, 
           y = max(log10(season20_21$total_cases)) - 0.2, 
           label = paste0("R = ", R_value, "\n", "p < 0.001 " ),
           hjust = 0, size = 5) +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(size = 12))




######################################################
################# Flu Season 2021-22 #################
######################################################


# For flu
fsea21_22 <- filter(flucase, Flu_Season == "2021-2022Flu Season")
fsea21_22 <- select(fsea21_22, - Flu_Season)


#For COVID
covid$date <- as.Date(covid$date)

# Filter data within the date range and sum cases by state
csea21_22 <- covid %>%
  filter(date >= "2021-08-30" & date <= "2022-08-28") %>%
  group_by(NAME) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE))

# combined flu and covid cases
season21_22 <- merge(fsea21_22, csea21_22, by = "NAME", all = TRUE)


# Calculate correlation
cor.test(log10(season21_22$Totalcases), log10(season21_22$total_cases))


# Create the plot with annotations
ggplot(season21_22, aes(x = log10(Totalcases), y = log10(total_cases))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Flu Season 2021-22",
       x = "log10(Flu Cases)",
       y = "log10(COVID-19 Cases)") +
  theme_classic() +
  annotate("text", x = min(log10(season21_22$Totalcases)) + 0.2, 
           y = max(log10(season21_22$total_cases)) - 0.2, 
           label = paste0("R = 0.81", "\n", "p < 0.001 " ),
           hjust = 0, size = 5) +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(size = 12))




######################################################
################# Flu Season 2022-23 #################
######################################################


# For flu
fsea22_23 <- filter(flucase, Flu_Season == "2022-2023Flu Season")
fsea22_23 <- select(fsea22_23, - Flu_Season)


# Filter data within the date range and sum cases by state
csea22_23 <- covid %>%
  filter(date >= "2022-08-30" & date <= "2023-08-28") %>%
  group_by(NAME) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE))

# combined flu and covid cases
season22_23 <- merge(fsea22_23, csea22_23, by = "NAME", all = TRUE)


# Calculate correlation
cor.test(log10(season22_23$Totalcases), log10(season22_23$total_cases))


# Create the plot with annotations
ggplot(season22_23, aes(x = log10(Totalcases), y = log10(total_cases))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Flu Season 2022-23",
       x = "log10(Flu Cases)",
       y = "log10(COVID-19 Cases)") +
  theme_classic() +
  annotate("text", x = min(log10(season21_22$Totalcases)) + 0.2, 
           y = 6, 
           label = paste0("R = 0.79", "\n", "p < 0.001 " ),
           hjust = 0, size = 5) +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(size = 12))




#####################################################################
###################### Spatial Analysis #############################
#####################################################################


# Load U.S. states shapefile (replace 'us_states.shp' with the actual shapefile path)
us_states <- st_read("data/tl_2024_us_state/tl_2024_us_state.shp")
# Filter out the states in covid that are not in flucase
us_states <- us_states[us_states$NAME %in% unique(season20_21$NAME), ]

# Combined COVID-19 and flu cases with spatial data

season20_21 <- season20_21 %>%
  rename(
    fcase2021 = Totalcases,
    ccase2021 = total_cases)


season21_22 <- season21_22 %>%
  rename(
    fcase2122 = Totalcases,
    ccase2122 = total_cases)

season22_23 <- season22_23 %>%
  rename(
    fcase2223 = Totalcases,
    ccase2223 = total_cases)

# First, merge us_states with season20_21
spatial_data <- merge(us_states, season20_21, by.x = "NAME", by.y = "NAME", all.x = TRUE)

# Then, merge the result with season21_22
spatial_data <- merge(spatial_data, season21_22, by.x = "NAME", by.y = "NAME", all.x = TRUE)

# Finally, merge the result with season22_23
spatial_data <- merge(spatial_data, season22_23, by.x = "NAME", by.y = "NAME", all.x = TRUE)


# Create spatial neighbors based on adjacency
nb <- poly2nb(spatial_data)

# Convert neighbors to spatial weights
lw <- nb2listw(nb, style = "W")


# Moran's I test for each season and case type
results <- data.frame(
  Season = character(),
  CaseType = character(),
  Moran_I = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

for (season in c("2021", "2122", "2223")) {
  for (case_type in c("fcase", "ccase")) {
    column_name <- paste0(case_type, season)
    test_result <- moran.test(spatial_data[[column_name]], lw)
    results <- rbind(
      results,
      data.frame(
        Season = season,
        CaseType = case_type,
        Moran_I = test_result$estimate[1],
        P_Value = test_result$p.value
      )
    )
  }
}

# Export results to CSV
write.csv(results, "moran_test_results.csv", row.names = FALSE)



# Spatial Regression Analysis

#Compute the spatial lag for total_cases
spatial_data$lag_total_cases <- lag.listw(lw, spatial_data$total_cases)


moran.test(spatial_data$Totalcases, lw, y = spatial_data$total_cases)


moran.test(spatial_data$Totalcases, lw, x = spatial_data$lag_total_cases)


lw <- nb2listw(nb, style = "W")  # Ensure 'lw' is the correct listw object
# Calculate the spatial lag of total_cases
spatial_data$lag_total_cases <- lag.listw(lw, spatial_data$total_cases)

# Perform bivariate Moran's I using a linear model
lm_model <- lm(Totalcases ~ lag_total_cases, data = spatial_data)

# Conduct Moran's I test on the residuals of the linear model
bivariate_moran <- lm.morantest(lm_model, lw)

# Print the results
print(bivariate_moran)




## Mapping Flu data

# Load required libraries
library(ggplot2)
library(sf)
library(dplyr)

# Assuming 'us_states' is an sf object (spatial data) and already merged with season20_21 data
# Ensure flu cases from 2021 are renamed as fcase2021 during the merge step

# Check if the data is spatial
if (!inherits(us_states, "sf")) {
  stop("The 'us_states' dataset must be an sf object for mapping.")
}

# Create the map using ggplot2

ggplot(data = spatial_data) +
  geom_sf(aes(fill = fcase2021), color = "white") +  # Fill by flu cases
  scale_fill_viridis_c(option = "plasma", name = "Flu Cases (2021)") +  # Add color scale
  labs(
    title = "Flu Cases in the United States (2021)",
    subtitle = "Choropleth Map of Flu Cases",
    caption = "Source: Your Dataset"
  ) +
  theme_classic() +  # Clean minimal theme
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

# Print the map
print(flu_map)

