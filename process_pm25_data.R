# Read in data and preform basic cleanings
data <- read_csv("data/beijing-pm25.csv")
data <- data %>%
  mutate(datetime = make_datetime(year, month, day, hour)) %>%
  rename(wind_spd = Iws, dew_pt = DEWP, temp = TEMP, pm25 = pm2.5) %>%
  select(datetime, hour, pm25, dew_pt, temp, wind_spd) %>% 
  drop_na()

# Create categorical variables
for (colname in c("dew_pt", "temp", "wind_spd")) {
  colname_cat = paste0(colname, "_cat")
  
  # Compute mean and sd of the current row
  col_mean <- mean(as.numeric(unlist(data[colname])))
  col_sd <- sd(as.numeric(unlist(data[colname])))
  
  # Create categorical variable for the current row
  data[colname_cat] <- cut(
    as.numeric(unlist(data[colname])), 
    breaks = c(-Inf, col_mean - col_sd, col_mean + col_sd, Inf), 
    labels = c("low", "normal", "high")
  )
}

data$day_part <- cut(
  data$hour, 
  breaks = c(-Inf, 12, 18, Inf), 
  labels = c("morning", "afternoon", "evening")
)

data$air_quality <- cut(
  data$pm25, 
  breaks = c(-Inf, 50, 100, 200, 300, Inf),
  labels = c("good", "moderate", "unhealthy", "very unhealthy", "hazardous")
)