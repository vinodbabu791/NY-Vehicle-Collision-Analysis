library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(plotrix)
library(RColorBrewer)

master_data <-
  read.csv(
    'C:/Users/VINOD/Desktop/GitWit/R-project-001/Data/Master_Collision_Data.csv',
    header = TRUE
  )

str(master_data)

# quality 'Date' and 'Time' columns as R date and Time

master_data$Date <- as.Date(master_data$Date, '%m/%d/%Y')

master_data$Time <-
  as.POSIXct(strptime(master_data$Time, format = '%H:%M'), tz = "")

names(master_data) <-
  c(
    'Date',
    'Time',
    'Borough',
    'Zip_code',
    'Latitude',
    'Longitude',
    'Location',
    'On_Street_Name',
    'Cross_Street_Name',
    'Off_Street_Name',
    'Injured',
    'Killed',
    'Ped_Injured',
    'Ped_Killed',
    'Cyc_Injured',
    'Cyc_Killed',
    'Mot_Injured',
    'Mot_Killed',
    'Contributing_Factor_Vehicle',
    'Unique_Key',
    'Vehicle_Type_code'
  )

# Goal 2

goal2_data <-
  master_data[c('Date', 'Time', 'Injured', 'Killed', 'Unique_Key')]

# adding columns for grouping data by Year,Month and hour

goal2_data$month_group <- strftime(goal2_data$Date, '%B')
goal2_data$month_group <-
  factor(
    goal2_data$month_group,
    levels  = c(
      'January',
      'February',
      'March',
      'April',
      'May',
      'June',
      'July',
      'August',
      'September',
      'October',
      'November',
      'December'
    )
  )
goal2_data$year_group <-
  cut(goal2_data$Date, breaks = '1 year', right = FALSE)
levels(goal2_data$year_group) <- c('2015', '2016', '2017')
goal2_data$hour_group <-
  cut(
    goal2_data$Time,
    breaks = '1 hour',
    right = FALSE,
    labels = paste(0:23, 1:24, sep = '-')
  )
goal2_data$is_fatal <-
  cut(
    goal2_data$Killed,
    breaks = c(-Inf, 0, Inf),
    labels = c('Non Fatal', 'Fatal')
  )

# sorting dataset by date in ascending order

goal2_data <- arrange(goal2_data, Date)

count_data = data.frame(cbind(
  ddply(goal2_data, .(hour_group), function(x) {
    x %>% summarise(accidents_count = n())
  }),
  goal2_data %>% filter(., is_fatal == 'Fatal') %>% group_by(hour_group) %>% summarise(Fatal_count = n()) %>% .[-1]
))


# Goal 2 = Plotting only Fatal accidents
goal2 <-
  ggplot(subset(goal1_data, !(Killed %in% 0)), aes(hour_group)) +
  geom_bar(aes(fill = year_group), position = position_stack()) +
  theme_bw(base_size = 15) +
  geom_smooth(
    data = count_data,
    aes(x = 1:24, y = Fatal_count),
    method = 'auto',
    se = TRUE,
    span = 0.3
  ) +
  labs(x = 'Time Interval(1 hour)', y = 'Number of Fatalities', title = 'Fatality rate for each hour by year')

# Goal 3 = Plotting all accidents
goal3 <- ggplot(goal2_data, aes(hour_group)) +
  geom_bar(aes(fill = year_group), position = position_stack()) +
  theme_bw(base_size = 15) +
  geom_smooth(
    data = count_data,
    aes(x = 1:24, y = accidents_count),
    method = 'auto',
    se = TRUE,
    span = 0.3
  ) +
  labs(x = 'Time Interval(1 hour)', y = 'Number of Accidents', title = 'Number of accidents in each hour by year')

# Comparison of both the plots
grid.arrange(goal3, goal2, nrow = 2)

# #########Data calculations

# adding Fatality proporation per 10000 accidents

count_data <-
  count_data %>% mutate(prop_per_10000 = round((Fatal_count / accidents_count) *
                                                 10000, 2))

pie_plot <-
  pie3D(
    count_data$prop_per_10000,
    theta = 1.3,
    col = topo.colors(n = 24),
    labels = paste0(count_data$hour_group, '(', count_data$prop_per_10000, ')'),
    radius = 1.1,
    labelcex = 0.7,
    shade = 0.5,
    main = "Proportion of Mortality per 10000 accidents grouped by each hour",
    explode = 0.1
  )
