library("dplyr")
library("plotly")
library("ggplot2")
library("usmap")


incarceration_df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")



#Chart 1 

prison_pop <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)

prison_pop_ca <- prison_pop %>%
  filter(state == "CA")

prison_pop_ca <- prison_pop_ca %>%
  filter((county_name == "Santa Clara County") & (state == "CA"))

prison_pop_ca <- prison_pop_ca %>%
  filter(year >= "2000" & year<= "2016")

filtered_data <- prison_pop_ca %>%
  group_by(year, county_name)

chart1 <- ggplot(filtered_data, aes(year ,colour = Race)) +
  geom_line(aes(y = aapi_prison_pop, color = "Asian/Pacific Islander")) +
  geom_line(aes(y = black_prison_pop, color = "Black")) +
  geom_line(aes(y = latinx_prison_pop, color = "Latinx")) + 
  geom_line(aes(y = native_prison_pop, color = "Native American")) +
  geom_line(aes(y = white_prison_pop, color = "White")) +
  geom_line(aes(y = other_race_prison_pop, color = "Other/Unknown")) +
  scale_color_manual(values = c("Black", "Red", "Green", "Blue", "Purple", "Brown")) +
  labs(
    title = "The Different Population Counts for Different Races in Santa Clara County from 2000 to 2016",
    caption = "Source: Vera Institute",
    y= "Population",
    x = "Year",
        scale_x_continuous(breaks = seq(1970,2016, by = 5))
  )

ggplotly(chart1)
print(chart1)

# Chart 2


scatterplot <- ggplot(filtered_data, aes(x = year, y = black_jail_pop)) +
  geom_point() +
  labs(
    title = "Black Prison Populations",
    caption = "Source: Vera Institute",
    y = "Prison Population",
    x = "Year"
  )

print(scatterplot)

# Chart 3

us_pop_2018 <- incarceration_df %>%
  filter(year == '2018') %>%
  group_by(state) %>%
  summarize(total_state_pop = sum(total_pop))

us_map <- plot_usmap(data = us_pop_2018,
                     values = 'total_state_pop',
                     color = 'black') +
  scale_fill_continuous(low = 'white', high = 'red')
  labs(
    title = "Total Prison Population in US",
    caption = "Source: Vera Institute"
  )

print(us_map)

#Introduction Values

black_incarc_rate <- mean(incacertation_df$black_jail_pop, na.rm = TRUE) / mean(incacertation_df$total_jail_pop, na.rm = TRUE)

summary_info <- list()
summary_info$black_incar_rate <- mean(incacertation_df$black_jail_pop, na.rm = TRUE) / mean(incacertation_df$total_jail_pop, na.rm = TRUE)