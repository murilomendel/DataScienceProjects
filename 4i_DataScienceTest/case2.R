library(dslabs)
library(tidyverse)
library(ggplot2)
library(readr)
library(stringr)
library(scales)
library(dplyr)
library(forecast)
library(Ecdat)

# Importing data to R environment
cwd <- getwd()
file <- "/data/data_comexstat.csv"
path <- file.path(cwd, file)

bra_df <- as.data.frame(read_csv(path, col_names = TRUE))
separate_date <- bra_df %>% separate(date, c("year", "month", "day"), "-")
bra_df <- bra_df %>% mutate(year = separate_date$year)
bra_df[,9] <- sapply(bra_df[,9], as.numeric)

#Data Summary
head(bra_df)
summary(bra_df)

# Set of values for some columns
unique(bra_df$product)
unique(bra_df$state) %>% sort(decreasing = FALSE)
unique(bra_df$country) %>% sort(decreasing = FALSE)
unique(bra_df$route)

# Countries Ocurrences
table(bra_df$country)


# QUESTION 1

# Creating a monthly summary oin tons for each product
bra_soy_export_monthly <- bra_df %>% filter(type == "Export") %>%
    filter(product == c("soybean_meal", "soybean_oil", "soybeans")) %>%
    group_by(date, product) %>%
    summarise(tons = sum(tons))
bra_soy_export_monthly %>% group_by(product) %>% summarize(mean = mean(tons), std = sd(tons))

#Plotting the monthly Summary of brazilian exported soybeans
bra_soy_export_monthly %>% ggplot() +
  geom_line(aes(x = date, y = tons, color = product)) +
  scale_y_continuous(trans = 'log10', 
                     labels = trans_format("log10", math_format(10^.x)), 
                     breaks = trans_breaks("log10", function(x) 10^x)) +
  scale_x_date(breaks = date_breaks("years"), 
               labels = date_format("%m/%y")) +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Monthly Brazilian Soybeans Export",
       caption = "Data from: http://comexstat.mdic.gov.br/") +
  xlab("Months)") +
  ylab("Quantity(tons)") +
  facet_grid(.~product)


# Creating a yearly export summary in tons for each product
bra_soy_export_yearly <- bra_df %>% filter(type == "Export") %>%
  filter(product == c("soybean_meal", "soybean_oil", "soybeans")) %>%
  group_by(year, product) %>%
  summarise(tons = sum(tons))

#Plotting the yearly Summary of brazilian exported soybeans
bra_soy_export_yearly %>% ggplot() +
  geom_line(aes(x = year, y = tons, color = product), lwd = 1.1) +
  scale_y_continuous(trans = log10_trans(),
                     limits = c(100000, 100000000),
                     labels = trans_format("log10", math_format(10^.x)), 
                     breaks = c(100000,1000000, 10000000, 100000000)) +
  scale_x_continuous(limits = c(1997,2019),
                     breaks = seq(1997,2019,1)) +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Yearly Brazilian Soybeans Export",
       caption = "Data from: http://comexstat.mdic.gov.br/") +
  xlab("Years") +
  ylab("Quantity(tons)")

# QUESTION 2

# Summary of every export in the last 5 years
export_5year <- bra_df %>% filter(type == "Export" & year >= 2015) %>%
  group_by(year,product) %>%
  summarise(total_tons = sum(tons), total_USD = sum(usd), yearly_Profit = mean(usd))

## Plotting the last 5 year export results from Brazil from quantity point of view
export_5year %>% ggplot() +
  geom_line(aes(x = year, y = tons, color = product), lwd = 1.1) +
  scale_y_continuous(trans = log10_trans(),
                     limits = c(100000, 100000000),
                     labels = trans_format("log10", math_format(10^.x)), 
                     breaks = c(100000,1000000, 10000000, 100000000)) +
  scale_x_continuous(limits = c(2015,2019),
                     breaks = seq(2015,2019,1)) +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Brazilian exports in the last 5 years",
       caption = "Data fprom: http://comexstat.mdic.gov.br/") +
  xlab("Years") +
  ylab("Quantity(tons)")

## Plotting the last 5 year export results from Brazil from the economic point of view
export_5year %>% ggplot() +
  geom_line(aes(x = year, y = yearly_profit, color = product), lwd = 1.1) +
  scale_y_continuous(trans = log10_trans(),
                     limits = c(10^7, 10^11),
                     labels = dollar, 
                     breaks = c(10^7,10^8, 10^9, 10^10, 10^11)) +
  scale_x_continuous(limits = c(2015,2019),
                     breaks = seq(2015,2019,1)) +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Brazilian exports in the last 5 years",
       caption = "Data from: http://comexstat.mdic.gov.br/") +
  xlab("Years") +
  ylab("Price(USD)")

# Ranking of exports in the last 5 years
rank_export_5year <- bra_df %>% filter(type == "Export" & year >= 2015) %>%
  group_by(product) %>%
  summarise(total_tons = sum(tons), total_USD = sum(usd), yearly_profit = mean(total_USD/5)) %>%
  arrange(desc(yearly_profit))

rank_export_5year <- data.frame(rank_export_5year)
print(rank_export_5year)

## Plotting the last 5 year export results from Brazil from the economic point of view
rank_export_5year %>% ggplot() +
  geom_col(aes(x = reorder(product, -yearly_profit), y = yearly_profit, fill = reorder(product, -yearly_profit))) +
  scale_y_continuous(label = dollar) +
  labs(title = "Ranking about brazilian exports in the last 5 years",
       caption = "Data from: http://comexstat.mdic.gov.br/") +
  theme(axis.text.x = element_text(angle = 45 ),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) +
  xlab("Products") +
  ylab("Profit(USD/year)")


# QUESTION 3
# Main routes for brazilian corn exports
corn_export_route <- bra_df %>% filter(product == "corn" & type == "Export" & year >= 2015) %>%
  group_by(route) %>%
  summarise(total_tons = sum(tons)) %>%
  arrange(desc(total_tons))

corn_export_route <- data.frame(corn_export_route)

print(corn_export_route)

product_vs_route <- bra_df %>% filter(type == "Export" & year >= 2015) %>%
  group_by(product, route) %>%
  summarise(total_tons = sum(tons)) %>%
  arrange(product, desc(total_tons))

product_vs_route

product_vs_route %>% ggplot() +
  geom_bar(aes(x = product, y = total_tons, fill = reorder(route, -total_tons)), 
           position = "dodge", stat = "identity") +
  scale_y_continuous(trans = log10_trans(),
                     labels = trans_format("log10", math_format(10^.x))) +
  labs(title = "Main export routes from Brazil",
       caption = "Data from: http://comexstat.mdic.gov.br/") +
  theme(axis.text.x = element_text(angle = 45 ),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) +
  xlab("Products") +
  ylab("Quantity(tons)")

# QUESTION 4
# Most important trade partners for brazil for corn in the last 3 years
# Import
trade_partner_corn_import <- bra_df %>% filter(product == "corn" & year <= 2017 & type == "Import") %>%
  group_by(country) %>%
  summarise(total_import = sum(usd)/3) %>%
  arrange(desc(total_import))

trade_partner_corn_import

# Export
trade_partner_corn_export <- bra_df %>% filter(product == "corn" & year <= 2017 & type == "Export") %>%
  group_by(country) %>%
  summarise(total_export = sum(usd)/3) %>%
  arrange(desc(total_export))

trade_partner_corn_export

#Joining tables
trade_partner_corn <- full_join(trade_partner_corn_export,trade_partner_corn_import)
trade_partner_corn <- trade_partner_corn %>% mutate(total_trade = total_export + total_export)

trade_partner_corn <- data.frame(trade_partner_corn)

# Plotting Results
trade_partner_corn[c(1:10),c(1,4)] %>% ggplot() +
  geom_bar(aes(x = reorder(country, -total_trade), y = total_trade, fill = reorder(country, -total_trade)), stat = "identity") +
  scale_y_continuous(labels = dollar) +
  labs(title = "Top 10 Brazil trade partners for Corn in the last 3 years",
       caption = "Data from: http://comexstat.mdic.gov.br/") +
  theme(axis.text.x = element_text(angle = 45 ),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) +
  xlab("Countries") +
  ylab("Profit(USD/Year)")


# Most important trade partners for brazil for sugar in the last 3 years
# Import
trade_partner_sugar_import <- bra_df %>% filter(product == "sugar" & year <= 2017 & type == "Import") %>%
  group_by(country) %>%
  summarise(total_import = sum(tons)/3) %>%
  arrange(desc(total_import))

trade_partner_corn_import

# Export
trade_partner_sugar_export <- bra_df %>% filter(product == "sugar" & year <= 2017 & type == "Export") %>%
  group_by(country) %>%
  summarise(total_export = sum(tons)/3) %>%
  arrange(desc(total_export))


trade_partner_corn_export

trade_partner_sugar <- full_join(trade_partner_sugar_export,trade_partner_sugar_import)
trade_partner_sugar <- trade_partner_sugar %>% mutate(total_trade = total_export + total_export)

trade_partner_sugar <- data.frame(trade_partner_sugar)

# Plotting Results
trade_partner_sugar[c(1:10),c(1,4)] %>% ggplot() +
  geom_bar(aes(x = reorder(country, -total_trade), y = total_trade, fill = reorder(country, -total_trade)), stat = "identity") + 
  scale_y_continuous(labels = dollar) +
  labs(title = "Top 10 Brazil trade partners for Sugar in the last 3 years",
       caption = "Data from: http://comexstat.mdic.gov.br/") +
  theme(axis.text.x = element_text(angle = 45 ),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) +
  xlab("Countries") +
  ylab("Profit(USD/Year)")

## QUESTION 5

top_states_each_product <- bra_df %>% filter(type == "Export") %>%
  group_by(product, state) %>%
  summarise(total_tons = sum(tons), total_profit = sum(usd), mean_profit = sum(usd)/22) %>%
  arrange(product, desc(total_profit), state)

top_states_each_product <- data.frame(top_states_each_product)

top_states_corn <- top_states_each_product %>% filter(product == "corn") %>% arrange(desc(total_profit))
top_states_wheat <- top_states_each_product %>% filter(product == "wheat") %>% arrange(desc(total_profit))
top_states_soybean_meat <- top_states_each_product %>% filter(product == "soybean_meal") %>% arrange(desc(total_profit))
top_states_soybean_oil <- top_states_each_product %>% filter(product == "soybean_oil") %>% arrange(desc(total_profit))
top_states_soybeans <- top_states_each_product %>% filter(product == "soybeans") %>% arrange(desc(total_profit))
top_states_sugar <- top_states_each_product %>% filter(product == "sugar") %>% arrange(desc(total_profit))

# Plotting Corn Results
top_states_corn %>% ggplot() +
  geom_bar(aes(x = reorder(state, -mean_profit), y = total_profit, fill = reorder(state, -mean_profit)), position = "dodge", stat = "identity") +
  scale_y_continuous(trans = log10_trans(),
                     labels = dollar) +
  labs(title = "Ranking of States for Corn exports",
       caption = "Data from: http://comexstat.mdic.gov.br/") +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) +
  xlab("States") +
  ylab("Profit(USD/Year)")

# Plotting wheat Results
top_states_wheat %>% ggplot() +
  geom_bar(aes(x = reorder(state, -mean_profit), y = total_profit, fill = reorder(state, -mean_profit)), position = "dodge", stat = "identity") +
  scale_y_continuous(trans = log10_trans(),
                     labels = dollar) +
  labs(title = "Ranking of States for Wheat exports",
       caption = "Data from: http://comexstat.mdic.gov.br/") +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) +
  xlab("States") +
  ylab("Profit(USD/Year)")


# Plotting Soybean Meat Results
top_states_soybean_meat %>% ggplot() +
  geom_bar(aes(x = reorder(state, -mean_profit), y = total_profit, fill = reorder(state, -mean_profit)), position = "dodge", stat = "identity") +
  scale_y_continuous(trans = log10_trans(),
                     labels = dollar) +
  labs(title = "Ranking of States for Soybean Meat exports",
       caption = "Data from: http://comexstat.mdic.gov.br/") +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) +
  xlab("States") +
  ylab("Profit(USD/Year)")

# Plotting Soybean Oil Results
top_states_soybean_oil %>% ggplot() +
  geom_bar(aes(x = reorder(state, -mean_profit), y = total_profit, fill = reorder(state, -mean_profit)), position = "dodge", stat = "identity") +
  scale_y_continuous(trans = log10_trans(),
                     labels = dollar) +
  labs(title = "Ranking of States for Soybean Oil exports",
       caption = "Data from: http://comexstat.mdic.gov.br/") +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) +
  xlab("States") +
  ylab("Profit(USD/Year)")

# Plotting Soybeans Oil Results
top_states_soybeans %>% ggplot() +
  geom_bar(aes(x = reorder(state, -mean_profit), y = total_profit, fill = reorder(state, -mean_profit)), position = "dodge", stat = "identity") +
  scale_y_continuous(trans = log10_trans(),
                     labels = dollar) +
  labs(title = "Ranking of States for Soybeans Oil exports",
       caption = "Data from: http://comexstat.mdic.gov.br/") +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) +
  xlab("States") +
  ylab("Profit(USD/Year)")

# Plotting Sugar Results
top_states_sugar %>% ggplot() +
  geom_bar(aes(x = reorder(state, -mean_profit), y = total_profit, fill = reorder(state, -mean_profit)), position = "dodge", stat = "identity") +
  scale_y_continuous(trans = log10_trans(),
                     labels = dollar) +
  labs(title = "Ranking of States for Sugar Oil exports",
       caption = "Data from: http://comexstat.mdic.gov.br/") +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) +
  xlab("States") +
  ylab("Profit(USD/Year)")


# Modelling
# Forecast for Soybeans, Soybean Meal and Corn exports in tons for the next 11 years (2020, 2030)
product_ton_year  <- bra_df %>% filter(type == "Export" & (product == "corn" | product == "soybeans" | product == "soybean_meal")) %>%
  group_by(product, year) %>%
  summarise(total_tons = sum(tons))

product_ton_year <- data.frame(product_ton_year)

corn_ton_year <- product_ton_year %>% filter(product == "corn")
soybeans_ton_year <- product_ton_year %>% filter(product == "soybeans")
soybean_meal_ton_year <- product_ton_year %>% filter(product == "soybean_meal")

corn_ton_year$product <- NULL
soybeans_ton_year$product <- NULL
soybean_meal_ton_year$product <- NULL

ggplot(data = corn_ton_year, aes(x = year, y = total_tons)) + geom_point()
ggplot(data = soybeans_ton_year, aes(x = year, y = total_tons)) + geom_point()
ggplot(data = soybean_meal_ton_year, aes(x = year, y = total_tons)) + geom_point()

# Time Series Converting
corn_TSobj <- ts(corn_ton_year)
soybeans_TSobj <- ts(soybeans_ton_year)
soybean_meal_TSobj <- ts(soybean_meal_ton_year)

# TBATS autocorrelations descriptions
# T: Trigonometric terms for seasonality
# B: Box-Cox transformations for heterogeneity
# A: ARMA errors for short-term dynamics
# T: Trend
# S: Seasonal (including multiple and non-integer periods)
tmp_tbats_corn <- tbats(corn_TSobj[,2])
tmp_tbats_soybeans <- tbats(soybeans_TSobj[,2])
tmp_tbats_soybeans_meal <- tbats(soybean_meal_TSobj[,2])

# Forecast by autocorrelations
tbats_corn <- forecast::forecast(tmp_tbats_corn)
tbats_soybeans <- forecast::forecast(tmp_tbats_soybeans)
tbats_soybean_meal <- forecast::forecast(tmp_tbats_soybeans_meal)

# Apply forecast Method for each time series
tbats_corn <- holt(corn_TSobj[,2], h = 11)
tbats_soybeans <- holt(soybeans_TSobj[,2], h = 11)
tbats_soybean_meal <- holt(soybean_meal_TSobj[,2], h = 11)

# Create Dataframe for forecasted values
forecast_corn <- data.frame(c(2020:2030), c(tbats_corn$mean))
forecast_soybeans <- data.frame(c(2020:2030), c(tbats_soybeans$mean))
forecast_soybean_meal <- data.frame(c(2020:2030), c(tbats_soybean_meal$mean))

## Forecasted Data Frame
colnames(forecast_corn) <- c("year", "total_tons")
colnames(forecast_soybeans) <- c("year", "total_tons")
colnames(forecast_soybean_meal) <- c("year", "total_tons")

# Joining tables
corn_ton_year_forecast <- bind_rows(corn_ton_year, forecast_corn)
soybeans_ton_year_forecast <- bind_rows(soybeans_ton_year, forecast_soybeans) 
soybean_meal_ton_year_forecast <- bind_rows(soybean_meal_ton_year, forecast_soybean_meal)

#Plotting Results
corn_ton_year_forecast %>% ggplot +
  geom_point(aes(x = year, y = total_tons), lwd = 1.1) +
  geom_vline(xintercept = 2019) +
  ggtitle("Total Corn production Prediction") +
  ylab("Quantity(ton)") + 
  xlab("Years")

#Plotting Results
soybeans_ton_year_forecast %>% ggplot +
  geom_point(aes(x = year, y = total_tons), lwd = 1.1) +
  geom_vline(xintercept = 2019) +
  ggtitle("Total Soybeans production Prediction") +
  ylab("Quantity(ton)") + 
  xlab("Years")

#Plotting Results
soybean_meal_ton_year_forecast %>% ggplot +
  geom_point(aes(x = year, y = total_tons), lwd = 1.1) +
  geom_vline(xintercept = 2019) +
  ggtitle("Total Soybean Meal production Prediction") +
  ylab("Quantity(ton)") + 
  xlab("Years")
