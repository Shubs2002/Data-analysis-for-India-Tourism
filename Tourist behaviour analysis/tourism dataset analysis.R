getwd()
#get data
data2 <- read.csv("india tour growth dataset.csv")

#data analysis
head(data2)
summary(data2)
str(data2)

#Data Cleaning
any(is.na(data2))
data2 <- na.omit(data2)

#Calculating Growth
data2$GrowthDomestic <- ((data2$Domestic.2020.21 - data2$Domestic.2019.20) / data2$Domestic.2019.20) * 100
data2$GrowthForeign <- ((data2$Foreign.2020.21 - data2$Foreign.2019.20) / data2$Foreign.2019.20) * 100

#Calculating Averages
average_growth_domestic <- mean(data2$GrowthDomestic, na.rm = TRUE)
average_growth_foreign <- mean(data2$GrowthForeign, na.rm = TRUE)
cat("Average Domestic Growth:", average_growth_domestic, "\n")
cat("Average Foreign Growth:", average_growth_foreign, "\n")

#Display of Bar-Plot
library(ggplot2)
# Create a data frame for plotting
avg_growth_data <- data.frame(
  Category = c("Domestic", "Foreign"),
  AverageGrowth = c(average_growth_domestic, average_growth_foreign)
)

bar_plot <- ggplot(avg_growth_data, aes(x = Category, y = AverageGrowth)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Average Growth Percentages",
       x = "Visitor Type",
       y = "Average Growth (%)")

print(bar_plot)

#Heat-map

library(reshape2)
data_20 <- data2[1:20, ]

# Select the relevant columns and rename them with periods
data_subset <- data_20[, c("Name.of.the.Monument", "Domestic.2019.20", "Foreign.2019.20", "Domestic.2020.21", "Foreign.2020.21")]
colnames(data_subset) <- c("Monument", "Domestic.2019.20", "Foreign.2019.20", "Domestic.2020.21", "Foreign.2020.21")

# Melt the data to create a format suitable for a heatmap
melted_data <- melt(data_subset, id.vars = "Monument")

# Create a heatmap
heatmap_plot <- ggplot(melted_data, aes(x = variable, y = Monument, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(
    x = "Monument",
    y = "Year",
    fill = "Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(heatmap_plot)

# Line Graph
ggplot(data = data_20, aes(x = Circle)) +
  geom_line(aes(y = Domestic.2019.20, color = "Domestic 2019-20"), size = 1) +
  geom_line(aes(y = Foreign.2019.20, color = "Foreign 2019-20"), size = 1) +
  geom_line(aes(y = Domestic.2020.21, color = "Domestic 2020-21"), size = 1) +
  geom_line(aes(y = Foreign.2020.21, color = "Foreign 2020-21"), size = 1) +
  xlab("City") +
  ylab("Number of Visitors") +
  labs(color = "Visitor Type") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_manual(values = c("Domestic 2019-20" = "blue", 
                                "Foreign 2019-20" = "red", 
                                "Domestic 2020-21" = "green", 
                                "Foreign 2020-21" = "purple")) +
  ggtitle("Monument Visitors Growth Over the Years")

#Plot locations on map

library(leaflet)
latlong <- read.csv("lonandlat2.csv")
mymap <- leaflet(data = latlong) %>%
  addTiles()
mymap <- mymap %>%
  addMarkers(
    lng = ~Longitude, 
    lat = ~Latitude,
    popup = ~paste("City:", City, "<br>Monument:", `Name.of.the.Monument`)
  )

mymap


#forecasting for domesstic growth
library(forecast)

# Create a dataset from the provided data
data_for_pred <- data.frame(
  Circle = c(data_20$Circle),
  Name_of_the_Monument = c(data_20$Name.of.the.Monument),
  Foreign_2019_20 = c(data_20$Foreign.2019.20),
  Foreign_2020_21 = c(data_20$Foreign.2020.21)
)

foreign_ts <- ts(data_for_pred$Foreign_2019_20, start = c(2019, 1), frequency = 1)
# Assuming that foreign growth is approximately linear
# Create a linear model to predict foreign tourist growth
foreign_model <- lm(data_for_pred$Foreign_2019_20 ~ time(foreign_ts))

# Create a time series object for the next 2 years
future_years <- ts(2021:2022, frequency = 1)

# Predict foreign tourists for the next 2 years based on the model
predicted_growth <- predict(foreign_model, newdata = data.frame(time = time(future_years)))
# Plot the historical and predicted values
# Create a sample dataset
plot(data_for_pred$Foreign_2019_20, type = "o", xlab = "Row no", ylab = "Foreign Tourist Arrivals", col = "blue", main = "Foreign Tourist Growth Prediction")
lines(data_for_pred$Foreign_2020_21, type = "o", col = "red")
lines(predicted_growth, type = "o", col = "green")
# Add a legend
legend("topright", legend = c("2019-20", "2020-21", "Predicted 2021-22", "Predicted 2022-23"), col = c("blue", "red", "green"), lty = 1, cex = 0.8)


data_transposed <- t(data_for_pred[, c("Foreign_2019_20", "Foreign_2020_21")])

# Plot each row as a time series
matplot(data_transposed, type = "l", xlab = "Year", ylab = "Foreign Tourist Arrivals", col = 1:nrow(data_transposed), lty = 1, main = "Foreign Tourist Growth Prediction")

# Add a legend
legend("topright", legend = data_for_pred$Name_of_the_Monument, col = 1:nrow(data_transposed), lty = 1, cex = 0.8)


