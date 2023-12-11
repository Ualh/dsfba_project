# 3 Exploratory data analysis
## 3.1 Switzerland

### 3.1.1 seasonality
#creating dataset for the three seasonality graphs
passenger_cars_processed <- df_v %>%
  filter(VehicleType == "Passenger car") %>%
  mutate(YearMonth = floor_date(Date, "month")) %>%
  group_by(YearMonth) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Year = year(YearMonth), 
         Month = factor(month(YearMonth), levels = 1:12, labels = month.abb)) %>%
  arrange(Year, Month)

# plot1
p_seaso_1 <- ggplot(passenger_cars_processed, aes(x = YearMonth, y = Count)) +
  geom_line(color = "darkblue", size = 0.5) +
  labs(title = "Passenger Car Adoption Over Time in Switzerland",
       x = "Date",
       y = "Number of Passenger Cars Registered") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Convert to an interactive plotly object
interactive_plot_seaso_1 <- ggplotly(p_seaso_1, width = 600, height = 400)

# Display the interactive plot
#interactive_plot_seaso_1

# Plotting the data with ggplot2, showing the trend within each year
p_seaso_2 <- ggplot(passenger_cars_processed, aes(x = Month, y = Count, group = Year, color = as.factor(Year))) +
  geom_smooth(se = FALSE, method = "loess", span = 0.5, size = 0.7) +
  labs(title = "Monthly Passenger Car Registrations by Year",
       x = "Month",
       y = "Number of Passenger Cars Registered",
       color = "Year") +
  theme_minimal() +
  scale_color_viridis_d() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

# Convert to an interactive plotly object
interactive_plot_seaso_2 <- ggplotly(p_seaso_2, width = 600, height = 400)

# Adjust plotly settings if needed, such as margins or layout
interactive_plot_seaso_2 <- interactive_plot_seaso_2 %>%
  layout(margin = list(l = 40, r = 10, b = 40, t = 40), # Adjust margins
         legend = list(orientation = "h", x = 0, xanchor = "left", y = -0.2)) # Adjust legend position

# Display the interactive plot
#interactive_plot_seaso_2

# Plotting the data with ggplot2, showing the trend within each year
p_seaso_3 <- ggplot(passenger_cars_processed, aes(x = Month, y = Count, group = Year, color = as.factor(Year))) +
  geom_line() +
  facet_wrap(~ Year, scales = "free_y") +  # Facet by year with free y scales
  labs(title = "Seasonal Trends in Passenger Car Registrations",
       x = "Month",
       y = "Number of Passenger Cars Registered") +
  theme_minimal() +
  scale_color_viridis_d(guide = FALSE) +  # Use viridis color scale and remove the guide/legend
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels by 45 degrees
  theme(axis.text.x = element_blank(), # This will remove the month labels
        axis.text.y = element_blank(), # This will remove the month labels
        axis.ticks.x = element_blank(), # This will remove the ticks on the x-axis
        legend.position = "none") # Remove the legend to clean up the plot

# Convert to an interactive plotly object
interactive_plot_seaso_3 <- ggplotly(p_seaso_3, width = 600, height = 400) %>%
  layout(xaxis = list(tickmode = "array",
                      tickvals = 1:12,
                      ticktext = month.abb))

# Display the interactive plot
#interactive_plot_seaso_3


### 3.1.2 Vehicule Registration by Fuel time over time

# Filter df_v for specific fuel types and vehicle type
filtered_df <- df_v %>%
  filter(Fuel %in% c("Petrol", "Diesel", "Conventional hybrid", "Plug-in hybrid", "Electricity") &
           VehicleType == "Passenger car")

# Group by Date and Fuel type, and summarize the count
fuel_type_trends <- filtered_df %>%
  group_by(Date, Fuel) %>%
  summarize(Count = sum(Count, na.rm = TRUE), .groups = 'drop')

# Plotting the trends over time by fuel type
p_fuel_over_time <- ggplot(fuel_type_trends, aes(x = Date, y = Count, color = Fuel)) +
  geom_line(alpha = 0.3) +  # Plot the actual lines
  geom_smooth(aes(group = Fuel), se = FALSE, method = "loess", span = 0.1) +  # Add smoothed lines per fuel type
  labs(title = "Vehicle Registrations by Fuel Type Over Time",
       x = "Date",
       y = "Number of Vehicles Registered") +
  theme_minimal() +
  theme(legend.position = "bottom")  # Adjust legend position to the bottom

# Convert to an interactive plotly object
interactive_plot_fuel_over_time <- ggplotly(p_fuel_over_time, width = 600, height = 400)
# Adjust plotly settings 
interactive_plot_fuel_over_time <- interactive_plot_fuel_over_time %>%
  layout(legend = list(orientation = "h", x = 0, xanchor = "left", y = -0.2))

# Display the interactive plot
#interactive_plot_fuel_over_time

### 3.1.3 Availability of Charging station

#### 3.1.3.1 Availability of Charging station in Switzerland

p_charging_station <-ggplot(df_charge_number_CH, aes(x = year, y = value, group = powertrain, color = powertrain)) +
  geom_line() +
  labs(title = "Available charging station in Switzerland over the years",
       x = "Year",
       y = "Number of Charging Stations") +
  theme_minimal()

# Convert to an interactive plotly object
interactive_plot_charging_station <- ggplotly(p_charging_station, width = 600, height = 400)
# Adjust plotly settings 
interactive_plot_charging_station <- interactive_plot_charging_station %>%
  layout(legend = list(orientation = "h", x = 0, xanchor = "left", y = -0.2))

# Display the interactive plot
#interactive_plot_charging_station

p_charging_station_fr <-ggplot(df_charge_number_FR, aes(x = year, y = value, group = powertrain, color = powertrain)) +
  geom_line() +
  labs(title = "Available charging station in France over the years",
       x = "Year",
       y = "Number of Charging Stations") +
  theme_minimal()

# Convert to an interactive plotly object
interactive_plot_charging_station_fr <- ggplotly(p_charging_station_fr, width = 600, height = 400)
# Adjust plotly settings 
interactive_plot_charging_station_fr <- interactive_plot_charging_station_fr %>%
  layout(legend = list(orientation = "h", x = 0, xanchor = "left", y = -0.2))

# Display the interactive plot
#interactive_plot_charging_station_fr



### 3.1.4 Map

#### 3.1.4.1 Count of Electricity car Registration for all years per cantons

# Read in the shapefile for Swiss cantons
swiss_cantons <- st_read("data/CH_map/swissBOUNDARIES3D_1_4_TLM_KANTONSGEBIET.shp")
# Define canton abbreviations for matching
abbreviation_values <- c("ZH", "BE", "LU", "UR", "SZ", "OW", "NW", "GL", "ZG", "FR", "SO", "BS", "BL", "SH", "AR", "AI", "SG", "GR", "AG", "TG", "TI", "VD", "VS", "NE", "GE", "JU")

# Prepare the EV data with sum over all years
df_v_map <- df_v %>%
  filter(!Location %in% c("Switzerland", "Confederation"), 
         Fuel == "Electricity", VehicleType == "Passenger car") %>%
  mutate(KANTONSNUM = match(Location, abbreviation_values)) %>%
  group_by(KANTONSNUM) %>%
  summarize(TotalEV = sum(Count), .groups = 'drop')

# Merge EV data with population data
df_v_map <- left_join(df_v_map, df_swisspop_2022, by = c("KANTONSNUM" = "KANTONSNUM"))

# Calculate EV registrations per capita
df_v_map <- df_v_map %>%
  mutate(EV_per_Capita = TotalEV / TotalPopulation)

# Merge with shapefile data
map_data <- left_join(swiss_cantons, df_v_map, by = "KANTONSNUM")

# Ensure the geometries are valid and the CRS is set to WGS 84
# Check if 'map_data' is already an sf object
if (!inherits(map_data, "sf")) {
  map_data_sf <- st_as_sf(map_data, wkt = "geometry")
} else {
  map_data_sf <- map_data
}

# Ensure the geometries are valid and the CRS is set
map_data_sf <- st_make_valid(map_data_sf)
# Reproject the data to WGS 84 (EPSG:4326)
map_data_sf <- st_transform(map_data_sf, crs = 4326)


# Create color palettes for the 'Total' and 'EV_per_Capita' columns
color_palette_total <- colorNumeric(palette = "viridis", domain = map_data_sf$TotalEV)
color_palette_per_capita <- colorNumeric(palette = "viridis", domain = map_data_sf$EV_per_Capita)

# Create the leaflet maps
leaflet_map_total <- leaflet(map_data_sf) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~color_palette_total(TotalEV),
    weight = 1,
    color = "#FFFFFF",
    fillOpacity = 0.7,
    popup = ~paste(NAME, "<br>Total EV Registrations: ", TotalEV)
  ) %>%
  addLegend(
    pal = color_palette_total, 
    values = ~TotalEV, 
    opacity = 0.7, 
    title = "Total EV <br> Registrations",
    position = "topright"
  )

leaflet_map_per_capita <- leaflet(map_data_sf) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~color_palette_per_capita(EV_per_Capita),
    weight = 1,
    color = "#FFFFFF",
    fillOpacity = 0.7,
    popup = ~paste(NAME, "<br>EV Registrations per Capita: ", 
                   round(EV_per_Capita, 3))
  ) %>%
  addLegend(
    pal = color_palette_per_capita, 
    values = ~EV_per_Capita, 
    opacity = 0.7, 
    title = "EV Registrations <br> per Capita",
    position = "topright"
  )

# Print the maps to view them
#leaflet_map_total

#### 3.1.4.2 Count of Electricity car Registration for all years per cantons Standardized

#leaflet_map_per_capita

## 3.2 Google Trend

p_gtrends <- ggplot(df_gtrends, aes(x = Date, y = SearchRatio)) +
  geom_line(color = "darkgreen", size = 1, stat='smooth', se = FALSE, method = "loess", span = 0.1, size = 1) +
  labs(x = "Date", y = "Google Search", title = "Google search About EV in Switzerland")

# Convert to an interactive plotly object
interactive_plot_gtrends <- ggplotly(p_gtrends, width = 600, height = 400)
# Adjust plotly settings 
interactive_plot_gtrends <- interactive_plot_gtrends %>%
  layout(legend = list(orientation = "h", x = 0, xanchor = "left", y = -0.2))

#interactive_plot_gtrends

# Display the interactive plot
#interactive_plot_fuel_over_time

## 3.3 Oil

## basic plot
#ggplot(df_oil, aes(x = Date , y = Price)) +
#  geom_line(color = "darkred", size = 1) +
#  labs(x = "Date", y = "Price", title = "Oil Price Over Time")

# Create a ggplot object with your data
p_oil <- ggplot(df_oil, aes(x = Date, y = Price, group = 1)) +
  geom_line(color = "darkred", size = 1) +
  labs(x = "Date", y = "Price", title = "Oil Price Over Time")

# Animate the plot with gganimate, revealing the line over time
animated_plot <- p_oil +
  transition_reveal(Date)

# Render the animation
#animate(animated_plot, renderer = gganimate::gifski_renderer(), width = 600, height = 400, res = 96)


## 3.4 Demographics

demographic_data_long <- df_demographic %>%
  pivot_longer(
    cols = c('Generation Z', 'Millennials', 'Generation X', 'Baby Boomers'),
    names_to = "Generation",
    values_to = "Population"
  )

# Plotting the data with ggplot2
p_demog <- ggplot(demographic_data_long, aes(x = Year, y = Population, color = Generation)) +
  geom_line(size = 1) +
  labs(title = "Demographic Trends in Switzerland",
       x = "Year",
       y = "Population") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Generation Z" = "blue", "Millennials" = "red", "Generation X" = "green", "Baby Boomers" = "purple"))

# Convert the ggplot object to an interactive plotly object
interactive_plot_demog <- ggplotly(p_demog, width = 600, height = 400)

# Adjust plotly settings 
interactive_plot_demog <- interactive_plot_demog%>%
  layout(legend = list(orientation = "h", x = 0, xanchor = "left", y = -0.2))

# The interactive plot can be displayed in an R Markdown document or a Shiny app
#interactive_plot_demog


## 3.6 Political Parties

# Let's start with 1999
political_data_1999 <- political_combined_data[,-8] %>%
  filter(Year == as.Date("1999-01-01")) %>%
  select(c("Canton", "Against", "Slightly Against", "Neutral", "Slightly in Favour", "In Favour"))

# We will use the K-Means method
# We start by looking for the right amount of clusters

fviz_nbclust(political_data_1999[,-1], kmeans, method = "wss") +
  geom_vline(xintercept = 7, linetype = 2)

# We can see that 7 clusters seems to be the choice

# Changing my tibble
pol_cantons_1999 <- as.data.frame(political_data_1999)
rownames(pol_cantons_1999) <- pol_cantons_1999$Canton
pol_cantons_1999 <- pol_cantons_1999[,-1]

# Fit k-means with 7 clusters
km.res <- kmeans(pol_cantons_1999, 5, nstart = 26)

fviz_cluster(km.res, data = pol_cantons_1999) +
  ggtitle("Cluster Analysis of Cantons' stance on sustainability 1999")


# Then the PCA:
pca_cantons <- prcomp(pol_cantons_1999)
pca_plot <- fviz_pca(pca_cantons,
                     col.ind = as.factor(km.res$cluster),
                     palette = "jco",
                     label = "all",
                     repel = TRUE) +
  ggtitle("PCA Plot for each Canton")

# Adjust zoom level by setting xlim and ylim
pca_plot + coord_cartesian(xlim = c(-100, 100), ylim = c(-100, 100))

#Now the PCA for the variables
pca_variables <- prcomp(t(pol_cantons_1999))

# Plot variables
fviz_pca_var(pca_variables, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) +
  ggtitle("PCA Plot for Variables")

# Now for 2023
political_data_2023 <- political_combined_data[,-8] %>%
  filter(Year == as.Date("2023-01-01")) %>%
  select(c("Canton", "Against", "Slightly Against", "Neutral", "Slightly in Favour", "In Favour"))

# Changing my tibble
pol_canton_2023 <- as.data.frame(political_data_2023)
row.names(pol_canton_2023) <- pol_canton_2023$Canton
pol_canton_2023 <- pol_canton_2023[,-1]

# How many clusters?
fviz_nbclust(pol_canton_2023, kmeans, method = "wss") +
  geom_vline(xintercept = 6, linetype = 2)

# We can see that 6 clusters seems to be the choice

# Fit k-means with 6 clusters
km.res_2023 <- kmeans(pol_canton_2023, 6, nstart = 26)
fviz_cluster(km.res_2023, data = pol_canton_2023) +
  ggtitle("Cluster Analysis of Cantons' stance on sustainability 2023")

#Then the PCA plot
pca_cantons <- prcomp(pol_canton_2023)
pca_plot <- fviz_pca(pca_cantons,
                     col.ind = as.factor(km.res$cluster),
                     palette = "jco",
                     label = "all",
                     repel = TRUE) +
  ggtitle("PCA Plot for each Canton")

# Adjust zoom level by setting xlim and ylim
pca_plot + coord_cartesian(xlim = c(-100, 100), ylim = c(-100, 100))

#Now the PCA for the variables
pca_variables <- prcomp(t(pol_canton_2023))

# Plot variables
fviz_pca_var(pca_variables, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) +
  ggtitle("PCA Plot for Variables")


## 3.8 EV and Google Trends
df_v_electric <- df_v |>
  filter(VehicleType == "Passenger car") |>
  filter(Fuel == 'Electricity')
df_electric_vehicles_agg <- df_v_electric %>% group_by(Date) %>% summarize(Count = sum(Count))
# Resample both datasets to a monthly frequency using the first day of the month

# Merge datasets
merged_df <- merge(df_electric_vehicles_agg, df_gtrends, by = "Date")

# Calculate the ratio for the secondary axis
max_values <- merged_df |> summarize(max_count = max(Count, na.rm = TRUE), max_search = max(SearchRatio, na.rm = TRUE))
ratio <- max_values$max_count / max_values$max_search

# Add the ratio-adjusted SearchRatio to the merged dataset
merged_df <- merged_df |> mutate(AdjustedSearchRatio = SearchRatio * ratio)

# Plotting with smoothing and color changes
p <- ggplot(merged_df, aes(x = Date)) +
  geom_bar(aes(y = AdjustedSearchRatio, fill = "Google Trends"), stat = "identity") +
  geom_smooth(aes(y = Count, color = "Electric Vehicles Smoothed"), method = "loess", span = 0.2) +
  scale_y_continuous(
    "Number of Electric Vehicles",
    sec.axis = sec_axis(~ . / ratio, name = "Google Trends")
  ) +
  labs(title = "Comparison of Electric Vehicle Rise and Google Trends Over Time",
       x = "Date", fill = "Legend", color = "Legend") +
  scale_fill_manual(values = c("Google Trends" = "darkred")) +
  scale_color_manual(values = c("Electric Vehicles Smoothed" = "blue")) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Convert to interactive plot
interactive_plot_ev_gtrends <- ggplotly(p, width = 600, height = 400) %>%
  layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.3))

#interactive_plot_ev_gtrends


## 3.9 EV and Oil Price

# Resample data to monthly frequency and calculate mean oil price
df_oil_monthly <- df_oil %>% 
  mutate(Date = as.Date(format(Date, "%Y-%m-01"))) %>%
  group_by(Date) %>% 
  summarize(Price = mean(Price), .groups = 'drop')

# Resample electric vehicles data to monthly frequency and sum counts
df_electric_vehicles_monthly <- df_electric_vehicles_agg %>% 
  mutate(Date = as.Date(format(Date, "%Y-%m-01"))) %>%
  group_by(Date) %>% 
  summarize(Count = sum(Count), .groups = 'drop')

# Merge datasets
df_merged <- full_join(df_electric_vehicles_monthly, df_oil_monthly, by = "Date")

# Calculate the ratio for the secondary axis
max_values <- df_merged |> summarize(max_count = max(Count, na.rm = TRUE), max_price = max(Price, na.rm = TRUE))
ratio <- max_values$max_count / max_values$max_price

# Add the ratio-adjusted Price to the merged dataset
df_merged <- df_merged |> mutate(AdjustedPrice = Price * ratio)

# Plotting with smoothing and color changes
p <- ggplot(df_merged, aes(x = Date)) +
  geom_smooth(aes(y = Count, color = "Electric Vehicles Smoothed"), method = "loess", span = 0.2) +
  geom_line(aes(y = AdjustedPrice, color = "Oil Price")) +
  scale_y_continuous(
    "Number of Electric Vehicles",
    sec.axis = sec_axis(~ . / ratio, name = "Oil Price")
  ) +
  labs(title = "Comparison of Electric Vehicle Rise and Oil Prices Over Time",
       x = "Date", color = "Legend") +
  scale_color_manual(values = c("Electric Vehicles Smoothed" = "blue", "Oil Price" = "darkred")) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Convert to interactive plot
interactive_plot_ev_oil <- ggplotly(p, width = 600, height = 400) %>%
  layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.3))

#interactive_plot_ev_oil

