source(here::here("scripts/setup.R"))

library(data.table)
google_trends_files <- paste0("data/", "googletrends_", c("auto-elettrica", "elektro-auto", "elektrofahrzeug", "elektromobil", "eletric-car", "EV", "vehicule-electrique", "voiture-electrique"), "_2005-2022.csv")
google_trends_data <- lapply(google_trends_files, fread)
vehicle_data_2005_2008 <- fread(paste0("data/", "road_vehicle_CH_2005-2008.csv"))
vehicle_data_2009_2022 <- fread(paste0("data/", "road_vehicle_CH_2009-2022.csv"))
oil_prices_data <- fread(paste0("data/", "BrentOilPrices.csv"))
demographic_data <- fread(paste0("data/", "demographic.csv"))
charging_station <- fread(paste0("data/", "charging_station.csv"))
france_v <- read_excel(paste0("data/", "parc_vp_france_2022.xlsx"), sheet = 2)
political_data_sheets_prep <- excel_sheets(paste0("data/", "political_data.xlsx"))
df_swisspop_2022 <- read_excel(paste0("data/", "swiss_pop.xlsx"), sheet = 1)
df_swisspop_2021 <- read_excel(paste0("data/", "swiss_pop.xlsx"), sheet = 2)
df_swisspop_2020 <- read_excel(paste0("data/", "swiss_pop.xlsx"), sheet = 3)
df_swisspop_2019 <- read_excel(paste0("data/", "swiss_pop.xlsx"), sheet = 4)
df_swisspop_2018 <- read_excel(paste0("data/", "swiss_pop.xlsx"), sheet = 5)
charge_ch_fr <- fread(paste0("data/","df_charging_points_CH_FR.csv"))

## 2.2 Data Wrangling

### 2.2.1 Data Wrangling : Oil clean
# Format 1: DD-MMM-YYYY (e.g., 15-Apr-2020)
oil_df_1 <- oil_prices_data[1:8360,] %>%
  separate(Date, into = c("Day", "Month", "Year"), sep = "-") %>%
  mutate(Date = dmy(paste(Day, Month, Year)))

# Format 2: MMM DD, YYYY (e.g., Apr 22, 2020)
# Handling the separator with space and comma
oil_df_2 <- oil_prices_data[8361:nrow(oil_prices_data),] %>%
  separate(Date, into = c("Month", "Day", "Year"), sep = " ", extra = "merge") %>%
  mutate(Day = word(Day, 1),  # Extracts just the day part
         Date = mdy(paste(Month, Day, Year)))

# Merge and filter for dates after 2005
df_oil <- rbind(oil_df_1, oil_df_2) %>%
  filter(year(Date) >= 2005) %>%
  select(Date, Price)

### 2.2.2 Data Wrangling : Google trend clean
dataset_names <- c(
  "googletrends_auto-elettrica_2005-2022.csv",
  "googletrends_elektro-auto_2005-2022.csv",
  "googletrends_elektrofahrzeug_2005-2022.csv",
  "googletrends_elektromobil_2005-2022.csv",
  "googletrends_eletric-car_2005-2022.csv",
  "googletrends_EV_2005-2022.csv",
  "googletrends_vehicule-electrique_2005-2022.csv",
  "googletrends_voiture-electrique_2005-2022.csv"
)

# Function to process each dataset
process_dataset <- function(file_path) {
  # Read the dataset
  google_trends_data <- read.csv(file_path)

  # Remove the dates from the index
  google_trends_data <- google_trends_data |> rownames_to_column(var = "Date")

  # Remove the first row using slice
  google_trends_data <- slice(google_trends_data, -1)

  # Check for NA
  #cat("NA count for", "data/", ": ", sum(is.na(google_trends_data$Date)), "\n")
  #cat("NA count for SearchCount in", "data/", ": ", sum(is.na(google_trends_data$Catégorie...Toutes.catégories)), "\n")

  # Rename col
  colnames(google_trends_data)[2] <- "SearchCount"

  # Convert to numeric
  google_trends_data$SearchCount <- as.numeric(google_trends_data$SearchCount)

  # Convert the column to date with the desired format
  google_trends_data$Date <- as.Date(paste(google_trends_data$Date, "01", sep = "-"))

  return(google_trends_data)
}

# Process each dataset and store in a list
processed_datasets <- list()

for (dataset_name in dataset_names) {
  file_path_gt <- file.path("data/", dataset_name)
  processed_dataset <- process_dataset(file_path_gt)
  processed_datasets[[dataset_name]] <- processed_dataset
}

# Merge datasets based on the "Date" column
merged_data <- reduce(processed_datasets, left_join, by = "Date")

# Sum the values for each search term for a given date
df_gtrends <- merged_data %>%
  rowwise() %>%
  mutate(Sum_SearchCount = sum(c_across(starts_with("SearchCount"))))

# Normalize the Sum_SearchCount values between 1 and 100
df_gtrends$SearchRatio <- df_gtrends$Sum_SearchCount / 8

# Print the result
df_gtrends <- df_gtrends[,-(2:10)]


### 2.2.3 Cleaning of demographic_data
# Clean and process the demographic data
df_demographic <- demographic_data %>%
  filter(Year >= 2005, Year <= 2022) %>%
  filter(str_detect(Sex, "total"), str_detect(`Citizenship (category)`, "total")) %>%
  mutate(Age_clean = gsub(" years", "", Age)) %>%
  filter(!str_detect(Age_clean, "total")) %>%
  mutate(Age_num = as.numeric(Age_clean)) %>%
  filter(!is.na(Age_num), Age_num >= 18, Age_num <= 98) %>%
  select(Year, Age = Age_num, `Population on 1 January`) %>%
  mutate(
    Generation = cut(Age, breaks = c(17, 26, 42, 58, Inf), labels = c("Generation Z", "Millennials", "Generation X", "Baby Boomers"), include.lowest = TRUE),
    Year = ymd(paste(Year, "01", "01", sep = "-")) # Converting Year to Date format
  ) %>%
  group_by(Year, Generation) %>%
  summarise(Population = sum(`Population on 1 January`, na.rm = TRUE)) %>%
  pivot_wider(names_from = Generation, values_from = Population)

### 2.2.4 Data Wrangling : Swiss vehicle clean
# Define a function to process vehicle data, excluding the first row as it's not actual data
process_vehicle_data <- function(vehicle_data, start_year, end_year) {
  # the first row is a header or metadata and should be excluded
  vehicle_data <- vehicle_data[-1, ]
  
  col_names <- c("Canton", "VehicleGroupType", "Fuel", "Month", paste0("X", start_year:end_year))
  names(vehicle_data) <- col_names
  
  vehicle_data %>%
    filter(!str_detect(trimws(VehicleGroupType), "^>")) %>%
    rename(Location = Canton, VehicleType = VehicleGroupType) %>%
    mutate(VehicleType = str_remove(VehicleType, "^\\.\\.\\.\\s*")) %>%
    pivot_longer(cols = all_of(paste0("X", start_year:end_year)), names_to = "Year", values_to = "Count") %>%
    mutate(Year = as.numeric(str_remove(Year, "X")),
           MonthNum = match(Month, month.name),
           Date = as.Date(paste(Year, ifelse(is.na(MonthNum), 1, MonthNum), "01", sep = "-"), format = "%Y-%m-%d")) %>%
    select(-Month, -Year, -MonthNum)
}

# Process 2005 to 2008 and 2009 to 2022 data
v_2005_2008 <- process_vehicle_data(vehicle_data_2005_2008, 2005, 2008)
v_2009_2022 <- process_vehicle_data(vehicle_data_2009_2022, 2009, 2022)

# Merge and arrange data
df_v <- bind_rows(v_2005_2008, v_2009_2022) %>%
  arrange(Date)

# Canton and Fuel Type Standardization
standard_names <- c(
  "Switzerland" = "Switzerland", "Zürich" = "ZH", "Bern" = "BE", "Luzern" = "LU",
  "Uri" = "UR", "Schwyz" = "SZ", "Obwalden" = "OW", "Nidwalden" = "NW",
  "Glarus" = "GL", "Zug" = "ZG", "Fribourg" = "FR", "Solothurn" = "SO",
  "Basel-Stadt" = "BS", "Basel-Landschaft" = "BL", "Schaffhausen" = "SH",
  "Appenzell Ausserrhoden" = "AR", "Appenzell Innerrhoden" = "AI", 
  "St. Gallen" = "SG", "Graubünden" = "GR", "Aargau" = "AG", 
  "Thurgau" = "TG", "Ticino" = "TI", "Vaud" = "VD", "Valais" = "VS",
  "Neuchâtel" = "NE", "Genève" = "GE", "Jura" = "JU", "Confederation" = "Confederation"
)

df_v <- df_v %>%
  mutate(Location = iconv(Location, from = "latin1", to = "UTF-8")) %>%
  mutate(Location = map_chr(str_split(Location, " / "), ~ .x[1])) %>%
  mutate(Location = standard_names[Location],
         Fuel = case_when(
           Fuel %in% c("Petrol-electricity: conventional hybrid", "Diesel-electricity: conventional hybrid") ~ "Conventional hybrid",
           Fuel %in% c("Petrol-electricity: plug-in hybrid", "Diesel-electricity: plug-in hybrid") ~ "Plug-in hybrid",
           Fuel == "Gas (monovalent and bivalent)" ~ "Gas",
           TRUE ~ Fuel))

# Count number of vehicles for a particular year
#vehicle_count_2022 <- df_v %>%
#  filter(Location == "Switzerland", year(Date) == 2023, VehicleType == "Passenger car") %>%
#  summarize(TotalCount = sum(Count))

### 2.2.5 Data wrangling French vehicle clean

df_v_fr <- france_v[25:37, ] %>%
  t() %>%
  as_tibble(.name_repair = "minimal") %>%
  setNames(.[1, ]) %>%
  slice(-1) %>%
  mutate(Year = 2011:2022) %>%
  select(-c("Particulier", "Gaz", "Gaz HNR", "Gaz HR", "Hydrog\u00e8ne et autre ZE", "Inconnu")) %>%
  mutate(across(-Year, ~ floor(as.numeric(.)))) %>%
  mutate(
    Conventional_Hybrid = as.numeric(`Diesel HNR`) + as.numeric(`Essence HNR`),
    Plug_in_Hybrid = as.numeric(`Diesel HR`) + as.numeric(`Essence HR`),
    across(c(Diesel, Essence, Conventional_Hybrid, Plug_in_Hybrid, Electrique), as.numeric),
    Diesel_delta = Diesel - lag(Diesel),
    Essence_delta = Essence - lag(Essence),
    Conventional_Hybrid_delta = Conventional_Hybrid - lag(Conventional_Hybrid),
    Plug_in_Hybrid_delta = Plug_in_Hybrid - lag(Plug_in_Hybrid),
    Electrique_delta = Electrique - lag(Electrique)
  ) %>%
  filter(!is.na(Diesel_delta)) %>%
  select(Date = Year, Diesel, Diesel_delta, Essence, Essence_delta, Conventional_Hybrid, Conventional_Hybrid_delta, Plug_in_Hybrid, Plug_in_Hybrid_delta, Electrique, Electrique_delta) %>%
  mutate(Date = as.Date(paste(Date, "-01-01", sep = ""), format = "%Y-%m-%d"))

# Display cleaned data
reactable(head(df_v_fr, 100), sortable = TRUE, searchable = TRUE)


# Filtering Swiss data for specific fuel types
swiss_specific_fuel <- df_v %>%
  filter(Fuel %in% c("Diesel", "Electricity", "Conventional hybrid", "Plug-in hybrid", "Petrol")) %>%
  filter(Location == 'Switzerland') |>
  filter(VehicleType == 'Passenger car') |>
  filter(Date > as.Date('2012-01-01')) |>
  filter(Date < as.Date('2021-12-31'))

# Selecting equivalent columns from the French dataset
french_specific_fuel <- df_v_fr %>%
  select(Date, Diesel_delta, Essence_delta, Conventional_Hybrid_delta, Plug_in_Hybrid_delta, Electrique_delta) # Adjust column names accordingly

# Reshape French dataset to long format for easier plotting
french_specific_fuel_long <- french_specific_fuel %>%
  pivot_longer(cols = -Date, names_to = "Fuel", values_to = "Count")

# Standardize counts in each dataset
swiss_specific_fuel <- swiss_specific_fuel %>%
  mutate(Count = scale(Count))

french_specific_fuel_long <- french_specific_fuel_long %>%
  mutate(Count = scale(Count))

# Rename the 'Fuel' column in the French dataset
french_specific_fuel_long <- french_specific_fuel_long %>%
  mutate(Fuel = case_when(
    Fuel == "Diesel_delta" ~ "Diesel",
    Fuel == "Essence_delta" ~ "Petrol",
    Fuel == "Conventional_Hybrid_delta" ~ "Conventional hybrid",
    Fuel == "Plug_in_Hybrid_delta" ~ "Plug-in hybrid",
    Fuel == "Electrique_delta" ~ "Electricity"
  ))

# Define color palette for each fuel type
fuel_colors <- c("Diesel" = "black", "Electricity" = "green", "Conventional hybrid" = "purple", "Plug-in hybrid" = "blue", "Petrol" = "orange")

p <- ggplot() +
  geom_smooth(data = swiss_specific_fuel, aes(x = Date, y = Count, color = Fuel), 
              method = "loess", se = FALSE, size = 1.5) +
  geom_line(data = french_specific_fuel_long, aes(x = Date, y = Count, color = Fuel), 
            alpha = 0.4, size = 0.8) +
  scale_color_manual(values = fuel_colors, 
                     labels = c("Diesel", "Electricity", "Conventional hybrid", 
                                "Plug-in hybrid", "Petrol"),
                     breaks = c("Diesel", "Electricity", "Conventional hybrid", 
                                "Plug-in hybrid", "Petrol")) +
  labs(x = "Date", y = "Standardized Count", color = "Fuel Type") +
  theme_minimal() +
  geom_text(data = data.frame(x = as.Date("2021-01-01"), y = c(3, 2.8), 
                              label = c("Switzerland has", "the thickest line")), 
            aes(x = x, y = y, label = label, color = label), 
            size = 4, show.legend = FALSE)

interactive_plot <- ggplotly(p, width = 600, height = 400, tooltip = c("x", "y", "color"))
interactive_plot <- interactive_plot %>%
  layout(legend = list(orientation = "h", x = 0, xanchor = "left", y = -0.2))
interactive_plot




# Filtering Swiss data for specific fuel types
swiss_specific_fuel <- df_v %>%
  filter(Fuel %in% c("Electricity", "Conventional hybrid", "Plug-in hybrid")) %>%
  filter(Location == 'Switzerland' & VehicleType == 'Passenger car' & Date > as.Date('2012-01-01') & Date < as.Date('2021-12-31'))

# Selecting equivalent columns from the French dataset
french_specific_fuel <- df_v_fr %>%
  select(Date, Diesel_delta, Essence_delta, Conventional_Hybrid_delta, Plug_in_Hybrid_delta, Electrique_delta) # Adjust column names accordingly

# Reshape French dataset to long format for easier plotting
french_specific_fuel_long <- french_specific_fuel %>%
  pivot_longer(cols = -Date, names_to = "Fuel", values_to = "Count") %>%
  mutate(Fuel = case_when(
    Fuel == "Conventional_Hybrid_delta" ~ "Conventional hybrid",
    Fuel == "Plug_in_Hybrid_delta" ~ "Plug-in hybrid",
    Fuel == "Electrique_delta" ~ "Electricity"
  ))

# Standardize counts in each dataset
swiss_specific_fuel <- swiss_specific_fuel %>%
  mutate(Count = scale(Count))

french_specific_fuel_long <- french_specific_fuel_long %>%
  filter(Fuel %in% c("Conventional hybrid", "Plug-in hybrid", "Electricity")) %>%
  mutate(Count = scale(Count))

# Define color palette for each fuel type
fuel_colors <- c("Conventional hybrid" = "purple", "Plug-in hybrid" = "blue", "Electricity" = "green")

# Plotting with faceting and improved axis text
p <- ggplot() +
  geom_smooth(data = swiss_specific_fuel, aes(x = Date, y = Count, color = Fuel), 
              method = "loess", se = FALSE, size = 1.5) +
  geom_line(data = french_specific_fuel_long, aes(x = Date, y = Count, color = Fuel), 
            alpha = 0.4, size = 0.8) +
  scale_color_manual(values = fuel_colors, labels = c("Switzerland", "France")) +
  labs( y = "Standardized Count") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 10, angle = 0),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 12, margin = margin(t = 10)),
        axis.title.y = element_text(size = 12, margin = margin(r = 10))) +
  facet_wrap(~Fuel, scales = 'free_y', ncol = 1)

# Convert to interactive plot
interactive_plot <- ggplotly(p, width = 600, height = 600, tooltip = c("x", "y", "color"))
interactive_plot <- interactive_plot %>%
  layout(legend = list(orientation = "h", x = 0, xanchor = "left", y = -0.2))
interactive_plot

# Create the leaflet maps
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

str(df_v_map)
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
(leaflet_map_total)
(leaflet_map_per_capita)
