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

df_swisspop_2022 <- df_swisspop_2022 %>%
  slice(-1:-4) %>%  # Remove the first 4 rows
  select(Canton = 1, TotalPopulation = 2)  # Select only the canton names and population figures

# Remove rows with NAs in the Canton column
df_swisspop_2022 <- df_swisspop_2022 %>%
  filter(!is.na(Canton))

# Map Canton names to abbreviations
df_swisspop_2022 <- df_swisspop_2022 %>%
  mutate(CantonAbbreviation = case_when(
    Canton == "Zurich" ~ "ZH",
    Canton == "Bern" ~ "BE",
    Canton == "Lucerne" ~ "LU",
    Canton == "Uri" ~ "UR",
    Canton == "Schwyz" ~ "SZ",
    Canton == "Obwalden" ~ "OW",
    Canton == "Nidwalden" ~ "NW",
    Canton == "Glarus" ~ "GL",
    Canton == "Zug" ~ "ZG",
    Canton == "Fribourg" ~ "FR",
    Canton == "Solothurn" ~ "SO",
    Canton == "Basel-Stadt" ~ "BS",
    Canton == "Basel-Landschaft" ~ "BL",
    Canton == "Schaffhausen" ~ "SH",
    Canton == "Appenzell A. Rh." ~ "AR",
    Canton == "Appenzell I. Rh." ~ "AI",
    Canton == "St. Gallen" ~ "SG",
    Canton == "Graubünden" ~ "GR",
    Canton == "Aargau" ~ "AG",
    Canton == "Thurgau" ~ "TG",
    Canton == "Ticino" ~ "TI",
    Canton == "Vaud" ~ "VD",
    Canton == "Valais" ~ "VS",
    Canton == "Neuchâtel" ~ "NE",
    Canton == "Geneva" ~ "GE",
    Canton == "Jura" ~ "JU",
    TRUE ~ NA_character_  # For unrecognized cantons
  ))

# Map Canton names to KANTONSNUM
df_swisspop_2022 <- df_swisspop_2022 %>%
  mutate(KANTONSNUM = case_when(
    Canton == "Graubünden" ~ 18,
    Canton == "Bern" ~ 2,
    Canton == "Valais" ~ 23,
    Canton == "Vaud" ~ 22,
    Canton == "Ticino" ~ 21,
    Canton == "St. Gallen" ~ 17,
    Canton == "Zurich" ~ 1,
    Canton == "Fribourg" ~ 10,
    Canton == "Lucerne" ~ 3,
    Canton == "Aargau" ~ 19,
    Canton == "Uri" ~ 4,
    Canton == "Thurgau" ~ 20,
    Canton == "Schwyz" ~ 5,
    Canton == "Jura" ~ 26,
    Canton == "Neuchâtel" ~ 24,
    Canton == "Solothurn" ~ 11,
    Canton == "Glarus" ~ 8,
    Canton == "Basel-Landschaft" ~ 13,
    Canton == "Obwalden" ~ 6,
    Canton == "Nidwalden" ~ 7,
    Canton == "Geneva" ~ 25,
    Canton == "Schaffhausen" ~ 14,
    Canton == "Appenzell A. Rh." ~ 15,
    Canton == "Zug" ~ 9,
    Canton == "Appenzell I. Rh." ~ 16,
    Canton == "Basel-Stadt" ~ 12,
    TRUE ~ NA_integer_  # For unrecognized cantons
  ))


df_swisspop_2022$TotalPopulation <- as.integer(df_swisspop_2022$TotalPopulation)
# Filter out rows where CantonAbbreviation is NA
df_swisspop_2022 <- df_swisspop_2022 %>%
  filter(!is.na(CantonAbbreviation))






