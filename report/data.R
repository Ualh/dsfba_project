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


### 2.2.6 Availability of charging stations
# Combine 'year' and 'month' columns to create a new 'Date' column
charging_station$Date <- as.Date(paste(charging_station$year, charging_station$month, "01", sep = "-"), format = "%Y-%m-%d")

# Rearrange columns with 'Date' as the first column and drop 'year' and 'month'
df_charging_station <- charging_station %>%
  select(Date, everything()) %>%
  select(-year, -month)

### 2.2.7 Availability of charging station in France and Switzerland

# Only selecting necessary columns
charge_ch_fr <- charge_ch_fr %>%
  select(c("year", "region", "powertrain", "value"))

# Setting year as.Date
charge_ch_fr$year <- as.Date(
  paste(charge_ch_fr$year,"-01-01", sep = ""), format = "%Y-%m-%d")

df_charge_number_CH <- charge_ch_fr %>%
  filter(region == "Switzerland")

df_charge_number_FR <- charge_ch_fr %>%
  filter(region == "France")


### 2.2.8 Political strength per canton
# We import each different sheets (one per canton) into a data set
political_data_sheets <- lapply(setdiff(excel_sheets("data/political_data.xlsx"), "Contenu"), 
                                function(sheet) {
                                  p_data <- suppressMessages(read_excel("data/political_data.xlsx", sheet = sheet))
                                  return(p_data)
                                })

sheet_names <- setdiff(political_data_sheets_prep, "Contenu")

named_data_list <- setNames(political_data_sheets, sheet_names)

# Now to the cleaning part
for (i in seq_along(named_data_list)) {
  # setting one dataset we work on
  current_dataset <- named_data_list[[i]]
  
  # where is "taux de participation"
  index_to_keep <- which(current_dataset[[1]] == "Taux de participation")[1]
  
  # keep only rows until "taux de participation" and delete the first one "force des partis"
  named_data_list[[i]] <- current_dataset[2:index_to_keep-2, ]
  
  # remove columns 2 and 3 (they are all NAs)
  named_data_list[[i]] <- named_data_list[[i]][ ,-c(2,3)]
  
  # transposing the data sets to have years in a single columns
  transposed_data <- t(named_data_list[[i]])
  named_data_list[[i]] <- as_tibble(transposed_data[-1, ])
  
  # set column names and change date as.Date
  colnames(named_data_list[[i]]) <- transposed_data[1, ]
  
  named_data_list[[i]][[2]] <- as.Date(
    paste(named_data_list[[i]][[2]], "-01-01", sep = ""), format = "%Y-%m-%d")
  
  # find the columns that are only NAs and remove them
  named_data_list[[i]] <- named_data_list[[i]][, colSums(!is.na(named_data_list[[i]])) > 0, drop = FALSE]
  
  # Removing all non-numeric values
  named_data_list[[i]][, -1] <- apply(named_data_list[[i]][, -1], 2, function(x) as.numeric(gsub("[^0-9.]", "", x)))
  
  # Setting the first column name as "Date"
  colnames(named_data_list[[i]])[1] <- "Date"
  
  # Removing the rows before 01.01.1999
  threshold_date <- as.Date("1999-01-01")
  named_data_list[[i]] <- named_data_list[[i]][named_data_list[[i]]$Date >= threshold_date, ]
}

# Creating a map for the political parties stance on sustainability
sustainability_mapping <- c(
  "Against" = c("MCG (MCR)","PBD",	"PBD 1",	"PBD 2",	"UDC"),
  "Slightly Against" = c("Lega","PDC",	"PDC 1",	"PDC 2",	"PLR",	"PLS",	"UDF"),
  "Neutral" = c("Adl",	"DS",	"PdL",	"POCH",	"PSL",	"PST",	"Rép.",	"Sol.","Separ.", "Autres"),
  "Slightly in Favour" = c("Il Centro", "Il Centro 1",	"Le Centre 1","Le Centre",	"Le Centre 2",	"PCS","PPD",	"PPD 1"),
  "In Favour" = c("AVF",	"AVF 1",	"PEV", "PS",	"PSA",	"PVL","VERDI",	"VERDI 2","VERT-E-S  ",	"VERT-E-S 2",	"VERT-E-S 3")
)

# Looping through the data sets the change the names of the parties by their stance
for (i in seq_along(named_data_list)){
  current_dataset <- named_data_list[[i]]
  
  for (party_name in names(current_dataset)[-1]) {
    stance <- sapply(sustainability_mapping, function(x) party_name %in% x)
    stance <- names(stance)[which(stance)]
    
    if (length(stance) > 0) {
      selected_columns <- intersect(c(party_name, stance), colnames(current_dataset))
      
      current_dataset[[stance]] <- rowSums(current_dataset[selected_columns], na.rm = TRUE)
      current_dataset[[party_name]] <- NULL
    }
  }
  named_data_list[[i]] <- current_dataset
}

# Creating my 5 columns
categories <- c("Against", "Slightly Against", "Neutral", "Slightly in Favour", "In Favour")

# Aggregating the information of the data sets into these 5 categories
for (i in seq_along(named_data_list)) {
  current_dataset <- named_data_list[[i]]
  
  # Extract the Date column
  result_dataset <- current_dataset[, "Date", drop = FALSE]
  
  # Loop through each category and aggregate values
  for (category in categories) {
    matching_columns <- grep(paste0("^", category, "\\d*$"), colnames(current_dataset), value = TRUE)
    result_dataset[[category]] <- rowSums(current_dataset[matching_columns], na.rm = TRUE)
  }
  
  named_data_list[[i]] <- result_dataset
}

# Storing the data sets into a list:
list_politic <- list()

for (i in seq_along(named_data_list)) {
  tibble_name <- paste0("politic_", sheet_names[i])
  list_politic[[tibble_name]] <- named_data_list[[i]]
}

# Correcting a NA in politic_AI
new_dates <- list_politic[["politic_VD"]]$Date
list_politic[["politic_AI"]]$Date <- new_dates

# Creating a data set per year for Switzerland
political_combined_data <- bind_rows(list_politic, .id = "Canton")
political_combined_data$Year <- as.integer(format(political_combined_data$Date, "%Y"))
political_combined_data <- political_combined_data[, -which(names(political_combined_data) == "Date")]

political_summarized_data <- political_combined_data %>%
  group_by(Year, Canton) %>%
  summarize(
    Against = sum(Against),
    `Slightly Against` = sum(`Slightly Against`),
    Neutral = sum(Neutral),
    `Slightly in Favour` = sum(`Slightly in Favour`),
    `In Favour` = sum(`In Favour`)
  )

yearly_political_datasets <- list()
unique_years <- unique(political_combined_data$Year)
for (year in unique_years) {
  year_political_dataset <- political_combined_data %>% filter(Year == year)
  yearly_political_datasets[[as.character(year)]] <- year_political_dataset
}

# now accessible via ' political_yearly_data$Year '
political_combined_data <- political_combined_data %>%
  mutate(
    Canton = sub("politic_", "", Canton),  # Remove 'politic_' prefix
    Year = ymd(paste(Year, "01", "01"))    # Convert Year to date type
  )

political_combined_data <- political_combined_data %>%
  mutate(
    KANTONSNUM = case_when(
      Canton == "ZH" ~ 1,
      Canton == "BE" ~ 2,
      Canton == "LU" ~ 3,
      Canton == "UR" ~ 4,
      Canton == "SZ" ~ 5,
      Canton == "OW" ~ 6,
      Canton == "NW" ~ 7,
      Canton == "GL" ~ 8,
      Canton == "ZG" ~ 9,
      Canton == "FR" ~ 10,
      Canton == "SO" ~ 11,
      Canton == "BS" ~ 12,
      Canton == "BL" ~ 13,
      Canton == "SH" ~ 14,
      Canton == "AR" ~ 15,
      Canton == "AI" ~ 16,
      Canton == "SG" ~ 17,
      Canton == "GR" ~ 18,
      Canton == "AG" ~ 19,
      Canton == "TG" ~ 20,
      Canton == "TI" ~ 21,
      Canton == "VD" ~ 22,
      Canton == "VS" ~ 23,
      Canton == "NE" ~ 24,
      Canton == "GE" ~ 25,
      Canton == "JU" ~ 26
    )
  )

### 2.2.9 Swiss Population
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


