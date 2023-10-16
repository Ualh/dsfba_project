# dsfba_project
Data Science - Project Proposal
• Group Number: U
Project Title: Analysing the Impact of External Factors on the Adoption of Electric Vehicles in 
Switzerland (2005-2022)
Background and Motivation: 
With climatic challenges emerging as pivotal concerns of our times, understanding the dynamics 
driving environmentally conscious consumer behaviors is paramount. Our team's keen interest in 
environmental sustainability and the transition to electric vehicles in Switzerland spurs this research. 
This dedication to deciphering the nuances of eco-friendly consumption patterns is the driving force 
behind our topic selection.
Project Objectives:
Scientific and inferential goals for this project:
1. Correlation Analysis
2. Causal Inference
3. Time Series Analysis
4. Regional Disparities
Learning and accomplishments: 
1. Analyse, visualize, and identify the influences of environmental factors (such as Google 
Trends data, oil prices, and demographic metrics, and more) on EV adoption rates across 
Swiss regions.
2. Beyond correlation, determine if there's any evidence to suggest that these external factors 
directly cause changes in electric vehicle adoption.
3. Analyze the trend of electric vehicle adoption over time and predict future adoption rates 
based on historical data and external factors.
4. Identify any regional disparities in electric vehicle adoption and determine if certain regions 
in Switzerland are more influenced by external factors than others.
5. Gauge the impact of policy shifts and automotive industry trends on electric vehicle uptake 
in Switzerland.
Interesting optional features:
• Sentiment Analysis: Correlation between evolving public sentiment and EV adoption in 
Switzerland.
• Demographic Cluster Analysis: Key Swiss demographic groups favoring EV adoption.
• Economic Factors Analysis: Impact of combined economic indicators on EV adoption.
• Political Cluster Analysis: Relation between Cantons (states) aligning with specific political 
parties and their patterns of EV adoption.
• Neighbor Comparison: Influencing factors in EV adoption: Switzerland vs. neighbors.
• Infrastructure Analysis: Relation between EV charging infrastructure growth and EV 
adoption.
• Policy Impact: Effects of government incentives and regulations on EV uptake.
• Tech Advancements: Influence of battery technology advancements on EV popularity.
• Brand Analysis: Which car brands or models are driving the EV adoption trend in 
Switzerland?
• Environmental Impact: Correlation between air quality metrics and EV adoption rates.
• Consumer Behavior: Analysis of consumer buying behavior and preferences related to EVs.
• Public Transport Influence: Analyze if strong public transport networks in Swiss cities affect 
EV adoption.
• Tax Benefits and Incentives: Study the direct impact of tax breaks or incentives for EV 
owners on adoption rates.
Research Questions:
1. How does the rise in environmental consciousness, as depicted by Google Trends, correlate 
with electric vehicle adoption in Switzerland across different regions and over time, 
especially in relation to oil price fluctuations and demographic changes?
2. Given fluctuations in oil prices, demographic shifts, and major policy or automotive industry 
trends, which of these factors have a direct causal impact on the shifts in the adoption of 
electric vehicles in Switzerland?
3. Based on past electric vehicle adoption trends in Switzerland, can we forecast future 
adoption rates and pinpoint times of significant increases or decreases correlated with major 
events or policy changes?
4. In comparing regions in Switzerland, which areas show higher or lower adoption of electric 
vehicles, and how does this regional adoption align or vary with external factors like oil price 
changes, environmental consciousness, and demographic shifts?
5. To what extent have policy alterations, global environmental incidents, and shifts in the 
automotive industry affected electric vehicle adoption in Switzerland?
Data Sources: We will be procuring data from a mix of sources:
• Swiss government databases for vehicle registration details.
• Kaggle.
• Publicly accessible data on Google Trends and oil prices.
Data Description: Our dataset will encompass several tables detailing:
• Vehicle registrations segmented by region, vehicle type, and propulsion method (spanning 
2005-2022).
• road_vehicle_CH_2005-2008.csv: 170,016 rows, 8 columns, 1,360,128 cells.
• road_vehicle_CH_2009-2022.csv: 170,016 rows, 16 columns, 2,720,256 cells.
• Google Trends data focusing on search queries associated with environmental consciousness.
• google_trend_EV_2004-2023.csv: 239 rows, 1 columns, 239 cells.
• Oil price data documenting yearly fluctuations.
• BrentOilPrices.csv: 9,011 rows, 2 columns, 18,022 cells.
• Data capturing political shifts to gauge changes in ecological perspectives.
• TBD
• Swiss demographic data 
• demographic.csv: 47,736 rows, 17 columns, 811,512 cells.
See the annex for more details
Schedule & Timeline:
• Week 1-3: Topic selection.
• Week 3-5: Data collection and cleaning.
• Week 6-8: Data analysis and trend spotting.
• Week 8-10: Report compilation and visualization.
• Week 11: Review and refinements.
• Week 12: Final project hand-in.
ANNEX: 
1. BrentOilPrices.csv
• Observations (Rows): 9,011
• Variables (Columns):
• Date: The date when the oil price was recorded.
• Price: Brent Oil price on the given date.
• Relationship with Other Tables: Can be related with EV adoption datasets using the 
Date to study the influence of oil prices on EV adoption.
2. demographic.csv
• Observations (Rows): 47,736
• Variables (Columns):
• Year: Year of the demographic data.
• Citizenship (category): Classification of citizenship status.
• Sex, Age, Population on 1 January, and so on: Various demographic 
indicators.
• Relationship with Other Tables: Can be linked with EV adoption datasets using the 
Year to analyze the effect of demographic changes on EV adoption.
3. road_vehicle_CH_2005-2008.csv
• Observations (Rows): 170,016
• Variables (Columns):
• Canton: The Swiss region.
• Vehicle group / type: Type or group of the vehicle.
• Fuel: Fuel type used by the vehicle.
• Month and Year columns (2005-2008): Monthly data of vehicle numbers for 
each year.
• Relationship with Other Tables: Can be combined with the 2009-2022 dataset for a 
comprehensive view. Also, can be correlated with the oil price and demographic data 
using the Year to study various influences on vehicle types.
4. road_vehicle_CH_2009-2022.csv
• Observations (Rows): 170,016
• Variables (Columns):
• Canton, Vehicle group / type, Fuel, Month, and Year columns (2009-2022): 
Continuation of the previous dataset.
• Relationship with Other Tables: A continuation of the 2005-2008 dataset. Can be 
linked with oil price and demographic data using the Year.
5. google_trend_EV_2004-2023.csv
• Observations (Rows): 194
• Variables (Columns):
• Month: Month and year of the recorded data.
• Electric car: Google Trend score indicating the popularity of the term over 
time.
• Relationship with Other Tables: Can be related with the EV adoption datasets using 
the Month to correlate search trends with actual adoption rates.
As of today we have 5 datasets, capturing time-series data that can be cross-referenced to study 
influences on EV adoption in Switzerland. These datasets cover factors like oil prices, demographics, 
Google Trends, regional breakdowns, and public interest in electric vehicles.
