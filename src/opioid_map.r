library(shiny)
library(ggplot2)
library(maps)
library(tidycensus)
library(tidyverse)

# Read the data
file_path <- "data/VSRR_Provisional_County-Level_Drug_Overdose_Death_Counts.csv"
data <- read.csv(file_path, colClasses = c(FIPS = "character"))

# Add leading zeros to FIPS for all FIPS under five characters
data$FIPS <- ifelse(nchar(data$FIPS) < 5, paste0("0", data$FIPS), data$FIPS)

# Convert Provisional.Drug.Overdose.Deaths to numeric
data$deaths <- as.numeric(data$Provisional.Drug.Overdose.Deaths)

# Replace NA for deaths with 10 divided by the square root of two
data$deaths[is.na(data$deaths)] <- 10 / sqrt(2)

total_deaths <- data %>%
        group_by(FIPS, ST_ABBREV, STATE_NAME, COUNTYNAME, STATEFIPS, COUNTYFIPS) %>%
        summarise(deaths = sum(deaths))

# Set census API key
census_api_key("fc3a1f71e813e66a629f0fb02fe94e6cce309ce1")

# Add population data
county_pop <- get_acs(
        geography = "county",
        variables = c("B01003_001", "B19013_001", "B17001_002"),
        year = 2019,
        survey = "acs5",
        geometry = TRUE
)

# Change from long to wide to create three variables with the value of estimate by each level of variable
county_pop_wide <- county_pop %>%
        pivot_wider(names_from = variable, values_from = c(estimate,moe))

# Rename variables in county_pop_wide to be more descriptive
names(county_pop_wide)[names(county_pop_wide) == "estimate_B01003_001"] <- "total_population"
names(county_pop_wide)[names(county_pop_wide) == "estimate_B19013_001"] <- "median_income"
names(county_pop_wide)[names(county_pop_wide) == "estimate_B17001_002"] <- "poverty_population"

# Rename moe variables in county_pop_wide to be more descriptive
names(county_pop_wide)[names(county_pop_wide) == "moe_B01003_001"] <- "total_population_moe"
names(county_pop_wide)[names(county_pop_wide) == "moe_B19013_001"] <- "median_income_moe"
names(county_pop_wide)[names(county_pop_wide) == "moe_B17001_002"] <- "poverty_population_moe"

#  Merge deaths to population data
county_map <- merge(county_pop_wide, 
        total_deaths[c("FIPS", "deaths", "ST_ABBREV", "STATE_NAME", "COUNTYNAME", "STATEFIPS", "COUNTYFIPS")], 
        by.x = "GEOID", by.y = "FIPS", all.x = TRUE)

# Create crude rate variable for deaths (per 100,000 population)
county_map$crude_rate <- county_map$deaths / county_map$total_population * 100000

# Shiny app
ui <- fluidPage(
titlePanel("Crude Rate of Drug Overdose Deaths"),
selectInput("variable", "Select Variable", 
        choices = c("Total Population" = "total_population",
        "Median Income" = "median_income",
        "Percent in Poverty" = "poverty_population",
        "Count of Opioid Deaths" = "deaths", 
        "Crude Rate of Opioid Deaths per 100k Population" = "crude_rate")),
        plotOutput("mapPlot")
)

server <- function(input, output) {
        output$mapPlot <- renderPlot({
                variable <- input$variable
                
                ggplot(county_map, aes(fill = !!sym(variable))) +
                        geom_sf() +
                        theme_void() +
                        coord_sf(xlim = c(-125, -65), ylim = c(25, 50)) +
                        scale_fill_viridis_c(option = "magma", trans = "log2") +
                        labs(title = paste("Choropleth of", str_replace(variable, "_", " "), "in US Counties"))
        })
}

shinyApp(ui = ui, server = server)



