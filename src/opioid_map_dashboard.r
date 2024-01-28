library(shiny)
library(ggplot2)
library(maps)
library(tidycensus)
library(tidyverse)
library(skimr)

# Read the data
file_path <- "data/VSRR_Provisional_County-Level_Drug_Overdose_Death_Counts.csv"
data <- read.csv(file_path, colClasses = c(FIPS = "character"))

# Add leading zeros to FIPS for all FIPS under five characters
data$FIPS <- ifelse(nchar(data$FIPS) < 5, paste0("0", data$FIPS), data$FIPS)

# Convert Provisional.Drug.Overdose.Deaths to numeric
data$deaths <- as.numeric(data$Provisional.Drug.Overdose.Deaths)

# Add variable indicating US region based on STATE_NAME variable
data$US_region <- case_when(
    data$STATE_NAME %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont") ~ "Northeast",
    data$STATE_NAME %in% c("Delaware", "District of Columbia", "Maryland", "Virginia", "West Virginia", "North Carolina", "South Carolina", "Georgia", "Florida") ~ "South",
    data$STATE_NAME %in% c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin", "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota") ~ "Midwest",
    data$STATE_NAME %in% c("Alaska", "California", "Hawaii", "Oregon", "Washington", "Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming") ~ "West",
    TRUE ~ NA_character_
)

total_deaths <- data %>%
        group_by(FIPS, ST_ABBREV, STATE_NAME, COUNTYNAME, STATEFIPS, COUNTYFIPS) %>%
        summarise(deaths = sum(deaths))

# Add a very small constant to all zero values to avoid infinite results from log
    total_deaths[total_deaths == 0] <- 1e-10

# # Set census API key
# census_api_key(Sys.getenv("CENSUS_API_KEY"))

# Get list of variables from tidycensus
acs_complete <- load_variables(year = 2022, "acs5", cache = TRUE)

# Create a vector with variable names and descriptive names for ACS variables
acs_vars <- c("B01003_001", "B19013_001", "B17001_002", "B15003_017", "B25077_001", "B23025_005", "B02001_002", "B02001_003", "B03002_003")
acs_var_names <- c("Total Population", "Median Income", "Number in Poverty", "High School Graduate Count", "Median Home Price", "Number of Unemployed", "White Population", "Black Population", "Hispanic Population")

# # Create labeled vector of variable names for ACS variables
acs_vars <- setNames(acs_vars, acs_var_names)

# Create a vector with variable names and descriptive names for all variables
variable_names <- c(acs_vars, "deaths", "crude_rate")
descriptive_names <- c(acs_var_names, "Count of Opioid Deaths", "Opioid Deaths per 100k")

# Create labeled vector of variable names for all variables
variables <- setNames(variable_names, descriptive_names)

# Create a vector with only variable names not in acs_vars
outcome_variables <- (variables)[!((variables) %in% (acs_vars))]

# Add population data
county_pop <- get_acs(
        geography = "county",
        variables = acs_vars,
        year = 2022,
        survey = "acs5",
        geometry = TRUE,
        cache = TRUE
)

# Change from long to wide to create three variables with the value of estimate by each level of variable
county_pop_wide <- county_pop %>%
        pivot_wider(names_from = variable, values_from = c(estimate,moe))

#  Merge deaths to population data
county_map <- merge(county_pop_wide, 
        total_deaths[c("FIPS", "deaths", "ST_ABBREV", "STATE_NAME", "COUNTYNAME", "STATEFIPS", "COUNTYFIPS")], 
        by.x = "GEOID", by.y = "FIPS", all.x = TRUE)

# Remove _estimate from all variable names
names(county_map) <- str_replace(names(county_map), "estimate_", "")

# Initiatlize empty crude rate variable
county_map$crude_rate <- NA

# Replace outcome variable names with formatted names
names(county_map)[names(county_map) %in% outcome_variables] <- names(outcome_variables)

impute_deaths <- function(county_map, imputeNullValues) {
    if (imputeNullValues) {
        county_map$"Count of Opioid Deaths"[is.na(county_map$"Count of Opioid Deaths")] <- 10 / sqrt(2)
    }
    
    county_map$"Opioid Deaths per 100k" <- county_map$"Count of Opioid Deaths" / county_map$"Total Population" * 100000
    
    return(county_map)
}

# Shiny app
ui <- fluidPage(
    navbarPage(
        "Opioid Trends Dashboard",
        tabPanel("Descriptives",
            titlePanel("Visualizing Opioid Deaths in US Counties"),
            selectInput("variable", "Select Variable", 
                                    choices = names(variables)),
            checkboxInput("imputeNullValues", "Impute Null Values", value = TRUE),
            checkboxInput("logValues", "Apply Log Transform", value = TRUE),
            plotOutput("mapPlot", height = "50vh", hover = hoverOpts(id ="plot_hover")),
            # verbatimTextOutput("hover_info"),
            plotOutput("histPlot", height = "20vh"),
            tableOutput("table")
        ),
        tabPanel("Associations",
            titlePanel(paste("Associations Between Opioid Deaths and Demographic Variables in US Counties")),
            selectInput("x_variable", "Predictor Variable", 
                                    choices = names(acs_vars)),
            selectInput("y_variable", "Outcome Variable", 
                                    choices = names(outcome_variables)),
            checkboxInput("imputeNullReg", "Impute Null Values", value = TRUE),
            plotOutput("scatterPlot", height = "50vh"),
            tableOutput("regressionTable")
        ),
        tabPanel("Trends",
                titlePanel("Trends in Opoid Deaths in US Counties"),
                plotOutput("trendsPlot")
            ),
        tabPanel("Census Variables",
                titlePanel("Census Variables"),
                dataTableOutput("censusTable")
            )
    )
)

server <- function(input, output) {
    output$mapPlot <- renderPlot({
        variable <- input$variable
                
        ggplot(impute_deaths(county_map, input$imputeNullValues), aes(fill = !!sym(variable))) +
            geom_sf() +
            theme_void() +
            coord_sf(xlim = c(-125, -65), ylim = c(25, 50)) +
            scale_fill_viridis_c(option = "magma", trans = "log2", name = str_to_title(str_replace(variable, "_", " "))) +
            labs(title = paste("Choropleth of", str_to_title(str_replace(variable, "_", " ")), "in US Counties"))
    })

# output$hover_info <- renderPrint({
#     if (!is.null(input$plot_hover)) {
#         hover <- input$plot_hover
        
#         # Convert hover$x and hover$y to a spatial point
#         hover_point <- sf::st_point(c(hover$x, hover$y))

#         # Find the intersecting county
#         intersect_county <- sf::st_intersection(county_map, sf::st_point(c(hover$x, hover$y))       
#         # # Find the intersecting county
#         # intersect_county <- sf::st_intersection(county_map, hover_point)
        
#         # # Get the county name
#         # county_name <- intersect_county$COUNTYNAME
        
#         # # Print the county name
#         # print(county_name)
#     }
# })
    
    
    output$histPlot <- renderPlot({
        variable <- input$variable
        
        ggplot(impute_deaths(county_map, input$imputeNullValues), aes(x = if (input$logValues) log(!!sym(variable)) else !!sym(variable))) +
            geom_histogram(fill = "steelblue", color = "white", bins = 20) +
            labs(title = paste("Histogram of", str_to_title(str_replace(variable, "_", " ")))) +
            xlab(str_to_title(str_replace(variable, "_", " "))) +
            ylab("Frequency")
    })

        output$scatterPlot <- renderPlot({
        x_variable <- input$x_variable
        y_variable <- input$y_variable
        
        ggplot(impute_deaths(county_map, input$imputeNullReg), aes(x = !!sym(x_variable), y = !!sym(y_variable))) +
            geom_point() +
            coord_cartesian(xlim = c(0, max(impute_deaths(county_map, input$imputeNullReg)[[x_variable]], na.rm = TRUE) * 1.1)) +
            geom_smooth(method = "lm", se = FALSE) +
            labs(title = paste("Scatterplot of", str_to_title(str_replace(x_variable, "_", " ")), "vs", str_to_title(str_replace(y_variable, "_", " ")))) +
            xlab(str_to_title(str_replace(x_variable, "_", " "))) +
            ylab(str_to_title(str_replace(y_variable, "_", " ")))
    })

# Add table that shows skim results for selected variable
output$table <- renderTable({
    variable <- input$variable
    skim(impute_deaths(county_map, input$imputeNullValues)[[variable]])
})

# Create a formatted regression table output
output$regressionTable <- renderTable({
    x_variable <- input$x_variable
    y_variable <- input$y_variable
    
    model <- lm(impute_deaths(county_map, input$imputeNullReg)[[y_variable]] ~ impute_deaths(county_map, input$imputeNullReg)[[x_variable]])
    
    # Create a formatted summary of the model
    regression_summary <- summary(model)
    
    # Create a data frame with the coefficients and p-values
    regression_table <- data.frame(
            "Variable" = c("Intercept", str_to_title(str_replace(x_variable, "_", " "))),
            "Coefficient" = round(regression_summary$coefficients[,1], 2),
            "Std. Error" = round(regression_summary$coefficients[,2], 2),
            "t-Value" = round(regression_summary$coefficients[,3], 2),
            "P-Value" = round(regression_summary$coefficients[,4], 2)
    )

    regression_table
})

output$censusTable <- renderDataTable({
    acs_complete
})

# Create a plot of trend over time from 2020 to 2023
output$trendsPlot <- renderPlot({
    # Code for creating the plot of trend over time from 2020 to 2023
    # Create variable for date using Month and Year
    data$date <- as.Date(paste(data$Year, data$Month, "01", sep = "-"))

    # Create data_summary from data df
    data_summary <- data %>%
        group_by(date, US_region) %>%
        summarise(total_deaths = sum(deaths, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(US_region = ifelse(is.na(US_region), "Total", US_region))

    ggplot(data_summary, aes(x = date, y = total_deaths, color = US_region)) +
        geom_point() +
        geom_smooth(method = "loess") +
        scale_color_manual(values = c("black", "blue", "red", "green", "purple"), 
                        breaks = c("Total", "Northeast", "South", "Midwest", "West"),
                        labels = c("Total", "Northeast", "South", "Midwest", "West")) +
        labs(title = "Trend of Total Deaths from 2020 to 2023",
            x = "Date",
            y = "Total Deaths")

})




}

shinyApp(ui = ui, server = server)