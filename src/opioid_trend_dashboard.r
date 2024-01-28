library(shiny)

# Read the data
file_path <- "data/VSRR_Provisional_County-Level_Drug_Overdose_Death_Counts.csv"
data <- read.csv(file_path)

# Define the UI
ui <- fluidPage(
    titlePanel("Population Histogram and Scatterplot"),
    sidebarLayout(
        sidebarPanel(
            selectInput("hist_variable", "Select Variable for Histogram", choices = names(data)[sapply(data, is.numeric)], selected = "E_TOTPOP"),
            checkboxInput("log_transform_hist", "Apply Log Transformation to Histogram Variable", value = FALSE),
            selectInput("scatter_x_variable", "Select X Variable for Scatterplot", choices = names(data)[sapply(data, is.numeric)], selected = "E_TOTPOP"),
            checkboxInput("log_transform_x", "Apply Log Transformation to X Variable", value = FALSE),
            selectInput("scatter_y_variable", "Select Y Variable for Scatterplot", choices = names(data)[sapply(data, is.numeric)], selected = "E_UNINSUR"),
            checkboxInput("log_transform_y", "Apply Log Transformation to Y Variable", value = FALSE),
            tableOutput("output_table")  # Move output_table to sidebar panel
        ),
        mainPanel(
            plotOutput("histogram"),
            plotOutput("scatterplot")
        )
    )
)

# Define the server
server <- function(input, output) {
    output$histogram <- renderPlot({
        var <- data[[input$hist_variable]]
        if (input$log_transform_hist) {
            var <- ifelse(var == 0, var + 1e-10, var)
            var <- log(var)
        }
        hist(var, main = paste(input$hist_variable, "Histogram"), xlab = input$hist_variable)
    })

    output$scatterplot <- renderPlot({
        x_var <- data[[input$scatter_x_variable]]
        y_var <- data[[input$scatter_y_variable]]

        if (input$log_transform_x) {
            x_var <- ifelse(x_var == 0, x_var + 1e-10, x_var)
            x_var <- log(x_var)
        }
        if (input$log_transform_y) {
            y_var <- ifelse(y_var == 0, y_var + 1e-10, y_var)
            y_var <- log(y_var)
        }

        lm_model <- lm(y_var ~ x_var)
        formula_text <- paste("y =", round(coef(lm_model)[1], 2), "+", round(coef(lm_model)[2], 2), "x")

        plot(x_var, y_var, main = "Scatterplot", xlab = paste(ifelse(input$log_transform_x, "Log of", ""), input$scatter_x_variable), ylab = paste(ifelse(input$log_transform_y, "Log of", ""), input$scatter_y_variable))
        abline(lm_model, col = "red")
        text(x = mean(x_var), y = mean(y_var), labels = formula_text, pos = 3, cex = 1.2)
    })

    output$output_table <- renderTable({  # Fix output table name
        x_var <- data[[input$scatter_x_variable]]
        y_var <- data[[input$scatter_y_variable]]

        if (input$log_transform_x) {
            x_var <- ifelse(x_var == 0, x_var + 1e-10, x_var)
            x_var <- log(x_var)
        }
        if (input$log_transform_y) {
            y_var <- ifelse(y_var == 0, y_var + 1e-10, y_var)
            y_var <- log(y_var)
        }

        lm_model <- lm(y_var ~ x_var)
        output_table <- summary(lm_model)$coefficients
        output_table <- round(output_table, 2)
        output_table <- cbind(Variable = rownames(output_table), output_table)
        colnames(output_table)[2:4] <- c("Estimate", "Std. Error", "t value")
        output_table <- output_table[, c("Variable", "Estimate", "Std. Error", "t value")]
        output_table
    })
}


# Run the Shiny app
shinyApp(ui = ui, server = server)
