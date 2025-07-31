library(shiny)
library(DT)
library(shinydashboard)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(title = "Book Data Analysis"),
  dashboardSidebar(
    fileInput("file1", "Upload CSV", accept = ".csv")
  ),
  dashboardBody(
    fluidRow(
      box(title = "Quantitative and Qualitative Variables", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
          uiOutput("quantitative_header"),
          DTOutput("quantitative_table"),
          uiOutput("qualitative_header"),
          DTOutput("qualitative_table"))
    ),
    fluidRow(
      box(title = "Missing Values Count", status = "warning", width = 12, solidHeader = TRUE, collapsible = TRUE,
          uiOutput("missing_values_output"))
    ),
    fluidRow(
      box(title = "Visualizations", status = "info", width = 12, solidHeader = TRUE, collapsible = TRUE,
          plotOutput("boxplot_price"),
          plotOutput("pie_chart_genre"),
          plotOutput("barplot_sales_volume"),
          plotOutput("density_plot_pages"),
          plotOutput("barplot_ratings"))
    ),
    fluidRow(
      box(title = "Predictive Modeling (Simple Linear Regression)", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
          selectInput("response_variable", "Select Predictor Variable:", choices = c("Price", "Pages", "Rating")),
          uiOutput("model_results"))
    ),
    fluidRow(
      box(title = "Outlier Detection", status = "success", width = 12, solidHeader = TRUE, collapsible = TRUE,
          selectInput("outlier_method", "Select Outlier Detection Method:",
                      choices = c("IQR Method" = "IQR", "Z-Score Method" = "ZScore"), selected = "IQR"),
          DTOutput("outlier_table"),
          uiOutput("outlier_plots"))
    )
  )
)

server <- function(input, output, session) {
  
  book_data <- reactiveVal(NULL)
  
# task 3.3
  detect_outliers <- function(data, method) {
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    outliers <- data.frame()
    for (col in numeric_cols) {
      vals <- data[[col]]
      if (method == "IQR") {
        Q1 <- quantile(vals, 0.25, na.rm = TRUE)
        Q3 <- quantile(vals, 0.75, na.rm = TRUE)
        IQR_val <- Q3 - Q1
        outlier_indices <- which(vals < (Q1 - 1.5 * IQR_val) | vals > (Q3 + 1.5 * IQR_val))
      } else {
        z_scores <- scale(vals)
        outlier_indices <- which(abs(z_scores) > 3)
      }
      if (length(outlier_indices) > 0) {
        outliers <- rbind(outliers, data.frame(Variable = col, Index = outlier_indices, Value = vals[outlier_indices]))
      }
    }
    return(outliers)
  }
  
  observeEvent(input$file1, {
    req(input$file1)
    tryCatch({
      data <- read.csv(input$file1$datapath)
      data <- data[, !(names(data) %in% c("BookID", "Title"))]
      data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], factor)
      book_data(data)
      showNotification("File successfully uploaded!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      book_data(NULL)
    })
  })
  
#task 3.1
  quantitative <- reactive({
    req(book_data())
    names(book_data())[sapply(book_data(), is.numeric)]
  })
  
  qualitative <- reactive({
    req(book_data())
    names(book_data())[sapply(book_data(), is.factor) | sapply(book_data(), is.character)]
  })
  
  output$quantitative_header <- renderUI({ req(quantitative()); h4("Quantitative Variables") })
  output$qualitative_header <- renderUI({ req(qualitative()); h4("Qualitative Variables") })
  
  output$quantitative_table <- renderDT({
    datatable(data.frame(Variable = quantitative()), options = list(paging = FALSE, searching = FALSE, info = FALSE))
  })
  output$qualitative_table <- renderDT({
    datatable(data.frame(Variable = qualitative()), options = list(paging = FALSE, searching = FALSE, info = FALSE))
  })
 
#task 3.2 
  observeEvent(book_data(), {
    req(book_data())
    missing_values <- colSums(is.na(book_data()))
    output$missing_values_output <- renderUI({
      missing_data_table <- datatable(data.frame(Column = names(missing_values), Missing_Values = missing_values), options = list(paging = FALSE, searching = FALSE, info = FALSE))
      tagList(h4("Missing Values in Each Column:"), missing_data_table)
    })
  })
 
#task 3.4
  observeEvent(book_data(), {
    req(book_data())
    data <- book_data()
    if ("Price" %in% names(data)) {
      data$LogPrice <- log1p(data$Price)
      output$boxplot_price <- renderPlot({
        boxplot(data$LogPrice, main = "Boxplot of Log(Price)", ylab = "Log(Price)", col = "lightgreen")
      })
    }
    if ("Genre" %in% names(data)) {
      output$pie_chart_genre <- renderPlot({
        genre_counts <- table(data$Genre)
        pie(genre_counts, main = "Pie Chart of Genre", col = rainbow(length(genre_counts)), labels = paste(names(genre_counts), "\n", genre_counts))
      })
    }
    if ("SalesVolume" %in% names(data)) {
      data$LogSalesVolume <- log1p(data$SalesVolume)
      output$barplot_sales_volume <- renderPlot({
        sales_volume_bins <- cut(data$LogSalesVolume, breaks = 10)
        barplot(table(sales_volume_bins), main = "Bar Plot of Sales Volume", col = "lightblue")
      })
    }
    if ("Pages" %in% names(data)) {
      output$density_plot_pages <- renderPlot({
        plot(density(data$Pages), main = "Density Plot of Pages", xlab = "Pages", col = "blue", lwd = 2)
      })
    }
    if ("Rating" %in% names(data)) {
      output$barplot_ratings <- renderPlot({
        rating_counts <- table(data$Rating)
        barplot(rating_counts, main = "Bar Plot of Ratings", col = "lightcoral")
      })
    }
  })
  
#task 3.5
  observeEvent(input$response_variable, {
    req(book_data(), input$response_variable)
    data <- book_data()
    predictor <- input$response_variable
    if (!(predictor %in% names(data)) || !("SalesVolume" %in% names(data))) {
      output$model_results <- renderUI({ p("Required columns missing.") })
      return()
    }
    model_data <- na.omit(data[, c("SalesVolume", predictor)])
    if (!is.numeric(model_data[[predictor]])) {
      output$model_results <- renderUI({ p("Selected predictor is not numeric.") })
      return()
    }
    model <- lm(SalesVolume ~ ., data = model_data)
    predictions <- predict(model)
    mse <- mean((model_data$SalesVolume - predictions)^2)
    r_squared <- summary(model)$r.squared
    output$model_results <- renderUI({
      tagList(
        h4(paste("Regression: SalesVolume ~", predictor)),
        p(paste("MSE:", round(mse, 4))),
        p(paste("R-squared:", round(r_squared, 4))),
        DTOutput("coeff_table"),
        plotOutput("regression_plot")
      )
    })
    output$coeff_table <- renderDT({
      datatable(data.frame(Variable = names(model$coefficients),
                           Coefficient = round(model$coefficients, 4)))
    })
    output$regression_plot <- renderPlot({
      ggplot(model_data, aes_string(x = predictor, y = "SalesVolume")) +
        geom_point(color = "blue") +
        geom_smooth(method = "lm", col = "red", se = FALSE) +
        theme_minimal()
    })
  })
  
  # Reactive outlier results
  outlier_results <- reactive({
    req(book_data(), input$outlier_method)
    detect_outliers(book_data(), input$outlier_method)
  })
  
  # Outlier table
  output$outlier_table <- renderDT({
    outliers <- outlier_results()
    if (nrow(outliers) > 0) {
      datatable(outliers, options = list(pageLength = 10))
    } else {
      datatable(data.frame(Message = "No outliers detected."), options = list(dom = 't'))
    }
  })
  
  # Outlier plots
  output$outlier_plots <- renderUI({
    req(book_data())
    outliers <- outlier_results()
    data <- book_data()
    method <- input$outlier_method
    
    plot_list <- lapply(unique(outliers$Variable), function(var) {
      output[[paste0("plot_", var)]] <- renderPlot({
        if (method == "IQR") {
          boxplot(data[[var]], main = paste("Boxplot of", var), col = "orange")
        } else {
          ggplot(data, aes_string(x = var)) +
            geom_histogram(bins = 30, fill = "skyblue", color = "black") +
            ggtitle(paste("Histogram of", var, "(Z-Score Method)")) +
            theme_minimal()
        }
      })
      plotOutput(paste0("plot_", var))
    })
    do.call(tagList, plot_list)
  })
}

shinyApp(ui = ui, server = server)
