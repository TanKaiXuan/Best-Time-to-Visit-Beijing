# Load packages -----------------------------------------------------
library(shiny)
library(ggplot2)
library(dplyr)
library(tools)
library(readr)
library(tidyverse)
library(cluster)
library(factoextra)
library(DT)
library(shinythemes)
library(kableExtra)


 
# Load data ---------------------------------------------------------
weather_data <- read_csv("data/pm2.5_data_2010_2014.csv")

# Pre-process data --------------------------------------------------
weather_data <- na.omit(weather_data)

# Define UI ---------------------------------------------------------
ui <- fluidPage(theme = shinytheme("spacelab"),
  tags$style(type="text/css", ".recalculating { opacity: 1.0; }"),
  titlePanel("Best Time to Visit Beijing"),
  
  tabsetPanel(id = "contents", 
              
    tabPanel("About Beijing", 
             mainPanel(
             tags$h2("Start from Scratch"),
             tags$br(),
             tags$h5("With a rich history spanning three millennia, Beijing has always been one of the most popular tourist hotspots in the world. You'll never get bored in this beautiful capital as the city has so much to offer! Wondering when is the best time to visit Beijing? Here we suggest the best time to visit Beijing based on historical meteorological data."),
             tags$br(),
             HTML(paste("<h4 align = 'center' style='color:#A2B9D1'><b> Average Temperature </b></h4> <br>")),
             tableOutput("temp_kable"),
             hr(), 
             br(),
             HTML(paste("<h3><b> Spring in Beijing: </b> </h3><br>")),
             HTML(paste("Spring typically starts from April to May. Spring can be delightful to visit as it's windy and flowers start to bloom. The temperature can be fluctutated and has large temperature difference between day and night. There is much wind and dust in spring, and sometimes sandstorms. ")),
          
             br(),
             tags$img(height = 600, 
             width = 900,
             src = "spring.jpg"),
             br(),
             # Horizontal line for visual separation
             hr(),
             
             br(),  
             HTML(paste("<h3><b> Summer in Beijing: </b></h3> <br>")),
             HTML(paste("Summer typically starts from June to the end of August. The temperature can goes up to more than 40 &#8451;. If you'd like to visit Beijing in summer, be prepared for the scorching sun, heavy rainfall and high humidity! ")),
            
             br(),
             tags$img(height = 600, 
                      width = 900, 
                      src = "summer.jpg"),
             br(),
             # Horizontal line for visual separation
             hr(),
             
             br(),    
             HTML(paste("<h3><b> Autumn in Beijing: </b></h3> <br>")),
             HTML(paste("The autumn (from September to November) generally has nice and sunny days, where you can enjoy comfortable autumn clean airs outdoor.")),
             
             br(),
             tags$img(height = 600, 
                      width = 900, 
                      src = "autumn.jpg"),
             br(),
             # Horizontal line for visual separation
             hr(),
             
             br(),    
             HTML(paste("<h3><b> Winter in Beijing: </b></h3> <br>")),
             HTML(paste("Winter generally starts from December to February. Beijing gets very dry and cold in winter. The winter months tend to have poorer air quality. However, if you're lucky, you may have the chance to see the snow! ")),
             
             br(),
             tags$img(height = 600, 
                      width = 900, 
                      src = "winter.jpg"),
             br(),
             # Horizontal line for visual separation
             hr(),
             
             br(),    
             HTML(paste("<h3><b> The 5th Season in Beijing: </b> </h3><br>")),
             br(),
             tags$img(height = 600, 
                      width = 900, 
                      src = "smog.jpg"),
             br(),
             # Horizontal line for visual separation
             hr()
             )
    ),
    
    tabPanel("Weather Clustering", 
             # Sidebar layout with a input and output definitions
             sidebarLayout(
               
               # Inputs: Select variables to plot
               sidebarPanel(
            
                 # select number of center
                 sliderInput(inputId = "k_center", 
                             label = "Cluster number:", 
                             min = 2, max = 11, 
                             value = 2),
                 hr(),
                 
                 # Select which features of weather to cluster
                 checkboxGroupInput(inputId = "selected_feature",
                                    label = "Select feature(s):",
                                    choices = c("PM2.5 concentration" = "pm2.5", 
                                                "Temperature" = "TEMP", 
                                                "Dew Point" = "DEWP", 
                                                "Pressure" = "PRES", 
                                                #"Combined wind direction" = "cbwd", 
                                                "Cumulated wind speed" = "Iws", 
                                                "Cumulated hours of snow" = "Is", 
                                                "Cumulated hours of rain" = "Ir"),
                                    selected = c("pm2.5", "TEMP"))
               ),
               
               mainPanel(
                 br(),
                 HTML(paste("<b> K-means Clustering: </b> <br>")),
                 br(),
                 # Show k-means plot
                 plotOutput(outputId = "kmeansplot"),
                 br(),  # a little bit of visual separation
                 hr(),
                 br(),
                 HTML(paste("<b> Hierarchical Clustering: </b> <br>")),
                 br(),
                 plotOutput(outputId = "clusterplot"),
                 hr()
               )
             )
             
    ),
    
    tabPanel("Time Recommendation",
             sidebarLayout(
               sidebarPanel(
                 br(),
                 HTML(paste("<strong> Select the range that you prefer: </strong> <br>")),
                 hr(),
                 # select range of pm2.5
                 sliderInput(inputId = "range_pm2.5", 
                             label = "PM2.5 concentration:*", 
                             min = 0, max = 800, 
                             value = 50),
                 # select range of temp
                 sliderInput(inputId = "range_temp", 
                             label = "Temperature:", 
                             min = -20, max = 50, 
                             value = c(15,25)),
                 # select range of dewp
                 sliderInput(inputId = "range_dewp", 
                             label = "Dew Point:", 
                             min = 0, max = 40, 
                             value = c(15,20)),
                 # select range of pres
                 sliderInput(inputId = "range_pres", 
                             label = "Pressure:", 
                             min = 800, max = 1200, 
                             value = c(950,1000)),
                 # select range of iws
                 sliderInput(inputId = "range_iws", 
                             label = "Cumulated wind speed:", 
                             min = 0, max = 500, 
                             value = c(0,100)),
                 # select range of is
                 sliderInput(inputId = "range_is", 
                             label = "Cumulated hours of snow:", 
                             min = 0, max = 24, 
                             value = c(0,4)),
                 # select range of ir
                 sliderInput(inputId = "range_ir", 
                             label = "Cumulated hours of rain:", 
                             min = 0, max = 48, 
                             value = c(0,4)),
                 hr(),
                 HTML(paste("<b>* PM2.5 Concentration: </b> <br>")),
                 br(),
                 HTML(paste("0-50  : Good <br>")),
                 HTML(paste("51-100: Moderate <br>")),
                 HTML(paste("101-150: Unhealthy for Sensitive Groups <br>")),
                 HTML(paste("151-200: Unhealthy <br>")),
                 HTML(paste("201-300: Very Unhealthy <br>")),
                 HTML(paste("301-500: Hazardous <br>"))
               ),
               
               mainPanel(
                 # Show pie plot
                 plotOutput(outputId = "recom_pie"),
                 hr(),
                 HTML(paste("<b> The time we recommended to visit Beijing is: </b> <br>")),
                 br(),
                 uiOutput(outputId = "recom"),
                 br()
               )
             )
    ),
    
    tabPanel("Graph Plotting", 
             
             # Sidebar layout with a input and output definitions
             sidebarLayout(
               
               # Inputs: Select variables to plot
               sidebarPanel(
                 # Inputs excluded for brevity
                 # Select variable for y-axis
                 selectInput(inputId = "y", 
                             label = "Y-axis:",
                             choices = c("PM2.5 concentration" = "pm2.5", 
                                         "Temperature" = "TEMP", 
                                         "Dew Point" = "DEWP",
                                         "Pressure" = "PRES", 
                                         "Cumulated hours of snow" = "Is", 
                                         "Cumulated hours of rain" = "Ir"),
                             selected = "pm2.5"),
                 
                 # Select variable for x-axis
                 selectInput(inputId = "x", 
                             label = "X-axis:",
                             choices = c("Day" = "day", 
                                         "Hour" = "hour",
                                         "Month" = "month"), 
                             selected = "Day"),
                 
                 # Select variable for color
                 selectInput(inputId = "z", 
                             label = "Color by:",
                             choices = c("Cumulated wind speed" = "Iws",
                                        "Combined wind direction" = "cbwd"),
                             selected = "Iws"),
                 
                 # Set alpha level
                 sliderInput(inputId = "alpha", 
                             label = "Alpha:", 
                             min = 0, max = 1, 
                             value = 0.5)
               ),
               
               # Output
               mainPanel(
                 br(),
                 # Select which years of weather to plot
                 checkboxGroupInput(inputId = "selected_month",
                                    label = "Select month(s):",
                                    choices = c("1", "2", "3", "4", "5", "6", 
                                                "7", "8", "9", "10", "11", "12"),
                                    selected = c("1", "2", "3"),
                                    inline = TRUE),
                 hr(),
                 
                 # Show scatterplot
                 plotOutput(outputId = "scatterplot"),
                 br(),  # a little bit of visual separation
                 
                 HTML(paste("<b> cv: </b> calm and variable &nbsp | &nbsp")),
                 HTML(paste("<b> NE: </b> northeast &nbsp | &nbsp")),
                 HTML(paste("<b> NW: </b> northwest &nbsp | &nbsp")),
                 HTML(paste("<b> SE: </b> southeast <br>")),
                 br(),
                 hr(),
                 
                 # Print number of obs plotted
                 uiOutput(outputId = "n"),
                 br()   # a little bit of visual separatio
               )
             )
    ),
  
    tabPanel("Data Details", 
             sidebarLayout(
               sidebarPanel(
                 br(),
                 HTML(paste("<b> The summary of Data: </b> <br>")),
                 br(),
                 uiOutput(outputId = "summary_pm2.5"),
                 br(),
                 uiOutput(outputId = "summary_temp"),
                 br(),
                 uiOutput(outputId = "summary_dewp"),
                 br(),
                 uiOutput(outputId = "summary_pres"),
                 br(),
                 uiOutput(outputId = "summary_iws"),
                 br(),
                 uiOutput(outputId = "summary_is"),
                 br(),
                 uiOutput(outputId = "summary_ir"),
                 br(),
                 # Show data table
                 checkboxInput(inputId = "show_data",
                               label = "Show data table",
                               value = TRUE)
               ),
               
               mainPanel(
                 # Horizontal line for visual separation
                 hr(),
                 DT::dataTableOutput(outputId = "weathertable")
               )
             )
    ),
    tabPanel("About Us", 
             tags$br(),
             tags$h2("About This Shiny Application"),
             tags$br(),
             tags$p("This Shiny application is developed for travelers to find the Best Time to visit Beijing based on meteorological information from 2010 to 2014.  The meteorological information are Air Pollution Level, Ambient Temperature, Dew Point, Atmospheric Pressure, Cumulated Wind Speed, Cumulated Hours of Snow and Cumulated Hours of Rain."),
             tags$p("The application is developed using R language on RStudio and deployed on the Shinyapps.io server. Various widgets are used for time recommendation and data visualization."),
             hr(),
             tags$h2("The Course"),
             tags$p("This project is part of the group assignment for the Principles of Data Science course (WQD7001), Master of Data Science - Faculty of Computer Science and Information Technology, University of Malaya"),
             tags$p("Date of completion: 10th Dec 2018"),
             hr(),
             tags$h2("The Team"),
             tags$p("WQD180050   Cheng Jiechao"),
             tags$p("WQD180024   Teng Lung Yun"),
             tags$p("WQD180037   Pau Kah Ho"),
             tags$p("WQD180063   Yong Wai Shin"),
             hr(),
             tags$h2("Documentations"),
             tags$h4("Slider"),
             tags$br(),
             HTML('<center><img src = "slider.png", height = 300, width = 500></center>'),
             tags$p("For everyone who wants to know the best time to visit Beijing, this is the perfect app.  This app allows users to customize their visit preferences from a myriad of attributes, such as Air Pollution Level, Ambient Temperature, Dew Point, Atmospheric Pressure, Cumulated Wind Speed, Cumulated Hours of Snow and Cumulated Hours of Rain."),
             tags$h4("Pie Chart"),
             tags$br(),
             HTML('<center><img src="pie.png"></center>'),
             tags$p("The selected factors are then translated into Recommended Months to visit Beijing."),
             tags$h4("Explore the Data with DotPlot"),
             HTML('<center><img src="dotplot.png" height = "300px", width = "500px"></center>'),
             tags$p("After that, user can delve into the details of The Best Months to find the best time to visit Beijing.  This additional feature will show an information graph that depicts user-preferred attributes.  With this addition, users will definitely be able to get a rough idea regarding the best time to travel in Beijing in the day and can accordingly make adjustments.  It is also very useful if you need to, say, meet someone at a particular time.")
    ),
    tabPanel("Supporting Documentation", 
             br(),
             tags$h2("Understanding the Codes "),
             br(),
             tags$p("The source code is available at",
             tags$a("GitHub.", href = "https://github.com/jetrobert/Best-Time-to-Visit-Beijing")),
             hr(),
             HTML(paste("<em> This Shiny App called Best-Time-to-Visit-Beijing helps 
                        users to identify the optimal time to visit Beijing by 
                        using data analysis with useful functions. <em> <br>")),
             br(),
             HTML(paste("<b> 1. The dataset </b> is collected from UCI repository. <br>")),
             br(),
             HTML(paste("<b> 2. read_csv() </b> is the special case of the general read_delim(). 
                        It's useful for reading the most common types of flat file data, 
                        comma separated values. <br>")),
             br(),
             HTML(paste("<b> 3. na.omit() </b> is useful for dealing with NAs in e.g., data frames. 
                        It returns the object with incomplete cases removed. <br>")),
             br(),
             HTML(paste("<b> 4. select() </b> keeps the columns based on conditions.
                        It's provided by Dplyr package in R. <br>")),
             br(),
             HTML(paste("<b> 5. scale() </b> is generic function whose default method centers 
                        and/or scales the columns of a numeric matrix. <br>")),
             br(),
             HTML(paste("<b> 6. paste() </b> converts its arguments (via as.character) 
                        to character strings, and concatenates them 
                        (separating them by the string given by sep). <br>")),
             br(),
             HTML(paste("<b> 7. group_by() </b> provided by Dplyr takes an existing tbl 
                        and converts it into a grouped tbl where operations are 
                        performed by 'group'. <br>")),
             br(),
             HTML(paste("<b> 8. summarise() </b> is typically used on grouped data created 
                        by group_by(). The output will have one row for each group. <br>")),
             br(),
             HTML(paste("<b> 9. rownames() </b> retrieves or sets the row or column names 
                        of a matrix-like object. <br>")),
             br(),
             HTML(paste("<b> 10. kmeans() </b> performs k-means clustering on a data matrix. <br>")),
             br(),
             HTML(paste("<b> 11. fviz_cluster() </b> provides ggplot2-based elegant visualization 
                        of partitioning methods including kmeans, pam etc. <br>")),
             br(),
             HTML(paste("<b> 12. dist() </b> computes and returns the auto-distance/similarity matrix 
                        between either rows or columns of a matrix/data frame, or a list, 
                        as well as the cross-distance matrix between two matrices/data 
                        frames/lists. <br>")),
             br(),
             HTML(paste("<b> 13. hclust() </b> is hierarchical cluster analysis on a set of 
                        dissimilarities and methods for analyzing it. <br>")),
             br(),
             HTML(paste("<b> 14. plot() </b> is a generic function for plotting of R objects. <br>")),
             br(),
             HTML(paste("<b> 15. filter() </b> finds rows/cases where conditions are true. 
                        Unlike base subsetting with [, rows where the condition evaluates
                        to NA are dropped. <br>")),
             br(),
             HTML(paste("<b> 16. unlist() </b> simplifies a list structure x to produce a vector 
                        which contains all the atomic components which occur in x. <br>")),
             br(),
             HTML(paste("<b> 17. table() </b> uses the cross-classifying factors to build a 
                        contingency table of the counts at each combination of factor 
                        levels. <br>")),
             br(),
             HTML(paste("<b> 18. names() </b> is a function to get or set the names of an object. <br>")),
             br(),
             HTML(paste("<b> 19. pie() </b> draws a pie chart. <br>")),
             br(),
             HTML(paste("<b> 20. sort() </b> sorts (or orders) a vector or factor (partially) into 
                        ascending or descending order. <br>")),
             br(),
             HTML(paste("<b> 21. ggplot() </b> initializes a ggplot object. It can be used to declare 
                        the input data frame for a graphic and to specify the set of plot 
                        aesthetics intended to be common throughout all subsequent layers 
                        unless specifically overridden. <br>")),
             br(),
             HTML(paste("<b> 22. factor() </b> is used to encode a vector as a factor (the terms 
                        ‘category’ and ‘enumerated type’ are also used for factors). 
                        If argument ordered is TRUE, the factor levels are assumed to be ordered. <br>")),
             br(),
             HTML(paste("<b> 23. summary() </b> is a generic function used to produce result 
                        summaries of the results of various model fitting functions. 
                        It invokes particular methods which depend on the class of the 
                        first argument. <br>"))
    )
  )
)

# Define server function --------------------------------------------
server <- function(input, output) {
  
  # Print average temperature table 
  output$temp_kable <- function(){
    temp_summary <- weather_data %>% select(month, TEMP) %>% group_by(month) %>%
      summarize("Average Temperature degree "= as.integer(mean(TEMP)), "Minimum Temperature degree" = min(TEMP), "Maximum Temperature degree" = max(TEMP))
    temp_summary$month <- c("Jan", "Feb", "March","April","May","June","July","Aug","Sept","Oct","Nov","Dec")
    temp_summary <- temp_summary %>% knitr::kable("html")  %>%   kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, position = "center")  %>% row_spec(0, bold = T)
    temp_summary <- gsub("degree","(&#8451;)", temp_summary)
    temp_summary <- gsub("month","Month", temp_summary)
  }
  # select a subset for clustering task
  weather_cluster_subset <- reactive({
    cluster_subset <- select(weather_data, month, pm2.5:PRES, Iws:Ir) %>% 
    group_by(month) %>% 
    summarize(pm2.5 = mean(pm2.5), DEWP = mean(DEWP), TEMP = mean(TEMP), 
              PRES = mean(PRES),  Iws = mean(Iws), Is = mean(Is), 
              Ir = mean(Ir), na.rm=TRUE) %>%
    data.frame()
    rownames(cluster_subset) <- cluster_subset[,1]
    cluster_subset <- scale(cluster_subset[,2:8])
    cluster_subset <- data.frame(cluster_subset)
    select(cluster_subset, input$selected_feature)
  })
  
  # Create k-means object the plotOutput function is expecting
  output$kmeansplot <- renderPlot({
    k2 <- kmeans(weather_cluster_subset(), centers = input$k_center, nstart = 12)
    fviz_cluster(k2, data = weather_cluster_subset())
  })
  
  # Create hierarchical clustering object the plotOutput function is expecting
  output$clusterplot <- renderPlot({
    dmx <- dist(weather_cluster_subset(), method = "euclidean")
    # Hierarchical clustering using Complete Linkage
    hc <- hclust(dmx, method = "complete" )
    # Plot the obtained dendrogram
    plot(hc, cex = 0.6, hang = -1)
    rect.hclust(hc, k = input$k_center, border = 2:5)
  })
  
  # measure the value that can be accepted
  month_day_recom <- reactive({
    recom_subset <- weather_data %>%
    filter(pm2.5 <= input$range_pm2.5) %>% 
    filter(TEMP >= input$range_temp[1], TEMP <= input$range_temp[2], DEWP >= input$range_dewp[1], DEWP <= input$range_dewp[2]) %>% 
    filter(PRES >= input$range_pres[1], PRES <= input$range_pres[2], Iws >= input$range_iws[1], Iws <= input$range_iws[2]) %>% 
    filter(Is >= input$range_is[1], Is <= input$range_is[2], Ir >= input$range_ir[1], Ir <= input$range_ir[2])
  })
  
  # plot the matched mont_day with pie chart
  output$recom_pie <- renderPlot({
    value_cnt <- as.numeric(count(month_day_recom()))
    if(value_cnt>0){
      recom_month <- unlist(select(month_day_recom(), month))
      month_sum <- table(recom_month)
      x <- month_sum
      labels <-  names(month_sum)
      piepercent<- round(100*x/sum(x), 1)
      # Plot the chart.
      pie(x, labels = piepercent, main = "Percentage Share of Month Recommendation (%)",col = rainbow(length(x)))
      legend("topright", labels, cex = 0.8, title = "Month",
             fill = rainbow(length(x)))
    } 
  })
  
  # print the matched mont_day
  output$recom <- renderUI({
    value_cnt <- as.numeric(count(month_day_recom()))
    if(value_cnt>0){
      recom_month <- unlist(select(month_day_recom(), month))
      recom_day <- unlist(select(month_day_recom(), day))
      month_day <- sort(unique(paste(recom_month, "-", recom_day)))
      HTML(paste("(year - month - day): this_year - ", month_day, "<br>"))
    } else {
      HTML(print("reset your range ! <br>"))
    }
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    month_subset <- filter(weather_data, month %in% input$selected_month)
    ggplot(data = month_subset, mapping = aes_string(x = input$x, y = input$y,
                                                     col = input$z)) +
      stat_smooth(colour='blue', span=0.2) +
      geom_point(alpha = input$alpha)
  })
  
  # Print number of weather plotted
  output$n <- renderUI({
    years <- weather_data$year %>%
      factor(levels = unique(weather_data$year)) 
    counts <- table(years)
    
    HTML(paste("There are", counts, "records of year", unique(weather_data$year), "in this dataset. <br>"))
  })
  
  # Print summary of pm2.5
  output$summary_pm2.5 <- renderUI({
    weather_sum <-select(weather_data, c("pm2.5")) %>%
      summary()
    HTML(paste("<b> PM2.5 </b>", weather_sum, "<br>"))
  })
  # Print summary of temp
  output$summary_temp <- renderUI({
    weather_sum <-select(weather_data, c("TEMP")) %>%
      summary()
    HTML(paste("<b> Temperature </b>", weather_sum, "<br>"))
  })
  # Print summary of dewp
  output$summary_dewp <- renderUI({
    weather_sum <-select(weather_data, c("DEWP")) %>%
      summary()
    HTML(paste("<b> Dew Point </b>", weather_sum, "<br>"))
  })
  # Print summary of press
  output$summary_pres <- renderUI({
    weather_sum <-select(weather_data, c("PRES")) %>%
      summary()
    HTML(paste("<b> Pressure </b>", weather_sum, "<br>"))
  })
  # Print summary of iws
  output$summary_iws <- renderUI({
    weather_sum <-select(weather_data, c("Iws")) %>%
      summary()
    HTML(paste("<b> Cumulated wind speed </b>", weather_sum, "<br>"))
  })
  # Print summary of is
  output$summary_is <- renderUI({
    weather_sum <-select(weather_data, c("Is")) %>%
      summary()
    HTML(paste("<b> Cumulated hours of snow </b>", weather_sum, "<br>"))
  })
  # Print summary of ir
  output$summary_ir <- renderUI({
    weather_sum <-select(weather_data, c("Ir")) %>%
      summary()
    HTML(paste("<b> Cumulated hours of rain </b>", weather_sum, "<br>"))
  })
  # Print data table if checked
  output$weathertable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = weather_data[, 1:8], 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    })
}

# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)
