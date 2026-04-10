library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(shinycssloaders)
library(data.table)

df <- fread("C:/Users/HP/Downloads/winemag.csv", encoding = "UTF-8")
df <- as.data.frame(df)

df$country[is.na(df$country) | df$country == ""] <- "Unknown"
df$province[is.na(df$province) | df$province == ""] <- "Unknown"
df$variety[is.na(df$variety) | df$variety == ""] <- "Unknown"
df$region_1[is.na(df$region_1) | df$region_1 == ""] <- "Unknown"
df$taster_name[is.na(df$taster_name) | df$taster_name == ""] <- "Unknown"
df$winery[is.na(df$winery) | df$winery == ""] <- "Unknown"
df$designation[is.na(df$designation) | df$designation == ""] <- "Unknown"

df$price <- as.numeric(df$price)
df$price[is.na(df$price)] <- median(df$price, na.rm = TRUE)
df$points <- as.numeric(df$points)

df$price_bin <- cut(df$price,
                    breaks = c(0, 10, 20, 30, 50, 100, 200, Inf),
                    labels = c("Under10", "10to20", "20to30",
                               "30to50", "50to100", "100to200", "Over200"),
                    include.lowest = TRUE)

df$points_bin <- cut(df$points,
                     breaks = c(79, 84, 87, 90, 93, 96, 100),
                     labels = c("80-84", "85-87", "88-90",
                                "91-93", "94-96", "97-100"),
                     include.lowest = TRUE)

top_countries <- df[df$country != "Unknown", ] %>%
  count(country, sort = TRUE) %>%
  head(20) %>% pull(country) %>% as.character()

top_varieties <- df[df$variety != "Unknown", ] %>%
  count(variety, sort = TRUE) %>%
  head(30) %>% pull(variety) %>% as.character()


ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Wine Dashboard", titleWidth = 280),
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Country Analysis", tabName = "country", icon = icon("globe")),
      menuItem("Variety Analysis", tabName = "variety", icon = icon("flask")),
      menuItem("Distributions", tabName = "distribution", icon = icon("chart-area")),
      menuItem("Price vs Points", tabName = "relationship", icon = icon("random")),
      menuItem("Taster Analysis", tabName = "taster", icon = icon("user")),
      menuItem("Top Wines", tabName = "topwines", icon = icon("trophy")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table")),
      hr(),
      h4("  Filters", style = "color:white; padding-left:15px;"),
      sliderInput("ptsf", "Points:", min = 80, max = 100, value = c(80, 100)),
      sliderInput("pricef", "Price:", min = 0, max = 500, value = c(0, 500)),
      selectInput("countryf", "Country:", choices = c("All", top_countries), selected = "All"),
      selectInput("varietyf", "Variety:", choices = c("All", top_varieties), selected = "All"),
      actionButton("resetf", "Reset Filters", icon = icon("refresh"), style = "margin:10px;width:90%;")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(".content-wrapper{background-color:#f4f6f9;}"))),
    tabItems(
      
      tabItem(tabName = "overview",
              h2("Wine Reviews Overview"),
              fluidRow(
                valueBoxOutput("k1", width = 3),
                valueBoxOutput("k2", width = 3),
                valueBoxOutput("k3", width = 3),
                valueBoxOutput("k4", width = 3)
              ),
              fluidRow(
                box(title = "Points Distribution", status = "primary", solidHeader = TRUE, width = 8,
                    withSpinner(plotlyOutput("ov1", height = "350px"))),
                box(title = "Top 10 Countries", status = "primary", solidHeader = TRUE, width = 4,
                    withSpinner(plotlyOutput("ov2", height = "350px")))
              ),
              fluidRow(
                box(title = "Price Segments", status = "success", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("ov3", height = "320px"))),
                box(title = "Top 10 Varieties", status = "info", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("ov4", height = "320px")))
              )
      ),
      
      tabItem(tabName = "country",
              h2("Country and Region Analysis"),
              fluidRow(
                box(title = "Avg Points by Country", status = "primary", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("c1", height = "420px"))),
                box(title = "Avg Price by Country", status = "primary", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("c2", height = "420px")))
              ),
              fluidRow(
                box(title = "Country Bubble Chart", status = "success", solidHeader = TRUE, width = 12,
                    withSpinner(plotlyOutput("c3", height = "450px")))
              ),
              fluidRow(
                box(title = "Province Drill-Down", status = "info", solidHeader = TRUE, width = 6,
                    selectInput("ctysel", "Select Country:", choices = top_countries, selected = top_countries[1]),
                    withSpinner(plotlyOutput("c4", height = "380px"))),
                box(title = "Points by Country Box Plot", status = "warning", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("c5", height = "430px")))
              )
      ),
      
      tabItem(tabName = "variety",
              h2("Variety Analysis"),
              fluidRow(
                box(title = "Top Varieties by Avg Points", status = "primary", solidHeader = TRUE, width = 6,
                    sliderInput("vmin", "Min Reviews:", min = 10, max = 500, value = 100),
                    withSpinner(plotlyOutput("v1", height = "400px"))),
                box(title = "Top Varieties by Count", status = "primary", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("v2", height = "450px")))
              ),
              fluidRow(
                box(title = "Variety Bubble Chart", status = "success", solidHeader = TRUE, width = 12,
                    withSpinner(plotlyOutput("v3", height = "450px")))
              ),
              fluidRow(
                box(title = "Points by Variety", status = "info", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("v4", height = "400px"))),
                box(title = "Price by Variety", status = "warning", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("v5", height = "400px")))
              )
      ),
      
      tabItem(tabName = "distribution",
              h2("Price and Points Distributions"),
              fluidRow(
                box(title = "Points Histogram", status = "primary", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("d1", height = "350px"))),
                box(title = "Price Histogram", status = "primary", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("d2", height = "350px")))
              ),
              fluidRow(
                box(title = "Points by Price Segment", status = "success", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("d3", height = "350px"))),
                box(title = "Price by Points Segment", status = "info", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("d4", height = "350px")))
              ),
              fluidRow(
                box(title = "Summary Statistics", status = "warning", solidHeader = TRUE, width = 12,
                    withSpinner(DTOutput("d5")))
              )
      ),
      
      tabItem(tabName = "relationship",
              h2("Price vs Points Relationship"),
              fluidRow(
                box(title = "Controls", status = "primary", solidHeader = TRUE, width = 12,
                    column(4, selectInput("rc", "Color By:",
                                          choices = c("country", "variety", "points_bin", "price_bin"), selected = "points_bin")),
                    column(4, sliderInput("rs", "Sample Size:", min = 1000, max = 15000, value = 5000, step = 1000)),
                    column(4, checkboxInput("rl", "Log Scale Price", TRUE))
                )
              ),
              fluidRow(
                box(title = "Scatter Plot", status = "primary", solidHeader = TRUE, width = 8,
                    withSpinner(plotlyOutput("r1", height = "500px"))),
                box(title = "Correlation", status = "primary", solidHeader = TRUE, width = 4,
                    withSpinner(plotlyOutput("r2", height = "250px")),
                    hr(),
                    uiOutput("r3"))
              ),
              fluidRow(
                box(title = "Avg Points by Price Bin", status = "success", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("r4", height = "350px"))),
                box(title = "Avg Price by Points Bin", status = "info", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("r5", height = "350px")))
              )
      ),
      
      tabItem(tabName = "taster",
              h2("Taster Analysis"),
              fluidRow(
                box(title = "Reviews per Taster", status = "primary", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("t1", height = "420px"))),
                box(title = "Avg Points by Taster", status = "primary", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("t2", height = "420px")))
              ),
              fluidRow(
                box(title = "Points by Taster Box Plot", status = "success", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("t3", height = "400px"))),
                box(title = "Taster x Country Heatmap", status = "warning", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("t4", height = "400px")))
              )
      ),
      
      tabItem(tabName = "topwines",
              h2("Top Wines and Best Value"),
              fluidRow(
                box(title = "Top 20 Highest Rated", status = "primary", solidHeader = TRUE, width = 12,
                    withSpinner(DTOutput("tw1")))
              ),
              fluidRow(
                box(title = "Top 20 Best Value", status = "success", solidHeader = TRUE, width = 12,
                    withSpinner(DTOutput("tw2")))
              ),
              fluidRow(
                box(title = "Top Wineries", status = "info", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("tw3", height = "420px"))),
                box(title = "Most Expensive", status = "danger", solidHeader = TRUE, width = 6,
                    withSpinner(DTOutput("tw4")))
              )
      ),
      
      tabItem(tabName = "data",
              h2("Data Explorer"),
              fluidRow(
                box(title = "Filtered Data", status = "primary", solidHeader = TRUE, width = 12,
                    downloadButton("dl", "Download CSV"),
                    br(), br(),
                    withSpinner(DTOutput("dt1")))
              )
      )
    )
  )
)


server <- function(input, output, session) {
  
  fd <- reactive({
    d <- df
    d <- d[d$points >= input$ptsf[1] & d$points <= input$ptsf[2], ]
    d <- d[d$price >= input$pricef[1] & d$price <= input$pricef[2], ]
    if (input$countryf != "All") d <- d[d$country == input$countryf, ]
    if (input$varietyf != "All") d <- d[d$variety == input$varietyf, ]
    d
  })
  
  observeEvent(input$resetf, {
    updateSliderInput(session, "ptsf", value = c(80, 100))
    updateSliderInput(session, "pricef", value = c(0, 500))
    updateSelectInput(session, "countryf", selected = "All")
    updateSelectInput(session, "varietyf", selected = "All")
  })
  
  output$k1 <- renderValueBox({
    valueBox(format(nrow(fd()), big.mark = ","), "Total Reviews",
             icon = icon("glass-cheers"), color = "purple")
  })
  
  output$k2 <- renderValueBox({
    valueBox(round(mean(fd()$points, na.rm = TRUE), 1), "Avg Points",
             icon = icon("star"), color = "blue")
  })
  
  output$k3 <- renderValueBox({
    avg_p <- round(mean(fd()$price, na.rm = TRUE), 0)
    valueBox(paste0("$", avg_p), "Avg Price",
             icon = icon("tag"), color = "green")
  })
  
  output$k4 <- renderValueBox({
    valueBox(length(unique(fd()$country)), "Countries",
             icon = icon("globe"), color = "yellow")
  })
  
  output$ov1 <- renderPlotly({
    d <- fd() %>% count(points) %>% arrange(points)
    plot_ly(d, x = ~points, y = ~n, type = "bar",
            marker = list(color = "#8e44ad")) %>%
      layout(xaxis = list(title = "Points", dtick = 1),
             yaxis = list(title = "Count"))
  })
  
  output$ov2 <- renderPlotly({
    d <- fd() %>% filter(country != "Unknown") %>%
      count(country, sort = TRUE) %>% head(10) %>% arrange(n)
    plot_ly(d, y = ~reorder(country, n), x = ~n,
            type = "bar", orientation = "h",
            marker = list(color = "#8e44ad")) %>%
      layout(xaxis = list(title = "Reviews"),
             yaxis = list(title = ""), margin = list(l = 100))
  })
  
  output$ov3 <- renderPlotly({
    d <- fd() %>% filter(!is.na(price_bin)) %>% count(price_bin)
    plot_ly(d, labels = ~price_bin, values = ~n,
            type = "pie", hole = 0.45, textinfo = "label+percent")
  })
  
  output$ov4 <- renderPlotly({
    d <- fd() %>% filter(variety != "Unknown") %>%
      count(variety, sort = TRUE) %>% head(10) %>% arrange(n)
    plot_ly(d, y = ~reorder(variety, n), x = ~n,
            type = "bar", orientation = "h",
            marker = list(color = "#2980b9")) %>%
      layout(xaxis = list(title = "Reviews"),
             yaxis = list(title = ""), margin = list(l = 140))
  })
  
  output$c1 <- renderPlotly({
    d <- fd() %>% filter(country != "Unknown") %>%
      group_by(country) %>%
      summarise(avg = round(mean(points, na.rm = TRUE), 1),
                n = n(), .groups = "drop") %>%
      filter(n >= 50) %>% top_n(15, avg) %>% arrange(avg)
    plot_ly(d, y = ~reorder(country, avg), x = ~avg,
            type = "bar", orientation = "h",
            marker = list(color = "#8e44ad"),
            text = ~avg, textposition = "outside") %>%
      layout(xaxis = list(title = "Avg Points",
                          range = c(min(d$avg) - 1, max(d$avg) + 1.5)),
             yaxis = list(title = ""), margin = list(l = 110))
  })
  
  output$c2 <- renderPlotly({
    d <- fd() %>% filter(country != "Unknown") %>%
      group_by(country) %>%
      summarise(avg = round(mean(price, na.rm = TRUE), 1),
                n = n(), .groups = "drop") %>%
      filter(n >= 50) %>% top_n(15, avg) %>% arrange(avg)
    plot_ly(d, y = ~reorder(country, avg), x = ~avg,
            type = "bar", orientation = "h",
            marker = list(color = "#27ae60"),
            text = ~avg, textposition = "outside") %>%
      layout(xaxis = list(title = "Avg Price"),
             yaxis = list(title = ""), margin = list(l = 110))
  })
  
  output$c3 <- renderPlotly({
    d <- fd() %>% filter(country != "Unknown") %>%
      group_by(country) %>%
      summarise(ap = round(mean(points, na.rm = TRUE), 1),
                apr = round(mean(price, na.rm = TRUE), 1),
                n = n(), .groups = "drop") %>%
      filter(n >= 30) %>% top_n(20, n)
    plot_ly(d, x = ~apr, y = ~ap, size = ~n, color = ~country,
            type = "scatter", mode = "markers",
            marker = list(sizemode = "diameter",
                          sizeref = max(d$n) / 50, opacity = 0.7),
            text = ~paste0(country, " | Pts:", ap,
                           " | Price:", apr, " | n:", n),
            hoverinfo = "text") %>%
      layout(xaxis = list(title = "Avg Price"),
             yaxis = list(title = "Avg Points"), showlegend = FALSE)
  })
  
  output$c4 <- renderPlotly({
    d <- fd() %>%
      filter(country == input$ctysel, province != "Unknown") %>%
      group_by(province) %>%
      summarise(avg = round(mean(points, na.rm = TRUE), 1),
                n = n(), .groups = "drop") %>%
      filter(n >= 5) %>% top_n(15, n) %>% arrange(avg)
    plot_ly(d, y = ~reorder(province, avg), x = ~avg,
            type = "bar", orientation = "h",
            marker = list(color = "#2ecc71"),
            text = ~avg, textposition = "outside") %>%
      layout(xaxis = list(title = "Avg Points"),
             yaxis = list(title = ""), margin = list(l = 150))
  })
  
  output$c5 <- renderPlotly({
    tp <- fd() %>% filter(country != "Unknown") %>%
      count(country, sort = TRUE) %>% head(8) %>% pull(country)
    d <- fd() %>% filter(country %in% tp)
    plot_ly(d, y = ~points, color = ~country, type = "box") %>%
      layout(yaxis = list(title = "Points"), showlegend = FALSE)
  })
  
  output$v1 <- renderPlotly({
    d <- fd() %>% filter(variety != "Unknown") %>%
      group_by(variety) %>%
      summarise(avg = round(mean(points, na.rm = TRUE), 1),
                n = n(), .groups = "drop") %>%
      filter(n >= input$vmin) %>% top_n(15, avg) %>% arrange(avg)
    plot_ly(d, y = ~reorder(variety, avg), x = ~avg,
            type = "bar", orientation = "h",
            marker = list(color = "#8e44ad"),
            text = ~avg, textposition = "outside") %>%
      layout(xaxis = list(title = "Avg Points",
                          range = c(min(d$avg) - 0.5, max(d$avg) + 1)),
             yaxis = list(title = ""), margin = list(l = 160))
  })
  
  output$v2 <- renderPlotly({
    d <- fd() %>% filter(variety != "Unknown") %>%
      count(variety, sort = TRUE) %>% head(15) %>% arrange(n)
    plot_ly(d, y = ~reorder(variety, n), x = ~n,
            type = "bar", orientation = "h",
            marker = list(color = "#2980b9")) %>%
      layout(xaxis = list(title = "Count"),
             yaxis = list(title = ""), margin = list(l = 160))
  })
  
  output$v3 <- renderPlotly({
    d <- fd() %>% filter(variety != "Unknown") %>%
      group_by(variety) %>%
      summarise(ap = round(mean(points, na.rm = TRUE), 1),
                apr = round(mean(price, na.rm = TRUE), 1),
                n = n(), .groups = "drop") %>%
      filter(n >= 50) %>% top_n(25, n)
    plot_ly(d, x = ~apr, y = ~ap, size = ~n, text = ~variety,
            type = "scatter", mode = "markers+text",
            marker = list(opacity = 0.7, sizemode = "diameter",
                          sizeref = max(d$n) / 40),
            textposition = "top center",
            textfont = list(size = 8)) %>%
      layout(xaxis = list(title = "Avg Price"),
             yaxis = list(title = "Avg Points"), showlegend = FALSE)
  })
  
  output$v4 <- renderPlotly({
    tp <- fd() %>% filter(variety != "Unknown") %>%
      count(variety, sort = TRUE) %>% head(8) %>% pull(variety)
    d <- fd() %>% filter(variety %in% tp)
    plot_ly(d, y = ~points, color = ~variety, type = "box") %>%
      layout(yaxis = list(title = "Points"), showlegend = FALSE)
  })
  
  output$v5 <- renderPlotly({
    tp <- fd() %>% filter(variety != "Unknown") %>%
      count(variety, sort = TRUE) %>% head(8) %>% pull(variety)
    d <- fd() %>% filter(variety %in% tp)
    plot_ly(d, y = ~price, color = ~variety, type = "box") %>%
      layout(yaxis = list(title = "Price"), showlegend = FALSE)
  })
  
  output$d1 <- renderPlotly({
    plot_ly(fd(), x = ~points, type = "histogram", nbinsx = 21,
            marker = list(color = "#8e44ad",
                          line = list(color = "white", width = 0.5))) %>%
      layout(xaxis = list(title = "Points"),
             yaxis = list(title = "Frequency"))
  })
  
  output$d2 <- renderPlotly({
    d <- fd() %>% filter(price <= 200)
    plot_ly(d, x = ~price, type = "histogram", nbinsx = 40,
            marker = list(color = "#27ae60",
                          line = list(color = "white", width = 0.5))) %>%
      layout(xaxis = list(title = "Price (showing under 200)"),
             yaxis = list(title = "Frequency"))
  })
  
  output$d3 <- renderPlotly({
    d <- fd() %>% filter(!is.na(price_bin)) %>%
      group_by(price_bin) %>%
      summarise(avg = round(mean(points, na.rm = TRUE), 1), .groups = "drop")
    plot_ly(d, x = ~price_bin, y = ~avg, type = "bar",
            marker = list(color = "#8e44ad"),
            text = ~avg, textposition = "outside") %>%
      layout(xaxis = list(title = "Price Segment"),
             yaxis = list(title = "Avg Points"))
  })
  
  output$d4 <- renderPlotly({
    d <- fd() %>% filter(!is.na(points_bin)) %>%
      group_by(points_bin) %>%
      summarise(avg = round(mean(price, na.rm = TRUE), 1), .groups = "drop")
    plot_ly(d, x = ~points_bin, y = ~avg, type = "bar",
            marker = list(color = "#27ae60"),
            text = ~avg, textposition = "outside") %>%
      layout(xaxis = list(title = "Points Segment"),
             yaxis = list(title = "Avg Price"))
  })
  
  output$d5 <- renderDT({
    d <- fd()
    tbl <- data.frame(
      Metric = c("Points", "Price"),
      Count = c(sum(!is.na(d$points)), sum(!is.na(d$price))),
      Mean = c(round(mean(d$points, na.rm = TRUE), 2),
               round(mean(d$price, na.rm = TRUE), 2)),
      Median = c(median(d$points, na.rm = TRUE),
                 median(d$price, na.rm = TRUE)),
      SD = c(round(sd(d$points, na.rm = TRUE), 2),
             round(sd(d$price, na.rm = TRUE), 2)),
      Min = c(min(d$points, na.rm = TRUE),
              min(d$price, na.rm = TRUE)),
      Max = c(max(d$points, na.rm = TRUE),
              max(d$price, na.rm = TRUE))
    )
    datatable(tbl, options = list(dom = "t"), rownames = FALSE)
  })
  
  output$r1 <- renderPlotly({
    set.seed(42)
    d <- fd()
    if (nrow(d) > input$rs) d <- d[sample(nrow(d), input$rs), ]
    cv <- input$rc
    if (cv %in% c("country", "variety")) {
      tp <- d %>% count(!!sym(cv), sort = TRUE) %>%
        head(10) %>% pull(!!sym(cv))
      d$cg <- ifelse(d[[cv]] %in% tp, as.character(d[[cv]]), "Other")
    } else {
      d$cg <- as.character(d[[cv]])
    }
    xv <- if (input$rl) log10(d$price + 1) else d$price
    xt <- if (input$rl) "Log10 Price" else "Price"
    plot_ly(d, x = ~xv, y = ~points, color = ~cg,
            type = "scatter", mode = "markers",
            marker = list(size = 4, opacity = 0.4),
            text = ~paste0(title, " | ", country,
                           " | Pts:", points, " Price:", price),
            hoverinfo = "text") %>%
      layout(xaxis = list(title = xt),
             yaxis = list(title = "Points"))
  })
  
  output$r2 <- renderPlotly({
    rv <- round(cor(fd()$points, fd()$price, use = "complete.obs"), 3)
    m <- matrix(c(1, rv, rv, 1), nrow = 2)
    plot_ly(x = c("Points", "Price"), y = c("Points", "Price"),
            z = m, type = "heatmap",
            colorscale = "RdBu", zmin = -1, zmax = 1,
            text = round(m, 3), texttemplate = "%{text}") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = ""))
  })
  
  output$r3 <- renderUI({
    r <- round(cor(fd()$points, fd()$price, use = "complete.obs"), 3)
    s <- "Very Weak"
    if (abs(r) >= 0.7) s <- "Strong"
    if (abs(r) >= 0.4 & abs(r) < 0.7) s <- "Moderate"
    if (abs(r) >= 0.2 & abs(r) < 0.4) s <- "Weak"
    HTML(paste0("<div style='padding:10px;'>",
                "<p><b>r = </b>", r, "</p>",
                "<p><b>Strength:</b> ", s, "</p>",
                "<p><b>R-squared:</b> ", round(r * r, 3), "</p>",
                "</div>"))
  })
  
  output$r4 <- renderPlotly({
    d <- fd() %>% filter(!is.na(price_bin)) %>%
      group_by(price_bin) %>%
      summarise(avg = round(mean(points, na.rm = TRUE), 1), .groups = "drop")
    plot_ly(d, x = ~price_bin, y = ~avg, type = "scatter",
            mode = "lines+markers",
            line = list(color = "#8e44ad", width = 3),
            marker = list(size = 10)) %>%
      layout(xaxis = list(title = "Price Segment"),
             yaxis = list(title = "Avg Points"))
  })
  
  output$r5 <- renderPlotly({
    d <- fd() %>% filter(!is.na(points_bin)) %>%
      group_by(points_bin) %>%
      summarise(avg = round(mean(price, na.rm = TRUE), 1), .groups = "drop")
    plot_ly(d, x = ~points_bin, y = ~avg, type = "scatter",
            mode = "lines+markers",
            line = list(color = "#27ae60", width = 3),
            marker = list(size = 10)) %>%
      layout(xaxis = list(title = "Points Segment"),
             yaxis = list(title = "Avg Price"))
  })
  
  output$t1 <- renderPlotly({
    d <- fd() %>% filter(taster_name != "Unknown") %>%
      count(taster_name, sort = TRUE) %>% head(15) %>% arrange(n)
    plot_ly(d, y = ~reorder(taster_name, n), x = ~n,
            type = "bar", orientation = "h",
            marker = list(color = "#8e44ad")) %>%
      layout(xaxis = list(title = "Reviews"),
             yaxis = list(title = ""), margin = list(l = 150))
  })
  
  output$t2 <- renderPlotly({
    d <- fd() %>% filter(taster_name != "Unknown") %>%
      group_by(taster_name) %>%
      summarise(avg = round(mean(points, na.rm = TRUE), 1),
                n = n(), .groups = "drop") %>%
      filter(n >= 20) %>% arrange(avg)
    plot_ly(d, y = ~reorder(taster_name, avg), x = ~avg,
            type = "bar", orientation = "h",
            marker = list(color = "#c0392b"),
            text = ~avg, textposition = "outside") %>%
      layout(xaxis = list(title = "Avg Points",
                          range = c(min(d$avg) - 1, max(d$avg) + 1)),
             yaxis = list(title = ""), margin = list(l = 150))
  })
  
  output$t3 <- renderPlotly({
    tp <- fd() %>% filter(taster_name != "Unknown") %>%
      count(taster_name, sort = TRUE) %>% head(10) %>% pull(taster_name)
    d <- fd() %>% filter(taster_name %in% tp)
    plot_ly(d, x = ~taster_name, y = ~points,
            type = "box", color = ~taster_name) %>%
      layout(xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Points"), showlegend = FALSE)
  })
  
  output$t4 <- renderPlotly({
    tt <- fd() %>% filter(taster_name != "Unknown") %>%
      count(taster_name, sort = TRUE) %>% head(10) %>% pull(taster_name)
    tc <- fd() %>% filter(country != "Unknown") %>%
      count(country, sort = TRUE) %>% head(10) %>% pull(country)
    d <- fd() %>%
      filter(taster_name %in% tt, country %in% tc) %>%
      count(taster_name, country) %>%
      pivot_wider(names_from = country, values_from = n, values_fill = 0)
    tas <- d$taster_name
    ctr <- colnames(d)[-1]
    mat <- as.matrix(d[, -1])
    plot_ly(x = ctr, y = tas, z = mat,
            type = "heatmap", colorscale = "Purples") %>%
      layout(xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = ""),
             margin = list(l = 150, b = 100))
  })
  
  output$tw1 <- renderDT({
    d <- fd() %>% arrange(desc(points), desc(price)) %>%
      select(title, country, variety, winery, points, price, province) %>%
      head(20)
    datatable(d, options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$tw2 <- renderDT({
    d <- fd() %>% filter(price > 0) %>%
      mutate(value = round(points / price, 2)) %>%
      arrange(desc(value)) %>%
      select(title, country, variety, points, price, value, winery) %>%
      head(20)
    datatable(d, options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$tw3 <- renderPlotly({
    d <- fd() %>% filter(winery != "Unknown") %>%
      group_by(winery) %>%
      summarise(avg = round(mean(points, na.rm = TRUE), 1),
                n = n(), .groups = "drop") %>%
      filter(n >= 10) %>% top_n(15, avg) %>% arrange(avg)
    plot_ly(d, y = ~reorder(winery, avg), x = ~avg,
            type = "bar", orientation = "h",
            marker = list(color = "#f39c12"),
            text = ~paste0(avg, " (", n, ")"),
            textposition = "outside") %>%
      layout(xaxis = list(title = "Avg Points",
                          range = c(min(d$avg) - 0.5, max(d$avg) + 2)),
             yaxis = list(title = ""), margin = list(l = 180))
  })
  
  output$tw4 <- renderDT({
    d <- fd() %>% arrange(desc(price)) %>%
      select(title, country, variety, points, price) %>% head(15)
    datatable(d, options = list(pageLength = 8, scrollX = TRUE, dom = "t"),
              rownames = FALSE)
  })
  
  output$dt1 <- renderDT({
    d <- fd() %>%
      select(title, country, variety, winery, points, price,
             province, region_1, taster_name)
    datatable(d, options = list(pageLength = 25, scrollX = TRUE),
              filter = "top", rownames = FALSE)
  })
  
  output$dl <- downloadHandler(
    filename = function() { paste0("wine_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(fd(), file, row.names = FALSE) }
  )
}

shinyApp(ui = ui, server = server)