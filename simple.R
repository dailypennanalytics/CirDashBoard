library(shiny)

# Define UI for dataset viewer application
ui <- shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Circulation Dashboard"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to viewsidebarLayout(
  sidebarPanel(
    helpText("Please input today's circulation data"),
    dateInput("date", "Date", value = NULL, min = NULL, max = NULL,
              format = "yyyy-mm-dd", startview = "month", weekstart = 0,
              language = "en", width = NULL),
    sliderInput("range", "date filter:",
                min = as.Date(Sys.Date(), format="%B %d %Y")-90, max =  as.Date(Sys.Date(), format="%B %d %Y"),
                value = c(as.Date(Sys.Date(), format="%B %d %Y")-30,
                          as.Date(Sys.Date(), format="%B %d %Y"))),
    selectInput("location", "location",c('Grad School Student Cntr '='Grad School Student Cntr ','Arch Building'='Arch Building','Franklin Building'='Franklin Building','36th   Walnut  Blue Box'='36th   Walnut  Blue Box','Annenberg School'='Annenberg School','Graduate School of Education'='Graduate School of Education','Stitler Hall'='Stitler Hall','Steinberg Dietrich Hall'='Steinberg Dietrich Hall','McNeil Bldg '='McNeil Bldg ','Caster Bldg '='Caster Bldg ','Huntsman Hall'='Huntsman Hall','38th   Walnut  Blue Box'='38th   Walnut  Blue Box','39th   Walnut  Blue Box'='39th   Walnut  Blue Box','Hillel Foundation'='Hillel Foundation','Class of 1920 Commons'='Class of 1920 Commons','Harnwell College House'='Harnwell College House','Harrison College House'='Harrison College House','Rodin College House'='Rodin College House','W E B  DuBois House'='W E B  DuBois House','The Radian  in clear rack'='The Radian  in clear rack','40th   Walnut  Blue Box'='40th   Walnut  Blue Box','4015 Walnut St   Outside'='4015 Walnut St   Outside','Hamilton Village  Blue Box'='Hamilton Village  Blue Box','41st   Locust  Blue Box'='41st   Locust  Blue Box','Dental School'='Dental School','40th   Spruce  Blue Box'='40th   Spruce  Blue Box','Van Pelt Manor College House'='Van Pelt Manor College House','Class of 1925 House'='Class of 1925 House','3401 Walnut St  B C Wing\n3401 Walnut Street A B'='3401 Walnut St  B C Wing\n3401 Walnut Street A B','34th   Chestnut Blue Box'='34th   Chestnut Blue Box','34th   Spruce Blue Box'='34th   Spruce Blue Box','34th   Walnut Blue Box'='34th   Walnut Blue Box','36th   Chestnut Blue Box'='36th   Chestnut Blue Box','38th   Spruce Blue Box'='38th   Spruce Blue Box','Chemistry Building'='Chemistry Building','Cohen Hall'='Cohen Hall','College Hall'='College Hall','CVS'='CVS','CVS  Franklin s Table'='CVS  Franklin s Table','DRL'='DRL','Fisher Bennett'='Fisher Bennett','Gates Pavilion'='Gates Pavilion','Hill College House'='Hill College House','Houston Hall  Food Court '='Houston Hall  Food Court ','Houston Hall  Main '='Houston Hall  Main ','Houston Hall Info Desk'='Houston Hall Info Desk','Kings Court'='Kings Court','Law School'='Law School','Levine'='Levine','Meyerson'='Meyerson','Moore'='Moore','New College House'='New College House','Quad  lower '='Quad  lower ','Quad  upper '='Quad  upper ','Sansom Place East'='Sansom Place East','Sansom Place West'='Sansom Place West','Towne'='Towne','Van Pelt Library'='Van Pelt Library','Williams'='Williams','3401 Walnut St  B C Wing'='3401 Walnut St  B C Wing','3401 Walnut Street A B'='3401 Walnut Street A B','34th   Chestnut  Blue Box'='34th   Chestnut  Blue Box','34th   Spruce St   Blue Box'='34th   Spruce St   Blue Box','34th   Walnut  Blue Box'='34th   Walnut  Blue Box','36th   Chestnut  Blue Box'='36th   Chestnut  Blue Box','38th   Spruce St   Blue Box'='38th   Spruce St   Blue Box','Chemistry Building'='Chemistry Building','Claudia Cohen Hall'='Claudia Cohen Hall','College Hall'='College Hall','David Rittenhouse Labs'='David Rittenhouse Labs','Fisher Bennett Hall'='Fisher Bennett Hall','Food Court CVS'='Food Court CVS','Gates Pavilion  increased by 25 '='Gates Pavilion  increased by 25 ','Houston Hall  Food Court '='Houston Hall  Food Court ','Houston Hall  Main Floor '='Houston Hall  Main Floor ','Kings Court English House'='Kings Court English House','Law School'='Law School','Levine Hall'='Levine Hall','Meyerson Hall'='Meyerson Hall','Moore School'='Moore School','New College House'='New College House','Quad  Lower Gate   reduced by 75 '='Quad  Lower Gate   reduced by 75 ','Quad  Upper Gate '='Quad  Upper Gate ','Sansom Place East'='Sansom Place East','Sansom Place West'='Sansom Place West','Towne Bldg '='Towne Bldg ','Van Pelt Library'='Van Pelt Library','Williams Hall'='Williams Hall')),
    selectInput("routes", "Routes:",
                c("Route1"="Route1","Route2"="Route2")), 
    numericInput("delivery", "Delivery", value = 0, min = NA, max = NA, step = NA,
                 width = NULL),
    numericInput("returned", "Returned", value = 0, min = NA, max = NA, step = NA,
                 width = NULL),
    actionButton("add", "Generate"),
    actionButton("go", "Export"),
    actionButton("filter", "Filter"),
    textOutput("saved")
  ),
  # Show a summary of the dataset and an HTML table with the requested
  # number of observations
  mainPanel(
    tabsetPanel(
      tabPanel("Raw Data", DT::dataTableOutput("view")),
      tabPanel("Filtered Data", DT::dataTableOutput("filtered_view"),DT::dataTableOutput("summary"))
    )
  )
))

# Define server logic required to summarize and view the selected dataset
server <- shinyServer(function(input, output) {
  circulation = data.frame("location" = c(),"routes" = c(),"date" = c(),"delivered"=c(),"returned"=c())
  
  # Return the requested dataset
  dataset_clean <- eventReactive(input$add,{
    circulation <<- rbind(data.frame("location" = c(input$location),
                                                "routes" = c(input$routes),
                                                "date" = c(input$date),
                                                "delivered"= c(as.integer(input$delivery)),
                                                "returned"= c(as.integer(input$returned))),circulation)
  })
  
  filter <- eventReactive(input$filter,{
    circulation %>% dplyr::filter(date <= input$range[2] & date >= input$range[1])
  })
  
  generate_summary <- eventReactive(input$go,{
    df = dataset_clean()
    today <- Sys.Date()
    today = as.Date(today, format="%B %d %Y")
    today_rate = df %>% filter(date == today) %>% summarize(Return_rate = mean(returned/delivered))
    week_rate = df %>% filter(date >= today - 7) %>% summarize(Return_rate = mean(returned/delivered))
    month_rate = df %>% filter(date >= today - 30) %>% summarize(Return_rate = mean(returned/delivered))
    summary = cbind(today_rate,week_rate,month_rate)
    colnames(summary) = c("last day return rate", 
                          "last week return rate", 
                          "last month return rate")
  })
  
  getwd()
  library("ggplot2")
  library("dplyr")
  library("tidyr")
  
  output$view = DT::renderDataTable(dataset_clean())
  output$filtered_view = DT::renderDataTable(filter())
  output$summary = DT::renderDataTable(generate_summary())
  
})


shinyApp(ui = ui, server = server)
