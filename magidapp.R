library(shinydashboard)
library(ggplot2)
library(DT)



ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Magid Data Dashboard"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Correlation Explorer", tabName = "dashboard", icon = icon("chart-line")),
       menuItem("Data Viewer", tabName = "widgets", icon = icon("chess-board")),
       menuItem("Ranked Stats", tabName = "evaluation", icon = icon("users"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(status="info",
                box(width="40%", plotOutput(outputId = "scatterplot")),
                
                box(status="success",
                  title = "Controls",
                  selectInput(inputId = "y", label = "Y-axis:",
                               choices = c("show_name", "network_name", "genre", "grp_lab", "grp_lev", "network_type", "incidence","involve_idx", "evaluation", "atts_raw_funny", "atts_raw_fun", "atts_raw_dramatic", "atts_raw_calming", "atts_raw_informative", "atts_raw_relatable", "atts_raw_edgy", "atts_raw_mindless", "atts_raw_intelligent", "atts_raw_scary", "atts_raw_mean", "atts_raw_disturbing", "n_size"),
                               selected = "evaluation"),
                  #color
                  selectInput(inputId = "h", label = "Fill-color:",
                              choices = c("show_name", "network_name", "genre", "grp_lab", "grp_lev", "network_type", "incidence","involve_idx", "evaluation", "atts_raw_funny", "atts_raw_fun", "atts_raw_dramatic", "atts_raw_calming", "atts_raw_informative", "atts_raw_relatable", "atts_raw_edgy", "atts_raw_mindless", "atts_raw_intelligent", "atts_raw_scary", "atts_raw_mean", "atts_raw_disturbing", "n_size"),
                              selected = "n_size"),
                  # Select variable for x-axis
                  selectInput(inputId = "x", label = "X-axis:",
                              choices = c("show_name", "network_name", "genre", "grp_lab", "grp_lev", "network_type", "incidence","involve_idx", "evaluation", "atts_raw_funny", "atts_raw_fun", "atts_raw_dramatic", "atts_raw_calming", "atts_raw_informative", "atts_raw_relatable", "atts_raw_edgy", "atts_raw_mindless", "atts_raw_intelligent", "atts_raw_scary", "atts_raw_mean", "atts_raw_disturbing", "n_size"),
                              selected = "incidence")
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              fluidRow(
                column(width = 12,
                box( status ="info", width=NULL,solidHeader = TRUE,
                     DT::dataTableOutput("mytable"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"))
              
      )),
    
      # Third tab content
      tabItem(tabName = "evaluation",
              fluidRow(
                       column(width = 6,
                              box( title = "Evaluation By Genre",status="info", width=NULL,solidHeader = TRUE,
                        DT::dataTableOutput("eval")),
                        
                        box(
                          title = "Evaluation By Network", width = NULL, solidHeader = TRUE, status = "primary",
                          DT::dataTableOutput("eval2"))
                        ),
                       column(width=6,
                              box(
                                title = "Involvement By Genre", width = NULL, solidHeader = TRUE, status = "warning",
                                DT::dataTableOutput("ii")),
                              box(
                                title = "Involvement By Network", width = NULL, solidHeader = TRUE, status = "warning",
                                DT::dataTableOutput("ii2")))
                          
                              
                       
                       ))
              
    
 
  )))

server <- function(input, output) {
  # Create scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    ggplot(data = data, aes_string(x = input$x, y = input$y, color=input$h)) + 
      theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
      geom_point(shape=1) + geom_smooth(method=lm) 
  })
  
  evalttable<-aggregate(data$evaluation, list(data$genre), mean)
  eval2ttable<-aggregate(data$evaluation, list(data$network_name), mean)
  
  itable<-aggregate(data$involve_idx, list(data$genre), mean)
  i2table<-aggregate(data$involve_idx, list(data$network_name), mean)
    
  output$eval =DT::renderDataTable({
    evalttable
  })
  
  output$eval2 =DT::renderDataTable({
    eval2ttable
  })
  
  output$ii =DT::renderDataTable({
    itable
  })
  
  output$ii2 =DT::renderDataTable({
    i2table
  })
  
  output$mytable = DT::renderDataTable({
    data
  })
  
}
shinyApp(ui, server)