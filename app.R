library(tidyverse)
library(shiny)

final <- read_csv("./data/df_[final]_small.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Producto - Cliente - Agencia"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("producto",
                  "Producto_ID:",
                  choices = final$Producto_ID %>% unique()),
      
      selectInput("cliente",
                  "Cliente_ID:",
                  choices = final$Cliente_ID %>% unique()), 
      
      selectInput("agencia",
                  "Agencia_ID:",
                  choices = final$Agencia_ID %>% unique())
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("monthPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  filtered_data <- reactive({
    final %>% filter(Producto_ID == input$producto, Agencia_ID == input$agencia, Cliente_ID == input$cliente)
  })
  
  output$monthPlot <- renderPlot({
    filtered_data() %>%
      ggplot(aes(x = Semana, y = Demanda_uni_equil)) +
      geom_line() +
      labs(title = "Semana - Demanda") + theme_light()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)