library(shiny)
library(ggplot2)
library(tidyr)
library(magrittr)


brown <- function(nsamples, step, Tmax, B0 = 0, m = 0, s = 1){
  Time <- seq(from = 0, to = Tmax, by = step)
  df <- data.frame(Time)
  M <- matrix(NA, nrow = length(Time), ncol = nsamples)
  df <- cbind(df, M)
  df[1, 2:(1+nsamples)] <- B0
  for(i in 2:nrow(df)){
      df[i, 2:(nsamples+1)] <- df[i-1,2:(nsamples+1)] + sqrt(step)*rnorm(n=nsamples, mean = m, sd = s)
  }
  df %<>%
    gather(Sample, B, 2:(nsamples+1))
  return(df)
}


ui <- fluidPage(
  
  tags$h1("Simulation of Brownian Motion", 
          align = "center"),
  
  sidebarPanel(align = "center",
    selectInput(inputId = "nsamples",
                "Number of samples", choices = 1:10),
    selectInput(inputId = "step",
                "Step length", choices = c(10^-4, 10^-3, 10^-2, 10^-1), selected = 10^-3),
    sliderInput(inputId = "Tmax", 
                "T max", value = 1, min = 1, max = 5, step = 0.05),
    sliderInput(inputId = "B0",
                "Initial position", value = 0, min = -2, max = 2, step = 0.01),
    sliderInput(inputId = "m",
                "Mean", value = 0, min = -1, max = 1, step = 0.01),
    sliderInput(inputId = "s",
                "Standard deviation", value = 1, min = 0.00, max = 2, step = 0.01),
    actionButton(inputId = "resample", label = "Resample")
  ),
  
  mainPanel(
    p("You can control the different parameters of the simulations in the sidebar pannel.",
      align = "center"),
    plotOutput(outputId = "graph")
  )
  
)


server <- function(input, output) {
  
  rv <- reactiveValues(data = brown(nsamples = 1,
                                     step = 0.001,
                                     Tmax = 1,
                                     B0 = 0,
                                     m = 0,
                                     s = 1))
  
   observeEvent(input$resample, {
     rv$data <- brown(nsamples = as.numeric(input$nsamples),
                      step = as.numeric(input$step),
                      Tmax = input$Tmax,
                      B0 = input$B0,
                      m = input$m,
                      s = input$s)
   })                    
                     
  output$graph <- renderPlot({
    ggplot(rv$data, aes(x = Time, y = B, color = Sample)) + 
      geom_line(size = 0.7) +
      theme_bw() +
      scale_color_discrete(guide = FALSE) +
      labs(
        x = "t", y = expression(B[t]))
    })
  
} 

shinyApp(ui = ui, server = server)