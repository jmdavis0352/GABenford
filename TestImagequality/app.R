#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("samplesize",
                        "Samplesize",
                        min = 10,
                        max = 5000,
                        value = 5000)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("testPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$testPlot <-  renderPlot({
            
            Bdat <-data.frame(A = runif(input$samplesize, 0 , 100000), B = runif(input$samplesize), C = rpois(input$samplesize,300), D = rexp(input$samplesize,1/200), 
                              E = rnorm(input$samplesize,100000, 1000), FF = exp(runif(input$samplesize,0, 100)), G = 2^runif(input$samplesize,0, 30), H = runif(input$samplesize,0,100), I = rnbinom(input$samplesize,5, .3), J = rgamma(input$samplesize,2.4, 5))
            
            Bdat$Benford <- apply(Bdat, 1, prod)
            BenfordPerfect <- benford(Bdat$Benford, number.of.digits = 1)
            
            
            p <- dig.distjd(Bdat$Benford , label = paste0('Product of 10 RV\n(n = ', input$samplesize, ')' ))  
            
            p$graph  + annotate('text', x=3, y = .27, label = paste0('Mean Absolute Deviation (MAD): ', round(BenfordPerfect$MAD, 4), '\n', 'MAD Conformity - Nigrini (2012): ', BenfordPerfect$MAD.conformity, sep = ''), hjust = 0, size = 5.5) +theme(text = element_text(size = 16))
            
 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
