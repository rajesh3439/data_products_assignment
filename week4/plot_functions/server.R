#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$distPlot <- renderPlot({
        dist <- switch(input$dist,
                       norm = rnorm,
                       unif = runif,
                       lnorm = rlnorm,
                       exp = rexp,
                       rnorm)
        # dens <-switch(input$dist,
        #               norm = dnorm,
        #               unif = dunif,
        #               lnorm = dlnorm,
        #               exp = dexp,
        #               dnorm)
        npoints<-input$npoints
        x <- dist(npoints)
        hist(x, main=paste("Histogram of",npoints,"samples"), prob=TRUE,
             col=4, density=20, angle=45)
        lines(density(x),lwd=4,col=5)
        
    })
    
    output$outText <- renderText(switch(input$dist,
                   norm = "Normal Distribution",
                   unif = "Uniform Distribution",
                   lnorm = "Log Normal Distribution",
                   exp = "Exponential Distribution",
                   "Normal Distribution"))
    # output$outText <- renderText("Histogram")
})
