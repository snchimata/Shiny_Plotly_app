library(shiny)
library(plotly)

ui =  fluidPage(titlePanel("Creating the tabs!"),
                sidebarLayout(sidebarPanel(
                  radioButtons(
                    inputId  = "characterstic",
                    label = "Select a characteristic",
                    choices = c(
                      "Mileage" = "mpg",
                      "Displacement" = "disp",
                      "Horsepower" = "hp",
                      "Rear axle ratio" = "drat",
                      "Weight" = "wt"
                    ),
                    selected = "disp"
                  )
                ),
                mainPanel(tabsetPanel(
                  tabPanel("Summary", verbatimTextOutput("myplot0")),
                  tabPanel("Boxplot", plotlyOutput("myplot1")),
                  tabPanel("Histogram", plotlyOutput("myplot2")),
                  tabPanel("StripPlot", plotlyOutput("myplot3")),
                  tabPanel("DensityPlot", plotlyOutput("myplot4"))
                ))))

server = function(input, output) {

  var = reactive({
    mtcars[, input$characterstic]
  })


  output$myplot0 = renderPrint({
    summary(var())
  })

  output$myplot4  = renderPlotly({

    Z=
      ggplot(mtcars, aes(x=var()))+geom_density()+labs(x=input$characterstic)

    ggplotly(Z)
    #plot_ly(mtcars,x=var(), y = "", type = "scatter")
  })


  output$myplot3  = renderPlotly({

    Z=
      ggplot(mtcars, aes(x=var(),y=1))+geom_point()+labs(x=input$characterstic, y = "")

    ggplotly(Z)
    #plot_ly(mtcars,x=var(), y = "", type = "scatter")
  })


  output$myplot2  = renderPlotly({
    Z= ggplot(mtcars, aes(x=var()))+geom_histogram()+labs(x=input$characterstic)
    #hist(var(), main = "Histogram", xlab=input$characterstic)
    ggplotly(Z)
  })

  output$myplot1  = renderPlotly({
    # boxplot(mtcars[, input$characterstic], main = "Boxplot", xlab=input$characterstic)
    Z= ggplot(mtcars, aes(x="",y=mtcars[,input$characterstic]))+geom_boxplot()+labs(x = input$characterstic, y = "Frequency" )
    ggplotly(Z)
    #plot_ly(mtcars,y = mtcars[,input$characterstic],type = "box")
  })
}
shinyApp(ui, server)
