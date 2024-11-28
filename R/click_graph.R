#' Use shiny to return x- and y-coordinates when clicking on a plot
#'
#' @param x A saved plot
#' @import ggplot2
#' @import shiny
#' @source https://shiny.posit.co/r/articles/build/plot-interaction/
#' @author Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J, McPherson J, Dipert A, Borges B (2022). _shiny: Web Application Framework for R_. R package version 1.7.4, <https://CRAN.R-project.org/package=shiny>.
#' @examples
#' #click_graph(plot(rnorm(10),rnorm(10)))
#'
#' @export
click_graph<-function(x){

  ui <- shiny::fluidPage(
    shiny::plotOutput("plot1", click = "plot_click"),
    shiny::verbatimTextOutput("info")
  )

  server <- function(input, output) {
    output$plot1 <- shiny::renderPlot({
      x
    })

    output$info <- shiny::renderText({
      paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
    })
  }

  return(shinyApp(ui, server))
}
