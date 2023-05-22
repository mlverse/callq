
<!-- README.md is generated from README.Rmd. Please edit that file -->

# callq

<!-- badges: start -->

[![R-CMD-check](https://github.com/mlverse/callq/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mlverse/callq/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

callq implements a multi process task queue using
[callr](https://github.com/r-lib/callr) as the backend for launching
process and [promises](https://github.com/rstudio/promises) as the
interface after pushing tasks to the queue.

The scope of callq is very similar to the scope of
`promises::future_promise()`. It is mostly used in
[shiny](https://github.com/rstudio/shiny) and
[plumber](https://github.com/rstudio/plumber) applications to allow
executing long running computations in a background process while
leaving the main process available to process requests from new
sessions.

Compared to the `promises::future_promises` solution it has a few
differences listed below:

- With callq you can’t configure the backend for task execution, it
  always uses callr sessions.
- callq always uses persistent workers, ie, R sessions are created when
  initializing the queue and always reused for evaluating the tasks.
  This also happens with `future` depending on the backend,
  `multisession` reuses sessions too.
- it’s possible to keep state in callq workers. You can use `<<-` to
  assign to the parent environment, which is not cleanup up after each
  task execution.
- callq allows manually specifying a worker to execute a task.

## Installation

You can install the development version of callq like so:

``` r
remotes::install_github("mlverse/callq")
```

## Examples

Below we show how to use callq as a task queue in a shiny app that runs
runs long computations.

The following app takes \~3s to start compared to the 10s that it would
take if the ‘long-running’ computations were not running in background
workers.

``` r
library(shiny)
library(promises)
library(callq)

q <- task_q$new(num_workers = 4)

ui <- fluidPage(
  fluidRow(
    plotOutput("one"),
    plotOutput("two"),  
  ),
  fluidRow(
    plotOutput("three"),
    plotOutput("four"),  
  )
)

make_plot <- function(time) {
  Sys.sleep(time)
  runif(10)
}

server <- function(input, output, session) {
  output$one <- renderPlot({q$push(make_plot, list(time = 2.5)) %...>% plot()})
  output$two <- renderPlot({q$push(make_plot, list(time = 2.5)) %...>% plot()})
  output$three <- renderPlot({q$push(make_plot, list(time = 2.5)) %...>% plot()})
  output$four <- renderPlot({q$push(make_plot, list(time = 2.5)) %...>% plot()})
}

shiny::shinyApp(ui, server)
```
