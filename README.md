
<!-- README.md is generated from README.Rmd. Please edit that file -->

# callq

<!-- badges: start -->
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
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Example

Below we will use callq to create a task queue and run tasks on it. The
motivating example here is:

1.  We crate a large data.frame on each worker
2.  We execute tasks that depend on the data.frame that lives in the
    worker

``` r
library(callq)
library(promises)
library(magrittr)

q <- task_q$new(num_workers = 2)

initialize_large_df <- function() {
  state <<- new.env(parent = emptyenv())
  state$df <<- data.frame(x = runif(1e5), y = runif(1e4))
  "done"
}

p <- q$worker_map(initialize_large_df) %>% 
  promise_all(.list = .) %>% 
  then(function(x) {
    cat("Initialization done")
  })
#> exec: starting the loop 
#> tasks size:  1 | idles:  0 
#> rescheduling

p1 <- q$push(~summary(state$df))
p2 <- q$push(~summary(state$df))

p1 %>%
  then(function(x) {
    print(x)
  })
```
