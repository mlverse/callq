test_that("basic usage", {
  q <- task_q$new(num_workers = 2)
  results <- list()
  for (i in 1:10) {
    results[[i]] <- q$push(function() {
      "hello world"
    }, args = list())
  }
  lapply(results, function(x) {
    expect_equal(extract_value(x), "hello world")
  })
})

test_that("can initialize workers", {
  q <- task_q$new(num_workers = 2)
  init <- q$worker_map(function() {
    state <<- "hello world"
    Sys.sleep(5)
    "init"
  })
  results <- lapply(1:10, function(x) q$push(function() state))
  for (i in seq_along(results)) {
    expect_equal(extract_value(results[[i]]), "hello world")
  }
})

test_that("redirect stdout", {
  q <- task_q$new(num_workers = 1)
  expect_snapshot({
    p <- q$push(function() {
      for (i in 1:10) {
        cat("hello world", "\n");
      }
      i
    })
    result <- extract_value(p)
  })
  expect_equal(result, 10)
})
