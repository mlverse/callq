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
  skip_on_os("windows")
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

test_that("redirect stderr", {
  skip_on_os("windows")
  q <- task_q$new(num_workers = 1)
  out <- capture.output(type = "message", {
    p <- q$push(function() {
      for (i in 1:10) {
        message("hello world2")
      }
      i
    })
    result <- extract_value(p)
  })
  expect_snapshot({
    for (line in out) {
      cat(line, "\n")
    }
  })
  expect_equal(result, 10)
})

test_that("can work with mirai", {
  q <- task_q$new(num_workers = 2, backend = "mirai")
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

test_that("can initialize mirai workers", {
  q <- task_q$new(num_workers = 2, backend = "mirai")
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
