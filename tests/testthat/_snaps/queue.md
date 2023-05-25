# redirect stdout

    Code
      p <- q$push(function() {
        for (i in 1:10) {
          cat("hello world", "\n")
        }
        i
      })
      result <- extract_value(p)
    Output
      [callq worker: <1>] hello world 
      [callq worker: <1>] hello world 
      [callq worker: <1>] hello world 
      [callq worker: <1>] hello world 
      [callq worker: <1>] hello world 
      [callq worker: <1>] hello world 
      [callq worker: <1>] hello world 
      [callq worker: <1>] hello world 
      [callq worker: <1>] hello world 
      [callq worker: <1>] hello world 

