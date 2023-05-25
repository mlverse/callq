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

# redirect stderr

    Code
      for (line in out) {
        cat(line, "\n")
      }
    Output
      [callq worker: <1>] hello world2 
      [callq worker: <1>] hello world2 
      [callq worker: <1>] hello world2 
      [callq worker: <1>] hello world2 
      [callq worker: <1>] hello world2 
      [callq worker: <1>] hello world2 
      [callq worker: <1>] hello world2 
      [callq worker: <1>] hello world2 
      [callq worker: <1>] hello world2 
      [callq worker: <1>] hello world2 

