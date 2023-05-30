#' Task Queue
#'
#' @description
#' A task queue implementation that allows for async programming by returning
#' promises when tasks are pushed.
#'
#' @export
#' @importFrom cli cli_abort
#' @importFrom fastmap fastqueue
#' @importFrom rlang as_function
#' @importFrom R6 R6Class
task_q <- R6::R6Class(
  classname = "callq_task_q",
  lock_objects = FALSE,
  public = list(
    #' @field workers list containing instances of workers objects
    workers = NULL,
    #' @field tasks a [fastmap::fastqueue()] containing tasks currently in the queue.
    #' Each task is represented as a named list containing:
    #' - `func`: The function to be called in the background worker.
    #' - `args`: A list containing the arguments to `func`
    #' - `resolve` and `reject`: [promises::promise()] functions to be used for
    #'   handle the promise.
    #' - `id`: Id of the task
    tasks = NULL,
    #' @description
    #' Creates a new task queue
    #' @param num_workers Number of workers in the queue. This is the number of R
    #'  processes that will be initialized and available to respond the queue.
    #' @param ... Currently unused. To allow future expansion.
    #' @param backend Currenttly only 'callr' and 'mirai' are supported.
    #' @param worker_options Numerous options that can be passed to the [callr::r_session]
    #'  object. Should be created with [callr::r_session_options()].
    #' @param process_tasks_delay Number of seconds in the future to delay execution. There is no
    #'  guarantee that the function will be executed at the desired time, but it
    #'  should not execute earlier.
    #' @param redirect_output Bool indicating if workers output should be redirected
    #'  to the main process. By default it's redirected. When it's `TRUE`, the `worker_options`
    #'  `stdout` and `stderr` element should be a `|`.
    initialize = function(num_workers, ..., backend = "callr", worker_options = NULL, process_tasks_delay = 0.1, redirect_output = TRUE) {
      self$num_workers <- num_workers
      self$worker_options <- worker_options

      self$backend <- backend
      private$Worker <- if (backend == "callr") {
        Worker
      } else if (backend == "mirai") {
        WorkerMirai
      } else {
        cli::cli_abort("Unsupported backend {.val {backend}}")
      }

      self$tasks <- fastmap::fastqueue()
      self$process_tasks_delay <- process_tasks_delay
      self$redirect_output <- redirect_output

      private$start_workers()
    },
    #' @description
    #' Push a new task to the queue
    #' Returns a [promises::promise()] that can be used in the same contexts as eg,
    #' [promises::future_promise()].
    #' @param func a function object to call in one of the background workers.
    #'   See notes in [callr::r()].
    #' @param args arguments to pass to the function `func`. Must be a list.
    push = function(func, args = list()) {
      func <- rlang::as_function(func)
      if (!is.list(args)) cli::cli_abort("{.arg args} must be a list.")
      promise <- promises::promise(function(resolve, reject) {
        self$tasks$add(new_task(
          func = func,
          args = args,
          resolve = resolve,
          reject = reject
        ))
      })
      private$call_tasks()
      promise
    },
    #' @description
    #' Allows applying a function for every worker.
    #' It can be useful to initialize workers, eg, add make sure some state is
    #' initialized for each worker before start adding tasks.
    #' Returns a list of promises. One per worker.
    #' Tasks scheduled via `worker_map` take precendence over tasks added with `push`,
    #' but don't interrupt the evaluation of a task that is already running.
    #' @param func a function object to call in one of the background workers.
    #'   See notes in [callr::r()].
    #' @param args arguments to pass to the function `func`. Must be a list.
    worker_map = function(func, args = list()) {
      lapply(seq_along(self$workers), function(index) {
        self$worker_exec(index, func, args)
      })
    },
    #' @description
    #' Run a task in the worker specified with an index
    #' @param index integer specifying a worker index
    #' @param func a function object to call in one of the background workers.
    #'   See notes in [callr::r()].
    #' @param args arguments to pass to the function `func`. Must be a list.
    worker_exec = function(index, func, args = list()) {
      if (!is.numeric(index) && index < 1 || index > self$num_workers) {
        cli::cli_abort(c(
          x = "{.arg index} must be an integer in the [1, {self$num_workers}] range.",
          i = "Got {.arg index} = {.val index}"
        ))
      }
      worker <- self$workers[[index]]
      result <- promises::promise(function(resolve, reject) {
        worker$add_task(new_task(func, args, resolve, reject))
      })
      if (is.null(self$loop)) {
        private$later_process()
      }
      result
    }
  ),
  private = list(
    Worker = NULL,
    start_workers = function() {
      self$workers <- lapply(
        seq_len(self$num_workers),
        function(x) {
          private$Worker$new(options = self$worker_options, id = x)
        }
      )
    },
    call_tasks = function() {
      # if no tasks in the task list we skip 'calling tasks'
      if (self$tasks$size() == 0) return()

      # tasks on the list, we start the loop
      if (is.null(self$loop)) {
        private$later_process()
      }

      # queries idle works, no idle works = nothing to do
      idle_workers <- private$get_idles()
      if (length(idle_workers) == 0) {
        return()
      }

      # add tasks to idle workers
      for (worker in idle_workers) {
        task <- self$tasks$remove()
        if (is.null(task)) break
        worker$add_task(task)
      }
    },
    resolve_tasks = function() {
      # resolve tasks if they are ready
      for (worker in self$workers) {
        if (worker$is_resolved()) {
          worker$resolve_task()
        }
      }
    },
    redirect_output = function() {
      for (worker in self$workers) {
        worker$redirect_output()
      }
    },
    process_tasks = function() {
      private$call_tasks()

      if (self$redirect_output) {
        private$redirect_output()
      }

      private$resolve_tasks()
    },
    later_process = function() {
      if (is.null(self$loop)) {
        current_loop <- later::current_loop()
        self$loop <- later::create_loop(parent = current_loop)
      }

      # process tasks might have zero out the tasks in the poll but still the
      # event loop might still be required. just in case, we run the loop another
      # time.
      tasks <- private$n_tasks()
      private$process_tasks()

      # if no more tasks and workers are all idle, we don't need to reschedule
      # the process_tasks event loop.
      if (tasks == 0) {
        self$loop <- NULL
        return()
      }

      later::later(
        private$later_process,
        delay = self$process_tasks_delay,
        loop = self$loop
      )
    },
    get_idles = function() {
      Filter(function(x) x$is_idle(), self$workers)
    },
    n_tasks = function() {
      tasks <- self$tasks$size()
      for (worker in self$workers) {
        tasks <- tasks + worker$tasks$size()
      }
      tasks
    }
  )
)

new_task <- function(func, args, resolve, reject, id = uuid()) {
  structure(class = "callq_task", list(
    func = func,
    args = args,
    resolve = resolve,
    reject = reject,
    id = id
  ))
}

print.callq_task <- function(x, ...) {
  cat("Task <", x$id, ">\n")
}

uuid <- function() {
  uuid::UUIDgenerate()
}

Worker <- R6::R6Class(
  public = list(
    tasks = NULL,
    session = NULL,
    id = NULL,
    initialize = function(options, id = uuid()) {
      if (is.null(options))
        options <- callr::r_session_options(stdout = "|", stderr= "|")

      self$session <- callr::r_session$new(options = options, wait = FALSE)
      self$tasks <- fastmap::fastqueue()
      self$id <- id
    },
    call_task = function() {
      if (!self$is_idle()) return()
      task <- self$tasks$peek()
      if (is.null(task)) return()
      self$session$call(task$func, task$args)
    },
    add_task = function(task) {
      self$tasks$add(task)
      self$call_task()
    },
    redirect_output = function() {
      out <- self$session$read_output_lines()
      if (length(out) > 0) {
        for (line in out) {
          cat("[callq worker: <", self$id, ">] ",  line, "\n", sep="")
        }
      }

      out <- self$session$read_error_lines()
      if (length(out) > 0) {
        for (line in out) {
          cat("[callq worker: <", self$id, ">] ",  line, "\n", sep="", file = stderr())
        }
      }
    },
    resolve_task = function() {
      if (self$session$get_state() == "starting") {
        self$session$read()
        return(self$call_task())
      }

      out <- self$session$read()
      task <- self$tasks$remove()

      if (!is.null(out$error)) {
        task$reject(out$error)
      } else {
        task$resolve(out$result)
      }

      # the task has been resolved, but it's possible that worker still has
      # tasks to run. That's why we try to reschedule call the remaining tasks.
      self$call_task()
    },
    is_idle = function() {
      self$session$get_state() == "idle"
    },
    is_resolved = function() {
      self$session$poll_process(1) == "ready"
    }
  )
)

WorkerMirai <- R6::R6Class(
  public = list(
    tasks = NULL,
    id = NULL,
    initialize = function(options, id = uuid()) {
      if (is.null(options))
        options <- list()

      id <- paste0(as.character(id), "-", uuid())
      opts <- list(n = 1L, dispatcher = FALSE, .compute = id, cleanup = 0L)
      for(nm in names(options)) {
        if (nm %in% names(opts)) {
          cli::cli_abort("The option {.arg {nm}} must not be manually specified.")
        }
      }
      do.call(mirai::daemons, append(options, opts))

      self$tasks <- fastmap::fastqueue()
      self$id <- id
    },
    call_task = function() {
      if (!self$is_idle()) return()
      task <- self$tasks$peek()
      if (is.null(task)) return()
      private$running_task <- mirai::mirai(
        {do.call(func, args)},
        args = task$args,
        func = task$func,
        .compute = self$id
      )
    },
    add_task = function(task) {
      self$tasks$add(task)
      self$call_task()
    },
    resolve_task = function() {
      out <- .subset2(private$running_task, "data")
      task <- self$tasks$remove()
      private$running_task <- NULL

      if (mirai::is_error_value(out)) {
        task$reject(out)
      } else {
        task$resolve(out)
      }

      # the task has been resolved, but it's possible that worker still has
      # tasks to run. That's why we try to reschedule call the remaining tasks.
      self$call_task()
    },
    redirect_output = function() {
      # not implemented
    },
    is_idle = function() {
      is.null(private$running_task)
    },
    is_resolved = function() {
      !is.null(private$running_task) && !mirai::unresolved(private$running_task)
    }
  ),
  private = list(
    running_task = NULL,
    finalizer = function() {
      mirai::daemons(0L, .compute = self$id)
    }
  )
)

#' Used for etsting purposes only
#' @keywords internal
#' @importFrom promises %...>% %...!%
extract_value <- function(promise) {
  promise_value <- NULL
  error <- NULL
  promise %...>%
    (function(value) promise_value <<- value) %...!%
    (function(reason) error <<- reason)

  start <- Sys.time()
  while (is.null(promise_value) && is.null(error)) {
    if (difftime(Sys.time(), start, units = "secs") > 30) {
      stop("Waited too long")
    }
    later::run_now()
    Sys.sleep(0.01)
  }

  if (!is.null(error))
    stop(error)
  else
    promise_value
}

