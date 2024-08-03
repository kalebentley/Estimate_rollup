handle_error <- function(message) {
  call_stack <- sys.calls()
  error_line <- if (length(call_stack) > 1) deparse(call_stack[[length(call_stack) - 1]]) else "unknown line"
  stop(paste(message, "Error at:", error_line))
}