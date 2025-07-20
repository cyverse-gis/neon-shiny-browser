setwd('~/neon-shiny-browser')
library(shiny)

# Function to check if a port is available
is_port_available <- function(port) {
  tryCatch({
    # Try to create a server socket on the port
    con <- socketConnection(host = "127.0.0.1", port = port, server = FALSE, blocking = FALSE, open = "r+")
    close(con)
    return(FALSE)  # If connection succeeds, port is in use
  }, error = function(e) {
    return(TRUE)   # If connection fails, port is available
  })
}

# Function to find an available port
find_available_port <- function(start_port = 4199, max_attempts = 100) {
  for (i in 0:(max_attempts - 1)) {
    port <- start_port + i
    if (is_port_available(port)) {
      return(port)
    }
  }
  # If we get here, couldn't find an available port
  stop("Could not find an available port after ", max_attempts, " attempts starting from ", start_port)
}

# Find an available port starting from 4199
port <- find_available_port()

cat("Starting NEON Shiny Browser on port", port, "\n")
cat("Opening viewer automatically...\n")

# Run the app with automatic viewer opening
runApp(port = port, launch.browser = function(url) {
  cat("App is ready at:", url, "\n")
  # Use RStudio viewer instead of external browser
  if (rstudioapi::isAvailable()) {
    rstudioapi::viewer(url)
    cat("Opened in RStudio viewer\n")
  } else {
    utils::browseURL(url)
    cat("Opened in default browser\n")
  }
})
