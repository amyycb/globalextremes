library(shinytest2)

# Define the path to your app
app_path <- "C:/Users/nab260/OneDrive - Newcastle University/Documents/IDF/app"

# Load the app
app <- AppDriver$new(app_path, seed = 123, shiny_args = list(display.mode = "normal"))

# Define input grid
input_grid <- expand.grid(
  method = c(TRUE, FALSE),
  dur = c(1, 3, 6, 24),
  display = c("rl", "param"),
  filter = c('reg', 'res', 'val'),
  param = c('location', 'scale', 'shape', 'index'),
  param1 = c('location', 'scale', 'shape'),
  stringsAsFactors = FALSE
)

# Create a log to store results
results <- data.frame(row = integer(), status = character(), message = character(), stringsAsFactors = FALSE)

# Track previous input to avoid redundant waits
previous_input <- NULL

# Loop through each combination
for (i in 1:nrow(input_grid)) {
  combo <- input_grid[i, ]
  tryCatch({
    # Set inputs with wait_ = FALSE and timeout_ = 10000
    app$set_inputs(
      method = combo$method,
      dur = combo$dur,
      display = combo$display,
      filter = combo$filter,
      param = combo$param,
      param1 = combo$param1,
      wait_ = FALSE,
      timeout_ = 100
    )

    # Only wait if inputs changed
    if (is.null(previous_input) || !identical(combo, previous_input)) {
      app$wait_for_idle()
    }

    # Log result
    results <- rbind(results, data.frame(row = i, status = "PASS", message = "", stringsAsFactors = FALSE))
    previous_input <- combo
  }, error = function(e) {
    results <- rbind(results, data.frame(row = i, status = "FAIL", message = e$message, stringsAsFactors = FALSE))
  })
}

# Save results to CSV
write.csv(results, "shiny_input_test_results1.csv", row.names = FALSE)

# Stop the app
app$stop()
