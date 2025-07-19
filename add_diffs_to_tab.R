colnames(data[["diffs"]][["1h"]])
methods <- c("sga", "rfa")
durs <- c("1h", "3h", "6h", "24h")
data <- list()

for (m in methods) {
  data[[m]] = list()

  for (dur in durs) {
    tab <- read.csv(paste0("data/", toupper(m), "_", dur, "_gev.csv"))
    data[[m]][[dur]] <- tab
  }
}

data[["diffs"]] = list()
for (dur in durs) {
  tab <- read.csv(paste0("data/", dur, "_diffs.csv"))
  data[["diffs"]][[dur]] <- tab
}
m = "rfa"
dur = "1h"
#data[["diffs"]][[dur]]$rp30 = NaN # reget missing
rls = c(10, 100)

for (dur in durs) {
  for (rl in rls) {
    data[[m]][[dur]][paste0("rp", rl, "_vs_PXR2")] = NaN
    cond = data[[m]][[dur]]$station_id %in% data[["diffs"]][[dur]]$X
    data[[m]][[dur]][cond, paste0("rp", rl, "_vs_PXR2")] = data[["diffs"]][[dur]][paste0("rp", rl)]
  }
}
for (m in methods) {
  for (dur in durs) {
    print(data[[m]][[dur]])
  }
}
for (m in methods) {
  for (dur in durs) {
    write.csv(data[[m]][[dur]], paste0("data/", toupper(m), "_", dur, "_all.csv"))
  }
}
dim(data[["diffs"]][[dur]][c("rp10", "rp30", "rp100")])
dim(data[[m]][[dur]][c("rp10", "rp30", "rp100")])
data[[m]][[dur]][c("rp10_vs_PXR2", "rp30_vs_PXR2", "rp100_vs_PXR2")]
dim(data[["diffs"]][[dur]])
data[[m]][[dur]][data[[m]][[dur]]$station_id %in% data[["diffs"]][[dur]]$X,c("rp10_vs_PXR2", "rp30_vs_PXR2", "rp100_vs_PXR2")]





cols <- brewer.pal(8, "Dark2")  # "Dark2" has 8 distinct colors
ramp <- function(values, colors) {
  color_indices <- (values %% length(colors)) + 1
  return(colors[color_indices])
}
col <- ramp(filtered$cluster, cols) # %% 8 + 1)

filtered = data[["rfa"]][["1h"]]
1:max(filtered$cluster) 

%% 8 + 1
cols <- brewer.pal(8, "Dark2")  # "Dark2" has 8 distinct colors

# Define the integer values between 0 and 300 (example)
vals <- 0:300  # Example: Values from 0 to 300

# Function to cycle through colors for each unique integer value
discrete_colormap <- function(values, colors) {
  # Cycle through the colors by using modulo on the values
  color_indices <- (values %% length(colors)) + 1  # To cycle the colors
  return(colors[color_indices])
}

ramp <- Dark2(1:max(filtered$cluster))
      print(ramp)
      
      
      
 cols <- brewer.pal(8, "Dark2")
ramp <- function(val) {
  idx = val %% length(cols) + 1
  return(cols[idx])
}
  col <- function(values) {
        color_indices <- (values %% length(cols)) + 1
        return(cols[color_indices])
      }