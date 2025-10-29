# Quick summary stats for numeric data
summary_stats = function(data) {
  nums = sapply(data, is.numeric)
  summary(data[, nums])
}

# Check for missing values in dataset
missing_report = function(data) {
  miss_count = sapply(data, function(x) sum(is.na(x)))
  miss_pct = round(miss_count / nrow(data) * 100, 2)
  
  result = data.frame(
    Variable = names(data),
    Missing = miss_count,
    Percent_Missing = miss_pct
  )
  
  # only return variables with missing data
  result[result$Missing > 0, ]
}

# Simple function to plot distributions
quick_hist = function(data, col_name) {
  hist(data[[col_name]], 
       main = paste("Distribution of", col_name),
       xlab = col_name,
       col = "lightblue",
       border = "white")
}