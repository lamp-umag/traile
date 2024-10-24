# Function for t-test and cohen's d
library(effsize)  # for cohen's d

run_t_tests <- function(data, group_var) {
  composite_vars <- names(data)[startsWith(names(data), "c")]
  
  results <- lapply(composite_vars, function(var) {
    t_result <- t.test(get(var) ~ get(group_var), data = data)
    coh_d <- cohen.d(data[[var]] ~ data[[group_var]])
    
    c(t = round(t_result$statistic, 2),
      p = round(t_result$p.value, 3),
      d = round(coh_d$estimate, 2))
  })
  
  results_df <- do.call(rbind, results)
  rownames(results_df) <- composite_vars
  colnames(results_df) <- c("t", "p", "d")
  return(results_df)
}

# Run tests for each demographic variable
demo_vars <- c("prol", "pdep", "psex", "page")
lapply(demo_vars, function(x) {
  cat("\nT-tests for", x, ":\n")
  print(run_t_tests(adf, x))
})