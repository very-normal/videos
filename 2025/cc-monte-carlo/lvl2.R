# Level 2: Parallel computation
library(tidyverse)
library(furrr)

runReplication = function(test, filename, seed) {
  
  set.seed(seed)
  
  if (filename %in% list.files("./sims-lvl2"))
      return()
      
      A  = rnorm(27, mean = 10500, sd = 1200)
      B  = rnorm(27, mean = 11000, sd = 1200)
      
      if (test == "Student")
        hypothesis_test = t.test(A, B, var.equal = T)
      if (test == "Welch")
        hypothesis_test = t.test(A, B, var.equal = F)
      if (test == "MW")
        hypothesis_test = wilcox.test(A, B)
      
      result = list(
        testname = test,
        result = hypothesis_test[["p.value"]] < 0.05
      )
      
      saveRDS(result, file = filename)
      
}

sims = expand_grid(
  test = c("Student", "Welch", "MW"),
  rep = 1:10000
) |> 
  mutate(
    filename = paste0("./sims-lvl2/",test, "-", rep, ".rds"),
    seed = row_number()
  )

# Sequential version of running simulations
# pwalk(list(sims[["test"]], 
#            sims[["filename"]],
#            sims[["seed"]]), 
#       runReplication)

# Parallelized version of running simulations
plan(multisession, workers = 4)
future_pwalk(list(sims[["test"]], 
                  sims[["filename"]],
                  sims[["seed"]]), 
             runReplication)

# Read back in results and analyze
results = foreach(file = list.files("sims-lvl2"), 
                  .combine = bind_rows) %do% {
  paste0("./sims-lvl2/", file) |> readRDS()
}

results |> 
  group_by(testname) |> 
  summarize( power = mean(result) )
