# Level 3: High-performance computer
library(tidyverse)
library(foreach)
library(furrr)

runReplication = function(test, shift, distribution, filename, seed) {
  
  set.seed(seed)
  
  if (distribution == "Normal") {
    A  = rnorm(27, mean = 10500, sd = 1200)
    B  = rnorm(27, mean = 10500 + shift, sd = 1200)
  } else if (distribution == "Cauchy") {
    A  = rcauchy(27, location = 10500, scale = 1200)
    B  = rcauchy(27, location = 10500 + shift, scale = 1200)
  }
  
  if (test == "Student")
    hypothesis_test = t.test(A, B, var.equal = T)
  if (test == "Welch")
    hypothesis_test = t.test(A, B, var.equal = F)
  if (test == "MW")
    hypothesis_test = wilcox.test(A, B)
  
  result = list(
    test = test,
    shift = shift, 
    distribution = distribution,
    result = hypothesis_test[["p.value"]] < 0.05
  )
  
  saveRDS(result, file = filename)
  
}

# Get the ID of the machine
ID = Sys.getenv("SLURM_ARRAY_TASK_ID")
 
# For running on local, needed to initialize ID
if (ID == "")
  ID = 1

sims = expand_grid(
  test = c("Student", "Welch", "MW"),
  shift = c(300, 600, 900),
  distribution = c("Normal", "Cauchy"),
  rep = 1:10000
) |> 
  mutate(
    filename = paste0("sims-lvl3/",test, "-", distribution, "-", shift, "-", rep, ".rds"),
    seed = row_number(),
    machine = rep(c(1, 2), length.out = 180000)
  ) |> 
  filter( machine == ID )


plan(multisession, workers = parallel::detectCores() - 1)

future_pwalk(list(sims[["test"]], 
                  sims[["shift"]],
                  sims[["distribution"]],
                  sims[["filename"]],
                  sims[["seed"]]), 
             runReplication, 
             .options=furrr_options(seed=T))

results = foreach(file = list.files("sims-lvl3"), 
                  .combine = bind_rows) %do% {
                    path = paste0("sims-lvl3/", file)
                    readRDS(path)
                  }

results |> 
  group_by(test, shift, distribution) |> 
  summarize( power = mean(result) )

results |> 
  ggplot(aes(x = shift, y = power, color = test)) +
  geom_point() +
  geom_line() +
  facet_wrap(~distribution) + 
  theme_minimal() +
  theme(legend.position = "bottom")
