# Level 1: Local computer
library(tidyverse)

styleA = rnorm(n = 27, mean = 10500, sd = 1200)
styleB = rnorm(n = 27, mean = 10500 + 600, sd = 1200)

test = t.test(styleA, styleB, var.equal = T)

result = test[["p.value"]] < 0.05

results = logical(10000)
for (i in 1:10000) {
  
  styleA = rnorm(n = 27, mean = 10500, sd = 1200)
  styleB = rnorm(n = 27, mean = 10500 + 600, sd = 1200)
  
  test = t.test(styleA, styleB, var.equal = T)
  
  results[i] = test[["p.value"]] < 0.05
  
}
