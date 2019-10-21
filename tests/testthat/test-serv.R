context("test serv method")

library(golem)
library(vici)

test_that("load example data", {
  df <- vici::ICS_ex
  expect_true(df["1",1] == 1 && as.character(df["1",4]) == "Placebo")
})

test_that("update var",{
  
})

test_that("clean output",{
  server <- app_server
  cat(str(server))
  expect_true(1==1)
})
