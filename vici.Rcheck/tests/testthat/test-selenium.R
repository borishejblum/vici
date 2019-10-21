context("Selenium Test")

library(RSelenium)
library(testthat)
#library(future)


#skip_on_cran()

#rD <- RSelenium::rsDriver(
#  browser = "firefox",
#  extraCapabilities = list(
#    "moz:firefoxOptions" = list(
#      args = list()#('--headless')
#    )
#  )
#)

remDr <- remoteDriver(browserName = "firefox",port=4455L) #rD$client 
remDr$open(silent = FALSE)
remDr$setTimeout(type = "page load", milliseconds = 5000)
appURL <- "http://google.com"#"http://127.0.0.1:8080"
#app %<-% vici::run_app()


test_that("can connect to app", {
  #skip_on_cran()
  remDr$navigate(appURL)
  #Sys.sleep(5)
  webElem <- NULL
  while(is.null(webElem)){
    webElem <- tryCatch({remDr$findElement(using = "xpath", value = "//*[@id=\"hplogo\"]")},
                        error = function(e){NULL})
    #loop until element with name <value> is found in <webpage url>
  }
  url <- remDr$getCurrentUrl()
  cat("url => ")
  cat(as.character(url),"\n")
  cat("page title =>")
  #cat(as.character(remDr$getTitle(appURL)),"\n")
  #cat("Page Source => ")
  #cat(as.character(remDr$getPageSource()),"\n")
  #webElem <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/h2")
  #textWebElem <- webElem$getElementText()
  #expect_equal(as.character(textWebElem), "VICI: accurate estimation of Vaccine Induced Cellular Immunogenicity with bivariate modeling")  
})

remDr$close()
#rD$server$stop()