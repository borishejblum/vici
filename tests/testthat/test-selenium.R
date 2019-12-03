context("Selenium Test")

library(RSelenium)
library(testthat)
library(processx)


waitFor <- function(how,id){
  #webElem <- NULL
  
  i <- 0
  while(i<=1000){
    webElem <<- tryCatch({remDr$findElement(using = how, value = id)},
                         error = function(e){NULL})
    #loop until element with name <value> is found in <webpage url>
    
    i <- i+1
    if(!is.null(webElem)){
      break()
    }
  }
  if(!is.null(webElem)){
    return(webElem)
  }
  else{
    stop(paste0(id," not found \n"))
    #cat(paste0(id," not found \n"))
  }
}

wd <- getwd()

#Uncomment this for local test


remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                 port = 4445L,
                                 browserName = "firefox")
remDr$open()
appURL <- "http://10.0.75.1:3838"




test_that("can connect to app", {
  remDr$navigate("http://shiny:3838")
  webElem <- waitFor("xpath","/html/body/div[2]/h2")#remDr$findElement(using = "xpath", value = "/html/body/div[2]/h2")
  textWebElem <- webElem$getElementText()
  expect_equal(as.character(textWebElem), "VICI: accurate estimation of Vaccine Induced Cellular Immunogenicity with bivariate modeling")
  
})

test_that("Scénario standard example Data",{

  remDr$navigate("http://shiny:3838")
  loadButton <- waitFor("id", "settings_pan_ui_1-loadExample")
  loadButton$clickElement()
  
  Arm <- waitFor("xpath","/html/body/div[2]/div/div[2]/div/div/div[2]/div[2]/div/table/tbody/tr[1]/td[5]")#remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/div[2]/div/div/div[2]/div[2]/div/table/tbody/tr[1]/td[5]")
  expect_equal(as.character(Arm$getElementText()), "Placebo")
  Response1 <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/div[2]/div/div/div[2]/div[2]/div/table/tbody/tr[1]/td[6]")
  expect_equal(as.character(Response1$getElementText()), "0.0801")
  
  fit <- remDr$findElement(using = "id", value = "modelfit_ui_1-fit")
  fit$clickElement()
  
  fRep <- waitFor("xpath","/html/body/div[2]/div/div[2]/div/div/div[1]/div/div[2]/div[2]/div/div/div[1]/div/div[2]/table/tbody/tr[1]/td[1]")#remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/div[2]/div/div/div[1]/div/div[2]/div[2]/div/div/div[1]/div/div[2]/table/tbody/tr[1]/td[1]")
  expect_equal(as.character(fRep$getElementText()),"Response1 : Average response in reference stimulation NS in reference arm Placebo")
  standardError1 <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/div[2]/div/div/div[1]/div/div[2]/div[2]/div/div/div[1]/div/div[2]/table/tbody/tr[1]/td[3]")
  expect_equal(as.character(standardError1$getElementText()),"0.00760")
  pValue1 <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/div[2]/div/div/div[1]/div/div[2]/div[2]/div/div/div[1]/div/div[2]/table/tbody/tr[1]/td[4]")
  expect_equal(as.character(pValue1$getElementText()),"0.00000")
  
  lRep <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/div[2]/div/div/div[1]/div/div[2]/div[2]/div/div/div[1]/div/div[2]/table/tbody/tr[11]/td[1]")
  expect_equal(as.character(lRep$getElementText()),"Response1 : Effect of arm A3 on response in stimulation S2")
  standardError2 <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/div[2]/div/div/div[1]/div/div[2]/div[2]/div/div/div[1]/div/div[2]/table/tbody/tr[11]/td[3]")
  expect_equal(as.character(standardError2$getElementText()),"0.01200")
  pValue2 <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/div[2]/div/div/div[1]/div/div[2]/div[2]/div/div/div[1]/div/div[2]/table/tbody/tr[11]/td[4]")
  expect_equal(as.character(pValue2$getElementText()),"0.72415")
 
})

# test_that("Upload file and intra_Fit",{
#   remDr$navigate(appURL)
#   
#   # uploadBtn <- waitFor("css",".btn-file")
#   # uploadBtn$clickElement()
#   
#   rdBtn <- waitFor("xpath","/html/body/div[2]/div/div[1]/form/div[3]/div/div[2]/label/input")
#   rdBtn$clickElement()
#   
#   input <- waitFor("xpath","/html/body/div[2]/div/div[1]/form/div[1]/div[1]/input")
#   uploadTrgt <- waitFor("xpath","/html/body/div[2]/div/div[1]/form/div[1]/div[1]")#waitFor("id", "settings_pan_ui_1-datafile")
#   #uploadTrgt$setElementAttribute("style","display:true")
#   tryCatch({
#     remDr$executeScript(script = "arguments[0].removeAttribute('readonly','readonly');",args = list(input))
#     cat("File to upload: ")
#     f <- 'monfichier1_cp.csv'
#     cat(f,"\n")
#     uploadTrgt$sendKeysToElement(list(f))},
#     warning = function(war) {
#       cat("warning: ")
#       cat(war,"\n")
#     },
#     error = function(err) { 
#       RSelenium::errorHandler$errorDetails(type = "value") 								
#     }, 
#     finally = {  
#       
#     }
#   )
#   Arm <- waitFor("xpath","/html/body/div[2]/div/div[2]/div/div/div[2]/div[2]/div/table/tbody/tr[1]/td[2]")#remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/div[2]/div/div/div[2]/div[2]/div/table/tbody/tr[1]/td[5]")
#   expect_equal(as.character(Arm$getElementText()), "223")
#   Response1 <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/div[2]/div/div/div[2]/div[2]/div/table/tbody/tr[1]/td[4]")
#   expect_equal(as.character(Response1$getElementText()), "NS")
#   
#   modelSelecter <- waitFor("xpath","/html/body/div[2]/div/div[1]/form/div[4]/div/div/div[1]")
#   modelSelecter$clickElement()
#   intra <- waitFor("xpath","/html/body/div[2]/div/div[1]/form/div[4]/div/div/div[2]/div/div[2]")
#   intra$clickElement()
# 
#   subjectSelecter <- waitFor("xpath", "/html/body/div[2]/div/div[1]/form/div[5]/div/div/div[1]")
#   subjectSelecter$clickElement()
#   subject <- waitFor("xpath", "/html/body/div[2]/div/div[1]/form/div[6]/div/div/div[2]/div/div[1]")
#   subject$clickElement()
# 
#   stimuSelecter <- waitFor("xpath", "/html/body/div[2]/div/div[1]/form/div[6]/div/div/div[1]")
#   stimuSelecter$clickElement()
#   stimu <- waitFor("xpath","/html/body/div[2]/div/div[1]/form/div[6]/div/div/div[2]/div/div[2]")
#   stimu$clickElement()
#   bckg <- waitFor("xpath","/html/body/div[2]/div/div[1]/form/div[9]/div/div/div")
#   bckg$clickElement()
#   bckSelecter <- waitFor("xpath","/html/body/div[2]/div/div[1]/form/div[9]/div/div/div/div[2]/div/div[3]")
# 
#   TimeSelecter <- waitFor("xpath","/html/body/div[2]/div/div[1]/form/div[15]/div/div/div")
#   TimeSelecter$clickElement()
#   Time <- waitFor("xpath","/html/body/div[2]/div/div[1]/form/div[15]/div/div/div/div[2]/div/div[1]")
#   Time$clickElement()
#   TimeIdentifier <- waitFor("xpath","/html/body/div[2]/div/div[1]/form/div[16]/div/div/div/div[1]")
#   TimeIdentifier$clickElement()
#   TimeID <- waitFor("xpath","/html/body/div[2]/div/div[1]/form/div[16]/div/div/div/div[2]/div/div[1]")
# 
#   fit <- remDr$findElement(using = "id", value = "modelfit_ui_1-fit")
#   fit$clickElement()
# })

remDr$close()
#rD$server$stop()