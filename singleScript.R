library(rvest)
library(dplyr)
library(xtable)
library(stringr)
library(reshape2)
library(lubridate)
library(xml2)
library(tidyr)
library(purrr)
library(writexl)

# ---URL & KEYWORD-------------------------------------------------------------------------
# Define a key word
# keyWord <- "google%20data%20studio"
# keyWord <- "access"
# keyWord <- "Tableau"
# keyWord <- "sales%20force"
# keyWord <- "R%20python"
# keyWord <- "power%20bi"
# keyWord <- "rstudio"
# keyWord <- "power%20pivot"
# keyWord <- "data%20science"
# keyWord <- "critical%20thinking"
# keyWord <- "leadership"
# keyWord <- "teamwork"
# keyWord <- "positive%20attitude"
keyWord <- "adhoc"
# URL = paste0("http://www.occ.com.mx/empleos-en-mexico-y-el-mundo/", keyWord)
URL = "https://www.occ.com.mx/empleos-en-mexico-y-el-mundo/para-trabajar-en-Kelly-Services"

# read htmlContent
htmlContent <- read_html(URL)

# ---GET ALL THE CLASSES-------------------------------------------------------------------------
theClasses <- htmlContent %>%
  html_nodes("*") %>%
  html_attr("class") %>%
  unique() %>% 
  as_data_frame()

# ---GET NUMBER OF PAGES-------------------------------------------------------------------------
# Get number of pages
numberOfPages <- htmlContent %>% 
  # Get data from "last page" button
  html_nodes('.last-page') %>% 
  html_attr('href') %>% 
  str_replace("\\D+", "")

# Validate number of pages
if (numberOfPages == "") {
  numberOfPages = 1
} else {
  numberOfPages <- as.numeric(numberOfPages)
}
# https://www.occ.com.mx/empleos-en-mexico-y-el-mundo/access?page=1

for (k in 1:numberOfPages) {
  theURLByPage <- paste0(URL, "?page=", k)
  # read htmlContent
  htmlContent <- read_html(theURLByPage)
  # read data frame to save info
  dataBase <- readRDS("dataBase.rds")
  
  # -----------Webscrapping starts-----------------------------
  
  # ---GET HEADERS-------------------------------------------------------------------------
  # Get job title
  jobTitles <- htmlContent %>% 
    # Get data from "last page" button
    html_nodes('.title') %>% 
    html_text() %>%
    toupper() %>% 
    as_data_frame() %>% 
    filter(value != "CORREO ELECTRÓNICO")
  colnames(jobTitles) <- c("JOB_TITLE")
  
  # Get company name
  companyNames <- htmlContent %>% 
    # Get data from "last page" button
    html_nodes('.name') %>% 
    html_text() %>%
    toupper() %>% 
    as_data_frame()
  colnames(companyNames) <- c("COMPANY")
  
  # Get location name
  theLocations <- htmlContent %>% 
    # Get data from "last page" button
    html_nodes('.location') %>% 
    html_text() %>%
    toupper() %>% 
    as_data_frame() %>% 
    mutate(KEY = keyWord)
  colnames(theLocations) <- c("LOCATION", "KEY")
  
  # Get salary amount
  theSalary <- htmlContent %>% 
    # Get data from "last page" button
    html_nodes(xpath = '//*[@id="salaryInfo"]') %>% 
    html_text() %>%
    toupper() %>% 
    as_data_frame()
  colnames(theSalary) <- c("SALARY")
  
  # Transform information into numbers
  theSalary  <- theSalary %>% 
    mutate(IS_AMOUNT = str_detect(SALARY, "-")) %>% 
    mutate(SALARY = str_replace_all(SALARY, ",", "")) %>%
    mutate(POSITION = as.numeric(gregexpr("-",SALARY))-2) %>% 
    mutate(LENGTH = nchar(SALARY)) %>% 
    mutate(MIN = as.numeric(substr(SALARY,2,POSITION))) %>% 
    mutate(MAX = as.numeric(substr(SALARY,POSITION+5,LENGTH)))
  
  # Remove complementary data
  theSalary$IS_AMOUNT <- NULL
  theSalary$POSITION <- NULL
  theSalary$LENGTH <- NULL
  
  # Get description
  theDescription <- htmlContent %>% 
    # Get data from "last page" button
    html_nodes('.job-description') %>% 
    html_text() %>%
    # toupper() %>% 
    as_data_frame()
  # mutate(KEY = keyWord)
  colnames(theDescription) <- c("DESCRIPTION")
  
  # Get date
  theDate <- htmlContent %>% 
    # Get data from "last page" button
    html_nodes('.publish-date') %>% 
    html_text() %>%
    # toupper() %>% 
    as_data_frame()
  # mutate(KEY = keyWord)
  colnames(theDate) <- c("PUBLISH_DATE")
  
  # Get link to job
  theLink <- htmlContent %>% 
    # Get data from "last page" button
    html_nodes('.job-ad') %>% 
    xml_child('a') %>% 
    xml_attrs('href') %>% 
    sapply("[", "href") %>% 
    as_data_frame()  
  
  # add complement to link
  theLink <- theLink %>% 
    mutate(LINK = paste0("https://www.occ.com.mx", value))
  
  # remove partial link
  theLink$value <- NULL
  
  # toupper() %>% 
  as_data_frame()
  # mutate(KEY = keyWord)
  colnames(theDate) <- c("PUBLISH_DATE")  
  
  # Build data frame
  allTheData <- bind_cols(jobTitles, companyNames, theLocations, theSalary, theDescription, theDate, theLink)
  allTheData <- allTheData %>% 
    mutate(REPORT_DATE = Sys.Date())
  # remove individual data frames
  remove(jobTitles, companyNames, theLocations, theSalary, htmlContent, theDescription, theDate, theLink)
  # saveRDS(allTheData, file = "allTheData.rds")
  
  # attach data to database
  dataBase <- bind_rows(dataBase, allTheData)
  
  # save dataBase
  saveRDS(dataBase, file = "dataBase.rds")
  
  # remove references
  remove(dataBase, allTheData)
  
  
  # -----------Webscrapping finished-----------------------------
}

remove(k, keyWord, numberOfPages, theURLByPage, theClasses, URL)

