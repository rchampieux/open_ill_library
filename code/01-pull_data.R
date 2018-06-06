#'---
#'title: "Pull Data"
#'author: "Jessica Minnier"
#'date: '`r Sys.Date()`'
#'output: github_document
#'---
  
#' r setup, include=FALSE
library(tidyverse)
library(readxl)
library(skimr)
library(janitor)
library(here)

#info from http://tophcito.blogspot.com/2015/11/accessing-apis-from-r-and-little-r.html
library(httr)
library(jsonlite)

knitr::opts_chunk$set(
  eval       = TRUE,    # whether to run code in code chunk
  include    = TRUE,    # whether to include the chunk output
  echo       = TRUE,   # Whether to show code chunk in final output
  error      = TRUE,    # whether to display error messages
  message    = FALSE,   # whether to preserve messages
  warning    = FALSE,   # whether to preserve warnings
  comment    = "#>",    # a character string to append at start
  # of each line of results in final document
  tidy       = FALSE,   # whether to tidy code chunks for display
  dpi        = 96, 
  fig.width  = 6,       # consistent width for figures
  fig.asp    = 0.618,   # the golden ratio, can be adjusted in individual chunks
  out.width  = "100%",   # controls the output size
  fig.align  = "center" # give plot room to breathe
)


#' # Read Data

pacific_borrow <- read_excel(
  here("Pacific","Pacific-Borrowing articles 2016 2017 filled and cancelled.xlsx"),
  sheet = "Pacific_Borrowing_Sample")%>%janitor::clean_names()

pacific_lending <- read_excel(
  here("Pacific","Pacific-Lending Articles 2016 2017 Filled and Cancelled or Conditionaled.xlsx"),
  sheet = "Pacific_Lending_Sample")%>%janitor::clean_names()

pacific = bind_rows("borrow"=pacific_borrow,"lending"=pacific_lending,.id = "type")
pacific = pacific%>%mutate(
  doi=ifelse(substr(doi,1,1)=="1",doi,paste0("1",doi)) # some dois start with 0, assuming need a 10 instead
)

alldata = pacific

#' # Unpaywall

#' API: http://unpaywall.org/api/v2


url  <- "https://api.unpaywall.org/"
path <- "/v2/"
email <- "minnier@ohsu.edu"

#' get the result in JSON
raw.result <- GET(url = url, path = path)
raw.result
names(raw.result)


#' Need to make a function that creates an appended path
makePath <- function(classifier) {
  classifier = paste0("/v2/",classifier,"?email=",email)
  return(classifier)
}

#' make a query out of a list of articles

# make stacked data frame of all queried data sets
# add column query_used to choose doi or some other identifier
query_dois  <- unique(na.omit(alldata$doi))
query_paths <- lapply(as.list(tmpdois), makePath)

#' Some testing, one article first
#' Note this is different than OA button since it is not of the form ?url=, and also we need /v2/ which has
#' to be input into path not url (or else it goes away for some reason).

# should be
# https://api.unpaywall.org/v2/10.1371/journal.pone.0163591?email=minnier@ohsu.edu
# raw.result.test = GET(url = url, path = path, query = list(url=unlist(queryart$articles[2])))
raw.result = GET(url = url, path = query_paths[[2]])
names(raw.result)
this.raw.content <- rawToChar(raw.result$content)
this.content <- fromJSON(this.raw.content)
names(this.content)
this.content$best_oa_location$url

# test map
all_content = list(this.content,this.content)
purrr::map_df(all_content,magrittr::extract,
              c("doi","is_oa","journal_is_in_doaj","data_standard","title"))

# function to extract OA availability, after fromJSON
extract_availibility = function(rawcontent) {
  if(length(rawcontent$best_oa_location$url)>0){
    bind_cols(as_data_frame(rawcontent$best_oa_location$url),
              data_frame(
                evidence=rawcontent$best_oa_location$evidence,
                host_type=rawcontent$best_oa_location$host_type))%>%
      rename(url=value)
  }else{
    data_frame(url=NA) # could still add title rawcontent$title
  }
}
extract_availibility(fromJSON(rawToChar(raw.result$content)))
extract_availibility(GET(url = url, path = "/v2/test")) # should be NA



#' Now try all queries
#' 

unpaywall_raw <- vector(mode   = "list",
                        length = length(query_paths))

#' make sure 1 request per second

t0 = Sys.time()
for (i in 1:length(unpaywall.results)) {
  this.path        <- query_paths[[i]]
  raw <- GET(url = url, path = this.path)
  unpaywall_raw[[i]] <- fromJSON(rawToChar(raw$content))
  message(".", appendLF = FALSE)
  Sys.sleep(time = 1)
}
Sys.time()-t0

avail     <- unpaywall_raw%>%map_df(extract_availibility)
main_res  <- purrr::map_df(unpaywall_raw,magrittr::extract,
                          c("doi","is_oa","journal_is_in_doaj","data_standard","title"))
res0      <- bind_cols(main_res,avail)
res       <- left_join(alldata%>%mutate(query=doi),res0%>%add_column(query=query_dois),by="query")

#' write to a file:

write_csv(res,
          path=here::here("results",paste0(lubridate::today(),"_unpaywall.csv")))






