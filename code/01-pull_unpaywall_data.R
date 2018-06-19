#'---
#'title: "Pull Unpaywall Data"
#'author: "Jessica Minnier"
#'date: '`r Sys.Date()`'
#'output: github_document
#'---
  
#+ r setup, include=FALSE
library(tidyverse)
library(readxl)
library(skimr)
library(janitor)
library(here)

#info from http://tophcito.blogspot.com/2015/11/accessing-apis-from-r-and-little-r.html
library(httr)
library(jsonlite)

date_updated <- "2018-06-18"
unpaywall_datafile <- paste0(date_updated,"-unpaywall_raw.RData")
unpaywall_results_file <- paste0(date_updated,"-unpaywall_results.csv")
#unpaywall_results_file <- paste0(lubridate::today(),"_unpaywall.csv")
update_raw_data <- FALSE

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

tmpborrow <- read_excel(
  here("Pacific","Pacific-Borrowing articles 2016 2017 filled and cancelled.xlsx"),
  sheet = "Pacific_Borrowing_Sample")%>%janitor::clean_names()

tmplending <- read_excel(
  here("Pacific","Pacific-Lending Articles 2016 2017 Filled and Cancelled or Conditionaled.xlsx"),
  sheet = "Pacific_Lending_Sample")%>%janitor::clean_names()

pacific = bind_rows("borrow"=tmpborrow,"lending"=tmplending,.id = "type")%>%
  add_column(institution="pacific",.before = "type")

tmpborrow <- read_excel(
  here("PSU","PSU 2017articlesborrowing.xlsx"),
  sheet = "PSU_Borrowing_Sample",
  na = c("","N/A"))%>%janitor::clean_names()

tmplending <- read_excel(
  here("PSU","Copy of PSU 2017articleslending.xlsx"),
  sheet = "PSU_Lending_Sample",
  na = c("","N/A"))%>%janitor::clean_names()

psu = bind_rows("borrow"=tmpborrow,"lending"=tmplending,.id = "type")%>%
  add_column(institution="PSU",.before = "type")


tmpborrow <- read_excel(
  here("UofP","UP16-17_borrowing_articles.xlsx"),
  sheet = "UP_Borrowing_Sample",
  na = c("","N/A"))%>%janitor::clean_names()

tmplending <- read_excel(
  here("UofP","UP16_17_lending_articles.xlsx"),
  sheet = "UP_Lending_Sample",
  na = c("","N/A"))%>%janitor::clean_names()

uofp = bind_rows("borrow"=tmpborrow,"lending"=tmplending,.id = "type")%>%
  add_column(institution="UP",.before = "type") 

alldata = bind_rows(pacific,psu)
alldata = bind_rows(alldata,uofp)
alldata = alldata%>%mutate(
  doi=ifelse(substr(doi,1,1)=="0",paste0("1",doi),doi), # some dois start with 0, assuming need a 10 instead
  doi=gsub("\n","",doi), #\n at end of doi causes issues
  doi=gsub("doi:","",doi), #one had doi: in front
  doi=gsub("Get rights and content","",doi),
  doi_input = doi, # UP added non-dois to doi column, so removing these from query to save time
  doi=ifelse(substr(doi,1,1)=="1",doi,NA)
)

#' Number of unique DOIs:
length(unique(alldata$doi))

alldata%>%tabyl(institution)%>%adorn_pct_formatting()
alldata%>%tabyl(institution,type)

#' # Unpaywall

#' API: http://unpaywall.org/api/v2

url  <- "https://api.unpaywall.org/"
path <- "/v2/"
email <- "minnier@ohsu.edu"

#' get the result in JSON
raw.result <- GET(url = url, path = path)
raw.result
names(raw.result)


#' Make a function that creates an appended path
makePath <- function(classifier) {
  classifier = paste0("/v2/",classifier,"?email=",email)
  return(classifier)
}

#' make a query out of a list of articles

# make stacked data frame of all queried data sets
# add column query_used to choose doi or some other identifier
query_dois  <- unique(na.omit(alldata$doi))
query_paths <- lapply(as.list(query_dois), makePath)

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
# note, map_df + extract doesn't work if there is an error since these fields are not present, need to adapt this in the extract function
purrr::map_df(all_content,magrittr::extract,
              c("doi","is_oa","journal_is_in_doaj","data_standard","title"))

# function to extract OA availability, main contect, after fromJSON
extract_unpaywall_data = function(rawcontent) {
  rawcontent = jsonlite:::null_to_na(rawcontent)
  main_names = c("doi","is_oa","journal_is_in_doaj","data_standard","title")
  
  # first check for error
  if(length(rawcontent$HTTP_status_code)>0){
    error = TRUE
    message = rawcontent$message
    main_data <- matrix(NA,nrow=1,ncol=length(main_names),dimnames = list(NULL,main_names))%>%as_tibble()
    oa_avail <- data_frame(url=NA)
  }else{
    error = FALSE
    message = ifelse(is.null(rawcontent$message),NA,rawcontent$message) # likely null
    
    # get info
    main_data = rawcontent%>%magrittr::extract(main_names)%>%as_tibble()
    
    if(!is.na(rawcontent$best_oa_location[1])){
      oa_avail <- bind_cols(as_data_frame(rawcontent$best_oa_location$url),
                            data_frame(
                              evidence=rawcontent$best_oa_location$evidence,
                              host_type=rawcontent$best_oa_location$host_type))%>%
        rename(url=value)
    }else{
      oa_avail <- data_frame(url=NA) # could still add title rawcontent$title
    }
  }
  bind_cols(main_data,oa_avail)%>%add_column(error=error,message=message)
}
extract_unpaywall_data(fromJSON(rawToChar(raw.result$content)))
#extract_unpaywall_data(fromJSON(rawToChar(GET(url = url, path = "/v2/test")))) # should be NAs

tmp = GET(url = url, path = "/v2/10.1615/CritRevPhysRehabilMed.2015012338?email=minnier@ohsu.edu")
extract_unpaywall_data(fromJSON(rawToChar(tmp$content)))
# should get an error message
tmp = GET(url = url, path = "/v2/10.1615/CritRevPhysRehabilMed.201301029?email=minnier@ohsu.edu")
extract_unpaywall_data(fromJSON(rawToChar(tmp$content)))


#' Now try all queries
#' 

unpaywall_raw <- vector(mode   = "list",
                        length = length(query_paths))
length(unpaywall_raw)

#' make sure 1 request per second

safe_fromJSON = safely(fromJSON)

tryload = try(load(here("results",unpaywall_datafile)))
if((class(tryload)=="try-error")||(update_raw_data)){
  t0 <- Sys.time()
  for (i in 1:length(unpaywall_raw)) {
    this.path        <- query_paths[[i]]
    raw <- GET(url = url, path = this.path)
    unpaywall_raw[[i]] <- safe_fromJSON(rawToChar(raw$content))
    message(".", appendLF = FALSE)
    Sys.sleep(time = 1)
  }
  Sys.time()-t0

  unpaywall_raw0 = unpaywall_raw
  unpaywall_error = unpaywall_raw%>%map(magrittr::extract2,"error")
  unpaywall_raw = unpaywall_raw%>%map(magrittr::extract2,"result")
  names(unpaywall_error) = names(unpaywall_raw) = query_dois
  
  # if length==3, there was an error captured by openpaywall; if length==0, there was an application error
  unpaywall_raw%>%map_int(length)%>%table() 
  unpaywall_error%>%map_int(length)%>%table() 
  
  save(unpaywall_raw,unpaywall_error,file=here("results",unpaywall_datafile))
}


#jsonlite:::null_to_na(unpaywall_raw)[[2]]
main_res     <- unpaywall_raw%>%map_df(extract_unpaywall_data,.id="query")
# main_res  <- purrr::map(unpaywall_raw,magrittr::extract,
#                           c("doi","is_oa","journal_is_in_doaj","data_standard","title"))
# main_res  <- main_res%>%purrr::discard(is.null)
res       <- left_join(alldata%>%mutate(query=doi),main_res%>%rename(doi_unpaywall=doi),by="query")

#' write to a file:

write_csv(res,
          path=here::here("results",unpaywall_results_file))


res%>%tabyl(type,error)%>%adorn_title()

res%>%tabyl(is_oa)%>%adorn_percentages()

res %>% ggplot(aes(x=institution,fill=is_oa)) + geom_bar(position = "dodge")

# is_oa is null when either the doi was missing so could  not submit to unpaywall, or if there was an error (invalid doi usually)

# add flag for why is_oa is missing: missing doi in input vs invalid doi

