#'---
#'title: "Pull Unpaywall Data"
#'author: "Jessica Minnier"
#'date: '`r Sys.Date()`'
#'output: github_document
#'---
  
#+ r setup, include=FALSE
library(here)
source(here("code","00-front_matter.R"))

# UPDATE THIS TO CHANGE FILES
date_updated <- "2018-06-18"
unpaywall_datafile <- paste0(date_updated,"-unpaywall_raw.RData")
unpaywall_results_file <- paste0(date_updated,"-unpaywall_results.csv")
update_raw_data <- FALSE

#' # Read Data

source(here("code","00-clean_input_data.R"))

#' Number of samples per institution:
alldata%>%tabyl(institution)%>%adorn_pct_formatting()%>%adorn_totals()

alldata%>%tabyl(institution,type)%>%adorn_totals()


#' Number of unique DOIs:
#' 
length(unique(alldata$doi))

#' A few DOIs show up twice:
#' 
tmp = sort(table(alldata$doi),decreasing = TRUE)
tmp[tmp>1]



#' # Unpaywall API

#' API: http://unpaywall.org/api/v2

url  <- "https://api.unpaywall.org/"
path <- "/v2/"
email <- "minnier@ohsu.edu"

#' get the result in JSON
raw_result <- GET(url = url, path = path)
raw_result
names(raw_result)


#' Make a function that creates an appended path
make_path <- function(classifier) {
  classifier = paste0("/v2/",classifier,"?email=",email)
  return(classifier)
}

#' make a query out of a list of articles

query_dois  <- unique(na.omit(alldata$doi))
query_paths <- lapply(as.list(query_dois), make_path)

#' ## Some testing, one article first
#' 
#' Note this is different than OA button since it is not of the form ?url=, and also we need /v2/ which has
#' to be input into path not url (or else it goes away for some reason).

# should be
# https://api.unpaywall.org/v2/10.1371/journal.pone.0163591?email=minnier@ohsu.edu
# raw.result.test = GET(url = url, path = path, query = list(url=unlist(queryart$articles[2])))
raw_result = GET(url = url, path = query_paths[[2]])
names(raw_result)
this_raw_content <- rawToChar(raw_result$content)
this_content <- fromJSON(this_raw_content)
names(this_content)
this_content$best_oa_location$url

#' # Function: extract_unpaywall_data()
#' 
#' Function to extract OA availability, main contect, after fromJSON:
#' 
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
extract_unpaywall_data(fromJSON(rawToChar(raw_result$content)))
#extract_unpaywall_data(fromJSON(rawToChar(GET(url = url, path = "/v2/test")))) # should be NAs

tmp = GET(url = url, path = "/v2/10.1615/CritRevPhysRehabilMed.2015012338?email=minnier@ohsu.edu")
extract_unpaywall_data(fromJSON(rawToChar(tmp$content)))
# should get an error message
tmp = GET(url = url, path = "/v2/10.1615/CritRevPhysRehabilMed.201301029?email=minnier@ohsu.edu")
extract_unpaywall_data(fromJSON(rawToChar(tmp$content)))


#' # Run extract function on set of unique DOIs in sample
#' 
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
main_res     <- main_res%>%mutate(
  oa_result = case_when(
    error ~ "doi_input_error",
    is_oa ~ "oa_found",
    !is_oa ~ "oa_not_found")
)

#' Unique queries: OA found
#' 
main_res%>%tabyl(oa_result)


#' ## Combine results with original data
#' 
#' Merge with original data, some had missing or duplicate dois
                                  
# main_res  <- purrr::map(unpaywall_raw,magrittr::extract,
#                           c("doi","is_oa","journal_is_in_doaj","data_standard","title"))
# main_res  <- main_res%>%purrr::discard(is.null)
colnames(main_res)[-1] <- paste0("unpaywall_",colnames(main_res)[-1])
res <- left_join(alldata%>%mutate(query=doi),main_res,by="query")

res$unpaywall_oa_result[is.na(res$unpaywall_oa_result)] = "no_doi_input"

#' ## Write to a file:
#' 
res <- res %>% mutate(unpaywall_query = query)

write_csv(res,
          path=here::here("results",unpaywall_results_file))

#' # Unpaywall OA results:
#' 
res%>%tabyl(unpaywall_oa_result)
res%>%tabyl(unpaywall_oa_result,institution)%>%adorn_title()
res%>%tabyl(unpaywall_oa_result,type)%>%adorn_title()

res %>% ggplot(aes(x=institution,fill=unpaywall_oa_result)) + geom_bar(position = "dodge") + 
  theme_minimal()

#' ## OA results: evidence
#' 
res%>%filter(unpaywall_is_oa==1)%>%tabyl(unpaywall_evidence)%>%adorn_pct_formatting()


#' ## Which dois result in an error?
#' 
res%>%filter(unpaywall_error)%>%select(institution,type,query,unpaywall_error,unpaywall_message)%>%kable




