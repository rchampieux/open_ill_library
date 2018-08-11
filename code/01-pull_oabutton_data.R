#'---
#'title: "Pull OA Button Data"
#'author: "Jessica Minnier"
#'date: '`r Sys.Date()`'
#'output: github_document
#'---

#+ r setup, include=FALSE
library(here)
library(urltools)
library(glue)
source(here("code","00-front_matter.R"))

# UPDATE THIS TO CHANGE FILES
#date_updated <- "2018-07-01"
date_updated <- "2018-08-10"
oabutton_datafile <- paste0(date_updated,"-oabutton_raw.RData")
oabutton_results_file <- paste0(date_updated,"-oabutton_results.csv")
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

#' # Open Access Button
#' 
#' Info: https://openaccessbutton.org/api

url  <- "https://api.openaccessbutton.org"
path <- "/"
get_apikey <- try(load(file="~/Dropbox/oabutton_apikey.RData")) # apikey
if(class(get_apikey)=="try-error") {apikey <- ""} # for others running

#' A blank query. Get the result in JSON
raw.result <- GET(url = url, path = path)
raw.result
names(raw.result)

path <- "availability"

#' Need to make a function that creates the appropriate URL
# Note: need to encode just classifier or else it doesn't work in api
makeQuery <- function(classifier, name="url") {
  classifier = paste0(url,"/",path,"?",name,"=",urltools::url_encode(classifier),"&apikey=",apikey)
  return(classifier)
}

#' Make a query out of a list of articles; if DOI not available, use title.
alldata = alldata %>% 
  mutate(query=ifelse(is.na(doi),article_title,doi),
         queryname=ifelse(is.na(doi),"title","doi")) %>% 
  mutate(query=ifelse(is.na(query),photo_article_title,query),
         queryname=ifelse(is.na(query),"title",queryname))

query_data  <- alldata%>%select(query,queryname) %>% filter(!is.na(query)) %>% unique %>% 
  add_column(url=url,apikey=apikey,path=path) %>%
  mutate(
    query_nice = urltools::url_encode(query),
    query_path = (glue::glue("{url}/{path}?{queryname}={query_nice}&apikey={apikey}")))

nrow(alldata)
nrow(query_data)
queries <- query_data$query_path


#' Some testing, one article first

# should be
# https://api.openaccessbutton.org/availability?url=http%3A%2F%2Fjournals.plos.org%2Fplosone%2Farticle%3Fid%3Dinfo%253Adoi%2F10.1371%2Fjournal.pone.0163591
# raw.result.test = GET(url = url, path = path, query = list(url=unlist(alldata$doi[1])))
# raw.result = GET(makeQuery("10.1371/journal.pone.0163591")) # example
# raw.result = GET(queries[[191]]) # a title
raw.result = GET(queries[[28]]) # a doi with OA
names(raw.result)
this.raw.content <- rawToChar(raw.result$content)
this.content <- fromJSON(this.raw.content)
this.content$data$availability

rawcontent = this.content # for testing
extract_availibility = function(rawcontent) {
  if(length(rawcontent$data$availability)>0){
    bind_cols(data_frame(match=rawcontent$data$match),
              as_data_frame(rawcontent$data$availability),
              data_frame(
                source=rawcontent$data$meta$article$source,
                title=ifelse(length(rawcontent$data$meta$article$title)>0,rawcontent$data$meta$article$title,NA) # not always available
                ))
  }else{
    bind_cols(data_frame(match=rawcontent$data$match),url=NA)
  }
}
extract_availibility(this.content)

#' Testing the extraction function:
#' 
raw = GET(queries[[6]])
extract_availibility(fromJSON(rawToChar(raw$content)))

raw = GET(makeQuery("10.1016/j.chemosphere.2016.06.071"))
extract_availibility(fromJSON(rawToChar(raw$content)))

raw = GET(makeQuery("Using aquatic vegetation to remediate nitrate, ammonium, and soluble reactive
2 phosphorus in simulated runoff",name="title"))
extract_availibility(fromJSON(rawToChar(raw$content)))

#' Now try all queries
#' 

tryload = try(load(here("results",oabutton_datafile)))
if((class(tryload)=="try-error")||(update_raw_data)){

  oabutton_raw <- vector(mode   = "list",
                         length = length(queries))
  safe_fromJSON = safely(fromJSON)
  
  #' make sure 1 request per second

  t0 = Sys.time()
  for (i in 1:length(oabutton_raw)) {
    this.query       <- queries[[i]]
    raw              <- GET(this.query)
    oabutton_raw[[i]] <-  safe_fromJSON(rawToChar(raw$content))
    message(".", appendLF = FALSE)
    if(i%%50==0) print(i)
    Sys.sleep(time = 1)
  }
  Sys.time()-t0
  
  names(oabutton_raw) = query_data$query
  oabutton_raw0 <- oabutton_raw
  oabutton_error <- oabutton_raw0%>%map(magrittr::extract2,"error")
  oabutton_raw <- oabutton_raw0%>%map(magrittr::extract2,"result")
  names(oabutton_error) <- names(oabutton_raw) <- query_data$query
  rm(oabutton_raw0)
  
  save(oabutton_raw, oabutton_error, file=here("results",oabutton_datafile))
}

main_res     <- oabutton_raw%>%map_df(extract_availibility,.id="query")

#' Note OA button finds a doi for this but it doesn't match the title nor the author; 
#' could be other examples of this?
#' 
#' 
main_res[259,]
alldata[match(main_res$query[259],alldata$photo_article_title),]

main_res     <- main_res%>%mutate(
  oabutton_oa_result = case_when(
    is.na(source) ~ "oa_not_found",
    !is.na(source) ~ "oa_found")
)

#' Unique queries: OA found
#' 
main_res%>%tabyl(oabutton_oa_result)

#' Merge with original data, some had missing or duplicate dois

res <- left_join(alldata,main_res%>%rename(oabutton_match=match,
                                           oabutton_url=url,
                                           oabutton_type=type,
                                           oabutton_title=title,
                                           oabutton_source=source),by="query")
# res <- res %>% mutate(
#   oabutton_oa_result = ifelse(is.na(oabutton_oa_result),"no_doi_or_title_input",oabutton_oa_result)
#   )

#' ## Write to a file:
#' 
res <- res %>% rename(oabutton_query=query,oabutton_queryname=queryname)

write_csv(res,
          path=here::here("results",oabutton_results_file))


#' # OA Button OA results:
#' 
res%>%tabyl(oabutton_oa_result)
res%>%tabyl(oabutton_oa_result,institution)%>%adorn_title()
res%>%tabyl(oabutton_oa_result,type)%>%adorn_title()

res %>% ggplot(aes(x=institution,fill=oabutton_oa_result)) + geom_bar(position = "dodge") + 
  theme_minimal()

#' ## OA results: source
#' 
res%>%filter(oabutton_oa_result=="oa_found")%>%tabyl(oabutton_source)%>%
  adorn_totals()%>%
  adorn_pct_formatting()




