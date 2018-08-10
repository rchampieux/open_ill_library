#'---
#'title: "Pull OA Button Data"
#'author: "Jessica Minnier"
#'date: '`r Sys.Date()`'
#'output: github_document
#'---

#+ r setup, include=FALSE
library(here)
source(here("code","00-front_matter.R"))

# UPDATE THIS TO CHANGE FILES
date_updated <- "2018-07-01"
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
apikey <- "7d8ba1ed9bd29e178475b9b8f2c211"

# get the result in JSON
raw.result <- GET(url = url, path = path)
raw.result
names(raw.result)

path <- "availability"

#' Need to make a function that creates a list out of a query and adds appropriate name
makeQuery <- function(classifier) {
  classifier = paste0(classifier,"?apikey=",apikey)
  this.query <- list(classifier)
  names(this.query) <- "url"
  return(this.query)
}

# make a query out of a list of articles
alldata = alldata %>% 
  mutate(query=ifelse(is.na(doi),article_title,doi)) %>% 
  mutate(query=ifelse(is.na(query),photo_article_title,query))
query_all  <- unique(na.omit(alldata$query))
nrow(alldata)
length(query_all)
queries <- lapply(as.list(query_all), makeQuery)

#' Some testing, one article first

# should be
# https://api.openaccessbutton.org/availability?url=http%3A%2F%2Fjournals.plos.org%2Fplosone%2Farticle%3Fid%3Dinfo%253Adoi%2F10.1371%2Fjournal.pone.0163591
# raw.result.test = GET(url = url, path = path, query = list(url=unlist(alldata$doi[1])))
raw.result = GET(url = url, path = path, query = queries[[6]]) # a doi
# raw.result = GET(url = url, path = path, query = queries[[191]]) # a title
names(raw.result)
this.raw.content <- rawToChar(raw.result$content)
this.content <- fromJSON(this.raw.content)
this.content$data$availability

extract_availibility = function(rawcontent) {
  if(length(rawcontent$data$availability)>0){
    bind_cols(data_frame(match=rawcontent$data$match),
              as_data_frame(rawcontent$data$availability),
              data_frame(source=rawcontent$data$meta$article$source))
  }else{
    bind_cols(data_frame(match=rawcontent$data$match),url=NA)
  }
}
extract_availibility(this.content)
raw = GET(url = url, path = path, query = queries[[6]])
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
    raw              <- GET(url = url, path = path, query = this.query)
    oabutton_raw[[i]] <-  safe_fromJSON(rawToChar(raw$content))
    message(".", appendLF = FALSE)
    if(i%%50==0) print(i)
    Sys.sleep(time = 1)
  }
  Sys.time()-t0
  
  names(oabutton_raw) = query_all
  oabutton_raw0 = oabutton_raw
  oabutton_error = oabutton_raw0%>%map(magrittr::extract2,"error")
  oabutton_raw = oabutton_raw0%>%map(magrittr::extract2,"result")
  names(oabutton_error) = names(oabutton_raw) = query_all
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
                                           oabutton_source=source),by="query")
#' ## Write to a file:

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
res%>%filter(oabutton_oa_result=="oa_found")%>%tabyl(oabutton_source)%>%adorn_pct_formatting()




