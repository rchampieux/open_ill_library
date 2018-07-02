Pull OA Button Data
================
Jessica Minnier
2018-07-02

Read Data
=========

``` r
source(here("code","00-clean_input_data.R"))
```

Number of samples per institution:

``` r
alldata%>%tabyl(institution)%>%adorn_pct_formatting()%>%adorn_totals()
```

| institution |     n| percent |
|:------------|-----:|:--------|
| pacific     |   542| 34.2%   |
| PSU         |   533| 33.7%   |
| UP          |   508| 32.1%   |
| Total       |  1583| -       |

``` r
alldata%>%tabyl(institution,type)%>%adorn_totals()
```

| institution |  borrow|  lending|
|:------------|-------:|--------:|
| pacific     |     262|      280|
| PSU         |     278|      255|
| UP          |     270|      238|
| Total       |     810|      773|

Number of unique DOIs:

``` r
length(unique(alldata$doi))
```

    #> [1] 1232

A few DOIs show up twice:

``` r
tmp = sort(table(alldata$doi),decreasing = TRUE)
tmp[tmp>1]
```

|  10.1016/0045-7930(86)90013-7|  10.1016/0091-3057(84)90199-0|  10.1016/S0304-3878(02)00131-1|  10.1080/00141844.2015.1028564|  10.1080/02687030902732745|  10.1097/HRP.0000000000000100|  10.1177/1747954116655049|  10.3109/01612840.2015.1055020|
|-----------------------------:|-----------------------------:|------------------------------:|------------------------------:|--------------------------:|-----------------------------:|-------------------------:|------------------------------:|
|                             2|                             2|                              2|                              2|                          2|                             2|                         2|                              2|

Open Access Button
==================

Info: <https://openaccessbutton.org/api>

``` r
url  <- "https://api.openaccessbutton.org"
path <- "/"
apikey <- "7d8ba1ed9bd29e178475b9b8f2c211"

# get the result in JSON
raw.result <- GET(url = url, path = path)
raw.result
```

    #> Response [https://api.openaccessbutton.org/]
    #>   Date: 2018-07-02 19:35
    #>   Status: 200
    #>   Content-Type: application/json; charset=utf-8
    #>   Size: 43 B
    #> {
    #>   "data": "The Open Access Button API."

``` r
names(raw.result)
```

    #>  [1] "url"         "status_code" "headers"     "all_headers" "cookies"    
    #>  [6] "content"     "date"        "times"       "request"     "handle"

``` r
path <- "availability"
```

Need to make a function that creates a list out of a query and adds appropriate name

``` r
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
```

    #> [1] 1583

``` r
length(query_all)
```

    #> [1] 1575

``` r
queries <- lapply(as.list(query_all), makeQuery)
```

Some testing, one article first

``` r
# should be
# https://api.openaccessbutton.org/availability?url=http%3A%2F%2Fjournals.plos.org%2Fplosone%2Farticle%3Fid%3Dinfo%253Adoi%2F10.1371%2Fjournal.pone.0163591
# raw.result.test = GET(url = url, path = path, query = list(url=unlist(alldata$doi[1])))
raw.result = GET(url = url, path = path, query = queries[[6]]) # a doi
# raw.result = GET(url = url, path = path, query = queries[[191]]) # a title
names(raw.result)
```

    #>  [1] "url"         "status_code" "headers"     "all_headers" "cookies"    
    #>  [6] "content"     "date"        "times"       "request"     "handle"

``` r
this.raw.content <- rawToChar(raw.result$content)
this.content <- fromJSON(this.raw.content)
this.content$data$availability
```

    #> list()

``` r
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
```

| match                                                                             | url |
|:----------------------------------------------------------------------------------|:----|
| <https://doi.org/10.1001/archpsyc.60.3.303?apikey=7d8ba1ed9bd29e178475b9b8f2c211> | NA  |

``` r
raw = GET(url = url, path = path, query = queries[[6]])
extract_availibility(fromJSON(rawToChar(raw$content)))
```

| match                                                                             | url |
|:----------------------------------------------------------------------------------|:----|
| <https://doi.org/10.1001/archpsyc.60.3.303?apikey=7d8ba1ed9bd29e178475b9b8f2c211> | NA  |

Now try all queries

``` r
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
```

Note OA button finds a doi for this but it doesn't match the title nor the author; could be other examples of this?

``` r
main_res[259,]
```

| query                                                                                                                                                                                                                                                      | match                                              | url | type | source |
|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:---------------------------------------------------|:----|:-----|:-------|
| Does early intervention for psychosis work? An analysis of outcomes of early intervention in psychosis based on the critical period hypothesis, measured by number of admissions and bed days used over a period of six years, the first three in an early | <https://doi.org/10.1097/00004850-199801001-00006> | NA  | NA   | NA     |

``` r
alldata[match(main_res$query[259],alldata$photo_article_title),]
```

| institution | type   | doi | photo\_article\_title                                                                                                                                                                                                                                      | photo\_journal\_title | photo\_journal\_volume | photo\_journal\_issue | photo\_journal\_year | photo\_journal\_inclusive\_pages | photo\_article\_author | transaction\_status |  transaction\_date| issn      |  base\_fee| lending\_library | reason\_for\_cancellation | call\_number | location        | maxcost | document\_type | system\_id | ifm\_cost | copyright\_payment\_method | ccc\_number | copyright\_comp | article\_title | journal\_title | volume | issue | year | pages | article\_author | x\_1 | cited\_in |  creation\_date| photo\_item\_publisher | notes\_on\_access | notes | doi\_input |  doi\_present| query                                                                                                                                                                                                                                                      |
|:------------|:-------|:----|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:----------------------|:-----------------------|:----------------------|:---------------------|:---------------------------------|:-----------------------|:--------------------|------------------:|:----------|----------:|:-----------------|:--------------------------|:-------------|:----------------|:--------|:---------------|:-----------|:----------|:---------------------------|:------------|:----------------|:---------------|:---------------|:-------|:------|:-----|:------|:----------------|:-----|:----------|---------------:|:-----------------------|:------------------|:------|:-----------|-------------:|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| pacific     | borrow | NA  | Does early intervention for psychosis work? An analysis of outcomes of early intervention in psychosis based on the critical period hypothesis, measured by number of admissions and bed days used over a period of six years, the first three in an early | Psychiatria Danubina. | 22 Suppl 1             | 1                     | 2010                 | S72-                             | Agius, Mark            | Request Finished    |           42704.42| 0353-5053 |         NA| OA               | NA                        | NA           | JOURNAL WEBSITE | NA      | Article        | OTH        | NA        | NA                         | NA          | US:CCL          | NA             | NA             | NA     | NA    | NA   | NA    | NA              | NA   | NA        |              NA| NA                     | NA                | NA    | NA         |             0| Does early intervention for psychosis work? An analysis of outcomes of early intervention in psychosis based on the critical period hypothesis, measured by number of admissions and bed days used over a period of six years, the first three in an early |

``` r
main_res     <- main_res%>%mutate(
  oabutton_oa_result = case_when(
    is.na(source) ~ "oa_not_found",
    !is.na(source) ~ "oa_found")
)
```

Merge with original data, some had missing or duplicate dois

``` r
res <- left_join(alldata,main_res%>%rename(oabutton_match=match,
                                           oabutton_url=url,
                                           oabutton_type=type,
                                           oabutton_source=source),by="query")
```

Write to a file:
----------------

``` r
write_csv(res,
          path=here::here("results",oabutton_results_file))
```

OA Button OA results:
=====================

``` r
res%>%tabyl(oabutton_oa_result)
```

| oabutton\_oa\_result |     n|    percent|
|:---------------------|-----:|----------:|
| oa\_found            |    35|  0.0221099|
| oa\_not\_found       |  1548|  0.9778901|

``` r
res%>%tabyl(oabutton_oa_result,institution)%>%adorn_title()
```

|                      | institution |     |     |
|----------------------|:------------|-----|-----|
| oabutton\_oa\_result | pacific     | PSU | UP  |
| oa\_found            | 10          | 9   | 16  |
| oa\_not\_found       | 532         | 524 | 492 |

``` r
res%>%tabyl(oabutton_oa_result,type)%>%adorn_title()
```

|                      | type   |         |
|----------------------|:-------|---------|
| oabutton\_oa\_result | borrow | lending |
| oa\_found            | 18     | 17      |
| oa\_not\_found       | 792    | 756     |

``` r
res %>% ggplot(aes(x=institution,fill=oabutton_oa_result)) + geom_bar(position = "dodge") + 
  theme_minimal()
```

<img src="01-pull_oabutton_data_files/figure-markdown_github/unnamed-chunk-12-1.png" width="100%" style="display: block; margin: auto;" />

OA results: source
------------------

``` r
res%>%filter(oabutton_oa_result=="oa_found")%>%tabyl(oabutton_source)%>%adorn_pct_formatting()
```

| oabutton\_source |    n| percent |
|:-----------------|----:|:--------|
| base             |   25| 71.4%   |
| doaj             |    3| 8.6%    |
| eupmc            |    2| 5.7%    |
| oadoi            |    5| 14.3%   |
