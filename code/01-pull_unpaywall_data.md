Pull Unpaywall Data
================
Jessica Minnier
2018-06-19

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

Unpaywall API
=============

API: <http://unpaywall.org/api/v2>

``` r
url  <- "https://api.unpaywall.org/"
path <- "/v2/"
email <- "minnier@ohsu.edu"
```

get the result in JSON

``` r
raw_result <- GET(url = url, path = path)
raw_result
```

    #> Response [https://api.unpaywall.org/v2/]
    #>   Date: 2018-06-19 20:34
    #>   Status: 200
    #>   Content-Type: application/json
    #>   Size: 103 B
    #> {
    #>   "documentation_url": "https://unpaywall.org/api/v2",
    #>   "msg": "Don't panic",
    #>   "version": "2.0.1"

``` r
names(raw_result)
```

    #>  [1] "url"         "status_code" "headers"     "all_headers" "cookies"    
    #>  [6] "content"     "date"        "times"       "request"     "handle"

Make a function that creates an appended path

``` r
make_path <- function(classifier) {
  classifier = paste0("/v2/",classifier,"?email=",email)
  return(classifier)
}
```

make a query out of a list of articles

``` r
query_dois  <- unique(na.omit(alldata$doi))
query_paths <- lapply(as.list(query_dois), make_path)
```

Some testing, one article first
-------------------------------

Note this is different than OA button since it is not of the form ?url=, and also we need /v2/ which has to be input into path not url (or else it goes away for some reason).

``` r
# should be
# https://api.unpaywall.org/v2/10.1371/journal.pone.0163591?email=minnier@ohsu.edu
# raw.result.test = GET(url = url, path = path, query = list(url=unlist(queryart$articles[2])))
raw_result = GET(url = url, path = query_paths[[2]])
names(raw_result)
```

    #>  [1] "url"         "status_code" "headers"     "all_headers" "cookies"    
    #>  [6] "content"     "date"        "times"       "request"     "handle"

``` r
this_raw_content <- rawToChar(raw_result$content)
this_content <- fromJSON(this_raw_content)
names(this_content)
```

    #>  [1] "best_oa_location"   "data_standard"      "doi"               
    #>  [4] "doi_url"            "genre"              "is_oa"             
    #>  [7] "journal_is_in_doaj" "journal_is_oa"      "journal_issns"     
    #> [10] "journal_name"       "oa_locations"       "published_date"    
    #> [13] "publisher"          "title"              "updated"           
    #> [16] "year"               "z_authors"

``` r
this_content$best_oa_location$url
```

    #> NULL

Function: extract\_unpaywall\_data()
====================================

Function to extract OA availability, main contect, after fromJSON:

``` r
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
```

| doi               | is\_oa | journal\_is\_in\_doaj |  data\_standard| title                                                                                                          | url | error | message |
|:------------------|:-------|:----------------------|---------------:|:---------------------------------------------------------------------------------------------------------------|:----|:------|:--------|
| 10.1159/000308973 | FALSE  | FALSE                 |               2| The Effect of Natural and Artificial Light via the Eye on the Hormonal and Metabolic Balance of Animal and Man | NA  | FALSE | NA      |

``` r
#extract_unpaywall_data(fromJSON(rawToChar(GET(url = url, path = "/v2/test")))) # should be NAs

tmp = GET(url = url, path = "/v2/10.1615/CritRevPhysRehabilMed.2015012338?email=minnier@ohsu.edu")
extract_unpaywall_data(fromJSON(rawToChar(tmp$content)))
```

| doi                                      | is\_oa | journal\_is\_in\_doaj |  data\_standard| title                                                                                                                 | url | error | message |
|:-----------------------------------------|:-------|:----------------------|---------------:|:----------------------------------------------------------------------------------------------------------------------|:----|:------|:--------|
| 10.1615/critrevphysrehabilmed.2015012338 | FALSE  | FALSE                 |               2| Short-Term Effects of Kinesiotaping on Fine Motor Function in Children with Cerebral Palsy-A Quasi-Experimental Study | NA  | FALSE | NA      |

``` r
# should get an error message
tmp = GET(url = url, path = "/v2/10.1615/CritRevPhysRehabilMed.201301029?email=minnier@ohsu.edu")
extract_unpaywall_data(fromJSON(rawToChar(tmp$content)))
```

| doi | is\_oa | journal\_is\_in\_doaj | data\_standard | title | url | error | message                                                                                                                   |
|:----|:-------|:----------------------|:---------------|:------|:----|:------|:--------------------------------------------------------------------------------------------------------------------------|
| NA  | NA     | NA                    | NA             | NA    | NA  | TRUE  | '10.1615/CritRevPhysRehabilMed.201301029' is an invalid doi. See <http://doi.org/10.1615/CritRevPhysRehabilMed.201301029> |

Run extract function on set of unique DOIs in sample
====================================================

Now try all queries

``` r
unpaywall_raw <- vector(mode   = "list",
                        length = length(query_paths))
length(unpaywall_raw)
```

    #> [1] 1231

make sure 1 request per second

``` r
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
```

Combine results with original data
----------------------------------

Merge with original data, some had missing or duplicate dois

``` r
# main_res  <- purrr::map(unpaywall_raw,magrittr::extract,
#                           c("doi","is_oa","journal_is_in_doaj","data_standard","title"))
# main_res  <- main_res%>%purrr::discard(is.null)
res <- left_join(alldata%>%mutate(query=doi),main_res%>%rename(doi_unpaywall=doi),by="query")

res$oa_result[is.na(res$oa_result)] = "no_doi_input"
```

Write to a file:
----------------

``` r
write_csv(res,
          path=here::here("results",unpaywall_results_file))
```

Unpaywall OA results:
=====================

``` r
res%>%tabyl(oa_result)
```

| oa\_result        |     n|    percent|
|:------------------|-----:|----------:|
| doi\_input\_error |    20|  0.0126342|
| no\_doi\_input    |   344|  0.2173089|
| oa\_found         |   210|  0.1326595|
| oa\_not\_found    |  1009|  0.6373973|

``` r
res%>%tabyl(oa_result,institution)%>%adorn_title()
```

|                   | institution |     |     |
|-------------------|:------------|-----|-----|
| oa\_result        | pacific     | PSU | UP  |
| doi\_input\_error | 14          | 4   | 2   |
| no\_doi\_input    | 99          | 134 | 111 |
| oa\_found         | 72          | 55  | 83  |
| oa\_not\_found    | 357         | 340 | 312 |

``` r
res%>%tabyl(oa_result,type)%>%adorn_title()
```

|                   | type   |         |
|-------------------|:-------|---------|
| oa\_result        | borrow | lending |
| doi\_input\_error | 9      | 11      |
| no\_doi\_input    | 222    | 122     |
| oa\_found         | 114    | 96      |
| oa\_not\_found    | 465    | 544     |

``` r
res %>% ggplot(aes(x=institution,fill=oa_result)) + geom_bar(position = "dodge") + 
  theme_minimal()
```

<img src="01-pull_unpaywall_data_files/figure-markdown_github/unnamed-chunk-15-1.png" width="100%" style="display: block; margin: auto;" />

OA results: evidence
--------------------

``` r
res%>%filter(is_oa==1)%>%tabyl(evidence)%>%adorn_pct_formatting()
```

| evidence                                                 |    n| percent |
|:---------------------------------------------------------|----:|:--------|
| oa journal (via doaj)                                    |    3| 1.4%    |
| oa repository (via OAI-PMH doi match)                    |   48| 22.9%   |
| oa repository (via OAI-PMH title and first author match) |   53| 25.2%   |
| oa repository (via OAI-PMH title and last author match)  |    1| 0.5%    |
| oa repository (via OAI-PMH title match)                  |    1| 0.5%    |
| oa repository (via pmcid lookup)                         |    1| 0.5%    |
| open (via crossref license, author manuscript)           |    2| 1.0%    |
| open (via crossref license)                              |    5| 2.4%    |
| open (via free pdf)                                      |   79| 37.6%   |
| open (via page says license)                             |   14| 6.7%    |
| open (via page says Open Access)                         |    3| 1.4%    |

Which dois result in an error?
------------------------------

``` r
res%>%filter(error)%>%select(institution,type,query,error,message)%>%kable
```

| institution | type    | query                                   | error | message                                                                                                                   |
|:------------|:--------|:----------------------------------------|:------|:--------------------------------------------------------------------------------------------------------------------------|
| pacific     | borrow  | 10.1007/s0059                           | TRUE  | '10.1007/s0059' is an invalid doi. See <http://doi.org/10.1007/s0059>                                                     |
| pacific     | borrow  | 10.1007/s1182                           | TRUE  | '10.1007/s1182' is an invalid doi. See <http://doi.org/10.1007/s1182>                                                     |
| pacific     | borrow  | 10.1007/s4061                           | TRUE  | '10.1007/s4061' is an invalid doi. See <http://doi.org/10.1007/s4061>                                                     |
| pacific     | borrow  | 10.1234/0123456701234567891             | TRUE  | '10.1234/0123456701234567891' is an invalid doi. See <http://doi.org/10.1234/0123456701234567891>                         |
| pacific     | borrow  | 10.3233/NRE-2012-0775                   | TRUE  | '10.3233/NRE-2012-0775' is an invalid doi. See <http://doi.org/10.3233/NRE-2012-0775>                                     |
| pacific     | borrow  | 10.3290/j.ohpd.a32823                   | TRUE  | '10.3290/j.ohpd.a32823' is an invalid doi. See <http://doi.org/10.3290/j.ohpd.a32823>                                     |
| pacific     | lending | 10.1093/clipsy.6.1.6                    | TRUE  | '10.1093/clipsy.6.1.6' is an invalid doi. See <http://doi.org/10.1093/clipsy.6.1.6>                                       |
| pacific     | lending | 10.1300/J294v10n03\_08                  | TRUE  | '10.1300/J294v10n03\_08' is an invalid doi. See <http://doi.org/10.1300/J294v10n03_08>                                    |
| pacific     | lending | 10.1615/CritRevPhysRehabilMed.201301029 | TRUE  | '10.1615/CritRevPhysRehabilMed.201301029' is an invalid doi. See <http://doi.org/10.1615/CritRevPhysRehabilMed.201301029> |
| pacific     | lending | 10.1922/CDH\_3716Gibson05               | TRUE  | '10.1922/CDH\_3716Gibson05' is an invalid doi. See <http://doi.org/10.1922/CDH_3716Gibson05>                              |
| pacific     | lending | 10.3290/j.ohpd.a29374                   | TRUE  | '10.3290/j.ohpd.a29374' is an invalid doi. See <http://doi.org/10.3290/j.ohpd.a29374>                                     |
| pacific     | lending | 10.3290/j.ohpd.a32679                   | TRUE  | '10.3290/j.ohpd.a32679' is an invalid doi. See <http://doi.org/10.3290/j.ohpd.a32679>                                     |
| pacific     | lending | 10.3290/j.qi.a31533                     | TRUE  | '10.3290/j.qi.a31533' is an invalid doi. See <http://doi.org/10.3290/j.qi.a31533>                                         |
| pacific     | lending | 10.3899/jrheum                          | TRUE  | '10.3899/jrheum' is an invalid doi. See <http://doi.org/10.3899/jrheum>                                                   |
| PSU         | borrow  | 10.15517/rfl.v16i2.19484                | TRUE  | '10.15517/rfl.v16i2.19484' is an invalid doi. See <http://doi.org/10.15517/rfl.v16i2.19484>                               |
| PSU         | lending | 10.3726/978-3-0353-0378-0\_13           | TRUE  | '10.3726/978-3-0353-0378-0\_13' is an invalid doi. See <http://doi.org/10.3726/978-3-0353-0378-0_13>                      |
| PSU         | lending | 10.4018/ijepr.201504010                 | TRUE  | '10.4018/ijepr.201504010' is an invalid doi. See <http://doi.org/10.4018/ijepr.201504010>                                 |
| PSU         | lending | 10.1080/03007769508591590               | TRUE  | '10.1080/03007769508591590' is an invalid doi. See <http://doi.org/10.1080/03007769508591590>                             |
| UP          | borrow  | 10.1080/00220973.1994.11072347          | TRUE  | '10.1080/00220973.1994.11072347' is an invalid doi. See <http://doi.org/10.1080/00220973.1994.11072347>                   |
| UP          | borrow  | 10.2310/7070.2009.080165                | TRUE  | '10.2310/7070.2009.080165' is an invalid doi. See <http://doi.org/10.2310/7070.2009.080165>                               |
