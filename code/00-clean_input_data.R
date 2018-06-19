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
