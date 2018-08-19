library(tidyverse)
library(readxl)
library(skimr)
library(janitor)

#info from http://tophcito.blogspot.com/2015/11/accessing-apis-from-r-and-little-r.html
library(httr)
library(jsonlite)
library(printr)
library(here)

#library(rcrossref)



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
