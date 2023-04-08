#https://cran.r-project.org/web/packages/ghql/ghql.pdf
#https://gist.github.com/rentrop/83cb1d8fc8593726a808032e55314019

# Packages ----
require(httr)
require(jsonlite)

# Function ----

GQL <- function(query, 
                ..., 
                .token = NULL,
                .variables = NULL, 
                .operationName = NULL, 
                .url = url){
  pbody <- list(query = query, variables = .variables, operationName = .operationName)
  if(is.null(.token)){
    res <- POST(.url, body = pbody, encode="json", ...)
  } else {
    auth_header <- paste("bearer", .token)
    res <- POST(.url, body = pbody, encode="json", add_headers(Authorization=auth_header), ...)
  }
  res <- content(res, as = "parsed", encoding = "UTF-8")
  if(!is.null(res$errors)){
    warning(toJSON(res$errors))
  }
  res$data
}