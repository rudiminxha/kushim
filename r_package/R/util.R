#' Test
#' @param none
#'
#' @get_info_around_me

get_info_around_me <- function(long=40.70823, lat=-74.01052, mile_radius=0.5, type = ''){
  endpoint <- "http://live.dbpedia.org/sparql"
  options <- NULL
  prefix <- c("db","http://dbpedia.org/resource/")

  q <- get_search_query(long, lat, mile_radius, type)

  res <- SPARQL(endpoint,q,ns=prefix,extra=options)$results

  res

}



##QUERIES##

get_search_query <- function(long=40.70823, lat=-74.01052, mile_radius=0.5, type = ''){
  #convert miles to degrees with approximation
  radius <- mile_radius/69
  long_beg <- long - radius
  long_end <- long + radius
  lat_beg <- lat - radius
  lat_end <- lat + radius

  sparql_prefix <- "PREFIX dbp: <http://dbpedia.org/property/>
     PREFIX dc: <http://purl.org/dc/terms/>
     PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
     PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>"


  if(type == ''){
    q <- paste(sparql_prefix,
      sprintf('SELECT * WHERE {
      ?s geo:lat ?lat .
      ?s geo:long ?long .
      FILTER(?lat > %s && ?lat < %s && ?long > %s && ?long < %s)}
      ',long_beg, long_end, lat_beg, lat_end)
  )  
  return(q)  
  }
  
  q <- paste(sparql_prefix,
    sprintf('SELECT * WHERE {
    ?s a dbo:%s .
    ?s geo:lat ?lat .
    ?s geo:long ?long .
    FILTER(?lat > %s && ?lat < %s && ?long > %s && ?long < %s)}
      ',type, lat_beg, lat_end, long_beg, long_end)
  )
  return(q)  
}


