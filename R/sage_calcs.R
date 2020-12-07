#' calculates sage cover percent.
#'
#' @param x type list produced by \code{read_dima()}.
#' @param by this can be either \code{"plot"} or \code{"line"}. Default is \code{"line"}
#' @return a tibble of sage cover percentages.
#' @examples
#' library(AIMtools)
#' # Make sure you are connected to AGOL Online
#' library(arcgisbinding)
#' arc.check_product()
#'
#' # Load data from AGOL online
#' agol_data<-load_data()
#'
#' sage_cover(agol_data, by="line")
sage_cover<-function(x, by="line", gusg=TRUE){
  sagebrush_cover<-x%>%
    cover(type="detailed", by=by)%>%
    dplyr::filter(stringr::str_detect(symbol, "^AR"))%>%
    dplyr::filter(symbol != "ARPU9" & symbol != "ARCA14")%>%
    dplyr::mutate(sage_type = dplyr::case_when(
      symbol == "ARBI3" ~ "Bigelow sagebrush",
      symbol == "ARLU" ~ "White sagebrush",
      stringr::str_detect(symbol, "^ARTR") ~ "Big sagebrush",
      symbol == "ARNO4" ~ "Black sagebrush",
      stringr::str_detect(symbol, "^ARAR") ~ "Little sagebrush"
    ))
  if(gusg==TRUE){
    sagebrush_cover<-sagebrush_cover%>%
      dplyr::filter(stringr::str_detect(symbol, "^ARTR|^ARNO"))
  }
  return(sagebrush_cover)

}

dima_sage_cover<-function(x, by="line"){
  dima_cover(x, by=by)%>%
    dplyr::filter(stringr::str_detect(symbol, "^AR"))%>%
    dplyr::filter(symbol != "ARPU9" & symbol != "ARCA14")%>%
    dplyr::mutate(sage_type = dplyr::case_when(
      symbol == "ARBI3" ~ "Bigelow sagebrush",
      symbol == "ARLU" ~ "White sagebrush",
      stringr::str_detect(symbol, "^ARTR") ~ "Big sagebrush",
      symbol == "ARNO4" ~ "Black sagebrush",
      stringr::str_detect(symbol, "^ARAR") ~ "Little sagebrush",
    ))
}
