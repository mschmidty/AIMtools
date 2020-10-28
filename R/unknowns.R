#' counts number of unknowns that have been identified.
#'
#' @param x type list produced by \code{read_dima()}.
#' @param by default is FALSE.  Can count by unknown type if by is set to \code{type}
#' @return A table of the number of unkonwns with a FinalCode identified.
#' @examples
#' library(AIMtools)
#' # Make sure you are connected to AGOL Online
#' library(arcgisbinding)
#' arc.check_product()
#'
#' # Load data from AGOL online
#' agol_data<-load_data()
#'
#' #Table of identified unknowns.
#' unknown_count(agol_data)
#'
#' #Table of unknowns by type ("AF", "PF", "SH", etc.).
#' unknown_count(agol_data, by="type")
#'
unknown_count<-function(x, by=FALSE){
  if(by=="type"){
    x$unknown_plants%>%
      dplyr::filter(stringr::str_detect(PlotKey, "TRFO|COS01000"))%>%
      dplyr::mutate(ID_d = ifelse(is.na(FinalCode), "No", "Yes"),
                    Type = stringr::str_extract(UnknownCode, "^.."))%>%
      dplyr::count(ID_d, Type)%>%
      dplyr::arrange(Type)
  }else{
    x$unknown_plants%>%
      dplyr::filter(stringr::str_detect(PlotKey, "TRFO|COS01000"))%>%
      dplyr::mutate(ID_d = ifelse(is.na(FinalCode), "No", "Yes"))%>%
      dplyr::count(ID_d)
  }
}


