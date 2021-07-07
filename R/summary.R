#' produces a summary of how many plots have been completed "Eval" and how many plots have been "Rejected
#'
#' @param x type list produced by \code{read_dima()} or a list produced from \code{read_dima()} (if a DIMA list, dima must be set to \code{TRUE}.
#' @param year if you want an identifying column to be added to the resulting tibble add a numeric year.
#' @param dima if \code{TRUE} x is expected to be a DIMA list created from \code{read_dima()}.

eval_stats<-function(x, year = FALSE, dima=FALSE){
  if(dima==FALSE){
    summary_table<-x$plots%>%
      dplyr::filter(stringr::str_detect(PlotKey, "COS01000|TRFO"))%>%
      dplyr::count(stratum, EvalStatus, sort=T)%>%
      dplyr::filter(EvalStatus!="OverSample" & EvalStatus!="Calibration")%>%
      tidyr::pivot_wider(names_from=EvalStatus, values_from=n)%>%
      dplyr::mutate_at(vars(Eval, Rejected), replace_na, 0)
  }
  if(dima){
    summary_table<-x$plots%>%
      dplyr::select(PlotID)%>%
      dplyr::filter(stringr::str_detect(PlotID, "^[A-Z][A-Z]"))%>%
      dplyr::mutate(strata=str_extract(PlotID, "^GRSGInt|^[A-Z][A-Z]"))%>%
      strata_full_names()%>%
      dplyr::count(Strata, sort=TRUE)%>%
      dplyr::rename(Eval = n)%>%
      dplyr::mutate(Rejected = NA)
  }

  if(is.numeric(year)){
    summary_table<-summary_table%>%
      dplyr::mutate(year = year)
  }

  return(summary_table)
}
