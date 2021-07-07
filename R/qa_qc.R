#' produces a table with a variety of species richness related qc checks, with information on what the problem is and how to fix it.
#'
#' @param x type list produced by \code{read_dima()} function.
sp_rich_qc<-function(x, output=FALSE){

  rich<-sp_rich(data=x,unknowns=T)%>%
    dplyr::rename(symbol=2, rich_count=3)

  lpi<-cover(x, type="detailed", by="plot")%>%
    dplyr::count(PlotID, symbol, PlotKey)%>%
    dplyr::rename(lpi_count=n)%>%
    dplyr::filter(stringr::str_detect(symbol, ".{3,}"))

  lpi_check<-dplyr::left_join(lpi, rich, by=c("PlotID", "symbol"))%>%
    dplyr::filter(is.na(rich_count))%>%
    dplyr::select(PlotID, symbol, PlotKey)%>%
    dplyr::mutate(error="Symbol appears in LPI form but not in Species Richness",
                  fix=paste0("Add this symbol to Species Richness to plot ", PlotID))

  unknowns<-x$unknown_plants%>%
    dplyr::mutate(FinalCode=ifelse(is.na(FinalCode), UnknownCode, FinalCode))%>%
    dplyr::select(PlotID, FinalCode, Office, PlotKey)%>%
    dplyr::filter(!is.na(FinalCode))

  richness<-sp_rich(x)

  unknown_check<-dplyr::left_join(unknowns, richness, by=c("PlotID", "FinalCode"="species_code") )%>%
    dplyr::mutate(FinalCode=stringr::str_trim(FinalCode))%>%
    dplyr::filter(is.na(count))%>%
    dplyr::select(PlotID, FinalCode, Office, PlotKey)%>%
    dplyr::rename(symbol=FinalCode)%>%
    dplyr::mutate(error=paste0(symbol, ": Found in Unknown Plant list but not in Species  Richness on ", PlotID),
                  fix=NA)

  final_check<-dplyr::bind_rows(unknown_check, lpi_check)


  if(is.character(output)){
    readr::write_csv(final_check, output)
  }else{
    final_check
  }
}
#' Checks to see if species identified as \code{FinalCode} have matches in the plant list used by Survey123. Returns PlotID FinalCode that doesn't have a match, error and fix columns.
#'
#' @param x type list produced by \code{read_dima()} function.
unknown_qc<-function(x, output=FALSE){

  final_check<-x$unknown%>%
    dplyr::select(PlotID, FinalCode, Office, PlotKey)%>%
    dplyr::mutate(FinalCode=str_trim(FinalCode))%>%
    dplyr::left_join(x$plant_list, by=c("FinalCode"="name"))%>%
    dplyr::filter(is.na(label))%>%
    dplyr::select(PlotID, FinalCode, Office, PlotKey)%>%
    dplyr::filter(!is.na(FinalCode))%>%
    dplyr::arrange(Office)%>%
    dplyr::mutate(error="FinalCode in Unknown list does not have match in plant list",
                  fix=paste0(PlotID, ": Add this symbol to Survey123 Species update list or change symbol to synonym that is on the list. "))

  if(is.character(output)){
    readr::write_csv(final_check, output)
  }else{
    final_check
  }
}


#' Runs all qc functions and returns a list of tibbles, each with qc checks. The tables returned are \code(unknown_symbol_check) and \code(sp_rich_check) for now.
#'
#' @param x type list produced by \code{read_dima()} function.
qa_qc<-function(x){
  qc_list<-list()

  qc_list[["unknown_symbol_check"]]<-unknown_qc(x)
  qc_list[["sp_rich_check"]]<-sp_rich_qc(x)

  qc_list
}

