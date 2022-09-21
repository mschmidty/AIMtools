#' Analyze species richness data from AIM data hosted on ArcGIS online.
#'
#' @param data a list of data frames created by \code{load_data()}.
#' @param func analysis function. Can be character or vector of characters. Options are "count".
#' @param unknowns \code{TRUE} or \code{FALSE}.  If \code{TRUE} identified unknowns in database will be merged into count data.
# Need to get rid of duplicates per plot
sp_rich<-function(data, func="count", unknowns=TRUE){
  if(unknowns == TRUE){
    unknown_data<-data[["unknown_plants"]]%>%
      dplyr::select(UnknownCode, FinalCode)
  }
  if(func == "count"){
    cleaned_species_rich<-data[["spec_rich_detail"]]%>%
      tibble::as_tibble()%>%
      dplyr::left_join(dplyr::select(data$species_richness, PlotID, globalid), by=c("parentglobalid"="globalid"))%>%
      dplyr::mutate(species_code=SpeciesList,
                    SpeciesList=ifelse(!is.na(UnknownCode), UnknownCode, SpeciesList))

    if(unknowns==TRUE){
      cleaned_species_rich<-cleaned_species_rich%>%
        dplyr::left_join(unknown_data, by="UnknownCode")%>%
        dplyr::mutate(species_code=ifelse(!is.na(FinalCode), FinalCode, SpeciesList))
    }

    cleaned_species_rich%>%
      dplyr::group_by(RecKey, PlotID, species_code)%>%
      dplyr::summarize(count=dplyr::n())%>%
      dplyr::ungroup()%>%
      dplyr::mutate(species_code=sub("^\\s+", "", species_code))
  }
}


#' Analyze species richness from AIM data in a DIMA database.
#' #'
#' @param x a list of data frames created by \code{read_dima()}.
#' @param func analysis function. Can be character or vector of characters. Options are "count".
sp_rich_dima <-function(x, func = "count"){
  if(func=="count"){
    join_1<-dplyr::select(x$lines, LineKey, PlotKey)%>%
      dplyr::mutate(PlotKey=as.character(PlotKey))

    join_2<-dplyr::select(x$plots, PlotKey, PlotID)%>%
      dplyr::mutate(PlotKey=as.character(PlotKey))



    detail<-x$sp_rich_detail%>%
      dplyr::select(-SpeciesCount)%>%
      tidyr::unnest(symbol = stringr::str_split(SpeciesList, ";"))%>%
      dplyr::select(-SpeciesList)%>%
      dplyr::filter(symbol!="")%>%
      dplyr::left_join(dplyr::select(x$sp_rich_header, LineKey, RecKey), by="RecKey" )%>%
      dplyr::left_join(join_1)%>%
      dplyr::left_join(join_2)

  }
  detail
}
