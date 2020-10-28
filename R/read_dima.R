read_dima<-function(file_path){

  read_table<-function(connection, name) tibble::as_tibble(RODBC::sqlFetch(connection, name))

  if(Sys.getenv("R_ARCH") != "/i386"){

    stop('You are currently using a 64-bit version of R. Please change your R version to 32-bit in "Tools>Global Options" and in the dropdown change your R version to 32-bit. Microsoft Access Does not work with 64-bit R.')
  }
  con<-RODBC::odbcConnectAccess(file_path)

  data<-list(
    plots = read_table(con, "tblPlots"),
    lines = read_table(con, "tblLines"),
    lpi_detail = read_table(con, "tblLPIDetail")%>%
      dplyr::mutate_all(dplyr::na_if,""),
    lpi_header = read_table(con, "tblLPIHeader"),
    sp_rich_detail = read_table(con, "tblSpecRichDetail"),
    sp_rich_header = read_table(con, "tblSpecRichHeader"),
    gap_detail = read_table(con, "tblGapDetail"),
    gap_header = read_table(con, "tblGapHeader"),
    unknown_plants = read_table(con,"UnknownTracking")
  )

  RODBC::odbcClose(con)

  data[["join_table"]]<-dplyr::select(data$lines, PlotKey, LineKey, LineID)%>%
    dplyr::mutate(PlotKey = as.character(PlotKey))%>%
    dplyr::left_join(dplyr::select(data$plots, PlotKey, PlotID)%>%
                       dplyr::mutate(PlotKey = as.character(PlotKey))
                     , by="PlotKey")

  data[["plant_list"]]<-readr::read_csv("data/plant_list/CO_Survey123_species_20200504.csv", col_types = readr::cols())%>%
    dplyr::select(list_name:fulllist)

  data
}
