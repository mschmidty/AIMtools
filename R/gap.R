#' returns total gap and gap percent by plot.
#'
#' @param x data of type list created by \code{read_dima())}
dima_gap<-function(x){
  gap_base <- x$gap_detail %>%
    dplyr::left_join(dplyr::select(data$gap_header, RecKey, LineKey)) %>%
    dplyr::left_join(data$join_table)

  gap_percent <- gap_base %>%
    dplyr::group_by(PlotID, PlotKey) %>%
    dplyr::summarize(total_gap = sum(Gap), total_gap_percent = total_gap/7500) %>%
    dplyr::ungroup()

  gap_by_category <- gap_base %>%
    dplyr::mutate(gap_cat = dplyr::case_when(Gap < 20 ~ "gap_not_long_enough",
                                             Gap >= 20 & Gap <= 50 ~ "gap_25_to_50cm",
                                             Gap > 50 & Gap <= 100 ~ "gap_51_to_100cm",
                                             Gap > 100 & Gap <= 200 ~ "gap_101_to_200cm",
                                             Gap > 200 ~ "gap_over_200cm")) %>%
    dplyr::group_by(PlotKey, gap_cat) %>%
    dplyr::summarise(perc_gap_by_cat = sum(Gap)/7500) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = gap_cat, values_from = perc_gap_by_cat) %>%
    dplyr::select(PlotKey, gap_25_to_50cm, gap_51_to_100cm, gap_101_to_200cm, gap_over_200cm)

  dplyr::left_join(gap_percent, gap_by_category, by = "PlotKey")
}


#' returns total gap and gap percent by plot of survey123 data.
#'
#' @param x data of type list created by \code{load_data()}
gap<-function(x){
  x$gap%>%
    tibble::as_tibble()%>%
    dplyr::filter(stringr::str_detect(LineKey, "TRFO|COS01000"))%>%
    dplyr::group_by(PlotKey, PlotID)%>%
    dplyr::summarize(
      pct_gap_25_to_50cm = mean(as.numeric(pctCanCat1)),
      pct_gap_51_to_100cm = mean(as.numeric(pctCanCat2)),
      pct_gap_101_to_200cm = mean(as.numeric(pctCanCat3)),
      pct_gap_over_200cm = mean(as.numeric(pctCanCat3)),
    )%>%
    dplyr::ungroup()%>%
    dplyr::mutate(pct_total_gap = pct_gap_25_to_50cm+pct_gap_51_to_100cm+pct_gap_101_to_200cm+pct_gap_over_200cm)%>%
    dplyr::mutate(strata=stringr::str_detect(PlotID, "^.."))
}
