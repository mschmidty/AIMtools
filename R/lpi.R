#' produces cover by type by plot
#'
#' @param data type list produced by \code{read_dima()}.
dima_cover<-function(data, by="line"){

  line_cover<-data$lpi_detail%>%
    dplyr::select(RecKey,
                  TopCanopy,
                  subset(names(data$lpi_detail), grepl("^Lower", names(data$lpi_detail))))%>%
    tidyr::pivot_longer(!RecKey,
                        names_to="cover",
                        values_to="symbol",
                        values_drop_na=TRUE)%>%
    dplyr::group_by(RecKey, symbol)%>%
    dplyr::summarize(cover_perc = dplyr::n()/50, n=dplyr::n())%>%
    dplyr::ungroup()%>%
    dplyr::left_join(dplyr::select(data$lpi_header, LineKey, RecKey))%>%
    dplyr::left_join(data$join_table)%>%
    dplyr::left_join(data$plant_list, by=c("symbol"="name"))%>%
    fix_plant_list()

  if(by=="line"){
    line_cover%>%
      dplyr::mutate(strata=stringr::str_extract(PlotID, "^.."))
  }else if(by == "plot"){
    line_cover%>%
      dplyr::group_by(PlotID, PlotKey, Type, GrowthHabitSub, symbol)%>%
      dplyr::summarize(cover_perc=mean(cover_perc, na.rm=T))%>%
      dplyr::ungroup()%>%
      dplyr::mutate(strata=stringr::str_extract(PlotID, "GRSG|^.."))
  }
}


#' produces herbaceous and woody heights by type (woody and herbaceous as well as big sage) by plot.
#'
#' @param data type list produced by \code{read_dima()}.
#' @param raw if \code{TRUE} returns raw dataset not summarized.
dima_heights<-function(data, raw=FALSE){
  height_sub<-data$lpi_detail%>%
    dplyr::select(RecKey, PointNbr, HeightWoody:ChkboxHerbaceous)%>%
    dplyr::filter(PointNbr %% 5 == 0)%>%
    dplyr::left_join(dplyr::select(data$lpi_header, RecKey, LineKey))%>%
    dplyr::left_join(data$join_table)

  if(raw==TRUE){
    return(height_sub)
  }

  height_sub%>%
    dplyr::group_by(PlotKey, PlotID)%>%
    dplyr::na_if(0)%>%
    dplyr::summarize(woody_avg = mean(HeightWoody, na.rm = T),
                     num_woody_heights = 30-sum(is.na(HeightWoody)),
                     herb_avg_height = mean(HeightHerbaceous, na.rm = T),
                     num_herb_heights = 30-sum(is.na(HeightWoody)),
                     big_sage_avg_height = mean(HeightWoody[grepl("^ARTR", SpeciesWoody)], na.rm = TRUE),
                     big_sage_count = sum(grepl("^ARTR", SpeciesWoody)),
                     sage_shape_s_proportion = sum(ShrubShape=="S", na.rm=T)/big_sage_count,
                     sage_shape_m_proportion = sum(ShrubShape=="M", na.rm=T)/big_sage_count,
                     sage_shape_c_proportion = sum(ShrubShape=="C", na.rm=T)/big_sage_count)%>%
    dplyr::ungroup()%>%
    dplyr::mutate(strata=stringr::str_extract(PlotID, "^.."))
}


#' produces cover and soil surface by symbol (species or other) by plot or line
#'
#' @param x data type list produced by \code{load_data()}.
#' @param type can be \code{"general"} for generalized information by line or \code{"detailed"} for symbol level data.
#' @param by if type is \code{"detailed"} this can be either \code{"plot"} or \code{"line"}. Default is \code{"line"}
#' @param raw if \code{TRUE} returns raw LPI data without any cover data calculated.
cover<-function(x, type="general", raw = FALSE, by="line"){
  ## Load Unknown Plants Table for joining.
  unknown_plants<-x$unknown_plants%>%
    tibble::as_tibble()%>%
    dplyr::select(UnknownCode, FinalCode)

  ## Build Join Table for Uknown plants.
  unknown_codes_lpi <- x$lpi_detail%>%
    tibble::as_tibble()%>%
    dplyr::select(PointNbr,
                  parentglobalid,
                  UnknownCodeTop,
                  subset(names(.), grepl("UnknownCodeLower.", names(.))))%>%
    tidyr::pivot_longer(!PointNbr:parentglobalid,
                        names_to="cover_unknown",
                        values_to="symbol_unknown",
                        values_drop_na = TRUE)%>%
    dplyr::mutate(cover_unknown = stringr::str_extract(cover_unknown,"Lower.|Top$"),
                  cover_unknown = ifelse(cover_unknown == "Top", "TopCanopy", cover_unknown))

  ## Load Plant List
  plant_list<-plant_list

  ## Get Parent Keys for grouping.
  parent_keys<-x$lpi%>%
    dplyr::select(PlotID, globalid, PlotKey)

  if(type=="general"){
    x$lpi%>%
      tibble::as_tibble()%>%
      dplyr::select(PlotID, PlotKey, LineKey, avgwoody:pctrockcover)%>%
      dplyr::mutate_at(dplyr::vars(avgwoody:pctrockcover), as.numeric)%>%
      dplyr::group_by(PlotID, PlotKey)%>%
      dplyr::summarize_at(dplyr::vars(avgwoody:pctrockcover), mean, na.rm=T)%>%
      dplyr::ungroup()
  }
  else if(type=="detailed"){

    line <- x$lpi_detail%>%
      tibble::as_tibble()%>%
      dplyr::select(RecKey,
                    PointNbr,
                    parentglobalid,
                    TopCanopy,
                    subset(names(.), grepl("^Lower", names(.))),
                    SoilSurface)%>%
      tidyr::pivot_longer(!RecKey:parentglobalid,
                          names_to="cover",
                          values_to="symbol",
                          values_drop_na = TRUE)%>%
      dplyr::left_join(unknown_codes_lpi, by = c("parentglobalid", "PointNbr", "cover" = "cover_unknown"))%>%
      dplyr::mutate(symbol = ifelse(!is.na(symbol_unknown)&stringr::str_detect(symbol,"XXXX$"), symbol_unknown, symbol))%>%
      dplyr::select(-symbol_unknown)%>%
      dplyr::left_join(unknown_plants, by = c("symbol"="UnknownCode"))%>%
      dplyr::mutate(symbol = ifelse(is.na(FinalCode), symbol, FinalCode))

    if(!raw){
      line<-line%>%
        dplyr::group_by(RecKey, parentglobalid, symbol)%>%
        dplyr::summarize(cover_perc = dplyr::n()/50)%>%
        dplyr::ungroup()
    }

    line<-line%>%
      dplyr::left_join(plant_list%>%
                         dplyr::select(name, Type, GrowthHabitSub),
                       by=c("symbol" = "name"))%>%
      dplyr::mutate(Type =  dplyr::case_when(
        stringr::str_detect(symbol, "^PF|^AF|^PG|^AG") ~ "NonWoody",
        stringr::str_detect(symbol, "^SH") ~ "Woody",
        symbol %in% c("S","P", "BR", "CY", "D", "M", "LC", "BY", "EL", "W") ~ "NonPlant",
        TRUE ~ Type
      ),
      GrowthHabitSub = dplyr::case_when(
        stringr::str_detect(symbol, "^PF|^AF") ~ "Forb",
        stringr::str_detect(symbol, "^PG|^AG") ~ "Graminoid",
        stringr::str_detect(symbol, "^SH") ~ "Shrub",
        symbol %in% c("S","P", "BR", "CY", "D", "M", "LC", "BY", "EL", "W") ~ "NonPlant",
        TRUE ~ GrowthHabitSub
      ))%>%
      dplyr::left_join(parent_keys, by = c("parentglobalid" = "globalid"))%>%
      dplyr::select(-parentglobalid)

    if(raw){
      return(line)
    }

      if(by == "plot"){
        line%>%
          dplyr::group_by(PlotKey, PlotID, Type, GrowthHabitSub, symbol)%>%
          dplyr::summarize(cover_perc = mean(cover_perc))%>%
          dplyr::ungroup()%>%
          dplyr::mutate(strata=stringr::str_extract(PlotID, "^.."))
      }else if ( by == "line"){
        line%>%
          dplyr::mutate(strata=stringr::str_extract(PlotID, "^.."))
      }else{
        stop('by must be either "line" tor return line level data or "plot" to return plot level data')
      }
  }
}



#' produces heights for all plots by line or plot.
#'
#' @param x data type list produced by \code{load_data()}.
#' @param by type data will be returned as. Can return by \code{"line"} (default) or \code{"plot"}

height <- function(x, by="line"){
  ## Build Unknown Plants list
  unknown_plants<-x$unknown_plants%>%
    tibble::as_tibble()%>%
    dplyr::select(UnknownCode, FinalCode)

  plant_list<-readr::read_csv("data/plant_list/CO_Survey123_species_20200504.csv", col_types = readr::cols())%>%
    dplyr::select(list_name:fulllist)

  parent_keys<-x$lpi%>%
    dplyr::select(PlotID, globalid, PlotKey, LineNumber)

  height_subset <- x$lpi_detail%>%
    dplyr::select(parentglobalid,
                  RecKey,
                  PointNbr,
                  ShrubShape:HeightHerbaceous2
    )%>%
    dplyr::filter(!is.na(HeightHerbaceous) | !is.na(HeightWoody))


  ## Height Woody
  woody_heights<-height_subset%>%
    dplyr::select(!subset(names(.), grepl("Herbaceous", names(.))))%>%
    dplyr::filter(SpeciesWoody!="N")%>%
    dplyr::left_join(unknown_plants, by=c("UnknownCodeWoody" = "UnknownCode"))%>%
    dplyr::mutate(SpeciesWoody = ifelse(!is.na(FinalCode), FinalCode, SpeciesWoody))%>%
    dplyr::select(-UnknownCodeWoody, -FinalCode)%>%
    dplyr::group_by(parentglobalid, SpeciesWoody)%>%
    dplyr::summarise(avg_height= mean(HeightWoody, rm.na = T), n=dplyr::n())%>%
    dplyr::ungroup()%>%
    dplyr::mutate(cat = "Woody")%>%
    dplyr::rename(symbol = SpeciesWoody)


  ## Height Herbaceous
  herb_heights <- height_subset%>%
    dplyr::select(parentglobalid,subset(names(.), grepl("Herbaceous", names(.))))%>%
    dplyr::mutate(SpeciesHerbaceous = ifelse(!is.na(UnknownCodeHerbaceous), UnknownCodeHerbaceous, SpeciesHerbaceous))%>%
    dplyr::left_join(unknown_plants, by=c("UnknownCodeHerbaceous" = "UnknownCode"))%>%
    dplyr::mutate(SpeciesHerbaceous = ifelse(!is.na(FinalCode), FinalCode, SpeciesHerbaceous))%>%
    dplyr::select(-UnknownCodeHerbaceous, -FinalCode)%>%
    dplyr::group_by(parentglobalid, SpeciesHerbaceous)%>%
    dplyr::summarise(avg_height = mean(HeightHerbaceous, rm.na = T), n=dplyr::n())%>%
    dplyr::ungroup()%>%
    dplyr::mutate(cat = "Herbaceous")%>%
    dplyr::rename(symbol = SpeciesHerbaceous)

  heights_by_line <- rbind(woody_heights, herb_heights)%>%
    dplyr::left_join(dplyr::select(plant_list, name, Type, GrowthHabitSub), by=c("symbol"="name"))%>%
    dplyr::left_join(parent_keys, by=c("parentglobalid" = "globalid"))%>%
    dplyr::mutate(Type =  dplyr::case_when(
      stringr::str_detect(symbol, "^PF|^AF|^PG|^AG") ~ "NonWoody",
      stringr::str_detect(symbol, "^SH") ~ "Woody",
      symbol=="S" | symbol=="P" ~ "NonPlant",
      TRUE ~ Type
    ),
    GrowthHabitSub = dplyr::case_when(
      stringr::str_detect(symbol, "^PF|^AF") ~ "Forb",
      stringr::str_detect(symbol, "^PG|^AG") ~ "Graminoid",
      stringr::str_detect(symbol, "^SH") ~ "Shrub",
      symbol=="S" | symbol=="P" ~ "NonPlant",
      TRUE ~ GrowthHabitSub
    ))

  if(by == "line"){
    heights_by_line%>%
      dplyr::mutate(strata=stringr::str_extract(PlotID, "^.."))
  }else if(by == "plot"){
    heights_by_line%>%
      dplyr::group_by(PlotID, PlotKey, symbol, cat, Type, GrowthHabitSub)%>%
      dplyr::summarize(avg_height = mean(avg_height, rm.na=T), n=sum(n))%>%
      dplyr::ungroup()%>%
      dplyr::mutate(strata=stringr::str_extract(PlotID, "^.."))
  }

}
