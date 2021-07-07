fix_plant_list<-function(x){
  x%>%
    dplyr::mutate(Type =  dplyr::case_when(
      stringr::str_detect(symbol, "^PF|^AF|^PG|^AG") ~ "NonWoody",
      stringr::str_detect(symbol, "^SH") ~ "Woody",
      symbol %in% c("S","P", "BR", "CY", "D", "M", "LC", "BY", "EL", "W", "L", "AM") ~ "NonPlant",
      symbol == "None" ~ "None",
      TRUE ~ Type
    ),
    GrowthHabitSub = dplyr::case_when(
      stringr::str_detect(symbol, "^PF|^AF") ~ "Forb",
      stringr::str_detect(symbol, "^PG|^AG") ~ "Graminoid",
      stringr::str_detect(symbol, "^SH") ~ "Shrub",
      symbol %in% c("S","P", "BR", "CY", "D", "M", "LC", "BY", "EL", "W",  "L", "AM") ~ "NonPlant",
      symbol == "None" ~ "None",
      TRUE ~ GrowthHabitSub
    ))
}


# ONLY works wit PLOT LEVEL DATA AT THE MOMENT.
simplify_cover<-function(x, sage=FALSE, growth_type = TRUE){
  if(sage==FALSE){
    if(growth_type == TRUE){
      y<-x%>%
        dplyr::select(PlotID, strata, symbol, cover_perc, GrowthHabitSub, Type)
    }else if (growth_type == FALSE){
      y<-x%>%
        dplyr::select(PlotID, strata, symbol, cover_perc)
    }
  }else if(sage==TRUE){
    y<-x%>%
      dplyr::select(PlotID, strata, symbol, cover_perc, sage_type)
  }else{
    stop("Something went wrong.  Make sure sage is set to TRUE or FALSE")
  }
  y
}

'%!in%' <- function(x,y)!('%in%'(x,y))


strata_full_names<-function(x, year = FALSE){
  x%>%
    dplyr::mutate(Strata = case_when(
      strata =="SS"~"Sagebrush Steppe",
      strata =="PJ"~"Pinyon Juniper Woodland",
      strata =="SD"~"Salt Desert",
      strata =="GR"~"Grassland",
      strata =="MS"~"Mixed Mountain Shrub",
      strata =="AS"~"Aspen",
      strata =="PP"~"Ponderosa Pine",
      strata =="RI"~"Riparian",
      strata =="MC"~"Mixed Conifer",
      strata =="GRSGInt" ~ "Sage Grouse Intensification",
      strata =="GRSG" ~ "Sage Grouse Intensification"
    ))
}

filter_fo<-function(x, field_office="TRFO"){

  field_office<-c("TRFO")
  filter_string<-c("TRFO|COS01000|Tres_Rios_Field_Office")

  data.frame(field_office, filter_string)

  filter_term<-data.frame(field_office, filter_string)%>%
    dplyr::filter(field_office==field_office)

  if("RecKey" %in% names(x)){
    x%>%
      dplyr::filter(stringr::str_detect(RecKey, filter_term$filter_string))
  }else{
    x%>%
      dplyr::filter(stringr::str_detect(PlotKey, filter_term$filter_string))
  }
}
