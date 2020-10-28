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
