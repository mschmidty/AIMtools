
read_survey_data<-function(index, url){
  arc.open(paste0(url, "/", index))%>%
    arc.select()%>%
    tibble::as_tibble()
}

load_data<-function(year = 2021){
  if(Sys.getenv("R_ARCH") != "/x64"){

    stop('You are currently using a 64-bit version of R. Please change your R version to 64-bit in "Tools>Global Options" and in the dropdown change your R version to 64-bit. Microsoft Access Does not work with 64-bit R.')
  }
  '%!in%'<- Negate("%in%")
  if("arcgisbinding" %!in% (.packages())){
    stop("Please load the arcgisbinding package and run arc.check_product()")
  }
  if(year == 2019){
    url_2019<-"https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/BLM_CO_AIM_2019_Plots_Service/FeatureServer"

    plot_names<-c("plots", #0
                  "gap", #1
                  "lpi", #2
                  "species_richness", #3
                  "soil_stability", #4
                  "plot_observation", #5
                  "plot_char", #6
                  "aim_photos", #7
                  "known_errors", #8
                  "unknown_plants", #9
                  "routes", #10
                  "spec_rich_detail", #11
                  "soil_pit_horizons", #12
                  "lpi_detail", #13
                  "gap_detail") #14

    api_id<-seq(from=0, to=14, by=1)

    data_list<-lapply(api_id, read_survey_data, url_2019)

    names(data_list)<-plot_names

  }else if(year == 2020){
    url_2020<-"https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/BLM_CO_AIM_2020_Plots_Service/FeatureServer"

    api_id<-c(seq(from=0, to=9, by=1), seq(from=11, to=12, by=1), 14, 15)

    plot_names<-c( "plots",
                   "aim_photos",
                   "gap",
                   "known_errors",
                   "lpi",
                   "plot_char",
                   "plot_observation",
                   "soil_stability",
                   "species_richness",
                   "unknown_plants",
                   "gap_detail",
                   "lpi_detail",
                   "soil_pit_horizons",
                   "spec_rich_detail")

    data_list<-lapply(api_id, read_survey_data, url_2020)

    names(data_list)<-plot_names


  }else if(year==2021){
    url_2021<-"https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/BLM_CO_AIM_2021_Plots_Service/FeatureServer"

    api_id<-c(seq(from=0, to=9, by=1), seq(from=11, to=13, by=1), 15, 16)

    plot_names<-c( "plots",
                   "aim_photos",
                   "gap",
                   "known_errors",
                   "lpi",
                   "plot_char",
                   "plot_observation",
                   "soil_stability",
                   "species_richness",
                   "unknown_plants",
                   "basal_detail",
                   "gap_detail",
                   "lpi_detail",
                   "soil_pit_horizons",
                   "spec_rich_detail")

    data_list<-lapply(api_id, read_survey_data, url_2021)

    names(data_list)<-plot_names
  }else if(year==2022){
    url_2022<-"https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/BLM_CO_AIM_2022_Plots_Service/FeatureServer"
    api_id<-c(seq(from=0, to=9, by=1), seq(from=11, to=13, by=1), 15, 16)

    plot_names<-c(
      "plots", #0
      "aim_photos", #1
      "gap", #2
      "known_errors", #3
      "lpi", #4
      "plot_char", #5
      "plot_observation", #6
      "soil_stability", #7
      "species_richness", #8
      "unknown_plants", #9
      "basal_detail", #11
      "gap_detail", #12
      "lpi_detail", #13
      "soil_pit_horizons",#15
      "spec_rich_detail" #16
    )
    data_list<-lapply(api_id, read_survey_data, url_2022)
    names(data_list)<-plot_names
  }
  data_list[["unknown_plants"]]<-data_list$unknown_plants%>%
   dplyr::mutate(FinalCode==stringr::str_trim(FinalCode))


  data_list[["plant_list"]]<-plant_list

  data_list[["spatial"]]<-data_list$plot_observation%>%
    dplyr::filter(stringr::str_detect(PlotKey, "TRFO|COS01000"))%>%
    dplyr::mutate(easting = ifelse(is.na(Easting), DesignLong, Easting),
                  northing = ifelse(is.na(Northing), DesignLat, Northing))%>%
    dplyr::select(PlotKey, easting, northing)%>%
    dplyr::filter(!is.na(easting))%>%
    sf::st_as_sf(coords = c("easting", "northing"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

  return(data_list)

}
