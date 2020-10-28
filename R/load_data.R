
read_survey_data<-function(index, url){
  arc.open(paste0(url, "/", index))%>%
    arc.select()%>%
    tibble::as_tibble()
}

load_data<-function(year = 2020){
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

    return(data_list)
  }else if(year == 2020){
    url_2020<-"https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/BLM_CO_AIM_2020_Plots_Service/FeatureServer"

    api_id<-c(seq(from=0, to=9, by=1), seq(from=11, to=12, by=1), 14, 15)

    plot_names<-c("plots",
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
    data_list[["unknown_plants"]]<-data_list$unknown_plants%>%
      dplyr::mutate(FinalCode==stringr::str_trim(FinalCode))

    data_list[["plant_list"]]<-readr::read_csv("data/plant_list/CO_Survey123_species_20200504.csv")

    return(data_list)
  }

}
