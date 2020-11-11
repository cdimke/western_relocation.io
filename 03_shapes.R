## Mapping occurences of increased devices and areas identified as rural/ high seasonal housing
##Load in state data and compile
load('./western_relocation/Cache/device_static.RData')
#outer_bank<-device_static%>%
#  dplyr::filter(FIPS %in% c(37053,37055,37095))%>%
#  dplyr::select(covid)%>%
#  summarise(covid=mean(covid))
#gar_grant<-device_static%>%
#  dplyr::filter(FIPS %in% c(49017,49019))%>%
#  dplyr::select(covid)

map<-counties()%>%
  st_as_sf()%>%
  rename(FIPS=GEOID)%>%
  left_join(.,device_static)
save(map,file='./western_relocation/Cache/map.RData')
rm(device_static)
### National Park Layer
nps<-read_sf('./western_relocation/Data/NPS/NPS.shp')%>%
  dplyr::filter(UNIT_TYPE=="National Park")%>%
  dplyr::filter(STATE!="AK",
                STATE!="HI",
                STATE!="PR")%>%
  dplyr::filter(PARKNAME%in%c("Yellowstone","Grand Teton","Glacier","Arches","Bryce Canyon"))%>%
  st_transform(4326)
nps_labels<-nps%>%
  st_centroid()%>%
  st_coordinates()%>%
  cbind(nps$PARKNAME,.)%>%
  as.data.frame()%>%
  rename(PARKNAME=V1)%>%
  dplyr::select(PARKNAME,X,Y)%>%
  mutate(PARKNAME=paste(PARKNAME,"NP",sep=" "))%>%
  rbind(c("Telluride",-107.8382,37.9122),
        c("Sun Valley",-114.3994,43.6605),
        c("Silverton",-107.3955,37.5305))%>%
  mutate(type=ifelse(str_detect(PARKNAME,"NP"),"park","ski"))


##### Creating  map
pal<-colorNumeric(pal=warmcool(100),domain=c(-40,40))
pal_bin<-colorFactor(palette=c('black','white'), domain=c(0,"Lands"))
####### deignation
yes<-map%>%
  #dplyr::filter(STATEFP%in%c("04","06","08","16","30","32","35","41","49","53","56"))%>%
  dplyr::select(covid,paper_plus)%>%
  mutate(covid=ifelse(covid<39&covid>-39,covid,ifelse(covid>39,39,-39)))%>%
  leaflet()%>%
  addMapPane(name = "polygons", zIndex = 410) %>% 
  addMapPane(name = "maplabels", zIndex = 420) %>% # higher zIndex rendered on top
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  addProviderTiles("CartoDB.PositronOnlyLabels", 
                   options = leafletOptions(pane = "maplabels"),
                   group = "map labels") %>%
  addPolygons(color = "#444444",weight = 1, smoothFactor = 0.5,
              opacity = .001, fillOpacity = .8,fillColor = ~pal(covid),
              group="Resident Device Change",
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
   addPolygons(data=map%>%
                  dplyr::filter(STATEFP%in%c("04","06","08","16","30","32","35","41","49","53","56"))%>%
                  dplyr::filter(FIPS%in%c("49033","08111","08079","49017","08091","08053","49009","04012","49019"))%>%
                  mutate(covid=ifelse(covid<39&covid>-39,covid,ifelse(covid>39,39,-39))),
                color = ~pal(covid),weight = 2, smoothFactor = 0.5,
                opacity = .8, fillOpacity = .001,
               group="Resident Device Change") %>%
  #addPolygons(data=nps_labels[5:8,]%>%
  #              st_as_sf(., coords = c("X", "Y"), crs = 4326, agr = "constant")%>%
  #              st_buffer(.025),
  #            group="nps",
  #            weight = 5,
  #            opacity = 1,
  #            color = "black",
  #            fillOpacity = 0.01
  #              )%>%
  #addPolygons(data=nps,group="nps",
  #            weight = 3,
  #            opacity = 1,
  #            color = "white",
  #            fillOpacity = 0.02) %>%
  #addLabelOnlyMarkers(data = nps_labels,
  #                    ~as.numeric(X),~as.numeric(Y),
  #                    label = ~PARKNAME,
  #                    labelOptions = labelOptions(noHide = TRUE, direction = 'top', textsize = "15px",textOnly = TRUE),
  #                    group="nps") %>%
  addLegend(pal = pal,
            labFormat = labelFormat(suffix = "%"),
            values=c(-40,40),
            group="Resident Device Change")%>%
  addLayersControl(overlayGroups = c("map labels",
                                     "Resident Device Change"))%>%
  hideGroup(c("map labels","nps"))
htmlwidgets::saveWidget(yes,'./us_limited.html')
mapshot(yes,'./test_short.html')
### More change stats
san_juan_hinsdale_mineral<-map%>%
  dplyr::filter(STATEFP=='08' &County_Name %in% c('Mineral County','San Juan County','Hinsdale County'))%>%
  dplyr::select(covid,County_Name)
nan_duke<-map%>%
  dplyr::filter(STATEFP=='25' &County_Name %in% c('Nantucket County','Dukes County'))%>%
  dplyr::select(covid,County_Name)
