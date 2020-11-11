#### THis matches the static data with case data for comparison graphs
NYT_cases <- read_csv("western_relocation/Data/us-counties.csv")%>%
  mutate(fips=as.character(fips))
load('./western_relocation/Cache/device_static.RData')
cases<-device_static%>%
  dplyr::select(cbg,RUCC_2013,seasonal, total_pop,Population_2010)%>%
separate(cbg,c('fips','cbg'),sep=5)%>%
  left_join(NYT_cases,.)%>%
  unite(.,cbg,c("fips","cbg"),sep="")%>%
  na.omit()%>%
  mutate(total_pop=total_pop+1,
    per_cap=(cases/Population_2010))
rm(device_static,NYT_cases)
graph<-cases%>%
  mutate(rural=ifelse(RUCC_2013 %in% c(6,7,8,9)&seasonal>.25,'season',
                      ifelse(RUCC_2013 %in% c(6,7,8,9),'rural','urban')))%>%
  group_by(rural,date)%>%
  summarise(cases=mean(cases),
            per_cap=mean(per_cap))%>%
  ungroup()

graph%>%
  ggplot(data=.)+
  geom_line(aes(x=date,y=per_cap,color=rural))
