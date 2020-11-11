###THis script readsin Judes device data, widens it and brings in static data from 01 as well as ERS county 
###code designations.
devices<-read_csv('./western_relocation/Data/cbg_norm.csv')
count<-read_xls('./western_relocation/Data/ruralurbancodes2013.xls')
load('./western_relocation/Cache/static.RData')
device_static<-devices%>%
  dplyr::select(!total_devices)%>%
  pivot_wider(.,id_cols=cbg,names_from=c(year,month),values_from=c(devices,device_frac))%>%
  separate(cbg,c('FIPS','cbg'),sep=5)%>%
  group_by(FIPS)%>%
  dplyr::select(!cbg)%>%
  summarise_all(sum, na.rm=TRUE)%>%
  left_join(.,static)%>%
  left_join(.,count)%>%
  group_by(FIPS)%>%
  mutate(covid=100*(devices_2020_9-devices_2020_2)/devices_2020_2,
         covid_frac=100*(device_frac_2020_10-device_frac_2020_2)/device_frac_2020_2,
         covid_year_frac=100*(device_frac_2020_10-device_frac_2019_10)/device_frac_2019_10,
         covid_year=100*(devices_2020_10-devices_2019_10)/devices_2019_10)%>%
  ungroup()%>%
  mutate(seasonal=vacant_rec/total_hh,
         paper=ifelse(RUCC_2013 %in% c(8,9)&seasonal>.25,1,0),#following th UNH factsheet
         paper_plus=ifelse(RUCC_2013 %in% c(6,7,8,9)&seasonal>.25,1,0),
                      ifelse(RUCC_2013 %in% c(6,7,8,9),'rural','urban'),
         rural=ifelse(RUCC_2013 %in% c(6,7,8,9)&seasonal>.25,'season',
                               ifelse(RUCC_2013 %in% c(6,7,8,9),'rural','urban')))#extending it

save(device_static,file='./western_relocation/Cache/device_static.RData')
rm(devices,count,static)

cor<-device_static%>%
  dplyr::select(where(is.numeric))%>%
  cor(use="complete.obs", method="pearson")



