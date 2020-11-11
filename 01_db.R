###############Pulling important variables
### Per capita income is being replaced by median household income, which I prefer as a measure as it is not suseptible to upward bias from a few incomes
### Percent over 65 years old
age<-read_csv("./western_relocation/Data/safegraph_open_census_data/data/cbg_b01.csv")%>%
  dplyr::select(census_block_group,starts_with(('B01001e')))%>%
  rename(total_pop=B01001e1,
         male_65_66=B01001e20,
         male_67_69=B01001e21,
         male_70_74=B01001e22,
         male_75_79=B01001e23,
         male_80_84=B01001e24,
         male_85_up=B01001e25,
         fem_65_66=B01001e44,
         fem_67_69=B01001e45,
         fem_70_74=B01001e46,
         fem_75_79=B01001e47,
         fem_80_84=B01001e48,
         fem_85_up=B01001e49)%>%
  mutate(over_65_percent=(male_65_66+male_67_69+male_70_74+male_75_79+male_80_84+male_85_up+
                              fem_65_66+fem_67_69+fem_70_74+fem_75_79+fem_80_84+fem_85_up)/total_pop)%>%
  dplyr::select(census_block_group,over_65_percent,total_pop)%>%
  na_mean()%>%
  separate(census_block_group,c('FIPS','cbg'),sep=5)%>%
  dplyr::select(!cbg)%>%
  group_by(FIPS)%>%
  summarise(over_65_percent=mean(over_65_percent),
            total_pop=sum(total_pop))


##### Gathering the household income bins to calculate GINI coefficient for Income
inc<-read_csv("./western_relocation/Data/safegraph_open_census_data/data/cbg_b19.csv")%>%
  dplyr::select(census_block_group,starts_with('B19001e'),B19013e1)%>%
  rename(total_hh=B19001e1,
         hh_under_10k=B19001e2,
         hh_10_15k=B19001e3,
         hh_15_20k=B19001e4,
         hh_20_25k=B19001e5,
         hh_25_30k=B19001e6,
         hh_30_35k=B19001e7,
         hh_35_40k=B19001e8,
         hh_40_45k=B19001e9,
         hh_45_50k=B19001e10,
         hh_50_60k=B19001e11,
         hh_60_75k=B19001e12,
         hh_75_100k=B19001e13,
         hh_100_125k=B19001e14,
         hh_125_150k=B19001e15,
         hh_150_200k=B19001e16,
         hh_200k_up=B19001e17,
         med_hh_inc=B19013e1)%>%
  na_mean()%>%
  separate(census_block_group,c('FIPS','cbg'),sep=5)%>%
  dplyr::select(!cbg)%>%
  group_by(FIPS)%>%
  summarise(across(starts_with("hh"), sum),
            med_hh_inc=mean(med_hh_inc),
            total_hh=sum(total_hh))
vacant<-read_csv("./western_relocation/Data/safegraph_open_census_data/data/cbg_b25.csv")%>%
  dplyr::select(census_block_group,starts_with(('B25004e')))%>%
  rename(vacant_houses=B25004e1,
         vacant_for_rent=B25004e2,
         vacant_rented=B25004e3,
         vacant_for_sale=B25004e4,
         vacant_sold=B25004e5,
         vacant_rec=B25004e6,
         vacant_migrant=B25004e7,
         vacant_other=B25004e8)%>%
  dplyr::select(census_block_group,contains('vacant'))%>%
  separate(census_block_group,c('FIPS','cbg'),sep=5)%>%
  dplyr::select(!cbg)%>%
  group_by(FIPS)%>%
  summarise(across(starts_with("vacant"), sum))




static<-age%>%
  left_join(.,vacant)%>%
  left_join(.,inc)

var_16<-data.frame(1:nrow(static),1:nrow(static))%>%
  rename(FIPS=X1.nrow.static.,
         gini_income=X1.nrow.static..1)
for(i in 1:nrow(static) ){
  var_16[i,1]<-static[i,1]%>%as.character()
  var_16[i,2]<-Gini(c(0,10,15,20,25,30,35,40,45,50,60,75,100,125,150,200),n=static[i,13:28])
}

static<-static%>%
  left_join(.,var_16)
save(static,file='./western_relocation/Cache/static.RData')
rm(age,vacant,inc,var_16,static)

