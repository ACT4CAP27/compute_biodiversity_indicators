library(tidyverse)

# load bii coefficients
bii_coef <- readRDS("bii_coef_agmip.RData") %>% dplyr::select(ANYREGION,ITEM_AG,pot_npp,BII)

# load land data
data_in <- read_csv("agmip_test.csv")
data_land <- data_in %>% filter(VAR_ID=="LAND")

# add bii coefs
data_land <- data_land %>% left_join(bii_coef) %>% drop_na()

# EU countries
eu_cty <- c("AUT","BEL","BGR","CYP","CZE","DEU",
            "DNK","ESP","EST","FIN","FRA","GRC","HRV",
            "HUN","IRL","ITA","LTU","LUX","LVA","MLT",
            "NLD","POL","PRT","ROU","SVK","SVN","SWE")

# EU mapping
agmip_reg_eu_map <- tibble(ANYREGION=eu_cty,AGMIPREG="EUE")

data_land_eu <- data_land %>% filter(ANYREGION %in% eu_cty) %>%
  group_by(VAR_ID,VAR_UNIT,ALLSCEN1,ALLSCEN2,ALLSCEN3,ScenYear) %>%
  summarise(BII_val=sum(BII*pot_npp*Value)/sum(pot_npp*Value)) %>%
  mutate(ANYREGION="EUE") %>% relocate(ANYREGION,.after = VAR_UNIT)

# global outputs
data_land_wld <- data_land %>%
  group_by(VAR_ID,VAR_UNIT,ALLSCEN1,ALLSCEN2,ALLSCEN3,ScenYear) %>%
  summarise(BII_val=sum(BII*pot_npp*Value)/sum(pot_npp*Value)) %>%
  mutate(ANYREGION="WLD") %>% relocate(ANYREGION,.after = VAR_UNIT)

# north america
data_land_nam <- data_land %>% filter(ANYREGION %in% c("CAN","USA")) %>%
  group_by(VAR_ID,VAR_UNIT,ALLSCEN1,ALLSCEN2,ALLSCEN3,ScenYear) %>%
  summarise(BII_val=sum(BII*pot_npp*Value)/sum(pot_npp*Value)) %>%
  mutate(ANYREGION="NAM") %>% relocate(ANYREGION,.after = VAR_UNIT)

# other america
data_land_oam <- data_land %>% filter(ANYREGION %in% c("BRA","OSA")) %>%
  group_by(VAR_ID,VAR_UNIT,ALLSCEN1,ALLSCEN2,ALLSCEN3,ScenYear) %>%
  summarise(BII_val=sum(BII*pot_npp*Value)/sum(pot_npp*Value)) %>%
  mutate(ANYREGION="OAM") %>% relocate(ANYREGION,.after = VAR_UNIT)

# africa & middle east
data_land_ame <- data_land %>% filter(ANYREGION %in% c("MEN","SSA")) %>%
  group_by(VAR_ID,VAR_UNIT,ALLSCEN1,ALLSCEN2,ALLSCEN3,ScenYear) %>%
  summarise(BII_val=sum(BII*pot_npp*Value)/sum(pot_npp*Value)) %>%
  mutate(ANYREGION="AME") %>% relocate(ANYREGION,.after = VAR_UNIT)

# southern asia
data_land_sas <- data_land %>% filter(ANYREGION %in% c("CHN","IND","SEA","OAS")) %>%
  group_by(VAR_ID,VAR_UNIT,ALLSCEN1,ALLSCEN2,ALLSCEN3,ScenYear) %>%
  summarise(BII_val=sum(BII*pot_npp*Value)/sum(pot_npp*Value)) %>%
  mutate(ANYREGION="SAS") %>% relocate(ANYREGION,.after = VAR_UNIT)

# original regions
data_all <- data_land %>%
  group_by(VAR_ID,VAR_UNIT,ANYREGION,ALLSCEN1,ALLSCEN2,ALLSCEN3,ScenYear) %>%
  summarise(BII_val=sum(BII*pot_npp*Value)/sum(pot_npp*Value))

# merge data
data_cons <- data_all %>% rbind(data_land_eu) %>% rbind(data_land_wld) %>%
  rbind(data_land_nam) %>% rbind(data_land_oam) %>%
  rbind(data_land_ame) %>% rbind(data_land_sas) %>%
  mutate(VAR_ID="BII",VAR_UNIT="%",ITEM_AG="TOT") %>% relocate(ITEM_AG,.after = ANYREGION) %>%
  rename(Value="BII_val")

if (!any(str_detect(data_in$VAR_ID,regex("[:print:]*bii[:print:]*",ignore_case = T)))){
  data_out <- data_in %>% rbind(data_cons)
  write_csv(data_out,"agmip_test.csv")
}



# ggplot(data_cons,aes(x=ScenYear,y=BII_val,col=ALLSCEN2)) + geom_line() +
#   facet_wrap(~ANYREGION,scales = "free") + theme_light()