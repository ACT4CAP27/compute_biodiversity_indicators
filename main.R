library(tidyverse)

  # load land data
  data_in <- read_csv("inputs/input_data.csv") 

if (!any(str_detect(data_in$variable,regex("[:print:]*bii[:print:]*",ignore_case = T)))){

  # load bii coefficients
  bii_coef <- readRDS("aux_data/bii_coef_agmip.RData") 

  colnames(bii_coef)[1:2] <- c("region","item")
  colnames(bii_coef) <- tolower(colnames(bii_coef))
  bii_coef <- bii_coef %>% mutate(region=tolower(region),item=tolower(item)) %>% 
  dplyr::select(region,item,pot_npp,bii)


  data_land <- data_in %>% filter(variable=="land")

  # add bii coefs
  data_land <- data_land %>% left_join(bii_coef) %>% drop_na()

  # eu countries
  eu_cty <- c("aut","bel","bgr","cyp","cze","deu",
              "dnk","esp","est","fin","fra","grc","hrv",
              "hun","irl","ita","ltu","lux","lva","mlt",
              "nld","pol","prt","rou","svk","svn","swe")

  # eu mapping
  agmip_reg_eu_map <- tibble(region=eu_cty,agmipreg="eue")

  data_land_eu <- data_land %>% filter(region %in% eu_cty) %>%
    group_by(variable,unit,scenario,year) %>%
    summarise(bii_val=sum(bii*pot_npp*value)/sum(pot_npp*value)) %>%
    mutate(region="eue") %>% relocate(region,.after = unit)

  # global outputs
  data_land_wld <- data_land %>%
    group_by(variable,unit,scenario,year) %>%
    summarise(bii_val=sum(bii*pot_npp*value)/sum(pot_npp*value)) %>%
    mutate(region="wld") %>% relocate(region,.after = unit)

  # north america
  data_land_nam <- data_land %>% filter(region %in% c("can","usa")) %>%
    group_by(variable,unit,scenario,year) %>%
    summarise(bii_val=sum(bii*pot_npp*value)/sum(pot_npp*value)) %>%
    mutate(region="nam") %>% relocate(region,.after = unit)

  # other america
  data_land_oam <- data_land %>% filter(region %in% c("bra","osa")) %>%
    group_by(variable,unit,scenario,year) %>%
    summarise(bii_val=sum(bii*pot_npp*value)/sum(pot_npp*value)) %>%
    mutate(region="oam") %>% relocate(region,.after = unit)

  # africa & middle east
  data_land_ame <- data_land %>% filter(region %in% c("men","ssa")) %>%
    group_by(variable,unit,scenario,year) %>%
    summarise(bii_val=sum(bii*pot_npp*value)/sum(pot_npp*value)) %>%
    mutate(region="ame") %>% relocate(region,.after = unit)

  # southern asia
  data_land_sas <- data_land %>% filter(region %in% c("chn","ind","sea","oas")) %>%
    group_by(variable,unit,scenario,year) %>%
    summarise(bii_val=sum(bii*pot_npp*value)/sum(pot_npp*value)) %>%
    mutate(region="sas") %>% relocate(region,.after = unit)

  # original regions
  data_all <- data_land %>%
    group_by(variable,unit,region,scenario,year) %>%
    summarise(bii_val=sum(bii*pot_npp*value)/sum(pot_npp*value))

  # merge data
  data_cons <- data_all %>% rbind(data_land_eu) %>% rbind(data_land_wld) %>%
    rbind(data_land_nam) %>% rbind(data_land_oam) %>%
    rbind(data_land_ame) %>% rbind(data_land_sas) %>%
    mutate(variable="bii",unit="%",item="tot") %>% relocate(item,.after = region) %>%
    rename(value="bii_val")

  print("bii not existing - adding to file")
  data_out <- data_in %>% rbind(data_cons)
  write_csv(data_out,"/code/outputs/output_w_bi.csv")
} else {
  print("bii already exists in the input file")
}

print(list.files("/code/outputs"))


# ggplot(data_cons,aes(x=year,y=bii_val,col=allscen2)) + geom_line() +
#   facet_wrap(~region,scales = "free") + theme_light()