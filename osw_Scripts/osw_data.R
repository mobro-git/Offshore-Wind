## ----import data------------------------------------------
results <- c("osw_data/OffshoreWind_Resultsdata_07292019.xlsx")
sheets <- readxl::excel_sheets(results)
data_global <- ReadAllSheets(results)


## ----scenarios------------------------------------------
scenario_cost <- as.data.frame(data_global$`Cost Reductions`) %>% 
  select(-`2011`, -`2010`) %>%
  gather(`2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "Costs") %>%
  mutate(CostCurve = factor(CostCurve, levels = levels_costred))
scenario_cost$Year <- as.numeric(scenario_cost$Year)

scenario_emissions <- as.data.frame(data_global$`Emissions Caps`) %>% 
  select(-`2011`, -`2010`) %>%
  gather(`2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "Emissions") %>%
  mutate(Cap = factor(Cap, levels = levels_emred)) %>% 
  filter(Cap != "0" & Cap != "10" & Cap != "20")


## ----LCOE------------------------------------------------------
lcoe <- as.data.frame(data_global$LCOE)


## ----offshore wind------------------------------------------------------
osw <- as.data.frame(data_global$`Offshore Wind`) %>%
  categorize() %>%
  mutate(costred = factor(costred, levels = levels_costred)) %>%
  mutate(emred = factor(emred, levels = levels_emred))


## ----~osw varcap----------------------------------------------------------
osw_varcap <- osw %>% 
  filter_osw(VAR_Cap, "Vintage", "-", "Attribute")

osw_varcap_long_reg <- osw_varcap %>%
  gather(`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "VAR_Cap") %>%
  mutate(costred = factor(costred, levels = levels_costred)) %>%
  mutate(emred = factor(emred, levels = levels_emred))

osw_varcap_long <- osw_varcap_long_reg %>%
  group_by(Scenario, Year, emred, costred) %>%
  summarize(VAR_Cap = sum(VAR_Cap)) 

osw_levels <- osw_varcap_long_reg %>%
  group_by(Region) %>%
  summarize(totalcap = sum(VAR_Cap)) %>%
  arrange(totalcap) %>%
  pull(Region)

osw_varcap_long_reg <- osw_varcap_long_reg %>% 
  mutate(Region = factor(Region, levels = osw_levels))
 

## ----~osw varncap---------------------------------------------------------
osw_varncap <- osw %>% 
  filter_osw(VAR_Ncap, "Vintage", "-", "Attribute")

osw_varncap_long_reg <- osw_varncap %>%
  gather(`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "VAR_Ncap") %>%
  mutate(costred = factor(costred, levels = levels_costred)) %>%
  mutate(emred = factor(emred, levels = levels_emred))

osw_varncap_long <- osw_varncap_long_reg %>%
  group_by(Scenario, Year, emred, costred) %>%
  summarize(VAR_Ncap = sum(VAR_Ncap))

osw_varncap_long_reg <- osw_varncap_long_reg %>% 
  mutate(Region = factor(Region, levels = osw_levels))


## ----~osw varfout---------------------------------------------------------
osw_varfout <- osw %>% 
  filter_osw(VAR_FOut, "Vintage", "-", "Attribute")

osw_varfout_long_reg <- osw_varfout %>%
  gather(`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "VAR_FOut") %>%
  mutate(costred = factor(costred, levels = levels_costred)) %>%
  mutate(emred = factor(emred, levels = levels_emred))

osw_varfout_long <- osw_varfout_long_reg %>%
  group_by(Scenario, Year, emred, costred) %>%
  summarize(VAR_FOut = sum(VAR_FOut))

osw_varfout_long_reg <- osw_varfout_long_reg %>% 
  mutate(Region = factor(Region, levels = osw_levels))


## ----~osw 2050 totals-----------------------------------------------------
osw_varcap_2050total <- osw %>% 
  filter(Attribute == "VAR_Cap") %>%
  group_by(Scenario) %>%
  mutate(`2050 Total` = sum(`2050`)) %>% 
  ungroup() %>%
  select(emred, costred, `2050 Total`) %>% 
  distinct() %>%
  spread(key = costred, value = `2050 Total`)
names(osw_varcap_2050total) <- c("", "40", "50", "60", "70", "80")

osw_varfout_2050total <- osw %>% 
  filter(Attribute == "VAR_FOut") %>%
  group_by(Scenario) %>%
  mutate(`2050 Total` = sum(`2050`)) %>% 
  ungroup() %>%
  select(emred, costred, `2050 Total`) %>% 
  distinct() %>%
  spread(key = costred, value = `2050 Total`)
names(osw_varfout_2050total) <- c("", "40", "50", "60", "70", "80")


## ----~osw regional totals-------------------------------------------------
osw_varcap_regiontotals <- osw %>% 
  filter(Attribute == "VAR_Cap") %>%
  group_by(Region) %>%
  summarize(`2050 Total` = mean(`2050`)) %>%
  select(Region, `2050 Total`) %>%
  arrange(`2050 Total`)

osw_varfout_regiontotals <- osw %>% 
  filter(Attribute == "VAR_FOut") %>%
  group_by(Region) %>%
  summarize(`2050 Total` = mean(`2050`)) %>%
  select(Region, `2050 Total`) %>%
  arrange(`2050 Total`)


## ----elc process--------------------------------------------------
ccs <- as.data.frame(data_global$`CO2 Retrofits for Coal`) %>%
  select(-`2011`, -`2010`) %>%
  categorize()

ccs_retro <- ccs %>% filter(Category == "CO2 RETROFITS - COAL")
ccs_base <- ccs %>% filter(Category == "ELC-COAL")

elc <- as.data.frame(data_global$`ELC Produced by Process Set`) %>% 
  select(-`2011`, -`2010`, -Commodity, -Attribute) %>% 
  categorize() %>% 
  process()
retirements <- elc

elc_long_reg <- elc %>% 
  gather(`2015`,`2020`,`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "VAR_FOut") %>%
  select(Process, Scenario, emred, costred, everything())

elc_levels <- elc_long_reg %>%
  group_by(Process) %>%
  summarize(elcproduction = sum(VAR_FOut)) %>%
  arrange(elcproduction) %>%
  pull(Process)

elc_long_reg <- elc_long_reg %>%
  mutate(Process = factor(Process, levels = elc_levels)) %>%
  filter(Process != "Other") %>%
  ungroup()
elc_long_reg$emred <- factor(elc_long_reg$emred, levels = levels_emred)
elc_long_reg$costred <- factor(elc_long_reg$costred, levels = levels_costred)

elc_long <- elc_long_reg %>%
  group_by(Scenario, emred, costred, Process, Year) %>%
  summarize(VAR_FOut = sum(VAR_FOut)) %>%
  ungroup()

retirements <- retirements %>% 
  mutate("2020r" = `2020`-`2015`) %>%
  mutate("2025r" = `2025`-`2020`) %>%
  mutate("2030r" = `2030`-`2025`) %>%
  mutate("2035r" = `2035`-`2030`) %>%
  mutate("2040r" = `2040`-`2035`) %>%
  mutate("2045r" = `2045`-`2040`) %>%
  mutate("2050r" = `2050`-`2045`) %>%
  select(-`2015`, -`2020`, -`2025`, -`2030`, -`2035`, -`2040`, -`2045`, -`2050`) %>%
  arrange(Process)
colnames(retirements)[6:12] <- c(seq(2020, 2050, by = 5))

retire_long_reg <- retirements %>%
  gather(`2020`,`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "retire") %>% 
  select(Process, Scenario, emred, costred, everything())

retire_levels <- retire_long_reg %>%
  group_by(Process) %>%
  summarize(retirements = sum(retire)) %>%
  arrange(retirements) %>%
  pull(Process)

retire_long_reg <- retire_long_reg %>%
  mutate(Process = factor(Process, levels = retire_levels)) %>%
  mutate(emred = factor(emred, levels = levels_emred)) %>%
  mutate(costred = factor(costred, levels = levels_costred)) %>%
  ungroup()

retire_long <- retire_long_reg %>%
  group_by(Scenario, emred, costred, Process, Year) %>%
  summarize(retire = sum(retire)) %>%
  ungroup()

basecase_production_reg <- elc_long_reg %>%
  filter(emred == "BAU" & costred == "20") %>%
  select("Process", "Region", "Year", "VAR_FOut")
colnames(basecase_production_reg)[4] <- c("baseprod")

prod_dif_reg <- left_join(elc_long_reg, basecase_production_reg, 
                          by = c("Process", "Year", "Region")) 
prod_dif_reg[is.na(prod_dif_reg)] <- 0  
prod_dif_reg <- prod_dif_reg %>% mutate(diff = VAR_FOut - baseprod)

basecase_production <- elc_long %>%
  filter(emred == "BAU" & costred == "20") %>%
  select("Process", "Year", "VAR_FOut")
colnames(basecase_production)[3] <- c("baseprod")

prod_dif <- left_join(elc_long, basecase_production, by = c("Process", "Year")) 
prod_dif[is.na(prod_dif)] <- 0  
prod_dif <- prod_dif %>% mutate(diff = VAR_FOut - baseprod)


## ----emissions----------------------------------------------------
emissions <- as.data.frame(data_global$`ELC Emissions Totals`) %>% 
  emission() %>%
  categorize() %>%
  select(-Attribute, -`2010`, -`2011`)

emissions_long_reg <- emissions %>%
  gather(`2015`,`2020`,`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "Emissions") %>%
  mutate(costred = factor(costred, levels = levels_costred)) %>%
  mutate(emred = factor(emred, levels = levels_emred)) %>%
  mutate(Commodity = factor(Commodity, levels = levels_emissions))

emissions_long <- emissions_long_reg %>%
  group_by(Scenario, emred, costred, Commodity, Year) %>%
  summarize(Emissions = sum(Emissions)) %>%
  ungroup() 

emissions_bau <- emissions_long %>% filter(emred == "BAU" & costred == "20")


## ----2050 emissions totals----------------------------------------
emissions2050_reg <- emissions_long_reg %>% filter(Year == "2050") 
emissions2050 <- emissions2050_reg %>%
  group_by(Scenario,emred,costred,Year,Commodity) %>%
  summarize(Emissions = sum(Emissions))


## ----elc total----------------------------------------------------
elctotal <- as.data.frame(data_global$`ELC All Production`) %>%
  select(-Commodity, -Attribute, -`2010`, -`2011`) %>%
  categorize()
elctotal[is.na(elctotal)] <- 0

elctotal_long_reg <- elctotal %>%
  gather(`2015`,`2020`,`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "VAR_FOut") %>%
  group_by(Scenario, costred, emred, Region, Year) %>%
  summarize(VAR_FOut = sum(VAR_FOut)) %>%
  ungroup() %>%
  mutate(costred = factor(costred, levels = levels_costred)) %>%
  mutate(emred = factor(emred, levels = levels_emred))

elctotal_long <- elctotal_long_reg %>%
  group_by(Scenario,emred,costred,Year) %>%
  summarize(VAR_FOut = sum(VAR_FOut))


## ----2050 elc total-----------------------------------------------
elctotal2050_reg <- elctotal_long_reg %>% filter(Year == "2050")
elctotal2050 <- elctotal2050_reg %>%
  group_by(Scenario,emred,costred,Year) %>%
  summarize(VAR_FOut = sum(VAR_FOut))


## ----end use------------------------------------------------------
com <- as.data.frame(data_global$`Electricity to Commercial`) %>%  
  mutate(Sector = "Commercial")
ind <- as.data.frame(data_global$`Electricity to Industrial`) %>% 
  mutate(Sector = "Industrial")
res <- as.data.frame(data_global$`Electricity to Residential`) %>% 
  mutate(Sector = "Residential")
tran <- as.data.frame(data_global$`Electricity to Transportation`) %>% 
  mutate(Sector = "Transportation")

enduse_reg <- bind_rows(com, ind, res, tran) %>% 
  categorize() %>% 
  select(-`2010`, -`2011`, -Commodity, -Attribute, -Process) %>%
  gather(`2015`,`2020`,`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "Consumption") %>%
  mutate(Sector = factor(Sector, levels = levels_sector)) %>%
  mutate(costred = factor(costred, levels = levels_costred)) %>%
  mutate(emred = factor(emred, levels = levels_emred))

enduse <- enduse_reg %>%
  group_by(Scenario,emred,costred,Year,Sector) %>%
  summarize(Consumption = sum(Consumption)) %>%
  ungroup




