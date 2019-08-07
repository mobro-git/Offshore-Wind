## ----import data------------------------------------------
results <- c("osw_data/OffshoreWind_Resultsdata_07182019.xlsx")
sheets <- readxl::excel_sheets(results)
data_global <- ReadAllSheets(results)


## ----scenarios------------------------------------------
scenario_cost <- as.data.frame(data_global$`Cost Reductions`)
scenario_cost <- scenario_cost %>% select(-`2011`, -`2010`) %>%
  gather(`2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "Costs")
scenario_cost$CostCurve <- factor(scenario_cost$CostCurve, levels = levels_costred)
scenario_cost$Year <- as.numeric(scenario_cost$Year)

scenario_emissions <- as.data.frame(data_global$`Emissions Caps`)
scenario_emissions <- scenario_emissions %>% select(-`2011`, -`2010`) %>%
  gather(`2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "Emissions")
scenario_emissions$Cap <- factor(scenario_emissions$Cap, levels = levels_emred)
scenario_emissions <- scenario_emissions %>% filter(Cap != "0" & Cap != "10" & Cap != "20")


## ----offshore wind------------------------------------------------------
osw <- as.data.frame(data_global$`Offshore Wind`)
osw <- categorize(osw)
osw$costred <- factor(osw$costred, levels = levels_costred)
osw$emred <- factor(osw$emred, levels = levels_emred)


## ----~osw varcap----------------------------------------------------------
osw_varcap <- osw %>% 
  filter_osw(VAR_Cap, "emred", "costred", "Vintage", "-", "Region")

osw_varcap_long <- osw %>%
  filter(Attribute == "VAR_Cap") %>%
  gather(`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, key = "Year", value = "VAR_Cap") %>%
  group_by(Scenario, Year) %>%
  summarize(VAR_Cap = sum(VAR_Cap)) %>%
  arrange(Scenario) %>%
  ungroup() %>%
  categorize() 
osw_varcap_long$costred <- factor(osw_varcap_long$costred, levels = levels_costred)
osw_varcap_long$emred <- factor(osw_varcap_long$emred, levels = levels_emred)


## ----~osw varncap---------------------------------------------------------
osw_varncap <- osw %>% 
  filter_osw(VAR_Ncap, "emred", "costred", "Vintage", "-", "Region")

osw_varncap_long <- osw %>%
  filter(Attribute =="VAR_Ncap") %>%
  gather(`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, key = "Year", value = "VAR_Ncap") %>%
  group_by(Scenario, Year) %>%
  summarize('VAR_Ncap' = sum(VAR_Ncap)) %>%
  arrange(Scenario) %>%
  ungroup() %>%
  categorize()
osw_varncap_long$costred <- factor(osw_varncap_long$costred, levels = levels_costred)
osw_varncap_long$emred <- factor(osw_varncap_long$emred, levels = levels_emred)


## ----~osw varfout---------------------------------------------------------
osw_varfout <- osw %>% 
  filter_osw(VAR_FOut, "emred", "costred", "Vintage", "-", "Region")

osw_varfout_long <- osw %>%
  filter(Attribute =="VAR_FOut") %>%
  gather(`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, key = "Year", value = "VAR_FOut") %>%
  group_by(Scenario, Year) %>%
  summarize('VAR_FOut' = sum(VAR_FOut)) %>%
  arrange(Scenario) %>%
  ungroup() %>%
  categorize()
osw_varfout_long$costred <- factor(osw_varfout_long$costred, levels = levels_costred)
osw_varfout_long$emred <- factor(osw_varfout_long$emred, levels = levels_emred)

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
elc <- as.data.frame(data_global$`ELC by Process Set`)
colnames(elc)[1] <- "Process"
elc <- elc %>% select(-`2011`, -`2010`) %>% categorize() %>% process()
retirements <- elc

elc_long_all <- elc %>% 
  gather(`2015`,`2020`,`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, key = "Year", value = "VAR_FOut") %>%
  group_by(Scenario, Year, Process) %>%
  select(Process, Scenario, emred, costred, everything()) %>%
  ungroup()

elc_levels <- elc_long_all %>%
  group_by(Process) %>%
  summarize(elcproduction = sum(VAR_FOut)) %>%
  arrange(elcproduction) %>%
  pull(Process)

elc_long <- elc_long_all %>%
  mutate(Process = factor(Process, levels = elc_levels)) %>%
  filter(Process != "Other") %>%
  arrange(Process)
elc_long$emred <- factor(elc_long$emred, levels = levels_emred)
elc_long$costred <- factor(elc_long$costred, levels = levels_costred)

retire <- retirements %>% 
  mutate("2020r" = `2020`-`2015`) %>%
  mutate("2025r" = `2025`-`2020`) %>%
  mutate("2030r" = `2030`-`2025`) %>%
  mutate("2035r" = `2035`-`2030`) %>%
  mutate("2040r" = `2040`-`2035`) %>%
  mutate("2045r" = `2045`-`2040`) %>%
  mutate("2050r" = `2050`-`2045`) %>%
  select(-`2015`, -`2020`, -`2025`, -`2030`, -`2035`, -`2040`, -`2045`, -`2050`) %>%
  arrange(Process)
colnames(retire)[5:11] <- c(seq(2020, 2050, by = 5))

retire_long <- retire %>%
  gather(`2020`,`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, key = "Year", value = "retire") %>% 
  group_by(Scenario, Year, Process) %>%
  select(Process, Scenario, emred, costred, everything()) %>%
  ungroup()

retire_levels <- retire_long %>%
  group_by(Process) %>%
  summarize(retirements = sum(retire)) %>%
  arrange(retirements) %>%
  pull(Process)

retire_long <- retire_long %>%
  mutate(Process = factor(Process, levels = retire_levels))
retire_long$emred <- factor(retire_long$emred, levels = levels_emred)
retire_long$costred <- factor(retire_long$costred, levels = levels_costred)

basecase_production <- elc_long %>%
  filter(emred == "BAU" & costred == "20") %>%
  select("Process", "Year", "VAR_FOut")
colnames(basecase_production)[3] <- c("baseprod")

prod_dif <- left_join(elc_long, basecase_production, by = c("Process", "Year")) 
prod_dif[is.na(prod_dif)] <- 0  
prod_dif <- prod_dif %>% mutate(diff = VAR_FOut - baseprod)

## ----emissions----------------------------------------------------
so2 <- as.data.frame(data_global$`ELC SO2 Emissions`) %>% mutate(Commodity = "SO2") 
nox <- as.data.frame(data_global$`ELC NOX Emissions`) %>% mutate(Commodity = "NOx")
pm2.5 <- as.data.frame(data_global$`ELC PM25 Emissions`) %>% mutate(Commodity = "PM 2.5")
co2 <- as.data.frame(data_global$`ELC CO2 Emissions`) %>% mutate(Commodity = "CO2")
ch4 <- as.data.frame(data_global$`ELC CH4 Emissions`) %>% mutate(Commodity = "CH4")

emissions <- bind_rows(so2, nox, pm2.5, co2, ch4) %>% categorize() %>% select(-`2011`, -`2010`)
emissions$costred <- factor(emissions$costred, levels = levels_costred)
emissions$emred <- factor(emissions$emred, levels = levels_emred)
emissions$Commodity <- factor(emissions$Commodity, levels = levels_emissions)
emissions_long <- emissions %>% 
  gather(`2015`,`2020`,`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, key = "Year", value = "Emissions")
emissions_bau <- emissions_long %>% filter(emred == "BAU" & costred == "20")


## ----2050 emissions totals----------------------------------------
emissions2050 <- as.data.frame(data_global$`ELC Emissions Totals - 2050`)
emissions2050 <- emissions2050 %>% categorize()

emissions2050$costred <- factor(emissions2050$costred, levels = levels_costred)
emissions2050$emred <- factor(emissions2050$emred, levels = levels_emred)


## ----2050 elc total-----------------------------------------------
elctotal2050 <- as.data.frame(data_global$`ELC Total Production 2050`)
elctotal2050 <- categorize(elctotal2050) 
elctotal2050$costred <- factor(elctotal2050$costred, levels = levels_costred)
elctotal2050$emred <- factor(elctotal2050$emred, levels = levels_emred)


## ----elc total----------------------------------------------------
elctotal <- as.data.frame(data_global$`ELC Total Production`)
elctotal <- categorize(elctotal) 
elctotal <- elctotal %>% select(-`2011`, -`2010`) %>%
  gather(`2015`,`2020`,`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, key = "Year", value = "VAR_FOut") %>%
  select(Scenario, emred, costred, Year, everything())
elctotal$costred <- factor(elctotal$costred, levels = levels_costred)
elctotal$emred <- factor(elctotal$emred, levels = levels_emred)


## ----end use------------------------------------------------------
com <- as.data.frame(data_global$`Electricity to Commercial`) %>%  mutate(Sector = "Commercial")
ind <- as.data.frame(data_global$`Electricity to Industrial`) %>% mutate(Sector = "Industrial")
res <- as.data.frame(data_global$`Electricity to Residential`) %>% mutate(Sector = "Residential")
tran <- as.data.frame(data_global$`Electricity to Transportation`) %>% mutate(Sector = "Transportation")

enduse <- bind_rows(com, ind, res, tran) %>% categorize() %>% select(-`2010`, -`2011`) %>%
  gather(`2015`,`2020`,`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, key = "Year", value = "Consumption")
enduse$costred <- factor(enduse$costred, levels = levels_costred)
enduse$emred <- factor(enduse$emred, levels = levels_emred)


## ----LCOE------------------------------------------------------
lcoe <- as.data.frame(data_global$LCOE)



