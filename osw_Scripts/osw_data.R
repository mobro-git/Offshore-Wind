## ----import data------------------------------------------

# points to the excel file of output from VEDA_BE, reads in the names of the sheets,
# and plugs the spreadsheet into the function I created to turn all sheets into
# separate dataframes in the current environment

results <- c("osw_data/OffshoreWind_Resultsdata_12032019.xlsx")
sheets <- readxl::excel_sheets(results)
data_global <- ReadAllSheets(results)


## ----scenarios------------------------------------------

# reads in the two scenario tables and turns them into data frames for graphing

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

# pulls in the 2018 AEO LCOE table to display in my report/paper

lcoe <- as.data.frame(data_global$LCOE)


## ----offshore wind------------------------------------------------------

# pulls in OSW data  

osw <- as.data.frame(data_global$`Offshore Wind`) %>%
  categorize() %>%
  mutate_all(~replace(.,is.na(.), 0)) %>%
  mutate(costred = factor(costred, levels = levels_costred)) %>%
  mutate(emred = factor(emred, levels = levels_emred))


# sets map information for the regional map with avg OSW buildout

reg.names <- structure(list(lat = c(447343.4,27443.77,-300000,-87448.23,-904151.5,-1060878,
                                    -1348881,-536203.6,-118611,-2118611,-2094320), 
                            long = c(2351032,1906626,1100017,85286,1729611,1150017,
                                     300240.27,-1018340,-2067140,-350821.4,-1167859), 
                            region = c("I", "II", "III", "IV", "V", "VI", 
                                       "VII", "VIII", "IX", "IX", "IX")), 
                       row.names = c(NA, -11L), class = "data.frame")


## ----~osw total capacity (var_cap)----------------------------------------------------------

# filter down osw data to only look at var_cap. make it "long" for easier filtering
# and also because ggplot works much better with long data. create one dataframe that
# has regions associated and one that sums over regions. levels pulled based on the
# total amount of osw in each region, used to order graphs and data from
# least to most amount of osw

osw_varcap <- osw %>% 
  filter_osw(VAR_Cap, "Vintage", "-", "Attribute")

osw_varcap_long_reg <- osw_varcap %>%
  gather(`2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "VAR_Cap") %>%
  mutate(costred = factor(costred, levels = levels_costred)) %>%
  mutate(emred = factor(emred, levels = levels_emred))

osw_varcap_long <- osw_varcap_long_reg %>%
  group_by(Scenario, Year, emred, costred) %>%
  summarize(VAR_Cap = sum(VAR_Cap)) %>%
  ungroup()

osw_levels <- osw_varcap_long_reg %>%
  group_by(Region) %>%
  summarize(totalcap = sum(VAR_Cap)) %>%
  arrange(totalcap) %>%
  pull(Region)

osw_varcap_long_reg <- osw_varcap_long_reg %>% 
  mutate(Region = factor(Region, levels = osw_levels)) %>%
  ungroup()
 

## ----~osw new capacity (var_ncap)---------------------------------------------------------

# filter down osw data to only look at var_ncap. make it "long" for easier filtering
# and also because ggplot works much better with long data. create one dataframe that
# has regions associated and one that sums over regions. 

osw_varncap <- osw %>% 
  filter_osw(VAR_Ncap, "Vintage", "-", "Attribute")

osw_varncap_long_reg <- osw_varncap %>%
  gather(`2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "VAR_Ncap") %>%
  mutate(costred = factor(costred, levels = levels_costred)) %>%
  mutate(emred = factor(emred, levels = levels_emred))

osw_varncap_long <- osw_varncap_long_reg %>%
  group_by(Scenario, Year, emred, costred) %>%
  summarize(VAR_Ncap = sum(VAR_Ncap)) %>%
  ungroup()

osw_varncap_long_reg <- osw_varncap_long_reg %>% 
  mutate(Region = factor(Region, levels = osw_levels)) %>%
  ungroup()


## ----~osw elc production (var_fout)---------------------------------------------------------

# filter down osw data to only look at var_cap. make it "long" for easier filtering
# and also because ggplot works much better with long data. create one dataframe that
# has regions associated and one that sums over regions. 

osw_varfout <- osw %>% 
  filter_osw(VAR_FOut, "Vintage", "-", "Attribute")

osw_varfout_long_reg <- osw_varfout %>%
  gather(`2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "VAR_FOut") %>%
  mutate(costred = factor(costred, levels = levels_costred)) %>%
  mutate(emred = factor(emred, levels = levels_emred))

osw_varfout_long <- osw_varfout_long_reg %>%
  group_by(Scenario, Year, emred, costred) %>%
  summarize(VAR_FOut = sum(VAR_FOut)) %>%
  ungroup()

osw_varfout_long_reg <- osw_varfout_long_reg %>% 
  mutate(Region = factor(Region, levels = osw_levels)) %>%
  ungroup


## ----~osw 2050 totals-----------------------------------------------------

# creates matrix tables for total osw capacity and total osw elc produced 
# snapshot of 2050, end of modeling time horizon, summed over all regions

osw_varcap_2050total <- osw %>% 
  filter(Attribute == "VAR_Cap") %>%
  group_by(Scenario) %>%
  mutate(`2050 Total` = sum(`2050`)) %>% 
  ungroup() %>%
  select(emred, costred, `2050 Total`) %>% 
  distinct() %>%
  spread(key = costred, value = `2050 Total`) %>%
  mutate_if(is.numeric, ~round(.,0))
names(osw_varcap_2050total)[1] <- c("")

osw_varfout_2050total <- osw %>% 
  filter(Attribute == "VAR_FOut") %>%
  group_by(Scenario) %>%
  mutate(`2050 Total` = sum(`2050`)) %>% 
  ungroup() %>%
  select(emred, costred, `2050 Total`) %>% 
  distinct() %>%
  spread(key = costred, value = `2050 Total`) %>%
  mutate_if(is.numeric, ~round(.,0))
names(osw_varfout_2050total)[1] <- c("")


## ----~osw regional totals-------------------------------------------------

# creates values to compare regions osw capacities, mean var_cap for each region
# across all scenarios. same process for var_fout

osw_varcap_regiontotals <- osw %>% 
  filter(Attribute == "VAR_Cap") %>%
  group_by(Region) %>%
  summarize(`2050 Total` = mean(`2050`)) %>%
  select(Region, `2050 Total`) %>%
  arrange(`2050 Total`) %>%
  mutate_if(is.numeric, ~round(.,1))

osw_varfout_regiontotals <- osw %>% 
  filter(Attribute == "VAR_FOut") %>%
  group_by(Region) %>%
  summarize(`2050 Total` = mean(`2050`)) %>%
  select(Region, `2050 Total`) %>%
  arrange(`2050 Total`) %>% 
  mutate_if(is.numeric, ~round(.,1))


## ----grid mix--------------------------------------------------

# pull in elc produced by process set data

elc <- as.data.frame(data_global$`ELC Produced by Process Set`) %>% 
  select(-`2011`, -`2010`, -Commodity, -Attribute) %>%
  process() %>%
  categorize() %>%
  rename("Technology" = "Process")

## ----~ccs--------------------------------------------------

# calculations for coal CCS 
# pull in coal retrofits sheet, has total coal and coal CCS processes - find the % of
# fuel that goes from CCS process to coal and applies that % to coal elc production to 
# get a value to add as coal with CCS to the grid mix

ccs <- as.data.frame(data_global$`CO2 Retrofits for Coal`) %>%
  select(-`2011`, -`2010`) %>%
  categorize()

ccs_retro <- ccs %>% filter(Category == "CO2 RETROFITS - COAL")
ccs_base <- ccs %>% filter(Category == "ELC-COAL")

ccs_retro <- ccs %>% 
  filter(Category == "CO2 RETROFITS - COAL" & Attribute == "VAR_FOut") %>%
  select(-`2015`, -`2020`, -`2025`, -`2030`, -`2035`, -`2040`, -Attribute) %>%
  gather(`2045`, `2050`, key = "Year", value = "VAR_FOut") %>%
  group_by(Scenario, Region, Year, emred, costred) %>%
  summarize("Coal CCS" = sum(VAR_FOut))

ccs_base <- ccs %>% 
  filter(Category == "ELC-COAL" & Attribute == "VAR_FIn") %>%
  select(-`2015`, -`2020`, -`2025`, -`2030`, -`2035`, -`2040`, -Attribute) %>%
  gather(`2045`, `2050`, key = "Year", value = "VAR_FIn") %>%
  group_by(Scenario, Region, Year, emred, costred) %>%
  summarize("Coal" = sum(VAR_FIn)) %>%
  ungroup()
  
ccs_percent <- left_join(ccs_retro, ccs_base, 
                         by = c("Scenario", "Region", "Year", "emred", "costred")) %>%
  mutate(CCSpercent = round((`Coal CCS` / Coal), 2)) %>%
  filter(CCSpercent > 0) %>%
  select(Scenario, Region, Year, CCSpercent, emred, costred)

coal_calculations <- elc %>%
  filter(Technology == "Coal") %>%
  gather(`2015`,`2020`,`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "VAR_FOut") %>%
  left_join(., ccs_percent, by = c("Scenario", "Region", "Year", "emred", "costred")) %>%
  mutate_all(~replace(.,is.na(.), 0)) %>%
  mutate(`Coal CCS` = VAR_FOut * CCSpercent) %>%
  mutate(`Coal` = VAR_FOut - `Coal CCS`) %>%
  select(Scenario, Region, Year, `Coal CCS`, Coal, emred, costred) %>%
  gather(`Coal CCS`, Coal, key = "Technology", value = "VAR_FOut") %>%
  spread(key = Year, value = VAR_FOut) %>%
  select(Scenario, Technology, Region, 
         `2015`,`2020`,`2025`, `2030`, `2035`, `2040`, `2045`, `2050`,
         costred, emred)

# add coal CCS rows to the elc data frame 

elc <- elc %>% filter(Technology != "Coal") %>%
  bind_rows(., coal_calculations)

# duplicate the current elc dataframe to calculate retirements - pull now before 
# all of the changes below are made

retirements <- elc

# ----~elc production----

# make elc data "long" for easier filtering and also because ggplot works 
# much better with long data. create one dataframe that has regions associated 
# and one that sums over regions. levels for factoring pulled to order from
# least to highest contribution to grid mix

elc_long_reg <- elc %>% 
  gather(`2015`,`2020`,`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "VAR_FOut") %>%
  select(Technology, Scenario, emred, costred, everything())

elc_levels <- elc_long_reg %>%
  group_by(Technology) %>%
  summarize(elcproduction = sum(VAR_FOut)) %>%
  arrange(elcproduction) %>%
  pull(Technology)

elc_long_reg <- elc_long_reg %>%
  mutate(Technology = factor(Technology, levels = elc_levels)) %>%
  filter(Technology != "Other") %>%
  ungroup()
elc_long_reg$emred <- factor(elc_long_reg$emred, levels = levels_emred)
elc_long_reg$costred <- factor(elc_long_reg$costred, levels = levels_costred)

elc_long <- elc_long_reg %>%
  group_by(Scenario, emred, costred, Technology, Year) %>%
  summarize(VAR_FOut = sum(VAR_FOut)) %>%
  ungroup()

# ----~retirements------------------------------

# calculates retirements based on difference between one time period's elc prod in 
# each process set to the next. separate dataframe for the base case and all other scenarios
# other scenarios are all in reference to the base case, showing just the capacity
# additions and subtractions over what already happens in the base case

retirements <- retirements %>% 
  mutate("2020r" = `2020`-`2015`) %>%
  mutate("2025r" = `2025`-`2020`) %>%
  mutate("2030r" = `2030`-`2025`) %>%
  mutate("2035r" = `2035`-`2030`) %>%
  mutate("2040r" = `2040`-`2035`) %>%
  mutate("2045r" = `2045`-`2040`) %>%
  mutate("2050r" = `2050`-`2045`) %>%
  select(-`2015`, -`2020`, -`2025`, -`2030`, -`2035`, -`2040`, -`2045`, -`2050`) %>%
  arrange(Technology)
colnames(retirements)[6:12] <- c(seq(2020, 2050, by = 5))

retire_long_reg <- retirements %>%
  gather(`2020`,`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "retire") %>% 
  select(Technology, Scenario, emred, costred, everything())

retire_long_reg <- retire_long_reg %>%
  mutate(Technology = factor(Technology, levels = elc_levels)) %>%
  mutate(emred = factor(emred, levels = levels_emred)) %>%
  mutate(costred = factor(costred, levels = levels_costred)) %>%
  ungroup()

retire_long <- retire_long_reg %>%
  group_by(Scenario, emred, costred, Technology, Year) %>%
  summarize(retire = sum(retire)) %>%
  ungroup()

basecase_production_reg <- elc_long_reg %>%
  filter(emred == "BAU" & costred == "20") %>%
  select("Technology", "Region", "Year", "VAR_FOut")
colnames(basecase_production_reg)[4] <- c("baseprod")

prod_dif_reg <- left_join(elc_long_reg, basecase_production_reg, 
                          by = c("Technology", "Year", "Region")) 
prod_dif_reg[is.na(prod_dif_reg)] <- 0  
prod_dif_reg <- prod_dif_reg %>% mutate(diff = VAR_FOut - baseprod)

basecase_production <- elc_long %>%
  filter(emred == "BAU" & costred == "20") %>%
  select("Technology", "Year", "VAR_FOut")
colnames(basecase_production)[3] <- c("baseprod")

prod_dif <- left_join(elc_long, basecase_production, by = c("Technology", "Year")) 
prod_dif[is.na(prod_dif)] <- 0  
prod_dif <- prod_dif %>% mutate(diff = VAR_FOut - baseprod)

# ----~new capacity------------------------------

# pulls in the new capacity by process set from the excel workbook. this represents actual
# new capacity added, not changes in production, which is what is reflected in "retirements"
# separate dataframe for the base case and all other scenarios
# other scenarios are all in reference to the base case, showing just the capacity
# additions over what already happens in the base case

newcap <- as.data.frame(data_global$`New Capacity by Process Set`) %>%
  categorize() %>%
  process() %>%
  rename("Technology" = "Process")

newcap_long_reg <- newcap %>%
  gather(`2015`,`2020`,`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "Ncap") %>%
  mutate(costred = factor(costred, levels = levels_costred)) %>%
  mutate(emred = factor(emred, levels = levels_emred)) %>%
  mutate(Technology = factor(Technology, levels = elc_levels)) %>%
  ungroup

newcap_total_reg <- newcap_long_reg %>%
  group_by(Scenario, Technology, Region, costred, emred) %>%
  summarize(Ncap = sum(Ncap))

newcap_total_reg_bau <- newcap_total_reg %>% 
  filter(emred == "BAU" & costred == "20") %>%
  ungroup() %>%
  select(Technology, Region, Ncap)
colnames(newcap_total_reg_bau)[3] <- c("BAUncap")

newcap_total_reg_diff <- left_join(newcap_total_reg, newcap_total_reg_bau, by = c("Technology", "Region"))
newcap_total_reg_diff[is.na(newcap_total_reg_diff)] <- 0
newcap_total_reg_diff <- newcap_total_reg_diff %>% mutate(diff = Ncap - BAUncap)
newcap_total_reg_diff$diff <- round(newcap_total_reg_diff$diff,2)

newcap_long <- newcap_long_reg %>%
  group_by(Scenario, Technology, Year, emred, costred, Attribute) %>%
  summarize(Ncap = sum(Ncap)) %>%
  select(Scenario, Technology, Year, Ncap, emred, costred, Attribute)

newcap_total <- newcap_total_reg %>%
  group_by(Scenario, Technology, costred, emred) %>%
  summarize(Ncap = sum(Ncap)) 

newcap_total_bau <- newcap_total %>% 
  filter(emred == "BAU" & costred == "20") %>%
  ungroup() %>%
  select(Technology, Ncap)
colnames(newcap_total_bau)[2] <- c("BAUncap")
  
newcap_total_diff <- left_join(newcap_total, newcap_total_bau, by = "Technology")
newcap_total_diff[is.na(newcap_total_diff)] <- 0
newcap_total_diff <- newcap_total_diff %>% mutate(diff = Ncap - BAUncap)
newcap_total_diff$diff <- round(newcap_total_diff$diff,2)

# ----~rps------------------------------

# calculating the % of renewables in each time period, regionally and nationally
# renewables defined as solar, terrestrial wind, and offshore wind

renewables <- c("Solar", "Terrestrial Wind", "Offshore Wind")

rps_reg <- elc_long_reg %>%
  mutate(RPS = case_when(
    Technology %in% renewables ~ "Renewable",
    TRUE ~ "Other")) %>%
  group_by(Scenario, Region, Year, RPS, costred, emred) %>%
  summarise(Output = sum(VAR_FOut)) %>%
  spread(key = RPS, value = Output) %>%
  mutate(Total = sum(Other, Renewable)) %>%
  mutate(perRenew = round(Renewable/Total*100, 2)) %>% 
  mutate(perOther = round(Other/Total*100, 2)) %>%
  ungroup()

rps <- elc_long %>% 
  mutate(RPS = case_when(
    Technology %in% renewables ~ "Renewable",
    TRUE ~ "Other")) %>%
  group_by(Scenario, Year, RPS, costred, emred) %>%
  summarise(Output = sum(VAR_FOut)) %>%
  spread(key = RPS, value = Output) %>%
  mutate(Total = sum(Other, Renewable)) %>%
  mutate(perRenew = Renewable/Total*100) %>% 
  mutate(perOther = Other/Total*100) %>%
  ungroup()

# ----~market share------------------------------

# calculating the % of total generation coming from OSW

oswmarket <- elc_long %>%
  group_by(Technology, Scenario, emred, costred, Year) %>%
  summarise(Output = sum(VAR_FOut)) %>%
  spread(key = Technology, value = Output) %>%
  mutate_all(~replace(.,is.na(.), 0)) %>% 
  ungroup() %>%
  mutate(Total = 
           `Coal CCS`+`Offshore Wind`+Hydro+`Terrestrial Wind`+Solar+Nuclear+Coal+`Natural Gas`) %>%
  gather(`Coal CCS`, `Offshore Wind`, Hydro, `Terrestrial Wind`, 
          Solar, Nuclear, Coal, `Natural Gas`, key = "Technology", value = "Output") %>%
  select(Scenario, emred, costred, Technology, Year, Output, Total) %>%
  mutate(MarketShare = round(Output/Total*100, 2))

oswmarket_table <- oswmarket %>%
  filter(Year == "2050") %>%
  select(Technology, emred, costred, MarketShare) %>%
  spread(key = costred, value = MarketShare) %>%
  rename("CO2 Cap"="emred")

oswmarket_small <- oswmarket %>% 
  filter(!costred %in% c("20", "30", "80")) %>%
  filter(emred %in% c("BAU", "40", "60"))

totaloutput <- oswmarket %>%
  filter(Year == "2050") %>%
  select(Technology, emred, costred, Output) %>%
  spread(key = costred, value = Output) %>%
  rename("CO2 Cap"="emred") 
  

## ----emissions----------------------------------------------------

# pulls in emissions data for CO2, NOx, SO2, CH4, PM 2.5. both regional and cumulative
# sets of data produced

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
  ungroup() %>%
  mutate(costred = factor(costred, levels = levels_costred)) %>%
  mutate(emred = factor(emred, levels = levels_emred)) %>%
  mutate(Commodity = factor(Commodity, levels = levels_emissions))

emissions_bau_reg <- emissions_long_reg %>% filter(emred == "BAU" & costred == "20")
emissions_bau <- emissions_long %>% filter(emred == "BAU" & costred == "20")

emissions2010_long_reg <- as.data.frame(data_global$`ELC Emissions Totals`) %>% 
  emission() %>%
  categorize() %>%
  select(Scenario, emred, costred, Commodity, Region, `2010`) %>%
  mutate(Year = 2010) %>%
  filter(emred == "BAU" & costred == "20") %>%
  mutate(Commodity = factor(Commodity, levels = levels_emissions))
colnames(emissions2010_long_reg)[6] <- c("Emissions2010")

emissions2010_long <- emissions2010_long_reg %>%
  group_by(emred, costred, Commodity, Year) %>%
  summarize(Emissions2010 = sum(Emissions2010))


## ----~2050 emissions totals----------------------------------------

# pulls just the 2050 totals for each emission, both regional and cumulative
# data sets produced

emissions2050_reg <- emissions_long_reg %>% filter(Year == "2050") 
emissions2050 <- emissions2050_reg %>%
  group_by(Scenario,emred,costred,Year,Commodity) %>%
  summarize(Emissions = sum(Emissions))

## ----~% emissions reduction----------------------------------------

# calculates % and value reduction from baseline for each emission

emissions_percent_reg <- emissions2010_long_reg %>% select(Commodity, Region, Emissions2010) %>%
  left_join(emissions_long_reg, emissions_percent_reg, by = c("Commodity", "Region")) %>%
  select(Scenario, emred, costred, Commodity, Region, Year, Emissions2010, Emissions) %>%
  mutate(percent.red = round(((Emissions2010 - Emissions) / Emissions2010), 2))

emissions_percent <- emissions2010_long %>% ungroup() %>% select(Commodity, Emissions2010) %>%
  left_join(emissions_long, emissions_percent, by = "Commodity") %>%
  select(Scenario, emred, costred, Commodity, Year, Emissions2010, Emissions) %>%
  mutate(percent.red = round(((Emissions2010 - Emissions) / Emissions2010), 2)) %>%
  mutate(value.red = Emissions2010 - Emissions)

## ----~2050 % emissions reductions----------------------------------------

# pulls just the 2050 reductions for each emission, both regional and cumulative
# data sets produced

emissions2050_percent_reg <- emissions_percent_reg %>% filter(Year == "2050") 
emissions2050_percent <- emissions_percent %>% filter(Year == "2050")

## ----~industrial emissions----------------------------------------

# all five emissions from the industrial sector, not regional - includes CHP and
# all other emissions sources for industrial sector

indemissions <- as.data.frame(data_global$`IND Emissions`) %>% 
  select(-Attribute) %>%
  gather(`2010`, `2011`, `2015`,`2020`,`2025`, `2030`, `2035`, `2040`, `2045`, `2050`,
         key = "Year", value = "Emissions") %>%
  categorize() %>%
  mutate(Commodity = case_when(
    str_detect(Commodity, "CO2") ~ "CO[2]",
    str_detect(Commodity, "SO2") ~ "SO[2]",
    str_detect(Commodity, "NOX") ~ "NO[X]",
    str_detect(Commodity, "CH4") ~ "CH[4]",
    TRUE ~ "PM[2.5]")) %>%
  mutate(costred = factor(costred, levels = levels_costred)) %>%
  mutate(emred = factor(emred, levels = levels_emred))

# baseline industrial emissions to calculate changes over time and scenarios

ind2010 <- indemissions %>% filter(Year == "2010")

# remove 2010 and 2011 from dataset for better graphing

indemissions <- indemissions %>% filter(Year != "2010" & Year != "2011")

## ----~tradeoff IND and grid emissions----

# calculates % and value reduction from baseline for each emission

indreductions <- left_join(indemissions, ind2010, 
                         by = c("Scenario", "Commodity", "costred", "emred")) %>%
  rename("Year" = "Year.x") %>%
  rename("IndEmissions" = "Emissions.x") %>%
  rename("IndEmissions2010" = "Emissions.y") %>%
  select(-Year.y) %>%
  mutate(indred = IndEmissions2010 - IndEmissions) %>%
  mutate(indredperc = indred/IndEmissions2010)

## ----~industrial vs electric sector

# compares emissions changes from grid with emissions changes from industrial 
# to calculate net change in emissions with tradeoff/switching between grid elc
# and industrial CHP in the tighter CO2 scenarios

indtradeoff <- left_join(emissions_percent, indreductions, 
                         by = c('Scenario', 'Commodity', 'Year', 'costred', 'emred')) %>%
  filter(costred != "40") %>%
  mutate(netdecrease = value.red + indred) %>%
  mutate(tradeoff = indred / value.red * 100)

co2tradeoff <- indtradeoff %>% filter(Commodity == "CO[2]" & Year == "2050") %>%
  select(Scenario, emred, costred, value.red, indred, netdecrease, tradeoff)

so2tradeoff <- indtradeoff %>% filter(Commodity == "SO[2]" & Year == "2050") %>%
  select(Scenario, emred, costred, value.red, indred, netdecrease, tradeoff)

ch4tradeoff <- indtradeoff %>% filter(Commodity == "CH[4]" & Year == "2050") %>%
  select(Scenario, emred, costred, value.red, indred, netdecrease, tradeoff)

noxtradeoff <- indtradeoff %>% filter(Commodity == "NO[X]" & Year == "2050") %>%
  select(Scenario, emred, costred, value.red, indred, netdecrease, tradeoff)

pm2.5tradeoff <- indtradeoff %>% filter(Commodity == "PM[2.5]" & Year == "2050") %>%
  select(Scenario, emred, costred, value.red, indred, netdecrease, tradeoff)

## ----elc total----------------------------------------------------

# total electricity produced in each scenario, agnostic of process. both regional
# and cumulative data sets produced. Need to remove "trade" processes to 
# prevent overcounting

elctotal <- as.data.frame(data_global$`ELC All Production`) %>%
  select(-Commodity, -Attribute, -`2010`, -`2011`) %>%
  categorize() %>%
  filter(!str_detect(Process, "Trd"))
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


## ----~2050 elc total-----------------------------------------------

# pulls just the 2050 total elc produced, regional and cumulative

elctotal2050_reg <- elctotal_long_reg %>% filter(Year == "2050")
elctotal2050 <- elctotal2050_reg %>%
  group_by(Scenario,emred,costred,Year) %>%
  summarize(VAR_FOut = sum(VAR_FOut))


## ----end use------------------------------------------------------

# creates a dataframe with all of the end use elc consumption. data pulled in from
# VBE by sector (commercial, industrial, residential, transportation) and combined
# into one larger enduse dataframe. 
# separate dataframes made for regional and cumulative

com <- as.data.frame(data_global$`Electricity to Commercial`) %>%  
  mutate(Sector = "Commercial") %>%
  select(-`2010`, -`2011`, -Commodity, -Attribute, -Process)
res <- as.data.frame(data_global$`Electricity to Residential`) %>% 
  mutate(Sector = "Residential") %>%
  select(-`2010`, -`2011`, -Commodity, -Attribute, -Process)
tran <- as.data.frame(data_global$`Electricity to Transportation`) %>% 
  mutate(Sector = "Transportation") %>%
  select(-`2010`, -`2011`, -Commodity, -Attribute, -Process)

ind_all <- as.data.frame(data_global$`Electricity to Industrial`) %>%
  mutate_all(~replace(.,is.na(.), 0)) %>%
  mutate_if(is.numeric, ~round(.,2)) %>%
  select(-`2010`, -`2011`, -Commodity, -Attribute) %>%
  gather(`2015`,`2020`,`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "Consumption") %>%
  group_by(Scenario, Region, Year) %>%
  summarize(Consumption = sum(Consumption)) %>%
  spread(key = Year, value = Consumption) %>% 
  mutate(Sector = "All Industrial")

ind_sep <- as.data.frame(data_global$`Electricity to Industrial`) %>%
  select(-`2010`, -`2011`, -Attribute) %>%
  rename("Sector" = "Commodity") %>%
  mutate(Sector = case_when(
    str_detect(Sector, "CHP") ~ "CHP Industrial",
    TRUE ~ "Grid Industrial")) %>%
  select(-Sector, everything())

enduse_reg <- bind_rows(com, res, tran, ind_all, ind_sep) %>% 
  categorize() %>% 
  gather(`2015`,`2020`,`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "Consumption") %>%
  mutate(Sector = factor(Sector, levels = levels_sector)) %>%
  mutate(costred = factor(costred, levels = levels_costred)) %>%
  mutate(emred = factor(emred, levels = levels_emred)) %>%
  filter(costred != 40)

enduse <- enduse_reg %>%
  group_by(Scenario,emred,costred,Year,Sector) %>%
  summarize(Consumption = sum(Consumption)) %>%
  ungroup() 


## ----correlation------------------------------------------------------

# pulls osw total capacity, emissions amounts, and total elc produced into one
# dataframe and formats the data (primarily into numeric values instead of categorical)
# so that it can be plugged into a correlation function

## ----~only osw scenarios (28)----

oswcor <- osw_varcap_2050total %>%
  rename("emred" = "") %>%
  gather("50", "60", "70", "80", key = "costred", value = "cap2050") %>% 
  left_join(., emissions2050, by = c("emred", "costred")) %>%
  spread(key = Commodity, value = Emissions) %>% 
  left_join(., elctotal2050, by = c("emred", "costred", "Scenario", "Year")) %>%
  rename("Total Elc" = "VAR_FOut") %>%
  left_join(., rps, by = c("Scenario", "emred", "costred", "Year")) %>%
  select(emred, costred, `cap2050`, `CO[2]`, `SO[2]`, `CH[4]`, `PM[2.5]`, `NO[X]`, `Total Elc`, perRenew)
oswcor[] <- lapply(oswcor, as.character)
oswcor <- oswcor %>% mutate(emred = replace(emred, emred == "BAU", 20)) %>%
  mutate(emred = as.numeric(emred)) %>%
  mutate(costred = as.numeric(costred)) %>%
  mutate(`CO[2]` = as.numeric(`CO[2]`)) %>%
  mutate(`SO[2]` = as.numeric(`SO[2]`)) %>%
  mutate(`NO[X]` = as.numeric(`NO[X]`)) %>%
  mutate(`PM[2.5]` = as.numeric(`PM[2.5]`)) %>%
  mutate(`CH[4]` = as.numeric(`CH[4]`)) %>%
  mutate(`cap2050` = as.numeric(`cap2050`)) %>%
  mutate(`Total Elc` = as.numeric(`Total Elc`)) %>%
  mutate(perRenew = as.numeric(perRenew)) %>%
  mutate_all(~replace(.,is.na(.), 0)) %>% 
  mutate_if(is.numeric, ~round(.,2)) %>%
  select(`cap2050`, everything())

## ----~all scenarios (42)----

oswscen <- osw_varcap_2050total %>%
  rename("emred" = "") %>%
  gather("50", "60", "70", "80", key = "costred", value = "cap2050")

allcor <- emissions2050 %>%
  spread(key = Commodity, value = Emissions) %>% 
  left_join(., oswscen, by = c("emred", "costred")) %>%
  left_join(., elctotal2050, 
                    by = c("emred", "costred", "Scenario", "Year")) %>%
  rename("Total Elc" = "VAR_FOut") %>%
  left_join(., rps, by = c("Scenario", "emred", "costred", "Year")) %>%
  ungroup() %>%
  select(emred, costred, `cap2050`, `CO[2]`, `SO[2]`, `CH[4]`, `PM[2.5]`, `NO[X]`, `Total Elc`, perRenew)
allcor[] <- lapply(allcor, as.character)
allcor <- allcor %>% ungroup() %>% mutate(emred = replace(emred, emred == "BAU", 20)) %>%
  mutate(emred = as.numeric(emred)) %>%
  mutate(costred = as.numeric(costred)) %>%
  mutate(`CO[2]` = as.numeric(`CO[2]`)) %>%
  mutate(`SO[2]` = as.numeric(`SO[2]`)) %>%
  mutate(`NO[X]` = as.numeric(`NO[X]`)) %>%
  mutate(`PM[2.5]` = as.numeric(`PM[2.5]`)) %>%
  mutate(`CH[4]` = as.numeric(`CH[4]`)) %>%
  mutate(`cap2050` = as.numeric(`cap2050`)) %>%
  mutate(`Total Elc` = as.numeric(`Total Elc`)) %>%
  mutate(perRenew = as.numeric(perRenew)) %>%
  mutate_all(~replace(.,is.na(.), 0)) %>%
  mutate_if(is.numeric, ~round(.,2))


