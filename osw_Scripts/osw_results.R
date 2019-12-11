## Load Setup and Data Scripts ----

# allows me to quickly run my two previous scripts when I make changes in them or
# if I'm just going to work with results and need to pull in the data

source("osw_Scripts/osw_setup.R")
source("osw_Scripts/osw_data.R")

## Scenarios ----

## ~ Cost ----

# creates the graphs showing the different cost scenarios used

costscen_plot <- ggplot(scenario_cost) +
  geom_label(data = . %>% filter(Year == last(Year)),
             aes(label = paste(CostCurve,"%"), x = Year, y = Costs), 
             size = 3, alpha = 0.5, nudge_y = 4) +
  #scale_y_continuous(limits = c(0, 100), expand = c(.01, 0)) +
  labs(x = "Year", y = "Cost Reduction", 
       title = "OSW Cost Curves", 
       subtitle = "Percentage decrease from 2018 offshore wind CAPEX costs") +
  yt +
  x_cont +
  nolegend
  
costscen_col <- costscen_plot +
  geom_line(aes(x = Year, y = Costs, color = CostCurve, group = CostCurve), size = 1) +
  cost_color

costscen_gray <- costscen_plot +
  geom_line(aes(x = Year, y = Costs, color = CostCurve, group = CostCurve), size = 1) +
  gray_color

costscen_bw <- costscen_plot +
  geom_line(aes(x = Year, y = Costs, linetype = CostCurve, group = CostCurve), size = 1) 


## ~ Emissions ----

# creates the graphs showing the different co2 cap scenarios used

emissionsscen_plot <- ggplot(scenario_emissions) +
  geom_label(data = . %>% filter(Year == last(Year)) %>% filter(Cap != "BAU"),
             aes(label = paste(Cap,"%"), x = Year, y = Emissions), 
             size = 3, alpha = 0.5, nudge_y = 80) +
  geom_label(data = . %>% filter(Year == last(Year)) %>% filter(Cap == "BAU"),
             aes(label = Cap, x = Year, y = Emissions), 
             size = 3, alpha = 0.5, nudge_y = 80) +
  ylim(0, 2200) +
  labs(x = "Year", y = "CO2 Emissions (Mt)", 
       title = "CO2 Emissions Caps", 
       subtitle = "Scenarios as overall % reduction of 2010 electric sector CO2 emissions") +
  yt +
  x_disc +
  nolegend
  
emissionsscen_col <- emissionsscen_plot + 
  geom_line(aes(x = Year, y = Emissions, color = Cap, group = Cap), size = 1) + 
  em_color 

emissionsscen_bw <- emissionsscen_plot +
  geom_line(aes(x = Year, y = Emissions, linetype = Cap, group = Cap), size = 1)

## LCOE ----

# kable table to show 2018 AEO LCOE for comparison between osw and other technologies

lcoe_table <- lcoe %>% 
  kable(booktabs = T, caption = "Estimated LCOE capacity-weighted average for new generation 
        resources entering service in 2023 (2018 \\$/MWh)", linesep = "") %>% 
  kable_styling(latex_options = c("striped", "hold_position", "scale_down")) %>%
  column_spec(1, width = "2.95 cm") %>%
  column_spec(2:8, width = "1.2 cm") %>%
  column_spec(9, width = "1.5 cm") %>%
  group_rows("Dispatchable technologies", 1, 5) %>%
  group_rows("Non-dispatchable technologies", 6, 9) %>%
  footnote(general = "U.S. EIA Annual Energy Outlook 2019")

## Offshore Wind ----

## ~ Total Capacity ----

# total osw by scenario, co2 caps are multicolor lines and cost scenarios are facets

cap_plot <- ggplot(osw_varcap_long) +
  labs(x = "Year", y = "Total Installed Capacity (GW)",
       title = "Offshore Wind Total Capacity",
       color = "CO2 Cap (%)",
       linetype = "CO2 Cap (%)") + 
  facet_grid(~costred, labeller=labeller(costred = costlabels)) +
  yt +
  bottom1 + 
  bottom2 +
  x_disc
  
cap_col_side <- cap_plot + 
  geom_line(aes(x=Year, y=VAR_Cap, color = emred, group = Scenario), size = 1) + 
  em_color
   
cap_bw_side <- cap_plot +
  geom_line(aes(x=Year, y=VAR_Cap, linetype = emred, group = Scenario), size = 1) 

cap_gray_side <- cap_plot + 
  geom_line(aes(x=Year, y=VAR_Cap, color = emred, group = Scenario), size = 1) + 
  gray_color

# heatmaps of 2050 total osw capacity, cumulative

cap_heatmap <- osw_varcap_long %>% filter(Year == "2050") %>%
  ggplot(aes(x = costred, y = emred)) +
  geom_tile(aes(fill = VAR_Cap), colour = "gray") +
  labs(x = "Cost Reduction (%)", y = "CO2 Cap (%)",
       title = "2050 Offshore Wind Capacity",
       fill = "Capacity (GW)") +
  st 
  
cap_col_heat <- cap_heatmap + 
  geom_text(aes(label = round(VAR_Cap, 0)), color = "black", size = 3) +
  fill_cont

cap_bw_heat <- cap_heatmap + 
  geom_text(aes(label = round(VAR_Cap, 0)), color = "white", size = 3) +
  gray_fill_cont 

## ~ New Capacity ----

# new osw by scenario, co2 caps are multicolor lines and cost scenarios are facets

newcap_plot <- ggplot(osw_varncap_long) +
  labs(x = "Year", y = "New Installed Capacity (GW)",
       title = "Offshore Wind New Capacity",
       color = "CO2 Cap (%)",
       linetype = "CO2 Cap (%)") + 
  facet_grid(costred~., scales = "free_y", labeller=labeller(costred = costlabels)) +
  yt +
  bottom1 +
  bottom2 +
  x_disc

newcap_col_top <- newcap_plot +
  geom_line(aes(x=Year, y=VAR_Ncap, color = emred, group = Scenario), size = 1) + 
  em_color

newcap_bw_top <- newcap_plot +
  geom_line(aes(x=Year, y=VAR_Ncap, linetype = emred, group = Scenario), size = 1)

newcap_gray_top <- newcap_plot +
  geom_line(aes(x=Year, y=VAR_Ncap, color = emred, group = Scenario), size = 1) + 
  gray_color

## ~ Production ----

# osw elc production by scenario, co2 caps are multicolor lines and cost scenarios are facets

output_plot <- ggplot(osw_varfout_long) + 
  labs(x = "Year", y = "Offshore Wind Output (PJ)",
       title = "Offshore Wind Output",
       color = "CO2 Cap (%)",
       linetype = "CO2 Cap (%)") +
  facet_grid(~costred, labeller=labeller(costred = costlabels)) +
  yt +
  bottom1 +
  x_disc

output_col_side <- output_plot+
  geom_line(aes(x=Year, y=VAR_FOut, color = emred, group = Scenario), size = 1) +
  em_color

output_bw_side <- output_plot + 
  geom_line(aes(x=Year, y=VAR_FOut, linetype = emred, group = Scenario), size = 1)

output_gray_side <- output_plot+
  geom_line(aes(x=Year, y=VAR_FOut, color = emred, group = Scenario), size = 1) +
  gray_color

## ~ Regions ----

# REGIONAL total osw by scenario, co2 caps are multicolor lines and cost scenarios are facets

cap.reg.col.list <- list(data = osw_varcap_long_reg, yvar = "VAR_Cap", colvar = "emred",
                          coltheme = em_color, title = "Offshore Wind Total Capacity", 
                          ylab = "Capacity (GW)")

cap_reg1_col <- do.call(lineplot.region, c(cap.reg.col.list,list(region = "R1")))
cap_reg2_col <- do.call(lineplot.region, c(cap.reg.col.list,list(region = "R2")))
cap_reg3_col <- do.call(lineplot.region, c(cap.reg.col.list,list(region = "R3")))
cap_reg5_col <- do.call(lineplot.region, c(cap.reg.col.list,list(region = "R5")))
cap_reg6_col <- do.call(lineplot.region, c(cap.reg.col.list,list(region = "R6")))
cap_reg7_col <- do.call(lineplot.region, c(cap.reg.col.list,list(region = "R7")))
cap_reg9_col <- do.call(lineplot.region, c(cap.reg.col.list,list(region = "R9")))

# facetgrid with all regions shown in one plot

cap_allreg_col <- do.call(lineplot.region, cap.reg.col.list)
cap_allreg_col_free <- do.call(lineplot.region, c(cap.reg.col.list,list(scale="free_y"))) +
  theme(axis.text.y = element_text(face = "bold")) +
  labs(caption = "*Capacity scales are not fixed")

cap.reg.bw.list <- list(data = osw_varcap_long_reg, yvar = "VAR_Cap", typevar = "emred",
                        title = "Offshore Wind Total Capacity", ylab = "Capacity (GW)")

cap_reg1_bw <- do.call(lineplot.region, c(cap.reg.bw.list,list(region = "R1")))
cap_reg2_bw <- do.call(lineplot.region, c(cap.reg.bw.list,list(region = "R2")))
cap_reg3_bw <- do.call(lineplot.region, c(cap.reg.bw.list,list(region = "R3")))
cap_reg5_bw <- do.call(lineplot.region, c(cap.reg.bw.list,list(region = "R5")))
cap_reg6_bw <- do.call(lineplot.region, c(cap.reg.bw.list,list(region = "R6")))
cap_reg7_bw <- do.call(lineplot.region, c(cap.reg.bw.list,list(region = "R7")))
cap_reg9_bw <- do.call(lineplot.region, c(cap.reg.bw.list,list(region = "R9")))

# facetgrid with all regions shown in one plot

cap_allreg_bw <- do.call(lineplot.region, cap.reg.bw.list)
cap_allreg_bw_free <- do.call(lineplot.region, c(cap.reg.bw.list,list(scale="free_y"))) +
  theme(axis.text.y = element_text(face = "bold")) +
  labs(caption = "*Capacity scales are not fixed")

# REGIONAL new osw by scenario, co2 caps are multicolor lines and cost scenarios are facets

ncap.reg.col.list <- list(data = osw_varncap_long_reg, yvar = "VAR_Ncap", colvar = "emred",
            coltheme = em_color, title = "Offshore Wind New Capacity", 
            ylab = "New Capacity (GW)")

newcap_reg1_col <- do.call(lineplot.region, c(ncap.reg.col.list,list(region = "R1")))
newcap_reg2_col <- do.call(lineplot.region, c(ncap.reg.col.list,list(region = "R2")))
newcap_reg3_col <- do.call(lineplot.region, c(ncap.reg.col.list,list(region = "R3")))
newcap_reg4_col <- do.call(lineplot.region, c(ncap.reg.col.list,list(region = "R4")))
newcap_reg5_col <- do.call(lineplot.region, c(ncap.reg.col.list,list(region = "R5")))
newcap_reg6_col <- do.call(lineplot.region, c(ncap.reg.col.list,list(region = "R6")))
newcap_reg7_col <- do.call(lineplot.region, c(ncap.reg.col.list,list(region = "R7")))
newcap_reg9_col <- do.call(lineplot.region, c(ncap.reg.col.list,list(region = "R9")))

# facetgrid with all regions shown in one plot

newcap_allreg_col <- do.call(lineplot.region, ncap.reg.col.list)
newcap_allreg_col_free <- do.call(lineplot.region, c(ncap.reg.col.list,list(scale="free_y"))) +
  theme(axis.text.y = element_text(face = "bold")) +
  labs(caption = "*Capacity scales are not fixed")

ncap.reg.bw.list <- list(data = osw_varncap_long_reg, yvar = "VAR_Ncap", typevar = "emred",
                          title = "Offshore Wind New Capacity", ylab = "New Capacity (GW)")

newcap_reg1_bw <- do.call(lineplot.region, c(ncap.reg.bw.list,list(region = "R1")))
newcap_reg2_bw <- do.call(lineplot.region, c(ncap.reg.bw.list,list(region = "R2")))
newcap_reg3_bw <- do.call(lineplot.region, c(ncap.reg.bw.list,list(region = "R3")))
newcap_reg4_bw <- do.call(lineplot.region, c(ncap.reg.bw.list,list(region = "R4")))
newcap_reg5_bw <- do.call(lineplot.region, c(ncap.reg.bw.list,list(region = "R5")))
newcap_reg6_bw <- do.call(lineplot.region, c(ncap.reg.bw.list,list(region = "R6")))
newcap_reg7_bw <- do.call(lineplot.region, c(ncap.reg.bw.list,list(region = "R7")))
newcap_reg9_bw <- do.call(lineplot.region, c(ncap.reg.bw.list,list(region = "R9")))

# facetgrid with all regions shown in one plot

newcap_allreg_bw <- do.call(lineplot.region, ncap.reg.bw.list)
newcap_allreg_bw_free <- do.call(lineplot.region, c(ncap.reg.bw.list,list(scale="free_y"))) +
  theme(axis.text.y = element_text(face = "bold")) +
  labs(caption = "*Capacity scales are not fixed")


## ~ Maps ----

# sets variables for which states are in which census region

r1 <- c("ME","NH","VT","MA","RI","CT")
r2 <- c("NY","PA","NJ")
r3 <- c("WI", "MI", "OH", "IN", "IL")
r4 <- c("ND","SD","NE", "KS", "IA","MN","MO")
r5 <- c("WV","MD","DE","VA","NC","SC",
        "GA","FL","DC")
r6 <- c("KY","TN","AL","MS")
r7 <- c("LA","AR","OK","TX")
r8 <- c("MT","WY","ID","CO","NV","UT","AZ","NM")
r9 <-c("WA","OR","CA","HI","AK")

# pulls in the states map from the us_map package and adds columns for census region
# and avg osw capacity in each region (needs to be manually updated)

state_map <- us_map(regions = "states") %>%
  mutate(censusRegion = case_when(
    abbr %in% r1 ~ "R1",
    abbr %in% r2 ~ "R2",
    abbr %in% r3 ~ "R3",
    abbr %in% r4 ~ "R4",
    abbr %in% r5 ~ "R5",
    abbr %in% r6 ~ "R6",
    abbr %in% r7 ~ "R7",
    abbr %in% r8 ~ "R8",
    abbr %in% r9 ~ "R9"
  )) %>%
  mutate(avgOSW = case_when(
    abbr %in% r1 ~ 20.6,
    abbr %in% r2 ~ 26.2,
    abbr %in% r3 ~ 39.1,
    abbr %in% r5 ~ 51.8,
    abbr %in% r7 ~ 21.7,
    abbr %in% r9 ~ 46.1
  ))

# singles out region 8 because no offshore wind is available in 8 - dont want it to
# skew the heatmap 

r8map <- subset(state_map, abbr %in% c(r8, r4, r6))

# map showing average osw capacity over all scenarios - for comparison between regions
# total osw capacity

regOSW_map <- ggplot() + 
  geom_polygon(data = state_map, aes(x = long, y = lat, group = group, fill = avgOSW),
               color = "white") +
  geom_polygon(data = r8map, aes(x = long, y = lat, group = group),
               color = "gray", fill = "white") +
  geom_label(data = reg.names, aes(x = long, y = lat, label = region), size = 4) +
  coord_fixed(1.2) +
  labs(title = "Distribution of Offshore Wind Capacity",
       fill = "GW",
       subtitle = "Average cumulative capacity across all scenarios") +
  theme_bw() +
  noaxes

regOSW_map_col <- regOSW_map + color_fill_cont

regOSW_map_bw <- regOSW_map + gray_fill_cont


## ~ Tables ----

# set kable options

options(knitr.kable.NA = '-')

# kable tables for osw capacity and elc generation by region

cap_region_table <- osw_varcap_regiontotals %>%
  kable(booktabs = T, caption = "Average Installed Capacity (GW)", linesep = "") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))%>%
  footnote(general = "Average is across all scenarios")

output_region_table <- osw_varfout_regiontotals %>% mutate_if(is.numeric, ~round(.,1)) %>% 
  kable(booktabs = T, caption = "Average Electricity Output (PJ)", linesep = "") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))%>%
  footnote(general = "Average is across all scenarios")

# matrix kable tables for total osw capacity and elc production in 2050

cap_2050_table <- osw_varcap_2050total %>%
  kable(booktabs = T, caption = "Offshore Wind Total Installed Capacity (GW): 2050", 
        digits = 1, linesep = "") %>% 
  kable_styling(latex_options = c("striped", "hold_position")) %>% 
  add_header_above(c("CO2 Cap (%)", "Cost Reduction (%)" = 5)) 

output_2050_table <- osw_varfout_2050total %>% 
  kable(booktabs = T, caption = "Offshore Wind Total Output (PJ): 2050", 
        digits = 1, linesep = "") %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>% 
  add_header_above(c("CO2 Cap (%)", "Cost Reduction (%)" = 5)) 

## Grid Mix ----

## ~ Basecase ----

baseprod <- elc_long %>% filter(emred == "BAU" & costred == "20") %>%
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Technology: Reference Case") +
  yt +
  x_disc

baseprod_line_col <- baseprod + 
  geom_line(aes(x = Year, y = VAR_FOut, color = Technology, group = Technology), size = .85) +
  geom_text_repel(data = . %>% filter(Year == last(Year)),
             aes(label = Technology, x = Year, y = VAR_FOut, color = Technology),
             size = 5, vjust = -2) +
  osw_color +
  nolegend

baseprod_line_bw <-baseprod + 
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Technology, group = Technology), size = .75) +
  geom_label(data = . %>% filter(Year == last(Year)),
             aes(label = Technology, x = Year, y = VAR_FOut), 
             size = 3, alpha = .3, vjust = 0.1, hjust = 1) +
  nolegend

baseprod_fill_col <- baseprod +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Technology, group = Technology)) +
  osw_fill +
  bottom1

baseprod_fill_bw <- baseprod +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Technology, group = Technology), color = "black") +
  gray_fill +
  bottom1

baseprod_reg <- elc_long_reg %>% filter(emred == "BAU" & costred == "20") %>%
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Technology: Baseline") +
  yt +
  x_disc_l

baseprod_reg_line_col <- baseprod_reg + 
  geom_line(aes(x = Year, y = VAR_FOut, color = Technology, group = Technology), size = .75) +
  facet_grid(.~Region) +
  osw_color +
  bottom1

baseprod_reg_line_bw <-baseprod_reg + 
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Technology, group = Technology), size = .75) +
  geom_label(data = . %>% filter(Year == last(Year)),
             aes(label = Technology, x = Year, y = VAR_FOut), 
             size = 3, alpha = .3, vjust = 0.1, hjust = 1) +
  facet_grid(.~Region) +
  bottom1

baseprod_reg_fill_col <- baseprod_reg +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Technology, group = Technology)) +
  facet_grid(.~Region) +
  osw_fill +
  bottom1 

baseprod_reg_fill_bw <- baseprod_reg +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Technology, group = Technology), color = "black") +
  facet_grid(.~Region) +
  gray_fill +
  bottom1

baseprod_renew_reg <- elc_long_reg %>% filter(emred == "BAU" & costred == "20") %>%
  filter(Technology %in% c("Hydro", "Solar", "Terrestrial Wind", "Offshore Wind")) %>%
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Technology: Baseline") +
  yt +
  x_disc_l

baseprod_reg_renew_line_col <- baseprod_renew_reg + 
  geom_line(aes(x = Year, y = VAR_FOut, color = Technology, group = Technology), size = .75) +
  facet_grid(.~Region) +
  osw_color +
  bottom1

## ~ Overall ----

gridmix_all <- elc_long %>% filter(costred != "20") %>%
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Technology") +
  facet_grid(emred~costred, labeller = labeller(emred = emissionlabels, costred = costlabels)) +
  yt +
  x_disc_l +
  bottom1 
  
gridmix_all_col <- gridmix_all +
  geom_line(aes(x = Year, y = VAR_FOut, color = Technology, group = Technology)) +
  osw_color
  
gridmix_all_bw <- gridmix_all +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Technology, group = Technology))

gridmix <- elc_long %>% 
  filter(!costred %in% c("20", "30", "40")) %>%
  filter(emred %in% c("BAU", "40", "60", "80")) %>%
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Technology") +
  facet_grid(emred~costred, labeller = labeller(emred = emissionlabels, costred = costlabels)) +
  yt +
  x_disc_l +
  bottom1

gridmix_col <- gridmix +
  geom_line(aes(x = Year, y = VAR_FOut, color = Technology, group = Technology), size = 1) +
  osw_color

gridmix_bw <- gridmix + 
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Technology, group = Technology))

gridmix_poster <- elc_long %>% 
  filter(!costred %in% c("20", "30", "40")) %>%
  filter(emred %in% c("BAU", "40", "70")) %>%
  ggplot() +
  geom_line(aes(x = Year, y = VAR_FOut, color = Technology, group = Technology), size = 1) +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Technology") +
  facet_grid(emred~costred, labeller=labeller(emred = emissionlabels, costred = costlabels)) +
  yt +
  x_disc_l +
  osw_color +
  theme(legend.title=element_blank())  

## ~ Regions ----

gridmix_r1_col <- grid.region.col("R1")
gridmix_r2_col <- grid.region.col("R2")
gridmix_r3_col <- grid.region.col("R3")
gridmix_r4_col <- grid.region.col("R4")
gridmix_r5_col <- grid.region.col("R5")
gridmix_r6_col <- grid.region.col("R6")
gridmix_r7_col <- grid.region.col("R7")
gridmix_r8_col <- grid.region.col("R8")
gridmix_r9_col <- grid.region.col("R9")

gridmix_r1_bw <- grid.region.bw("R1")
gridmix_r2_bw <- grid.region.bw("R2")
gridmix_r3_bw <- grid.region.bw("R3")
gridmix_r4_bw <- grid.region.bw("R4")
gridmix_r5_bw <- grid.region.bw("R5")
gridmix_r6_bw <- grid.region.bw("R6")
gridmix_r7_bw <- grid.region.bw("R7")
gridmix_r8_bw <- grid.region.bw("R8")
gridmix_r9_bw <- grid.region.bw("R9")

gridmix_r1_bar_col <- grid.region.bar.col("R1")
gridmix_r2_bar_col <- grid.region.bar.col("R2")
gridmix_r3_bar_col <- grid.region.bar.col("R3")
gridmix_r4_bar_col <- grid.region.bar.col("R4")
gridmix_r5_bar_col <- grid.region.bar.col("R5")
gridmix_r6_bar_col <- grid.region.bar.col("R6")
gridmix_r7_bar_col <- grid.region.bar.col("R7")
gridmix_r8_bar_col <- grid.region.bar.col("R8")
gridmix_r9_bar_col <- grid.region.bar.col("R9")

gridmix_r1_bar_bw <- grid.region.bar.bw("R1")
gridmix_r2_bar_bw <- grid.region.bar.bw("R2")
gridmix_r3_bar_bw <- grid.region.bar.bw("R3")
gridmix_r4_bar_bw <- grid.region.bar.bw("R4")
gridmix_r5_bar_bw <- grid.region.bar.bw("R5")
gridmix_r6_bar_bw <- grid.region.bar.bw("R6")
gridmix_r7_bar_bw <- grid.region.bar.bw("R7")
gridmix_r8_bar_bw <- grid.region.bar.bw("R8")
gridmix_r9_bar_bw <- grid.region.bar.bw("R9")


## ~ Emissions Reductions ----

embau <- elc_long %>% filter(emred == "BAU" & !costred %in% c("20"))
em30 <- elc_long %>% filter(emred == "30" & !costred %in% c("20", "80"))
em40 <- elc_long %>% filter(emred == "40" & !costred %in% c("20", "80"))
em50 <- elc_long %>% filter(emred == "50" & !costred %in% c("20", "80"))
em60 <- elc_long %>% filter(emred == "60" & !costred %in% c("20", "80"))
em70 <- elc_long %>% filter(emred == "70" & !costred %in% c("20", "80"))
em80 <- elc_long %>% filter(emred == "80" & !costred %in% c("20", "80"))

bau_facetcost <-  embau %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Technology",
       subtitle = "No Emissions Reductions") +
  facet_grid(.~costred, labeller = labeller(emred = emissionlabels, costred = costlabels)) +
  yt +
  x_disc

bau_facetcost_line_col <- bau_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, color = Technology, group = Technology), size = 0.75) +
  osw_color +
  bottom1  

bau_facetcost_line_bw <- bau_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Technology, group = Technology), size = 0.75) +
  bottom1  

bau_facetcost_fill_col <- bau_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Technology, group = Technology), color = "black") +
  osw_fill + 
  bottom1  

bau_facetcost_fill_bw <- bau_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Technology, group = Technology), color = "black") +
  gray_fill +
  bottom1  

em30_facetcost <-  em30 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Technology",
       subtitle = "30% Emissions Reductions") +
  facet_grid(.~costred, labeller = labeller(emred = emissionlabels, costred = costlabels)) +
  yt +
  x_disc

em30_facetcost_line_col <- em30_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, color = Technology, group = Technology), size = 0.75) +
  osw_color +
  bottom1  

em30_facetcost_line_bw <- em30_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Technology, group = Technology), size = 0.75) +
  bottom1  

em30_facetcost_fill_col <- em30_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Technology, group = Technology), color = "black") +
  osw_fill +
  bottom1  

em30_facetcost_fill_bw <- em30_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Technology, group = Technology), color = "black") +
  gray_fill +
  bottom1  

em40_facetcost <-  em40 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Technology",
       subtitle = "40% Emissions Reductions") +
  facet_grid(.~costred, labeller = labeller(emred = emissionlabels, costred = costlabels)) +
  yt +
  x_disc

em40_facetcost_line_col <- em40_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, color = Technology, group = Technology), size = 0.75) +
  osw_color +
  bottom1  

em40_facetcost_line_bw <- em40_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Technology, group = Technology), size = 0.75) +
  bottom1  

em40_facetcost_fill_col <- em40_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Technology, group = Technology), color = "black") +
  osw_fill +
  bottom1  

em40_facetcost_fill_bw <- em40_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Technology, group = Technology), color = "black") +
  gray_fill +
  bottom1  

em50_facetcost <-  em50 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Technology",
       subtitle = "50% Emissions Reductions") +
  facet_grid(.~costred, labeller = labeller(emred = emissionlabels, costred = costlabels)) +
  yt +
  x_disc

em50_facetcost_line_col <- em50_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, color = Technology, group = Technology), size = 0.75) +
  osw_color +
  bottom1  

em50_facetcost_line_bw <- em50_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Technology, group = Technology), size = 0.75) +
  bottom1  

em50_facetcost_fill_col <- em50_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Technology, group = Technology), color = "black") +
  osw_fill +
  bottom1  

em50_facetcost_fill_bw <- em50_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Technology, group = Technology), color = "black") +
  gray_fill +
  bottom1  

em60_facetcost <-  em60 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Technology",
       subtitle = "60% Emissions Reductions") +
  facet_grid(.~costred, labeller = labeller(emred = emissionlabels, costred = costlabels)) +
  yt +
  x_disc

em60_facetcost_line_col <- em60_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, color = Technology, group = Technology), size = 0.75) +
  osw_color +
  bottom1  

em60_facetcost_line_bw <- em60_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Technology, group = Technology), size = 0.75) +
  bottom1  

em60_facetcost_fill_col <- em60_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Technology, group = Technology), color = "black") +
  osw_fill +
  bottom1  

em60_facetcost_fill_bw <- em60_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Technology, group = Technology), color = "black") +
  gray_fill +
  bottom1  

em70_facetcost <-  em70 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Technology",
       subtitle = "70% Emissions Reductions") +
  facet_grid(.~costred, labeller = labeller(emred = emissionlabels, costred = costlabels)) +
  yt +
  x_disc

em70_facetcost_line_col <- em70_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, color = Technology, group = Technology), size = 0.75) +
  osw_color +
  bottom1  

em70_facetcost_line_bw <- em70_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Technology, group = Technology), size = 0.75) +
  bottom1  

em70_facetcost_fill_col <- em70_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Technology, group = Technology), color = "black") +
  osw_fill +
  bottom1  

em70_facetcost_fill_bw <- em70_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Technology, group = Technology), color = "black") +
  gray_fill +
  bottom1  

em80_facetcost <-  em80 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Technology",
       subtitle = "80% Emissions Reductions") +
  facet_grid(.~costred, labeller = labeller(emred = emissionlabels, costred = costlabels)) +
  yt +
  x_disc

em80_facetcost_line_col <- em80_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, color = Technology, group = Technology), size = 0.75) +
  osw_color +
  bottom1  

em80_facetcost_line_bw <- em80_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Technology, group = Technology), size = 0.75) +
  bottom1  

em80_facetcost_fill_col <- em80_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Technology, group = Technology), color = "black") +
  osw_fill +
  bottom1  

em80_facetcost_fill_bw <- em80_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Technology, group = Technology), color = "black") +
  gray_fill +
  bottom1  

## ~ Cost Reductions ----

cost50 <- elc_long %>% filter(costred == "50" & !emred %in% c("30", "80"))
cost60 <- elc_long %>% filter(costred == "60" & !emred %in% c("30", "80"))
cost70 <- elc_long %>% filter(costred == "70" & !emred %in% c("30", "80"))
cost80 <- elc_long %>% filter(costred == "80" & !emred %in% c("30", "80"))

cost50_facetem <-  cost50 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Technology",
       subtitle = "50% Cost Reductions") +
  facet_grid(.~emred, labeller = labeller(emred = emissionlabels, costred = costlabels)) +
  yt +
  x_disc_l

cost50_facetem_line_col <- cost50_facetem +
  geom_line(aes(x = Year, y = VAR_FOut, color = Technology, group = Technology), size = 0.75) +
  osw_color +
  bottom1  

cost50_facetem_line_bw <- cost50_facetem +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Technology, group = Technology), size = 0.75) +
  bottom1  

cost50_facetem_fill_col <- cost50_facetem +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Technology, group = Technology), color = "black") +
  osw_fill +
  bottom1  

cost50_facetem_fill_bw <- cost50_facetem +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Technology, group = Technology), color = "black") +
  gray_fill +
  bottom1  

cost60_facetem <-  cost60 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Technology",
       subtitle = "60% Cost Reductions") +
  facet_grid(.~emred, labeller = labeller(emred = emissionlabels, costred = costlabels)) +
  yt +
  x_disc_l

cost60_facetem_line_col <- cost60_facetem +
  geom_line(aes(x = Year, y = VAR_FOut, color = Technology, group = Technology), size = 0.75) +
  osw_color +
  bottom1  

cost60_facetem_line_bw <- cost60_facetem +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Technology, group = Technology), size = 0.75) +
  bottom1  

cost60_facetem_fill_col <- cost60_facetem +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Technology, group = Technology), color = "black") +
  osw_fill +
  bottom1  

cost60_facetem_fill_bw <- cost60_facetem +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Technology, group = Technology), color = "black") +
  gray_fill +
  bottom1  

cost70_facetem <-  cost70 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Technology",
       subtitle = "70% Cost Reductions") +
  facet_grid(.~emred, labeller = labeller(emred = emissionlabels, costred = costlabels)) +
  yt +
  x_disc_l

cost70_facetem_line_col <- cost70_facetem +
  geom_line(aes(x = Year, y = VAR_FOut, color = Technology, group = Technology), size = 0.75) +
  osw_color +
  bottom1  

cost70_facetem_line_bw <- cost70_facetem +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Technology, group = Technology), size = 0.75) +
  bottom1  

cost70_facetem_fill_col <- cost70_facetem +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Technology, group = Technology), color = "black") +
  osw_fill +
  bottom1  

cost70_facetem_fill_bw <- cost70_facetem +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Technology, group = Technology), color = "black") +
  gray_fill +
  bottom1  

cost80_facetem <-  cost80 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Technology",
       subtitle = "80% Cost Reductions") +
  facet_grid(.~emred, labeller = labeller(emred = emissionlabels, costred = costlabels)) +
  yt +
  x_disc_l

cost80_facetem_line_col <- cost80_facetem +
  geom_line(aes(x = Year, y = VAR_FOut, color = Technology, group = Technology), size = 0.75) +
  osw_color +
  bottom1  

cost80_facetem_line_bw <- cost80_facetem +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Technology, group = Technology), size = 0.75) +
  bottom1  

cost80_facetem_fill_col <- cost80_facetem +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Technology, group = Technology), color = "black") +
  osw_fill +
  bottom1  

cost80_facetem_fill_bw <- cost80_facetem +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Technology, group = Technology), color = "black") +
  gray_fill +
  bottom1  

## ~ Heat Maps ----

elc_long_heatmaps <- elc_long %>% 
  filter(Year == "2050") %>%
  filter(costred != "20")

osw <- elc_long_heatmaps %>% filter(Technology == "Offshore Wind")
wnd <- elc_long_heatmaps %>% filter(Technology == "Terrestrial Wind")
sol <- elc_long_heatmaps %>% filter(Year == "2050" & Technology == "Solar" & costred != "20")
coal <- elc_long_heatmaps %>% filter(Technology == "Coal")
nga <- elc_long_heatmaps %>% filter(Technology == "Natural Gas")
nuk <- elc_long_heatmaps %>% filter(Technology == "Nuclear")
hyd <- elc_long_heatmaps %>% filter(Technology == "Hydro")
ccs <- elc_long_heatmaps %>% filter(Technology == "Coal CCS") %>%
  replace_with_na(replace = list(VAR_FOut = 0))
other <- elc_long_heatmaps %>% filter(Technology == "Other") %>%
  mutate(VAR_FOut = round(VAR_FOut, 0))

osw_grid_heatmap_col <- grid.heatmap.col(osw, "Offshore Wind Electricity Output: 2050")
wnd_grid_heatmap_col <- grid.heatmap.col(wnd, "Terrestrial Wind Electricity Output: 2050")
sol_grid_heatmap_col <- grid.heatmap.col(sol, "Solar Electricity Output: 2050")
coal_grid_heatmap_col <- grid.heatmap.col(coal, "Coal Electricity Output: 2050")
nga_grid_heatmap_col <- grid.heatmap.col(nga, "Natural Gas Electricity Output: 2050")
nuk_grid_heatmap_col <- grid.heatmap.col(nuk, "Nuclear Electricity Output: 2050")
hyd_grid_heatmap_col <- grid.heatmap.col(hyd, "Hydropower Electricity Output: 2050")
ccs_grid_heatmap_col <- grid.heatmap.col(ccs, "Coal with CCS Electricity Output: 2050")
other_grid_heatmap_col <- grid.heatmap.col(other, "Other Electricity Output: 2050")

osw_grid_heatmap_bw <- grid.heatmap.bw(osw, "Offshore Wind Electricity Output: 2050")
wnd_grid_heatmap_bw <- grid.heatmap.bw(wnd, "Terrestrial Wind Electricity Output: 2050")
sol_grid_heatmap_bw <- grid.heatmap.bw(sol, "Solar Electricity Output: 2050")
coal_grid_heatmap_bw <- grid.heatmap.bw(coal, "Coal Electricity Output: 2050")
nga_grid_heatmap_bw <- grid.heatmap.bw(nga, "Natural Gas Electricity Output: 2050")
nuk_grid_heatmap_bw <- grid.heatmap.bw(nuk, "Nuclear Electricity Output: 2050")
hyd_grid_heatmap_bw <- grid.heatmap.bw(hyd, "Hydropower Electricity Output: 2050")
ccs_grid_heatmap_bw <- grid.heatmap.bw(ccs, "Coal with CCS Retrofits Electricity Output: 2050")
other_grid_heatmap_bw <- grid.heatmap.bw(other, "Other Electricity Output: 2050")

heatmap_col <- grid.heatmap.col(elc_long, "Grid Mix Production by Technology")
heatmap_bw <- grid.heatmap.bw(elc_long, "Grid Mix Production by Technology")


## ~ RPS ---------------------

rps_graph <- rps %>% filter(costred != "40" & costred != "20") %>%
  filter(emred %in% c("BAU", "40", "60", "80")) %>%
  ggplot() +
  geom_line(aes(x = Year, y = perRenew, color = costred, group = Scenario),
            size = 0.75) +
  labs(x = "Year", y = "Renewables (%)",
       title = "Renewable Technology Contribution to Electricity Production", 
       color = "Cost Reduction (%)") +
  facet_grid(.~emred, labeller = labeller(emred = emissionlabels)) +
  yt +
  x_disc_l +
  bottom1 +
  bottom2

rps_col <- rps_graph + costosw_color

rps_bw <- rps_graph + gray_color


## ~ Market Share -----------------

marketShare <- oswmarket %>% split(.$Technology) %>% map(summary)

marketshare_heatmap <- oswmarket %>%
  ggplot(aes(x = costred, y = emred, fill = MarketShare)) +
  geom_tile() +
  facet_wrap(~Technology) +
  labs(x = "Offshore Wind Cost Reductions (%)",
       y = "CO2 Cap (%)",
       title = "Technology Market Share: 2050",
       fill = "Market Share\n(%)") +
  scale_fill_distiller(palette = "Spectral") +
  st

marketShareTable <- oswmarket_table %>%
  kable(longtable = T, booktabs = T, caption = "2050 Percent Market Share by Technology",
        digits = 1, linesep = "") %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>% 
  column_spec(1, width = "2 cm") %>%
  add_header_above(c(" " = 2, "Cost Reduction (%)" = 6)) %>%
  collapse_rows(columns = 1, latex_hline = "major")

  
totaloutput_table <- totaloutput %>%
  kable(longtable = T, booktabs = T, caption = "2050 Elc Production by Technology (PJ)",
        digits = 1, linesep = "") %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>% 
  column_spec(1, width = "2 cm") %>%
  add_header_above(c(" " = 2, "Cost Reduction (%)" = 6)) %>%
  collapse_rows(columns = 1, latex_hline = "major")


## ~ Retirements and Additions----

# baseline 

base_retire <- retire_long %>%
  filter(emred == "BAU" & costred == "20") %>%
  filter(Technology != "Other") %>%
  ggplot() +
  labs(x = "Year", y = "Change in Production (PJ)",
       title = "BAU Retirements and Additions") +
  yt

base_retire_fill_col <- base_retire + 
  geom_bar(aes(x = Year, y = retire, fill = Technology), 
           stat = "identity", position = "stack", width = 0.9, color = "black") +
  osw_fill +
  zero

base_retire_fill_bw <- base_retire + 
  geom_bar(aes(x = Year, y = retire, fill = Technology), 
           stat = "identity", position = "stack", width = 0.9, colour = "black") +
  gray_fill +
  zero

# diff summary 

prod_dif_all_col <- prod.dif.col(prod_dif,"Changes in Grid Mix over Baseline")

prod_dif_all_bw <- prod.dif.bw(prod_dif,"Changes in Grid Mix over Baseline") 

prod_dif_col <- prod.dif.col(
  (prod_dif %>% 
     filter(costred %in% c("50", "60", "70", "80")) %>% 
     filter(emred %in% c("BAU", "40", "60", "80"))),
  "Changes in Grid Mix over Reference Case")

prod_dif_bw <- prod.dif.bw(
  (prod_dif %>% 
     filter(costred %in% c("40", "50", "70")) %>% 
     filter(emred %in% c("BAU", "40", "70"))),
  "Changes in Grid Mix over Baseline") 

# regional diff summary

prod_dif_min <- prod_dif_reg %>% 
  filter(costred %in% c("40", "50", "70")) %>% 
  filter(emred %in% c("BAU", "40", "70"))

prod_dif_r1_col <- prod.dif.col(
  (prod_dif_min %>% filter(Region == "R1")), "Changes in Grid Mix over Baseline", "R1")

prod_dif_r1_bw <- prod.dif.bw(
  (prod_dif_min %>% filter(Region == "R1")), "Changes in Grid Mix over Baseline", "R1")

prod_dif_r2_col <- prod.dif.col(
  (prod_dif_min %>% filter(Region == "R2")), "Changes in Grid Mix over Baseline", "R2")

prod_dif_r2_bw <- prod.dif.bw(
  (prod_dif_min %>% filter(Region == "R2")), "Changes in Grid Mix over Baseline", "R2")

prod_dif_r3_col <- prod.dif.col(
  (prod_dif_min %>% filter(Region == "R3")), "Changes in Grid Mix over Baseline", "R3")

prod_dif_r3_bw <- prod.dif.bw(
  (prod_dif_min %>% filter(Region == "R3")), "Changes in Grid Mix over Baseline", "R3")

prod_dif_r4_col <- prod.dif.col(
  (prod_dif_min %>% filter(Region == "R4")), "Changes in Grid Mix over Baseline", "R4")

prod_dif_r4_bw <- prod.dif.bw(
  (prod_dif_min %>% filter(Region == "R4")), "Changes in Grid Mix over Baseline", "R4")

prod_dif_r5_col <- prod.dif.col(
  (prod_dif_min %>% filter(Region == "R5")), "Changes in Grid Mix over Baseline", "R5")

prod_dif_r5_bw <- prod.dif.bw(
  (prod_dif_min %>% filter(Region == "R5")), "Changes in Grid Mix over Baseline", "R5")

prod_dif_r6_col <- prod.dif.col(
  (prod_dif_min %>% filter(Region == "R6")), "Changes in Grid Mix over Baseline", "R6")

prod_dif_r6_bw <- prod.dif.bw(
  (prod_dif_min %>% filter(Region == "R6")), "Changes in Grid Mix over Baseline", "R6")

prod_dif_r7_col <- prod.dif.col(
  (prod_dif_min %>% filter(Region == "R7")), "Changes in Grid Mix over Baseline", "R7")

prod_dif_r7_bw <- prod.dif.bw(
  (prod_dif_min %>% filter(Region == "R7")), "Changes in Grid Mix over Baseline", "R7")

prod_dif_r8_col <- prod.dif.col(
  (prod_dif_min %>% filter(Region == "R8")), "Changes in Grid Mix over Baseline", "R8")

prod_dif_r8_bw <- prod.dif.bw(
  (prod_dif_min %>% filter(Region == "R8")), "Changes in Grid Mix over Baseline", "R8")

prod_dif_r9_col <- prod.dif.col(
  (prod_dif_min %>% filter(Region == "R9")), "Changes in Grid Mix over Baseline", "R9")

prod_dif_r9_bw <- prod.dif.bw(
  (prod_dif_min %>% filter(Region == "R9")), "Changes in Grid Mix over Baseline", "R9")

# diff by emissions

prod_dif_bau_col <- prod.dif.em.col(
  (prod_dif %>% filter(emred == "BAU" & costred %in% c("40", "50", "60", "70", "80"))),
  "Changes in Grid Mix over Baseline: BAU")

prod_dif_bau_bw <- prod.dif.em.bw(
  (prod_dif %>% filter(emred == "BAU" & costred %in% c("40", "50", "60", "70", "80"))),
  "Changes in Grid Mix over Baseline: BAU")

prod_dif_e30_col <- prod.dif.em.col(
  (prod_dif %>% filter(emred == "30" & costred %in% c("40", "50", "60", "70", "80"))),
  "Changes in Grid Mix over Baseline: E30")

prod_dif_e30_bw <- prod.dif.em.bw(
  (prod_dif %>% filter(emred == "30" & costred %in% c("40", "50", "60", "70", "80"))),
  "Changes in Grid Mix over Baseline: E30")

prod_dif_e40_col <- prod.dif.em.col(
  (prod_dif %>% filter(emred == "40" & costred %in% c("40", "50", "60", "70", "80"))),
  "Changes in Grid Mix over Baseline: E40")

prod_dif_e40_bw <- prod.dif.em.bw(
  (prod_dif %>% filter(emred == "40" & costred %in% c("40", "50", "60", "70", "80"))),
  "Changes in Grid Mix over Baseline: E40")

prod_dif_e50_col <- prod.dif.em.col(
  (prod_dif %>% filter(emred == "50" & costred %in% c("40", "50", "60", "70", "80"))),
  "Changes in Grid Mix over Baseline: E50")

prod_dif_e50_bw <- prod.dif.em.bw(
  (prod_dif %>% filter(emred == "50" & costred %in% c("40", "50", "60", "70", "80"))),
  "Changes in Grid Mix over Baseline: E50")

prod_dif_e60_col <- prod.dif.em.col(
  (prod_dif %>% filter(emred == "60" & costred %in% c("40", "50", "60", "70", "80"))),
  "Changes in Grid Mix over Baseline: E60")

prod_dif_e60_bw <- prod.dif.em.bw(
  (prod_dif %>% filter(emred == "60" & costred %in% c("40", "50", "60", "70", "80"))),
  "Changes in Grid Mix over Baseline: E60")

prod_dif_e70_col <- prod.dif.em.col(
  (prod_dif %>% filter(emred == "70" & costred %in% c("40", "50", "60", "70", "80"))),
  "Changes in Grid Mix over Baseline: E70")

prod_dif_e70_bw <- prod.dif.em.bw(
  (prod_dif %>% filter(emred == "70" & costred %in% c("40", "50", "60", "70", "80"))),
  "Changes in Grid Mix over Baseline: E70")

prod_dif_e80_col <- prod.dif.em.col(
  (prod_dif %>% filter(emred == "80" & costred %in% c("40", "50", "60", "70", "80"))),
  "Changes in Grid Mix over Baseline: E80")

prod_dif_e80_bw <- prod.dif.em.bw(
  (prod_dif %>% filter(emred == "80" & costred %in% c("40", "50", "60", "70", "80"))),
  "Changes in Grid Mix over Baseline: E80")

# diff by costs

prod_dif_c40_col <- prod.dif.cost.col(
  (prod_dif %>% filter(costred == "40")),
  "Changes in Grid Mix over Baseline: C40")

prod_dif_c40_bw <- prod.dif.cost.bw(
  (prod_dif %>% filter(costred == "40")),
  "Changes in Grid Mix over Baseline: C40")

prod_dif_c50_col <- prod.dif.cost.col(
  (prod_dif %>% filter(costred == "50")),
  "Changes in Grid Mix over Baseline: C50")

prod_dif_c50_bw <- prod.dif.cost.bw(
  (prod_dif %>% filter(costred == "50")),
  "Changes in Grid Mix over Baseline: C50")

prod_dif_c60_col <- prod.dif.cost.col(
  (prod_dif %>% filter(costred == "60")),
  "Changes in Grid Mix over Baseline: C60")

prod_dif_c60_bw <- prod.dif.cost.bw(
  (prod_dif %>% filter(costred == "60")),
  "Changes in Grid Mix over Baseline: C60")

prod_dif_c70_col <- prod.dif.cost.col(
  (prod_dif %>% filter(costred == "70")),
  "Changes in Grid Mix over Baseline: C70")

prod_dif_c70_bw <- prod.dif.cost.bw(
  (prod_dif %>% filter(costred == "70")),
  "Changes in Grid Mix over Baseline: C70")

prod_dif_c80_col <- prod.dif.cost.col(
  (prod_dif %>% filter(costred == "80")),
  "Changes in Grid Mix over Baseline: C80")

prod_dif_c80_bw <- prod.dif.cost.bw(
  (prod_dif %>% filter(costred == "80")),
  "Changes in Grid Mix over Baseline: C80")

## ~ New Capacity by Technology Set----

test <- newcap_total_diff %>% filter(emred == "80" & costred == "80")

testplot <- ggplot(data = test) +
  geom_bar(aes(x = 1, y = diff, fill = Technology, group = Scenario), 
           stat = "identity", color = "black") +
  facet_grid(costred~emred, labeller = labeller(emred = emissionlabels, costred = costlabels)) +
  osw_fill +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  zero

newcap_diff_bar <- ggplot(data = newcap_total_diff) +
  geom_bar(aes(x = 1, y = diff, fill = Technology, group = Scenario), 
           stat = "identity", color = "black") +
  facet_grid(emred~costred, labeller = labeller(emred = emissionlabels, costred = costlabels)) +
  osw_fill +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  zero


## Emissions ----

## ~ Timelines ----

emis_bau <- ggplot(data = emissions_bau, aes(x = Year, y = Emissions, fill = Commodity)) +
  labs(y = "Emissions (kt)*",
       title = "Baseline Electric Sector Emissions",
       caption = "*Units are Mt for CO2 and kt for all other emissions") +
  yt +
  bottom1

emis_bau_fill_col <- emis_bau +
  geom_bar(stat = "identity", position = "stack", aes(fill = Commodity), color = "black") +
  commodity_fill

emis_bau_line_col <- emis_bau +
  geom_line(aes(group = Commodity, color = Commodity)) +
  commodity_color

emis_bau_fill_bw <- emis_bau +
  geom_bar(stat = "identity", position = "stack", color = "black", aes(fill = Commodity)) +
  gray_fill

emis_bau_line_bw <- emis_bau +
  geom_line(aes(group = Commodity, linetype = Commodity))
  
emis_plot <- emissions_long %>% filter(!costred %in% c("40", "30", "20")) %>%
  filter(emred %in% c("BAU", "40", "60", "80")) %>%
  ggplot(aes(x = Year, y= Emissions)) +
  labs(x = "Year", y = "Emissions (kt)*",
       title = "Electric Sector Emissions Output",
       caption = "*Units are Mt for CO2",
       color = "Cost Reduction (%)",
       linetype = "CO2 Cap (%)") +
  facet_wrap(~Commodity, scales = "free_y", nrow = 1, labeller = label_parsed) +
  yt +
  x_disc_l +
  bottom1 + bottom2 +
  theme(legend.box = "vertical") +
  guides(colour = guide_legend(nrow = 1)) +
  linetype

emis_col <- emis_plot +
  geom_line(aes(x=Year, y=Emissions, color = costred, group = Scenario, linetype = emred), size = 1) +
  costosw_color

emis_bw <- emis_plot +
  geom_line(aes(x=Year, y=Emissions, color = costred, group = Scenario, linetype = emred)) +
  gray_color

emis_perc_plot <- emissions_percent %>% filter(!costred %in% c("40", "30", "20")) %>%
  filter(emred %in% c("BAU", "40", "60", "80")) %>%
  ggplot(aes(x = Year, y= percent.red)) +
  labs(x = "Year", y = "% Reduction from 2010",
       title = "Electric Sector Emissions Reductions",
       linetype = "CO2 Cap (%)",
       color = "Cost Reduction (%)") +
  facet_wrap(~Commodity, nrow = 1, labeller = label_parsed) +
  yt +
  x_disc_l +
  bottom1 + 
  bottom2 +
  theme(legend.box = "vertical") +
  guides(colour = guide_legend(nrow = 1)) +
  scale_y_reverse() +
  linetype

emis_perc_col <- emis_perc_plot +
  geom_line(aes(x=Year, y=percent.red, color = costred, group = Scenario, linetype = emred), 
            size = 1) +
  costosw_color

emis_perc_pm2.5 <- emissions_percent %>% filter(!costred %in% c("40", "30", "20")) %>%
  filter(emred %in% c("BAU", "40", "60", "80")) %>%
  filter(Commodity == "PM[2.5]") %>%
  ggplot(aes(x = Year, y= percent.red)) +
  labs(x = "Year", y = "% Reduction from 2010",
       title = "Electric Sector Emissions Reductions",
       linetype = "CO2 Cap (%)",
       color = "Cost Reduction (%)") +
  facet_wrap(~Commodity, nrow = 1, labeller = label_parsed) +
  yt +
  x_disc_l +
  bottom1 + 
  bottom2 +
  theme(legend.box = "vertical") +
  guides(colour = guide_legend(nrow = 1)) +
  scale_y_reverse() +
  linetype +
  geom_line(aes(x=Year, y=percent.red, color = costred, group = Scenario, linetype = emred)) +
  costosw_color

emis_perc_bar <- emissions_percent %>% 
  filter(!costred %in% c("40", "30", "20")) %>%
  filter(emred %in% c("BAU", "40", "60", "80")) %>%
  filter(Year == "2050") %>%
  ggplot(aes(x = " ", y= percent.red)) +
  labs(x = "Year", y = "% Reduction from 2010",
       title = "Electric Sector Emissions Reductions",
       fill = "Cost Reduction (%)",
       alpha = "CO2 Reduction (%)") +
  facet_wrap(~Commodity, nrow = 1, labeller = label_parsed) +
  yt +
  x_disc_l +
  bottom1 + 
  bottom2 +
  theme(legend.box = "vertical") +
  guides(colour = guide_legend(nrow = 1)) +
  scale_y_reverse() +
  geom_bar(stat = "identity", position = "dodge",
           aes(fill = costred, group = Scenario, alpha = emred),
           color = "black") +
  costosw_fill

co2_plot <- emissions_long %>% filter(Commodity == "CO[2]") %>%
  ggplot(aes(x = Year, y = Emissions)) +
  facet_grid(~emred, labeller = labeller(emred = emissionlabels, costred = costlabels)) +
  labs(x = "Year", y = "Emissions (Mt)",
       title = bquote("Electric Sector"~CO[2]~"Emissions"),
       color = "Cost\nReduction\n(%)") +
  yt +
  x_disc_l

co2_col <- co2_plot +
  geom_line(aes(group = Scenario, color = costred)) +
  cost_color

co2_bw <- co2_plot +
  geom_line(aes(group = Scenario, linetype = costred))

so2_plot <- emissions_long %>% filter(Commodity == "SO[2]") %>%
  ggplot(aes(x = Year, y = Emissions)) +
  facet_grid(~emred, labeller = labeller(emred = emissionlabels, costred = costlabels)) +
  labs(x = "Year", y = "Emissions (kt)",
       title = bquote("Electric Sector"~SO[2]~"Emissions"),
       color = "Cost\nReduction\n(%)") +
  yt +
  x_disc_l

so2_col <- so2_plot +
  geom_line(aes(group = Scenario, color = costred)) +
  cost_color

so2_bw <- so2_plot +
  geom_line(aes(group = Scenario, linetype = costred))

nox_plot <- emissions_long %>% filter(Commodity == "NO[X]") %>%
  ggplot(aes(x = Year, y = Emissions)) +
  facet_grid(~emred, labeller = labeller(emred = emissionlabels, costred = costlabels)) +
  labs(x = "Year", y = "Emissions (kt)",
       title = bquote("Electric Sector"~NO[X]~"Emissions"),
       color = "Cost\nReduction\n(%)") +
  yt +
  x_disc_l

nox_col <- nox_plot +
  geom_line(aes(group = Scenario, color = costred)) +
  cost_color

nox_bw <- nox_plot +
  geom_line(aes(group = Scenario, linetype = costred))

ch4_plot <- emissions_long %>% filter(Commodity == "CH[4]") %>%
  ggplot(aes(x = Year, y = Emissions)) +
  facet_grid(~emred, labeller = labeller(emred = emissionlabels, costred = costlabels)) +
  labs(x = "Year", y = "Emissions (kt)",
       title = bquote("Electric Sector"~CH[4]~"Emissions"),
       color = "Cost\nReduction\n(%)") +
  yt +
  x_disc_l

ch4_col <- ch4_plot +
  geom_line(aes(group = Scenario, color = costred)) +
  cost_color

ch4_bw <- ch4_plot +
  geom_line(aes(group = Scenario, linetype = costred))

pm2.5_plot <- emissions_long %>% filter(Commodity == "PM[2.5]") %>%
  ggplot(aes(x = Year, y = Emissions)) +
  facet_grid(~emred, labeller = labeller(emred = emissionlabels, costred = costlabels)) +
  labs(x = "Year", y = "Emissions (kt)",
       title = bquote("Electric Sector"~PM[2.5]~"Emissions"),
       color = "Cost\nReduction\n(%)") +
  yt +
  x_disc_l

pm2.5_col <- pm2.5_plot +
  geom_line(aes(group = Scenario, color = costred)) +
  cost_color

pm2.5_bw <- pm2.5_plot +
  geom_line(aes(group = Scenario, linetype = costred))


idx_em <- match(emissions_long$emred, names(emissionlabels))
emissions_long1 <- emissions_long
emissions_long1$emred <- emissionlabels[idx_em]
idx_osw <- match(osw_varcap_long$emred, names(emissionlabels))
osw_varcap_long1 <- osw_varcap_long
osw_varcap_long1$emred <- emissionlabels[idx_osw]


# ggplot() +
#   geom_line(data = emissions_long1 %>% 
#               filter(Commodity == "CO[2]") %>%
#               filter(!costred %in% c("20","30","40")),
#             aes(x = Year, y = Emissions, group = Scenario), color = "black") +
#   geom_bar(data = osw_varcap_long1, stat = "identity",
#             aes(x = Year, y = VAR_Cap*3), alpha = 0.5, color = "black") +
#   scale_y_continuous(sec.axis = sec_axis(~./3, name = "Offshore Wind Capacity (GW)")) +
#   facet_grid(emred~costred, 
#              labeller = labeller(emred = label_wrap_gen(width = 10),
#                                  costred = costlabels)) +
#   yt +
#   x_disc_l
# 
# ggplot() +
#   geom_line(data = emissions_long1 %>% 
#               filter(Commodity != "PM[2.5]" & Commodity != "CH[4]") %>%
#               filter(!costred %in% c("20","30","40")),
#             aes(x = Year, y = Emissions, group = Commodity, color = Commodity)) +
#   geom_bar(data = osw_varcap_long1, stat = "identity", color = "black", 
#            aes(x = Year, y = VAR_Cap*3), alpha = 0.5) +
#   scale_y_continuous(sec.axis = sec_axis(~./3, name = "Offshore Wind Capacity (GW)")) +
#   facet_grid(emred~costred, 
#              labeller = labeller(emred = label_wrap_gen(width = 10),
#                                  costred = costlabels)) +
#   yt +
#   x_disc_l +
#   bottom1 +
#   commodity_color
# 
# ggplot() +
#   geom_line(data = emissions_long1 %>% 
#               filter(Commodity == "PM[2.5]" | Commodity == "CH[4]") %>%
#               filter(!costred %in% c("20","30","40")),
#             aes(x = Year, y = Emissions, group = Commodity, color = Commodity)) +
#   geom_bar(data = osw_varcap_long1, stat = "identity",
#            aes(x = Year, y = VAR_Cap*.25), alpha = 0.5, color = "black") +
#   scale_y_continuous(sec.axis = sec_axis(~./.25, name = "Offshore Wind Capacity (GW)")) +
#   facet_grid(emred~costred, scales = "free_y",
#              labeller = labeller(emred = label_wrap_gen(width = 10),
#                                  costred = costlabels)) +
#   yt +
#   x_disc_l +
#   bottom1 +
#   commodity_color
# 
# 
# ggplot() +
#   geom_line(data = emissions_long %>% filter(Commodity == "CO[2]" &
#                                                emred == "50" &
#                                                costred != "20" &
#                                                costred != "30"),
#             aes(x = Year, y = Emissions, group = Scenario), color = "black") +
#   geom_line(data = osw_varcap_long %>% filter(emred == "50" &
#                                                 costred != "20" &
#                                                 costred != "30"),
#             aes(x = Year, y = VAR_Cap*3, group = Scenario),
#             color = "deepskyblue4") +
#   scale_y_continuous(sec.axis = sec_axis(~./3, name = "Offshore Wind Capacity (GW)")) +
#   facet_grid(costred~emred, labeller = labeller(emred = emissionlabels, costred = costlabels))
# 
# ggplot() +
#   geom_line(data = emissions_long %>% filter(emred == "20" | emred == "50" | emred == "80") %>%
#               filter(costred == "50" | costred == "70") %>%
#               filter(Commodity != "CH4" | Commodity != "PM 2.5"),
#             aes(x = Year, y = Emissions, group = Commodity, color = Commodity)) +
#   geom_line(data = osw_varcap_long %>% filter(emred == "20" | emred == "50" | emred == "80") %>%
#               filter(costred == "50" | costred == "70"),
#             aes(x = Year, y = VAR_Cap*3, group = Scenario, color = "Offshore Wind")) +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7)) +
#   scale_color_manual(breaks = c("CO2", "NOx", "SO2", "Offshore Wind"),
#                      values = c("chartreuse4", "seashell4", "deepskyblue4", "darkgoldenrod3", "black", "blue")) +
#   scale_y_continuous(sec.axis = sec_axis(~./3, name = "Offshore Wind Capacity (GW)")) +
#   facet_grid(costred~emred, labeller = label_both) +
#   labs(y = "Emissions*", caption = "*Emissions units are kt for NOx and SO2 and Mt for CO2")

## ~ Industrial Emissions ----

# indemissions %>% filter(Commodity == "CO[2]") %>% ggplot(aes(x = Year, y = Emissions)) +
#   geom_line(aes(group = Scenario)) +
#   facet_grid(costred~emred)

## ~ Heatmaps ----

# emissions data 

co2_hm_col <- em.heatmap.col(emissions2050, "CO[2]", "Electric Sector CO2 Emissions (Mt): 2050")
so2_hm_col <- em.heatmap.col(emissions2050, "SO[2]", "Electric Sector SO2 Emissions (kt): 2050")
nox_hm_col <- em.heatmap.col(emissions2050, "NO[X]", "Electric Sector NOx Emissions (kt): 2050")
ch4_hm_col <- em.heatmap.col(emissions2050, "CH[4]", "Electric Sector CH4 Emissions (kt): 2050")
pm2.5_hm_col <- em.heatmap.col(emissions2050, "PM[2.5]", "Electric Sector PM 2.5 Emissions (kt): 2050")

co2_hm_bw <- em.heatmap.bw(emissions2050, "CO[2]", "Electric Sector CO2 Emissions (Mt): 2050")
so2_hm_bw <- em.heatmap.bw(emissions2050, "SO[2]", "Electric Sector SO2 Emissions (kt): 2050")
nox_hm_bw <- em.heatmap.bw(emissions2050, "NO[X]", "Electric Sector NOx Emissions (kt): 2050")
ch4_hm_bw <- em.heatmap.bw(emissions2050, "CH[4]", "Electric Sector CH4 Emissions (kt): 2050")
pm2.5_hm_bw <- em.heatmap.bw(emissions2050, "PM[2.5]", "Electric Sector PM 2.5 Emissions (kt): 2050")

# percent reduction emissions

co2_hm_per_col <- em.heatmap.per.col(emissions2050_percent, "CO[2]", "Electric Sector CO2 Emissions: 2050")
so2_hm_per_col <- em.heatmap.per.col(emissions2050_percent, "SO[2]", "Electric Sector SO2 Emissions: 2050")
nox_hm_per_col <- em.heatmap.per.col(emissions2050_percent, "NO[X]", "Electric Sector NOx Emissions: 2050")
ch4_hm_per_col <- em.heatmap.per.col(emissions2050_percent, "CH[4]", "Electric Sector CH4 Emissions: 2050")
pm2.5_hm_per_col <- em.heatmap.per.col(emissions2050_percent, "PM[2.5]", "Electric Sector PM 2.5 Emissions: 2050")

co2_hm_per_bw <- em.heatmap.per.bw(emissions2050_percent, "CO[2]", "Electric Sector CO2 Emissions: 2050")
so2_hm_per_bw <- em.heatmap.per.bw(emissions2050_percent, "SO[2]", "Electric Sector SO2 Emissions: 2050")
nox_hm_per_bw <- em.heatmap.per.bw(emissions2050_percent, "NO[X]", "Electric Sector NOx Emissions: 2050")
ch4_hm_per_bw <- em.heatmap.per.bw(emissions2050_percent, "CH[4]", "Electric Sector CH4 Emissions: 2050")
pm2.5_hm_per_bw <- em.heatmap.per.bw(emissions2050_percent, "PM[2.5]", "Electric Sector PM 2.5 Emissions: 2050")

emis_perc_hm <- emissions2050_percent %>%
  ggplot(aes(x = costred, y= emred)) +
  geom_tile(aes(fill = percent.red), colour = "white", size = 0.25) +
  labs(x = "Offshore Wind Cost Reductions (%)",
       y = "CO2 Reduction (%)",
       title = "Electric Sector Emissions Reduction: 2050",
       fill = "Reduction\n(%)") +
  facet_wrap(~Commodity, nrow = 1, labeller = label_parsed) +
  st +
  fill_cont

## Total Electricity Production ----

elcdata <- elctotal_long %>% filter(!costred %in% c("20", "30")) 
elc2050 <- elcdata %>% filter(Year == "2050")

elctotal_line <- ggplot(elcdata) +
  facet_grid(~costred, labeller = labeller(costred = costlabels)) +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Total Electricity Production by Scenario",
       color = "CO2 Cap (%)",
       linetype = "CO2 Cap (%)") +
  yt +
  bottom1 +
  bottom2 +
  x_disc_l

elctotal_line_col <- elctotal_line +
  geom_line(aes(x = Year, y = VAR_FOut, color = emred, group = Scenario)) +
  em_color

elctotal_line_bw <- elctotal_line +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = emred, group = Scenario))

elctotal_byem <- ggplot(elcdata) +
  geom_line(aes(x = Year, y = VAR_FOut, color = costred, group = Scenario), size = 0.75) +
  facet_grid(~emred, labeller = labeller(emred = emissionlabels)) +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Total Electricity Production by Scenario",
       color = "Cost Reduction (%)",
       linetype = "Cost Reduction (%)") +
  yt +
  bottom1 +
  costosw_color

elctotal_heat <- ggplot(elc2050, aes(x = costred, y = emred)) +
  geom_tile(aes(fill = VAR_FOut), colour = "gray", size = 0.25) +
  labs(x = "Offshore Wind Cost Reductions (%)",
       y = "CO2 Cap (%)",
       title = "Total Electricity Production: 2050",
       fill = "Electricity\nProduced\n(PJ)")

elctotal_heat_col <- elctotal_heat + fill_cont + 
  geom_text(aes(label = round(VAR_FOut, 0)), color = "black", size = 3)
  
elctotal_heat_bw <- elctotal_heat + gray_fill_cont +
  geom_text(aes(label = round(VAR_FOut, 0)), color = "white", size = 3)

## End Uses ----

## ~ Timelines ----

enduse_line <- enduse %>% filter(!Sector %in% c("CHP Industrial", "Grid Industrial")) %>%
  group_by(Scenario, emred, costred, Year) %>%
  summarize(Consumption = sum(Consumption)) %>%
  filter(!costred %in% c("20", "30", "40")) %>%
  ggplot() +
  facet_grid(~costred, labeller = labeller(costred = costlabels)) +
  labs(x = "Year", y = "Electricity Consumption (PJ)",
       title = "Total Electricity Consumption by Scenario",
       color = "CO2 Cap (%)",
       subtitle = "Grid and CHP electricity for Industrial sector") +
  yt +
  bottom1 +
  geom_line(aes(x = Year, y = Consumption, color = emred, group = Scenario)) +
  em_color

enduse_nochp_line <- enduse %>% filter(!Sector %in% c("All Industrial", "CHP Industrial")) %>%
  group_by(Scenario, emred, costred, Year) %>%
  summarize(Consumption = sum(Consumption)) %>%
  filter(!costred %in% c("20", "30", "40")) %>%
  ggplot() +
  facet_grid(~costred, labeller = labeller(costred = costlabels)) +
  labs(x = "Year", y = "Electricity Consumption (PJ)",
       title = "Total Electricity Consumption by Scenario",
       color = "CO2 Cap (%)", 
       subtitle = "Only grid electricity for Industrial sector") +
  yt +
  bottom1 +
  geom_line(aes(x = Year, y = Consumption, color = emred, group = Scenario)) +
  em_color

sector_line <- ggplot(enduse %>% 
                        filter(costred != 40) %>% 
                        filter(!Sector %in% c("CHP Industrial", "Grid Industrial"))) +
  facet_grid(Sector~costred, scales = "free_y", labeller = labeller(costred = costlabels)) +
  labs(x = "Year", y = "Electricity Consumption (PJ)",
       title = "Sector Electricity Consumption by Scenario",
       color = "CO2 Cap (%)") +
  yt +
  bottom1 +
  bottom2 +
  geom_line(aes(x = Year, y = Consumption, color = emred, group = Scenario)) +
  em_color +
  x_disc_l

sector_split_ind <- ggplot(enduse %>% 
                             filter(!costred %in% c("20", "40")) %>% 
                             filter(Sector %in% c("CHP Industrial", "Grid Industrial"))) +
  facet_grid(Sector~costred, scales = "free_y", labeller = labeller(costred = costlabels)) +
  labs(x = "Year", y = "Electricity Consumption/Production (PJ)",
       title = "Industrial Electricity Consumption &Production",
       color = "CO2 Cap (%)") +
  yt +
  bottom1 +
  bottom2 +
  geom_line(aes(x = Year, y = Consumption, color = emred, group = Scenario),
            size = 0.75) +
  em_color +
  x_disc_l

sector_bar_ind <- ggplot(enduse %>%
                           filter(!costred %in% c("20", "40")) %>% 
                           filter(Sector %in% c("CHP Industrial", "Grid Industrial")) %>%
                           filter(Year == "2050")) +
  facet_grid(Sector~costred, scales = "free_y", labeller = labeller(costred = costlabels)) +
  labs(y = "Electricity Consumption (PJ)", x = "CO2 Reduction (%)",
       title = "Sector Electricity Consumption by Scenario",
       fill = "CO2 Cap (%)") +
  yt +
  bottom1 +
  bottom2 +
  geom_bar(aes(x = emred, y = Consumption, fill = emred), 
           stat = "identity", position = "dodge") +
  em_fill +
  x_disc_l

trn_line <- enduse %>% filter(Sector == "Transportation" & costred != 40) %>%
  ggplot() +
  facet_grid(~costred, labeller = labeller(costred = costlabels)) +
  labs(x = "Year", y = "Electricity Consumption (PJ)",
       title = "Transportation Electricity Consumption by Scenario",
       color = "CO2 Cap (%)") +
  yt +
  bottom1 +
  geom_line(aes(x = Year, y = Consumption, color = emred, group = Scenario)) +
  em_color

res_line <- enduse %>% filter(Sector == "Residential" & costred != 40) %>%
  ggplot() +
  facet_grid(~costred, labeller = labeller(costred = costlabels)) +
  labs(x = "Year", y = "Electricity Consumption (PJ)",
       title = "Residential Electricity Consumption by Scenario",
       color = "CO2 Cap (%)") +
  yt +
  bottom1 +
  geom_line(aes(x = Year, y = Consumption, color = emred, group = Scenario)) +
  em_color

com_line <- enduse %>% filter(Sector == "Commercial" & costred != 40) %>%
  ggplot() +
  facet_grid(~costred, labeller = labeller(costred = costlabels)) +
  labs(x = "Year", y = "Electricity Consumption (PJ)",
       title = "Commercial Electricity Consumption by Scenario",
       color = "CO2 Cap (%)") +
  yt +
  bottom1 +
  geom_line(aes(x = Year, y = Consumption, color = emred, group = Scenario)) +
  em_color

ind_line <- enduse %>% filter(Sector == "All Industrial" & costred != 40) %>%
  ggplot() +
  facet_grid(~costred, labeller = labeller(costred = costlabels)) +
  labs(x = "Year", y = "Electricity Consumption (PJ)",
       title = "Industrial Electricity Consumption by Scenario",
       color = "CO2 Cap (%)") +
  yt +
  bottom1 +
  geom_line(aes(x = Year, y = Consumption, color = emred, group = Scenario)) +
  em_color
  
ind_split_line <- enduse %>% 
  filter(Sector == "CHP Industrial" | Sector == "Grid Industrial") %>%
  filter(!costred %in% c("20", "30", "40")) %>%
  filter(emred %in% c("BAU", "40", "60", "80")) %>%
  ggplot() +
  facet_grid(emred~costred, labeller = labeller(costred = costlabels, emred = emissionlabels)) +
  yt + x_disc_l +
  labs(x = "Year", y = "Electricity Consumption (PJ)",
       title = "Industrial Electricity Consumption by Scenario") +
  geom_line(aes(x = Year, y = Consumption, color = Sector, group = Sector)) +
  scale_color_manual(name = "Electricity Source", labels = c("Industrial CHP", "Grid ELC"), 
                     values = c("#B45F04", "#0B3861")) +
  bottom1

ind_split_bar <- enduse %>% 
  filter(Sector == "CHP Industrial" | Sector == "Grid Industrial") %>%
  filter(Year == "2050") %>%
  filter(!costred %in% c("20", "30", "40")) %>%
  filter(emred %in% c("BAU", "40", "60", "80")) %>%
  ggplot() +
  geom_bar(aes(x = " ", y = Consumption, fill = Sector), 
           stat = "identity", position = "dodge") +
  facet_grid(emred~costred, labeller = labeller(costred = costlabels, emred = emissionlabels)) +
  yt + x_disc_l +
  labs(x = " ", y = "Electricity Consumption (PJ)",
       title = "Industrial Electricity Consumption by Scenario") +
  
  scale_fill_manual(name = "Electricity Source", labels = c("Industrial CHP", "Grid ELC"), 
                     values = c("#B45F04", "#0B3861")) +
  bottom1

## ~ Heatmaps ----

enduse_heat <- enduse %>% filter(Year == "2050") %>%
  ggplot(aes(x = costred, y = emred, fill = Consumption)) +
  geom_tile(colour = "gray", size = 0.25) +
  labs(x = "Offshore Wind Cost Reductions (%)",
       y = "CO2 Cap (%)",
       title = "Sector Electricity Consumption: 2050",
       fill = "Electricity\nConsumed\n(PJ)") +
  fill_cont +
  facet_wrap(~Sector, scales = "free_y")

trn_heat <- enduse %>% filter(Year == "2050" & Sector == "Transportation") %>%
  ggplot(aes(x = costred, y = emred, fill = Consumption)) +
  geom_tile(colour = "gray", size = 0.25) +
  labs(x = "Offshore Wind Cost Reductions (%)",
       y = "CO2 Cap (%)",
       title = "Transportation Electricity Consumption: 2050",
       fill = "Electricity\nConsumed\n(PJ)") +
  fill_cont

com_heat <- enduse %>% filter(Year == "2050" & Sector == "Commercial") %>%
  ggplot(aes(x = costred, y = emred, fill = Consumption)) +
  geom_tile(colour = "gray", size = 0.25) +
  labs(x = "Offshore Wind Cost Reductions (%)",
       y = "CO2 Cap (%)",
       title = "Commercial Electricity Consumption: 2050",
       fill = "Electricity\nConsumed\n(PJ)") +
  fill_cont

res_heat <- enduse %>% filter(Year == "2050" & Sector == "Residential") %>%
  ggplot(aes(x = costred, y = emred, fill = Consumption)) +
  geom_tile(colour = "gray", size = 0.25) +
  labs(x = "Offshore Wind Cost Reductions (%)",
       y = "CO2 Cap (%)",
       title = "Residential Electricity Consumption: 2050",
       fill = "Electricity\nConsumed\n(PJ)") +
  fill_cont

ind_heat <- enduse %>% filter(Year == "2050" & Sector == "Industrial") %>%
  ggplot(aes(x = costred, y = emred, fill = Consumption)) +
  geom_tile(colour = "gray", size = 0.25) +
  labs(x = "Offshore Wind Cost Reductions (%)",
       y = "CO2 Cap (%)",
       title = "Industrial Electricity Consumption: 2050",
       fill = "Electricity\nConsumed\n(PJ)") +
  fill_cont

ind_grid_heat <- enduse %>% filter(Year == "2050" & Sector == "Grid Industrial") %>%
  ggplot(aes(x = costred, y = emred, fill = Consumption)) +
  geom_tile(colour = "gray", size = 0.25) +
  labs(x = "Offshore Wind Cost Reductions (%)",
       y = "CO2 Cap (%)",
       title = "Industrial Grid Electricity Consumption: 2050",
       fill = "Electricity\nConsumed\n(PJ)") +
  fill_cont

ind_chp_heat <- enduse %>% filter(Year == "2050" & Sector == "CHP Industrial") %>%
  ggplot(aes(x = costred, y = emred, fill = Consumption)) +
  geom_tile(colour = "gray", size = 0.25) +
  labs(x = "Offshore Wind Cost Reductions (%)",
       y = "CO2 Cap (%)",
       title = "Industrial CHP Electricity Consumption: 2050",
       fill = "Electricity\nConsumed\n(PJ)") +
  fill_cont


## Correlations ----

## ~ OSW Only Scenarios (28) ----

correlations <- sapply(oswcor, cor.test, method="spearman", exact = F, y = oswcor$`cap2050`)
correlations.table <- as.data.frame(correlations)
chart.Correlation(oswcor, histogram = TRUE, method = "spearman")

## ~ All Scenarios (42) ----

all.correlations <- sapply(allcor, cor.test, method="spearman", exact = F, y = allcor$`cap2050`)
all.correlations.table <- as.data.frame(all.correlations)
chart.Correlation(allcor, histogram = FALSE, method = "spearman")


## Regression ---- 

# box plots 

# ggplot(oswcor) + geom_boxplot(aes(x = costred, y = `CO[2]`, group = emred))
# ggplot(oswcor) + geom_boxplot(aes(x = costred, y = `CO[2]`, group = emred, color = emred))
# ggplot(oswcor) + geom_boxplot(aes(x = costred, y = `CO[2]`, group = emred, fill = emred))
# ggplot(oswcor) + geom_boxplot(aes(x = costred, y = `CO[2]`, group = emred, fill = costred))
# ggplot(oswcor) + geom_boxplot(aes(x = costred, y = `CO[2]`, group = costred))
# ggplot(oswcor) + geom_boxplot(aes(x = emred, y = `CO[2]`, group = emred))
# ggplot(oswcor) + geom_boxplot(aes(x = emred, y = `SO[2]`, group = emred))
# ggplot(oswcor) + geom_boxplot(aes(x = costred, y = `SO[2]`, group = costred))
# ggplot(oswcor) + geom_boxplot(aes(x = costred, y = `CH[4]`, group = costred))
# ggplot(oswcor) + geom_boxplot(aes(x = costred, y = `CH[4]`, group = costred))
# ggplot(oswcor) + geom_boxplot(aes(x = costred, y = cap2050, group = costred))
# ggplot(oswcor, aes(x = emred, y = cap2050, group = emred)) + 
#   geom_boxplot() +
#   geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1)

## ~ Regressions for all data ----

# Regressions are for all cases WITH OSW 

{
cap2050.fit <- lm(`cap2050`~emred+costred, data = oswcor)
cap2050.sum <- summary(cap2050.fit)
cap2050.relimp <- calc.relimp(cap2050.fit, type  = "lmg", rela=TRUE)
cap2050.het <- bptest(cap2050.fit) # pass p > 0.05

totalelc.fit <- lm(`Total Elc`~emred+costred, data = oswcor)
totalelc.sum <- summary(totalelc.fit)
totalelc.relimp <- calc.relimp(totalelc.fit, type  = "lmg", rela=TRUE)
totalelc.het <- bptest(totalelc.fit) # pass p > 0.05

rps.fit <- lm(`perRenew`~emred+costred, data = oswcor)
rps.sum <- summary(rps.fit)
rps.relimp <- calc.relimp(rps.fit, type  = "lmg", rela=TRUE)
rps.het <- bptest(rps.fit) # pass p > 0.05

renew.fit <- lm(Renewable~emred+costred, data = oswcor)
renew.sum <- summary(renew.fit)
renew.relimp <- calc.relimp(renew.fit, type  = "lmg", rela=TRUE)
renew.het <- bptest(renew.fit) # pass p > 0.05

co2.fit <- lm(`CO[2]`~emred+cap2050, data = oswcor)
co2.sum <- summary(co2.fit)
co2.relimp <- calc.relimp(co2.fit, type  = "lmg", rela=TRUE)
co2.het <- bptest(co2.fit) # DOESNT PASS, 0.046
## If trying to reduce/remove heteroskedasticity
co2.robustSE_white <- coeftest(co2.fit, vcov = vcovHC(co2.fit, "HC1"))
co2.robust <- lmrob(`CO[2]`~emred+cap2050, data = oswcor)
co2.rob.sum <- summary(co2.robust)

so2.fit <- lm(`SO[2]`~emred+cap2050, data = oswcor)
so2.sum <- summary(so2.fit)
so2.relimp <- calc.relimp(so2.fit, type  = "lmg", rela=TRUE)
so2.het <- bptest(so2.fit) # pass p > 0.05

nox.fit <- lm(`NO[X]`~emred+cap2050, data = oswcor)
nox.sum <- summary(nox.fit)
nox.relimp <- calc.relimp(nox.fit, type  = "lmg", rela=TRUE)
nox.het <- bptest(nox.fit) # pass p > 0.05

pm2.5.fit <- lm(`PM[2.5]`~emred+cap2050, data = oswcor)
pm2.5.sum <- summary(pm2.5.fit)
pm2.5.relimp <- calc.relimp(pm2.5.fit, type  = "lmg", rela=TRUE)
pm2.5.het <- bptest(pm2.5.fit) # pass p > 0.05

ch4.fit <- lm(`CH[4]`~emred+cap2050, data = oswcor)
ch4.sum <- summary(ch4.fit)
ch4.relimp <- calc.relimp(ch4.fit, type  = "lmg", rela=TRUE)
ch4.het <- bptest(ch4.fit) # DOESNT PASS, 0.023
## If trying to reduce/remove heteroskedasticity
ch4.robustSE_white <- coeftest(ch4.fit, vcov = vcovHC(ch4.fit, "HC1"))
ch4.robust <- lmrob(`CH[4]`~emred+cap2050, data = oswcor)
ch4.rob.sum <- summary(ch4.robust)

gridcoef_names <- c("CO2 Cap" = "emred", "Cost Reduction" = "costred")
emissioncoef_names <- c("CO2 Cap" = "emred", "OSW Capacity" = "cap2050")
emissionmodel_names <- c(expression(CO[2]), expression(SO[2]), expression(NO[X]),
                         expression(PM[2.5]), expression(CH[4]))

grid_modeltable <- export_summs(cap2050.fit, rps.fit, totalelc.fit,
                                scale = FALSE,
                                model.names = c("OSW Capacity","% Renewables","Total Elc"),
                                coefs = gridcoef_names)

grid_graph <- plot_summs(cap2050.fit, rps.fit, totalelc.fit, 
                         scale = FALSE, 
                         model.names = c("OSW Capacity", "% Renewables","Total Elc"),
                         coefs = gridcoef_names)

emission_modeltable <- export_summs(co2.fit, so2.fit, nox.fit, ch4.fit, pm2.5.fit,
                                    scale = FALSE,
                                    model.names = emissionmodel_names,
                                    coefs = emissioncoef_names)

emission_graph <- plot_summs(co2.fit,so2.fit,nox.fit,pm2.5.fit,ch4.fit,
                             scale = FALSE,
                             model.names = emissionmodel_names,
                             coefs = emissioncoef_names) 
}

## ~ Regressions by emissions scenarios ----

# Regressions just for emissions dependent variables. OSW capacity is the only coefficient,
# and there are 7 regressions for each emission (1 for each co2 cap scenario). This is meant
# to assess in which cases cost has the biggest influence.

{
oswcor.bau <- oswcor %>% filter(emred == "20")
oswcor.30 <- oswcor %>% filter(emred == "30")
oswcor.40 <- oswcor %>% filter(emred == "40")
oswcor.50 <- oswcor %>% filter(emred == "50")
oswcor.60 <- oswcor %>% filter(emred == "60")
oswcor.70 <- oswcor %>% filter(emred == "70")
oswcor.80 <- oswcor %>% filter(emred == "80")
  
co2.bau <- lm(`CO[2]`~cap2050, data = oswcor.bau)
co2.30 <- lm(`CO[2]`~cap2050, data = oswcor.30)
co2.40 <- lm(`CO[2]`~cap2050, data = oswcor.40)
co2.50 <- lm(`CO[2]`~cap2050, data = oswcor.50)
co2.60 <- lm(`CO[2]`~cap2050, data = oswcor.60)
co2.70 <- lm(`CO[2]`~cap2050, data = oswcor.70)
co2.80 <- lm(`CO[2]`~cap2050, data = oswcor.80)

nox.bau <- lm(`NO[X]`~cap2050, data = oswcor.bau)
nox.30 <- lm(`NO[X]`~cap2050, data = oswcor.30)
nox.40 <- lm(`NO[X]`~cap2050, data = oswcor.40)
nox.50 <- lm(`NO[X]`~cap2050, data = oswcor.50)
nox.60 <- lm(`NO[X]`~cap2050, data = oswcor.60)
nox.70 <- lm(`NO[X]`~cap2050, data = oswcor.70)
nox.80 <- lm(`NO[X]`~cap2050, data = oswcor.80)

so2.bau <- lm(`SO[2]`~cap2050, data = oswcor.bau)
so2.30 <- lm(`SO[2]`~cap2050, data = oswcor.30)
so2.40 <- lm(`SO[2]`~cap2050, data = oswcor.40)
so2.50 <- lm(`SO[2]`~cap2050, data = oswcor.50)
so2.60 <- lm(`SO[2]`~cap2050, data = oswcor.60)
so2.70 <- lm(`SO[2]`~cap2050, data = oswcor.70)
so2.80 <- lm(`SO[2]`~cap2050, data = oswcor.80)

pm2.5.bau <- lm(`PM[2.5]`~cap2050, data = oswcor.bau)
pm2.5.30 <- lm(`PM[2.5]`~cap2050, data = oswcor.30)
pm2.5.40 <- lm(`PM[2.5]`~cap2050, data = oswcor.40)
pm2.5.50 <- lm(`PM[2.5]`~cap2050, data = oswcor.50)
pm2.5.60 <- lm(`PM[2.5]`~cap2050, data = oswcor.60)
pm2.5.70 <- lm(`PM[2.5]`~cap2050, data = oswcor.70)
pm2.5.80 <- lm(`PM[2.5]`~cap2050, data = oswcor.80)

ch4.bau <- lm(`CH[4]`~cap2050, data = oswcor.bau)
ch4.30 <- lm(`CH[4]`~cap2050, data = oswcor.30)
ch4.40 <- lm(`CH[4]`~cap2050, data = oswcor.40)
ch4.50 <- lm(`CH[4]`~cap2050, data = oswcor.50)
ch4.60 <- lm(`CH[4]`~cap2050, data = oswcor.60)
ch4.70 <- lm(`CH[4]`~cap2050, data = oswcor.70)
ch4.80 <- lm(`CH[4]`~cap2050, data = oswcor.80)


modelnames_emred <- c("BAU", "30% Cap", "40% Cap", "50% Cap",
                    "60% Cap", "70% Cap", "80% Cap")

co2_modeltable <- export_summs(co2.bau, co2.30, co2.40, co2.50, co2.60, 
                               co2.70, co2.80,
                               scale = FALSE,
                               model.names = modelnames_emred,
                               coefs = c("OSW Capacity" = "cap2050"))

co2_graph <- plot_summs(co2.bau, co2.30, co2.40, co2.50, co2.60, 
                        co2.70, co2.80,
                        scale = FALSE,
                        model.names = modelnames_emred,
                        coefs = c("OSW Capacity" = "cap2050"))

so2_modeltable <- export_summs(so2.bau, so2.30, so2.40, so2.50, so2.60, 
                               so2.70, so2.80,
                               scale = FALSE,
                               model.names = modelnames_emred,
                               coefs = c("OSW Capacity" = "cap2050"))

so2_graph <- plot_summs(so2.bau, so2.30, so2.40, so2.50, so2.60, 
                        so2.70, so2.80,
                        scale = FALSE,
                        model.names = modelnames_emred,
                        coefs = c("OSW Capacity" = "cap2050"))

nox_modeltable <- export_summs(nox.bau, nox.30, nox.40, nox.50, nox.60, 
                               nox.70, nox.80,
                               scale = FALSE,
                               model.names = modelnames_emred,
                               coefs = c("OSW Capacity" = "cap2050"))

nox_graph <- plot_summs(nox.bau, nox.30, nox.40, nox.50, nox.60, 
                        nox.70, nox.80,
                        scale = FALSE,
                        model.names = modelnames_emred,
                        coefs = c("OSW Capacity" = "cap2050"))

pm2.5_modeltable <- export_summs(pm2.5.bau, pm2.5.30, pm2.5.40, pm2.5.50, pm2.5.60, 
                               pm2.5.70, pm2.5.80,
                               scale = FALSE,
                               model.names = modelnames_emred,
                               coefs = c("OSW Capacity" = "cap2050"))

pm2.5_graph <- plot_summs(pm2.5.bau, pm2.5.30, pm2.5.40, pm2.5.50, pm2.5.60, 
                        pm2.5.70, pm2.5.80,
                        scale = FALSE,
                        model.names = modelnames_emred,
                        coefs = c("OSW Capacity" = "cap2050"))

ch4_modeltable <- export_summs(ch4.bau, ch4.30, ch4.40, ch4.50, ch4.60, 
                               ch4.70, ch4.80,
                               scale = FALSE,
                               model.names = modelnames_emred,
                               coefs = c("OSW Capacity" = "cap2050"))

ch4_graph <- plot_summs(ch4.bau, ch4.30, ch4.40, ch4.50, ch4.60, 
                        ch4.70, ch4.80,
                        scale = FALSE,
                        model.names = modelnames_emred,
                        coefs = c("OSW Capacity" = "cap2050"))

bau_modeltable <- export_summs(co2.bau, so2.bau, nox.bau, pm2.5.bau, ch4.bau,
                               scale = FALSE,
                               model.names = emissionmodel_names,
                               coefs = c("OSW Capacity" = "cap2050"))

bau_graph <- plot_summs(co2.bau, so2.bau, nox.bau, pm2.5.bau, ch4.bau,
                          scale = FALSE,
                          model.names = emissionmodel_names,
                          coefs = c("OSW Capacity" = "cap2050"))

e30_modeltable <- export_summs(co2.30, so2.30, nox.30, pm2.5.30, ch4.30,
                               scale = FALSE,
                               model.names = emissionmodel_names,
                               coefs = c("OSW Capacity" = "cap2050"))

e30_graph <- plot_summs(co2.30, so2.30, nox.30, pm2.5.30, ch4.30,
                        scale = FALSE,
                        model.names = emissionmodel_names,
                        coefs = c("OSW Capacity" = "cap2050"))

e40_modeltable <- export_summs(co2.40, so2.40, nox.40, pm2.5.40, ch4.40,
                               scale = FALSE,
                               model.names = emissionmodel_names,
                               coefs = c("OSW Capacity" = "cap2050"))

e40_graph <- plot_summs(co2.40, so2.40, nox.40, pm2.5.40, ch4.40,
                        scale = FALSE,
                        model.names = emissionmodel_names,
                        coefs = c("OSW Capacity" = "cap2050"))

e50_modeltable <- export_summs(co2.50, so2.50, nox.50, pm2.5.50, ch4.50,
                               scale = FALSE,
                               model.names = emissionmodel_names,
                               coefs = c("OSW Capacity" = "cap2050"))

e50_graph <- plot_summs(co2.50, so2.50, nox.50, pm2.5.50, ch4.50,
                        scale = FALSE,
                        model.names = emissionmodel_names,
                        coefs = c("OSW Capacity" = "cap2050"))

e60_modeltable <- export_summs(co2.60, so2.60, nox.60, pm2.5.60, ch4.60,
                               scale = FALSE,
                               model.names = emissionmodel_names,
                               coefs = c("OSW Capacity" = "cap2050"))

e60_graph <- plot_summs(co2.60, so2.60, nox.60, pm2.5.60, ch4.60,
                        scale = FALSE,
                        model.names = emissionmodel_names,
                        coefs = c("OSW Capacity" = "cap2050"))

e70_modeltable <- export_summs(co2.70, so2.70, nox.70, pm2.5.70, ch4.70,
                               scale = FALSE,
                               model.names = emissionmodel_names,
                               coefs = c("OSW Capacity" = "cap2050"))

e70_graph <- plot_summs(co2.70, so2.70, nox.70, pm2.5.70, ch4.70,
                        scale = FALSE,
                        model.names = emissionmodel_names,
                        coefs = c("OSW Capacity" = "cap2050"))

e80_modeltable <- export_summs(co2.80, so2.80, nox.80, pm2.5.80, ch4.80,
                               scale = FALSE,
                               model.names = emissionmodel_names,
                               coefs = c("OSW Capacity" = "cap2050"))

e80_graph <- plot_summs(co2.80, so2.80, nox.80, pm2.5.80, ch4.80,
                        scale = FALSE,
                        model.names = emissionmodel_names,
                        coefs = c("OSW Capacity" = "cap2050"))

emission_fullmodeltable <- rbind(
  co2_modeltable %>% mutate(Emission = "CO2"),
  so2_modeltable %>% mutate(Emission = "SO2"),
  nox_modeltable %>% mutate(Emission = "NOx"),
  pm2.5_modeltable %>% mutate(Emission = "PM 2.5"),
  ch4_modeltable %>% mutate(Emission = "CH4"))
emission_fullmodeltable <- emission_fullmodeltable[
  -c(4,6,7,10,12,13,16,18,19,22,24,25,28)] %>%
  select(Emission, everything()) %>%
  rename("Coefficient" = "names")
emission_fullmodeltable$Emission[17] <- c(" ")

}



## Save Plots ----

# plots <-

# lapply(plots,function(x){ggsave(file=paste(x,"pdf",sep="."),get(x))})
# https://stackoverflow.com/questions/20500706/saving-multiple-ggplots-from-ls-into-one-and-separate-files-in-r
