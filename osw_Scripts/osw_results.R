## Load Setup and Data Scripts ----

# allows me to quickly run my two previous scripts when I make changes in them or
# if I'm just going to work with results and need to pull in the data

# source("osw_Scripts/osw_setup.R")
# source("osw_Scripts/osw_data.R")

## Scenarios ----

## ~ Cost ----

# creates the graphs showing the different cost scenarios used

costscen_plot <- ggplot(scenario_cost) +
  geom_label(data = . %>% filter(Year == last(Year)),
             aes(label = paste(CostCurve,"%"), x = Year, y = Costs), 
             size = 3, alpha = 0.5, nudge_y = 4) +
  scale_y_continuous(limits = c(0, 100), expand = c(.01, 0)) +
  labs(x = "Year", y = "Cost Reduction", 
       title = "OSW Cost Curves", 
       subtitle = "Percentage decrease from 2018 offshore wind CAPEX costs") +
  yt +
  x_cont +
  nolegend
  
costscen_col <- costscen_plot +
  geom_line(aes(x = Year, y = Costs, color = CostCurve, group = CostCurve), size = 1) +
  cost_color

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
       title = "Offshore Wind Capacity",
       color = "Emissions Reduction (%)",
       linetype = "Emissions Reduction (%)") + 
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
  labs(x = "Cost Reduction (%)", y = "Emissions Reduction (%)",
       title = "2050 Offshore Wind Capacity",
       fill = "Capacity (GW)") +
  st +
  bottom1 +
  bottom2
  
cap_col_heat <- cap_heatmap + 
  geom_text(aes(label = VAR_Cap), color = "black", size = 5) +
  col_fill_cont

cap_bw_heat <- cap_heatmap + 
  geom_text(aes(label = VAR_Cap), color = "white", size = 5) +
  gray_fill_cont 

## ~ New Capacity ----

# new osw by scenario, co2 caps are multicolor lines and cost scenarios are facets

newcap_plot <- ggplot(osw_varncap_long) +
  labs(x = "Year", y = "New Installed Capacity (GW)",
       title = "Offshore Wind New Capacity",
       color = "Emissions Reduction (%)",
       linetype = "Emissions Reduction (%)") + 
  facet_grid(costred~., scales = "free_y") +
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
       color = "Emissions Reduction (%)",
       linetype = "Emissions Reduction (%)") +
  facet_grid(~costred) +
  yt +
  bottom1 + bottom2 +
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
    abbr %in% r1 ~ 21.5,
    abbr %in% r2 ~ 31.0,
    abbr %in% r3 ~ 44.1,
    abbr %in% r4 ~ 0.0,
    abbr %in% r5 ~ 50.5,
    abbr %in% r6 ~ 0.0,
    abbr %in% r7 ~ 20.7,
    abbr %in% r8 ~ 0.0,
    abbr %in% r9 ~ 45.0
  ))

# singles out region 8 because no offshore wind is available in 8 - dont want it to
# skew the heatmap 

r8map <- subset(state_map, abbr %in% r8)

# map showing average osw capacity over all scenarios - for comparison between regions
# total osw capacity

regOSW_map <- ggplot() + 
  geom_polygon(data = state_map, aes(x = long, y = lat, group = group, fill = avgOSW),
               color = "white") +
  geom_polygon(data = r8map, aes(x = long, y = lat, group = group),
               color = "gray", fill = "white") +
  geom_label(data = reg.names, aes(x = long, y = lat, label = region), size = 4) +
  coord_fixed(1.2) +
  labs(title = "Average Offshore Wind Capacity",
       fill = "GW") +
  theme_bw() +
  noaxes

regOSW_map_col <- regOSW_map + col_fill_cont

regOSW_map_bw <- regOSW_map + gray_fill_cont


## ~ Tables ----

# kable tables for osw capacity and elc generation by region

cap_region_table <- osw_varcap_regiontotals %>% 
  kable(booktabs = T, caption = "Average Installed Capacity (GW)", linesep = "") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))%>%
  footnote(general = "Average is across all scenarios")

output_region_table <- osw_varfout_regiontotals %>% 
  kable(booktabs = T, caption = "Average Electricity Output (PJ)", linesep = "") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))%>%
  footnote(general = "Average is across all scenarios")

# matrix kable tables for total osw capacity and elc production in 2050

cap_2050_table <- osw_varcap_2050total %>%
  kable(booktabs = T, caption = "Offshore Wind Total Installed Capacity (GW): 2050", 
        digits = 1, linesep = "") %>% 
  kable_styling(latex_options = c("striped", "hold_position")) %>% 
  add_header_above(c("CO2 Emissions\nReduction (%)", "Cost Reduction (%)" = 4)) 

output_2050_table <- osw_varfout_2050total %>% 
  kable(booktabs = T, caption = "Offshore Wind Total Output (PJ): 2050", 
        digits = 1, linesep = "") %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>% 
  add_header_above(c("CO2 Emissions\nReduction (%)", "Cost Reduction (%)" = 4)) 

## Grid Mix ----

## ~ Basecase ----

baseprod <- elc_long %>% filter(emred == "BAU" & costred == "20") %>%
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Process:\nBAU & 20% Cost Reduction") +
  yt +
  x_disc

baseprod_line_col <- baseprod + 
  geom_line(aes(x = Year, y = VAR_FOut, color = Process, group = Process), size = .75) +
  geom_label(data = . %>% filter(Year == last(Year)),
             aes(label = Process, x = Year, y = VAR_FOut, color = Process), 
             size = 3, alpha = .3, vjust = 0.1, hjust = 1) +
  osw_color +
  nolegend

baseprod_line_bw <-baseprod + 
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = .75) +
  geom_label(data = . %>% filter(Year == last(Year)),
             aes(label = Process, x = Year, y = VAR_FOut), 
             size = 3, alpha = .3, vjust = 0.1, hjust = 1) +
  nolegend

baseprod_fill_col <- baseprod +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process)) +
  osw_fill +
  bottom1 + bottom2

baseprod_fill_bw <- baseprod +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom1 + bottom2

## ~ Overall ----

gridmix_all <- elc_long %>%
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Process") +
  facet_grid(emred~costred) +
  yt +
  x_disc_l
  
gridmix_all_col <- gridmix_all +
  geom_line(aes(x = Year, y = VAR_FOut, color = Process, group = Process)) +
  osw_color
  
gridmix_all_bw <- gridmix_all +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process))

gridmix <- elc_long %>% 
  filter(!costred %in% c("20", "30", "80")) %>%
  filter(emred %in% c("BAU", "40", "60")) %>%
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Process") +
  facet_grid(emred~costred) +
  yt +
  x_disc_l

gridmix_col <- gridmix +
  geom_line(aes(x = Year, y = VAR_FOut, color = Process, group = Process), size = 1) +
  osw_color

gridmix_poster <- elc_long %>% 
  filter(!costred %in% c("20", "30", "40")) %>%
  filter(emred %in% c("BAU", "40", "70")) %>%
  ggplot() +
  geom_line(aes(x = Year, y = VAR_FOut, color = Process, group = Process), size = 1) +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Process") +
  facet_grid(emred~costred, labeller=labeller(emred = emissionlabels, costred = costlabels)) +
  yt +
  x_disc_l +
  osw_color +
  theme(legend.title=element_blank())

gridmix_bw <- gridmix + 
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process))
  

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
       title = "Electricity Production by Process",
       subtitle = "No Emissions Reductions") +
  facet_grid(.~costred) +
  yt +
  x_disc

bau_facetcost_line_col <- bau_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, color = Process, group = Process), size = 0.75) +
  osw_color +
  bottom1 + bottom2

bau_facetcost_line_bw <- bau_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom1 + bottom2

bau_facetcost_fill_col <- bau_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill + 
  bottom1 + bottom2

bau_facetcost_fill_bw <- bau_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom1 + bottom2

em30_facetcost <-  em30 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Process",
       subtitle = "30% Emissions Reductions") +
  facet_grid(.~costred) +
  yt +
  x_disc

em30_facetcost_line_col <- em30_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, color = Process, group = Process), size = 0.75) +
  osw_color +
  bottom1 + bottom2

em30_facetcost_line_bw <- em30_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom1 + bottom2

em30_facetcost_fill_col <- em30_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill +
  bottom1 + bottom2

em30_facetcost_fill_bw <- em30_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom1 + bottom2

em40_facetcost <-  em40 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Process",
       subtitle = "40% Emissions Reductions") +
  facet_grid(.~costred) +
  yt +
  x_disc

em40_facetcost_line_col <- em40_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, color = Process, group = Process), size = 0.75) +
  osw_color +
  bottom1 + bottom2

em40_facetcost_line_bw <- em40_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom1 + bottom2

em40_facetcost_fill_col <- em40_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill +
  bottom1 + bottom2

em40_facetcost_fill_bw <- em40_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom1 + bottom2

em50_facetcost <-  em50 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Process",
       subtitle = "50% Emissions Reductions") +
  facet_grid(.~costred) +
  yt +
  x_disc

em50_facetcost_line_col <- em50_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, color = Process, group = Process), size = 0.75) +
  osw_color +
  bottom1 + bottom2

em50_facetcost_line_bw <- em50_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom1 + bottom2

em50_facetcost_fill_col <- em50_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill +
  bottom1 + bottom2

em50_facetcost_fill_bw <- em50_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom1 + bottom2

em60_facetcost <-  em60 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Process",
       subtitle = "60% Emissions Reductions") +
  facet_grid(.~costred) +
  yt +
  x_disc

em60_facetcost_line_col <- em60_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, color = Process, group = Process), size = 0.75) +
  osw_color +
  bottom1 + bottom2

em60_facetcost_line_bw <- em60_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom1 + bottom2

em60_facetcost_fill_col <- em60_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill +
  bottom1 + bottom2

em60_facetcost_fill_bw <- em60_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom1 + bottom2

em70_facetcost <-  em70 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Process",
       subtitle = "70% Emissions Reductions") +
  facet_grid(.~costred) +
  yt +
  x_disc

em70_facetcost_line_col <- em70_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, color = Process, group = Process), size = 0.75) +
  osw_color +
  bottom1 + bottom2

em70_facetcost_line_bw <- em70_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom1 + bottom2

em70_facetcost_fill_col <- em70_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill +
  bottom1 + bottom2

em70_facetcost_fill_bw <- em70_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom1 + bottom2

em80_facetcost <-  em80 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Process",
       subtitle = "80% Emissions Reductions") +
  facet_grid(.~costred) +
  yt +
  x_disc

em80_facetcost_line_col <- em80_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, color = Process, group = Process), size = 0.75) +
  osw_color +
  bottom1 + bottom2

em80_facetcost_line_bw <- em80_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom1 + bottom2

em80_facetcost_fill_col <- em80_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill +
  bottom1 + bottom2

em80_facetcost_fill_bw <- em80_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom1 + bottom2

## ~ Cost Reductions ----

cost50 <- elc_long %>% filter(costred == "50" & !emred %in% c("30", "80"))
cost60 <- elc_long %>% filter(costred == "60" & !emred %in% c("30", "80"))
cost70 <- elc_long %>% filter(costred == "70" & !emred %in% c("30", "80"))
cost80 <- elc_long %>% filter(costred == "80" & !emred %in% c("30", "80"))

cost50_facetem <-  cost50 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Process",
       subtitle = "50% Cost Reductions") +
  facet_grid(.~emred) +
  yt +
  x_disc_l

cost50_facetem_line_col <- cost50_facetem +
  geom_line(aes(x = Year, y = VAR_FOut, color = Process, group = Process), size = 0.75) +
  osw_color +
  bottom1 + bottom2

cost50_facetem_line_bw <- cost50_facetem +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom1 + bottom2

cost50_facetem_fill_col <- cost50_facetem +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill +
  bottom1 + bottom2

cost50_facetem_fill_bw <- cost50_facetem +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom1 + bottom2

cost60_facetem <-  cost60 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Process",
       subtitle = "60% Cost Reductions") +
  facet_grid(.~emred) +
  yt +
  x_disc_l

cost60_facetem_line_col <- cost60_facetem +
  geom_line(aes(x = Year, y = VAR_FOut, color = Process, group = Process), size = 0.75) +
  osw_color +
  bottom1 + bottom2

cost60_facetem_line_bw <- cost60_facetem +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom1 + bottom2

cost60_facetem_fill_col <- cost60_facetem +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill +
  bottom1 + bottom2

cost60_facetem_fill_bw <- cost60_facetem +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom1 + bottom2

cost70_facetem <-  cost70 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Process",
       subtitle = "70% Cost Reductions") +
  facet_grid(.~emred) +
  yt +
  x_disc_l

cost70_facetem_line_col <- cost70_facetem +
  geom_line(aes(x = Year, y = VAR_FOut, color = Process, group = Process), size = 0.75) +
  osw_color +
  bottom1 + bottom2

cost70_facetem_line_bw <- cost70_facetem +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom1 + bottom2

cost70_facetem_fill_col <- cost70_facetem +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill +
  bottom1 + bottom2

cost70_facetem_fill_bw <- cost70_facetem +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom1 + bottom2

cost80_facetem <-  cost80 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Process",
       subtitle = "80% Cost Reductions") +
  facet_grid(.~emred) +
  yt +
  x_disc_l

cost80_facetem_line_col <- cost80_facetem +
  geom_line(aes(x = Year, y = VAR_FOut, color = Process, group = Process), size = 0.75) +
  osw_color +
  bottom1 + bottom2

cost80_facetem_line_bw <- cost80_facetem +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom1 + bottom2

cost80_facetem_fill_col <- cost80_facetem +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill +
  bottom1 + bottom2

cost80_facetem_fill_bw <- cost80_facetem +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom1 + bottom2

## ~ Heat Maps ----

elc_long_heatmaps <- elc_long %>% 
  filter(Year == "2050") %>%
  filter(costred != "20" & costred != "40")

osw <- elc_long_heatmaps %>% filter(Process == "Offshore Wind")
wnd <- elc_long_heatmaps %>% filter(Process == "Terrestrial Wind")
sol <- elc_long_heatmaps %>% filter(Year == "2050" & Process == "Solar" & costred != "20")
coal <- elc_long_heatmaps %>% filter(Process == "Coal")
nga <- elc_long_heatmaps %>% filter(Process == "Natural Gas")
nuk <- elc_long_heatmaps %>% filter(Process == "Nuclear")
hyd <- elc_long_heatmaps %>% filter(Process == "Hydro")
ccs <- elc_long_heatmaps %>% filter(Process == "Coal CCS") %>%
  replace_with_na(replace = list(VAR_FOut = 0))

osw_grid_heatmap_col <- grid.heatmap.col(osw, "Offshore Wind Electricity Output: 2050")
wnd_grid_heatmap_col <- grid.heatmap.col(wnd, "Terrestrial Wind Electricity Output: 2050")
sol_grid_heatmap_col <- grid.heatmap.col(sol, "Solar Electricity Output: 2050")
coal_grid_heatmap_col <- grid.heatmap.col(coal, "Coal Electricity Output: 2050")
nga_grid_heatmap_col <- grid.heatmap.col(nga, "Natural Gas Electricity Output: 2050")
nuk_grid_heatmap_col <- grid.heatmap.col(nuk, "Nuclear Electricity Output: 2050")
hyd_grid_heatmap_col <- grid.heatmap.col(hyd, "Hydropower Electricity Output: 2050")
ccs_grid_heatmap_col <- grid.heatmap.col(ccs, "Coal with CCS Electricity Output: 2050")

osw_grid_heatmap_bw <- grid.heatmap.bw(osw, "Offshore Wind Electricity Output: 2050")
wnd_grid_heatmap_bw <- grid.heatmap.bw(wnd, "Terrestrial Wind Electricity Output: 2050")
sol_grid_heatmap_bw <- grid.heatmap.bw(sol, "Solar Electricity Output: 2050")
coal_grid_heatmap_bw <- grid.heatmap.bw(coal, "Coal Electricity Output: 2050")
nga_grid_heatmap_bw <- grid.heatmap.bw(nga, "Natural Gas Electricity Output: 2050")
nuk_grid_heatmap_bw <- grid.heatmap.bw(nuk, "Nuclear Electricity Output: 2050")
hyd_grid_heatmap_bw <- grid.heatmap.bw(hyd, "Hydropower Electricity Output: 2050")
ccs_grid_heatmap_bw <- grid.heatmap.bw(ccs, "Coal with CCS Retrofits Electricity Output: 2050")

heatmap_col <- grid.heatmap.col(elc_long, "Grid Mix Production by Process")
heatmap_bw <- grid.heatmap.bw(elc_long, "Grid Mix Production by Process")

## ~ Retirements and Additions----

# baseline 

base_retire <- retire_long %>%
  filter(emred == "BAU" & costred == "20") %>%
  filter(Process != "Other") %>%
  ggplot() +
  labs(x = "Year", y = "Change (PJ)",
       title = "BAU Electricity Production Yearly Changes") +
  yt

base_retire_fill_col <- base_retire + 
  geom_bar(aes(x = Year, y = retire, fill = Process), 
           stat = "identity", position = "stack", width = 0.9) +
  osw_fill +
  zero

base_retire_fill_bw <- base_retire + 
  geom_bar(aes(x = Year, y = retire, fill = Process), 
           stat = "identity", position = "stack", width = 0.9, colour = "black") +
  gray_fill +
  zero

# diff summary 

prod_dif_all_col <- prod.dif.col(prod_dif,"Changes in Grid Mix over Baseline")

prod_dif_all_bw <- prod.dif.bw(prod_dif,"Changes in Grid Mix over Baseline") 

prod_dif_col <- prod.dif.col(
  (prod_dif %>% 
     filter(costred %in% c("40", "50", "70")) %>% 
     filter(emred %in% c("BAU", "40", "70"))),
  "Changes in Grid Mix over Baseline")

prod_dif_bw <- prod.dif.bw(
  (prod_dif %>% 
     filter(costred %in% c("40", "50", "70")) %>% 
     filter(emred %in% c("BAU", "40", "70"))),
  "Changes in Grid Mix over Baseline") 

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

## ~ New Capacity by Process Set----

test <- newcap_total_diff %>% filter(emred == "40")

testplot <- ggplot(data = newcap_total_diff) +
  geom_bar(aes(x = 1, y = diff, fill = Process, group = Scenario), stat = "identity") +
  facet_grid(costred~emred) +
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
  bottom1 + bottom2

emis_bau_fill_col <- emis_bau +
  geom_bar(stat = "identity", position = "stack", aes(fill = Commodity)) +
  commodity_fill

emis_bau_line_col <- emis_bau +
  geom_line(aes(group = Commodity, color = Commodity)) +
  commodity_color

emis_bau_fill_bw <- emis_bau +
  geom_bar(stat = "identity", position = "stack", color = "black", aes(fill = Commodity)) +
  gray_fill

emis_bau_line_bw <- emis_bau +
  geom_line(aes(group = Commodity, linetype = Commodity))
  
emis_plot <- testemissions %>% filter(!costred %in% c("40", "30", "20")) %>%
  ggplot(aes(x = Year, y= Emissions)) +
  labs(x = "Year", y = "Emissions (kt)*",
       title = "Electric Sector Emissions Output",
       subtitle = "*Units are Mt for CO2",
       linetype = "Emissions Reduction (%)",
       color = "Cost Reduction (%)") +
  facet_wrap(~Commodity, scales = "free_y", nrow = 1, labeller = label_parsed) +
  yt +
  #x_disc_l +
  bottom1 + bottom2 +
  theme(legend.box = "vertical") +
  guides(colour = guide_legend(nrow = 1))

emis_col <- emis_plot +
  geom_line(aes(x=Year, y=Emissions, color = costred, group = Scenario, linetype = emred), size = 1) +
  costosw_color

emis_bw <- emis_plot +
  geom_line(aes(x=Year, y=Emissions, color = costred, group = Scenario, linetype = emred)) +
  gray_color

emis_perc_plot <- emissions_percent %>% filter(!costred %in% c("40", "30", "20")) %>%
  ggplot(aes(x = Year, y= percent.red)) +
  labs(x = "Year", y = "Percent Reduction from 2010",
       title = "Electric Sector Emissions Output",
       linetype = "Emissions Reduction (%)",
       color = "Cost Reduction (%)") +
  facet_wrap(~Commodity, nrow = 1, labeller = label_parsed) +
  yt +
  x_disc_l +
  bottom1 + bottom2 +
  theme(legend.box = "vertical") +
  guides(colour = guide_legend(nrow = 1))

emis_perc_col <- emis_perc_plot +
  geom_line(aes(x=Year, y=percent.red, color = costred, group = Scenario, linetype = emred), size = 1) +
  costosw_color

emis_perc_bw <- emis_perc_plot +
  geom_line(aes(x=Year, y=percent.red, color = costred, group = Scenario, linetype = emred)) +
  gray_color

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


ggplot() +
  geom_line(data = emissions_long1 %>% 
              filter(Commodity == "CO[2]") %>%
              filter(!costred %in% c("20","30","40")),
            aes(x = Year, y = Emissions, group = Scenario), color = "black") +
  geom_bar(data = osw_varcap_long1, stat = "identity",
            aes(x = Year, y = VAR_Cap*3), alpha = 0.5) +
  scale_y_continuous(sec.axis = sec_axis(~./3, name = "Offshore Wind Capacity (GW)")) +
  facet_grid(emred~costred, 
             labeller = labeller(emred = label_wrap_gen(width = 10),
                                 costred = costlabels)) +
  yt +
  x_disc_l

ggplot() +
  geom_line(data = emissions_long1 %>% 
              filter(Commodity != "PM[2.5]" & Commodity != "CH[4]") %>%
              filter(!costred %in% c("20","30","40")),
            aes(x = Year, y = Emissions, group = Commodity, color = Commodity)) +
  geom_bar(data = osw_varcap_long1, stat = "identity",
           aes(x = Year, y = VAR_Cap*3), alpha = 0.5) +
  scale_y_continuous(sec.axis = sec_axis(~./3, name = "Offshore Wind Capacity (GW)")) +
  facet_grid(emred~costred, 
             labeller = labeller(emred = label_wrap_gen(width = 10),
                                 costred = costlabels)) +
  yt +
  x_disc_l +
  bottom1 +
  bottom2 +
  commodity_color

ggplot() +
  geom_line(data = emissions_long1 %>% 
              filter(Commodity == "PM[2.5]" | Commodity == "CH[4]") %>%
              filter(!costred %in% c("20","30","40")),
            aes(x = Year, y = Emissions, group = Commodity, color = Commodity)) +
  geom_bar(data = osw_varcap_long1, stat = "identity",
           aes(x = Year, y = VAR_Cap*.25), alpha = 0.5) +
  scale_y_continuous(sec.axis = sec_axis(~./.25, name = "Offshore Wind Capacity (GW)")) +
  facet_grid(emred~costred, scales = "free_y",
             labeller = labeller(emred = label_wrap_gen(width = 10),
                                 costred = costlabels)) +
  yt +
  x_disc_l +
  bottom1 +
  bottom2 +
  commodity_color





ggplot() +
  geom_line(data = emissions_long %>% filter(Commodity == "CO[2]" &
                                               emred == "50" &
                                               costred != "20" &
                                               costred != "30"),
            aes(x = Year, y = Emissions, group = Scenario), color = "black") +
  geom_line(data = osw_varcap_long %>% filter(emred == "50" &
                                                costred != "20" &
                                                costred != "30"),
            aes(x = Year, y = VAR_Cap*3, group = Scenario),
            color = "deepskyblue4") +
  scale_y_continuous(sec.axis = sec_axis(~./3, name = "Offshore Wind Capacity (GW)")) +
  facet_grid(costred~emred, labeller = labeller(emred = emissionlabels, costred = costlabels))

ggplot() +
  geom_line(data = emissions_long %>% filter(emred == "20" | emred == "50" | emred == "80") %>%
              filter(costred == "50" | costred == "70") %>%
              filter(Commodity != "CH4" | Commodity != "PM 2.5"),
            aes(x = Year, y = Emissions, group = Commodity, color = Commodity)) +
  geom_line(data = osw_varcap_long %>% filter(emred == "20" | emred == "50" | emred == "80") %>%
              filter(costred == "50" | costred == "70"),
            aes(x = Year, y = VAR_Cap*3, group = Scenario, color = "Offshore Wind")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7)) +
  scale_color_manual(breaks = c("CO2", "NOx", "SO2", "Offshore Wind"),
                     values = c("chartreuse4", "seashell4", "deepskyblue4", "darkgoldenrod3", "black", "blue")) +
  scale_y_continuous(sec.axis = sec_axis(~./3, name = "Offshore Wind Capacity (GW)")) +
  facet_grid(costred~emred, labeller = label_both) +
  labs(y = "Emissions*", caption = "*Emissions units are kt for NOx and SO2 and Mt for CO2")



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


## Total Electricity Production ----

elcdata <- elctotal_long %>% filter(!costred %in% c("20", "30", "40"))

elctotal_line <- ggplot(elcdata) +
  facet_grid(~costred) +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Total Electricity Production by Scenario",
       color = "Emissions Reduction (%)",
       linetype = "Emissions Reduction (%)") +
  yt +
  bottom1 + bottom2

elctotal_line_col <- elctotal_line +
  geom_line(aes(x = Year, y = VAR_FOut, color = emred, group = Scenario)) +
  em_color

elctotal_line_bw <- elctotal_line +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = emred, group = Scenario))

elctotal_heat <- ggplot(elcdata, aes(x = costred, y = emred)) +
  geom_tile(aes(fill = VAR_FOut), colour = "gray", size = 0.25) +
  labs(x = "Offshore Wind Cost Reductions (%)",
       y = "Emissions Reduction (%)",
       title = "Total Electricity Production: 2050",
       fill = "Electricity\nProduced\n(PJ)")

elctotal_heat_col <- elctotal_heat + col_fill_cont
  
elctotal_heat_bw <- elctotal_heat + gray_fill_cont

## End Uses ----

## ~ Timelines ----

enduse %>% filter(costred == "80" & emred == "40") %>%
  ggplot(aes(x = Year, y = Consumption, fill = Sector)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = col_sector) +
  labs(x = "Year", y = "Electricity Consumption (PJ)",
       title = "Electricity Consumption by Sector:\n40% Emissions Reduction & 80% Cost Reduction") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

enduse %>% filter(costred == "80" & emred == "40") %>%
  ggplot(aes(x = Year, y = Consumption, color = Sector)) +
  geom_line(aes(group = Sector)) +
  scale_color_manual(values = col_sector) +
  labs(x = "Year", y = "Electricity Consumption (PJ)",
       title = "Electricity Consumption by Sector:\n40% Emissions Reduction & 80% Cost Reduction") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

enduse %>% filter(costred == "80" & emred == "40") %>%
  ggplot(aes(x = Year, y = Consumption, fill = Sector)) +
  geom_bar(stat = "identity", position = "stack", colour = "black") +
  gray_fill +
  yt +
  labs(x = "Year", y = "Electricity Consumption (PJ)",
       title = "Electricity Consumption by Sector:\n40% Emissions Reduction & 80% Cost Reduction") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

enduse %>%
  filter(costred == "40") %>%
  ggplot(aes(x = Year, y = Consumption, fill = Sector)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = col_sector) +
  facet_wrap(~emred) +
  labs(x = "Year", y = "Electricity Consumption (PJ)",
       title = "Electricity Consumption by Sector: 40% Cost Reduction ") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

enduse %>%
  ggplot(aes(x = Year, y = Consumption, fill = Sector)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = col_sector) +
  facet_grid(costred~emred) +
  labs(x = "Year", y = "Electricity Consumption (PJ)",
       title = "Electricity Consumption by Sector: 40% Cost Reduction ") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

enduse %>%
  filter(Sector == "Transportation") %>% filter(costred != "20") %>%
  ggplot(aes(x = Year, y = Consumption, fill = Sector)) +
  geom_bar(stat = "identity", position = "stack", fill = "darkgoldenrod2") +
  facet_grid(costred~emred) +
  labs(x = "Year", y = "Electricity Consumption (PJ)",
       title = "Transportation Electricity Consumption: 40% Cost Reduction") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

enduse %>%
  filter(Sector == "Industrial") %>% filter(costred != "20") %>%
  ggplot(aes(x = Year, y = Consumption, fill = Sector)) +
  geom_bar(stat = "identity", position = "stack", fill = "darkgoldenrod2") +
  facet_grid(costred~emred) +
  labs(x = "Year", y = "Electricity Consumption (PJ)",
       title = "Industrial Electricity Consumption: 40% Cost Reduction") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

enduse %>%
  filter(Sector == "Residential") %>% filter(costred != "20") %>%
  ggplot(aes(x = Year, y = Consumption, fill = Sector)) +
  geom_bar(stat = "identity", position = "stack", fill = "darkgoldenrod2") +
  facet_grid(costred~emred) +
  labs(x = "Year", y = "Electricity Consumption (PJ)",
       title = "Residential Electricity Consumption: 40% Cost Reduction") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

enduse %>%
  filter(Sector == "Commercial") %>% filter(costred != "20") %>%
  ggplot(aes(x = Year, y = Consumption, fill = Sector)) +
  geom_bar(stat = "identity", position = "stack", fill = "darkgoldenrod2") +
  facet_grid(costred~emred) +
  labs(x = "Year", y = "Electricity Consumption (PJ)",
       title = "Commercial Electricity Consumption: 40% Cost Reduction") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

enduse %>%
  filter(costred == "40") %>%
  filter(Sector == "Industrial") %>%
  ggplot(aes(x = Year, y = Consumption, fill = Sector)) +
  geom_bar(stat = "identity", position = "stack", fill = "firebrick") +
  facet_wrap(~emred) +
  labs(x = "Year", y = "Electricity Consumption (PJ)",
       title = "Industrial Electricity Consumption: 40% Cost Reduction") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

enduse %>%
  filter(costred == "80") %>%
  filter(Sector == "Industrial") %>%
  ggplot(aes(x = Year, y = Consumption, fill = Sector)) +
  geom_bar(stat = "identity", position = "stack", fill = "firebrick") +
  facet_wrap(~emred) +
  labs(x = "Year", y = "Electricity Consumption (PJ)",
       title = "Industrial Electricity Consumption: 80% Cost Reduction") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

## ~ Heatmaps ----

enduse %>% filter(Year == "2050") %>% filter(Sector == "Transportation") %>%
  ggplot(aes(x = costred, y = emred, fill = Consumption)) +
  geom_tile(colour = "gray", size = 0.25) +
  labs(x = "Offshore Wind Cost Reductions (%)",
       y = "Emissions Reduction (%)",
       title = "Transportation Sector Electricity Consumption: 2050",
       fill = "Electricity\nConsumed\n(PJ)") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9))

## Correlations ----

## ~ OSW Only Scenarios (28)

correlations <- sapply(oswcor, cor.test, method="spearman", exact = F, y = oswcor$`cap2050`)
correlations.table <- as.data.frame(correlations)
chart.Correlation(oswcor, histogram = FALSE, method = "spearman")

summary(lm(cap2050~emred+costred+`CO[2]`+`SO[2]`+`CH[4]`+`PM[2.5]`+`NO[X]`+`Total Elc`,
          data = oswcor))

cap2050.fit <- lm(`cap2050`~emred+costred, data = oswcor)
cap2050.fit.sum <- summary(cap2050.fit)

totalelc.fit <- lm(`Total Elc`~emred+costred+cap2050, data = oswcor)
totalelc.fit.sum <- summary(totalelc.fit)

rps.fit <- lm(`perRenew`~emred+costred, data = oswcor)
rps.fit.sum <- summary(rps.fit)

co2.fit <- lm(`CO[2]`~emred+cap2050+`Total Elc`, data = oswcor)
co2.fit.sum <- summary(co2.fit)

so2.fit <- lm(`SO[2]`~emred+cap2050+`Total Elc`, data = oswcor)
so2.fit.sum <- summary(so2.fit)

nox.fit <- lm(`NO[X]`~emred+cap2050+`Total Elc`, data = oswcor)
nox.fit.sum <- summary(nox.fit)

pm2.5.fit <- lm(`PM[2.5]`~emred+cap2050+`Total Elc`, data = oswcor)
pm2.5.fit.sum <- summary(pm2.5.fit)

ch4.fit <- lm(`CH[4]`~emred+cap2050+`Total Elc`, data = oswcor)
ch4.fit.sum <- summary(ch4.fit)

gridcoef_names <- c("CO2 Cap" = "emred", "Cost Reduction" = "costred", "OSW Capacity" = "cap2050")
gridmodel_names <- c("cap2050.fit" = "OSW Capacity", "totalelc.fit" = "Total Elc")
emissioncoef_names <- c("CO2 Cap" = "emred", "OSW Capacity" = "cap2050", "Total Elc" = "`Total Elc`")
emissionmodel_names <- c("co2.fit" = expression(CO[2]), "so2.fit" = expression(SO[2]),
                     "nox.fit" = expression(NO[X]), "pm2.5.fit" = expression(PM[2.5]), 
                     "ch4.fit" = expression(CH[4]))

grid.modeltable <- export_summs(cap2050.fit, totalelc.fit,
                            scale = TRUE, coefs = gridcoef_names, model.names = gridmodel_names)
emission.modeltable <- export_summs(co2.fit, so2.fit, nox.fit, 
                                    ch4.fit, pm2.5.fit,
                                scale = TRUE, coefs = emissioncoef_names, model.names = emissionmodel_names)

### STILL NEED TO CHECK FOR HETEROSKEDASTICITY


## ~ All Scenarios (42)

all.correlations <- sapply(allcor, cor.test, method="spearman", exact = F, y = allcor$`cap2050`)
all.correlations.table <- as.data.frame(all.correlations)
chart.Correlation(allcor, histogram = FALSE, method = "spearman")






## Save Plots ----

# plots <-

# lapply(plots,function(x){ggsave(file=paste(x,"pdf",sep="."),get(x))})
# https://stackoverflow.com/questions/20500706/saving-multiple-ggplots-from-ls-into-one-and-separate-files-in-r
