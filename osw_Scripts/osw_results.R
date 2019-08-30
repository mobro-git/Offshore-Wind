## Scenarios ----

## ~ Cost ----

costscen_plot <- ggplot(scenario_cost) +
  geom_label(data = . %>% filter(Year == last(Year)),
             aes(label = paste(CostCurve,"%"), x = Year, y = Costs), 
             size = 3, alpha = 0.5, nudge_y = 4) +
  scale_y_continuous(limits = c(0, 100), expand = c(.01, 0)) +
  labs(x = "Year", y = "Cost Reduction (%)", 
       title = "OSW Cost Curves", 
       subtitle = "Percentage decrease from 2010 offshore wind CAPEX costs") +
  yt +
  x_cont +
  nolegend
  
costscen_col <- costscen_plot +
  geom_line(aes(x = Year, y = Costs, color = CostCurve, group = CostCurve), size = .75) +
  cost_color

costscen_bw <- costscen_plot +
  geom_line(aes(x = Year, y = Costs, linetype = CostCurve, group = CostCurve), size = .75)


## ~ Emissions ----

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
  geom_line(aes(x = Year, y = Emissions, color = Cap, group = Cap), size = .75) + 
  em_color 

emissionsscen_bw <- emissionsscen_plot +
  geom_line(aes(x = Year, y = Emissions, linetype = Cap, group = Cap), size = .75)

## LCOE ----

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

cap_plot <- ggplot(osw_varcap_long) +
  labs(x = "Year", y = "Total Installed Capacity (GW)",
       title = "Offshore Wind Capacity",
       color = "Emissions Reduction (%)",
       linetype = "Emissions Reduction (%)") + 
  facet_grid(~costred) +
  yt +
  bottom +
  x_disc
  
cap_col_side <- cap_plot + 
  geom_line(aes(x=Year, y=VAR_Cap, color = emred, group = Scenario)) + 
  em_color
   
cap_bw_side <- cap_plot +
  geom_line(aes(x=Year, y=VAR_Cap, linetype = emred, group = Scenario)) 

cap_gray_side <- cap_plot + 
  geom_line(aes(x=Year, y=VAR_Cap, color = emred, group = Scenario)) + 
  gray_color

cap_heatmap <- osw_varcap_long %>% filter(Year == "2050") %>%
  ggplot(aes(x = costred, y = emred)) +
  geom_tile(aes(fill = VAR_Cap), colour = "gray") +
  geom_text(aes(label = VAR_Cap), color = "white") +
  labs(x = "Cost Reduction (%)", y = "Emissions Reduction (%)",
       title = "2050 Offshore Wind Capacity",
       fill = "Capacity (GW)") +
  st +
  bottom
  
cap_col_heat <- cap_heatmap 

cap_bw_heat <- cap_heatmap + gray_fill_cont 

## ~ New Capacity ----

newcap_plot <- ggplot(osw_varncap_long) +
  labs(x = "Year", y = "New Installed Capacity (GW)",
       title = "Offshore Wind New Capacity",
       color = "Emissions Reduction (%)",
       linetype = "Emissions Reduction (%)") + 
  facet_grid(costred~., scales = "free_y") +
  yt +
  bottom +
  x_disc

newcap_col_top <- newcap_plot +
  geom_line(aes(x=Year, y=VAR_Ncap, color = emred, group = Scenario)) + 
  em_color

newcap_bw_top <- newcap_plot +
  geom_line(aes(x=Year, y=VAR_Ncap, linetype = emred, group = Scenario))

newcap_gray_top <- newcap_plot +
  geom_line(aes(x=Year, y=VAR_Ncap, color = emred, group = Scenario)) + 
  gray_color

## ~ Production ----

output_plot <- ggplot(osw_varfout_long) + 
  labs(x = "Year", y = "Offshore Wind Output (PJ)",
       title = "Offshore Wind Output",
       color = "Emissions Reduction (%)",
       linetype = "Emissions Reduction (%)") +
  facet_grid(~costred) +
  yt +
  bottom +
  x_disc

output_col_side <- output_plot+
  geom_line(aes(x=Year, y=VAR_FOut, color = emred, group = Scenario)) +
  em_color

output_bw_side <- output_plot + 
  geom_line(aes(x=Year, y=VAR_FOut, linetype = emred, group = Scenario))

output_gray_side <- output_plot+
  geom_line(aes(x=Year, y=VAR_FOut, color = emred, group = Scenario)) +
  gray_color

## ~ Regions ----

cap.reg.col.list <- list(data = osw_varcap_long_reg, yvar = "VAR_Cap", colvar = "emred",
                          coltheme = em_color, title = "Offshore Wind Total Capacity", 
                          ylab = "Capacity (GW)")

cap_reg1_col <- do.call(lineplot.region, c(cap.reg.col.list,list(region = "R1")))
cap_reg2_col <- do.call(lineplot.region, c(cap.reg.col.list,list(region = "R2")))
cap_reg3_col <- do.call(lineplot.region, c(cap.reg.col.list,list(region = "R3")))
cap_reg4_col <- do.call(lineplot.region, c(cap.reg.col.list,list(region = "R4")))
cap_reg5_col <- do.call(lineplot.region, c(cap.reg.col.list,list(region = "R5")))
cap_reg6_col <- do.call(lineplot.region, c(cap.reg.col.list,list(region = "R6")))
cap_reg7_col <- do.call(lineplot.region, c(cap.reg.col.list,list(region = "R7")))
cap_reg9_col <- do.call(lineplot.region, c(cap.reg.col.list,list(region = "R9")))

cap_allreg_col <- do.call(lineplot.region, cap.reg.col.list)
cap_allreg_col_free <- do.call(lineplot.region, c(cap.reg.col.list,list(scale="free_y"))) +
  theme(axis.text.y = element_text(face = "bold")) +
  labs(caption = "*Capacity scales are not fixed")

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

newcap_allreg_col <- do.call(lineplot.region, ncap.reg.col.list)
newcap_allreg_col_free <- do.call(lineplot.region, c(ncap.reg.col.list,list(scale="free_y"))) +
  theme(axis.text.y = element_text(face = "bold")) +
  labs(caption = "*Capacity scales are not fixed")

cap.reg.bw.list <- list(data = osw_varcap_long_reg, yvar = "VAR_Cap", typevar = "emred",
                         title = "Offshore Wind Total Capacity", ylab = "Capacity (GW)")

cap_reg1_bw <- do.call(lineplot.region, c(cap.reg.bw.list,list(region = "R1")))
cap_reg2_bw <- do.call(lineplot.region, c(cap.reg.bw.list,list(region = "R2")))
cap_reg3_bw <- do.call(lineplot.region, c(cap.reg.bw.list,list(region = "R3")))
cap_reg4_bw <- do.call(lineplot.region, c(cap.reg.bw.list,list(region = "R4")))
cap_reg5_bw <- do.call(lineplot.region, c(cap.reg.bw.list,list(region = "R5")))
cap_reg6_bw <- do.call(lineplot.region, c(cap.reg.bw.list,list(region = "R6")))
cap_reg7_bw <- do.call(lineplot.region, c(cap.reg.bw.list,list(region = "R7")))
cap_reg9_bw <- do.call(lineplot.region, c(cap.reg.bw.list,list(region = "R9")))

cap_allreg_bw <- do.call(lineplot.region, cap.reg.bw.list)
cap_allreg_bw_free <- do.call(lineplot.region, c(cap.reg.bw.list,list(scale="free_y"))) +
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

newcap_allreg_bw <- do.call(lineplot.region, ncap.reg.bw.list)
newcap_allreg_bw_free <- do.call(lineplot.region, c(ncap.reg.bw.list,list(scale="free_y"))) +
  theme(axis.text.y = element_text(face = "bold")) +
  labs(caption = "*Capacity scales are not fixed")


## ~ Maps ----
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
    abbr %in% r1 ~ 29.9,
    abbr %in% r2 ~ 73.2,
    abbr %in% r3 ~ 110.4,
    abbr %in% r4 ~ 0.7,
    abbr %in% r5 ~ 169.7,
    abbr %in% r6 ~ 9.6,
    abbr %in% r7 ~ 97.0,
    abbr %in% r8 ~ 0.0,
    abbr %in% r9 ~ 93.4
  ))

r8map <- subset(state_map, abbr %in% r8)

regOSW_map_col <- ggplot() + 
  geom_polygon(data = state_map, aes(x = long, y = lat, group = group, fill = avgOSW),
               color = "white") +
  geom_polygon(data = r8map, aes(x = long, y = lat, group = group),
               color = "gray", fill = "white") +
  geom_label(data = reg.names, aes(x = long, y = lat, label = region), size = 4) +
  coord_fixed(1.2) +
  labs(title = "Average Offshore Wind Capacity",
       fill = "GW") +
  theme_bw() +
  scale_fill_gradient(low = "#A0B7F5", high = "#062B91") +
  noaxes

regOSW_map_bw <- regOSW_map_col + gray_fill_cont


## ~ Tables ----

cap_region_table <- osw_varcap_regiontotals %>% 
  kable(booktabs = T, caption = "Average Installed Capacity (GW)", linesep = "") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))

output_region_table <- osw_varfout_regiontotals %>% 
  kable(booktabs = T, caption = "Average Electricity Output (PJ)", linesep = "") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))

cap_2050_table <- osw_varcap_2050total %>% 
  kable(booktabs = T, caption = "Offshore Wind Total Installed Capacity (GW): 2050", 
        digits = 1, linesep = "") %>% 
  kable_styling(latex_options = c("striped", "hold_position")) %>% 
  add_header_above(c("Emissions\nReduction (%)", "Cost Reduction (%)" = 5)) 

output_2050_table <- osw_varfout_2050total %>% 
  kable(booktabs = T, caption = "Offshore Wind Total Output (PJ): 2050", 
        digits = 1, linesep = "") %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>% 
  add_header_above(c("Emissions\nReduction (%)", "Cost Reduction (%)" = 5)) 

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
  bottom

baseprod_fill_bw <- baseprod +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom

## ~ Overall ----

gridmix_all <- elc_long %>%
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Process") +
  facet_grid(emred~costred) +
  yt +
  x_disc
  
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
  x_disc

gridmix_col <- gridmix +
  geom_line(aes(x = Year, y = VAR_FOut, color = Process, group = Process)) +
  osw_color

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
  bottom

bau_facetcost_line_bw <- bau_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom

bau_facetcost_fill_col <- bau_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill + 
  bottom

bau_facetcost_fill_bw <- bau_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom

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
  bottom

em30_facetcost_line_bw <- em30_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom

em30_facetcost_fill_col <- em30_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill +
  bottom

em30_facetcost_fill_bw <- em30_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom

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
  bottom

em40_facetcost_line_bw <- em40_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom

em40_facetcost_fill_col <- em40_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill +
  bottom

em40_facetcost_fill_bw <- em40_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom

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
  bottom

em50_facetcost_line_bw <- em50_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom

em50_facetcost_fill_col <- em50_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill +
  bottom

em50_facetcost_fill_bw <- em50_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom

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
  bottom

em60_facetcost_line_bw <- em60_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom

em60_facetcost_fill_col <- em60_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill +
  bottom

em60_facetcost_fill_bw <- em60_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom

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
  bottom

em70_facetcost_line_bw <- em70_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom 

em70_facetcost_fill_col <- em70_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill +
  bottom

em70_facetcost_fill_bw <- em70_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom

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
  bottom

em80_facetcost_line_bw <- em80_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom

em80_facetcost_fill_col <- em80_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill +
  bottom

em80_facetcost_fill_bw <- em80_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom

## ~ Cost Reductions ----

cost40 <- elc_long %>% filter(costred == "40" & !emred %in% c("30", "80"))
cost50 <- elc_long %>% filter(costred == "50" & !emred %in% c("30", "80"))
cost60 <- elc_long %>% filter(costred == "60" & !emred %in% c("30", "80"))
cost70 <- elc_long %>% filter(costred == "70" & !emred %in% c("30", "80"))
cost80 <- elc_long %>% filter(costred == "80" & !emred %in% c("30", "80"))

cost40_facetem <-  cost40 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Process",
       subtitle = "40% Cost Reductions") +
  facet_grid(.~emred) +
  yt +
  x_disc

cost40_facetem_line_col <- cost40_facetem +
  geom_line(aes(x = Year, y = VAR_FOut, color = Process, group = Process), size = 0.75) +
  osw_color +
  bottom

cost40_facetem_line_bw <- cost40_facetem +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom

cost40_facetem_fill_col <- cost40_facetem +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill +
  bottom

cost40_facetem_fill_bw <- cost40_facetem +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom

cost50_facetem <-  cost50 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Process",
       subtitle = "50% Cost Reductions") +
  facet_grid(.~emred) +
  yt +
  x_disc

cost50_facetem_line_col <- cost50_facetem +
  geom_line(aes(x = Year, y = VAR_FOut, color = Process, group = Process), size = 0.75) +
  osw_color +
  bottom

cost50_facetem_line_bw <- cost50_facetem +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom

cost50_facetem_fill_col <- cost50_facetem +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill +
  bottom

cost50_facetem_fill_bw <- cost50_facetem +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom

cost60_facetem <-  cost60 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Process",
       subtitle = "60% Cost Reductions") +
  facet_grid(.~emred) +
  yt +
  x_disc

cost60_facetem_line_col <- cost60_facetem +
  geom_line(aes(x = Year, y = VAR_FOut, color = Process, group = Process), size = 0.75) +
  osw_color +
  bottom

cost60_facetem_line_bw <- cost60_facetem +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom

cost60_facetem_fill_col <- cost60_facetem +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill +
  bottom

cost60_facetem_fill_bw <- cost60_facetem +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom

cost70_facetem <-  cost70 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Process",
       subtitle = "70% Cost Reductions") +
  facet_grid(.~emred) +
  yt +
  x_disc

cost70_facetem_line_col <- cost70_facetem +
  geom_line(aes(x = Year, y = VAR_FOut, color = Process, group = Process), size = 0.75) +
  osw_color +
  bottom

cost70_facetem_line_bw <- cost70_facetem +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom

cost70_facetem_fill_col <- cost70_facetem +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill +
  bottom

cost70_facetem_fill_bw <- cost70_facetem +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom

cost80_facetem <-  cost80 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Process",
       subtitle = "80% Cost Reductions") +
  facet_grid(.~emred) +
  yt +
  x_disc

cost80_facetem_line_col <- cost80_facetem +
  geom_line(aes(x = Year, y = VAR_FOut, color = Process, group = Process), size = 0.75) +
  osw_color +
  bottom

cost80_facetem_line_bw <- cost80_facetem +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom

cost80_facetem_fill_col <- cost80_facetem +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill +
  bottom

cost80_facetem_fill_bw <- cost80_facetem +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom

## ~ Heat Maps ----

osw <- elc_long %>% filter(Year == "2050" & Process == "Offshore Wind" & costred != "20")
wnd <- elc_long %>% filter(Year == "2050" & Process == "Terrestrial Wind" & costred != "20")
sol <- elc_long %>% filter(Year == "2050" & Process == "Solar" & costred != "20")
coal <- elc_long %>% filter(Year == "2050" & Process == "Coal" & costred != "20")
nga <- elc_long %>% filter(Year == "2050" & Process == "Natural Gas" & costred != "20")
nuk <- elc_long %>% filter(Year == "2050" & Process == "Nuclear" & costred != "20")
hyd <- elc_long %>% filter(Year == "2050" & Process == "Hydro" & costred != "20")
ccs <- elc_long %>% filter(Year == "2050" & Process == "Coal CCS" & costred != "20") %>%
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

test <- newcap_total %>% filter(emred == "40")

testplot <- ggplot(data = newcap_total) +
  geom_bar(aes(x = 1, y = Ncap, fill = Process, group = Scenario), stat = "identity") +
  facet_grid(costred~emred) +
  osw_fill +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


## Emissions ----

## ~ Timelines ----

emis_bau <- ggplot(data = emissions_bau, aes(x = Year, y = Emissions, fill = Commodity)) +
  labs(y = "Emissions (kt)*",
       title = "Baseline Electric Sector Emissions",
       caption = "*Units are Mt for CO2 and kt for all other emissions") +
  yt +
  bottom

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
  
emis_plot <- emissions_long %>% filter(!costred %in% c("30", "20")) %>%
  ggplot(aes(x = Year, y= Emissions)) +
  labs(x = "Year", y = "Emissions*",
       title = "Electric Sector Emissions Output",
       caption = "*Units are Mt for CO2",
       linetype = "Emissions Reduction (%)",
       color = "Cost Reduction (%)") +
  facet_wrap(~Commodity, scales = "free_y") +
  yt +
  x_disc +
  bottom +
  theme(legend.box = "vertical") +
  guides(colour = guide_legend(nrow = 1))

emis_col <- emis_plot +
  geom_line(aes(x=Year, y=Emissions, color = costred, group = Scenario, linetype = emred)) +
  costosw_color

emis_bw <- emis_plot +
  geom_line(aes(x=Year, y=Emissions, color = costred, group = Scenario, linetype = emred)) +
  gray_color

emissions_long %>% filter(Commodity == "CO2") %>%
  ggplot(aes(x = Year, y = Emissions)) +
  geom_line(aes(group = Scenario, color = costred)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7)) +
  facet_wrap(~emred, labeller = labeller(emred = elab, costred = clab)) +
  scale_color_brewer(palette = "Blues") +
  labs(x = "Year", y = "Emissions",
       title = "Electric Sector CO2 Emissions",
       color = "Cost\nReduction\n(%)") +
  scale_x_discrete(breaks = seq(2015,2050, by = 5))

ggplot() +
  geom_line(data = emissions_long %>% filter(Commodity == "CO2" &
                                               emred == "50" &
                                               costred != "20" &
                                               costred != "30"),
            aes(x = Year, y = Emissions, group = Scenario), color = "gray50") +
  geom_line(data = osw_varcap_long %>% filter(emred == "50" &
                                                costred != "20" &
                                                costred != "30"),
            aes(x = Year, y = VAR_Cap*3, group = Scenario),
            color = "deepskyblue4") +
  scale_y_continuous(sec.axis = sec_axis(~./3, name = "Offshore Wind Capacity (GW)")) +
  facet_grid(costred~emred, labeller = labeller(emred = elab, costred = clab))

ggplot() +
  geom_line(data = emissions_long %>% filter(emred == "20" | emred == "50" | emred == "80") %>%
              filter(costred != "20" & costred != "30") %>%
              filter(Commodity != "CH4" &  Commodity != "PM 2.5"),
            aes(x = Year, y = Emissions, group = Commodity, color = Commodity)) +
  geom_line(data = osw_varcap_long %>% filter(emred == "20" |
                                                emred == "50" |
                                                emred == "80" &
                                                costred != "20" &
                                                costred != "30"),
            aes(x = Year, y = VAR_Cap*3, group = Scenario, color = "Offshore Wind")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7)) +
  scale_color_manual(breaks = c("CO2", "NOx", "SO2", "Offshore Wind"),
                     values = c("chartreuse4", "seashell4", "deepskyblue4", "darkgoldenrod3")) +
  scale_y_continuous(sec.axis = sec_axis(~./3, name = "Offshore Wind Capacity (GW)")) +
  facet_grid(costred~emred, labeller = label_both) +
  labs(y = "Emissions*", caption = "*Emissions units are kt for NOx and SO2 and Mt for CO2")




## ~ Heatmaps ----

emissions2050 %>%
  filter(Commodity == "CO2") %>%
  ggplot(aes(x = costred, y = emred, fill = Emissions)) +
  geom_tile(colour = "gray", size = 0.25) +
  facet_wrap(~Commodity) +
  labs(x = "Cost Reduction (%)",
       y = "Emissions Reduction (%)",
       title = "Electric Sector CO2 Emissions: 2050",
       fill = "Emissions (Mt)") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9))

emissions2050 %>%
  filter(Commodity == "PM 2.5") %>%
  ggplot(aes(x = costred, y = emred, fill = Emissions)) +
  geom_tile(colour = "gray", size = 0.25) +
  facet_wrap(~Commodity) +
  labs(x = "Cost Reduction (%)",
       y = "Emissions Reduction (%)",
       title = "Electric Sector PM 2.5 Emissions: 2050",
       fill = "Emissions (kt)") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9))

emissions2050 %>%
  filter(Commodity == "SO2" | Commodity == "NOX") %>%
  ggplot(aes(x = costred, y = emred, fill = Emissions)) +
  geom_tile(colour = "gray", size = 0.25) +
  facet_wrap(~Commodity) +
  labs(x = "Cost Reduction (%)",
       y = "Emissions Reduction (%)",
       title = "Electric Sector SO2 and NOx Emissions: 2050",
       fill = "Emissions (kt)") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9))

emissions2050 %>%
  filter(Commodity == "CH4") %>%
  ggplot(aes(x = costred, y = emred, fill = Emissions)) +
  geom_tile(colour = "gray", size = 0.25) +
  facet_wrap(~Commodity) +
  labs(x = "Cost Reduction (%)",
       y = "Emissions Reduction (%)",
       title = "Electric Sector Methane Emissions: 2050",
       fill = "Emissions (kt)") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9))


## Total Electricity Production ----

elctotal_long %>%
  ggplot() +
  geom_line(aes(x=Year, y=VAR_FOut, color = emred, group = Scenario)) +
  em_color +
  facet_wrap(~costred, ncol = 4) +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Scenario",
       color = "Emissions\nReduction\n(%)") +
  yt +
  scale_x_discrete(breaks = seq(2015,2050, by = 5))

elctotal2050 %>%
  ggplot() +
  geom_tile(aes(x = costred, y = emred, fill = VAR_FOut), colour = "gray", size = 0.25) +
  labs(x = "Offshore Wind Cost Reductions (%)",
       y = "Emissions Reduction (%)",
       title = "Total Electricity Produced: 2050",
       fill = "Electricity\nProduced\n(PJ)")

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

enduse %>%
  filter(costred == "40") %>%
  ggplot(aes(x = Year, y = Consumption, fill = Sector)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = col_sector) +
  facet_wrap(~emred) +
  labs(x = "Year", y = "Electricity Consumption (PJ)",
       title = "Electricity Consumption by Sector: 40% Cost Reduction ") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

## ~ Heatmaps ----

enduse %>% filter(Year == "2050") %>% filter(Sector == "Commercial") %>%
  ggplot(aes(x = costred, y = emred, fill = Consumption)) +
  geom_tile(colour = "gray", size = 0.25) +
  labs(x = "Offshore Wind Cost Reductions (%)",
       y = "Emissions Reduction (%)",
       title = "Commercial Sector Electricity Consumption: 2050",
       fill = "Electricity\nConsumed\n(PJ)") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9))

enduse %>% filter(Year == "2050") %>% filter(Sector == "Industrial") %>%
  ggplot(aes(x = costred, y = emred, fill = Consumption)) +
  geom_tile(colour = "gray", size = 0.25) +
  labs(x = "Offshore Wind Cost Reductions (%)",
       y = "Emissions Reduction (%)",
       title = "Industrial Sector Electricity Consumption: 2050",
       fill = "Electricity\nConsumed\n(PJ)") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9))

enduse %>% filter(Year == "2050") %>% filter(Sector == "Residential") %>%
  ggplot(aes(x = costred, y = emred, fill = Consumption)) +
  geom_tile(colour = "gray", size = 0.25) +
  labs(x = "Offshore Wind Cost Reductions (%)",
       y = "Emissions Reduction (%)",
       title = "Residential Sector Electricity Consumption: 2050",
       fill = "Electricity\nConsumed\n(PJ)") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9))

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

enduse %>% filter(Year == "2050") %>% filter(costred != "20") %>%
  ggplot(aes(x = costred, y = emred, fill = Consumption)) +
  geom_tile(colour = "gray", size = 0.25) +
  labs(x = "Offshore Wind Cost Reductions (%)",
       y = "Emissions Reduction (%)",
       title = "Transportation Sector Electricity Consumption: 2050",
       fill = "Electricity\nConsumed\n(PJ)") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9)) +
  facet_grid(~Sector, scales = "free")

## Save Plots ----

# plots <-

# lapply(plots,function(x){ggsave(file=paste(x,"pdf",sep="."),get(x))})
# https://stackoverflow.com/questions/20500706/saving-multiple-ggplots-from-ls-into-one-and-separate-files-in-r
