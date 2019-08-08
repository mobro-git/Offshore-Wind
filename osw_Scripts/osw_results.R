## Scenarios ----

## ~ Cost ----

cost_plot <- ggplot(scenario_cost) +
  geom_label(data = . %>% filter(Year == last(Year)),
             aes(label = paste(CostCurve,"%"), x = Year, y = Costs), 
             size = 3, alpha = 0.5, nudge_y = 4) +
  scale_y_continuous(limits = c(0, 100), expand = c(.01, 0)) +
  labs(x = "Year", y = "Cost Reduction (%)", 
       title = "Cost Reduction Scenarios", 
       subtitle = "Percentage decrease from 2010 offshore wind CAPEX costs") +
  yt +
  x_cont +
  nolegend
  
cost_col <- cost_plot +
  geom_line(aes(x = Year, y = Costs, color = CostCurve, group = CostCurve), size = .75) +
  cost_color

cost_bw <- cost_plot +
  geom_line(aes(x = Year, y = Costs, linetype = CostCurve, group = CostCurve), size = .75)


## ~ Emissions ----

emissions_plot <- ggplot(scenario_emissions) +
  geom_label(data = . %>% filter(Year == last(Year)) %>% filter(Cap != "BAU"),
             aes(label = paste(Cap,"%"), x = Year, y = Emissions), 
             size = 3, alpha = 0.5, nudge_y = 80) +
  geom_label(data = . %>% filter(Year == last(Year)) %>% filter(Cap == "BAU"),
             aes(label = Cap, x = Year, y = Emissions), 
             size = 3, alpha = 0.5, nudge_y = 80) +
  ylim(0, 2200) +
  labs(x = "Year", y = "CO2 Emissions (Mt)", 
       title = "Emissions Reduction Scenarios", 
       subtitle = "Scenarios as overall % reduction of 2010 electric sector CO2 emissions") +
  yt +
  x_disc +
  nolegend
  
emissions_col <- emissions_plot + 
  geom_line(aes(x = Year, y = Emissions, color = Cap, group = Cap), size = .75) + 
  em_color 

emissions_bw <- emissions_plot +
  geom_line(aes(x = Year, y = Emissions, linetype = Cap, group = Cap), size = .75)

## LCOE ----

lcoe_table <- lcoe %>% 
  kable(booktabs = T, caption = "Estimated LCOE capacity-weighted average for new generation 
        resources entering service in 2023 (2018 \\$/MWh)", linesep = "") %>% 
  kable_styling(latex_options = c("striped", "hold_position", "scale_down")) %>%
  column_spec(1, width = "2.95 cm") %>%
  column_spec(2:8, width = "1.2 cm") %>%
  column_spec(9, width = "1.5 cm") %>%
  pack_rows("Dispatchable technologies", 1, 5) %>%
  pack_rows("Non-dispatchable technologies", 6, 9) %>%
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

cost40_facetcost <-  cost40 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Process",
       subtitle = "40% Cost Reductions") +
  facet_grid(.~emred) +
  yt +
  x_disc

cost40_facetcost_line_col <- cost40_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, color = Process, group = Process), size = 0.75) +
  osw_color +
  bottom

cost40_facetcost_line_bw <- cost40_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom

cost40_facetcost_fill_col <- cost40_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill +
  bottom

cost40_facetcost_fill_bw <- cost40_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom

cost50_facetcost <-  cost50 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Process",
       subtitle = "50% Cost Reductions") +
  facet_grid(.~emred) +
  yt +
  x_disc

cost50_facetcost_line_col <- cost50_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, color = Process, group = Process), size = 0.75) +
  osw_color +
  bottom

cost50_facetcost_line_bw <- cost50_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom

cost50_facetcost_fill_col <- cost50_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill +
  bottom

cost50_facetcost_fill_bw <- cost50_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom

cost60_facetcost <-  cost60 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Process",
       subtitle = "60% Cost Reductions") +
  facet_grid(.~emred) +
  yt +
  x_disc

cost60_facetcost_line_col <- cost60_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, color = Process, group = Process), size = 0.75) +
  osw_color +
  bottom

cost60_facetcost_line_bw <- cost60_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom

cost60_facetcost_fill_col <- cost60_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill +
  bottom

cost60_facetcost_fill_bw <- cost60_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom

cost70_facetcost <-  cost70 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Process",
       subtitle = "70% Cost Reductions") +
  facet_grid(.~emred) +
  yt +
  x_disc

cost70_facetcost_line_col <- cost70_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, color = Process, group = Process), size = 0.75) +
  osw_color +
  bottom

cost70_facetcost_line_bw <- cost70_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom

cost70_facetcost_fill_col <- cost70_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill +
  bottom

cost70_facetcost_fill_bw <- cost70_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom

cost80_facetcost <-  cost80 %>% 
  ggplot() +
  labs(x = "Year", y = "Electricity Production (PJ)",
       title = "Electricity Production by Process",
       subtitle = "80% Cost Reductions") +
  facet_grid(.~emred) +
  yt +
  x_disc

cost80_facetcost_line_col <- cost80_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, color = Process, group = Process), size = 0.75) +
  osw_color +
  bottom

cost80_facetcost_line_bw <- cost80_facetcost +
  geom_line(aes(x = Year, y = VAR_FOut, linetype = Process, group = Process), size = 0.75) +
  bottom

cost80_facetcost_fill_col <- cost80_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  osw_fill +
  bottom

cost80_facetcost_fill_bw <- cost80_facetcost +
  geom_area(aes(x = Year, y = VAR_FOut, fill = Process, group = Process), color = "black") +
  gray_fill +
  bottom

## ~ Heat Maps ----

osw <- elc_long %>% filter(Year == "2050" & Process == "Offshore Wind")
wnd <- elc_long %>% filter(Year == "2050" & Process == "Terrestrial Wind")
sol <- elc_long %>% filter(Year == "2050" & Process == "Solar")
coal <- elc_long %>% filter(Year == "2050" & Process == "Coal")
nga <- elc_long %>% filter(Year == "2050" & Process == "Natural Gas")
nuk <- elc_long %>% filter(Year == "2050" & Process == "Nuclear")
hyd <- elc_long %>% filter(Year == "2050" & Process == "Hydro")

osw_grid_heatmap <- grid.heatmap.col(osw)
wnd_grid_heatmap <- grid.heatmap.col(wnd)
sol_grid_heatmap <- grid.heatmap.col(sol)
coal_grid_heatmap <- grid.heatmap.col(coal)
nga_grid_heatmap <- grid.heatmap.col(nga)
nuk_grid_heatmap <- grid.heatmap.col(nuk)
hyd_grid_heatmap <- grid.heatmap.col(hyd)

osw_grid_heatmap_bw <- grid.heatmap.bw(osw)
wnd_grid_heatmap_bw <- grid.heatmap.bw(wnd)
sol_grid_heatmap_bw <- grid.heatmap.bw(sol)
coal_grid_heatmap_bw <- grid.heatmap.bw(coal)
nga_grid_heatmap_bw <- grid.heatmap.bw(nga)
nuk_grid_heatmap_bw <- grid.heatmap.bw(nuk)
hyd_grid_heatmap_bw <- grid.heatmap.bw(hyd)

## ~ Retirements ----

base_retire <- retire_long %>%
  filter(emred == "BAU" & costred == "20") %>%
  filter(Process != "Other") %>%
  ggplot(aes(x = Year, y = retire, fill = Process)) +
  geom_bar(stat = "identity", position = "stack", width = 0.9) +
  scale_fill_manual(values = col_osw) +
  labs(x = "Year", y = "Change in Electricity Production (PJ)",
       title = "Baseline Changes in Electricity Production") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.75)

prod_dif %>%
  filter(emred == "BAU" & costred == "40") %>%
  ggplot(aes(x = Year, y = diff, fill = Process)) +
  geom_bar(stat = "identity", position = "stack", width = 0.9) +
  scale_fill_manual(values = col_osw) +
  labs(x = "Year", y = "Change in Electricity Production (PJ)",
       title = "Baseline Changes in Electricity Production") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

prod_dif %>%
  filter(emred == "40" & costred == "40") %>%
  ggplot(aes(x = Year, y = diff, fill = Process)) +
  geom_bar(stat = "identity", position = "stack", width = 0.9) +
  scale_fill_manual(values = col_osw) +
  labs(x = "Year", y = "Change in Electricity Production (PJ)",
       title = "Baseline Changes in Electricity Production") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

prod_dif %>% 
  filter(costred %in% c("40", "50", "70")) %>% 
  filter(emred %in% c("BAU", "40", "70")) %>%
  ggplot(aes(x = Year, y = diff, fill = Process)) +
  geom_bar(stat = "identity", position = "stack", width = 0.9) +
  scale_fill_manual(values = col_osw) +
  labs(x = "Year", y = "Change in Electricity Production (PJ)",
       title = "Baseline Changes in Electricity Production") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_grid(costred~emred)


## Emissions ----

## ~ Timelines ----

emissions_long %>% filter(costred != "30" & costred != "20") %>%   
  ggplot(aes(x = Year, y = Emissions)) +
  geom_line(aes(group = Scenario, color = costred, linetype = emred)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7)) +
  scale_color_manual(values = c(diverge_hcl(5, h = c(40,246), c = 96))) +
  facet_wrap(~Commodity, scales = "free_y") +
  theme_bw() +
  labs(x = "Year", y = "Emissions",
       title = "Electric Sector Emissions Output",
       linetype = "Emissions\nReduction\n(%)",
       color = "Cost\nReduction\n(%)") +
  scale_x_discrete(breaks = seq(2015,2050, by = 5)) 

emissions_long %>% filter(Commodity == "CO2") %>%
  ggplot(aes(x = Year, y = Emissions)) +
  geom_line(aes(group = Scenario, color = costred)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7)) +
  facet_wrap(~emred) +
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
  facet_grid(costred~emred)

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


emissions_bau %>%
  ggplot(aes(x = Year, y = Emissions, fill = Commodity)) +
  geom_bar(stat = "identity", position = "stack") + 
  scale_fill_manual(values = c("deepskyblue4", "firebrick", "darkgoldenrod3", "seashell4", "chartreuse4")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9)) +
  labs(title = "Baseline Electric Sector Emissions", 
       caption = "*Emissions units are kt for NOx and SO2 and Mt for CO2") 

emissions_bau %>%
  ggplot(aes(x = Year, y = Emissions, color = Commodity)) +
  geom_line(aes(group = Commodity)) + 
  scale_color_manual(values = c("deepskyblue4", "firebrick", "darkgoldenrod3", "seashell4", "chartreuse4")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9)) +
  labs(title = "Baseline Electric Sector Emissions", 
       caption = "*Emissions units are kt for NOx and SO2 and Mt for CO2", 
       y = "Emissions*") 

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
  filter(costred == "40") %>%
  filter(Sector == "Transportation") %>% 
  ggplot(aes(x = Year, y = Consumption, fill = Sector)) +
  geom_bar(stat = "identity", position = "stack", fill = "darkgoldenrod2") +
  facet_wrap(~emred) +
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

# lapply(plots,function(x){ggsave(file=paste(x,"pdf",sep="."),get(x))})
# https://stackoverflow.com/questions/20500706/saving-multiple-ggplots-from-ls-into-one-and-separate-files-in-r
