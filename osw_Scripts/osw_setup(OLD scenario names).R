## ----Library----------------------------------------

# load all packages into the environment. there are more packages here than I ended up using,
# but the list just kept getting longer over time. could clean this up but doesnt seem worth
# the effort because no harm done
library(relaimpo)
library(robustbase)
library(sandwich)
library(lmtest)
library(graphics)
library(colorspace)
library(scales)
library(ggrepel)
library(here)
# library(ggtextures)
library(gridExtra)
library(ggpubr)
library(patternplot)
library(plotly)
library(flexdashboard)
library(DT)
library(naniar)
library(maps)
library(mapdata)
library(usmap)
library(ggmap)
library(maptools)
library(sf)
library(PerformanceAnalytics)
library(jtools)
library(ggstance)
library(huxtable)
library(readxl)
library(knitr)
library(tidyverse)
library(kableExtra)

## ----Functions-------------------------------------------

# function to pull in all sheets from an excel spreadsheet and turn them into 
# separate data frames. should make for easier data analysis and manipulation 

ReadAllSheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, 
                                                     sheet = X, 
                                                     skip = 6))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# function to turn my scenario titles (which are very convoluted and long) into 
# intelligible combinations of variables that I can filter on to better sort the 
# data and results

categorize <- function(table) {
  x <- table %>%
    mutate(costred = case_when(
      str_detect(Scenario, "CostRed40") ~ "40",
      str_detect(Scenario, "CostRed50") ~ "50",
      str_detect(Scenario, "CostRed60") ~ "60",
      str_detect(Scenario, "CostRed70") ~ "70",
      str_detect(Scenario, "CostRed80") ~ "80",
      str_detect(Scenario, "CostRed45") ~ "45",
      str_detect(Scenario, "CostRed55") ~ "55",
      TRUE ~ "20"
    )) %>%
    mutate(emred = case_when(
      str_detect(Scenario, "EmRedG30") ~ "30",
      str_detect(Scenario, "EmRedG40") ~ "40",
      str_detect(Scenario, "EmRedG50") ~ "50",
      str_detect(Scenario, "EmRedG60") ~ "60",
      str_detect(Scenario, "EmRedG70") ~ "70",
      str_detect(Scenario, "EmRedG80") ~ "80",
      TRUE ~ "BAU"
    )) %>% 
    mutate_if(is.numeric, ~round(.,2)) %>%
    mutate_all(~replace(.,is.na(.),0)) %>%
    mutate(Scenario = paste("E", emred, "C", costred, sep = ''))
}

# function to reformat names of processes to more intelligable names and consolidate
# those that I dont need to analyze (petroleum, msw, biomass) into "other"

process <- function(table) {
  x <- table %>%
    mutate(Process = case_when(
      str_detect(Process, "COAL") ~ "Coal",
      str_detect(Process, "HYD") ~ "Hydro",
      str_detect(Process, "NGA") ~ "Natural Gas",
      str_detect(Process, "NUK") ~ "Nuclear",
      str_detect(Process, "SOL") ~ "Solar",
      str_detect(Process, "WNDOF") ~ "Offshore Wind",
      str_detect(Process, "WNDON") ~ "Terrestrial Wind",
      TRUE ~ "Other"
    )) 
}

# function to reformat names of emissions for a nicer look. labels section below formats
# the subscripts for the molecular emissions so they look nicer in the graphs and tables

emission <- function(table) {
  x <- table %>%
    mutate(Commodity = case_when(
      str_detect(Commodity, "CH4") ~ "CH[4]",
      str_detect(Commodity, "CO2") ~ "CO[2]",
      str_detect(Commodity, "NOX") ~ "NO[X]",
      str_detect(Commodity, "SO2") ~ "SO[2]",
      str_detect(Commodity, "PM25") ~ "PM[2.5]"
    )) 
}

# function to more easily filter and select columns from a dataframe. ended up not really
# using this function that much, but could probably repurpose it for something else

filter_osw <- function(table,attribute,ridcol1=NULL,ridcol2=NULL, 
                       ridcol3=NULL,ridcol4=NULL,ridcol5=NULL,ridcol6=NULL) {
  x <- table %>%
    filter(Attribute == enexpr(attribute)) %>%
    select(-c(ridcol1, ridcol2, ridcol3, ridcol4, ridcol5, ridcol6))
}

# function to reduce code copying and pasting when duplicating the same graph for each region

lineplot.region <- function(data, region=NULL, yvar, facet="Region~costred",
                            scale=NULL, typevar=NULL, colvar=NULL, 
                            coltheme=NULL, title, ylab) {
  if(is.null(region)) {datas <- data}
  else {datas <- data %>% filter(Region == region)}
  p <- ggplot(data = datas, aes_string(x = "Year", y = yvar)) +
    geom_line(aes_string(group = "Scenario", 
                         linetype = typevar, 
                         color = colvar), size = 1) +
    facet_grid(facet, scales = scale, labeller=labeller(costred = costlabels)) +
    yt +
    coltheme +
    labs(x = "Year", y = ylab,
         title = title,
         color = "Emissions Reduction (%)",
         linetype = "Emissions Reduction (%)") +
    bottom1
}

# function to reduce code copying and pasting when duplicating heat maps

grid.heatmap.col <- function(data, title) {
  ggplot(data = data, aes(x = costred, y = emred, fill = VAR_FOut)) +
  geom_tile(colour = "gray", size = 0.25) +
  facet_grid(Technology ~ .) + 
  labs(x = "Offshore Wind Cost Reductions (%)",
       y = "Emissions Reduction (%)",
       title = title,
       fill = "Electricity\nProduced\n(PJ)") +
  color_fill_cont +
  st
}

grid.heatmap.bw <- function(data, title) {
  ggplot(data = data, aes(x = costred, y = emred, fill = VAR_FOut)) +
    geom_tile(colour = "white", size = 0.25) +
    facet_grid(Technology ~ .) + 
    labs(x = "Offshore Wind Cost Reductions (%)",
         y = "Emissions Reduction (%)",
         title = title,
         fill = "Electricity\nProduced\n(PJ)") +
    st +
    gray_fill_cont
}

grid.region.col <- function(region) {
  elc_long_reg %>% 
    filter(Region == region) %>%
    filter(!costred %in% c("20", "30", "40")) %>%
    filter(emred %in% c("BAU", "40", "70")) %>%
    ggplot() +
    geom_line(aes(x = Year, y = VAR_FOut, color = Technology, group = Technology), size = 1) +
    labs(x = "Year", y = "Electricity Production (PJ)",
         title = "Electricity Production by Technology",
         subtitle = region) +
    facet_grid(costred~emred, labeller=labeller(emred = emissionlabels, costred = costlabels)) +
    yt +
    x_disc_l +
    osw_color +
    theme(legend.title=element_blank()) +
    bottom1
}

grid.region.bw <- function(region) {
  elc_long_reg %>% 
    filter(Region == region) %>%
    filter(!costred %in% c("20", "30", "40")) %>%
    filter(emred %in% c("BAU", "40", "70")) %>%
    ggplot() +
    geom_line(aes(x = Year, y = VAR_FOut, color = Technology, group = Technology), size = 1) +
    labs(x = "Year", y = "Electricity Production (PJ)",
         title = "Electricity Production by Technology",
         subtitle = region) +
    facet_grid(costred~emred, labeller=labeller(emred = emissionlabels, costred = costlabels)) +
    yt +
    x_disc_l +
    gray_color +
    theme(legend.title=element_blank()) +
    bottom1
}

grid.region.bar.col <- function(region) {
  elc_long_reg %>% 
    filter(Region == region) %>%
    filter(!costred %in% c("20", "30", "40")) %>%
    filter(emred %in% c("BAU", "40", "70")) %>%
    ggplot() +
    geom_bar(aes(x = Year, y = VAR_FOut, fill = Technology), position = "stack", stat = "identity",
             color = "black") +
    labs(x = "Year", y = "Electricity Production (PJ)",
         title = "Electricity Production by Technology",
         subtitle = region) +
    facet_grid(costred~emred, labeller=labeller(emred = emissionlabels, costred = costlabels)) +
    yt +
    x_disc_l +
    osw_fill +
    theme(legend.title=element_blank()) +
    bottom1
}

grid.region.bar.bw <- function(region) {
  elc_long_reg %>% 
    filter(Region == region) %>%
    filter(!costred %in% c("20", "30", "40")) %>%
    filter(emred %in% c("BAU", "40", "70")) %>%
    ggplot() +
    geom_bar(aes(x = Year, y = VAR_FOut, fill = Technology), position = "stack", stat = "identity",
             color = "black") +
    labs(x = "Year", y = "Electricity Production (PJ)",
         title = "Electricity Production by Technology",
         subtitle = region) +
    facet_grid(costred~emred, labeller=labeller(emred = emissionlabels, costred = costlabels)) +
    yt +
    x_disc_l +
    gray_fill +
    theme(legend.title=element_blank()) +
    bottom1
}

  

# functions to reduce code copying and pasting when duplicating addition/retirement summary graphs

prod.dif.col <- function(data, title, subtitle = NULL) {
  ggplot(data = data, aes(x = Year, y = diff, fill = Technology)) +
    geom_bar(stat = "identity", position = "stack", width = 0.9, color = "black") +
    osw_fill +
    labs(x = "Year", y = "Change in Electricity Production (PJ)",
         title = title,
         subtitle = subtitle) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    zero +
    yt +
    x_disc_l +
    facet_grid(emred~costred, labeller = labeller(emred = emissionlabels, costred = costlabels),
               scales = "free_y") +
    bottom1
}

prod.dif.bw <- function(data, title, subtitle = NULL) {
  ggplot(data = data, aes(x = Year, y = diff, fill = Technology)) +
    geom_bar(stat = "identity", position = "stack", width = 0.9, color = "black") +
    gray_fill +
    labs(x = "Year", y = "Change in Electricity Production (PJ)",
         title = title,
         subtitle = subtitle) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    zero +
    yt + 
    x_disc_l +
    facet_grid(emred~costred, labeller = labeller(emred = emissionlabels, costred = costlabels)) +
    bottom1
}

# functions to reduce code copying and pasting when duplicating addition/retirement by emissions scen

prod.dif.em.col <- function(data, title) {
  ggplot(data = data, aes(x = Year, y = diff, fill = Technology)) +
    geom_bar(stat = "identity", position = "stack", width = 0.9, color = "black") +
    osw_fill +
    labs(x = "Year", y = "Change in Electricity Production (PJ)",
         title = title) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    zero +
    yt +
    facet_grid(costred~emred, labeller = labeller(emred = emissionlabels, costred = costlabels), 
               scales = "free_y") +
    bottom1
}

prod.dif.em.bw <- function(data, title) {
  ggplot(data = data, aes(x = Year, y = diff, fill = Technology)) +
    geom_bar(stat = "identity", position = "stack", width = 0.9, color = "black") +
    gray_fill +
    labs(x = "Year", y = "Change in Electricity Production (PJ)",
         title = title) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    zero +
    yt + 
    facet_grid(costred~emred, labeller = labeller(emred = emissionlabels, costred = costlabels)) +
    bottom1
}

# functions to reduce code copying and pasting when duplicating addition/retirement by cost scen

prod.dif.cost.col <- function(data, title) {
  ggplot(data = data, aes(x = Year, y = diff, fill = Technology)) +
    geom_bar(stat = "identity", position = "stack", width = 0.9, color = "black") +
    osw_fill +
    labs(x = "Year", y = "Change in Electricity Production (PJ)",
         title = title) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    zero +
    yt +
    facet_grid(costred~emred, labeller = labeller(emred = emissionlabels, costred = costlabels), 
               scales = "free_y") +
    bottom1
}

prod.dif.cost.bw <- function(data, title) {
  ggplot(data = data, aes(x = Year, y = diff, fill = Technology)) +
    geom_bar(stat = "identity", position = "stack", width = 0.9, color = "black") +
    gray_fill +
    labs(x = "Year", y = "Change in Electricity Production (PJ)",
         title = title) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    zero +
    yt +
    facet_grid(costred~emred, labeller = labeller(emred = emissionlabels, costred = costlabels), 
               scales = "free_y") +
    bottom1
}

em.heatmap.col <- function(data, em, title) {
  ggplot(data = data %>% filter(Commodity == em), aes(x = costred, y = emred, fill = Emissions)) +
    geom_tile(colour = "gray", size = 0.25) +
    labs(x = "Offshore Wind Cost Reductions (%)",
         y = "Emissions Reduction (%)",
         title = title,
         fill = "Emissions") +
    color_fill_cont +
    st
}

em.heatmap.bw <- function(data, em, title) {
  ggplot(data = data %>% filter(Commodity == em), aes(x = costred, y = emred, fill = Emissions)) +
    geom_tile(colour = "white", size = 0.25) +
    labs(x = "Offshore Wind Cost Reductions (%)",
         y = "Emissions Reduction (%)",
         title = title,
         fill = "Emissions") +
    gray_fill_cont +
    st
}

em.heatmap.per.col <- function(data, em, title) {
  ggplot(data = data %>% filter(Commodity == em), aes(x = costred, y = emred, fill = percent.red)) +
    geom_tile(colour = "gray", size = 0.25) +
    geom_text(aes(label = paste(percent.red*100, "%")), color = "black", size = 5) +
    labs(x = "Offshore Wind Cost Reductions (%)",
         y = "Emissions Reduction (%)",
         title = title,
         fill = "Emissions\nReduction (%)") +
    color_fill_cont +
    st
}

em.heatmap.per.bw <- function(data, em, title) {
  ggplot(data = data %>% filter(Commodity == em), aes(x = costred, y = emred, fill = percent.red)) +
    geom_tile(colour = "white", size = 0.25) +
    geom_text(aes(label = paste(percent.red*100, "%")), color = "white", size = 5) +
    labs(x = "Offshore Wind Cost Reductions (%)",
         y = "Emissions Reduction (%)",
         title = title,
         fill = "Emissions\nReduction (%)") +
    gray_fill_cont +
    st
}

## ----Factors-----------------------------------------------

# creates levels to be used when factoring the scenarios so they appear in the correct
# order in graphs (eg from lowest to highest cost and least to most stringent emissions)

levels_costred <- c("20", "30", "40", "50", "60", "70", "80")
levels_emred <- c("BAU", "30", "40", "50", "60", "70", "80")
levels_emissions <- c("CH[4]", "PM[2.5]", "SO[2]", "NO[X]", "CO[2]")
levels_sector <- c("Transportation", "All Industrial", "CHP Industrial", 
                   "Grid Industrial", "Commercial", "Residential")

## ----Labels-----------------------

# creates labels to be used on graphs instead of the variable name or value. looks nicer
# and can write out things (eg "business as usual") that I wouldnt otherwise want to type
# out when filtering through data or greating graphics

elab <- c("BAU" = "BAU", "30" = "E30", "40" = "E40",  "50" = "E50", "60" = "E60",
          "70" = "E70", "80" = "E80")
clab <- c("20" = "C20", "30" = "C30", "40" = "C40",  "50" = "C50", "60" = "C60", "70" = "C70", 
          "80" = "C80")

costlabels <- c(
  "20" = "20% Cost Red.",
  "30" = "30% Cost Red.",
  "40" = "40% Cost Red.",  
  "50" = "50% Cost Red.", 
  "60" = "60% Cost Red.", 
  "70" = "70% Cost Red.", 
  "80" = "80% Cost Red.")

emissionlabels <- c(
  "BAU" = "Business as usual", 
  "30" = "30% CO2 Red.", 
  "40" = "40% CO2 Red.",  
  "50" = "50% CO2 Red.", 
  "60" = "60% CO2 Red.", 
  "70" = "70% CO2 Red.", 
  "80" = "80% CO2 Red.")

airlabels <- c(
  "CO[2]" = expression(CO[2]),
  "SO[2]" = expression(SO[2]),
  "NO[X]" = expression(NO[X]),
  "CH[4]" = expression(CH[4]),
  "PM[2.5]" = expression(PM[2.5])
)

## ----Themes and Scales-----------------------------------------------

# creates templates for use in ggplot2 graphics to reduce code redundancy. some elements 
# create templates for axis scales and formatting, others set color scales for certain 
# groups of graphs, and others format the graph themes

bottom1 <- theme(legend.position = "bottom")
bottom2 <- guides(color = guide_legend(nrow = 1), linetype = guide_legend(nrow = 1))

yt <- theme_bw() + 
  theme(strip.background = element_rect(colour = "black", fill = "white"),
            axis.text.x = element_text(angle = 50, hjust = 1, size = 9),
            axis.text.y = element_text(size = 9))

st <- theme_bw() + 
  theme(strip.background = element_rect(colour = "black", fill = "white"),
            axis.text.x = element_text(size = 9),
            axis.text.y = element_text(size = 9))

nolegend <- guides(color = FALSE, linetype = FALSE, fill = FALSE)

noaxes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

x_cont <- scale_x_continuous(breaks = seq(2020,2050, by = 5), expand = c(0,1))
x_disc <- scale_x_discrete(breaks = seq(2020,2050, by = 5), expand = c(0,.2))
x_disc_l <- scale_x_discrete(breaks = seq(2020,2050, by = 10), expand = c(0,.2))

col_osw <- c(`Terrestrial Wind` = "#92CBF3", `Hydro` = "dodgerblue4", 
             `Solar` = "darkgoldenrod2", `Offshore Wind` = "#6BA85A", 
             `Nuclear` = "darkorange3", `Coal` = "gray9", `Natural Gas` = "darkslategray4", 
             `Coal CCS` = "gray")
col_sector <- c(`Commercial` = "chartreuse4", `Industrial` = "firebrick", 
                `Residential` = "cadetblue3", `Transportation` = "darkgoldenrod2")
col_em <- c("#462300","#80470E","#B27941","#EEB67F","#7FBDEE","#367FB7","#034679")
col_cost <- c("20" = "#C2DFF8", "30" = "#98C1E3", "40" = "#6FAADB", "50" = "#4596DA", 
              "60" = "#4579DA", "70" = "#2054B5", "80" = "#032F82")
col_costosw <- c("50" = "#C9D0FF", "60" = "#8F9BEA", "70" = "#364BD8", "80" = "#0017AE")
col_commodity <-  c("CH[4]" = "deepskyblue4", "PM[2.5]" = "firebrick", "SO[2]" = "darkgoldenrod3", 
                    "NO[X]" = "seashell4", "CO[2]" = "chartreuse4")

zero <- geom_hline(yintercept = 0, linetype = "dashed", color = "red")

osw_fill <- scale_fill_manual(values = col_osw)
osw_color <- scale_color_manual(values = col_osw)
gray_fill <- scale_fill_grey(start = 0.9, end = 0)
gray_color <- scale_color_grey(start = 0.8, end = 0)
color_fill_cont <- scale_fill_gradient(low = "#C9D0FF", high = "#0017AE", na.value = "gray")
gray_fill_cont <- scale_fill_gradient(low = "#bababa", high = "#08090a", na.value = "white")
em_color <- scale_color_manual(values = col_em)
cost_color <- scale_color_manual(values = col_cost)
costosw_color <- scale_color_manual(values = col_costosw)
sec_fill <- scale_fill_manual(values = col_sector)
sec_color <- scale_color_manual(values = col_sector)
commodity_fill <- scale_fill_manual(values = col_commodity, labels = airlabels)
commodity_color <- scale_color_manual(values = col_commodity, labels = airlabels)


