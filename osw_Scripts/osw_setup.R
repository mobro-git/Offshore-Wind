## ----Library----------------------------------------
library(graphics)
library(colorspace)
library(tidyverse)
library(readxl)
library(knitr)
library(kableExtra)
library(forcats)
library(scales)
library(ggrepel)
library(here)
library(ggtextures)
library(tibble)
library(gridExtra)
library(ggpubr)
library(patternplot)
library(plotly)
library(flexdashboard)
library(DT)
library(naniar)


## ----Functions-------------------------------------------
ReadAllSheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, 
                                                     sheet = X, 
                                                     skip = 6))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

categorize <- function(table) {
  x <- table %>%
    mutate(costred = case_when(
      str_detect(Scenario, "CostRed30") ~ "30",
      str_detect(Scenario, "CostRed40") ~ "40",
      str_detect(Scenario, "CostRed50") ~ "50",
      str_detect(Scenario, "CostRed60") ~ "60",
      str_detect(Scenario, "CostRed70") ~ "70",
      str_detect(Scenario, "CostRed80") ~ "80",
      TRUE ~ "20"
    )) %>%
    mutate(emred = case_when(
      str_detect(Scenario, "EmRedG0") ~ "0",
      str_detect(Scenario, "EmRedG10") ~ "10",
      str_detect(Scenario, "EmRedG20") ~ "20",
      str_detect(Scenario, "EmRedG30") ~ "30",
      str_detect(Scenario, "EmRedG40") ~ "40",
      str_detect(Scenario, "EmRedG50") ~ "50",
      str_detect(Scenario, "EmRedG60") ~ "60",
      str_detect(Scenario, "EmRedG70") ~ "70",
      str_detect(Scenario, "EmRedG80") ~ "80",
      TRUE ~ "BAU"
    )) %>% 
    mutate_if(is.numeric, ~round(.,2)) %>%
    mutate_all(~replace(.,is.na(.),0))
}

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

emission <- function(table) {
  x <- table %>%
    mutate(Commodity = case_when(
      str_detect(Commodity, "CH4") ~ "CH4",
      str_detect(Commodity, "CO2") ~ "CO2",
      str_detect(Commodity, "NOX") ~ "NOx",
      str_detect(Commodity, "SO2") ~ "SO2",
      str_detect(Commodity, "PM25") ~ "PM 2.5"
    )) 
}

filter_osw <- function(table,attribute,ridcol1=NULL,ridcol2=NULL, 
                       ridcol3=NULL,ridcol4=NULL,ridcol5=NULL,ridcol6=NULL) {
  x <- table %>%
    filter(Attribute == enexpr(attribute)) %>%
    select(-c(ridcol1, ridcol2, ridcol3, ridcol4, ridcol5, ridcol6))
}

lineplot.region <- function(data, region=NULL, yvar, facet="Region~costred",
                            scale=NULL, typevar=NULL, colvar=NULL, 
                            coltheme=NULL, title, ylab) {
  if(is.null(region)) {datas <- data}
  else {datas <- data %>% filter(Region == region)}
  p <- ggplot(data = datas, aes_string(x = "Year", y = yvar)) +
    geom_line(aes_string(group = "Scenario", 
                         linetype = typevar, 
                         color = colvar)) +
    facet_grid(facet, scales = scale) +
    yt +
    coltheme +
    labs(x = "Year", y = ylab,
         title = title,
         color = "Emissions Reduction (%)",
         linetype = "Emissions Reduction (%)") +
    bottom
}

grid.heatmap.col <- function(data, title) {
  ggplot(data = data, aes(x = costred, y = emred, fill = VAR_FOut)) +
  geom_tile(colour = "gray", size = 0.25) +
  facet_grid(Process ~ .) + 
  labs(x = "Offshore Wind Cost Reductions (%)",
       y = "Emissions Reduction (%)",
       title = title,
       fill = "Electricity\nProduced\n(PJ)") +
  scale_fill_gradient(na.value = "white") +
  st
}

grid.heatmap.bw <- function(data, title) {
  ggplot(data = data, aes(x = costred, y = emred, fill = VAR_FOut)) +
    geom_tile(colour = "gray", size = 0.25) +
    facet_grid(Process ~ .) + 
    labs(x = "Offshore Wind Cost Reductions (%)",
         y = "Emissions Reduction (%)",
         title = title,
         fill = "Electricity\nProduced\n(PJ)") +
    st +
    gray_fill_cont
}

## ----Factors-----------------------------------------------
levels_costred <- c("20", "30", "40", "50", "60", "70", "80")
levels_emred <- c("BAU", "30", "40", "50", "60", "70", "80")
levels_emissions <- c("CH4", "PM 2.5", "SO2", "NOx", "CO2")
levels_sector <- c("Transportation", "Industrial", "Commercial", "Residential")


## ----Themes and Scales-----------------------------------------------
bottom <- theme(legend.position = "bottom")

yt <- theme_bw() + 
  theme(strip.background = element_rect(colour = "black", fill = "white"),
            axis.text.x = element_text(angle = 50, hjust = 1, size = 9),
            axis.text.y = element_text(size = 9))

st <- theme_bw() + 
  theme(strip.background = element_rect(colour = "black", fill = "white"),
            axis.text.x = element_text(size = 9),
            axis.text.y = element_text(size = 9))

nolegend <- guides(color = FALSE, linetype = FALSE, fill = FALSE)

x_cont <- scale_x_continuous(breaks = seq(2020,2050, by = 10), expand = c(0,1))
x_disc <- scale_x_discrete(breaks = seq(2020,2050, by = 10), expand = c(0,.2))

col_osw <- c(`Terrestrial Wind` = "chartreuse4", `Hydro` = "skyblue3", 
             `Solar` = "darkgoldenrod2", `Offshore Wind` = "deepskyblue4", 
             `Nuclear` = "firebrick", `Coal` = "gray9", `Natural Gas` = "gray34", 
             `Coal CCS` = "darkorange3")
col_sector <- c(`Commercial` = "chartreuse4", `Industrial` = "firebrick", 
                `Residential` = "cadetblue3", `Transportation` = "darkgoldenrod2")
col_em <- c("#462300","#80470E","#B27941","#EEB67F","#7FBDEE","#367FB7","#034679")
col_cost <- sequential_hcl(7, h = c(260, 260), c = c(40,80), l = c(75,40))

zero <- geom_hline(yintercept = 0, linetype = "dashed", color = "red")

osw_fill <- scale_fill_manual(values = col_osw)
osw_color <- scale_color_manual(values = col_osw)
gray_fill <- scale_fill_grey(start = 0.9, end = 0)
gray_color <- scale_color_grey(start = 0.8, end = 0)
gray_fill_cont <- scale_fill_gradient(low = "#AEAEAE", high = "#000000", na.value = "white")
em_color <- scale_color_manual(values = col_em)
cost_color <- scale_color_manual(values = col_cost)
sec_fill <- scale_fill_manual(values = col_sector)
sec_color <- scale_color_manual(values = col_sector)



