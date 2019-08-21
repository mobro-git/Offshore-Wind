---
title: "Offshore Wind Results and Analysis"
author: "Morgan Browning"
date: "21 August, 2019"
output:
  html_document:
    number_sections: yes
    toc: yes
    keep_md: TRUE
  pdf_document:
    number_sections: yes
    toc: yes
---

***

# Disclosure

This document functions as an all-inclusive working directory for synthesis and graphical analysis of the results from the offshore wind research of Morgan Browning, an ORISE Fellow at the U.S. Environmental Protection Agency's Office of Research and Development. This document and its contents are not finalized nor are intended for publication. 

It is annotated primarily for ease of reproducability and a general understanding of the results. Graphs are provided with many variations to meet criteria of different publication and presentation platforms.

# Setup

Three scripts are loaded into this markdown document to allow for analysis of the data. The setup script loads the library, creates generalized functions, and creates global variables for color scales and factors. The data script loads the excel spreadsheet with all of the results data and performs the majority of data munging. The results script creates charts, graphs, and tables. This report functions as the annotated synthesis of the data and results.





# Scenarios

The nested parametric sensitivity analysis was built on combinations of two sets of scenarios:

1. Electric sector CO~2~ emissions caps, as a linear decrease to a given % decrease from 2010 emissions by 2050

2. Cost reductions of offshore wind, as a linear decrease to a given % decrease from 2010 costs by 2035, then level costs to 2050

<img src="osw_Report_files/figure-html/unnamed-chunk-2-1.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-2-2.png" width="50%" />



# LCOE

EIA's AEO 2019 provides the following values for the estimated levelized cost of electricity (capacity-weighted average) for new generation resources entering service in 2023 (2018 $/MWh). Offshore wind has the highest total LCOE by a large margin. The second most expensive technology is biomass. The AEO LCOE was used in the calculation of offshore wind costs for the above cost curves, but LCOE is not directly used in the model.

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>Estimated LCOE capacity-weighted average for new generation 
        resources entering service in 2023 (2018 \$/MWh)</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Plant Type </th>
   <th style="text-align:right;"> Capacity Factor (%) </th>
   <th style="text-align:right;"> Levelized capital cost </th>
   <th style="text-align:right;"> Levelized fixed O&amp;M </th>
   <th style="text-align:right;"> Levelized variable O&amp;M </th>
   <th style="text-align:right;"> Levelized transmission cost </th>
   <th style="text-align:right;"> Total system LCOE </th>
   <th style="text-align:left;"> Levelized tax credit </th>
   <th style="text-align:right;"> Total LCOE including tax credit </th>
  </tr>
 </thead>
<tbody>
  <tr grouplength="5"><td colspan="9" style="border-bottom: 1px solid;"><strong>Dispatchable technologies</strong></td></tr>
<tr>
   <td style="text-align:left;width: 2.95 cm;  padding-left: 2em;" indentlevel="1"> Conventional CC </td>
   <td style="text-align:right;width: 1.2 cm; "> 87 </td>
   <td style="text-align:right;width: 1.2 cm; "> 8.1 </td>
   <td style="text-align:right;width: 1.2 cm; "> 1.5 </td>
   <td style="text-align:right;width: 1.2 cm; "> 32.3 </td>
   <td style="text-align:right;width: 1.2 cm; "> 0.9 </td>
   <td style="text-align:right;width: 1.2 cm; "> 42.8 </td>
   <td style="text-align:left;width: 1.2 cm; "> NA </td>
   <td style="text-align:right;width: 1.5 cm; "> 42.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 2.95 cm;  padding-left: 2em;" indentlevel="1"> Advanced CC </td>
   <td style="text-align:right;width: 1.2 cm; "> 87 </td>
   <td style="text-align:right;width: 1.2 cm; "> 7.1 </td>
   <td style="text-align:right;width: 1.2 cm; "> 1.4 </td>
   <td style="text-align:right;width: 1.2 cm; "> 30.7 </td>
   <td style="text-align:right;width: 1.2 cm; "> 1.0 </td>
   <td style="text-align:right;width: 1.2 cm; "> 40.2 </td>
   <td style="text-align:left;width: 1.2 cm; "> NA </td>
   <td style="text-align:right;width: 1.5 cm; "> 40.2 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 2.95 cm;  padding-left: 2em;" indentlevel="1"> Advanced CT </td>
   <td style="text-align:right;width: 1.2 cm; "> 30 </td>
   <td style="text-align:right;width: 1.2 cm; "> 17.2 </td>
   <td style="text-align:right;width: 1.2 cm; "> 2.7 </td>
   <td style="text-align:right;width: 1.2 cm; "> 54.6 </td>
   <td style="text-align:right;width: 1.2 cm; "> 3.0 </td>
   <td style="text-align:right;width: 1.2 cm; "> 77.5 </td>
   <td style="text-align:left;width: 1.2 cm; "> NA </td>
   <td style="text-align:right;width: 1.5 cm; "> 77.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 2.95 cm;  padding-left: 2em;" indentlevel="1"> Geothermal </td>
   <td style="text-align:right;width: 1.2 cm; "> 90 </td>
   <td style="text-align:right;width: 1.2 cm; "> 24.6 </td>
   <td style="text-align:right;width: 1.2 cm; "> 13.3 </td>
   <td style="text-align:right;width: 1.2 cm; "> 0.0 </td>
   <td style="text-align:right;width: 1.2 cm; "> 1.4 </td>
   <td style="text-align:right;width: 1.2 cm; "> 39.4 </td>
   <td style="text-align:left;width: 1.2 cm; "> -2.5 </td>
   <td style="text-align:right;width: 1.5 cm; "> 36.9 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 2.95 cm;  padding-left: 2em;" indentlevel="1"> Biomass </td>
   <td style="text-align:right;width: 1.2 cm; "> 83 </td>
   <td style="text-align:right;width: 1.2 cm; "> 37.3 </td>
   <td style="text-align:right;width: 1.2 cm; "> 15.7 </td>
   <td style="text-align:right;width: 1.2 cm; "> 37.5 </td>
   <td style="text-align:right;width: 1.2 cm; "> 1.5 </td>
   <td style="text-align:right;width: 1.2 cm; "> 92.1 </td>
   <td style="text-align:left;width: 1.2 cm; "> NA </td>
   <td style="text-align:right;width: 1.5 cm; "> 92.1 </td>
  </tr>
  <tr grouplength="4"><td colspan="9" style="border-bottom: 1px solid;"><strong>Non-dispatchable technologies</strong></td></tr>
<tr>
   <td style="text-align:left;width: 2.95 cm;  padding-left: 2em;" indentlevel="1"> Wind, onshore </td>
   <td style="text-align:right;width: 1.2 cm; "> 44 </td>
   <td style="text-align:right;width: 1.2 cm; "> 27.8 </td>
   <td style="text-align:right;width: 1.2 cm; "> 12.6 </td>
   <td style="text-align:right;width: 1.2 cm; "> 0.0 </td>
   <td style="text-align:right;width: 1.2 cm; "> 2.4 </td>
   <td style="text-align:right;width: 1.2 cm; "> 42.8 </td>
   <td style="text-align:left;width: 1.2 cm; "> -6.1 </td>
   <td style="text-align:right;width: 1.5 cm; "> 36.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 2.95 cm;  padding-left: 2em;" indentlevel="1"> Wind, offshore </td>
   <td style="text-align:right;width: 1.2 cm; "> 45 </td>
   <td style="text-align:right;width: 1.2 cm; "> 95.5 </td>
   <td style="text-align:right;width: 1.2 cm; "> 20.4 </td>
   <td style="text-align:right;width: 1.2 cm; "> 0.0 </td>
   <td style="text-align:right;width: 1.2 cm; "> 2.1 </td>
   <td style="text-align:right;width: 1.2 cm; "> 117.9 </td>
   <td style="text-align:left;width: 1.2 cm; "> -11.5 </td>
   <td style="text-align:right;width: 1.5 cm; "> 106.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 2.95 cm;  padding-left: 2em;" indentlevel="1"> Solar PV </td>
   <td style="text-align:right;width: 1.2 cm; "> 29 </td>
   <td style="text-align:right;width: 1.2 cm; "> 37.1 </td>
   <td style="text-align:right;width: 1.2 cm; "> 8.8 </td>
   <td style="text-align:right;width: 1.2 cm; "> 0.0 </td>
   <td style="text-align:right;width: 1.2 cm; "> 2.9 </td>
   <td style="text-align:right;width: 1.2 cm; "> 48.8 </td>
   <td style="text-align:left;width: 1.2 cm; "> -11.5 </td>
   <td style="text-align:right;width: 1.5 cm; "> 37.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 2.95 cm;  padding-left: 2em;" indentlevel="1"> Hydroelectric </td>
   <td style="text-align:right;width: 1.2 cm; "> 75 </td>
   <td style="text-align:right;width: 1.2 cm; "> 29.9 </td>
   <td style="text-align:right;width: 1.2 cm; "> 6.2 </td>
   <td style="text-align:right;width: 1.2 cm; "> 1.4 </td>
   <td style="text-align:right;width: 1.2 cm; "> 1.6 </td>
   <td style="text-align:right;width: 1.2 cm; "> 39.1 </td>
   <td style="text-align:left;width: 1.2 cm; "> NA </td>
   <td style="text-align:right;width: 1.5 cm; "> 39.1 </td>
  </tr>
</tbody>
<tfoot>
<tr><td style="padding: 0; border: 0;" colspan="100%"><span style="font-style: italic;">Note: </span></td></tr>
<tr><td style="padding: 0; border: 0;" colspan="100%">
<sup></sup> U.S. EIA Annual Energy Outlook 2019</td></tr>
</tfoot>
</table>

# Offshore Wind

As offshore wind is the primary technology being assessed in this research, we have explored many facets of offshore wind buildout. These facets are explored below, both at a regional and national cumulative level.

## Capacity Buildout

Cumulative and new addition offshore wind capacity across all nine census regions, by cost and emissions reduction scenario.

<img src="osw_Report_files/figure-html/unnamed-chunk-5-1.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-5-2.png" width="50%" />



## Total Capacity

Total offshore wind capacity across all nine census regions in 2050, by cost and emissions reduction scenario.

![](osw_Report_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>Offshore Wind Total Installed Capacity (GW): 2050</caption>
 <thead>
<tr>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Emissions<br>Reduction (%)</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="5"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Cost Reduction (%)</div></th>
</tr>
  <tr>
   <th style="text-align:left;">  </th>
   <th style="text-align:right;"> 40 </th>
   <th style="text-align:right;"> 50 </th>
   <th style="text-align:right;"> 60 </th>
   <th style="text-align:right;"> 70 </th>
   <th style="text-align:right;"> 80 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> BAU </td>
   <td style="text-align:right;"> 2.5 </td>
   <td style="text-align:right;"> 248.1 </td>
   <td style="text-align:right;"> 635.3 </td>
   <td style="text-align:right;"> 706.4 </td>
   <td style="text-align:right;"> 738.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 30 </td>
   <td style="text-align:right;"> 2.5 </td>
   <td style="text-align:right;"> 247.9 </td>
   <td style="text-align:right;"> 635.2 </td>
   <td style="text-align:right;"> 706.4 </td>
   <td style="text-align:right;"> 738.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:right;"> 2.1 </td>
   <td style="text-align:right;"> 268.0 </td>
   <td style="text-align:right;"> 635.2 </td>
   <td style="text-align:right;"> 706.4 </td>
   <td style="text-align:right;"> 738.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:right;"> 4.4 </td>
   <td style="text-align:right;"> 310.9 </td>
   <td style="text-align:right;"> 636.7 </td>
   <td style="text-align:right;"> 706.4 </td>
   <td style="text-align:right;"> 738.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 60 </td>
   <td style="text-align:right;"> 13.2 </td>
   <td style="text-align:right;"> 367.3 </td>
   <td style="text-align:right;"> 638.0 </td>
   <td style="text-align:right;"> 706.4 </td>
   <td style="text-align:right;"> 738.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 70 </td>
   <td style="text-align:right;"> 14.9 </td>
   <td style="text-align:right;"> 421.4 </td>
   <td style="text-align:right;"> 641.3 </td>
   <td style="text-align:right;"> 709.6 </td>
   <td style="text-align:right;"> 738.7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 80 </td>
   <td style="text-align:right;"> 24.9 </td>
   <td style="text-align:right;"> 435.4 </td>
   <td style="text-align:right;"> 656.5 </td>
   <td style="text-align:right;"> 723.1 </td>
   <td style="text-align:right;"> 746.0 </td>
  </tr>
</tbody>
</table>

## Output

Total offshore wind electricity output across all nine census regions, by cost and emissions reduction scenario.

![](osw_Report_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>Offshore Wind Total Output (PJ): 2050</caption>
 <thead>
<tr>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Emissions<br>Reduction (%)</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="5"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Cost Reduction (%)</div></th>
</tr>
  <tr>
   <th style="text-align:left;">  </th>
   <th style="text-align:right;"> 40 </th>
   <th style="text-align:right;"> 50 </th>
   <th style="text-align:right;"> 60 </th>
   <th style="text-align:right;"> 70 </th>
   <th style="text-align:right;"> 80 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> BAU </td>
   <td style="text-align:right;"> 42.3 </td>
   <td style="text-align:right;"> 3845.4 </td>
   <td style="text-align:right;"> 8962.0 </td>
   <td style="text-align:right;"> 9848.8 </td>
   <td style="text-align:right;"> 10204.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 30 </td>
   <td style="text-align:right;"> 43.4 </td>
   <td style="text-align:right;"> 3841.7 </td>
   <td style="text-align:right;"> 8960.8 </td>
   <td style="text-align:right;"> 9848.5 </td>
   <td style="text-align:right;"> 10204.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:right;"> 36.0 </td>
   <td style="text-align:right;"> 4139.4 </td>
   <td style="text-align:right;"> 8960.8 </td>
   <td style="text-align:right;"> 9848.5 </td>
   <td style="text-align:right;"> 10204.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:right;"> 75.3 </td>
   <td style="text-align:right;"> 4758.1 </td>
   <td style="text-align:right;"> 8977.9 </td>
   <td style="text-align:right;"> 9848.5 </td>
   <td style="text-align:right;"> 10204.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 60 </td>
   <td style="text-align:right;"> 221.0 </td>
   <td style="text-align:right;"> 5562.3 </td>
   <td style="text-align:right;"> 8992.0 </td>
   <td style="text-align:right;"> 9848.5 </td>
   <td style="text-align:right;"> 10204.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 70 </td>
   <td style="text-align:right;"> 250.4 </td>
   <td style="text-align:right;"> 6315.4 </td>
   <td style="text-align:right;"> 9034.2 </td>
   <td style="text-align:right;"> 9880.0 </td>
   <td style="text-align:right;"> 10205.3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 80 </td>
   <td style="text-align:right;"> 409.6 </td>
   <td style="text-align:right;"> 6502.2 </td>
   <td style="text-align:right;"> 9235.0 </td>
   <td style="text-align:right;"> 10008.3 </td>
   <td style="text-align:right;"> 10287.1 </td>
  </tr>
</tbody>
</table>

## Regions

Cumulative and new addition offshore wind capacity by region.

<img src="osw_Report_files/figure-html/unnamed-chunk-12-1.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-12-2.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-12-3.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-12-4.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-12-5.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-12-6.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-12-7.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-12-8.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-12-9.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-12-10.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-12-11.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-12-12.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-12-13.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-12-14.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-12-15.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-12-16.png" width="50%" />



Cumulative and new addition offshore wind capacity by region, emissions reduction, and cost reduction.

<img src="osw_Report_files/figure-html/unnamed-chunk-14-1.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-14-2.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-14-3.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-14-4.png" width="50%" />

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>Average Installed Capacity (GW)</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Region </th>
   <th style="text-align:right;"> 2050 Total </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> R4 </td>
   <td style="text-align:right;"> 0.67000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R6 </td>
   <td style="text-align:right;"> 9.56000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R1 </td>
   <td style="text-align:right;"> 29.88514 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R2 </td>
   <td style="text-align:right;"> 73.21452 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R9 </td>
   <td style="text-align:right;"> 93.38871 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R7 </td>
   <td style="text-align:right;"> 96.97286 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R3 </td>
   <td style="text-align:right;"> 110.37893 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R5 </td>
   <td style="text-align:right;"> 169.72857 </td>
  </tr>
</tbody>
</table>

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>Average Electricity Output (PJ)</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Region </th>
   <th style="text-align:right;"> 2050 Total </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> R4 </td>
   <td style="text-align:right;"> 5.85375 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R6 </td>
   <td style="text-align:right;"> 68.65600 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R1 </td>
   <td style="text-align:right;"> 96.99500 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R2 </td>
   <td style="text-align:right;"> 224.66953 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R9 </td>
   <td style="text-align:right;"> 284.79601 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R3 </td>
   <td style="text-align:right;"> 326.76062 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R7 </td>
   <td style="text-align:right;"> 485.47903 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R5 </td>
   <td style="text-align:right;"> 526.62200 </td>
  </tr>
</tbody>
</table>

# Grid Mix

## Baseline Production

Grid mix without any offshore wind cost reduction or emissions cap.

<img src="osw_Report_files/figure-html/unnamed-chunk-17-1.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-17-2.png" width="50%" />


## All Scenarios

Complete Set

![](osw_Report_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

Parsed Set

![](osw_Report_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

## Emissions Cap

BAU emissions

<img src="osw_Report_files/figure-html/unnamed-chunk-21-1.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-21-2.png" width="50%" />


30% emissions reduction

<img src="osw_Report_files/figure-html/unnamed-chunk-23-1.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-23-2.png" width="50%" />


40% emissions reduction

<img src="osw_Report_files/figure-html/unnamed-chunk-25-1.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-25-2.png" width="50%" />


50% emissions reduction

<img src="osw_Report_files/figure-html/unnamed-chunk-27-1.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-27-2.png" width="50%" />


60% emissions reduction

<img src="osw_Report_files/figure-html/unnamed-chunk-29-1.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-29-2.png" width="50%" />


70% emissions reduction

<img src="osw_Report_files/figure-html/unnamed-chunk-31-1.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-31-2.png" width="50%" />


80% emissions reduction

<img src="osw_Report_files/figure-html/unnamed-chunk-33-1.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-33-2.png" width="50%" />


## Cost Reductions

40% cost reduction

<img src="osw_Report_files/figure-html/unnamed-chunk-35-1.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-35-2.png" width="50%" />


50% cost reduction

<img src="osw_Report_files/figure-html/unnamed-chunk-37-1.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-37-2.png" width="50%" />


60% cost reduction

<img src="osw_Report_files/figure-html/unnamed-chunk-39-1.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-39-2.png" width="50%" />


70% cost reduction

<img src="osw_Report_files/figure-html/unnamed-chunk-41-1.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-41-2.png" width="50%" />


80% cost reduction

<img src="osw_Report_files/figure-html/unnamed-chunk-43-1.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-43-2.png" width="50%" />


## Heatmaps

<img src="osw_Report_files/figure-html/unnamed-chunk-45-1.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-45-2.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-45-3.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-45-4.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-45-5.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-45-6.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-45-7.png" width="50%" /><img src="osw_Report_files/figure-html/unnamed-chunk-45-8.png" width="50%" />


## Retirements and Additions

<img src="osw_Report_files/figure-html/unnamed-chunk-47-1.png" width="50%" />

## Grid Mix Changes Over Baseline




# TEST

![](osw_Report_files/figure-html/unnamed-chunk-50-1.png)<!-- -->![](osw_Report_files/figure-html/unnamed-chunk-50-2.png)<!-- -->

