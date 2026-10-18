# The Greatest Threat to Pedestrians on Britain’s Pavements


Code to produce numbers and plots used in article for the Urban Cycling
Institute (UCI) on Pavement deaths in Great Britain.

Download and install R and Studio. Opening a .R file in Rstudio will
prompt installation of packages and on accepting will install, the
scripts should then be able to be run.

Grateful to anyone who spots any errors/has improvements. Please raise
an issue in this repo.

## STATS19

![](outputs/stats19/sev_year.png) Output of reproducible code for
individual stats: pedestrian casualties recorded in 2025: 19978<br>
pedestrian casualties recorded in 2024: 19176<br> difference = 802<br>
pedestrian casualties on pavement in 2025: 2628<br> pedestrian
casualties on pavement in 2024: 2628<br> number of bicycle, e-scooter,
and mobility scooter collisions on pavements 2025: 405<br> number of
bicycle, e-scooter, and mobility scooter collisions on pavements 2021:
278<br> number of motor vehicle collisions on pavements 2025: 2343<br>
number of motor vehicle collisions on pavements 2021: 1972<br> % of
vehicles involved in all pavement collisions that were motor vehicles in
2025: 1<br> fatal pedestrian casualties on pavement in 2025: 29<br>
fatal pedestrian casualties on pavement in 2024: 45<br> % of all fatal
pavement collisions that involved a motor car: 96<br> % of all fatal
pavement collisions that involved an unknown: 1<br> % of all fatal
pavement collisions that involved a Bicycle, e-scooter or mobility
scooter: 2<br> % of people KSI who were in private cars between 2021 and
2025: 37.9<br> % of people KSI were were on an escooter between 2021 and
2025: 1.5<br> % of escooter collisions also involving a motor vehicle:
70<br> Under 16s KSI on escooters between 2021 and 2025: 481<br> Under
16s KSI inside private cars between 2021 and 2025: 2796<br> Under 16s
KSI as pedestrians between 2021 and 2025: 6815<br> Pavement fatalities
since 09-07-2023: 95

![](outputs/stats19/vehicles_pavements.png)
![](outputs/stats19/av_pavement_fatalities.png)

## National travel survey plots

nts_plots.R downloads the national travel survey data and generates the
plots for all categories. In this case the interest was younger person
female walking rates. ![](outputs/nts/activity/index_Walking_Female.png)
![](outputs/nts/activity/total_Walking_Female.png)

## Homicide rates vs STATS19

ons_homicide.R downloads the ONS homicide rates and produces the street
crime data and also recalculates STATS19 data only including England and
Wales and for the same period as the crime data, which is April to March
of each year.

## Energy

### Sankey

sankey.R creates a diagram showing the kinetic energy needed to move a
1500kg car and 80kg person plus the chemical energy needed to do so with
a 25% efficient internal combustion engine. Can be adjusted for
different vehicles, persons and efficiencies.
![](outputs/energy/sankey.png)

### Interactive calculator

energy_transport_shiny.R is a shiny app that enables interactive inputs
of data and creation of vehicles.
https://blaisekelly.github.io/me/nrg_transport.html
