#Logi
#27.12.2019 Andres Võrk
#06.02.2020 AV

#andmete sisselaadimine


library(dplyr)
library(readxl)
library(knitr)
library(shiny)
library(shinydashboard)
library(shinyWidgets) 
#library(scales)
library(ggplot2)
library(plotly)
library(tidyr)
library(DT)
library(kableExtra)


#VH parameetrid -------------------
#Seda kasutab kahes kohas
#1) parameetrite tabeli "vhparameetridtabelisse" kuvamisel, server.R output$parameetrid_tabelis
#2) parameetrite maatriksis "vhparameetridm", kui arvutab välja eelarvejooni, vt 

#vhparameetrid <- read_xlsx(path = "data/vanemahüvitiseparameetrid.xlsx", sheet = "et_eur") %>% 

vhparameetrid <- read_xlsx(path = "data_app/vanemahuvitiseparameetrid.xlsx", sheet = "et_eur") %>% 
  as.data.frame()

vhparameetridtabelisse <- vhparameetrid %>%  dplyr::select(-lyhend) %>% 
  mutate_at(vars(2:(length(vhparameetrid)-1)), round, 1)

# #esimesed read ümardame 0 kohta peale koma
# vhparameetridtabelisse1 <- temp[1:nrow(vhparameetrid)-1,] %>% 
#   mutate_at(vars(2:length(vhparameetridtabelisse)), round)
# #Viimase rea ümardame 1 koht peale koma
# vhparameetridtabelisse2 <- temp[nrow(vhparameetrid),] %>% 
#   mutate_at(vars(2:length(vhparameetridtabelisse)), round, 1)
# vhparameetridtabelisse <- bind_rows(vhparameetridtabelisse1, vhparameetridtabelisse2)

#andmetabelina
vhparameetridt <- vhparameetrid %>% dplyr::select(-`Näitaja`) %>% 
  pivot_longer(names_to = "Aasta", values_to = "vaartus", cols = c(2:18)) %>% 
  pivot_wider(names_from = "lyhend", values_from = "vaartus")

#Kasutan siiski maatriksit
#Vt ka https://www.dummies.com/programming/r/how-to-name-matrix-rows-and-columns-in-r/
#jätame välja aasta ja paneme selle reanimeks
parameetreid <- dim(vhparameetridt)[2]
vhparameetridm <- as.matrix(vhparameetridt[, 2:parameetreid])
row.names(vhparameetridm) <- vhparameetridt$Aasta


#Kogukulud
load("data_app/toetusedkokku.RData")
#Töötamine
load("data_app/vhtootaminepikk.RData")
load("data_app/vhsuurus_df.RData")
load("data_app/syndjasm.RData")
load("data_app/vhempl.RData")
#VH saajate struktuur 2004-2007 vanuse ja laste arvu järgi
load("data_app/vhtabel1grprtype.RData")
#VH saajate struktuur vanuse järgi
load("data_app/vhtabel1grprtypevanusgr.RData")


#Keskmine hõivemäär laste arvu, aja ja lapse sünniaasta järgi
vhemplgr <- vhempl %>% group_by(timebirth, befchild, byearch) %>% 
  summarise(employed=mean(employed)) %>% 
  ungroup() 


#rmdfiles <- c("vanemahyvitisjatootaminevalemid_ver3.Rmd")
#sapply(rmdfiles, knit, quiet = T)

# ui <- shinyUI(
#   fluidPage(
#     withMathJax(includeMarkdown("vanemahyvitisjatootaminevalemid_ver2.md"))
#   )
# )
