

setwd("C:/Users/avork/Documents/CITIS/Som_Perepoliitika/VHToolaud")
load("vhsuurus_df.RData")

head(vhsuurus_df)
#meeste osakaal vanemahüvitise saajate seas

isadeosakaal <- vhsuurus_df %>%  dplyr::filter(!(ryhm %in% c("kokku"))) %>% 
  group_by(aasta, vhsuurus) %>%
  mutate(arvosakaal = arv/sum(arv)) %>%  
  dplyr::filter(ryhm =="isa", vhsuurus=="Kokku") %>% 
  ungroup() %>% dplyr::select(aasta, arvosakaal)

ggplot(isadeosakaal, aes(x = aasta, y = arvosakaal*100)) +
  geom_line() +
  geom_point() +
  labs(y = "Isade osakaal (%) vanemahüvitise saajate seas", x = "")


#Keskmine vanemahüvitis soo lõikes võrreldes eelmise kalendriaasta keskmise palgaga
keskminevh <- vhsuurus_df %>%  dplyr::filter(ryhm %in% c("ema", "isa"), vhsuurus =="Kokku") %>% 
  mutate(keskmhyvitis=ifelse(aasta<2011, summa/15.6466/arv, summa/arv))  
#%>% 
#  ggplot(aes(x=aasta, y = keskmhyvitis, color=ryhm)) +
#  geom_line()

#Keskmine EMTA palk!
# temp_df_tsd <- read_excel("C:/Users/avork/Documents/Andmed/EMTA/TSDkoondid/TSDvintagedata.xlsx", 
#                           sheet = "Sheet1")
# colnames(temp_df_tsd) <- c("andmetekpv", "aasta", "kuu", "palgatulu", "p_arv", "juht_tasu",
#                            "j_arv", "lep_tasu", "l_arv", "inarv")
# #aasta töötasu ja töötajate arv
#temp_df_tsd %>%  

#ESA keskmine palk
#Kokku pandud tabelitest 
#PA001: palk 2008-2018 ja SEM_ECO, kus oli kvartalite keskmised
esapalk <- read_excel("keskminepalk.xlsx")
esapalk$aasta = esapalk$aasta+1

keskminevh <- left_join(keskminevh, esapalk, by = "aasta")
keskminevh <- keskminevh %>%  mutate(keskmhyvitiss = keskmhyvitis/keskminepalk)

library(ggthemes)
keskminevh %>%  ggplot(aes(x=aasta, y = keskmhyvitiss, color=ryhm)) +
  geom_line() +
  theme_economist() + labs(x="", y = "Hüvitis võrreldes keskmise palgaga", color = "")

p1 <- keskminevh %>%  ggplot(aes(x=aasta, y = keskmhyvitis, color=ryhm)) +
  geom_line() +
  geom_point() +
  theme_economist() + 
  scale_x_continuous(breaks = c(2005, 2009, 2013, 2016, 2019)) +
  labs(x="", title = "Keskmine vanemahüvitis kuus", y = "eur", color = "") 

p2 <- ggplot(isadeosakaal, aes(x = aasta, y = arvosakaal*100)) +
  geom_line() +
  geom_point() +
  labs(title = "Isade osakaal vanemahüvitise saajate seas", x = "", y = "%") +
  theme_economist() +
  scale_x_continuous(breaks = c(2005, 2009, 2013, 2016, 2019))

library(ggpubr)
p <- ggarrange(p2, p1)
ggsave(filename = "vhsaajad.png", p, width = 10, height = 6)

pa <- annotate_figure(p, bottom = text_grob("Allikas: Sotsiaalkindlustusamet. Andmed aasta lõpu seisuga", size = 10, 
                                            vjust = -1.5, hjust = 1.25))
ggsave(filename = "vhsaajad2.png", pa, width = 10, height = 6)

#Pandud Facebooki 7.02.2020
#######################

### valemite joonistamine ----

vhparameetrid <- read_xlsx(path = "vanemahuvitiseparameetrid.xlsx", sheet = "et_eur") %>% 
  as.data.frame() 
#andmetabelina
vhparameetridt <- vhparameetrid %>% dplyr::select(-`Näitaja`) %>% 
  pivot_longer(names_to = "Aasta", values_to = "vaartus", cols = c(2:18)) %>% 
  pivot_wider(names_from = "lyhend", values_from = "vaartus")
#jätame välja aasta ja paneme selle reanimeks
parameetreid <- dim(vhparameetridt)[2]
vhparameetridm <- as.matrix(vhparameetridt[, 2:parameetreid])
row.names(vhparameetridm) <- vhparameetridt$Aasta


#### versioon 2 shiny'st ------------

palkylemine = 3000 #eurodes
lapsehooldustasu=700/15.6466

#Min ja max väärtuste aasta - poliitikaasta
Aasta = "2019"
#esialgne vanemahüvitis
vanhyv = 700
df <- data.frame(palk=seq(1, palkylemine, by = 1))
df2004 <- df %>% 
  mutate(aasta = 2004) %>% 
  #uus vanemahüvitis
  mutate(uusvanhyv = case_when(
    #kui palk on väiksem kui vanemahüvitise määr, siis ei muutu midagi
    palk <= vhparameetridm[Aasta, "vanhyvmaar"] ~ vanhyv,
    #kui palk on suurem kui vanemahüvitise määr aga väiksem kui 5kordne vanemahüvitise määr, siis rakendub vähendamise valem
    palk >= vhparameetridm[Aasta, "vanhyvmaar"] & palk <=5*vhparameetridm[Aasta, "vanhyvmaar"] ~ (vanhyv+palk)/vhparameetridm["2004", "vahendamisemaar"]- palk,
    #Kui palk on suurem kui 5kordne vanemahüvitise määr, siis on uus hüvitis 0
    palk >= 5*vhparameetridm[Aasta, "vanhyvmaar"] ~ 0)) %>% 
  #kokku brutotulu
  mutate(brutotulukokku= uusvanhyv+palk)
df2008 <- df %>%  
  mutate(aasta = 2008) %>% 
  #uus vanemahüvitis 2008
  mutate(uusvanhyv = case_when(
    #kui palk on väiksem kui vanemahüvitise määr, siis ei muutu midagi
    palk <= vhparameetridm[Aasta, "vanhyvmaar"] ~ vanhyv,
    #kui palk on suurem kui vanemahüvitise määr aga väiksem kui 5kordne vanemahüvitise määr, siis rakendub vähendamise valem
    #Vanemahüvitis ei tohi minna väiksemaks kui lapsehooldustasu
    palk >= vhparameetridm[Aasta, "vanhyvmaar"] & palk <=5*vhparameetridm[Aasta, "vanhyvmaar"] ~
      pmax(lapsehooldustasu, (vanhyv+(palk - vhparameetridm[Aasta, "vanhyvmaar"]))/vhparameetridm["2008", "vahendamisemaar"] - (palk - vhparameetridm[Aasta, "vanhyvmaar"])),
    #Kui palk on suurem kui 5kordne vanemahüvitise määr, siis on uus hüvitis lapsehooldustasu
    palk >= 5*vhparameetridm[Aasta, "vanhyvmaar"] ~ lapsehooldustasu)) %>% 
  #kokku brutotulu
  mutate(brutotulukokku= uusvanhyv+palk)
df2014 <- df %>%  
  mutate(aasta = 2014) %>% 
  #uus vanemahüvitis 2014
  mutate(uusvanhyv = case_when(
    #kui palk on väiksem kui vanemahüvitise määr, siis ei muutu midagi
    palk <= vhparameetridm[Aasta, "vanhyvmaar"] ~ vanhyv,
    #kui palk on suurem kui vanemahüvitise määr, siis rakendub vähendamise valem
    #Vanemahüvitis ei tohi minna väiksemaks kui pool algset vanemahüvitist
    #Endale kontroll: kas pool vanemahüvitise määras vanemahüvitist oleks alati suurem kui lapsehooldustasu
    palk >= vhparameetridm[Aasta, "vanhyvmaar"] ~ 
      pmax(0.5*vanhyv, vanhyv - (palk - vhparameetridm[Aasta, "vanhyvmaar"])/vhparameetridm["2014", "vahendamisemaar"]))) %>% 
  #kokku brutotulu
  mutate(brutotulukokku= uusvanhyv + palk)

df2018 <- df %>%  
  mutate(aasta = 2018) %>% 
  #uus vanemahüvitis 2018
  mutate(uusvanhyv = case_when(
    #kui palk on väiksem kui pool maksimaalsest vanemahüvitisest, siis ei muutu midagi
    palk <= 0.5*vhparameetridm[Aasta, "maksimum"] ~ vanhyv,
    #kui palk on suurem kui pool maksimaalsest vanemahüvitisest, siis rakendub vähendamise valem
    #Vanemahüvitis ei tohi minna väiksemaks kui pool algset vanemahüvitist
    #Endale kontroll: kas pool vanemahüvitise määras vanemahüvitist oleks alati suurem kui lapsehooldustasu
    palk >= 0.5*vhparameetridm[Aasta, "maksimum"] ~ 
      pmax(0.5*vanhyv, vanhyv - (palk - 0.5*vhparameetridm[Aasta, "maksimum"])/vhparameetridm["2018", "vahendamisemaar"]))) %>% 
  #kokku brutotulu
  mutate(brutotulukokku= uusvanhyv + palk)

dfkoos <- bind_rows(df2004, df2008, df2014, df2018)
dfpikk <- dfkoos %>% 
  pivot_longer(cols = c(uusvanhyv, brutotulukokku),
               names_to = c("tunnus"),
               values_to = c("vaartus"))
nimedjoonisele <- c('brutotulukokku' =  'Töötasu ja VH kokku',
                    'uusvanhyv' =  'Vanemahüvitis töötamise ajal')
dfpikk$tunnus <- factor(dfpikk$tunnus,  levels = c("uusvanhyv", "brutotulukokku"))

dfpikk <- dfpikk %>%  
  mutate(periood = case_when(
    aasta == "2004" ~ "1/2004 - 8/2007",
    aasta == "2008" ~ "9/2007 -12/2013",
    aasta == "2014" ~ "1/2014 - 2/2018",
    aasta == "2018" ~ "3/2018 - 6/2020"
  ))
dfpikk$periood <- factor(dfpikk$periood,  
                         levels = c("1/2004 - 8/2007", 
                                    "9/2007 -12/2013",
                                    "1/2014 - 2/2018",
                                    "3/2018 - 6/2020"))

ggplot(dfpikk, 
       aes(x = palk, y = vaartus, color = factor(periood), group=factor(periood))) +
  geom_line() +
  scale_y_continuous(limits = c(0, NA)) +
  facet_wrap(~tunnus, scales = "free_y", labeller = as_labeller(nimedjoonisele)) + 
  labs(color = "", x = "Vanemahüvitise saamise ajal teenitav brutopalk",
       y = "Eur kuus", 
       title = "Samaaegse töötamise mõju vanemahüvitise suurusele ja kogu brutotulule läbi aastate",
       subtitle = paste0("Sinu valitud esialgne vanemahüvitis ", vanhyv, " eurot. \nVanemahüvitise määr ", round(vhparameetridm[Aasta, "vanhyvmaar"],1), " eurot. \nVanemahüvitise maksimum ", round(vhparameetridm[Aasta, "maksimum"],1), " eurot."))  
ggplot(dfpikk, 
       aes(x = palk, y = vaartus, color = factor(periood), group=factor(periood))) +
  geom_line() +
  scale_y_continuous(limits = c(0, NA)) +
  facet_wrap(~tunnus, scales = "free_y", labeller = as_labeller(nimedjoonisele), nrow = 2) + 
  labs(color = "", x = "Vanemahüvitise saamise ajal teenitav brutopalk",
       y = "Eur kuus", 
       title = "Samaaegse töötamise mõju vanemahüvitise suurusele ja kogu brutotulule läbi aastate",
       subtitle = paste0("Sinu valitud esialgne vanemahüvitis ", vanhyv, " eurot. \nVanemahüvitise määr ", round(vhparameetridm[Aasta, "vanhyvmaar"],1), " eurot. \nVanemahüvitise maksimum ", round(vhparameetridm[Aasta, "maksimum"],1), " eurot.")) +
  theme_economist()

#FBsse
p3 <- ggplot(dfpikk %>%  dplyr::filter(tunnus =="uusvanhyv"), 
       aes(x = palk, y = vaartus, color = factor(periood), group=factor(periood))) +
  geom_line(size=2) +
  scale_y_continuous(limits = c(0, 800)) +
  #facet_wrap(~tunnus, scales = "free_y", labeller = as_labeller(nimedjoonisele), nrow = 2) + 
  labs(color = "", x = "Vanemahüvitise saamise ajal teenitav brutopalk",
       y = "Eur kuus", 
       title = "Samaaegse töötamise mõju vanemahüvitise brutosuurusele eri perioodidel",
       caption = "2019. aasta vanemahüvitise määr, esialgne vanemahüvitis €700") +
  theme_economist() + theme(legend.position = "right")

p3

#Töötajate osakaal üle aastate
load("vhtootaminepikk.RData")
temp <- vhtootaminepikk %>% dplyr::filter(as.character(tootab) %in%
                                            c("Mittetöötavad", "Töötasu kuni alammäär", "Töötasu üle alammäära")) 
temp$tootab <- factor(temp$tootab,
                               levels = c("Töötasu üle alammäära", "Töötasu kuni alammäär", "Mittetöötavad"))
p4 <- ggplot(temp %>%  filter(sugu=="mehed"),
             aes(x=as.Date(kpv), y = vaartus, fill=tootab)) +
  geom_area()+
  #facet_wrap(~sugu) +
  #facet_wrap(~sugu, scales = "free_y") +
#  theme(legend.position="bottom", axis.text.x = element_text(angle = 45),text = element_text(size=12))
  theme_economist() + theme(legend.position = "right") +
  geom_vline(xintercept = as.Date(c("2018-03-01", "2014-01-01", "2007-09-01")),
             linetype="dotted", 
             color = "black", size=1) +
  labs(x="", y = "", fill = "",
       title = "Vanemahüvitise saajad mehed samaeaegse töötamise järgi")

#labs(x="", y = "", fill = "",
#     title = "Vanemahüvitise saajad kalendrikuu alguse seisuga töötasu suuruse järgi")

p <- ggarrange(p4, p3, nrow = 2, ncol=1)
p
ggsave(filename = "vhjatootaminemehed.png", p, width = 10, height = 6)
ggsave(filename = "vhjatootaminemehed.png", p, width = 12, height = 8)


ggsave(filename = "vhjatootaminemehedylemine.png", p4, width = 12, height = 8)



pa <- annotate_figure(p, bottom = text_grob("Allikas: Sotsiaalkindlustusamet. Andmed aasta lõpu seisuga", size = 10, 
                                            vjust = -1.5, hjust = 1.25))
ggsave(filename = "vhsaajad2.png", pa, width = 10, height = 6)





