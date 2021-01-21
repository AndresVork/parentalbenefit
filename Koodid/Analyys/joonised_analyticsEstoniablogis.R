#setwd("C:/Users/avork/Documents/CITIS/Som_Perepoliitika/VHToolaud")
setwd("C:/Users/avork/OneDrive - Tartu Ülikool/CITIS/Som_Perepoliitika/VHToolaud")
#Andres Võrk

library(dplyr)
library(readxl)
library(ggplot2)
library(ggthemes)

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

palkylemine = 3000 #eurodes
lapsehooldustasu=700/15.6466

#Min ja max väärtuste aasta - poliitika aasta
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
  mutate(brutotulukokku= uusvanhyv+palk) # %>% 
  #kokku netotulu
  #mutate(brutotulukokku= uusvanhyv+palk) %>% 
  

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
#Arvude vaatamiseks 20.08.2020
#View(dfpikk %>%  filter(palk==2000))

# ggplot(dfpikk, 
#        aes(x = palk, y = vaartus, color = factor(periood), group=factor(periood))) +
#   geom_line() +
#   scale_y_continuous(limits = c(0, NA)) +
#   facet_wrap(~tunnus, scales = "free_y", labeller = as_labeller(nimedjoonisele)) + 
#   labs(color = "", x = "Vanemahüvitise saamise ajal teenitav brutopalk",
#        y = "Eur kuus", 
#        title = "Samaaegse töötamise mõju vanemahüvitise suurusele ja kogu brutotulule läbi aastate",
#        subtitle = paste0("Sinu valitud esialgne vanemahüvitis ", vanhyv, " eurot. \nVanemahüvitise määr ", round(vhparameetridm[Aasta, "vanhyvmaar"],1), " eurot. \nVanemahüvitise maksimum ", round(vhparameetridm[Aasta, "maksimum"],1), " eurot."))  
# 
# ggplot(dfpikk, 
#        aes(x = palk, y = vaartus, color = factor(periood), group=factor(periood))) +
#   geom_line() +
#   scale_y_continuous(limits = c(0, NA)) +
#   facet_wrap(~tunnus, scales = "free_y", labeller = as_labeller(nimedjoonisele), nrow = 2) + 
#   labs(color = "", x = "Vanemahüvitise saamise ajal teenitav brutopalk",
#        y = "Eur kuus", 
#        title = "Samaaegse töötamise mõju vanemahüvitise suurusele ja kogu brutotulule läbi aastate",
#        subtitle = paste0("Sinu valitud esialgne vanemahüvitis ", vanhyv, " eurot. \nVanemahüvitise määr ", round(vhparameetridm[Aasta, "vanhyvmaar"],1), " eurot. \nVanemahüvitise maksimum ", round(vhparameetridm[Aasta, "maksimum"],1), " eurot.")) +
#   theme_economist()

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

source("C:\\Users\\avork\\Documents\\kommentaarid\\Facebookijooniseid\\Rggplotwebiteemad.R")
source("http://kodu.ut.ee/~avork/files/Rkoode/Rggplotwebiteemad.R")

p3 + theme_web_classic()
#teine joonis Wordpressis
ggsave(filename = "vhvalemid.png", width = 10, height = 7)


#19.08.2020--------------- 
#Töötavad isad
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
       title = "Vanemahüvitist saavad mehed samaaegse töötamise järgi")
p4

p4 + theme_web_classic() + labs(caption = "Punktiirjoon näitab seaduste muutumise aega \nAllikas: Sotsiaalkindlustusamet, viimati vaadatud 15.08.2020")
#kolmas joonis wordpressis
ggsave(filename = "isadetootamine.png", width = 10, height = 7)

#FBsse
p4 + theme_web_classic() + theme(legend.position = c(0.2, 0.9)) +
  labs(caption = "Punktiirjoon näitab seaduste muutumise aega \nAllikas: Sotsiaalkindlustusamet, viimati vaadatud 24.10.2020")
ggsave(filename = "isadetootaminefb.png", width = 10, height = 7)
ggsave(filename = "isadetootaminefb.png", width = 9, height = 7)


##Kulud kokku------------
load("toetusedkokku.RData")
ggplot(toetusedkokku %>%  dplyr::filter(toetuseliik != "VHjaperetoetuskokku", tunnus == "vaartus"),
  aes(x = aasta, y = vaartus/1000, fill = toetuseliik))  +
  geom_col() +
  labs(title="Rahalised toetused lastega peredele", 
         y="Mln eurot", 
         x="", fill = "") +
  theme_web_classic()

#Kolmas joonis Wordpressis
ggsave(filename = "kuludeur.png", width = 10, height = 7)

# plot_ly(toetusedkokku %>%  dplyr::filter(toetuseliik != "VHjaperetoetuskokku", tunnus == "vaartus")) %>% 
#   add_trace(data = toetusedkokku %>%  dplyr::filter(toetuseliik != "VHjaperetoetuskokku", tunnus == "vaartus"), 
#             type = "bar", 
#             x = ~aasta, 
#             y = ~vaartus/1000, 
#             color = ~toetuseliik) %>%  
#   layout(barmode = "stack", title="Rahalised toetused lastega peredele", 
#          yaxis=list(title="Mln eurot"), 
#          xaxis=list(title="", dtick = 1))

