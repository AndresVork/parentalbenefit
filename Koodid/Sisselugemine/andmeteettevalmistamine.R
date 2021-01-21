#Andmete ettevalmistamine vanemahüvitise töölaua jaoks
#Andres võrk, TÜ
#Alustatud 20.dets 2019
# - 28.dets 2019
#Logi

# 9.juuli 2020 - andmete uuendamine, lisandusid 2019 andmed ESA andmebaasist (enne olid käsitsi lisatud)
# 

#TODO! list
#- Uuri, mis oli tabel SK22 Vanemahüvitise saajad ja mis erinevus SKA andmetega - tehtud
#- tee ka meeste osakaalu joonised igakuiselt määratud hüvitiste järgi - tehtud
#- vanemahüvitise ja töötamise valemid näidississetulekuga   - tehtud



#Allolevat koodi peab kohandama vastavalt sellele, kuidas lähteandmete failide kuju muutub.
#Osad tunnused võetakse sisse Statistikaameti andmebaasist
#Andmed salvestatakse töölaua kataloogi alamkataloogi /data

#paketid
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)

#projekti kaust
#failide algne asukoht, muuda seda
sourcedatapath = "C:/Users/avork/Documents/CITIS/Som_Perepoliitika/VHToolaud/lahteandmed/"
#töölaua andmete asukoht, muuda seda
dashboarddatapath = "C:/Users/avork/Documents/CITIS/Som_Perepoliitika/VHToolaud/data/"


#Vanemahüvitise reeglid --------------
vhparameetrid <- read_xlsx(path = paste0(sourcedatapath, "vanemahüvitiseparameetrid.xlsx"), sheet = "et_eur") %>% 
  as.data.frame()
vhparameetrid <- vhparameetrid %>% 
  mutate_at(vars(2:length(vhparameetrid)), round) #digits = 2

vhparameetrid_long <- vhparameetrid %>% 
  pivot_longer(cols = c(-`Näitaja`),names_to = "Aasta", values_to="vaartus") 


# Kulud ------------------
#Andmed ESA andmebaasist
#1. SK17: Peretoetused liigi järgi 
source("https://raw.githubusercontent.com/AndresVork/rsdmx_esa/master/rsdmx_esa_fn.R")
tempsummad <- rsdmx_esa("SK17")

#Vana versioon - vigane, sest tulid juurde uued alamkategooriad 2019
# vhjaperetoetused <- tempsummad %>% 
#   mutate(aasta = as.numeric(obsTime),
#          vaartus = obsValue,
#          toetuseliik = case_when(
#            DIM2label.et == "Vanemahüvitis" ~ "Vanemahüvitis",
#            DIM2label.et == "Toetused kokku" ~ "VHjaperetoetuskokku",
#            TRUE ~ "Peretoetused")) %>% 
#   filter(DIM1label.et =="Väljamakstud toetused, tuhat eurot",
#          #alalõiked välja
#          !(DIM2label.et %in% c("..sünnitoetus esimesele lapsele",
#                                "..sünnitoetus igale järgmisele lapsele",
#                                "..sünnitoetus mitmikele")),
#          aasta>=2000 & aasta <=2018) %>%  #2019 aasta oli ekslik lapsetoetuse osas, mida ei olnud ESA andmebaasis 09.07.2020
#   group_by(toetuseliik, aasta) %>% 
#   summarise(vaartus = sum(vaartus))

#09.07.2020 - ESA andmebaasis muutus ridade struktuur
#Tegin teist moodi. Võtan summa kokku ja lahutan vanemahüvitise maha, et leida peretoetused.
vhjaperetoetused <- tempsummad %>% 
  mutate(aasta = as.numeric(obsTime),
         vaartus = obsValue) %>% 
  filter(DIM1label.et =="Väljamakstud toetused, tuhat eurot",
         DIM2label.et %in% c("Vanemahüvitis", "Toetused kokku"),
         aasta>=2000 & aasta <=2019) %>% 
  rename(toetuseliik = DIM2label.et) %>% 
  dplyr::select(aasta, vaartus, toetuseliik) %>% 
  spread(key = "toetuseliik", value = "vaartus", fill = 0)  %>% 
  rename(VHjaperetoetuskokku = `Toetused kokku`) %>% 
  mutate(Peretoetused = VHjaperetoetuskokku - Vanemahüvitis) %>% 
  gather(key = "toetuseliik", value = "vaartus", -aasta)


#Siis kui veel ei olnud 2019 andmeid ESAs
#Lisame käsitsi SKA 2019 aruandest numbrid
#https://www.sotsiaalkindlustusamet.ee/sites/default/files/content-editors/Statistika/rsk_koond_2019_12_kuud.xlsx
# vhjaperetoetused2019 <- data.frame(toetuseliik = c("Peretoetused", "Vanemahüvitis", "VHjaperetoetuskokku"),
#                                    aasta = c(2019, 2019, 2019),
#                                    vaartus = c(564892.18048-262064.9, 262064.9, 564892.18048), stringsAsFactors = FALSE)
# vhjaperetoetused <- bind_rows(vhjaperetoetused, vhjaperetoetused2019)

#Sünnitushüvitus
tempsynnsummad <- rsdmx_esa("SK16")

synnitushyvitised <- tempsynnsummad %>% 
  mutate(aasta = as.numeric(obsTime),
         vaartus = obsValue,
         toetuseliik = "Sünnitushüvitis") %>% 
  filter(DIM1label.et =="Ravikindlustuse väljamaksed, tuhat eurot",
         DIM2label.et == "..sünnitushüvitis",
         aasta>=2000) %>% 
  dplyr::select(aasta, vaartus, toetuseliik) %>%  arrange(aasta)
  #save(synnitushyvitised, file = paste0(dashboarddatapath, "synnitushyvitised.RData"))


#09.07.2020 - välja kommenteeritud, sest 2019 andmed olid siis juba andmebaasis
#Lisame käsitsi HK 2020 eelarve dokumendist tegelikud kulud 2019 kohta
#https://www.haigekassa.ee/sites/default/files/eelarve/EHK_eelarve_2020_kodukale.pdf
#Tabel 17
# synnitushyvitised2019 <- data.frame(toetuseliik = c("Sünnitushüvitis"),
#                                   aasta = c(2019),
#                                   vaartus = c(56737), stringsAsFactors = FALSE)
# synnitushyvitised <- bind_rows(synnitushyvitised, synnitushyvitised2019)

toetusedkokku <- bind_rows(vhjaperetoetused, synnitushyvitised)

#Nominaalne SKP suhtarvuks
tempskp <- rsdmx_esa("RAA0012")
skp <- tempskp %>%  
  filter(DIM2label.et == "I-IV kvartal",
         DIM3label.et == "Sesoonselt ja tööpäevade arvuga korrigeerimata",
         DIM4label.et == "SKP jooksevhindades, miljonit eurot") %>% 
  mutate(aasta = as.numeric(obsTime),
          skpvaartus = obsValue) %>% 
  dplyr::select(aasta, skpvaartus)

toetusedkokku <- left_join(toetusedkokku, skp, by = "aasta") 

toetusedkokku <- toetusedkokku %>% 
  mutate(skpsuhtes = vaartus/1000/skpvaartus)

# toetusedkokku <- toetusedkokku %>% dplyr::select(-skpvaartus) %>% 
#   pivot_longer(cols = c(vaartus, skpsuhtes), names_to = "tunnus", values_to = "vaartus")
# 
# save(toetusedkokku, file = paste0(dashboarddatapath, "toetusedkokku.RData"))

#0-16 aastaste laste arv ja elanike arv
#Selleks, et leida kaks suhtarvu. Toetused ühe lapse kohta ja toetus ühe lapse kohta võrreldes SKP /inimese kohta
#: Rahvastik aasta alguses ja aastakeskmine rahvaarv soo ja vanuse järgi
temppop <- rsdmx_esa("RV0212")
rahvastik <- temppop %>% 
  mutate(tunnus = case_when(
    as.numeric(DIM2label.et)<=16 ~ "lapsed", 
    DIM2label.et=="Kokku" ~ "inimesed", 
    TRUE ~ "muu"),
    aasta = as.numeric(obsTime)) %>% 
  dplyr::filter(DIM4label.et=="Mehed ja naised", DIM3label.et =="Aastakeskmine rahvaarv",
                tunnus %in% c("lapsed", "inimesed")) %>% 
  group_by(aasta, tunnus) %>% 
  summarise(arv = sum(obsValue)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = tunnus, values_from = arv)
    
toetusedkokku <- left_join(toetusedkokku, rahvastik, by = "aasta") 
toetusedkokku <- toetusedkokku %>% 
  mutate(lapsekohta = vaartus*1000/lapsed,
         lapsekohtavsskppc = (vaartus*1000/lapsed)/(skpvaartus*1000000/inimesed))

toetusedkokku <- toetusedkokku %>% dplyr::select(-skpvaartus, -lapsed, -inimesed) %>%
  pivot_longer(cols = c(vaartus, skpsuhtes, lapsekohta, lapsekohtavsskppc), 
               names_to = "tunnus", values_to = "vaartus")

save(toetusedkokku, file = paste0(dashboarddatapath, "toetusedkokku.RData"))

 
# Vanemahüvitise saajad soo ja suuruse lõikes ESA andmebaasist aastase seisuga (?)
tempvh <- rsdmx_esa("SK22")
vhsuurusesa <- tempvh %>% 
  mutate(aasta = as.numeric(obsTime),
         sugu = DIM3label.et,
         vaartus= obsValue,
         suurus = DIM1label.et) %>% 
  dplyr::select(aasta, sugu, suurus, vaartus)
# ggplot(vhsuurusesa %>%  filter(suurus != "Kokku"), aes(x= aasta, y = vaartus, fill = suurus)) +
#          geom_area() +
#   facet_wrap(~sugu)


# Vanemahüvitise saajad ja samaaegne töötamine SKA andmetest -------------------
vhtootamine <- read_xlsx(path = paste0(sourcedatapath, "VHjasamaaegnetootamine.xlsx"), sheet = "koos")

#nimede saamiseks 
#paste0(colnames(vhtootamine), collapse =", ")

vhtootamine <- vhtootamine %>%  
  mutate_at(vars(koik_kokku:tootavadkuniviisalammaara_mehed),
            .fun=as.numeric)

save(vhtootamine, file = paste0(dashboarddatapath, "vhtootamine.RData"))

#Pikale kujule
#5 töötamise näitajat: kokku, töötab, kunialammaar, ulealammaar, kuniviisalammaar
#3 sugu: kokku, mehed, naised

vhtootaminepikk <- vhtootamine %>% 
  pivot_longer(cols = c(-kpv),
               names_to = c("tootab", "sugu"),
               names_pattern = "(.*)_(.*)",
               values_to = c("vaartus"))
#View(vhtootaminepikk)
#Teeme juurde kategooria mitte-töötavad = koik - tootavad
mittetootavad <- vhtootaminepikk %>% filter(tootab %in% c("koik", "tootavad")) %>% 
  pivot_wider(names_from = "tootab", values_from = "vaartus") %>%  
  mutate(mittetootavad = koik-tootavad) %>% 
  pivot_longer(names_to = "tootab", values_to = "vaartus", 
               cols = c(koik, tootavad, mittetootavad)) %>% 
  filter(tootab =="mittetootavad")
vhtootaminepikk <- bind_rows(vhtootaminepikk, mittetootavad) %>%  arrange(kpv, sugu, tootab)

vhtootaminepikk$tootab <- factor(vhtootaminepikk$tootab,
                                 levels = c("koik", "mittetootavad", "tootavad",
                                            "tootavadkunialammaar",
                                            "tootavadulealammaar",
                                            "tootavadkuniviisalammaara"),
                                 
                                 labels = c('Kõik', "Mittetöötavad", 'Töötavad',
                                            'Töötasu kuni alammäär',
                                            'Töötasu üle alammäära',
                                            'Töötasu kuni viis alammäära'))

save(vhtootaminepikk, file = paste0(dashboarddatapath, "vhtootaminepikk.RData"))

#Kontrolljoonised töötamine -----------

# p1 <- ggplot(vhtootaminepikk, aes(x=as.Date(kpv), y = vaartus, color=sugu)) +
# geom_line()+
# facet_wrap(~tootab, scales = "free_y")
# 
# ggplotly(p1)


# vhtootamisenimed <- c(koik =  'Kõik',
#                       tootavad =  'Töötavad',
#                       tootavadkunialammaar = 'Töötasu kuni alammäär',
#                       tootavadulealammaar =  'Töötasu üle alammäära',
#                       tootavadkuniviisalammaara =  'Töötasu kuni viis alammäära')

# ggplot(vhtootaminepikk, aes(x=as.Date(kpv), y = vaartus, color=sugu)) +
#     geom_line()+
#     facet_wrap(~tootab, scales = "free_y", labeller = as_labeller(vhtootamisenimed)) +
#     labs(x="", y = "", color = "", title = "Vanemahüvitise saajad kalendrikuu alguse seisuga töötasu suuruse järgi")


# Vanemahüvitise saajad ja eelnev töötasu SKA andmetest -------------------
# Käsitsi kokku pandud Exceli tabelitest aasta lõpu seisuga määratud vanemahüvitised

#https://rpubs.com/tf_peterson/readxl_import
#library(readxl)
#Faili nimi
xl_data <- paste0(sourcedatapath, "Maaratud_VH_Liikide_loikes.xlsx")
#lehtede nimed
tab_names <- excel_sheets(path = xl_data)
#kõik aastad ühe käsuga - võib olla saaks ka seda kasutada
# list_all <- lapply(tab_names, function(x) read_excel(path = xl_data, sheet = x, range = "A8:C43",
#                                                      col_names = c("naitaja", "arv", "summa")))
reanimed <- read_excel(path = paste0(sourcedatapath, "Maaratud_VH_Liikide_loikes_ryhmadenimed.xlsx"),
                        sheet = "Sheet1", range = "A1:B38")

#Teeme siiski tsükliga
#Tühi andmestik
vhsuurus_df <- data.frame(arv = as.numeric(),
                          summa = as.numeric(),
                          aasta = as.numeric(),
                          ryhm = as.character(),
                          vhsuurus = as.character(), stringsAsFactors = FALSE)
#Käime läbi kõik Exceli lehed ja võtame arv ja summa veerud
for (i in 1:length(tab_names)) {
  print(tab_names[i])
  tempvhliik <- read_excel(path = xl_data, sheet = tab_names[i], range = "B8:C44", 
                           col_names = c("arv", "summa"))
  #teeme aasta muutuja
  tempvhliik$aasta <- as.numeric(tab_names[i])
  #paneme juurde rea nimed eraldi tabelist
  tempvhliik <- bind_cols(tempvhliik, reanimed)
  #seome kokku varasema tabeliga
  vhsuurus_df <- bind_rows(vhsuurus_df, tempvhliik)
  }

#Teisendused
vhsuurus_df <- vhsuurus_df %>% 
  #Excelist importimisel tekkinud tühi rida välja
  dplyr::filter(vhsuurus != "tyhirida") %>% 
  #Puuduvad väärtused nulliks
  mutate(arv = replace_na(arv,0),
         summa = replace_na(summa,0)) %>% 
  #kokku väikesed kategooriad
  #teisendused
  mutate(
    #saajad
    ryhm = ifelse(ryhm %in% c("eestkostja", "hooldaja", "lapsendaja", "võõrasvanem"),
                       "muu", ryhm),
    #Vh rühmad
    vhsuurus = ifelse(vhsuurus %in% c("kuupalga alammäär järgnev laps"),
                           "kuupalga alammäär", vhsuurus)) %>%  
  #summeerimine väikesed grupid
  group_by(aasta, ryhm, vhsuurus) %>% 
  summarise(arv = sum(arv), 
            summa= sum(summa)) %>% 
  ungroup()
  
#Leiame ka "kokku" rühmade lõikes, seda praegu ei ole, on vaid kõik kokku
vhsuurus_dfkokku <- vhsuurus_df %>% 
  #"kokku" rühma uuesti ei liida
  dplyr::filter(ryhm !="kokku") %>% 
  group_by(aasta, ryhm) %>% 
  summarise(arv = sum(arv), 
            summa= sum(summa)) %>% 
  mutate(vhsuurus = "kokku") %>% 
  ungroup()
#lisame saadud read juurde esialgsele.
vhsuurus_df <- bind_rows(vhsuurus_df, vhsuurus_dfkokku) %>%  
  arrange(aasta, ryhm, vhsuurus)

#Faktorite järjestuseks
vhsuurus_df$vhsuurus <- factor(vhsuurus_df$vhsuurus,
                                  levels = c("vanemahüvitise määr", 
                                             "kuupalga alammäär", 
                                             "100", 
                                             "maksimum",
                                             "kokku"),
                              labels = c('Vanemahüvitise määras',
                                         'Kuupalga alammääras',
                                         'Eelmine töötasu',
                                         'Maksimaalses määras', 
                                         "Kokku"))

save(vhsuurus_df, file = paste0(dashboarddatapath, "vhsuurus_df.RData"))

#Kontroll
#joonisele
#Suuruse järgi
# vhsuurus_df %>% filter(ryhm %in% c("ema", "isa")) %>%
#   ggplot(aes(x=aasta, y = arv, fill = ryhm)) +
#   geom_area()+
#   facet_wrap(~vhsuurus, scales = "free_y") +
#   labs(fill = "", x = "", y = "Inimeste arv aasta lõpus")

#Isade osakaal
isadeosakaal <- vhsuurus_df %>%  filter(!(ryhm %in% c("kokku"))) %>% 
  group_by(aasta, vhsuurus) %>%
  mutate(arvosakaal = arv/sum(arv)) %>%  
  filter(ryhm =="isa") %>% 
  ungroup()

# ggplot(isadeosakaal, aes(x=aasta, y = arvosakaal*100)) +
#   geom_line()+
#   facet_wrap(~vhsuurus, scales = "free_y") +
#   labs(x = "", y = "%", title = "Isade osakaal vanemahüvitise saajate seas")

#Kokku ryhm kaupa
vhsuurus_df %>% filter(ryhm %in% c("ema", "isa", "kokku"), vhsuurus != "Kokku") %>% 
  ggplot(aes(x=aasta, y = arv, fill = vhsuurus)) +
  geom_area()+
  scale_x_continuous(breaks=c(2005:2019))+
  facet_wrap(~ryhm) +  #, scales = "free_y"
  labs(fill = "", x = "", y = "Inimeste arv aasta lõpus")

#Keskmine hüvitise suurus

################################
#Praxise mikroandmete laadimine
#TODO!
#
library(foreign)
vhdatapath = "C:/Users/avork/Documents/Praxis/vanemahüvitis/analyys2008/vhska/"
#Peaks olema lapsetoetused
vhtabel22 <- read.dta(paste0(vhdatapath, "vh_tabel22.dta"))

#Sünnitamise fakt eelneva töötasu lõikes
#Ei mäleta täpselt, kuidas see tehti
synd_befchild_eelnevsm <- read.dta(paste0(vhdatapath, "synd_befchild_eelnevsm.dta"))
str(synd_befchild_eelnevsm)

#Teeme uue muutuja, et mitmes laps sünnib. Paneme kokku 3 ja enam

#Tuleta meelde, kuidas vanus leiti
#jätame alles naised, kes on nooremad kui 42. See oli millegipärast vajalik
syndjasm <- synd_befchild_eelnevsm %>%
  filter(vanus>=15 & vanus <=45)  %>% 
  #vanusrühmad
  mutate(vanusgr = cut(vanus, breaks = c(15, 20, 25, 30, 35, 40, 45), include.lowest = TRUE, 
                       right = FALSE)) %>% 
  #Syndiv lapse jrk 
  mutate(syndivlaps = factor(case_when(
    befchild==0 ~ "Esimene laps",
    befchild==1 ~ "Teine laps",
    befchild==2 ~ "Kolmas laps",
    TRUE ~ "Neljasjaenam laps"), levels = c("Esimene laps", "Teine laps", "Kolmas laps", "Neljasjaenam laps"))) %>% 
  #ja teeme kvantiilid aasta ja varasema laste arvu lõikes
  group_by(aasta, syndivlaps) %>% 
  mutate(eelpalkq = ifelse(toine_sma<=0,0,
                           cut(toine_sma, 
                               breaks = quantile(toine_sma[toine_sma>0], 
                                                 probs = seq(0,1,0.2)), include.lowest = TRUE))) %>% 
  mutate(eelpalkq = factor(eelpalkq,
                           levels = c(0,1,2,3,4,5),
                           labels = c("Palk puudub", "I kvintiil", "II kvintiil", "III kvintiil", "IV kvintiil", "V kvintiil"))) %>% 
  ungroup()

#kontroll
# kntr <- syndjasm %>%  group_by(aasta, befchildtr, eelpalkq) %>% 
#   summarise(mins = min(toine_sma),
#             maxs = max(toine_sma),
#             meas = mean(toine_sma),
#             arv = n()) 
# View(kntr)

syndjasm <- syndjasm %>%  dplyr::select(vanus, befchild, aasta, syndivlaps, eelpalkq,
                                        synd, vanusgr)
#See loetakse sisse rakendusse
save(syndjasm, file = paste0(dashboarddatapath, "syndjasm.RData"))

#Vaga suur andmestik ikkagi
#Leiame sünnitamise osakaalud
aluminevanuspiir = 20
yleminevanuspiir = 35
#
#Synnitamise tõenäosus
syndtn <- syndjasm %>%  filter(vanus>=aluminevanuspiir, vanus<=yleminevanuspiir,
                               befchild<3) %>%
  group_by(aasta, syndivlaps, eelpalkq) %>%
  summarise(synnitamisetn = mean(synd),
            vaatlusi = n()) %>%
 ungroup()

# ggplot(syndtn, aes(x=as.factor(eelpalkq), y=synnitamisetn, fill = aasta)) +
#   geom_col(position = "dodge") +
#   facet_wrap(~syndivlaps, nrow = 1)

# ggplot(syndtn, aes(x=as.factor(eelpalkq), y=synnitamisetn, 
#                    fill = factor(aasta))) +  #fill = (aasta>2003),group = factor(aasta)
#   geom_col(position = "dodge") +
#   #scale_fill_brewer(palette="Blues") +
#   facet_wrap(~syndivlaps, nrow = 1) +
#   labs(x= "Ema eelneva aasta palgarühm", y = "Lapse sünnitamise tõenäosus", fill ="",
#        title = "Ema varasem sissetulek ja sünnitamise tõenäosus",
#        caption = "Kvintiilid leitud iga aasta ja laste arvu kombinatsiooni kohta eraldi") +
#   theme(axis.text.x = element_text(angle = 90))


#Valmis tabel
#Synnitamise tõenäosus vanusgrupiti
syndtn <- syndjasm %>%
  group_by(aasta, befchildtr, vanusgr, eelpalkq) %>%
  summarise(synnitamisetn = mean(synd),
            vaatlusi = n()) %>%
 ungroup()

# syndtn %>%  dplyr::filter(vanusgr %in% c("[15,20)")) %>% 
#   ggplot(aes(x=as.factor(eelpalkq), y = synnitamisetn, fill = as.factor(aasta))) +
#   geom_col(position = "dodge")+
#   facet_wrap(~befchildtr)

#syndjasm %>%  group_by(aasta) %>%  summarise(synd = sum(synd)) %>%  arrange(aasta)


######## Vanemahüvitise suurus, vanus jm --------

#Tabelil on imelik päis
#vhtabel1 <- read_csv("C:/Users/avork/Documents/Praxis/vanemahüvitis/andmed/ska2008/tabel_020708_tabel_1_VH.txt")

vhdatapath = "C:/Users/avork/Documents/Praxis/vanemahüvitis/analyys2008/vhska/"

#kõik lapsed kelle eest on saadud peretoetusi
vhtabel22 <- read.dta(paste0(vhdatapath, "vh_tabel22.dta"))
#peame tegema pere tunnuse, kui kaks vanemat kasvatavad sama last
#1. kõik vanemad, kes kasvatavad sama last kuuluvad samasse peresse
#2. kõik lapsed, kellel ühine vanem, kuuluvad samasse peresse
vhtabel22family <- vhtabel22 %>%  group_by(idch) %>% 
  mutate(idparmin = min(idpar)) %>% 
  group_by(idpar) %>% 
  mutate(idfamily = min(idparmin)) %>% 
  group_by(idfamily) %>% 
  mutate(nchild = n_distinct(idch),
         nparents = n_distinct(idpar)) %>% 
  ungroup() %>%  arrange(idfamily)

#head(vhtabel22family, 20)
#loeme kokku lapsed enne antud lapse sündi
vhtabel22children <- vhtabel22family %>% dplyr::select(idch, byearch, bmonthch, idfamily, nchild, nparents) %>% 
  distinct() %>% 
  group_by(idfamily) %>% 
  arrange(byearch, bmonthch) %>% 
  mutate(mitmeslaps = row_number()) %>%  #kaksikute läheb nässu, võtab eraldi lastena. hetkel ei viitsi tegeleda.
  ungroup() %>% 
  arrange(idfamily)

#Jätame alles sidumiseks tunnused
vhtabel22lapsisidumiseks <- vhtabel22children %>%  dplyr::select(idch, mitmeslaps)

#vanemahüvitise saajad
vhtabel1 <- read.dta(paste0(vhdatapath, "vh_tabel1.dta"))
#> dim(vhtabel1)
#[1] 68627    15

#seome juurde palju oli varem lapsi
vhtabel1 <- left_join(vhtabel1, vhtabel22lapsisidumiseks, by = "idch")

#Saajad vanuse ja suuruse järgi. Praxise 2009 raporti tabelid 2.5 ja 2.6

vhtabel1 <- vhtabel1 %>% 
  mutate(vanusgr = cut((year(pbbeg0) - byearpar), breaks = c(0, 20, 25, 30, 35, 40, 45, 100), include.lowest = TRUE, 
                     right = FALSE),
         pbbegyear0 = year(pbbeg0),
         pbdur0=pbend0-pbbeg0+1,
         pbdur1=pbend1-pbbeg1+1,
         #kui kolm või enam, siis paneme kokku. Kui polnud peretoetuste failis, siis sai VHd, aga mitte veel toetusi. Paneme 1.
         mitmeslaps = pmin(ifelse(is.na(mitmeslaps),1,mitmeslaps), 3)) %>%
  arrange(idpar, idch, -pbsum0, -pbsum1, pbdur0, pbdur1) %>% 
  #duplikaadid välja, kui sama vanem on saanud korduvalt sama lapse eest. võimalik, kui hüvitis muutub
  group_by(idpar, idch) %>% 
  slice(1) %>% 
  ungroup()

#dim(vhtabel1)
#[1] 68601    19
#26 rida välja
#Stata kommnetaar oli: "**14 naist on saanud kaks korda, 4 naist on saanud 4 korda" (14+4*3=26)

table(vhtabel1$vanusgr, vhtabel1$pbtype0, useNA = "ifany")
table(vhtabel1$mitmeslaps, vhtabel1$pbtype0, useNA = "ifany")

vhtabel1$pbtype0f <- factor(vhtabel1$pbtype0, levels = c(4, 3, 1, 2), 
                            labels = c("Vanemahüvitise määras", "Kuupalga alammääras", "100%tulu", "Maksimaalses suuruses"))

tabel1 <-prop.table(table(vhtabel1$vanusgr, vhtabel1$pbtype0f, useNA = "ifany"), 1)

# #proportsioonid
#mingi vale joonis
# vhtabel1 %>%
#   ggplot(aes(x=pbtype0f, y = pbtype0f, fill = vanusgr)) +
#   geom_col() +
#   facet_wrap(~pbbegyear0)
# addmargins(tabel1)
# 

#Agregeerime vanuse ja laste arvu järgi ära

#vana versioon
# vhtabel1gr <- vhtabel1 %>%
#   group_by(pbtype0f, vanusgr, pbbegyear0, gender) %>%
#   summarise(vaartus = n()) %>%
#   ungroup()

#iga liigi, vanusrühma, lapse arvu, algusaasta ja soo järgi inimeste arv kokku
vhtabel1gr <- vhtabel1 %>%
  group_by(pbtype0f, vanusgr, mitmeslaps, pbbegyear0, gender) %>%
  summarise(vaartus = n()) %>%
  ungroup()

# #Proportsioonid
# iga aasta, soo ja vanusrühma sees, milline on erinevate hüvitise liikide osakaal
vhtabel1grprtypevanusgr <- vhtabel1gr %>%
  #agregeerime laste rühma enne välja
  group_by(vanusgr, pbbegyear0, gender, pbtype0f) %>%
  summarise(vaartus = sum(vaartus)) %>% 
  group_by(vanusgr, pbbegyear0, gender, ) %>%
  mutate(osakaal = vaartus / sum(vaartus)) %>%
    ungroup()

ggplot(vhtabel1grprtypevanusgr %>%  dplyr::filter(gender =="M"), aes(x=vanusgr, y = vaartus, fill = pbtype0f)) +
  geom_col() +
  facet_wrap(~pbbegyear0) +
  theme(axis.text.x = element_text(angle = 90),text = element_text(size=12),
        legend.position = "bottom") +
  labs(fill = "", x = "Vanus", y = "")

save(vhtabel1grprtypevanusgr, file = paste0(dashboarddatapath, "vhtabel1grprtypevanusgr.RData"))

#kõik proportsioonid
vhtabel1grprtype <- vhtabel1gr %>%
  group_by(vanusgr, mitmeslaps, pbbegyear0, gender) %>%
  mutate(osakaal = vaartus / sum(vaartus)) %>%
  ungroup()

ggplot(vhtabel1grprtype %>%  dplyr::filter(gender =="N"), 
       aes(x=vanusgr, y = vaartus, fill = pbtype0f)) +
  geom_col() +
  facet_wrap(mitmeslaps~pbbegyear0, scales = "free_y") +
 theme(axis.text.x = element_text(angle = 90),text = element_text(size=12),
       legend.position = "bottom") +
  labs(fill = "", x = "Vanus", y = "")

save(vhtabel1grprtype, file = paste0(dashboarddatapath, "vhtabel1grprtype.RData"))


#Seome juurde varasemate laste arvu mingil moel
#siduda juurde idpar abil  laste arv kes sündinud enne, kui byearch ja  bmonthch
head(vhtabel1)

#kuidas see tehti?
#Tee järgi ikkagi
synd_befchild_eelnevsm <- read.dta(paste0(vhdatapath, "synd_befchild_eelnevsm.dta"))


#kuni viis saajat!
#iddwide <- idd %>%  pivot_wider(id_cols = pereid, names_from = parentnr,
#                                names_prefix = "vanem", values_from = idpar)
#neid, millel üle kolme on 199 last. 
#iddwide %>%  filter(!is.na(vanem2)) %>% nrow()


######## Vanemahüvitis ja teise vanema töötamine
#









######## Näidisleibkonnad -----------

# *alates 2014
# gen uusvanhyv2014= vanhyv if palk<=$vanhyvmaar
# *Hüvitise uue suuruse leidmiseks kasutatakse valemit: hüvitis – (tulu - hüvitise määr)/2
# replace uusvanhyv2014= vanhyv - (palk - ${vanhyvmaar})/2 if palk>$vanhyvmaar & palk<=$maxpalk
# *Seejuures säilitatakse alati vähemalt pool isikule määratud hüvitisest. Samuti ei vähendata hüvitist alla hüvitise määra suuruse summa.
# replace uusvanhyv2014= max(0.5*vanhyv,uusvanhyv2014,${vanhyvmaar})







