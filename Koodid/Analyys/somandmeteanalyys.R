#Vanemahüvitise saajate analüüs SoMi andmete põhjal
#Andres Võrk

pathtosomdata = "C:/Users/avork/Documents/CITIS/Som_Perepoliitika/SoMist/VHandmed_jaan2020/"

#lapsed ja vanemad
seotudvanemad <- read_xlsx(path = paste0(pathtosomdata, "SOM_vh_saaja_laps_teine_vanem_201810.xlsx"))
colnames(seotudvanemad) <- tolower(colnames(seotudvanemad))
head(seotudvanemad)
summary(seotudvanemad)
seotudvanemad %>%  group_by(laps_fis_id) %>% 
  summarise(ridu =n()) %>% 
  group_by(ridu) %>% 
  summarise(read = n())
#4 last kellel 3 vanemat

#vanemahüvitis ja töötamine
vhjasm <- read_xlsx(path = paste0(pathtosomdata, "SOM_VH_saajad_2017_2018_Parandatud.xlsx"), range = "R1C1:R396564C23")
#palju tühje ridu Excelis
vhjasm <- vhjasm %>%  dplyr::filter(!is.na(VH_SAAJA_FIS_ID))
#> dim(mehedvhsaajad)
#[1] 391624     23
colnames(vhjasm) <- tolower(colnames(vhjasm))
summary(vhjasm)
#Siin on 2017 aastal VH saanud inimesed. Võisid hakata varem saama

#TSDsaamised
tsdvhsaajad <- read_xlsx(path = paste0(pathtosomdata, "TSD_andmed.xlsx"), sheet = "VH_SAAJAD_TSD_andmed")
tsdvhteinevanem <- read_xlsx(path = paste0(pathtosomdata, "TSD_andmed.xlsx"), sheet = "TEINE VANEM TSD andmed")

#Kas üks vanem võib olla nii esimene kui teine vanem?

table(seotudvanemad$vh_saaja_fis_id %in% seotudvanemad$teine_vanem_fis_id)
#4771 saajat on korra ka teine vanem
table(seotudvanemad$teine_vanem_fis_id %in%  seotudvanemad$vh_saaja_fis_id )
#4786 teist vanemat on ka esimene vanem



table(vhjasm$vh_saaja_sugu)
vhjasm %>%  distinct(vh_saaja_fis_id, vh_saaja_sugu) %>%  group_by(vh_saaja_sugu) %>%  summarise(ridu = n())

#Kuidas on see tabel kokku pandud?
#Näib, et kõigi inimeste jaoks, kes töötavad on esitatud nii VH kui ka sotsiaalmaksu kuu.
#Nende jaoks, kes ei tööta, on toodud vaid VH saamise kuu
#Kuidas on kokku seotud omavahel vanemad?

glimpse(vhjasm)
#otsime üles vaid ühe ja sama lapsega seotud lapsevanemad

temp <- vhjasm %>% dplyr::select(vh_saaja_fis_id, vh_seotud_lapse_fis_id, vh_saamise_alguse_kp, vh_saamise_lõpu_kp) %>% 
  distinct() %>% 
  group_by(vh_seotud_lapse_fis_id) %>% 
  mutate(ridu = n()) %>% 
  distinct(vh_saaja_fis_id, vh_seotud_lapse_fis_id) %>%  #jätame kuupäevad välja
  mutate(vhsaajaid = n()) 

temp <- temp %>%  arrange(vh_seotud_lapse_fis_id)
table(temp$vhsaajaid)
temp  %>%  filter(vhsaajaid>1) %>%  head(20)
#Vaid 3 last kellel 3 saajat. Need ei mõjuta eriti midagi
#Hetkel ei hakka juurde panema, kas oli lisaks teisi lapsi kodus.

#Kuidas ehitada andmefail üles? Õnnetuseks VH alguse ja lõpu kuupäevad tekitavad topeltkirjeid

#VH kuu
table(vhjasm$vh_saamise_kuu)
#Excel on teinud mingid imelikud kuupäevad.
library(zoo)
vhjasm$vhkpv <- as.yearmon(as.Date(vhjasm$vh_saamise_kuu))
#vhjasm$vhaastakuu2 <- yearmon.POSIXct(vhjasm$vh_saamise_kuu)

table(vhjasm$vhaastakuu)
#Mingi viga on seotud jaanuariga 2017 selgelt!

#Sotsiaalmaksukuu
vhjasm$smkpv <- as.yearmon(as.Date(paste0(vhjasm$vh_saaja_sotsmaks_aasta, "-", vhjasm$vh_saaja_sotsmaks_kuu, "-01")))
#Taas on jaanuariga mingi probleem, sest selgelt vähem kui proportsioon nõuab

#seega on sul vaja vanema ID, lapse ID, vanema VH kuu ja TSD kuu
#Kas igal lapsel on olemas kaks vanemat?? Ei ole, vaid ca 10%l.
#Seega me ei tea, midagi isade TSD kohta, kes ei ole saanud vanemahüvitist. Me teame emade kohta, kes on saanud VHd.

#kõik sotsiaalmaks on töötavate eest
table(vhjasm$smkpv, vhjasm$vh_saaja_sm_liigi_kood_v, useNA = "ifany")

#1. Filtreerime välja need lapsed, kus isa saab või on saanud vanemahüvitist
#2. Võtame välja vajalikud tunnused

isavhlapsedid <- vhjasm %>%  filter(vh_saaja_sugu=="M") %>%  dplyr::select(vh_seotud_lapse_fis_id) %>%  distinct()

vhjasmisaga <- vhjasm %>%  filter(vh_seotud_lapse_fis_id %in% isavhlapsedid$vh_seotud_lapse_fis_id) %>% 
  dplyr::select(vh_seotud_lapse_fis_id, vh_saaja_fis_id, vh_saaja_sugu, vhkpv, smkpv, vh_liik)

head(vhjasmisaga)
#kas see kui inimesele on korraga mõlemad kuupäevad täidetud, siis ta sai töötasu ja vh korraga
#Mida need VH liigid näitasid?
#/VH   liik:   
# 1  100% ühe kalendrikuu tulu suuruses;
#2	maksimaalses suuruses;
#3	kuupalga alammääras;
#4	vanemahüvitise määras
#

#id-d
ids <- vhjasmisaga %>%  dplyr::select(vh_seotud_lapse_fis_id, vh_saaja_fis_id, vh_saaja_sugu) %>%  
  distinct() %>% group_by(vh_seotud_lapse_fis_id) %>%  mutate(saajanr = row_number())
#ajame laiale kujule
idswide <- ids %>%  pivot_wider(id_cols = vh_seotud_lapse_fis_id, 
                                names_from = saajanr, names_prefix = "vanem_", 
                                values_from = c(vh_saaja_fis_id, vh_saaja_sugu))
table(idswide$vh_saaja_sugu_vanem_1, idswide$vh_saaja_sugu_vanem_2, useNA = "ifany")
table(idswide$vh_saaja_sugu_vanem_2, idswide$vh_saaja_sugu_vanem_3, useNA = "ifany")

#Tee täistabel

head(ids)

#Proovime ise
tsdvhsaajad <- read_xlsx(path = paste0(pathtosomdata, "TSD_andmed.xlsx"), sheet = "VH_SAAJAD_TSD_andmed")
tsdvhteinevanem <- read_xlsx(path = paste0(pathtosomdata, "TSD_andmed.xlsx"), sheet = "TEINE VANEM TSD andmed")
#siin vist kõigi VH saajate töötasud
table(unique(tsdvhsaajad$`VH_SAAJA FIS ID`) %in% unique(tsdvhteinevanem$`TEINE VANEM FIS ID`))
table(unique(tsdvhteinevanem$`TEINE VANEM FIS ID`) %in% unique(tsdvhsaajad$`VH_SAAJA FIS ID`))
#276 inimest on mõlemas failis

table(unique(vhjasm$vh_saaja_fis_id) %in% unique(tsdvhteinevanem$`TEINE VANEM FIS ID`))
#2246 vanemat, kes said VH on ka teise vanemana TSDs
#38314 vanemat, kes said VH ei ole teise vanemana TSDs
table(unique(vhjasm$vh_saaja_fis_id) %in% unique(tsdvhsaajad$`VH_SAAJA FIS ID`))
#10641 kes said VH ei olnud  TSDs
#29922 kes said VH ja olid TSDs

table(unique(vhjasm$vh_saaja_fis_id) %in% unique(tsdvhteinevanem$`TEINE VANEM FIS ID`), 
      unique(vhjasm$vh_saaja_fis_id) %in% unique(tsdvhsaajad$`VH_SAAJA FIS ID`))
#8668 inimest ei olnud kummaski TSD failis

table(unique(tsdvhteinevanem$`TEINE VANEM FIS ID`)  %in%  unique(vhjasm$vh_saaja_fis_id))

table(unique(tsdvhsaajad$`VH_SAAJA FIS ID`)  %in%  unique(vhjasm$vh_saaja_fis_id))      




#Kontroll
#Võtame välja igal ajahetkel vanemahüvitist saavate inimeste arvu

