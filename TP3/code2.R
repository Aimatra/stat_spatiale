install.packages("classInt")
install.packages("RColorBrewer")
library(dplyr)
library(sf)
library(mapsf)
library(classInt)
library(leaflet)
library(readxl)

#Import des populat
pop_com_2019 <- read_excel("TP3/data/Pop_legales_2019.xlsx")
communes_fm <- st_read("TP3/fonds/commune_francemetro_2021.gpkg")

#Correction pour la ville de Paris
pop_com_2019<-pop_com_2019 %>% 
  mutate(COM=if_else(substr(COM,1,3)=="751","75056",COM)) %>% 
  group_by(code=COM) %>% 
  summarize(pop=sum(PMUN19))

#Jointure
communes_fm <- communes_fm %>% 
  left_join(pop_com_2019,
            by="code") %>%
  mutate(densite=pop/surf)

summary(communes_fm$densite)

#on remarque que la plupart des communes ont une densité inférieure à 100 personnes.
plot(communes_fm["densite"], border=FALSE)
plot(communes_fm["densite"],  breaks="quantile", main="quantile", border=FALSE) # c'est joli mais l'échelle pas ouf on a l'impression que le pays est très dense
plot(communes_fm["densite"],  breaks="sd", main="quantile", border=FALSE) # ce serait bien si on avait une loi normale
plot(communes_fm["densite"],  breaks="fisher", main="quantile", border=FALSE) # intéressant car on fait des classes extrêmes donc on voit bien les différents pôles

#On veut montrer des structures mais pas non plus forcer le trait

denspop_quant <- classIntervals(
  communes_fm$densite,
  style = "quantile",
  n=5
)

head(denspop_quant$var)
quantile(communes_fm$densite, probs = seq(0,1,0.2))
summary(communes_fm)
denspop_man_brks5 <- c(0,40, 100,1000,8000,27200)

popcomfm_sf <- communes_fm %>%
  mutate(
    densite_c = cut(
      densite,
      breaks=denspop_man_brks5,
      labels = paste0(denspop_man_brks5[1:5], "-", denspop_man_brks5[2:6]),
      include.lowest = TRUE,
      right = FALSE,
      ordered_result = TRUE
    )
  )
table(popcomfm_sf$densite)

pal2 <- c(
  RColorBrewer::brewer.pal(
    n=5,
    name="Greens"
    )[4:3],
  RColorBrewer::brewer.pal(
    n=5,
    name = "YlOrRd"
    )[c(2,4:5)]
)

plot(
  popcomfm_sf["densite_c"],
  pal=pal2,
  border=FALSE,
  main="Densité de Population"
)

#Exercice 2:
library(sf)
library(dplyr)
library(mapsf)
library(openxlsx)

dep_francemetro_2021<-st_read("TP3/fonds/dep_francemetro_2021.gpkg")

tx_pauvrete <- read_excel("TP3/data/Taux_pauvrete_2018.xlsx")
dep_francemetro_2018_pauv <- dep_francemetro_2021 %>% 
  left_join(tx_pauvrete %>% select(-Dept), by=c("code"="Code"))


mf_map(x= dep_francemetro_2018_pauv,
       var = "Tx_pauvrete",
       type = "choro",
       nbreaks = 4,
       breaks= "jenks")

colnames(dep_francemetro_2018_pauv)

#choroplethe= par applat de couleur

couleur <- rev(mf_get_pal(4,"Mint"))
mf_map(x = dep_francemetro_2018_pauv,
       var="Tx_pauvrete",
       type="choro",
       breaks= c(0,13,17,25, max(dep_francemetro_2018_pauv$Tx_pauvrete)),
       pal= couleur,
       leg_pos= NA,
       )
mf_inset_on(x = dep_francemetro_2018_pauv, pos="topright", cex=.2)
mf_init(dep_francemetro_2018_pauv %>% 
          filter(code%in% c("75","92","93","94")))
mf_map(dep_francemetro_2018_pauv %>% 
         filter(code%in% c("75","92","93","94")),
       var ="Tx_pauvrete",
       type = "choro",
       breaks = c(0,13,17,25, max(dep_francemetro_2018_pauv$Tx_pauvrete)),
       leg_pos=NA,
       add = TRUE)

mf_label(dep_francemetro_2018_pauv %>% 
           filter(code%in%c("75","92","93","94")),
         var="code",
         col="black")
mf_inset_off()


mf_legend(
  type="choro",
  title="Taux de pauvreté",
  val=c("", "Moins de 13", "De 13 à moins de 17","De 17 à moins de 25","25 ou plus"),
  pal=couleur,
  pos="left"
)
mf_map(mer, add=TRUE)
mf_layout(title = "Taux de pauvreté par département en 2018",
          credits = "Source : INSEE")  

#Exercice4
#ronds=effectifs, applat=taux,ratio
library(readr)
communes_fm <- st_read("TP3/fonds/commune_francemetro_2021.gpkg")
bpe20_sport_loisir_xy <- read_delim("TP3/data/bpe20_sport_loisir_xy.csv", 
                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)
summary(bpe20_sport_loisir_xy)




