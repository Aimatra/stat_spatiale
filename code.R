library(sf)
library(dplyr)
commune_france_metro <- st_read("fonds/commune_francemetro_2021.gpkg")
summary(commune_france_metro)
View(commune_france_metro)
st_crs(commune_france_metro)
commune_Bretagne <- commune_france_metro %>%
  filter(dep %in% c("22","56","35","29")) %>%
  select(code, libelle, epc, dep, surf)
is(commune_Bretagne,"sf")
plot(commune_Bretagne, lwd=0.5)
bretagne_geometry <- plot(st_geometry(commune_Bretagne))

commune_Bretagne<-commune_Bretagne%>%
  mutate(surf2=st_area(commune_Bretagne)) %>% 
  mutate(surf2=units::set_units(surf2, km*km))

dept_bretagne <- commune_Bretagne %>% 
  group_by(dep) %>% 
  summarise(superficie = sum(surf))
plot(dept_bretagne)

dept_bretagne2 <- commune_Bretagne %>%
  group_by(dep) %>%
  summarise(geom = st_union(geom))
plot(dept_bretagne2)
