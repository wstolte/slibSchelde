
require(tidyverse)

# nutrients and organic carbon
# 
url <- "http://geo.vliz.be/geoserver/wfs/ows?service=WFS&version=1.1.0&request=GetFeature&typeName=Dataportal%3Aabiotic_observations&resultType=results&viewParams=where%3Aobs.context+%26%26+ARRAY%5B1%5D+AND+standardparameterid+IN+%2825%5C%2C24%5C%2C27%5C%2C26%5C%2C2560%5C%2C3242%5C%2C2829%5C%2C1996%5C%2C528%5C%2C2370%5C%2C529%5C%2C530%5C%2C531%5C%2C532%5C%2C1018%5C%2C1843%5C%2C1019%5C%2C830%5C%2C828%5C%2C5245%5C%2C832%5C%2C831%5C%2C836%5C%2C834%5C%2C866%5C%2C2559%5C%2C1010%5C%2C3217%5C%2C1315%5C%2C1013%5C%2C1011%5C%2C833%5C%2C1021%5C%2C1022%5C%2C3557%5C%2C1026%5C%2C1025%5C%2C2444%5C%2C1033%5C%2C2366%5C%2C1032%5C%2C1972%5C%2C1179%5C%2C536%5C%2C1319%5C%2C1300%5C%2C5132%5C%2C663%5C%2C1301%5C%2C665%5C%2C667%5C%2C668%5C%2C5249%5C%2C3621%5C%2C675%5C%2C3707%5C%2C3587%5C%2C674%5C%2C1309%5C%2C1310%5C%2C1322%5C%2C5051%29%3Bcontext%3A0001&propertyName=stationname%2Clongitude%2Clatitude%2Cdatetime%2Cdepth%2Cparametername%2Cvaluesign%2Cvalue%2Cdataprovider%2Cdatasettitle&outputFormat=csv"
# httr::parse_url(url)

df <- read_csv(url, na = c("-", "999999999999"))

# check of hoogste waarde niet eigenlijk NA moet zijn
df %>% arrange(-value) %>% head(10) %>% select(value) %>% as.character()
# check for other indicators of NA
df %>% filter(grepl("999999", as.character(value))) %>% select(value) %>% as.character()
df %>% distinct(parametername, datasettitle)


pal = colorFactor(topo.colors(10), domain = as.factor(df$dataprovider))
df %>% distinct(latitude, longitude, dataprovider) %>%
  mutate(dataprovider = as.factor(dataprovider)) %>%
  filter(latitude>0) %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    stroke = F, fillOpacity = 1, fillColor =  ~pal(dataprovider),
    label = ~dataprovider) %>%
  addLegend("bottomright", pal = pal, values = ~dataprovider,
            # title = "dataprovider",
            opacity = 1)

# duplicate rows?
df %>% 
  drop_na(value) %>%
  duplicated() %>% which() %>% length()
# no


# duplicate measurements?
df %>% 
  drop_na(value) %>%
  select(-FID)  %>%
  duplicated() %>% which() %>% length()
# yes. 1
# 
duplicates <- df %>% 
select(-FID)  %>%
  duplicated() %>% which()


df_clean <- df[-duplicates,]

# There may be multiple values at same location and time
df_clean %>% 
  drop_na(value) %>%
  select(latitude, longitude, depth, datetime, parametername, stationname)  %>%
  duplicated() %>% which() %>% length()
# yes, 525

# See if we can average them
dupliSamples <- df_clean %>% 
  drop_na(value) %>%
  select(latitude, longitude, depth, datetime, parametername, stationname)  %>%
  duplicated() %>% which()

df_clean %>%
  group_by(latitude, longitude, depth, datetime, parametername, stationname) %>%
  summarize(n = n(), mean = mean(value, na.rm = T), sd = sd(value, na.rm = T), .groups = "drop") %>%
  arrange(-n) %>% head(54) %>% View()

# Some sd values are very high compared to mean. Still, averaging is necessary to work with the data further

# redefine df_clean without any duplicate measurements

df_clean <- df[-duplicates,] %>%
  select(latitude, longitude, depth, datetime, parametername, stationname, dataprovider, value) %>%
  group_by(latitude, longitude, depth, datetime, parametername, stationname, dataprovider) %>%
  summarize(mean = mean(value, na.rm = T), .groups = "drop")

# next steps is to harmonize the different parameters.. 

# koolstofmetingen
# df_clean %>% distinct(parametername) %>% filter(grepl("koolstof", parametername)) %>%
#   write_delim("data/koolstofparams.csv", delim = ";")

koolstofmapping <- read_delim("data/koolstofparams.csv", delim = ";")

df_C <- df_clean %>%filter(grepl("koolstof", parametername)) %>%
  full_join(koolstofmapping, by = c(parametername = "parametername")) %>%
  filter(!is.na(newParam)) %>%
  mutate(newValue = mean * factor) %>%
  drop_na(newValue) %>%
  select(-parametername, -mean, -factor) %>%
  # filter(newValue > 0 & newValue < 900) %>%
  drop_na(newValue)
  
write_delim(df_C, "data/orgC.csv", delim = ";")






# stikstof
df_clean %>% distinct(parametername) %>% filter(grepl("stikstof", parametername))


# fosfaat
df_clean %>% distinct(parametername) %>% filter(grepl("fosfaat", parametername))
