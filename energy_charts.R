library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(scales)
library(hrbrthemes)
library(tidyr)

###CHART 2=====================

#reading wind csv
electricity_wind <- read.csv("share-electricity-wind.csv")

electricity_wind <- electricity_wind %>% 
  rename("wind_electricity"="Wind....electricity.") %>% 
  filter(Year=="2021") 

#reading solar csv
electricity_solar <- read.csv("share-electricity-solar.csv")

electricity_solar <- electricity_solar %>% 
  rename("solar_electricity"="Solar....electricity.") %>% 
  filter(Year=="2021") 

#checking for differences in the names in the two df
setdiff(electricity_solar$Entity, electricity_wind$Entity)

#joining the two df and calculating total from wind and solar
elec_wind_solar <- electricity_solar %>% 
  select(c("Entity", "solar_electricity")) %>% 
  left_join(electricity_wind, by = "Entity") %>% 
  relocate(solar_electricity, .before = wind_electricity) %>% 
  mutate(total_elec=solar_electricity+ wind_electricity) 

#reading class excel
country_class <- read_excel("CLASS.xlsx", sheet = "List of economies") 

setdiff(elec_wind_solar$Entity, country_class$Economy)

#renaming the countries to match the class excel and left joining the datasets
elec_wind_solar<- elec_wind_solar %>% 
  mutate(Entity=recode(Entity,
                       "Bahamas"="Bahamas, The",
                       "Brunei" = "Brunei Darussalam",
                       "Cape Verde"= "Cabo Verde",
                       "Congo"= "Congo, Rep.",
                       "Cote d'Ivoire"= "Côte d’Ivoire",
                       "Czechia"= "Czech Republic",
                       "Democratic Republic of Congo"= "Congo, Dem. Rep.",
                       "Egypt"= "Egypt, Arab Rep.",
                       "Gambia" ="Gambia, The",
                       "Hong Kong"="Hong Kong SAR, China",
                       "Iran" = "Iran, Islamic Rep.",
                       "Kyrgyzstan" = "Kyrgyz Republic",
                       "Laos" = "Lao PDR",
                       "Macao" = "Macao SAR, China",
                       "Micronesia (country)"= "Micronesia, Fed. Sts.",
                       "Russia" = "Russian Federation",
                       "Slovakia" = "Slovak Republic",
                       "Syria"= "Syrian Arab Republic",
                       "Turkey" = "Türkiye",
                       "Venezuela" = "Venezuela, RB",
                       "Yemen" = "Yemen, Rep.",
                       "North Korea"= "Korea, Dem. People's Rep.",
                       "South Korea"= "Korea, Rep.",
                       "Taiwan"= "Taiwan, China",
                       "Saint Kitts and Nevis"= "St. Kitts and Nevis",
                       "Saint Lucia"= "St. Lucia",
                       "Saint Vincent and the Grenadines"= "St. Vincent and the Grenadines",
                       "Timor" = "Timor-Leste",
                       "Faeroe Islands"= "Faroe Islands",
                       "Sao Tome and Principe"= "São Tomé and Príncipe",
                       "United States Virgin Islands"= "Virgin Islands (U.S.)",
                       "Palestine"= "West Bank and Gaza"))  %>% 
  left_join(country_class, by = c("Entity"="Economy")) 

#calculating the average
electricity_error <- elec_wind_solar %>% 
  filter(`Income group`!="Low income") %>% 
  group_by(`Income group`) %>% 
  summarise(average = mean(total_elec)) %>%
  ungroup() %>% 
  drop_na() 

#creating the average chart
electricity_error %>% 
  ggplot(aes(`Income group`, average))+
  geom_col(position = position_dodge(width = .9), fill="steelblue")+
  geom_text(aes(label = round(average, 0)), vjust=-0.5)+
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, 1))+
  theme_bw()+
  theme(axis.text = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank())+
  xlab("")+
  ylab("Share of electricity production \n from wind and solar (%)")

ggsave("electricity_SolarWind_share.png", dpi = 300, height = 6, width = 8)


#creating voilin and box plot
elec_wind_solar %>%
  filter(`Income group`!="Low income") %>%
  drop_na(`Income group`) %>% 
  ggplot(aes(`Income group`, total_elec))+
  geom_violin(scale = "width") +
  geom_boxplot(width=0.1, color="#999999", size=0.5)+
  stat_summary(fun = "mean", colour = "red", size = 2, geom = "point")+
  stat_summary(fun="mean", colour="red", geom="text", size=3, 
               hjust=-.4, aes( label=round(..y.., digits=1)))+
  stat_summary(fun = "max", colour = "red", size = 2, geom = "point")+
  stat_summary(fun="max", colour="red", geom="text",size=3,vjust=-0.3,
               hjust=-.4, aes( label=round(..y.., digits=1)))+
  stat_summary(fun = "min", colour = "red", size = 2, geom = "point")+
  stat_summary(fun="min", colour="red", geom="text",size=3,vjust=1.4,  
               aes( label=round(..y.., digits=1)))+
  geom_point(size=1.8, alpha=0.2) +
  geom_text(aes(label=Code.x, color=`Region`),  check_overlap = TRUE, hjust=3, size=2.5)+
  scale_y_continuous(limits = c(0, 55), breaks = seq(0, 55, 5)) +
  theme_light()+
  theme(strip.text = element_text(face="bold", size = 12),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank())+
  xlab("")+
  ylab("Share of electricity production from solar and wind (%)")+
  ggtitle("Distribution of electricity production from solar and wind (%)")

ggsave("distribution_electricity_SolarWind.png", dpi = 300, height = 6, width = 10)


#Also creating the wind chart and solar chart separately
#For solar
#calculating the average
electricity_error_solar <- elec_wind_solar %>% 
  filter(`Income group`!="Low income") %>% 
  group_by(`Income group`) %>% 
  summarise(average = mean(solar_electricity)) %>%
  ungroup() %>% 
  drop_na() 

#creating the average chart
electricity_error_solar %>% 
  ggplot(aes(`Income group`, average))+
  geom_col(position = position_dodge(width = .9), fill="steelblue")+
  geom_text(aes(label = round(average, 1)), vjust=-0.5)+
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 1))+
  theme_bw()+
  theme(axis.text = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank())+
  xlab("")+
  ylab("Share of electricity production \n from solar (%)")

ggsave("electricity_Solar_share.png", dpi = 300, height = 6, width = 8)


#creating voilin and box plot
elec_wind_solar %>%
  filter(`Income group`!="Low income") %>%
  drop_na(`Income group`) %>% 
  ggplot(aes(`Income group`, solar_electricity))+
  geom_violin(scale = "width") +
  geom_boxplot(width=0.1, color="#999999", size=0.5)+
  stat_summary(fun = "mean", colour = "red", size = 2, geom = "point")+
  stat_summary(fun="mean", colour="red", geom="text", size=3, 
               hjust=-.4, aes( label=round(..y.., digits=1)))+
  stat_summary(fun = "max", colour = "red", size = 2, geom = "point")+
  stat_summary(fun="max", colour="red", geom="text",size=3,vjust=-0.3,
               hjust=-.4, aes( label=round(..y.., digits=1)))+
  stat_summary(fun = "min", colour = "red", size = 2, geom = "point")+
  stat_summary(fun="min", colour="red", geom="text",size=3,vjust=1.4,  
               aes( label=round(..y.., digits=1)))+
  geom_point(size=1.8, alpha=0.2) +
  geom_text(aes(label=Code.x, color=`Region`),  check_overlap = TRUE, hjust=3, size=2.5)+
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +
  theme_light()+
  theme(strip.text = element_text(face="bold", size = 12),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank())+
  xlab("")+
  ylab("Share of electricity production from solar (%)")+
  ggtitle("Distribution of electricity production from solar (%)")

ggsave("distribution_electricity_Solar.png", dpi = 300, height = 6, width = 10)

#From wind
#calculating the average
electricity_error_wind <- elec_wind_solar %>% 
  filter(`Income group`!="Low income") %>% 
  group_by(`Income group`) %>% 
  summarise(average = mean(wind_electricity)) %>%
  ungroup() %>% 
  drop_na() 

#creating the average chart
electricity_error_wind %>% 
  ggplot(aes(`Income group`, average))+
  geom_col(position = position_dodge(width = .9), fill="steelblue")+
  geom_text(aes(label = round(average, 1)), vjust=-0.5)+
  scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, 1))+
  theme_bw()+
  theme(axis.text = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank())+
  xlab("")+
  ylab("Share of electricity production \n from wind (%)")

ggsave("electricity_Wind_share.png", dpi = 300, height = 6, width = 8)


#creating voilin and box plot
elec_wind_solar %>%
  filter(`Income group`!="Low income") %>%
  drop_na(`Income group`) %>% 
  ggplot(aes(`Income group`, wind_electricity))+
  geom_violin(scale = "width") +
  geom_boxplot(width=0.1, color="#999999", size=0.5)+
  stat_summary(fun = "mean", colour = "red", size = 2, geom = "point")+
  stat_summary(fun="mean", colour="red", geom="text", size=3, 
               hjust=-.4, aes( label=round(..y.., digits=1)))+
  stat_summary(fun = "max", colour = "red", size = 2, geom = "point")+
  stat_summary(fun="max", colour="red", geom="text",size=3,vjust=-0.3,
               hjust=-.4, aes( label=round(..y.., digits=1)))+
  stat_summary(fun = "min", colour = "red", size = 2, geom = "point")+
  stat_summary(fun="min", colour="red", geom="text",size=3,vjust=1.4,  
               aes( label=round(..y.., digits=1)))+
  geom_point(size=1.8, alpha=0.2) +
  geom_text(aes(label=Code.x, color=`Region`),  check_overlap = TRUE, hjust=3, size=2.5)+
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 10)) +
  theme_light()+
  theme(strip.text = element_text(face="bold", size = 12),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank())+
  xlab("")+
  ylab("Share of electricity production from wind (%)")+
  ggtitle("Distribution of electricity production from wind (%)")

ggsave("distribution_electricity_Wind.png", dpi = 300, height = 6, width = 10)



###CHART 3=========================
#reading wind csv
electricity_wind_series <- read.csv("share-electricity-wind.csv")%>% 
  rename("wind_electricity"="Wind....electricity.")

#reading solar csv
electricity_solar_series <- read.csv("share-electricity-solar.csv") %>%
  rename("solar_electricity"="Solar....electricity.")

#joining the two df and calculating total from wind and solar
elec_wind_solar_series <- electricity_solar_series %>% 
  select(c("Entity","Year", "solar_electricity")) %>% 
  left_join(electricity_wind_series, by = c("Entity", "Year")) %>% 
  relocate(solar_electricity, .before = wind_electricity) %>% 
  mutate(total_elec=solar_electricity+ wind_electricity) 

#renaming the countries to match the class excel and left joining the datasets
elec_wind_solar_series<- elec_wind_solar_series %>% 
  mutate(Entity=recode(Entity,
                       "Bahamas"="Bahamas, The",
                       "Brunei" = "Brunei Darussalam",
                       "Cape Verde"= "Cabo Verde",
                       "Congo"= "Congo, Rep.",
                       "Cote d'Ivoire"= "Côte d’Ivoire",
                       "Czechia"= "Czech Republic",
                       "Democratic Republic of Congo"= "Congo, Dem. Rep.",
                       "Egypt"= "Egypt, Arab Rep.",
                       "Gambia" ="Gambia, The",
                       "Hong Kong"="Hong Kong SAR, China",
                       "Iran" = "Iran, Islamic Rep.",
                       "Kyrgyzstan" = "Kyrgyz Republic",
                       "Laos" = "Lao PDR",
                       "Macao" = "Macao SAR, China",
                       "Micronesia (country)"= "Micronesia, Fed. Sts.",
                       "Russia" = "Russian Federation",
                       "Slovakia" = "Slovak Republic",
                       "Syria"= "Syrian Arab Republic",
                       "Turkey" = "Türkiye",
                       "Venezuela" = "Venezuela, RB",
                       "Yemen" = "Yemen, Rep.",
                       "North Korea"= "Korea, Dem. People's Rep.",
                       "South Korea"= "Korea, Rep.",
                       "Taiwan"= "Taiwan, China",
                       "Saint Kitts and Nevis"= "St. Kitts and Nevis",
                       "Saint Lucia"= "St. Lucia",
                       "Saint Vincent and the Grenadines"= "St. Vincent and the Grenadines",
                       "Timor" = "Timor-Leste",
                       "Faeroe Islands"= "Faroe Islands",
                       "Sao Tome and Principe"= "São Tomé and Príncipe",
                       "United States Virgin Islands"= "Virgin Islands (U.S.)",
                       "Palestine"= "West Bank and Gaza"))  %>% 
  left_join(country_class, by = c("Entity"="Economy")) 

#creating average of income classification
elec_wind_solar_series_error <- elec_wind_solar_series %>% 
  # filter(`Income group`!="Low income") %>% 
  group_by(`Income group`, `Year`) %>% 
  summarise(average = mean(total_elec)) %>%
  ungroup() %>% 
  drop_na()

#creating line chart
elec_wind_solar_series_error %>% 
  ggplot(aes(x=Year, y=average, color=`Income group`))+
  geom_line(size=1)+
  theme_bw()+
  theme(axis.text = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank())+
  xlab("")+
  ylab("Average share of electricity production \n from solar and wind (%)")

ggsave("electricity_SolarWInd_series.png", dpi = 300, height = 6, width = 8)

#For Wind only series chart
#creating average of income classification
elec_wind_series_error <- elec_wind_solar_series %>% 
  #filter(`Income group`!="Low income") %>% 
  group_by(`Income group`, `Year`) %>% 
  summarise(average = mean(wind_electricity)) %>%
  ungroup() %>% 
  drop_na()

#creating line chart
elec_wind_series_error %>% 
  ggplot(aes(x=Year, y=average, color=`Income group`))+
  geom_line(size=1)+
  theme_bw()+
  theme(axis.text = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank())+
  xlab("")+
  ylab("Average share of electricity production \n from wind (%)")

ggsave("electricity_WInd_series.png", dpi = 300, height = 6, width = 8)

#From solar
#creating average of income classification
elec_solar_series_error <- elec_wind_solar_series %>% 
  #filter(`Income group`!="Low income") %>% 
  group_by(`Income group`, `Year`) %>% 
  summarise(average = mean(solar_electricity)) %>%
  ungroup() %>% 
  drop_na()

#creating line chart
elec_solar_series_error %>% 
  ggplot(aes(x=Year, y=average, color=`Income group`))+
  geom_line(size=1)+
  theme_bw()+
  theme(axis.text = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank())+
  xlab("")+
  ylab("Average share of electricity production \n from solar (%)")

ggsave("electricity_Solar_series.png", dpi = 300, height = 6, width = 8)


###CHART 4=====================
#reading the data
wdi_data <- read_excel("P_Data_Extract_From_World_Development_Indicators (1).xlsx",
                       sheet = "Data")

#subsetting only the gdp per capita
wdi_gdppercap <- wdi_data %>% 
  filter(`Series Name`=="GDP per capita (constant 2015 US$)") %>% 
  select(-c("1972":"1989")) 

#converting to long format
wdi_gdppercap_lng <- wdi_gdppercap %>% 
  pivot_longer(!c("Country", "Code", "Series Name"), values_to = "gdp_percap", 
               names_to = "year") 

wdi_gdppercap_lng <- wdi_gdppercap_lng %>% 
  filter(year%in%c(1990, 2000, 2010, 2020)) 

#now matching with country class df
setdiff(wdi_gdppercap_lng$Country, elec_wind_solar_series$Entity)

#renaming country names
wdi_gdppercap_lng <- wdi_gdppercap_lng %>% 
  mutate(Country=recode(Country,
                        "Cote d'Ivoire" ="Côte d’Ivoire",
                        "Czechia"= "Czech Republic",
                        "Sao Tome and Principe"="São Tomé and Príncipe",
                        "Turkiye" ="Türkiye")) 

#extract only four years of data from wind -solar share df
elec_wind_solar_series_sub <- elec_wind_solar_series %>% 
  filter(Year%in%c(1990, 2000, 2010, 2020)) 

wdi_gdppercap_lng <- wdi_gdppercap_lng %>% 
  rename(Year="year", Entity="Country") 

wdi_gdppercap_lng$Year <- as.numeric(wdi_gdppercap_lng$Year)

#joining the gdp and wind-solar share df
wdi_scatter_df <- wdi_gdppercap_lng %>% 
  left_join(elec_wind_solar_series, by = c("Entity", 
                                           "Year")) 
#converting column to numeric
wdi_scatter_df$gdp_percap <- as.numeric(wdi_scatter_df$gdp_percap)

#dropping na
wdi_scatter_df <- wdi_scatter_df %>% 
  drop_na(gdp_percap, total_elec, Region)


#creating tot scatter chart
wdi_scatter_df %>% 
  ggplot(aes(gdp_percap, total_elec, color=`Income group`))+
  geom_point(alpha=0.8)+
  geom_text(aes(label=Code.x), color="black",  check_overlap = TRUE, hjust=1.2, size=2.5)+
  theme_bw()+
  facet_wrap(~Year)+
  theme(axis.text = element_text(size = 10),
        strip.text = element_text(face="bold", size = 10),
        legend.position = "bottom",
        legend.title = element_blank())+
  xlab("GDP per capita (constant 2015 US$)")+
  ylab("Share of electricity production from solar and wind (%)")

ggsave("scatter_gdp_windSolar.png", dpi = 300, height = 6, width = 10)

#For wind
wdi_scatter_df %>% 
  ggplot(aes(gdp_percap, wind_electricity, color=`Income group`))+
  geom_point(alpha=0.8)+
  geom_text(aes(label=Code.x), color="black",  check_overlap = TRUE, hjust=1.2, size=2.5)+
  theme_bw()+
  facet_wrap(~Year)+
  theme(axis.text = element_text(size = 10),
        strip.text = element_text(face="bold", size = 10),
        legend.position = "bottom",
        legend.title = element_blank())+
  xlab("GDP per capita (constant 2015 US$)")+
  ylab("Share of electricity production from wind (%)")

ggsave("scatter_gdp_wind.png", dpi = 300, height = 6, width = 10)

#For solar
wdi_scatter_df %>% 
  ggplot(aes(gdp_percap, solar_electricity, color=`Income group`))+
  geom_point(alpha=0.8)+
  geom_text(aes(label=Code.x), color="black",  check_overlap = TRUE, hjust=1.2, size=2.5)+
  theme_bw()+
  facet_wrap(~Year)+
  theme(axis.text = element_text(size = 10),
        strip.text = element_text(face="bold", size = 10),
        legend.position = "bottom",
        legend.title = element_blank())+
  xlab("GDP per capita (constant 2015 US$)")+
  ylab("Share of electricity production from solar (%)")

ggsave("scatter_gdp_solar.png", dpi = 300, height = 6, width = 10)


###CHART 5==========================
#reading income classification for FY2020

income_Class_2020 <- read_excel("OGHIST(1).xlsx", sheet = "Country Analytical History")

income_Class_2020 <- income_Class_2020[1:218,]

income_Class_2020 <- income_Class_2020 %>% 
  select(c("Code", "Country", "FY20")) %>% 
  mutate(FY20=recode(FY20,
                     "L"= "Low income",
                     "LM"= "Lower middle income",
                     "UM"= "Upper middle income",
                     "H"= "High income")) 

#subsetting only gdp (ppp) and co2 emissions
wdi_ppp_co2 <-  wdi_data %>% 
  filter(`Series Name`%in% c("GDP per capita, PPP (constant 2017 international $)",
                             "CO2 emissions (metric tons per capita)")) %>% 
  select(c("Country", "Code", "Series Name", "2019")) 

#cover to wide format
wdi_ppp_co2 <- wdi_ppp_co2 %>% 
  pivot_wider(names_from = `Series Name`, values_from = `2019`)

#checking the names mismatch
setdiff(wdi_ppp_co2$Country, income_Class_2020$Country)

#recoding names
wdi_ppp_co2 <-  wdi_ppp_co2 %>% 
  mutate(Country=recode(Country,
                        "Cote d'Ivoire"="Côte d'Ivoire",
                        "Curacao"= "Curaçao",
                        "Czechia"= "Czech Republic",
                        "Korea, Dem. People's Rep."= "Korea, Dem. Rep.",
                        "Sao Tome and Principe"="São Tomé and Príncipe",
                        "Turkiye" = "Türkiye")) 

#converting to nmeric
wdi_ppp_co2$`GDP per capita, PPP (constant 2017 international $)` <- as.numeric(wdi_ppp_co2$`GDP per capita, PPP (constant 2017 international $)`)
wdi_ppp_co2$`CO2 emissions (metric tons per capita)` <- as.numeric(wdi_ppp_co2$`CO2 emissions (metric tons per capita)`)

#joining the income classification for FY2020 df
wdi_ppp_co2_income <- wdi_ppp_co2 %>% 
  left_join(income_Class_2020, by = c("Country", "Code")) %>% 
  rename("Income group"="FY20") %>% 
  drop_na() 

#creating chart
wdi_ppp_co2_income %>% 
  ggplot(aes(`GDP per capita, PPP (constant 2017 international $)`, 
             `CO2 emissions (metric tons per capita)`, color=`Income group`))+
  geom_point()+
  geom_smooth(se=FALSE, size=.7, linetype = "dotted")+
  scale_color_brewer(palette="Dark2")+
  theme_bw()+
  theme(axis.text = element_text(size = 10),
        strip.text = element_text(face="bold", size = 10),
        legend.position = "bottom",
        legend.title = element_blank())
ggsave("scatter_gdp_co2.png", dpi = 300, height = 6, width = 10)


#filtering data
wdi_data_decoup <- wdi_data %>% 
  filter(`Series Name`%in% c("GDP per capita, PPP (constant 2017 international $)",
                             "CO2 emissions (metric tons per capita)")) 

#removing column with less data
wdi_data_decoup <- wdi_data_decoup %>% 
  select(-c("1972":"1989")) %>% 
  select(-c("2020":"2021")) 

#converting to long format
wdi_data_decoup <- wdi_data_decoup %>% 
  pivot_longer(!c("Country", "Code", "Series Name"), names_to = "Year", values_to = "val") %>% 
  pivot_wider(names_from = "Series Name", values_from = "val") 


#checking the names mismatch
setdiff(wdi_data_decoup$Country, income_Class_2020$Country)

#recoding names
wdi_data_decoup<-  wdi_data_decoup %>% 
  mutate(Country=recode(Country,
                        "Cote d'Ivoire"="Côte d'Ivoire",
                        "Curacao"= "Curaçao",
                        "Czechia"= "Czech Republic",
                        "Korea, Dem. People's Rep."= "Korea, Dem. Rep.",
                        "Sao Tome and Principe"="São Tomé and Príncipe",
                        "Turkiye" = "Türkiye")) 

#converting to nmeric
wdi_data_decoup$`GDP per capita, PPP (constant 2017 international $)` <- as.numeric(wdi_data_decoup$`GDP per capita, PPP (constant 2017 international $)`)
wdi_data_decoup$`CO2 emissions (metric tons per capita)` <- as.numeric(wdi_data_decoup$`CO2 emissions (metric tons per capita)`)

#joining the income classification for FY2020 df
wdi_decoup_income <- wdi_data_decoup %>% 
  left_join(income_Class_2020, by = c("Country", "Code")) %>% 
  rename("Income group"="FY20") %>% 
  drop_na() 


#subsetting only few countries for charting
wdi_decoup_income_sub <- wdi_decoup_income %>% 
  filter(Country%in%c("China", "Germany", "Japan", "United Kingdom",
                      "United States", "India", "Indonesia", "Brazil", "Russian Federation",
                      "South Africa")) 

#creating chart
wdi_decoup_income_sub %>% 
  ggplot(aes(`GDP per capita, PPP (constant 2017 international $)`, 
             `CO2 emissions (metric tons per capita)`))+
  geom_point(aes(color=Country), show.legend = FALSE)+
  geom_smooth(aes(color=`Income group`), se=FALSE, size=1)+
  geom_text(aes(label=
                  ifelse(Year=="1990", Country, "")), 
            color="black", face="bold", vjust=1.2,  size=2.5, fontface = "bold")+
  scale_color_manual(breaks = c("High income", "Upper middle income", "Lower middle income"),
                     values=c("High income"="#4daf4a", "Upper middle income"="#377eb8", "Lower middle income"="#d95f02",
                              "Germany"="#a6cee3", "Japan"= "#1f78b4", "United Kingdom"="#b2df8a", 
                              "United States"="#33a02c","China"="#fb9a99", "Brazil"= "#e31a1c", "Russian Federation"="#fdbf6f", 
                              "South Africa"= "#ff7f00","India"= "#cab2d6","Indonesia"= "#6a3d9a"))+
  theme_classic()+
  theme(axis.text = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank())+
  ggtitle("Co2 emissions against GDP per capita, PPP (1990-2019)")

ggsave("decoup_co2_ppp.png", dpi = 300, height = 6, width = 10)
