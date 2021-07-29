library(readxl)
library(tidyverse)
library(dplyr)

#Importamos el dataset de health
Health <- read_csv("Health.csv")

Health <- filter(Health, Health['Type of Expenditure'] == "Total") #UPDATE
Health <- filter(Health, Health['Type of Programme'] == "Total") #UPDATE
Health <- filter(Health, Unit == "Percentage") #UPDATE
Health <- filter(Health, Branch == "Health" )#Agarramos Health
Health <- filter(Health, Source == "Public" )#Agarramos Health
Health <- filter(Health, Measure == "In percentage of Gross Domestic Product")#Agarramos solo el % GDP
Health <- select(Health,11, 12,14,21)

colnames(Health)[colnames(Health)== 'Value'] <- 'Health_GDP'#Cambiamos nombre de value a Health
colnames(Health)[colnames(Health)=='COUNTRY'] <- 'Country_Code'
colnames(Health)[colnames(Health)=='Country'] <- 'Country_Name'

Health <- pivot_wider(Health, names_from = Year , values_from = Health_GDP )
Health <- select(Health,1, 2,8,9,10)
Health <- pivot_longer(Health, 3:5, names_to = "Year", values_to = "Health_GDP")


#Importamos el data set de Housing
Housing <- read_csv("Housing.csv")

Housing <- filter(Housing, Housing['Type of Expenditure'] == "Total") #UPDATE
Housing <- filter(Housing, Housing['Type of Programme'] == "Total") #UPDATE
Housing <- filter(Housing, Unit == "Percentage") #UPDATE
Housing <- filter(Housing, Branch == "Housing" )#Agarramos Health
Housing <- filter(Housing, Source == "Public" )#Agarramos Health
Housing <- filter(Housing, Measure == "In percentage of Gross Domestic Product")#Agarramos solo el % GDP
Housing <- select(Housing,11, 12,14,21)

colnames(Housing)[colnames(Housing)== 'Value'] <- 'Housing_GDP'#Cambiamos nombre de value a Housing
colnames(Housing)[colnames(Housing)=='COUNTRY'] <- 'Country_Code'
colnames(Housing)[colnames(Housing)=='Country'] <- 'Country_Name'

Housing <- pivot_wider(Housing, names_from = Year , values_from = Housing_GDP )
Housing <- select(Housing,1, 2,8,9,10)
Housing<- pivot_longer(Housing, 3:5, names_to = "Year", values_to = "Housing_GDP")

#Importamos el data set de Unemployment
Unemployment <- read_csv("Unemployment.csv")

Unemployment <- filter(Unemployment, Unemployment['Type of Expenditure'] == "Total") #UPDATE
Unemployment <- filter(Unemployment, Unemployment['Type of Programme'] == "Total") #UPDATE
Unemployment <- filter(Unemployment, Unit == "Percentage") #UPDATE
Unemployment <- filter(Unemployment, Branch == "Unemployment" )#Agarramos Health
Unemployment <- filter(Unemployment, Source == "Public" )#Agarramos Health
Unemployment <- filter(Unemployment, Measure == "In percentage of Gross Domestic Product")#Agarramos solo el % GDP
Unemployment <- select(Unemployment,11, 12,14,21)

colnames(Unemployment)[colnames(Unemployment)== 'Value'] <- 'Unemployment_GDP'#Cambiamos nombre de value a Unemployment
colnames(Unemployment)[colnames(Unemployment)=='COUNTRY'] <- 'Country_Code'
colnames(Unemployment)[colnames(Unemployment)=='Country'] <- 'Country_Name'

Unemployment <- pivot_wider(Unemployment, names_from = Year , values_from = Unemployment_GDP )
Unemployment <- select(Unemployment,1, 2,8,9,10)
Unemployment<- pivot_longer(Unemployment, 3:5, names_to = "Year", values_to = "Unemployment_GDP")

#Importamos el data set de Family_Spend
Family_Spend <- read_csv("Family_Spend.csv")

Family_Spend <- filter(Family_Spend, Family_Spend['Type of Expenditure'] == "Total") #UPDATE
Family_Spend <- filter(Family_Spend, Family_Spend['Type of Programme'] == "Total") #UPDATE
Family_Spend <- filter(Family_Spend, Unit == "Percentage") #UPDATE
Family_Spend <- filter(Family_Spend, Branch == "Family" )#Agarramos Health
Family_Spend <- filter(Family_Spend, Source == "Public" )#Agarramos Health
Family_Spend <- filter(Family_Spend, Measure == "In percentage of Gross Domestic Product")#Agarramos solo el % GDP
Family_Spend <- select(Family_Spend,11, 12,14,21)

colnames(Family_Spend)[colnames(Family_Spend)== 'Value'] <- 'Family_Spend_GDP'#Cambiamos nombre de value a Family_Spend
colnames(Family_Spend)[colnames(Family_Spend)=='COUNTRY'] <- 'Country_Code'
colnames(Family_Spend)[colnames(Family_Spend)=='Country'] <- 'Country_Name'

Family_Spend <- pivot_wider(Family_Spend, names_from = Year , values_from = Family_Spend_GDP )
Family_Spend <- select(Family_Spend,1, 2,8,9,10)
Family_Spend<- pivot_longer(Family_Spend, 3:5, names_to = "Year", values_to = "Family_Spend_GDP")

#Se extrae y se arregla en formato tidy el dataset de EDUCATION
Education <- read_excel("education.xls")

Education <- select(Education, "Country Name", "Country Code", "Indicator Name", "Indicator Code", "2015", "2016", "2017")
Education$`Indicator Name`<- NULL
Education$`Indicator Code`<- NULL
#Renombrar columnas 
colnames(Education) <- c("Country_Name", "Country_Code", "2015","2016", "2017")
#Pasar de columnas a filas 
Education<- pivot_longer(Education, 3:5, names_to = "Year", values_to = "Education_GDP")
Education<-mutate(Education,across(4,as.numeric))

#Military
#Se obtiene el codigo de cada pais
Country_Cod<- Education[1:2]#Se obtiene un data set con el codigo de los paises y su respectivo pais
#Obtencion de datos Militares
Military <- read_excel("Military Expenditure GDP.xlsx")
colnames(Military)[1] <- "Country_Name"
Military<- left_join(Military,Country_Cod, by="Country_Name")#Se agrega el codigo del pais
Military<- pivot_longer(Military, 2:3, names_to = "Year", values_to = "Military_GDP")
Military<-mutate(Military,across(4,as.numeric))
Military <- distinct(Military)
Military$Year[Military$Year == "2010"] <- "2015"
Military$Year[Military$Year == "2019"] <- "2016"

#Dependant value

Life_satisfaction <- read_csv("Betterlife.csv")

Life_satisfaction <- filter(Life_satisfaction, Indicator == "Life satisfaction")
Life_satisfaction <- filter(Life_satisfaction, INEQUALITY == "TOT")
Life_satisfaction$Year <- "2016"
Life_satisfaction <- select(Life_satisfaction, LOCATION, Country, Year, Value)
colnames(Life_satisfaction)[colnames(Life_satisfaction)== 'Value'] <- 'Life_satisfaction_GDP'#Cambiamos nombre de value a Housing
colnames(Life_satisfaction)[colnames(Life_satisfaction)=='LOCATION'] <- 'Country_Code'
colnames(Life_satisfaction)[colnames(Life_satisfaction)=='Country'] <- 'Country_Name'
Life_satisfaction$Year <- as.integer(Life_satisfaction$Year)

#JOIN TABLES
full_data <- inner_join(Education, Health %>% select(Country_Code, Year, 'Health_GDP'), by = c("Country_Code", "Year"))

full_data <- inner_join(full_data, Housing %>% select(Country_Code, Year, 'Housing_GDP'), by = c("Country_Code", "Year"))

full_data <- inner_join(full_data, Family_Spend %>% select(Country_Code, Year, 'Family_Spend_GDP'), by = c("Country_Code", "Year"))

full_data <- inner_join(full_data, Unemployment %>% select(Country_Code, Year, 'Unemployment_GDP'), by = c("Country_Code", "Year"))

full_data <- inner_join(full_data, Military %>% select(Country_Code, Year, 'Military_GDP'), by = c("Country_Code", "Year"))

full_data$Year <- as.integer(full_data$Year)

full_data <- inner_join(full_data, Life_satisfaction %>% select(Country_Code, Year, 'Life_satisfaction_GDP'), by = c("Country_Code", "Year"))

#full_data <- drop_na(full_data)

regression <- lm(Life_satisfaction_GDP ~ Health_GDP + Housing_GDP + Family_Spend_GDP + Unemployment_GDP + Education_GDP + Military_GDP, data = full_data)

summary(regression)

write.csv(full_data,"full_data.csv", row.names = FALSE)


