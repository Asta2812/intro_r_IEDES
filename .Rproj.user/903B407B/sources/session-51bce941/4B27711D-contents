################################################################################
##############                 DATA AND CLIMATE                   ##############                  
##############                  Fourth Session                    ##############                  
##############               Jean-Baptiste Guiffard               ##############
################################################################################


#######################################
#### Web Scraping 
#######################################

#install.packages('rvest')

library(rvest)
library(tidyverse)
library(stringr)





# Scrapper Wikipedia

url <- "https://fr.wikipedia.org/wiki/Liste_de_batailles_du_XIXe_si%C3%A8cle"
code_html <- read_html(url, encoding="UTF-8")

code_html %>% html_nodes("title") %>% html_text()

tables <- code_html %>% html_nodes("main") %>% html_nodes("table")
batailles_afr <- as.data.frame(tables[1] %>% html_table(fill=TRUE))
batailles_afr$guerre <- ifelse(batailles_afr$Bataille==batailles_afr$Date.s., batailles_afr$Bataille,NA)
for (x in 1:length(batailles_afr$guerre)){
  if(is.na(batailles_afr[x,'guerre'])){
    batailles_afr[x,'guerre']=value
  }else{
    value=batailles_afr[x,'guerre']
  }
}
batailles_afr <- subset(batailles_afr, Résultat!=guerre)





# Evolution des discours politiques sur le climat en France

page <- read_html('https://www.gouvernement.fr/discours-et-rapports?content_type%5B%5D=speech')


raw_list <- page %>%
  html_nodes('div')


raw_list <- page %>%
  html_nodes(css="div[class='fr-card__body']")


titles <- raw_list %>%
  html_nodes(css="h4[class='fr-card__title fr-text-title--blue-france']") %>%
  html_text() %>%
  trimws()

dates <- raw_list %>%
  html_nodes(css="div[class='fr-card__end']") %>%
  html_nodes(css="p[class='fr-card__detail']") %>%
  html_text()%>%
  trimws() %>%
  gsub("Publié le ", "", .)
  #html_attr("datetime")

category <- raw_list %>%
  html_nodes(css="div[class='fr-card__start']") %>%
  html_nodes(css="p[class='fr-card__detail']") %>%
  html_text() %>%
  trimws() %>%
  substring(., 12)


links <- raw_list %>%
  html_nodes(css="div[class='fr-card__content']") %>%
  html_nodes(css="h4[class='fr-card__title fr-text-title--blue-france']") %>%
  html_nodes('a') %>%
  html_attr('href')




bdd1 <- data.frame(dates,
                   category,
                   titles,
                   links
                   )

for(row_x in 1:nrow(bdd1)){
  print(row_x)
  speech_link <- paste('https://www.gouvernement.fr',bdd1[row_x,"links"], sep="")
  page_speech<- read_html(speech_link)
  speech_x <- unlist(page_speech %>%
    #html_nodes(css="section[class='contents__content fr-mb-10w']") %>%
    html_nodes(css="div[class='fr-col-lg-9 fr-col-offset-lg-1']") %>%
    html_text()) %>%
    trimws()
  bdd1[row_x,'speech'] <- do.call(paste, c(as.list(speech_x), sep = " "))
  Sys.sleep(2)
}





#limite nombre de page : 12162


# Pour un certain nombre de discours, selection aleatoire des pages


full_bdd <- data.frame()
for (x in 1:36){ 
  print(x)
  page_x <- read_html(paste('https://www.gouvernement.fr/discours-et-rapports?page=',x,'&content_type%5B%5D=speech',sep=""))
  Sys.sleep(2)

  raw_list <- page_x %>%
    html_nodes(css="div[class='fr-card__body']")
  
  
  titles <- raw_list %>%
    html_nodes(css="h4[class='fr-card__title fr-text-title--blue-france']") %>%
    html_text() %>%
    trimws()
  
  dates <- raw_list %>%
    html_nodes(css="div[class='fr-card__end']") %>%
    html_nodes(css="p[class='fr-card__detail']") %>%
    html_text()%>%
    trimws() %>%
    gsub("Publié le ", "", .)
  #html_attr("datetime")
  
  category <- raw_list %>%
    html_nodes(css="div[class='fr-card__start']") %>%
    html_nodes(css="p[class='fr-card__detail']") %>%
    html_text() %>%
    trimws() %>%
    substring(., 12)
  
  
  links <- raw_list %>%
    html_nodes(css="div[class='fr-card__content']") %>%
    html_nodes(css="h4[class='fr-card__title fr-text-title--blue-france']") %>%
    html_nodes('a') %>%
    html_attr('href')
  
  
  
  
  
  bdd_x <- data.frame(dates, 
                     titles,
                     category,
                     links)
  
  
  full_bdd <- rbind(full_bdd,bdd_x)
  
}


write.csv2(full_bdd, 'full_bdd_speech_2024.csv')



for(row_x in 1:nrow(full_bdd)){
  print(row_x)
  speech_link <- paste('https://www.gouvernement.fr',full_bdd[row_x,"links"], sep="")
  page_speech<- read_html(speech_link)
  speech_x <- unlist(page_speech %>%
                       #html_nodes(css="section[class='contents__content fr-mb-10w']") %>%
                       html_nodes(css="div[class='fr-col-lg-9 fr-col-offset-lg-1']") %>%
                       html_text()) %>%
    trimws()
  full_bdd[row_x,'speech'] <- do.call(paste, c(as.list(speech_x), sep = " "))
  Sys.sleep(2)
}


write.csv2(full_bdd, 'full_bdd_speech_2024.csv')




library(rvest)

url <- "https://en.wikipedia.org/wiki/List_of_countries_by_ecological_footprint"
code_html <- read_html(url, encoding="UTF-8")

code_html %>% html_nodes("title") %>% html_text()

tables <- code_html %>% html_nodes("main") %>% html_nodes("table")
table_empr <- as.data.frame(tables[3] %>% html_table(fill=TRUE))
table_empr <- table_empr[-c(1,2),]

library(sf)
world_map <- st_read('world-administrative-boundaries/world-administrative-boundaries.shp')
plot(st_geometry(world_map))


table_empr$Country.region[table_empr$Country.region == "United States"] <- "United States of America"
table_empr$Country.region[table_empr$Country.region == "United Kingdom"] <- "U.K. of Great Britain and Northern Ireland"
table_empr$Country.region[table_empr$Country.region == "Russia"] <- "U.K. of Great Britain and Northern Ireland"
table_empr$Country.region[table_empr$Country.region == "Congo, Democratic Republic of the"] <- "Democratic Republic of the Congo"
table_empr$Country.region[table_empr$Country.region == "Russia"] <-"Russian Federation"
table_empr$Country.region[table_empr$Country.region == "Libya"] <-"Libyan Arab Jamahiriya"
table_empr$Country.region[table_empr$Country.region == "South Korea"] <-"Republic of Korea"
table_empr$Country.region[table_empr$Country.region == "Tanzania"] <- "United Republic of Tanzania"



merged_empres <- merge(world_map, table_empr, by.x="name", by.y="Country.region", all.x=TRUE)
list_countries <- subset(merged_empres, is.na(Ecologicalfootprint))$name
merged_empres$Ecologicalfootprint <- as.numeric(merged_empres$Ecologicalfootprint)

plot(merged_empres['Ecologicalfootprint'])
