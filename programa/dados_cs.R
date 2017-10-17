rm(list = ls())
gc()

#install.packages("RCurl")
#install.packages("data.table")


##### Comentários ##########
#   

# Carregando pacotes necessários
library(RCurl)
library(data.table)
library(gsubfn)
library(stringr)
library(purrr)
library(rvest) # the new package, version 0.3.0
library(RCurl)
library(XML)
library(dplyr)
library(reshape2)


  pathout <- "data/cs"
  all_match <- data.table()
  link <-"https://www.hltv.org/stats/players"
  
  html <- getURL(link,
           ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
  
  # html1 <- read_html(link, encoding = "UTF-8")
  # texto <- htmlParse(html1)
  # texto.doc <- capture.output(texto)

  info1 <- unlist(regmatches(html,
                                         gregexpr("(?<=playerCol\"><img alt=\").*(?=\" src=\")",
                                                  html,perl = T)))
  info2 <- unlist(regmatches(html,
                             gregexpr("(?<=href=\"/)stats/players/.*/.*(?=.\\sdata-tooltip-id=)",
                                      html,perl = T)))
  
  info3 <- unlist(regmatches(html,
                             gregexpr('(?<=playerCol\"><img alt=\").*(?=<.a><.td>)',
                                      html,perl = T)))
  info3 <- gsub(".*uniqueTooltipId.*\">(.*)","\\1",info3,perl = T)
  
  # team
  info4base <-  unlist(regmatches(html,
                              gregexpr('(?<=teamCol\">).*</td>',
                                       html,perl = T)))
  
  
  info4 <- gsub(".*href=\"/(.*)\"\\sdata-tooltip-id.*","\\1",info4base,perl = T)
  info4 <- gsub("</td>",NA,info4)
  
  info5 <- gsub(".*gtSmartphone-only\">(.*)</span></a></td>","\\1",info4base,perl = T)
  info5 <- gsub("</td>",NA,info5)
    
  
  info6_8 <-  unlist(regmatches(html,
                                  gregexpr('(?<=<td class=\"statsDetail\">).*(?=</td>)',
                                           html,perl = T)))
  info6 <- info6_8[(1:(length(info6_8)/2))*2-1]
  info8 <- info6_8[(1:(length(info6_8)/2))*2]
  info7 <-  unlist(regmatches(html,
                              gregexpr('(?<=<td class=\"kdDiffCol).*(?=</td>)',
                                       html,perl = T)))
  info7 <- gsub(".lost\">|.won\">","",info7,perl = T)
  info9 <-  unlist(regmatches(html,
                              gregexpr('(?<=<td class=\"ratingCol).*(?=</td>)',
                                       html,perl = T)))
  info9 <- gsub(".ratingNegative\">","-",info9,perl = T)
  info9 <- gsub(".won\">","",info9,perl = T)

  base_jogadores <- data.table(
    playerCol_pais = info1,
    playerCol_link_base = gsub("(stats/players/).*","\\1",info2),
    playerCol_link_var = gsub("stats/players/(.*)","\\1",info2),
    playerCol_link = info2,
    player_name = info3,
    player_id =gsub("stats/players/(.*)/.*","\\1",info2),
    player_team = info4,
    player_team_ref = info5,
    stats1 = info6,
    kdDiffCol = info7,
    stats2 = info8,
    ratingCol = info9
  )

  # salvando base jogadres
  filename <- paste0("J_cs_total.rds")
  saveRDS(base_jogadores,file = file.path(pathout,filename))
  
  i <- 1
for(i in 1:2){
  
  filename <- paste0("J_cs_",base_jogadores$player_id[i],".rds")
  
  if(file.exists(file.path(pathout,filename))){
    print(paste0("player ",base_jogadores$player_id[i]," já estava salvo!"))
    next
  }else{
  link_jogador_temp <- paste0("https://www.hltv.org/",base_jogadores$playerCol_link[i])
  
  html_jogador <- getURL(link_jogador_temp,
                 ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
  
  info_j<- unlist(regmatches(html_jogador,
                                gregexpr('(?<=<div class=\"stats-row\").*(?=</span></div>)',
                                         html_jogador,perl = T)))
  
  vars_name <- gsub(".*><span>(.*)</span><span.*","\\1",info_j)
  vars_name <- gsub("><span class=\"\">(.*)</span><span>.*","\\1",vars1_name,perl = T)
  vars <- gsub(".*</span><span.*>(.*)","\\1",info_j)
  
  base1_jogador_temp <- data.table(player_id = base_jogadores[i]$player_id,
                                  stats = vars_name,
                                  value_stats = vars,
                                  tab = "overview")
    
    
  link_jogador_temp <- paste0("https://www.hltv.org/",
                          base_jogadores$playerCol_link_base[i],
                          "individual/",
                          base_jogadores$playerCol_link_var[i])
  
  html_jogador <- getURL(link_jogador_temp,
                          ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
  
  info_j <- unlist(regmatches(html_jogador,
                                gregexpr('(?<=<div class=\"stats-row\"><span>).*',
                                         html_jogador,perl = T)))
  vars_name <- gsub("(.*)</span><span>.*","\\1",info_j)
  vars <- gsub(".*</span><span>(.*)</span></div>","\\1",info_j)
  
  base2_jogador_temp <- data.table(player_id = base_jogadores[i]$player_id,
                                   stats = vars_name,
                                   value_stats = vars,
                                   tab = "individual")
  
  
  ### career
  
  link_jogador_temp <- paste0("https://www.hltv.org/",
                              base_jogadores$playerCol_link_base[i],
                              "career/",
                              base_jogadores$playerCol_link_var[i])
  
  html_jogador <- getURL(link_jogador_temp,
                         ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
  
  info_j <- unlist(regmatches(html_jogador,
                              gregexpr('(?<=<td>).*(</td>)',
                                       html_jogador,perl = T)))
  info_j <- gsub("<span class=\"stat-rating\">(.*</span>.*)</td>.*","\\1",info_j,perl = T)
  info_j <- gsub("</span>\\s|</span>","",info_j,perl = T)
  info_j <- gsub("</td>","",info_j,perl = T)
  info_j <- matrix(info_j,ncol = 5,byrow = T)
  base3_jogador_temp <- data.table(info_j)
  setnames(base3_jogador_temp,names(base3_jogador_temp),c("periodo","all","online","lan","major"))
  base3_jogador_temp <- data.table(melt(base3_jogador_temp, id.vars = "periodo"))
  base3_jogador_temp[,`:=`(tab="career",
               player_id = base_jogadores[i]$player_id)]
  setnames(base3_jogador_temp,c("variable","value"),c("stats","value_stats"))
  
  ## juntando bases
  base_jogador_temp <- rbindlist(list(base1_jogador_temp,base2_jogador_temp,base3_jogador_temp)
                                 ,fill = T)
  
  
  saveRDS(base_jogador_temp,file = file.path(pathout,filename))
  
  print(paste0("player ",base_jogadores$player_id[i]," salvo!"))
  }
}