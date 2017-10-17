rm(list = ls())
gc()

#install.packages("RCurl")
#install.packages("data.table")


##### Comentários ##########
#   
# raspagem0: importa os links de acesso de uma pagina específica do site da FIFA
# raspagem1: importa as versões do FIFA para um link de um jogador específico
# raspagem2: importa as atualizações de uma versão específica de um jogador específico
# raspagem3: importa as informações|características do jogador em uma atualização específica de uma versão FIFA específica.



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

temporadas_vec <- 2008
temporada <- temporadas_vec[1]
jogos_vec <- 0
i <- 141
sobrescreve <- T
temporada <-2008
# download.file("https://www.transfermarkt.com/", destfile = "temp.html")
# content <- read_html('temp.html')



transfermarket <- function(temporadas_vec=0,jogos_vec=0,sobrescreve=T){
  pathout <- "data/games"
  
  if(temporadas_vec%in%0){
    temporadas_vec <- 2016:2005
  }
  
  for(temporada in temporadas_vec){
    
    
    all_match <- data.table()
    link <- paste0("https://www.transfermarkt.com/premier-league/gesamtspielplan/wettbewerb/GB1?saison_id=",temporada,
                   "&spieltagVon=1&spieltagBis=38")
    
    
    
    html_base_jogos <- "https://www.transfermarkt.com/"
    html1 <- read_html(link, encoding = "UTF-8")
    texto <- htmlParse(html1)
    texto.doc <- capture.output(texto)
    links_jogos <- grep(".*spielbericht.(.*).\".*",paste0(texto.doc),value = T,perl=T)
    links_jogos_direto <- gsub(".*id.*href../(.*)\".*","\\1",links_jogos)
    
    
    
    all_times <- data.table()
    all_jogadores <- data.table()
    all_gols <- data.table()
    all_cards <- data.table()
    all_subs <- data.table()
    all_saved<- data.table()
    
    if(jogos_vec%in%0){
      jogos_vec <- 1:length(links_jogos_direto)
    }
    
    arquivos_salvos <- list.files(paste0(pathout,"/"),
                                  paste0("S_jogadores.",temporada,".J.*rds"))
    jogos_vec_feitos <- unique(as.numeric(gsub(".*J(.*).rds","\\1",arquivos_salvos)))
    
    
    if(!sobrescreve){
      jogos_vec <- setdiff(jogos_vec,jogos_vec_feitos)
      if(length(jogos_vec)==0){
        next
      }
    }
    
    # Micro info jogos ----
    links_jogos_direto <-  sort(links_jogos_direto)
    # grep("1028928",links_jogos_direto)
    for(i in jogos_vec){
      Sys.sleep(1)
      link_jogo <- paste0(html_base_jogos,links_jogos_direto[i])
      html2 <- read_html(link_jogo, encoding = "UTF-8")
      texto2 <- htmlParse(html2)
      texto.doc2 <- capture.output(texto2)
      
      inicio_timeline <- grep("Timeline",texto.doc2,value = F,perl=T)
      inicio_linesup <- grep("Line-Ups",texto.doc2,value = F,perl=T)
      inicio_gols <- grep("Goals",texto.doc2,value = F,perl=T)
      inicio_saved <- grep("missed penalties",texto.doc2,value = F,perl=T)
      inicio_subs <- grep("Substit",texto.doc2,value = F,perl=T)
      inicio_cards <- grep("Cards",texto.doc2,value = F,perl=T)
      fim_linesup <- ifelse(length(inicio_gols)>0,
                            inicio_gols-1,
                            ifelse(length(inicio_saved)>0,
                                                inicio_saved-1,
                                   ifelse(length(inicio_subs)>0,
                                          inicio_subs-1,
                                          ifelse(length(inicio_cards)>0,
                                                 inicio_cards-1,length(texto.doc2)))))
      
      
      ##> timeline ----
      
      txt_time <- texto.doc2[inicio_timeline:(inicio_linesup-1)]
      inicio_time_home <- grep("sb-leiste-heim",txt_time,value = F,perl=T)
      inicio_time_visit <- grep("sb-leiste-gast",txt_time,value = F,perl=T)
      txt_time_home <- txt_time[inicio_time_home:(inicio_time_visit-1)]
      txt_time_visit <- txt_time[-(1:(inicio_time_visit-1))]
      
      ##>> home
      
      info_time_home <- 
        grep("sb-leiste-ereignis",txt_time_home,value = T,perl=T)
      info_time_home <- gsub(".*style(.*)</div>.*","\\1",info_time_home)
      info_time_home_classe <- gsub(".*class.\"(.*)\".*","\\1",info_time_home)
      info_time_home_tempo  <- gsub(".*top..(.*)\\%.*","\\1",info_time_home)
      
      if(length(info_time_home)==0){
        base_jogo_temp_home_times <- NULL
      }else{
      base_jogo_temp_home_times <- data.table(tipo="Casa",
                                              tipo_evento = info_time_home_classe,
                                              tempo_evento_perc = info_time_home_tempo,
                                              tempo_evento = as.numeric(info_time_home_tempo)*90/100)
      }
      ##>> visit
      
      info_time_visit <- 
        grep("sb-leiste-ereignis",txt_time_visit,value = T,perl=T)
      info_time_visit <- gsub(".*style(.*)</div>.*","\\1",info_time_visit)
      info_time_visit_classe <- gsub(".*class.\"(.*)\".*","\\1",info_time_visit)
      info_time_visit_tempo  <- gsub(".*top..(.*)\\%.*","\\1",info_time_visit)
      if(length(info_time_visit)==0){
        base_jogo_temp_visit_times <- NULL
      }else{

      base_jogo_temp_visit_times <- data.table(tipo="Fora",
                                               tipo_evento = info_time_visit_classe,
                                               tempo_evento_perc = info_time_visit_tempo,
                                               tempo_evento = as.numeric(info_time_visit_tempo)*90/100)
      }
      
      base_jogo_temp_time <- rbind(base_jogo_temp_home_times,
                                   base_jogo_temp_visit_times)
      rm(base_jogo_temp_home_times,base_jogo_temp_visit_times,
         info_time_visit_classe,info_time_visit_tempo,info_time_visit,
         info_time_home,info_time_home_classe,info_time_home_tempo)
      
      
      ##> relacionando com base de eventos (timeline)
      
      base_jogo_temp_time <- base_jogo_temp_time %>%
        mutate(tipo_evento = ifelse(tipo_evento=="sb-sprite sb-rot",
                                    "sb-sprite sb-gelb",tipo_evento)) %>%
        mutate(tipo_evento = ifelse(tipo_evento=="sb-sprite sb-eigentor",
                                    "sb-sprite sb-tor",tipo_evento)) %>%
        mutate(tipo_evento = ifelse(tipo_evento=="sb-sprite sb-gelbrot",
                                    "sb-sprite sb-gelb",tipo_evento)) %>%
        group_by(tipo_evento,tipo) %>%
        mutate(id_evento = cumsum(!is.na(tempo_evento)))
      
      
      
      #> times
        txt_times <- texto.doc2[inicio_linesup:fim_linesup]
        
        times <- grep("</a></nobr></div>",txt_times,value = T,perl=T)
        id_times <- gsub(".*id..(.*)\".href.*","\\1",times)
        times <- gsub(".*\">(.*)</a>.*","\\1",times)

      ##>> esquema tático
        
        times_n <- grep("</a></nobr></div>",txt_times,value = F,perl=T)
        times_n <- c(times_n,length(txt_times))
        
        esquema_tatico <- NULL
        info_tecnicos_nome <- NULL
        
        for(ti in 1:2){
          esquema_tatico_temp <- 
            grep("Starting line-up:",txt_times[times_n[ti]:times_n[ti+1]],value = T,perl=T)
          esquema_tatico_temp <- ifelse(length(esquema_tatico_temp)==1,
                                        gsub(".*Starting line-up..(.*)\t\t\t.*","\\1",esquema_tatico_temp),
                                        NA)
          esquema_tatico <- c(esquema_tatico,esquema_tatico_temp)
          
          ##>> técnicos ----
          info_tecnicos_nome_temp <- 
            grep("profil/trainer.*",txt_times[times_n[ti]:times_n[ti+1]],value = T,perl=T)
          info_tecnicos_nome_temp <- gsub(".*title..(.*)\".id.*","\\1",info_tecnicos_nome_temp)
          info_tecnicos_nome <- c(info_tecnicos_nome,info_tecnicos_nome_temp)                            
        }
        
        ## infojogo
        txt_infojogo <- texto.doc2[1:inicio_linesup]
        infojogo_n <- grep("sb-datum show-for-small",txt_infojogo,value = T,perl=T)
        rodada <- gsub(".*spieltag.*>(.*match\\sday).*","\\1",infojogo_n)
        dia_rodada <- gsub(".*datum.*>(.*)</a>.*","\\1",infojogo_n)
        hora_jogo <- gsub(".*(\\d+:\\d+.*M).*","\\1",infojogo_n)
        placar     <- grep("sb-halbzeit",txt_infojogo,value = T,perl=T)
        placar <- gsub(".*\t(\\d+:\\d+).*","\\1",placar)
        base_jogo_temp_times <- data.table(tipo = c("Casa","Fora"),
                                           nome_time = times,
                                           id_time = id_times,
                                           esquema_tatico = esquema_tatico,
                                           tecnico = info_tecnicos_nome,
                                           link_jogo = link_jogo,
                                           id_jogo = i,
                                           rodada = rodada,
                                           dia_rodada = dia_rodada,
                                           hora_jogo = hora_jogo,
                                           placar = placar,
                                           season = temporada)
        base_jogo_temp_times[,score:=ifelse(tipo=="Casa",gsub("(.*):.*","\\1",placar),
                                            gsub(".*:(.*)*","\\1",placar))]
        rm(info_tecnicos_nome_temp,esquema_tatico_temp,times_n,
           times,id_times)
      
      
      ##> jogadores
        times_n <- grep("</a></nobr></div>",txt_times,value = F,perl=T)
        txt_times[times_n]
        times_n <- c(times_n,length(txt_times))
        
        ti <- 1
        base_jogo_temp_jogadores <- data.table()
        ##>> id jogadores
        for( ti in 1:2){
          txt_times_temp <- txt_times[times_n[ti]:times_n[ti+1]]
          n_old <- grep("Goalkeeper",txt_times_temp,value = F,perl=T)
          if(length(n_old)==0){
            ##>> id jogadores ----
            info_jogadores_id <- 
              grep(".*aufstellung-rueckennummer-name.*id.\"(.*)\".*",
                   txt_times_temp,value = T,perl=T)
            
            
            info_jogadores_id <- gsub(".*id.\"(.*)\".*","\\1",info_jogadores_id)
            
            ##>> nome jogadores ----
            
            info_jogadores_nome <- 
              grep(".*a.href.*profil.spieler.*span..",txt_times_temp,value = T,perl=T)
            info_jogadores_link <- gsub(".*href.\".(.*)\".*","\\1",info_jogadores_nome)
            info_jogadores_nome <- gsub(".*spieler.*\".(.*)<.a.*","\\1",info_jogadores_nome)
            
            
            ##>> titulares ----
            
            base_jogo_temp_jogadores_temp <- data.table(id = info_jogadores_id,
                                                   nome = info_jogadores_nome)
            
            base_jogo_temp_jogadores_temp[,tipo:=ifelse(ti==1,"Casa","Fora")]
            base_jogo_temp_jogadores_temp[,classe:="Titular"]
            
            
            ##>> reservas ----
            
            
            info_reservas_numero_team <- 
              grep("Starting line-up:",txt_times_temp,value = F,perl=T)
            ##>>> home ----
            txt_times_temp_temp <- txt_times_temp[info_reservas_numero_team[1]:length(txt_times_temp)]
            info_reservas_numero <- 
              grep(".*ersatz-rn.*",txt_times_temp_temp,value = T,perl=T)
            info_reservas_numero <- gsub(".*ersatz-rn\".(.*)..div.*","\\1",info_reservas_numero)
            
            info_reservas_nome <- 
              grep(".a.title.*spielprofil_tooltip.*",txt_times_temp_temp,value = T,perl=T)
            info_reservas_nome <- gsub(".*title..(.*)\".class..spielprofil_tooltip.*","\\1",
                                       info_reservas_nome)
            
            info_reservas_id <- 
              grep(".a.title.*spielprofil_tooltip.*",txt_times_temp_temp,value = T,perl=T)
            info_reservas_id <- gsub(".*id.\"(.*)..href.*","\\1",info_reservas_id)
            
            base_jogo_temp_jogadores_banco_temp_reserva <- data.table(id = info_reservas_id,
                                                              nome = info_reservas_nome)
            
            base_jogo_temp_jogadores_banco_temp_reserva[,classe:="Reserva"]            
            
            if(ti == 1){
              base_jogo_temp_jogadores_banco_temp_reserva[,tipo:="Casa"]
              }else{
                base_jogo_temp_jogadores_banco_temp_reserva[,tipo:="Fora"]
            }
            
            base_jogo_temp_jogadores_temp <- rbind(base_jogo_temp_jogadores_temp,
                                                   base_jogo_temp_jogadores_banco_temp_reserva)
            base_jogo_temp_jogadores <- rbind(base_jogo_temp_jogadores,base_jogo_temp_jogadores_temp) 
          
            
            
            }else{
            info_jogadores_id <- 
              grep(".*spielprofil_tooltip.*id.\"(.*)\".*",
                   txt_times_temp,value = T,perl=T)
            
            info_jogadores_id <- unlist(regmatches(info_jogadores_id,
                                                   gregexpr("(?<=id=\")[0-9]{0,10}",
                                                            info_jogadores_id,perl = T)))
            
            ##>> nome jogadores ----
            
            info_jogadores_base <- 
              grep(".*spielprofil_tooltip.*id.\"(.*)\".*",
                   txt_times[times_n[ti]:times_n[ti+1]],value = T,perl=T)
            info_jogadores_nome <- unlist(regmatches(info_jogadores_base,
                                                     gregexpr("(?<=title=\").{1,50}(?=\".class)", info_jogadores_base,perl = T)))
            info_jogadores_link <- unlist(regmatches(info_jogadores_base,
                                                     gregexpr("(?<=href.\".).{1,50}(?=\">.{1,50}</a>)", 
                                                              info_jogadores_base,perl = T)))
            
            
            ##>> titulares ----
            
            base_jogo_temp_jogadores_temp <- data.table(id = info_jogadores_id,
                                                        nome = info_jogadores_nome)
            
            base_jogo_temp_jogadores_temp[,tipo:=ifelse(ti==1,"Casa","Fora")]
            base_jogo_temp_jogadores_temp[,classe:="Titular"]
            base_jogo_temp_jogadores <- rbind(base_jogo_temp_jogadores,base_jogo_temp_jogadores_temp) 
          }
        }
        
        rm(base_jogo_temp_jogadores_temp,info_jogadores_nome,info_jogadores_link,
           info_jogadores_id)

      base_jogo_temp_jogadores <- base_jogo_temp_jogadores %>%
        left_join(base_jogo_temp_times %>% 
                    select(tipo,nome_time,link_jogo,id_jogo,season),
                  by = c("tipo"="tipo"))
      
      ##> eventos
        fim_gols <- ifelse(length(inicio_saved)>0,inicio_saved-1,
                           ifelse(length(inicio_subs)>0,inicio_subs-1,
                                  ifelse(length(inicio_cards)>0,
                                         inicio_cards-1,length(texto.doc2))))
        fim_saved <- ifelse(length(inicio_subs)>0,inicio_subs-1,
                            ifelse(length(inicio_cards)>0,
                                   inicio_cards-1,length(texto.doc2)))
        
        fim_subs <- ifelse(length(inicio_cards)>0,inicio_cards-1,length(texto.doc2))
        fim_cards <- length(texto.doc2)
        
        
        ##>> gols e assistencias
        
        if(length(inicio_gols)>0){
          
          txt_gols <- texto.doc2[inicio_gols:fim_gols]
          
          ###>> total gols
          info_tot_gols <- 
            grep("sb-aktion-aktion",txt_gols,value = F,perl=T)
          ngols <- length(info_tot_gols)
          info_tot_gols <- c(info_tot_gols,length(txt_gols)+1)
          
          base_jogo_temp_gols <- data.table()
          for(k in 1:ngols){
            txt_temp <- txt_gols[info_tot_gols[k]:(info_tot_gols[k+1]-1)]
            
            ###>> assistencias ----
            info_assist_n <- 
              grep("Assist*",txt_temp,value = F,perl=T)
            
            if(length(info_assist_n)>0){
              info_assist <- txt_temp[info_assist_n]
              info_assist_id <- gsub(".*id.\"(.*)..href.*","\\1",info_assist)
              info_assist <- gsub(".*\">(.)","\\1",info_assist)
              info_assist_nome <- gsub("(.)</a.*","\\1",info_assist)
              info_assist_modo <- gsub(".*</a>,.(.*),.*","\\1",info_assist)
              info_assist_modo <- gsub(".*</a>,.(.*):.*","\\1",info_assist_modo) ##TRATANDO EXCEÇÃO (Penalty: fouled player)
              info_gols <- txt_temp[-info_assist_n] 
            }else{
              info_assist_id = NA
              info_assist_nome = NA
              info_assist_modo = NA
              info_gols <- txt_temp
            }
            
            
            ###>> gols ----
            
            info_gols <-  grep("wichtig*",info_gols,value = T,perl=T)
            info_gols_id <- gsub(".*id.\"(.*)..href.*","\\1",info_gols)
            info_gols <- gsub(".*\">(.)","\\1",info_gols)
            info_gols_nome <- gsub("(.)</a.*","\\1",info_gols)
            info_gols_modo <- gsub(".*</a>,.(.*),.*","\\1",info_gols)
            info_gols_modo <- gsub(".*</a>,.(.*)<.*","\\1",info_gols_modo) ## own-goal
            
            base_jogo_temp_gols_temp <- data.table(marcador_id = info_gols_id,
                                                   marcador = info_gols_nome,
                                                   modo_marcador = info_gols_modo,
                                                   assistente_id = info_assist_id,
                                                   assistente = info_assist_nome,
                                                   modo_assistente = info_assist_modo)
            
            base_jogo_temp_gols <- rbind(base_jogo_temp_gols,base_jogo_temp_gols_temp)
            rm(base_jogo_temp_gols_temp)
            
          }

          base_jogo_temp_gols <- base_jogo_temp_gols  %>%
            left_join(base_jogo_temp_jogadores %>% 
                        select(id,tipo,link_jogo,id_jogo,season),
                      by = c("marcador_id"="id"))
          ## SE FOR GOL CONTRA TROCA A REFERENCIA DO GOL
          base_jogo_temp_gols <- base_jogo_temp_gols %>% 
            mutate(tipo = ifelse(grepl("Own",modo_marcador),
                                 ifelse(tipo=="Fora","Casa","Fora"),tipo))
          
          base_jogo_temp_gols <- base_jogo_temp_gols %>%
            group_by(tipo) %>%
            mutate(id_evento = cumsum(!is.na(tipo)) ) %>%
            left_join(filter(base_jogo_temp_time ,tipo_evento == "sb-sprite sb-tor") %>%
                        select(tipo,tipo_evento,id_evento,tempo_evento),
                      by = c("tipo" = "tipo", "id_evento" = "id_evento"))
          
        }else{
          base_jogo_temp_gols <- data.table()
        }
        
        ##>> saved ----
        
        if(length(inicio_saved)>0){
          
          txt_gols <- texto.doc2[inicio_saved:fim_saved]
          
          ###>> total gols----
          
          info_tot_gols <- 
            grep("sb-aktion-aktion",txt_gols,value = F,perl=T)
          ngols <- length(info_tot_gols)
          info_tot_gols <- c(info_tot_gols,length(txt_gols)+1)
          
          base_jogo_temp_saved <- data.table()
          for(k in 1:ngols){
            txt_temp <- txt_gols[info_tot_gols[k]:(info_tot_gols[k+1]-1)]
            
            ###>> assistencias ----
            info_assist_n <- 
              grep("penalty",txt_temp,value = F,perl=T)
            
            if(length(info_assist_n)>0){
              info_assist <- txt_temp[info_assist_n]
              info_assist_id <- gsub(".*id.\"(.*)..href.*","\\1",info_assist)
              info_assist <- gsub(".*\">(.)","\\1",info_assist)
              info_assist_nome <- gsub("(.)</a.*","\\1",info_assist)
              info_assist_modo <- gsub(".*(Foul penalty).*","\\1",info_assist)
              info_gols <- txt_temp[-info_assist_n] 
            }else{
              info_assist_id = NA
              info_assist_nome = NA
              info_assist_modo = NA
              info_gols <- txt_temp
            }
            
            
            ###>> gols ----
            
            info_gols <-  grep("wichtig*",info_gols,value = T,perl=T)
            info_gols_id <- gsub(".*id.\"(.*)..href.*","\\1",info_gols)
            info_gols <- gsub(".*href(.*)","\\1",info_gols)
            info_gols_nome <- gsub(".*\">(.*)</a>.*","\\1",info_gols)
            info_gols_modo <- gsub(".*\">(.*)</span>\t.*","\\1",info_gols)
            
            
            base_jogo_temp_saved_temp <- data.table(marcador_id = info_gols_id,
                                                    marcador = info_gols_nome,
                                                    modo_marcador = info_gols_modo,
                                                    assistente_id = info_assist_id,
                                                    assistente = info_assist_nome,
                                                    modo_assistente = info_assist_modo)
            
            base_jogo_temp_saved <- rbind(base_jogo_temp_saved,base_jogo_temp_saved_temp)
            rm(base_jogo_temp_saved_temp)
            
          }
          
          base_jogo_temp_saved <- base_jogo_temp_saved  %>%
            left_join(base_jogo_temp_jogadores %>% 
                        select(id,tipo,link_jogo,id_jogo,season),
                      by = c("marcador_id"="id"))   %>%
            group_by(tipo) %>%
            mutate(id_evento = cumsum(!is.na(tipo)) ) %>%
            left_join(filter(base_jogo_temp_time ,tipo_evento == "sb-11m") %>%
                        select(tipo,tipo_evento,id_evento,tempo_evento),
                      by = c("tipo" = "tipo", "id_evento" = "id_evento"))
          
        }else{
          base_jogo_temp_saved <- data.table()
        }
        
        ##>> substituicoes ----
        
        if(length(inicio_subs)>0){
          
          txt_subs <- texto.doc2[inicio_subs:fim_subs]
          info_subs <- 
            grep("sb-aus",txt_subs,value = T,perl=T)
          info_subs <- gsub(".*href(.)","\\1",info_subs)
          info_subs_out_id <- gsub(".*spieler/(.*)/saison.*","\\1",info_subs)
          info_subs_out <- gsub(".*spieler.*\">(.*)</a>.*","\\1",info_subs)
          info_subs_modo <- gsub(".*,.(.*)..</span><span.*","\\1",info_subs)
          info_subs_in <- 
            grep("sb-ein",txt_subs,value = T,perl=T)
          info_subs_in <- gsub(".*href(.)","\\1",info_subs_in)
          info_subs_in_id <- gsub(".*spieler/(.*)/saison.*","\\1",info_subs_in)
          info_subs_in <- gsub(".*spieler.*\">(.*)</a>.*","\\1",info_subs_in)
          
          
          
          base_jogo_temp_subs <- data.table(jogador_out_id = info_subs_out_id,
                                            jogador_out = info_subs_out,
                                            jogador_out_modo = info_subs_modo,
                                            jogador_in_id = info_subs_in_id,
                                            jogador_in = info_subs_in)
          
          
          
          base_jogo_temp_subs <- base_jogo_temp_subs %>%
            left_join(base_jogo_temp_jogadores %>% 
                        select(id,tipo,link_jogo,id_jogo,season),
                      by = c("jogador_out_id"="id"))%>%
            group_by(tipo) %>%
            mutate(id_evento = cumsum(!is.na(tipo)) ) %>%
            left_join(filter(base_jogo_temp_time ,tipo_evento == "sb-sprite sb-wechsel") %>%
                        select(tipo,tipo_evento,id_evento,tempo_evento),
                      by = c("tipo" = "tipo", "id_evento" = "id_evento"))
        }else{
          base_jogo_temp_subs <- data.table()
        }

        ##>> cartoes ----
        
        if(length(inicio_cards)>0){
          txt_cards <- texto.doc2[inicio_cards:fim_cards]
          
          info_cards_n <- 
            grep("sb-aktion-aktion",txt_cards,value = F,perl=T)
          info_cards_id <-  gsub(".*id..(.*)..href.*","\\1",txt_cards[info_cards_n+1])
          info_cards_name <-  gsub(".*spieler.*\">(.*)</a>.*","\\1",txt_cards[info_cards_n+1])
          info_cards_type <-  gsub(".*(Second yellow|Yellow card|Red card).*","\\1",txt_cards[info_cards_n+2])
          info_cards_modo <-  gsub(".*,.(.*)<.*","\\1",txt_cards[info_cards_n+2])
          
          base_jogo_temp_cards <- data.table(jogador_card_id = info_cards_id,
                                             jogador_card = info_cards_name,
                                             card_tipo = info_cards_type,
                                             card_modo = info_cards_modo)
          
          
          base_jogo_temp_cards <- base_jogo_temp_cards %>%
            left_join(base_jogo_temp_jogadores %>% 
                        select(id,tipo,link_jogo,id_jogo,season),
                      by = c("jogador_card_id"="id"))
          
          base_jogo_temp_cards <- base_jogo_temp_cards %>%
            group_by(tipo) %>%
            mutate(id_evento = cumsum(!is.na(tipo)) ) %>%
            left_join(filter(base_jogo_temp_time ,
                             (tipo_evento %in% c("sb-sprite sb-gelb","sb-sprite sb-rot") & 
                                tempo_evento_perc != 0 )) %>%
                        select(tipo,tipo_evento,id_evento,tempo_evento),
                      by = c("tipo" = "tipo", "id_evento" = "id_evento"))
          
        }else{
          base_jogo_temp_cards <- data.table()
        }
      
      
      ##> Salvando bases ----
        

      name_file <- paste0("S_resultado_",temporada,"_J",i,".rds")
      saveRDS(data.table(base_jogo_temp_times),
                file.path(pathout,
                          name_file))

      name_file <- paste0("S_times_",temporada,"_J",i,".rds")
      saveRDS(data.table(base_jogo_temp_time),
              file.path(pathout,
                        name_file))
      
      name_file <- paste0("S_jogadores_",temporada,"_J",i,".rds")
      saveRDS(data.table(base_jogo_temp_jogadores),
              file.path(pathout,
                        name_file))
      
      name_file <- paste0("S_gols_",temporada,"_J",i,".rds")
      saveRDS(data.table(base_jogo_temp_gols),
              file.path(pathout,
                        name_file))
      
      name_file <- paste0("S_saved_",temporada,"_J",i,".rds")
      saveRDS(data.table(base_jogo_temp_saved),
              file.path(pathout,
                        name_file))
      
      
      name_file <- paste0("S_cards_",temporada,"_J",i,".rds")
      saveRDS(data.table(base_jogo_temp_cards),
              file.path(pathout,
                        name_file))
      
      name_file <- paste0("S_subs_",temporada,"_J",i,".rds")
      saveRDS(data.table(base_jogo_temp_subs),
              file.path(pathout,
                        name_file))
      
      print(paste0("Temporada ",temporada," e jogo ",i))
    }#JOGO
  }#TEMPORADA
}#FUNÇÃO

#### Loop
## Funcao entre 2016 e 2011 "249"
transfermarket(temporadas_vec = 2008,
               jogos_vec = 54:380, 
               sobrescreve = T)


temporada <- 2010

## Verificando cartões
pathout <- "data/games"
cartoes <- list.files(pathout,paste0("cards_",temporada,"_J"),full.names = T)
base_cartoes <- rbindlist(lapply(cartoes,readRDS))
base_cartoes <- base_cartoes[is.na(tempo_evento),] 

## Verificando jogadores
jogadores <- list.files(pathout,paste0("jogadores_",temporada,"_J"),full.names = T)
base_jogadores <- rbindlist(lapply(jogadores,readRDS))
base_jogadores <- base_jogadores[is.na(id_jogo),] 


## Verificando gols
gols <- list.files(pathout,paste0("gols_",temporada,"_J"),full.names = T)
golsv <- lapply(gols,readRDS)
base_gols <- rbindlist(golsv)
base_gols <- base_gols[is.na(tempo_evento),]
