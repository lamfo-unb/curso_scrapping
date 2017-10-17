rm(list = ls())
gc()

#install.packages("RCurl")
#install.packages("data.table")


##### Comentários ##########
#  
# raspagem0: importa os links de temporadas
# raspagem1: importa os jogadores em uma página específica de uma temporada específica
# raspagem2: importa as atualizações de uma versão específica de um jogador específico
# raspagem3: importa as informações|características do jogador em uma atualização específica de uma temporada FIFA específica.



# Carregando pacotes necessários
library(RCurl)
library(data.table)
library(gsubfn)
library(stringr)
library(purrr)

### first link (seasons)
raspagem0 <- function(link1){
  my_file <- getURL(link1,
                    ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
  
  
  
  geral <- strapplyc(my_file, "<li><a href=\"/(pt-br/player.*?)\">FIFA.*", simplify = c)
  resultado <- data.table(league="Barclays PL",link_season=paste0("https://www.fifaindex.com/",geral),league_link="/?gender=0&league=13")
}

## Pegando link dos jogadores por página 
raspagem1 <- function(link2,pg){
  
  my_file2 <- getURL(link2,
                     ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
  
  
  if(substr(my_file2,5,13)!= "Not Found"){
    
    geral <- strapplyc(my_file2, "<tr (.*?)><img src=", simplify = c)
    links <- gsub( ".*href=\"(.*?)\" title=.*","\\1",geral)
    link_jogadores <- paste0("https://www.fifaindex.com",links)
    nome_jogadores <- gsub( ".*title=\"(.*?)\"","\\1",geral)
    id_jogadores <-  gsub( "data-playerid=\"(.*?)\"><td.*","\\1",geral)
    
    base_temp <- data.table(id_jogador = id_jogadores , nome_jogador = nome_jogadores , link_jogador = link_jogadores)
    base_temp <- base_temp[!grepl("class",id_jogador),]
    base_temp[,number_page:=pg]
    return(base_temp)
  }else{
    return("Not found")
  }
}

### third link (in uptade)
raspagem2 <- function(link3){
  link_html <- getURL(link3,
                      ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
  
  if(link_html!= ""){
    links <- strapplyc(link_html, "<li><a href=\"/(pt-br/player/.*?)\">[0-9].*de.*de.*", simplify = c)[-1]
    data_atual <- strapplyc(link_html, "<li><a href=\"/pt-br/player/.*?\">([0-9]{1,2}.*de.*de.*[0-9]{4})", simplify = c)[-1]
    resultado <- data.table(link_atualiza=paste0("https://www.fifaindex.com/",links),data_atualizacao = data_atual)
    resultado[,link_jogador := link3, ]
    return(resultado)
  }
}

## Pegando informações dos jogadores 
raspagem3 <- function(link){
  link_html <- getURL(link,
                      ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
  if(link_html!= ""){
    geral <- strapplyc(link_html, "<p>(.*?)</span></p>", simplify = c)
    variaveis <- gsub( " <span class=\"pull-right\"><span class=\"label rating r.\">","|",geral)
    variaveis <- gsub( " <span class=\"pull-right\">","|",variaveis)
    variaveis <- gsub( "</span>","",variaveis)
    ## ajustando posições
    variaveis <- gsub( "<span class=\"label position [a-z]*\">","",variaveis)
    
    
    posicoes <- strapplyc(geral[6], "\">(.*?)</span></a>", simplify = c)
    variaveis[6]<-paste0("Posições|",paste0( gsub( ".*\">","",posicoes),collapse = ";"))
    
    ## Estrelas da perna ruim
    variaveis[8] <- gsub("<span.*/i>",str_count(variaveis[8], 'fa fa-star fa-lg'),variaveis[8])
    
    ## Estrelas fintas
    variaveis[9] <- gsub("<span.*/i>",str_count(variaveis[9], 'fa fa-star fa-lg'),variaveis[9])
    
    ## TIMES
    TIMES <- strapplyc(link_html, "<h3 class=\"panel-title\"><a href=\"(.*?)\"><img src=\".*", simplify = c)
    TIMES <- gsub( ".*title=\"(.*?)","",TIMES)
    TIMES <- paste0("Times|",paste0(TIMES,collapse = ";"))
    
    resultado <- c(variaveis,TIMES)
    variavel <- unlist(map(strsplit(resultado,"|",fixed=T),1))
    valor <- unlist(map(strsplit(resultado,"|",fixed=T),2))
    resultado <- data.table(variavel = variavel ,
                            valor  =  valor)
    resultado[,link_atualiza := link, ]
    return(resultado)
  }else{
    next
  }
}


cont_base <- 0

pathout <- "data/static"

i <- 2 # season
j <- 11 # page season player
k <- 1 # player in page
l <- 1 # informacões player


link1 <- "https://www.fifaindex.com/pt-br/players/1/?gender=0&league=13"
base_seasons <- raspagem0(link1)

## seasons
for(i in 10){
  pseason <- gsub("https://www.fifaindex.com/pt-br/players/","",base_seasons$link_season[i])
  pseason <- ifelse(pseason=="","fifa17",pseason)
  for(j in 1:25){
    name_file <- paste0("s",gsub("/","",pseason),"_pg",str_pad(j, 3, pad = "0"),".rds")
    if(file.exists(file.path(pathout,name_file))){
      print(paste0("página pulada ",j))
      next
    }else{
      base_info_jogador_total <- data.table()
      base_jogadores_total <- data.table()
      base_v_fifa_total <- data.table() 
      link2 <- paste0(base_seasons$link_season[i],j,base_seasons$league_link[i])
      base_jogadores <- raspagem1(link2,j)
      if(base_jogadores=="Not found"){
        next
      }else{
        base_jogadores[,temporada:=pseason]
        base_jogadores_total <- rbind(base_jogadores_total,base_jogadores)
        print(paste0("página ",i," Season ",pseason))
        for( k in 1:nrow(base_jogadores)){
          print(paste0("Jogador ",k))
          base_v_fifa <- raspagem2(base_jogadores$link_jogador[k])
          base_v_fifa <- merge(base_v_fifa,base_jogadores,by ="link_jogador",all.x=T)
          base_v_fifa_total <- rbind(base_v_fifa_total,base_v_fifa)
          ## pegando o primeiro e o último e sorteando outros 8
          for(l in 1:1){
            cont_base <- cont_base + 1
            base_info_jogador <- raspagem3(base_v_fifa$link_atualiza[l])
            print(paste0("página ",i," Jogador ",j," base ",cont_base," (+ ",nrow(base_info_jogador)," registros)"))
            base_info_jogador <- merge(base_info_jogador,base_v_fifa,by ="link_atualiza",all.x=T)
            base_info_jogador_total <- rbind(base_info_jogador_total,base_info_jogador)
            #Sys.sleep(1)
          }
        }
      }
      saveRDS(base_info_jogador_total,
              file.path(pathout,
                        name_file)  )
    }
  }
  
  
}


