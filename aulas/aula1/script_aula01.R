## ----exec,include  = FALSE, eval = FALSE---------------------------------
## ## diretório onde está este script
## arquivo <- rstudioapi::getActiveDocumentContext()$path
## arquivo <- iconv(arquivo,from = "utf8",to = "latin1")
## dir_script <- dirname(arquivo)
## 
## nome_arquivo <- file.path(dir_script,paste0("script_",
##                             gsub(pattern = "Rmd",
##                                  replacement = "R",
##                                  basename(arquivo))))
## 
## knitr::purl(input = arquivo,
##             output = nome_arquivo,
##             documentation = 1)

## ----docfunction, eval = F-----------------------------------------------
## 
## library(RCurl)
## library(data.table)
## library(gsubfn)
## library(stringr)
## library(rvest)
## 

## ----funcs, eval = F,echo=T----------------------------------------------
## 
## gsub
## grep
## getURL
## strapplyc
## unlist
## strsplit
## 

## ----grep1, eval = T, echo=TRUE------------------------------------------

texto <- c("eu amo R","eu amo LAMFO","eu odeio SVM")
grep("eu amo",texto)
grep("eu amo",texto,value = T)


## ----grep2, eval =T, echo=T----------------------------------------------

texto <- c("eu amo R","eu amo LAMFO","eu odeio SVM","eu detesto estatística")
grep("eu\\s(amo|odeio)",texto,value=T)
grep("eu\\s\\w*\\s\\w*",texto,value = T)


## ----gsub1, eval =T, echo=T----------------------------------------------

texto <- "eu amo python"
gsub("Python","R",texto,ignore.case = T)




## ----gsub2, eval =T, echo=T----------------------------------------------

texto <- c("eu amo R","eu amo LAMFO","eu odeio SVM","eu detesto estatística")
gsub("(eu amo ).*","\\1R",texto,ignore.case = T)




## ----gsub3, eval =T, echo=T----------------------------------------------

texto <- "eu amo SAS e não gosto de R"
gsub("(eu amo )[a-z]*( e não gosto de )[a-z]*","\\1R\\2SAS",texto,ignore.case = T)




## ----gsub4, eval = T,echo=F----------------------------------------------
## padrão de citação 
texto <- c("Pedro Henrique de Melo Albuquerque")
rtexto <- gsub("(\\w{1})\\w*\\s+","\\1. ",texto,ignore.case = T)
texto <- gsub("(\\w{1})\\w*\\s+","\\1. ",texto,ignore.case = T)
gsub("(.*\\.)\\s(\\w{2,}$)","\\2, \\1",
              texto,ignore.case = T)

## ----gsub5, eval = F,echo=F----------------------------------------------
## ## ignorando de ou do
## texto <- c("Pedro Henrique de Melo Albuquerque",
##            "Peng do Nascimento")
## gsub("([^((de|do)\\s)])\\w*\\s+","\\1. ",texto,ignore.case = T,perl = T)
## 

## ----strapplyc1, eval = T,echo=T-----------------------------------------
library(gsubfn)
strapply(c("12;34:56","12,23,34.54:56,89,,12","12.12.12"), "[0-9]+")
strapply(c("12;34:56","12,23,34.54:56,89,,12","12.12.12"), "[0-9]+", simplify = c)

## ----strsplit1, eval = T,echo=T------------------------------------------
strsplit(c("12.34.56"), "\\.")


# ----getURL1, eval = F,echo=T--------------------------------------------
    link <- "https://www.fifaindex.com/pt-br/"
    html <- getURL(link,
                    ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
    html <- unlist(strsplit(html,"\\n"))

# ----read_html1, eval = F,echo=T-----------------------------------------
    link <- "https://www.transfermarkt.com/premier-league/spieltagtabelle/wettbewerb/GB1/saison_id/2010"
    html <- read_html(link, encoding = "UTF-8")
    html <- htmlParse(html)
    html <- capture.output(html)

