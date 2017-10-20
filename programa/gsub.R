## Curso: Scraping brute force
## Aula:  1
## Data: 21/10/2017
## Instrutor: Igor



https://stackoverflow.com/questions/37425019/gsub-only-part-of-pattern

x <- "5455454545454A.      J.     Burnett65652332"
gsub("\\d*([A-Z]{1})\\.\\s*([A-Z]{1})\\.\\s*([A-Za-z]*)\\d*", "\\1-\\2-\\3", x)
"([A-Z])\\.\\s([A-Z])\\."