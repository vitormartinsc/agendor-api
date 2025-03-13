load_packages <- function() {
  message("Carregando pacotes default para projeto")
  .packages = c(
    'rbcb',
    'extrafont',
    'dplyr',
    'tidyverse',
    'Rblpapi',
    'ipeadatar',
    'readxl',
    'ggrepel',
    'quantmod',
    'alphavantager',
    'stringi',
    'vroom',
    'bizdays',
    'tidyverse',
    
    'lubridate'
  )
  
  for (package in .packages) {
    
    suppressMessages(require(package, character.only = TRUE))
    
  }
  
  message("Lista de pacotes carregados...")
  print(c(.packages))
  
  try(detach("package:plm", unload = TRUE), silent = T)
  
}

setwd("C:/Users/servi/agendor-api")

load_packages()

df = read_excel('teste.xlsx') %>% 
  select(-1)

df %>% 
  gather(date, value, -`Nome Completo`) %>% 
  mutate(value = ifelse(is.na(value), '0', value)) %>% 
  mutate(value = str_replace(value, '\\.', '') %>% 
           str_replace(',', '.')) %>% 
  mutate(value = as.numeric(value),
         date = dmy(date)) %>% 
  rename(name = 1) %>% 
  group_by(name) %>% 
  arrange(date) %>% 
  mutate(tpv_60_d = zoo::rollapplyr(value, width = 60,FUN = sum, partial=T),
         tpv_30_d = zoo::rollapplyr(value, width = 30,FUN = sum, partial=T),
         tpv_historico = cumsum(value)) %>% 
  ungroup %>% 
  filter(date == today()) %>% 
  select(-date)

df %>% 
  mutate(name = ifelse(
    name %in% db$`Cadastro Own`, db %>% 
      filter(`NOME Completo` == name) %>% 
      pull(`Pessoa Relacionada`),
    name
  ))

         