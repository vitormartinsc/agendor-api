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

load_packages()
library(googlesheets4)
library(googledrive)
library(openxlsx)

setwd("C:/Users/servi/agendor-api/")


sheet_id <- "1GpB1ed23HZhNKQciU3DSKC05ZowEirr0"
sheet_name <- "Base (inter)"  # Nome da sheet que será alterada

# 📥 Baixa o arquivo Excel do Google Drive
temp_file <- tempfile(fileext = ".xlsx")
drive_download(as_id(sheet_id), path = temp_file, overwrite = TRUE)

# 📖 Carrega o arquivo Excel inteiro (com todas as sheets)
wb <- loadWorkbook(temp_file)

db_inter <- read_excel(temp_file, sheet = 'Base (inter)', skip = 3) %>% 
  rename('Nome Completo' = 3) %>% 
  select(3, 6:ncol(.))
db_own <- read_excel(temp_file, sheet = 'Base (own)', skip = 2) # Ajuste a aba conforme necessário

colnames(db_own) <- ifelse(
  grepl("^\\d+\\.0$", colnames(db_own)),  # Identifica nomes numéricos com ".0"
  format(as.Date(as.numeric(colnames(db_own)), origin = "1899-12-30"), "%d/%m/%Y"),  # Converte de número para data
  colnames(db_own)  # Mantém os nomes normais
)

db_own
db_inter



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
  mutate(
    tpv_60_d = zoo::rollapplyr(value, width = 60,FUN = sum, partial=T),
    tpv_30_d = zoo::rollapplyr(value, width = 30,FUN = sum, partial=T),
    tpv_last_30_d = zoo::rollapplyr(lag(value, 30), width = 30, FUN = sum, partial = T),
    tpv_historico = cumsum(value)
  ) %>% 
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

         