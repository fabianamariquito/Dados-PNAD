###########################################################
####### Indicadores IBGE, Brasil/UF #######
###########################################################

# Cálculo dos indicadores a partir dos microdados da PNAD Contínua Trimestre, 
# usando pacote survey
# Pacotes usados
require(PNADcIBGE)
require(survey)
require(tidyverse)
require(xlsx)

setwd('C:\\UNIEPRO')
# Selecao var e UFs

selecvar <- c('Ano',
              'Capital',
              'Estrato',
              'posest',
              'RM_RIDE',
              'Trimestre',
              'UF',
              'UPA',
              'V1008',
              'V1014',
              'V1016',
              'V1022',
              'V1023',
              'V1027',
              'V1028',
              'V1029',
              'UF',    
              'VD4001',
              'VD4002',
              'VD4003',
              'VD4004',
              'VD4004A')

uf <- c("Rondônia",
        "Acre",
        "Amazonas",
        "Roraima",
        "Pará",
        "Amapá",
        "Tocantins",
        "Maranhão",
        "Piauí",
        "Ceará",
        "Rio Grande do Norte",
        "Paraíba",
        "Pernambuco",
        "Alagoas",
        "Sergipe",
        "Bahia",
        "Minas Gerais",
        "Espírito Santo",
        "Rio de Janeiro",
        "São Paulo",
        "Paraná",
        "Santa Catarina",
        "Rio Grande do Sul",
        "Mato Grosso do Sul",
        "Mato Grosso",
        "Goiás",
        "Distrito Federal")
# Selecao var e UFs

for(ano in 2012:2020){
  for(trimestre in 1:4){
    ano <- as.character(ano)
    trimestre <- as.character(trimestre)
    
    # BAIXAR OS DADOS DA PNAD - Já baixei
    # PNAD <- get_pnadc(year=ano, quarter=1, savedir=getwd())
    
    # LER OS DADOS DA PNAD, CASO JÁ ESTEJA BAIXADOS
    PNAD <- read_pnadc(paste0("PNADC_0",trimestre,ano,".txt"),
                       "Input_PNADC_trimestral.txt", vars = selecvar)
    
    # objeto do plano amostral
    PNAD <- pnadc_design(PNAD)  
  }
}  

####### Calculando taxa de desemprego ###########

######## Pessoas Desocupadas ##########
desocupados <- svytotal(~UF, subset(PNAD, VD4002==2), na.rm = T)
desocupados <- as.data.frame(desocupados) %>%
  mutate(Localidade = uf,
         desempregados = total) %>%
  select(Localidade, desempregados)

sum(desocupados$desempregados)

######## Pessoas na força de trabalho ##########
forcadetrab <- svytotal(~UF, subset(PNAD, VD4001==1), na.rm = T)
forcadetrab <- as.data.frame(forcadetrab) %>%
  mutate(Localidade = uf) %>%
  select(Localidade, total)

sum(forcadetrab$total)

######## Taxa de desemprego ##########
Taxadesemprego <- forcadetrab %>%
  left_join(desocupados, by="Localidade") %>% 
  bind_rows(summarise_all(., list(if(is.numeric(.)) sum(.) else "Brasil"))) %>%
  mutate(txdesemprego = (desempregados/total)*100, 
         ano = ano) %>%
  select(Localidade, txdesemprego, ano)
