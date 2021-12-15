setwd("C:/UNIEPRO")

##### Pacotes Utilizados #######
library(sidrar)
library(dplyr)
library(xlsx)
library(tidyr)
library(openxlsx)
library(kableExtra)

############## PRIMEIRA PLANILHA: Taxas #############

###### Importando dados e gerando tabelas#######

###### Tabela: Taxa de Desocupação ######
taxas1 <- get_sidra(x = 4099,
                   variable =4099,
                   period =c("last"=33),
                   geo = "State",
                   header = TRUE,
                   format = 1)
x = names(taxas1)
x[(names(taxas1) == "Valor")] = "Taxa de Desocupação (%) - 4099"
colnames(taxas1) = x

###### Tabela: Taxa de Subocupação e Desocupação ######
taxas2 <- get_sidra(x = 4099,
                    variable =4114,
                    period =c("last"=33),
                    geo = "State",
                    header = TRUE,
                    format = 1)
y = names(taxas2)
y[(names(taxas2) == "Valor")] = "Taxa de Subocupação e Desocupação (%) - 4114"
colnames(taxas2) = y

###### Tabela: Taxa Composta de Subutilização ######
taxas3 <- get_sidra(x = 4099,
                    variable =4118,
                    period =c("last"=33),
                    geo = "State",
                    header = TRUE,
                    format = 1)
z = names(taxas3)
z[(names(taxas3) == "Valor")] = "Taxa Composta de Subutilização (%) - 4118"
colnames(taxas3) = z

###### Compilando os dados na mesma tabela e excluindo colunas sem utlização ###########
taxas12 <- left_join(taxas1, taxas2, by=c("Nível Territorial (Código)","Unidade da Federação (Código)", 
                                       "Trimestre (Código)", "Unidade de Medida (Código)"))
Taxas <- left_join(taxas12, taxas3, by=c("Nível Territorial (Código)","Unidade da Federação (Código)", 
                                        "Trimestre (Código)", "Unidade de Medida (Código)"))
Taxas$`Nível Territorial (Código)` <- NULL
Taxas$`Unidade de Medida (Código)` <- NULL
Taxas$`Variável (Código).x` <- NULL
Taxas$`Variável (Código).y` <- NULL
Taxas$`Variável (Código)` <- NULL
Taxas <- rename(Taxas, UF = `Unidade da Federação (Código)`,  
                Trimestre = `Trimestre (Código)`)
######### Formatando rótulos da variável UF ##########
Taxas <- Taxas %>% 
  mutate(UF = ifelse(UF == 11, "RO",  
                       ifelse(UF == 12, "AC", 
                       ifelse(UF == 13, "AM",
                       ifelse(UF == 14, "RR",
                       ifelse(UF == 15, "PA",
                       ifelse(UF == 16, "AP",
                       ifelse(UF == 17, "TO",
                       ifelse(UF == 21, "MA",
                       ifelse(UF == 22, "PI",
                       ifelse(UF == 23, "CE",
                       ifelse(UF == 24, "RN",
                       ifelse(UF == 25, "PB",
                       ifelse(UF == 26, "PE",
                       ifelse(UF == 27, "AL",
                       ifelse(UF == 28, "SE",
                       ifelse(UF == 29, "BA",
                       ifelse(UF == 31, "MG",
                       ifelse(UF == 32, "ES",
                       ifelse(UF == 33, "RJ",
                       ifelse(UF == 35, "SP",
                       ifelse(UF == 41, "PR",
                       ifelse(UF == 42, "SC",
                       ifelse(UF == 43, "RS",
                       ifelse(UF == 50, "MS",
                       ifelse(UF == 51, "MT",
                       ifelse(UF == 52, "GO",
                       ifelse(UF == 53, "DF", NA)))))))))))))))))))))))))))) 
Taxas <- separate(Taxas, Trimestre, sep = c(4,6), 
         into = c("Ano", "Trimestre"))
Taxas <- arrange(Taxas, Trimestre)


############## SEGUNDA PLANILHA: Ocupação por Setor e UF #############

###### Importando dados e gerando tabelas #######
setor <- get_sidra(x = 5434,
                    variable =4090,
                    period =c("last"=33),
                    geo = "State",
                    header = TRUE,
                    format = 1)
w = names(setor)
w[(names(setor) == "Grupamento de atividades no trabalho principal - PNADC (Código)")] = "Pessoas Ocupadas por Setor - 5434"
colnames(setor) = w
###### Formatando rótulos e excluindo colunas sem utilização ############
setor$`Nível Territorial (Código)` <- NULL
setor$`Unidade de Medida (Código)` <- NULL
setor$`Variável (Código)` <- NULL
setor <- rename(setor, UF = `Unidade da Federação (Código)`,  
                Trimestre = `Trimestre (Código)`)
######### Formatando rótulos da variável UF e Pessoas ocupadas por setor ##########
setor <- setor %>% 
  mutate(UF = ifelse(UF == 11, "RO",  
              ifelse(UF == 12, "AC", 
              ifelse(UF == 13, "AM",
              ifelse(UF == 14, "RR",
              ifelse(UF == 15, "PA",
              ifelse(UF == 16, "AP",
              ifelse(UF == 17, "TO",
              ifelse(UF == 21, "MA",
              ifelse(UF == 22, "PI",
              ifelse(UF == 23, "CE",
              ifelse(UF == 24, "RN",
              ifelse(UF == 25, "PB",
              ifelse(UF == 26, "PE",
              ifelse(UF == 27, "AL",
              ifelse(UF == 28, "SE",
              ifelse(UF == 29, "BA",
              ifelse(UF == 31, "MG",
              ifelse(UF == 32, "ES",
              ifelse(UF == 33, "RJ",
              ifelse(UF == 35, "SP",
              ifelse(UF == 41, "PR",
              ifelse(UF == 42, "SC",
              ifelse(UF == 43, "RS",
              ifelse(UF == 50, "MS",
              ifelse(UF == 51, "MT",
              ifelse(UF == 52, "GO",
              ifelse(UF == 53, "DF", NA))))))))))))))))))))))))))),
  `Pessoas Ocupadas por Setor - 5434`= ifelse(`Pessoas Ocupadas por Setor - 5434` == 33355, "Total - 33355",
                                              ifelse(`Pessoas Ocupadas por Setor - 5434` == 33357, "Agricultura, pecuária, produção florestal, pesca e aquicultura - 33357",
                                              ifelse(`Pessoas Ocupadas por Setor - 5434` == 33358, "Indústria Geral - 33358",
                                              ifelse(`Pessoas Ocupadas por Setor - 5434` == 33359, "Indústria de Transformação - 33359",
                                              ifelse(`Pessoas Ocupadas por Setor - 5434` == 33360, "Construção - 33360",
                                              ifelse(`Pessoas Ocupadas por Setor - 5434` == 33361, "Comércio, reparação de veículos automotores e motocicletas - 33361",
                                              ifelse(`Pessoas Ocupadas por Setor - 5434` == 33362, "Transporte, armazenagem e correio - 33362",
                                              ifelse(`Pessoas Ocupadas por Setor - 5434` == 33363, "Alojamento e alimentação - 33363",
                                              ifelse(`Pessoas Ocupadas por Setor - 5434` == 33364, "Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas - 33364",
                                              ifelse(`Pessoas Ocupadas por Setor - 5434` == 39325, "Administração pública, defesa, seguridade social, educação, saúde humana e serviços sociais - 39325",
                                              ifelse(`Pessoas Ocupadas por Setor - 5434` == 33367, "Outro serviço - 33367",
                                              ifelse(`Pessoas Ocupadas por Setor - 5434` == 33368, "Serviço doméstico - 33368",
                                              ifelse(`Pessoas Ocupadas por Setor - 5434` == 33369, "Atividade mal definidas - 33369", NA))))))))))))))

setor <- setor %>% 
  spread(`Pessoas Ocupadas por Setor - 5434`, Valor)

setor <- separate(setor, Trimestre, sep = c(4,6), 
                  into = c("Ano", "Trimestre"))
setor <- arrange(setor, Trimestre)

############## TERCEIRA PLANILHA: Ocupação por Categorias e UF #############

###### Importando dados e gerando tabelas #######
categoria <- get_sidra(x = 4097,
                   variable =4090,
                   period =c("last"=33),
                   geo = "State",
                   header = TRUE,
                   format = 1)
v = names(categoria)
v[(names(categoria) == "Posição na ocupação e categoria do emprego no trabalho principal (Código)")] = "Pessoas Ocupadas por Categoria - 4097"
colnames(categoria) = v

###### Formatando rótulos e excluindo colunas sem utilização ############
categoria$`Nível Territorial (Código)` <- NULL
categoria$`Unidade de Medida (Código)` <- NULL
categoria$`Variável (Código)` <- NULL
categoria <- rename(categoria, UF = `Unidade da Federação (Código)`,  
                Trimestre = `Trimestre (Código)`)
######### Formatando rótulos da variável UF e Pessoas ocupadas por categoria ##########
categoria <- categoria %>% 
  mutate(UF = ifelse(UF == 11, "RO",  
              ifelse(UF == 12, "AC", 
              ifelse(UF == 13, "AM",
              ifelse(UF == 14, "RR",
              ifelse(UF == 15, "PA",
              ifelse(UF == 16, "AP",
              ifelse(UF == 17, "TO",
              ifelse(UF == 21, "MA",
              ifelse(UF == 22, "PI",
              ifelse(UF == 23, "CE",
              ifelse(UF == 24, "RN",
              ifelse(UF == 25, "PB",
              ifelse(UF == 26, "PE",
              ifelse(UF == 27, "AL",
              ifelse(UF == 28, "SE",
              ifelse(UF == 29, "BA",
              ifelse(UF == 31, "MG",
              ifelse(UF == 32, "ES",
              ifelse(UF == 33, "RJ",
              ifelse(UF == 35, "SP",
              ifelse(UF == 41, "PR",
              ifelse(UF == 42, "SC",
              ifelse(UF == 43, "RS",
              ifelse(UF == 50, "MS",
              ifelse(UF == 51, "MT",
              ifelse(UF == 52, "GO",
              ifelse(UF == 53, "DF", NA))))))))))))))))))))))))))),
         `Pessoas Ocupadas por Categoria - 4097`= ifelse(`Pessoas Ocupadas por Categoria - 4097` == 96165, "Total - 96165",
                                              ifelse(`Pessoas Ocupadas por Categoria - 4097` == 31721, "Empregado no setor privado, exclusive trabalhador doméstico - 31721",
                                              ifelse(`Pessoas Ocupadas por Categoria - 4097` == 31722, "Empregado no setor privado, exclusive trabalhador doméstico - com carteira de trabalho assinada - 31722",
                                              ifelse(`Pessoas Ocupadas por Categoria - 4097` == 31723, "Empregado no setor privado, exclusive trabalhador doméstico - sem carteira de trabalho assinada - 31723",
                                              ifelse(`Pessoas Ocupadas por Categoria - 4097` == 31724, "Trabalhador doméstico - 31724",
                                              ifelse(`Pessoas Ocupadas por Categoria - 4097` == 31725, "Trabalhador doméstico - com carteira de trabalho assinada - 31725",
                                              ifelse(`Pessoas Ocupadas por Categoria - 4097` == 31726, "Trabalhador doméstico - sem carteira de trabalho assinada - 31726",
                                              ifelse(`Pessoas Ocupadas por Categoria - 4097`== 31727, "Empregado no setor público - 31727",
                                              ifelse(`Pessoas Ocupadas por Categoria - 4097` == 31728, "Empregado no setor público, exclusive militar e funcionário público estatutário - com carteira de trabalho assinada - 31728",
                                              ifelse(`Pessoas Ocupadas por Categoria - 4097` == 31729, "Empregado no setor público, exclusive militar e funcionário público estatutário - sem carteira de trabalho assinada - 31730",
                                              ifelse(`Pessoas Ocupadas por Categoria - 4097` == 31730, "Empregado no setor público - militar e funcionário público estatutário - 31730",
                                              ifelse(`Pessoas Ocupadas por Categoria - 4097` == 96170, "Empregador - 96170",
                                              ifelse(`Pessoas Ocupadas por Categoria - 4097` == 96171, "Conta própria - 96171", 
                                              ifelse(`Pessoas Ocupadas por Categoria - 4097` == 31731, "Trabalhador familiar auxiliar - 31731", NA)))))))))))))))

categoria <- categoria %>% 
  spread(`Pessoas Ocupadas por Categoria - 4097`, Valor)

categoria <- separate(categoria, Trimestre, sep = c(4,6), 
                  into = c("Ano", "Trimestre"))
categoria <- arrange(categoria, Trimestre)

######### Exportando dados ###########
DadosIBGE <- createWorkbook()
addWorksheet(DadosIBGE, "Taxas")
addWorksheet(DadosIBGE, "Setor")
addWorksheet(DadosIBGE, "Categoria")

writeData(DadosIBGE, "Taxas", Taxas)
writeData(DadosIBGE, "Setor", setor)
writeData(DadosIBGE, "Categoria", categoria)

saveWorkbook(DadosIBGE,"C:/UNIEPRO/DadosIBGE.xlsx" , overwrite = T)
