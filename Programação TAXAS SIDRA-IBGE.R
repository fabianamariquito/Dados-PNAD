##### Pacotes Utilizados #######
library(sidrar)
library(dplyr)
library(xlsx)
library(tidyr)
library(openxlsx)
library(kableExtra)

############## PRIMEIRA PLANILHA: Taxas #############

###### Importando dados e gerando tabelas#######

###### Tabela: Taxa de Desocupa��o ######
taxas1 <- get_sidra(x = 4099,
                   variable =4099,
                   period =c("last"=33),
                   geo = "State",
                   header = TRUE,
                   format = 1)
x = names(taxas1)
x[(names(taxas1) == "Valor")] = "Taxa de Desocupa��o (%) - 4099"
colnames(taxas1) = x

###### Tabela: Taxa de Subocupa��o e Desocupa��o ######
taxas2 <- get_sidra(x = 4099,
                    variable =4114,
                    period =c("last"=33),
                    geo = "State",
                    header = TRUE,
                    format = 1)
y = names(taxas2)
y[(names(taxas2) == "Valor")] = "Taxa de Subocupa��o e Desocupa��o (%) - 4114"
colnames(taxas2) = y

###### Tabela: Taxa Composta de Subutiliza��o ######
taxas3 <- get_sidra(x = 4099,
                    variable =4118,
                    period =c("last"=33),
                    geo = "State",
                    header = TRUE,
                    format = 1)
z = names(taxas3)
z[(names(taxas3) == "Valor")] = "Taxa Composta de Subutiliza��o (%) - 4118"
colnames(taxas3) = z

###### Compilando os dados na mesma tabela e excluindo colunas sem utliza��o ###########
taxas12 <- left_join(taxas1, taxas2, by=c("N�vel Territorial (C�digo)","Unidade da Federa��o (C�digo)", 
                                       "Trimestre (C�digo)", "Unidade de Medida (C�digo)"))
Taxas <- left_join(taxas12, taxas3, by=c("N�vel Territorial (C�digo)","Unidade da Federa��o (C�digo)", 
                                        "Trimestre (C�digo)", "Unidade de Medida (C�digo)"))
Taxas$`N�vel Territorial (C�digo)` <- NULL
Taxas$`Unidade de Medida (C�digo)` <- NULL
Taxas$`Vari�vel (C�digo).x` <- NULL
Taxas$`Vari�vel (C�digo).y` <- NULL
Taxas$`Vari�vel (C�digo)` <- NULL
Taxas <- rename(Taxas, UF = `Unidade da Federa��o (C�digo)`,  
                Trimestre = `Trimestre (C�digo)`)
######### Formatando r�tulos da vari�vel UF ##########
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


############## SEGUNDA PLANILHA: Ocupa��o por Setor e UF #############

###### Importando dados e gerando tabelas #######
setor <- get_sidra(x = 5434,
                    variable =4090,
                    period =c("last"=33),
                    geo = "State",
                    header = TRUE,
                    format = 1)
w = names(setor)
w[(names(setor) == "Grupamento de atividades no trabalho principal - PNADC (C�digo)")] = "Pessoas Ocupadas por Setor - 5434"
colnames(setor) = w
###### Formatando r�tulos e excluindo colunas sem utiliza��o ############
setor$`N�vel Territorial (C�digo)` <- NULL
setor$`Unidade de Medida (C�digo)` <- NULL
setor$`Vari�vel (C�digo)` <- NULL
setor <- rename(setor, UF = `Unidade da Federa��o (C�digo)`,  
                Trimestre = `Trimestre (C�digo)`)
######### Formatando r�tulos da vari�vel UF e Pessoas ocupadas por setor ##########
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
                                              ifelse(`Pessoas Ocupadas por Setor - 5434` == 33357, "Agricultura, pecu�ria, produ��o florestal, pesca e aquicultura - 33357",
                                              ifelse(`Pessoas Ocupadas por Setor - 5434` == 33358, "Ind�stria Geral - 33358",
                                              ifelse(`Pessoas Ocupadas por Setor - 5434` == 33359, "Ind�stria de Transforma��o - 33359",
                                              ifelse(`Pessoas Ocupadas por Setor - 5434` == 33360, "Constru��o - 33360",
                                              ifelse(`Pessoas Ocupadas por Setor - 5434` == 33361, "Com�rcio, repara��o de ve�culos automotores e motocicletas - 33361",
                                              ifelse(`Pessoas Ocupadas por Setor - 5434` == 33362, "Transporte, armazenagem e correio - 33362",
                                              ifelse(`Pessoas Ocupadas por Setor - 5434` == 33363, "Alojamento e alimenta��o - 33363",
                                              ifelse(`Pessoas Ocupadas por Setor - 5434` == 33364, "Informa��o, comunica��o e atividades financeiras, imobili�rias, profissionais e administrativas - 33364",
                                              ifelse(`Pessoas Ocupadas por Setor - 5434` == 39325, "Administra��o p�blica, defesa, seguridade social, educa��o, sa�de humana e servi�os sociais - 39325",
                                              ifelse(`Pessoas Ocupadas por Setor - 5434` == 33367, "Outro servi�o - 33367",
                                              ifelse(`Pessoas Ocupadas por Setor - 5434` == 33368, "Servi�o dom�stico - 33368",
                                              ifelse(`Pessoas Ocupadas por Setor - 5434` == 33369, "Atividade mal definidas - 33369", NA))))))))))))))

setor <- setor %>% 
  spread(`Pessoas Ocupadas por Setor - 5434`, Valor)

setor <- separate(setor, Trimestre, sep = c(4,6), 
                  into = c("Ano", "Trimestre"))
setor <- arrange(setor, Trimestre)

############## TERCEIRA PLANILHA: Ocupa��o por Categorias e UF #############

###### Importando dados e gerando tabelas #######
categoria <- get_sidra(x = 4097,
                   variable =4090,
                   period =c("last"=33),
                   geo = "State",
                   header = TRUE,
                   format = 1)
v = names(categoria)
v[(names(categoria) == "Posi��o na ocupa��o e categoria do emprego no trabalho principal (C�digo)")] = "Pessoas Ocupadas por Categoria - 4097"
colnames(categoria) = v

###### Formatando r�tulos e excluindo colunas sem utiliza��o ############
categoria$`N�vel Territorial (C�digo)` <- NULL
categoria$`Unidade de Medida (C�digo)` <- NULL
categoria$`Vari�vel (C�digo)` <- NULL
categoria <- rename(categoria, UF = `Unidade da Federa��o (C�digo)`,  
                Trimestre = `Trimestre (C�digo)`)
######### Formatando r�tulos da vari�vel UF e Pessoas ocupadas por categoria ##########
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
                                              ifelse(`Pessoas Ocupadas por Categoria - 4097` == 31721, "Empregado no setor privado, exclusive trabalhador dom�stico - 31721",
                                              ifelse(`Pessoas Ocupadas por Categoria - 4097` == 31722, "Empregado no setor privado, exclusive trabalhador dom�stico - com carteira de trabalho assinada - 31722",
                                              ifelse(`Pessoas Ocupadas por Categoria - 4097` == 31723, "Empregado no setor privado, exclusive trabalhador dom�stico - sem carteira de trabalho assinada - 31723",
                                              ifelse(`Pessoas Ocupadas por Categoria - 4097` == 31724, "Trabalhador dom�stico - 31724",
                                              ifelse(`Pessoas Ocupadas por Categoria - 4097` == 31725, "Trabalhador dom�stico - com carteira de trabalho assinada - 31725",
                                              ifelse(`Pessoas Ocupadas por Categoria - 4097` == 31726, "Trabalhador dom�stico - sem carteira de trabalho assinada - 31726",
                                              ifelse(`Pessoas Ocupadas por Categoria - 4097`== 31727, "Empregado no setor p�blico - 31727",
                                              ifelse(`Pessoas Ocupadas por Categoria - 4097` == 31728, "Empregado no setor p�blico, exclusive militar e funcion�rio p�blico estatut�rio - com carteira de trabalho assinada - 31728",
                                              ifelse(`Pessoas Ocupadas por Categoria - 4097` == 31729, "Empregado no setor p�blico, exclusive militar e funcion�rio p�blico estatut�rio - sem carteira de trabalho assinada - 31730",
                                              ifelse(`Pessoas Ocupadas por Categoria - 4097` == 31730, "Empregado no setor p�blico - militar e funcion�rio p�blico estatut�rio - 31730",
                                              ifelse(`Pessoas Ocupadas por Categoria - 4097` == 96170, "Empregador - 96170",
                                              ifelse(`Pessoas Ocupadas por Categoria - 4097` == 96171, "Conta pr�pria - 96171", 
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
