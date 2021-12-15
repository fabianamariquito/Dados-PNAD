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
taxasd <- get_sidra(x = 6381,
                    variable =4099,
                    period =c("last"=97),
                    geo = "Brazil",
                    header = TRUE,
                    format = 1)
x = names(taxasd)
x[(names(taxasd) == "Valor")] = "Taxa de Desocupação (%) - 4099"
colnames(taxasd) = x

###### Tabela: Taxa de Subocupação e Desocupação ######
taxaso <- get_sidra(x = 6439,
                    variable =4114,
                    period =c("last"=95),
                    geo = "Brazil",
                    header = TRUE,
                    format = 1)
y = names(taxaso)
y[(names(taxaso) == "Valor")] = "Taxa de Subocupação e Desocupação (%) - 4114"
colnames(taxaso) = y

###### Tabela: Taxa Composta de Subutilização ######
taxasu <- get_sidra(x = 6441,
                    variable =4118,
                    period =c("last"=95),
                    geo = "Brazil",
                    header = TRUE,
                    format = 1)
z = names(taxasu)
z[(names(taxasu) == "Valor")] = "Taxa Composta de Subutilização (%) - 4118"
colnames(taxasu) = z
###### Compilando os dados na mesma tabela e excluindo colunas sem utlização ###########
taxasdo <- left_join(taxasd, taxaso, by=c("Nível Territorial (Código)","Brasil (Código)", 
                                          "Trimestre Móvel (Código)", "Unidade de Medida (Código)"))
TaxasMensal <- left_join(taxasdo, taxasu, by=c("Nível Territorial (Código)","Brasil (Código)", 
                                         "Trimestre Móvel (Código)", "Unidade de Medida (Código)"))
TaxasMensal$`Nível Territorial (Código)` <- NULL
TaxasMensal$`Brasil (Código)` <- NULL
TaxasMensal$`Unidade de Medida (Código)` <- NULL
TaxasMensal$`Variável (Código).x` <- NULL
TaxasMensal$`Variável (Código).y` <- NULL
TaxasMensal$`Variável (Código)` <- NULL
TaxasMensal <- rename(TaxasMensal,`Trimestre Móvel` = `Trimestre Móvel (Código)`)

TaxasMensal <- separate(TaxasMensal, `Trimestre Móvel`, sep = c(4,6), 
                  into = c("Ano", "Mês"))
TaxasMensal <- arrange(TaxasMensal, `Mês`)


############## SEGUNDA PLANILHA: Ocupação por Setor e UF #############

###### Importando dados e gerando tabelas #######
setorm <- get_sidra(x = 6323,
                   variable =4090,
                   period =c("last"=97),
                   geo = "Brazil",
                   header = TRUE,
                   format = 1)
w = names(setorm)
w[(names(setorm) == "Grupamento de atividades no trabalho principal - PNADC (Código)")] = "Pessoas Ocupadas por Setor - 5434"
colnames(setorm) = w
###### Formatando rótulos e excluindo colunas sem utilização ############
setorm$`Nível Territorial (Código)` <- NULL
setorm$`Unidade de Medida (Código)` <- NULL
setorm$`Brasil (Código)` <- NULL
setorm$`Variável (Código)` <- NULL
setorm <- rename(setorm, `Trimestre Móvel` = `Trimestre Móvel (Código)`)
######### Formatando rótulos da variável UF e Pessoas ocupadas por setor ##########
setorm <- setorm %>% 
  mutate(`Pessoas Ocupadas por Setor - 5434`= ifelse(`Pessoas Ocupadas por Setor - 5434` == 33355, "Total - 33355",
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

setorm <- setorm %>% 
  spread(`Pessoas Ocupadas por Setor - 5434`, Valor)

setorm <- separate(setorm, `Trimestre Móvel`, sep = c(4,6), 
                  into = c("Ano", "Mês"))
setorm <- arrange(setorm, `Mês`)


############## TERCEIRA PLANILHA: Ocupação por Categorias e UF #############

###### Importando dados e gerando tabelas #######
categoriam <- get_sidra(x = 6320,
                       variable =4090,
                       period =c("last"=97),
                       geo = "Brazil",
                       header = TRUE,
                       format = 1)
v = names(categoriam)
v[(names(categoriam) == "Posição na ocupação e categoria do emprego no trabalho principal (Código)")] = "Pessoas Ocupadas por Categoria - 4097"
colnames(categoriam) = v

###### Formatando rótulos e excluindo colunas sem utilização ############
categoriam$`Nível Territorial (Código)` <- NULL
categoriam$`Unidade de Medida (Código)` <- NULL
categoriam$`Brasil (Código)` <- NULL
categoriam$`Variável (Código)` <- NULL
categoriam <- rename(categoriam, `Trimestre Móvel` = `Trimestre Móvel (Código)`)
######### Formatando rótulos da variável UF e Pessoas ocupadas por categoria ##########
categoriam <- categoriam %>% 
  mutate(`Pessoas Ocupadas por Categoria - 4097`= ifelse(`Pessoas Ocupadas por Categoria - 4097` == 96165, "Total - 96165",
                                                         ifelse(`Pessoas Ocupadas por Categoria - 4097` == 96166, "Empregado",
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
                                                         ifelse(`Pessoas Ocupadas por Categoria - 4097` == 45934, "Empregador com CNPJ - 45934",
                                                         ifelse(`Pessoas Ocupadas por Categoria - 4097` == 45935, "Empregador sem CNPJ - 45935",
                                                         ifelse(`Pessoas Ocupadas por Categoria - 4097` == 96171, "Conta própria - 96171",
                                                         ifelse(`Pessoas Ocupadas por Categoria - 4097` == 45936, "Conta própria com CNPJ - 45936",
                                                         ifelse(`Pessoas Ocupadas por Categoria - 4097` == 45937, "Conta própria sem CNPJ - 45937",
                                                         ifelse(`Pessoas Ocupadas por Categoria - 4097` == 31731, "Trabalhador familiar auxiliar - 31731", NA))))))))))))))))))))

categoriam <- categoriam %>% 
  spread(`Pessoas Ocupadas por Categoria - 4097`, Valor)

categoriam <- separate(categoriam, `Trimestre Móvel`, sep = c(4,6), 
                      into = c("Ano", "Mês"))
categoriam <- arrange(categoriam, `Mês`)

######### Exportando dados ###########
DadosMensalIBGE <- createWorkbook()
addWorksheet(DadosMensalIBGE, "Taxas Mensal")
addWorksheet(DadosMensalIBGE, "Setor Mensal")
addWorksheet(DadosMensalIBGE, "Categoria Mensal")

writeData(DadosMensalIBGE, "Taxas Mensal", TaxasMensal)
writeData(DadosMensalIBGE, "Setor Mensal", setorm)
writeData(DadosMensalIBGE, "Categoria Mensal", categoriam)

saveWorkbook(DadosMensalIBGE,"C:/UNIEPRO/DadosMensalIBGE.xlsx" , overwrite = T)
