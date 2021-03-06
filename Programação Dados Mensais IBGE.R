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
taxasd <- get_sidra(x = 6381,
                    variable =4099,
                    period =c("last"=97),
                    geo = "Brazil",
                    header = TRUE,
                    format = 1)
x = names(taxasd)
x[(names(taxasd) == "Valor")] = "Taxa de Desocupa��o (%) - 4099"
colnames(taxasd) = x

###### Tabela: Taxa de Subocupa��o e Desocupa��o ######
taxaso <- get_sidra(x = 6439,
                    variable =4114,
                    period =c("last"=95),
                    geo = "Brazil",
                    header = TRUE,
                    format = 1)
y = names(taxaso)
y[(names(taxaso) == "Valor")] = "Taxa de Subocupa��o e Desocupa��o (%) - 4114"
colnames(taxaso) = y

###### Tabela: Taxa Composta de Subutiliza��o ######
taxasu <- get_sidra(x = 6441,
                    variable =4118,
                    period =c("last"=95),
                    geo = "Brazil",
                    header = TRUE,
                    format = 1)
z = names(taxasu)
z[(names(taxasu) == "Valor")] = "Taxa Composta de Subutiliza��o (%) - 4118"
colnames(taxasu) = z
###### Compilando os dados na mesma tabela e excluindo colunas sem utliza��o ###########
taxasdo <- left_join(taxasd, taxaso, by=c("N�vel Territorial (C�digo)","Brasil (C�digo)", 
                                          "Trimestre M�vel (C�digo)", "Unidade de Medida (C�digo)"))
TaxasMensal <- left_join(taxasdo, taxasu, by=c("N�vel Territorial (C�digo)","Brasil (C�digo)", 
                                         "Trimestre M�vel (C�digo)", "Unidade de Medida (C�digo)"))
TaxasMensal$`N�vel Territorial (C�digo)` <- NULL
TaxasMensal$`Brasil (C�digo)` <- NULL
TaxasMensal$`Unidade de Medida (C�digo)` <- NULL
TaxasMensal$`Vari�vel (C�digo).x` <- NULL
TaxasMensal$`Vari�vel (C�digo).y` <- NULL
TaxasMensal$`Vari�vel (C�digo)` <- NULL
TaxasMensal <- rename(TaxasMensal,`Trimestre M�vel` = `Trimestre M�vel (C�digo)`)

TaxasMensal <- separate(TaxasMensal, `Trimestre M�vel`, sep = c(4,6), 
                  into = c("Ano", "M�s"))
TaxasMensal <- arrange(TaxasMensal, `M�s`)


############## SEGUNDA PLANILHA: Ocupa��o por Setor e UF #############

###### Importando dados e gerando tabelas #######
setorm <- get_sidra(x = 6323,
                   variable =4090,
                   period =c("last"=97),
                   geo = "Brazil",
                   header = TRUE,
                   format = 1)
w = names(setorm)
w[(names(setorm) == "Grupamento de atividades no trabalho principal - PNADC (C�digo)")] = "Pessoas Ocupadas por Setor - 5434"
colnames(setorm) = w
###### Formatando r�tulos e excluindo colunas sem utiliza��o ############
setorm$`N�vel Territorial (C�digo)` <- NULL
setorm$`Unidade de Medida (C�digo)` <- NULL
setorm$`Brasil (C�digo)` <- NULL
setorm$`Vari�vel (C�digo)` <- NULL
setorm <- rename(setorm, `Trimestre M�vel` = `Trimestre M�vel (C�digo)`)
######### Formatando r�tulos da vari�vel UF e Pessoas ocupadas por setor ##########
setorm <- setorm %>% 
  mutate(`Pessoas Ocupadas por Setor - 5434`= ifelse(`Pessoas Ocupadas por Setor - 5434` == 33355, "Total - 33355",
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

setorm <- setorm %>% 
  spread(`Pessoas Ocupadas por Setor - 5434`, Valor)

setorm <- separate(setorm, `Trimestre M�vel`, sep = c(4,6), 
                  into = c("Ano", "M�s"))
setorm <- arrange(setorm, `M�s`)


############## TERCEIRA PLANILHA: Ocupa��o por Categorias e UF #############

###### Importando dados e gerando tabelas #######
categoriam <- get_sidra(x = 6320,
                       variable =4090,
                       period =c("last"=97),
                       geo = "Brazil",
                       header = TRUE,
                       format = 1)
v = names(categoriam)
v[(names(categoriam) == "Posi��o na ocupa��o e categoria do emprego no trabalho principal (C�digo)")] = "Pessoas Ocupadas por Categoria - 4097"
colnames(categoriam) = v

###### Formatando r�tulos e excluindo colunas sem utiliza��o ############
categoriam$`N�vel Territorial (C�digo)` <- NULL
categoriam$`Unidade de Medida (C�digo)` <- NULL
categoriam$`Brasil (C�digo)` <- NULL
categoriam$`Vari�vel (C�digo)` <- NULL
categoriam <- rename(categoriam, `Trimestre M�vel` = `Trimestre M�vel (C�digo)`)
######### Formatando r�tulos da vari�vel UF e Pessoas ocupadas por categoria ##########
categoriam <- categoriam %>% 
  mutate(`Pessoas Ocupadas por Categoria - 4097`= ifelse(`Pessoas Ocupadas por Categoria - 4097` == 96165, "Total - 96165",
                                                         ifelse(`Pessoas Ocupadas por Categoria - 4097` == 96166, "Empregado",
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
                                                         ifelse(`Pessoas Ocupadas por Categoria - 4097` == 45934, "Empregador com CNPJ - 45934",
                                                         ifelse(`Pessoas Ocupadas por Categoria - 4097` == 45935, "Empregador sem CNPJ - 45935",
                                                         ifelse(`Pessoas Ocupadas por Categoria - 4097` == 96171, "Conta pr�pria - 96171",
                                                         ifelse(`Pessoas Ocupadas por Categoria - 4097` == 45936, "Conta pr�pria com CNPJ - 45936",
                                                         ifelse(`Pessoas Ocupadas por Categoria - 4097` == 45937, "Conta pr�pria sem CNPJ - 45937",
                                                         ifelse(`Pessoas Ocupadas por Categoria - 4097` == 31731, "Trabalhador familiar auxiliar - 31731", NA))))))))))))))))))))

categoriam <- categoriam %>% 
  spread(`Pessoas Ocupadas por Categoria - 4097`, Valor)

categoriam <- separate(categoriam, `Trimestre M�vel`, sep = c(4,6), 
                      into = c("Ano", "M�s"))
categoriam <- arrange(categoriam, `M�s`)

######### Exportando dados ###########
DadosMensalIBGE <- createWorkbook()
addWorksheet(DadosMensalIBGE, "Taxas Mensal")
addWorksheet(DadosMensalIBGE, "Setor Mensal")
addWorksheet(DadosMensalIBGE, "Categoria Mensal")

writeData(DadosMensalIBGE, "Taxas Mensal", TaxasMensal)
writeData(DadosMensalIBGE, "Setor Mensal", setorm)
writeData(DadosMensalIBGE, "Categoria Mensal", categoriam)

saveWorkbook(DadosMensalIBGE,"C:/UNIEPRO/DadosMensalIBGE.xlsx" , overwrite = T)
