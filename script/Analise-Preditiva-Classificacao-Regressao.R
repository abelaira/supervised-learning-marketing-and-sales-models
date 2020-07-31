
# ===================================================================================================
# Case - Modelagem Preditiva Avançada
# -----------------------------------
# • A multinacional  de varejo Waldata está querendo expandir a sua presença na américa latina e 
# por isso decide firmar uma  parceria com a FGV para desenvolver um modelo preditivo do valor 
# de vendas. Além disso a companhia decide apostar em um segundo modelo de ‘targetads’ tornando 
# mais efetiva as campanhas de marketing. Assim a rede varejista pretende melhorar suas projeções 
# de fluxo  de caixa e otimizar a distribuição de seus produtos por departamentos.
#
# • Para desenvolver seu modelo você irá realizar as seguintes tarefas:
#
# 1) Importar os datasets RETAIL e MARKETING para o ambiente R.
#
# 2) Fazer uma exploração detalhada dos dados. (Distribuições, valores faltantes etc..)
#
# 3) Dividir  as bases em 70% para treino e 30% para teste do modelo. (Utilize  sempre seed(314)).
#
# 4) Testar modelos de classificação para as campanhas de marketing:
#      4.1) Regressão Logística, Árvores de Decisão, SVM , Redes Neurais e Algoritmo Genético 
#           para featureselection.
#
# 5) Testar modelos de regressão para o valor de vendas das lojas:
#      5.1) Regressão Linear, Árvore de Decisão, e Redes Neurais.
#
# 6) Validar  a performance dos modelos (R2 & Matriz de Confusão).
#
# 7) Fazer o “scoring” dos modelos para os dados nas respectivas bases de teste.
#
# ===================================================================================================

rm(list = ls())
getwd()
cores <- 8

# -------------------------------------------------------------
# 1) Importar os datasets RETAIL e MARKETING para o ambiente R.

if (!require(data.table)) install.packages('data.table')
require(data.table)

if (!require(stringr)) install.packages('stringr')
require(stringr)

if (!require(lubridate)) install.packages('lubridate')
require(lubridate)


# Importando os dados
retail <- fread(file = './data/Retail.csv')
str(retail)
retail
# Descrição Dataset
# -----------------
# Feature ========	Feature Description
# STORE..........:	ID da Loja
# DATE...........:	Datetime
# TEMPERATURE....:	Temperatura em Fahrenheit
# FUEL_PRICE.....:	Preço Galão Combustível em USD
# MARKDOWN1......:	Redução de Preço
# MARKDOWN2......:	Redução de Preço
# MARKDOWN3......:	Redução de Preço
# MARKDOWN4......:	Redução de Preço
# MARKDOWN5......:	Redução de Preço
# CPI............:	Consumer Price Index - Inflação
# UNEMPLOYMENT...:	Taxa de Desemprego
# ISHOLIDAY......:	Feriado ?
# WEEKLY_SALES...:	Valor Total de Vendas na Semanda em USD

market <- fread(file = './data/Marketing.csv')
str(market)
market
# Descrição Dataset
# -----------------
# Feature ==========	Feature Description
# AGE..............:	Idade
# JOB..............:	Profissão
# MARITAL_STATUS...:	Estado civil
# EDUCATION........:	Educação
# DEFAULT..........:	Contas Atrasadas ?
# HOUSING..........:	Hipoteca ?
# LOAN.............:	Empréstimo Pessoal ?
# CONTACT..........:	Tipo de Contato
# MONTH............:	Último Mês de Contato
# DAY_OF_WEEK......:	Último Dia da Semana de Contato
# DURATION.........:	Duração do Último Contato em segundos
# CAMPAIGN.........:	Tipo de Campanha de Marketing
# PDAYS............:	Número de Dias desde Último Contato (-1: Não Houve Contato)
# PREVIOUS.........:	Número de Contatos Antes da Campanha
# POUTCOME.........:	Resultado da Última Campanha
# EMP_VAR_RATE.....:	Taxa de Desemprego da Região
# CONS_PRICE_IDX...:	IGPM
# CONS_CONF_IDX....:	Índice de Confiança do Consumidor
# SUBSCRIBED.......:	Aderiu ao Serviço ?


# Tratando os data sets
unique(retail$Weekly_Sales) # número para ser convertido

retail[,':='(IsHoliday = as.factor(retail$IsHoliday),
             Weekly_Sales = as.double(retail$Weekly_Sales),
             Date = dmy(retail$Date))]
str(retail)
retail

unique(market$JOB) # fator sem ordem definida
unique(market$MARITAL_STATUS) # fator sem ordem definida
unique(market$EDUCATION) # fator ordenado: unknown, illiterate, basic_4y, basic_6y, basic_9y, high_school, professional_course, university_degree
unique(market$DEFAULT) # fator sem ordem definida
unique(market$HOUSING) # fator sem ordem definida
unique(market$LOAN) # fator sem ordem definida
unique(market$CONTACT) # fator sem ordem definida
unique(market$MONTH) # fator sem ordem definida
unique(market$DAY_OF_WEEK) # fator sem ordem definida
unique(market$POUTCOME) # fator sem ordem definida

unique(market$EMP_VAR_RATE) # número com formato incorreto, undescore no lugar do ponto
unique(market$CONS_PRICE_IDX) # número para ser convertido
unique(market$CONS_CONF_IDX) # número com formato incorreto, undescore no lugar do ponto

unique(market$SUBSCRIBED) # fator sem ordem definida

unique(str_replace_all(string = market$EMP_VAR_RATE, pattern = '_', replacement = '.'))
unique(str_replace_all(string = market$CONS_CONF_IDX, pattern = '_', replacement = '.'))

market[, ':='(JOB = as.factor(market$JOB),
              MARITAL_STATUS = as.factor(market$MARITAL_STATUS),
              EDUCATION = factor(market$EDUCATION,
                                 ordered = FALSE, 
                                 levels = c('unknown', 'illiterate', 'basic_4y', 'basic_6y', 'basic_9y',
                                            'high_school', 'professional_course', 'university_degree')),
              DEFAULT = as.factor(market$DEFAULT),
              HOUSING = as.factor(market$HOUSING),
              LOAN = as.factor(market$LOAN),
              CONTACT = as.factor(market$CONTACT),
              MONTH = as.factor(market$MONTH),
              DAY_OF_WEEK = as.factor(market$DAY_OF_WEEK),
              POUTCOME = as.factor(market$POUTCOME),
              EMP_VAR_RATE = as.numeric(str_replace_all(string = market$EMP_VAR_RATE, pattern = '_', replacement = '.')),
              CONS_PRICE_IDX = as.numeric(market$CONS_PRICE_IDX),
              CONS_CONF_IDX = as.numeric(str_replace_all(string = market$CONS_CONF_IDX, pattern = '_', replacement = '.')),
              SUBSCRIBED = as.factor(market$SUBSCRIBED))]
str(market)
market


# -------------------------------------------------------------------------------------
# 2) Fazer uma exploração detalhada dos dados. (Distribuições, valores faltantes etc..)

if (!require(mice)) install.packages('mice')
require(mice)

if (!require(ggplot2)) install.packages('ggplot2')
require(ggplot2)

if (!require(corrplot)) install.packages('corrplot')
require(corrplot)

if (!require(caret)) install.packages('caret')
require(caret)

if (!require(DMwR2)) install.packages('DMwR2')
require(DMwR2)

if (!require(doParallel)) install.packages('doParallel')
require(doParallel)

if (!require(gridExtra)) install.packages('gridExtra')
require(gridExtra)


# Verificando padrão de NA's nos data sets
# ----- Para o Dataset RETAIL ------
par(mar=c(1,1,1,1))
na_retail <- md.pattern(retail, rotate.names = TRUE)
# O Dataset possuem NA's nas colunas CPI, Unemployment, Weekly_Sales, MarkDown1, MarkDown2, MarkDown3, 
# MarkDown4, MarkDown5.

na_retail_ord <- setnames(as.data.table(na_retail),'V14','Qtd.NA')[,Qtd.Linhas:=rownames(na_retail)][,.SD,keyby=Qtd.NA]
na_retail_ord
na_retail_ord[31,]
nrow(retail)
na_retail_ord[31,
              c('Store','Date','Temperature','Fuel_Price','IsHoliday','CPI','Unemployment','Weekly_Sales',
                'MarkDown1','MarkDown2','MarkDown3','MarkDown4','MarkDown5')] / nrow(retail)
# Cerca de 50% das linhas estão sem MarkDown1-5. Não dá para descartá-los
# Cerca de 7% não possuem CPI (Inflação) e Unemployment (Taxa de Desemprego)
# Cerca de 21% não possui Weekly_Sales (Valor total das vendas na semana)

# Preenchendo os NA's no data set 
# Preenchendo os NA's das colunas CPI e Unemployment com o último valor disponível.
# Ordenando por loja e data, assim o último valor será referente a loja com informação da última data
retail2 <- copy(retail)
setorder(retail2, Store, Date)
setnafill(retail2, type = c('locf'), cols = c('CPI', 'Unemployment'))

# Para a variável *Weekly_Sales*, como é a variável *target* para o modelo de previsão de faturamento, 
# qualquer método de imputação pode criar um viés no modelo. Portanto, a melhor alternativa 
# nessa caso será o descarte das linhas com NA's. Como isso perderemos 21% do *Data Set* de faturamento.
retail2 <- na.omit(retail, cols = 'Weekly_Sales')

# Preenchendo os NA's das colunas MarkDown's1-5 (Redução de Preço) com zero
setnafill(retail2, type = c('const'), fill = 0, 
          cols = c('MarkDown1','MarkDown2','MarkDown3','MarkDown4','MarkDown5'))

# na_mark <- is.na(retail2$MarkDown1)
# retail2[which(na_mark), MarkDown1 := 0]
# na_mark <- is.na(retail2$MarkDown2)
# retail2[which(na_mark), MarkDown2 := 0]
# na_mark <- is.na(retail2$MarkDown3)
# retail2[which(na_mark), MarkDown3 := 0]
# na_mark <- is.na(retail2$MarkDown4)
# retail2[which(na_mark), MarkDown4 := 0]
# na_mark <- is.na(retail2$MarkDown5)
# retail2[which(na_mark), MarkDown5 := 0]

md.pattern(retail2, rotate.names = TRUE)


# ----- Para o Dataset MARKET -----
na_market <- md.pattern(market, rotate.names = TRUE)
# O Dataset possue NA's na coluna CONS_PRICE_IDX.

as.data.table(na_market)
na_market_ord <- setnames(as.data.table(na_market),'V20','Qtd.NA')[,Qtd.Linhas:=rownames(na_market)][,.SD,keyby=Qtd.NA]
na_market_ord
na_market_ord[3,]
nrow(market)
na_market_ord[3,
              c('AGE','JOB','MARITAL_STATUS','EDUCATION','DEFAULT','HOUSING','LOAN','CONTACT','MONTH',
                'DAY_OF_WEEK','DURATION','CAMPAIGN','PDAYS','PREVIOUS','POUTCOME','EMP_VAR_RATE',
                'CONS_PRICE_IDX','CONS_CONF_IDX','SUBSCRIBED')] / nrow(market)
# Cerca de 8% das linhas estão sem CONS_PRICE_IDX

# Retirando as linhas com NA's do data set
market2 <- na.omit(market, cols = 'CONS_PRICE_IDX')
# Como percentualmente os valores com NA's são poucos em relação ao total de linhas, optou-se por 
# retirá-los, dado que seu preenchimento poderia causar distorções pois as demais variáveis não possuem
# relação direta com CONS_PRICE_IDX (Índice de preço)
md.pattern(market2, rotate.names = TRUE)


# Analisando o Dataset de Faturamento de Loja - Retail
corrplot(cor(retail2[,-c('Date','IsHoliday')]),
         method = 'square',
         type = 'lower',
         diag = FALSE,
         title = 'Correlação',
         mar = c(1,1,1,1),
         addCoefasPercent = TRUE,
         addCoef.col = 'gray50',
         number.digits = 0)
# O Dataset possui uma alta correlação (>70%) positiva entre MarkDown1 e MarkDown4 (83%).
# Nâo possui itens com correlação positiva mediana (>50% e <70%).
# Possui baixa correlação positiva (>15% e <50%) entre: Fuel_Price e MarkDown1 (26%); 
# Fuel_Price e MarkDown4 (15%); Temperature e CPI (16%); Unemployment e Store (22%); 
# MarkDown1 e MarkDown2 (16%); MarkDown1 e MarkDown5 (18%); MarkDown1 e Weekly_Sales (20%); 
# MarkDown4 e Weekly_Sales (15%).
#
# Não possui itens com correlação negativa mediana e correlação negativa alta.
# Possui baixa correlação negativa (>15% e <50%) entre: CPI e Unemployment (30%); CPI e Store (21%); 
# CPI e Fuel_Price (19%); Store e Weekly_Sales (33%); Temperature e MorkDown2 (22%).


# ----------------------------------------------------------------------------------------------
# ----- Avaliando outras formas de imputação dos NA's no Data Set de Faturamento das Lojas
retail3 <- na.omit(retail, cols = 'Weekly_Sales')

ggplot(data = na.omit(retail), aes(y = Weekly_Sales)) +
  geom_point(aes(x = CPI), color = 'black', alpha = 0.2) +
  geom_point(aes(x = Unemployment), color = 'red', alpha = 0.2) +
  labs(x = 'CPI:preto, Unemployment:vermelho')

ggplot(data = retail2, aes(y = Weekly_Sales)) +
  geom_point(aes(x = CPI), color = 'black', alpha = 0.2) +
  geom_point(aes(x = Unemployment), color = 'red', alpha = 0.2) +
  labs(x = 'CPI:preto, Unemployment:vermelho')
# Verificando a distribuição dos dados com a variável CPI e Unemployment, percebe-se que
# houve um acréscimo grande nos grupos de das variáveis CPI e Unemployment.


# Para as variáveis MarkDowns1-5
g1 <- ggplot(data = retail2, aes(y = Weekly_Sales)) +
  geom_point(aes(x = MarkDown1), color = 'black', alpha = 0.4) +
  scale_x_log10()
g2 <- ggplot(data = retail2, aes(y = Weekly_Sales)) +
  geom_point(aes(x = MarkDown3), color = 'green', alpha = 0.4) +
  scale_x_log10()
g3 <- ggplot(data = retail2, aes(y = Weekly_Sales)) +
  geom_point(aes(x = MarkDown4), color = 'blue', alpha = 0.4) +
  scale_x_log10()
g4 <- ggplot(data = retail2, aes(y = Weekly_Sales)) +
  geom_point(aes(x = MarkDown5), color = 'red', alpha = 0.4) +
  scale_x_log10()
grid.arrange(g1 , g2 ,
             g3 , g4 ,
             ncol=2, nrow=2)
# O gráfico acima mostra a dependência da coluna Weekly_Sales com as variáves de MarkDown1-5 e
# o impacto que ocorreu na imputação dos valores ausentes nas variávies MarkDown, onde os valores
# ausentes foram preenchidos com valores zerados. A concentração dos valores zerados distorcem
# demasiadamente a relação com a variável Weekly_Sales.
#
# Para tentar corrigir as distorções nos MarkDowns1-5, CPI e Unemployment, optou-se por um 
# tratamento mais sofisticado para imputar os valores ausentes nas variáveis MarkDown1-5
# através do KNN-5.
#
# Ajustando o processamento paralelo em CORES (NÚCLEOS)
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

target <- retail3$Weekly_Sales
# OBS: O uso do KNN para o preenchimento dos NA's adiciona uma camada preditiva no tratatmento 
# de dados. O uso de KNN é comum para preenchimento de NA's pois consegue ser robusto e simples 
# ao mesmo tempo.
system.time(retail3 <- knnImputation(retail3[,!names(retail3) %in% "Weekly_Sales", with=F], 
                                            k = 5, 
                                            meth = 'weighAvg'))
retail3[,Weekly_Sales := target]

# Parando o processamento paralelo
stopCluster(cl)
anyNA(retail3)

# Verificando a correlação resultante
corrplot(cor(retail3[,-c('Date','IsHoliday')]),
         method = 'square',
         type = 'lower',
         diag = FALSE,
         title = 'Correlação',
         mar = c(1,1,1,1),
         addCoefasPercent = TRUE,
         addCoef.col = 'gray50',
         number.digits = 0)

# Para as variáveis CPI e Unemployment
g1 <- ggplot(data = retail2, aes(y = Weekly_Sales)) +
  geom_point(aes(x = CPI), color = 'black', alpha = 0.2) +
  geom_point(aes(x = Unemployment), color = 'red', alpha = 0.2) +
  labs(x = 'CPI:preto, Unemployment:vermelho', title = 'Imputação LOF')
g2 <- ggplot(data = retail3, aes(y = Weekly_Sales)) +
  geom_point(aes(x = CPI), color = 'black', alpha = 0.2) +
  geom_point(aes(x = Unemployment), color = 'red', alpha = 0.2) +
  labs(x = 'CPI:preto, Unemployment:vermelho', title = 'Imputação KNN-5')
grid.arrange(g1 , g2 ,
             ncol=1, nrow=2)

# Para as variáveis MarkDowns1-5
g1 <- ggplot(data = retail3, aes(y = Weekly_Sales)) +
  geom_point(aes(x = MarkDown1), color = 'black', alpha = 0.4) +
  scale_x_log10()
g2 <- ggplot(data = retail3, aes(y = Weekly_Sales)) +
  geom_point(aes(x = MarkDown3), color = 'green', alpha = 0.4) +
  scale_x_log10()
g3 <- ggplot(data = retail3, aes(y = Weekly_Sales)) +
  geom_point(aes(x = MarkDown4), color = 'blue', alpha = 0.4) +
  scale_x_log10()
g4 <- ggplot(data = retail3, aes(y = Weekly_Sales)) +
  geom_point(aes(x = MarkDown5), color = 'red', alpha = 0.4) +
  scale_x_log10()
grid.arrange(g1 , g2 ,
             g3 , g4 ,
             ncol=2, nrow=2)
# Percebe-se que para as variáveis MarkDown1-5 o viés dos valores zerados desaparaceram,
# impactando significativamente na correlação dessas variáveis com a variável Weekly_Sales.
# No caso das variáveis CPI e Unemployment, pouca coisa mudou.


# ----- Analisando o Dataset de Marketing
corrplot(cor(market2[,-c('JOB','MARITAL_STATUS','EDUCATION','DEFAULT','HOUSING','LOAN','CONTACT',
                         'MONTH','DAY_OF_WEEK','POUTCOME','SUBSCRIBED')]),
         method = 'square',
         type = 'lower',
         diag = FALSE,
         title = 'Correlação',
         mar = c(1,1,1,1),
         addCoefasPercent = TRUE,
         addCoef.col = 'gray50',
         number.digits = 0)
# O Dataset possui uma alta correlação positivo (>70%) entre: EMP_VAR_RATE e CONS_PRICE_IDX (78%).
# Possui correlação positiva mediana (>50% e <70%) entre: PDAYS e PREVIOUS (52%).
# Possui baixa correlação positiva (>15% e <50%) entre: EMP_VAR_RATE e CONS_CONF_IDX (19%); 
# CAMPAIGN e EMP_VAR_RATE (15%).
#
# Não possui itens com alta correlação negativa ou mediana.
# Possui baixa correlação negativa (>15% e <50%) entre: PREVIOUS e EMP_VAR_RATE (43%); 
# PDAYS e EMP_VAR_RATE (23%); PREVIOUS e CONS_PRICE_IDX (21%).


# Dado as correlações envolvendo CONS_PRICE_IDX: 
# CONS_PRICE_IDX (Índice de preço) e EMP_VAR_RATE (Taxa de desemprego) (78%),
# CONS_PRICE_IDX e PREVIOUS (Número de Contatos Antes da Campanha) (-21%),
# CONS_PRICE_IDX e CAMPAIGN (Tipo de Campanha de Marketing) (12%).
# Como PREVIOUS e CAMPAIGN possuem baixa correlação com CONS_PRICE_IDX e não possuem
# uma relação aparente com a variável CONS_PRICE_IDX, serão desconsideradas.
# Faremos uma análise somente com a variável EMP_VAR_RATE.
market2[,c('CONS_PRICE_IDX','EMP_VAR_RATE')] %>% ggplot(aes(y = CONS_PRICE_IDX, x = EMP_VAR_RATE)) +
  geom_point() +
  scale_y_continuous(limits = c(92,95), breaks = c(92,92.5,93,93.5,94,94.5,95)) +
  scale_x_continuous(limits = c(-3.5,2), breaks = c(-3.5,-3,-2.5,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2))
# Não se percebe qualquer relação linear entre as duas variáveis. Inclusive, para o mesmo valor de
# EMP_VAR_RATE encontramos diferentes valores para CONS_PRICE_IDX. Talvez para valores médios de 
# CONS_PRICE_IDX tenhamos uma relação linear com EMP_VAR_RATE.
# Portanto, foi desconsiderada a possibilidade de imputação de CONS_PRICE_IDX a partir de EMP_VAR_RATE.


# Considerando que CONS_PRICE_IDX seja uma variável que capta flutuações da inflação, logo torna-se
# dependente da data em que foi apurada. 
# Faremos uma análise de CONS_PRICE_IDX, MONTH e DAY_OF_WEEK, pois foi considerado que o valor de 
# CONS_PRICE_IDX foi obtido no mês e dia da semana de contato.
market[is.na(CONS_PRICE_IDX),c('CONS_PRICE_IDX','MONTH','DAY_OF_WEEK')] %>% 
  ggplot(aes(x = DAY_OF_WEEK, y = as.factor(CONS_PRICE_IDX))) +
  geom_jitter(aes(color = MONTH)) +
  labs(y = 'CONS_PRICE_IDX')
# Percebe-se que os valores da variável CONS_PRICE_IDX preenchidos com 'NA', estão todos no mês de 
# NOVEMBRO, sendo distribuídos de forma igualitária entre SEGUNDA, TERÇA, QUARTA, QUINTA e SEXTA.

# Fazendo uma análise no DataSet direcionada para MONTH igual a NOVEMBRO, teremos.
market[MONTH == 'nov',c('CONS_PRICE_IDX','MONTH')] %>% 
  ggplot(aes(y = MONTH, x = as.factor(CONS_PRICE_IDX))) +
  geom_jitter() +
  labs(x = 'CONS_PRICE_IDX')
# Mostrando que para o mês de novembro temos dois valores conhecidos para CONS_PRICE_IDX: 92.649 e 94.767.
# Isso me leva a concluir que esses dois valores representam anos diferentes para a variável CONS_PRICE_IDX.
# Como não temos essa informação no DataSet, não temos como imputar valor em CONS_PRICE_IDX através da
# variável MONTH, pois nos falta informação do ano.


market_mes <- data.table()
market_mes[, ':='(ID = c(3,4,5,6,7,8,9,10,11,12),
                  MES = factor(c('mar','apr','may','jun','jul','aug','sep','oct','nov','dec')),
                  QTD = c(nrow(market[MONTH == 'mar',c('CONS_PRICE_IDX','MONTH')]),
                          nrow(market[MONTH == 'apr',c('CONS_PRICE_IDX','MONTH')]),
                          nrow(market[MONTH == 'may',c('CONS_PRICE_IDX','MONTH')]),
                          nrow(market[MONTH == 'jun',c('CONS_PRICE_IDX','MONTH')]),
                          nrow(market[MONTH == 'jul',c('CONS_PRICE_IDX','MONTH')]),
                          nrow(market[MONTH == 'aug',c('CONS_PRICE_IDX','MONTH')]),
                          nrow(market[MONTH == 'sep',c('CONS_PRICE_IDX','MONTH')]),
                          nrow(market[MONTH == 'oct',c('CONS_PRICE_IDX','MONTH')]),
                          nrow(market[MONTH == 'nov',c('CONS_PRICE_IDX','MONTH')]),
                          nrow(market[MONTH == 'dec',c('CONS_PRICE_IDX','MONTH')])))]
as.data.frame(market_mes) %>% ggplot(aes(x = as.factor(ID), y = QTD)) +
  geom_point(colour = 'red', shape = -9, size = 3) +
  geom_segment(aes(x = as.factor(market_mes$ID), xend = as.factor(market_mes$ID), y = 0, yend = QTD),
               colour = 'blue', size = 5, alpha = 0.6) +
  geom_segment(aes(x = '11', xend = '11', y = 0, 
                   yend = nrow(market[is.na(CONS_PRICE_IDX) & MONTH == 'nov',c('CONS_PRICE_IDX','MONTH')])),
               colour = 'red', size = 5, alpha = 0.05) +
  scale_x_discrete(labels = market_mes$MES) +
  scale_y_continuous(breaks = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000,12000,13000,14000),
                     labels = c('0','1.000','2.000','3.000','4.000','5.000','6.000','7.000','8.000','9.000','10.000','11.000','12.000','13.000','14.000')) +
  annotate("text", 
           label = paste(round(nrow(market[is.na(CONS_PRICE_IDX) & MONTH == 'nov',c('CONS_PRICE_IDX','MONTH')]) /
                                 nrow(market[MONTH == 'nov',c('CONS_PRICE_IDX','MONTH')]) * 100, 2), "%"), 
           x = 9.3, y = 3500, size = 3, colour = "black", fontface = 'bold')
# Conclusão, a melhor solução a adotar nesse caso será a retirada de toda a variável CONS_PRICE_IDX 
# da modelagem, esse solução é a que possivelmente produzirá menos viés. Pois os valores NA's, que estão 
# localizados todos no mês de NOVEMBRO, representam 88,17% do conjunto desse mês.
market3 <- copy(market)
market3[,CONS_PRICE_IDX:=NULL]
market3


# ------------------------------------------------------------------------------------------------
# 3) Dividir  as bases em 70% para treino e 30% para teste do modelo. (Utilize  sempre seed(314)).

if (!require(MLmetrics)) install.packages('MLmetrics')
require(MLmetrics)

if (!require(InformationValue)) install.packages('InformationValue')
require(InformationValue)


set.seed(314)
i_retail <- createDataPartition(retail2$Weekly_Sales, p = 0.7, list = FALSE)
retail2_train <- retail2[i_retail,]
retail2_test <- retail2[-i_retail,]

set.seed(314)
i_retail <- createDataPartition(retail3$Weekly_Sales, p = 0.7, list = FALSE)
retail3_train <- retail3[i_retail,]
retail3_test <- retail3[-i_retail,]

set.seed(314)
i_market <- createDataPartition(market2$SUBSCRIBED, p = 0.7, list = FALSE)
market2_train <- market2[i_market,]
market2_test <- market2[-i_market,]

set.seed(314)
i_market <- createDataPartition(market3$SUBSCRIBED, p = 0.7, list = FALSE)
market3_train <- market3[i_market,]
market3_test <- market3[-i_market,]


# Avaliando o melhor Dataset de Retail para ser utilizado
if (FALSE) {
  # Ajustando o processamento paralelo em CORES (NÚCLEOS)
  cl <- makePSOCKcluster(cores)
  registerDoParallel(cl)
  
  # ---------------------------------------------------------------------------------
  # Usando o Dataset Retail2 (com preenchimento de valores zerados para MarkDowns1-5)
  # ----- Regressão Linear -----
  # Treinando o modelo
  set.seed(314)
  cv <- trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = TRUE,
                     summaryFunction = defaultSummary)
  
  mod_retail2_rl <- train(Weekly_Sales ~ ., 
                         data = retail2_train,
                         method = "lm", 
                         metric = 'RMSE',
                         trControl = cv)
  summary(mod_retail2_rl)
  
  mod_retail2_rl <- train(Weekly_Sales ~ ., 
                          data = retail2_train[,-c('Temperature')],
                          method = "lm", 
                          metric = 'RMSE',
                          trControl = cv)
  summary(mod_retail2_rl)
  
  mod_retail2_rl <- train(Weekly_Sales ~ ., 
                          data = retail2_train[,-c('IsHoliday','Temperature')],
                          method = "lm", 
                          metric = 'RMSE',
                          trControl = cv)
  summary(mod_retail2_rl)

    mod_retail2_rl <- train(Weekly_Sales ~ ., 
                          data = retail2_train[,-c('IsHoliday','Temperature','MarkDown4')],
                          method = "lm", 
                          metric = 'RMSE',
                          trControl = cv)
  summary(mod_retail2_rl)
  pred_retail2_rl <- predict(mod_retail2_rl, newdata = retail2_test)
  R2_Score(y_pred = pred_retail2_rl, y_true = retail2_test$Weekly_Sales)
  RMSE(y_pred = pred_retail2_rl, y_true = retail2_test$Weekly_Sales)

    
  # ----------------------------------------------------------------------
  # Usando o Dataset Retail3 (com preenchimento KNN-5 os valores ausentes)
  # ----- Regressão Linear -----
  set.seed(314)
  mod_retail3_rl <- train(Weekly_Sales ~ ., 
                          data = retail3_train,
                          method = "lm", 
                          metric = 'RMSE',
                          trControl = cv)
  summary(mod_retail3_rl)
  
  mod_retail3_rl <- train(Weekly_Sales ~ ., 
                          data = retail3_train[,-c('Fuel_Price')],
                          method = "lm", 
                          metric = 'RMSE',
                          trControl = cv)
  summary(mod_retail3_rl)
  
  mod_retail3_rl <- train(Weekly_Sales ~ ., 
                          data = retail3_train[,-c('Fuel_Price','Temperature')],
                          method = "lm", 
                          metric = 'RMSE',
                          trControl = cv)
  summary(mod_retail3_rl)
  
  mod_retail3_rl <- train(Weekly_Sales ~ ., 
                          data = retail3_train[,-c('Fuel_Price','Temperature','MarkDown4')],
                          method = "lm", 
                          metric = 'RMSE',
                          trControl = cv)
  summary(mod_retail3_rl)
  pred_retail3_rl <- predict(mod_retail3_rl, newdata = retail3_test)
  R2_Score(y_pred = pred_retail3_rl, y_true = retail3_test$Weekly_Sales)
  RMSE(y_pred = pred_retail3_rl, y_true = retail3_test$Weekly_Sales)
  
  
  # Parando o processamento paralelo
  stopCluster(cl)
}
# Setando o melhor Dataset para Market
dt_retail_train <- retail3_train
dt_retail_test <- retail3_test
# O uso do KNN-5 para imputação dos NA's do Data Set Retail se mostrou melhor que q imputação
# de zeros nas variáveis MarkDown1-5 e preenchimento com o último valor conhecido para as
# variáveis CPI e Unemployment. Apesar do R2 ter sido muito baixo, na imputação pelo KNN-5
# ele ficou melhor. Essa evidência do R2, somada ao RMSE apontam que o melhor Data Set
# para o Faturamento das Lojas (Retail) é o utilzado pelo segundo modelo, que é o Data Set
# (Retail3).



# Avaliando o melhor Dataset de Market para ser utilizado
if (FALSE) {
  # Ajustando o processamento paralelo em CORES (NÚCLEOS)
  cl <- makePSOCKcluster(cores)
  registerDoParallel(cl)
  
  cv <- trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = TRUE,
                     summaryFunction = twoClassSummary, classProbs = TRUE)
  
  # -------------------------------------------------------------------
  # Usando o Dataset Market2 (sem as linhas com NA's de CONS_PRICE_IDX)
  # ----- Regressão Logística -----
  mod_market2_rl <- train(SUBSCRIBED ~ .,
                          data = market2_train, 
                          method = "glm", 
                          metric = "ROC", 
                          trControl = cv)
  mod_market2_rl
  pred_market2_rl <- predict(mod_market2_rl, newdata = market2_test, type = 'prob')
  
  plotROC(actuals = as.integer(market2_test$SUBSCRIBED)-1, predictedScores = pred_market2_rl$yes)
  AUC(pred_market2_rl$yes, as.integer(market2_test$SUBSCRIBED)-1)
  LogLoss(pred_market2_rl$yes, as.integer(market2_test$SUBSCRIBED)-1)
  
  (cutoff <- optimalCutoff(actuals = as.integer(market2_test$SUBSCRIBED)-1, predictedScores = pred_market2_rl$yes))
  pred_market2 <- ifelse(pred_market2_rl$yes > cutoff, 'yes', 'no')
  pred_market2 <- as.factor(pred_market2)
  (mc_market2_rl <- caret::confusionMatrix(data = pred_market2, positive = 'yes', 
                                           reference = market2_test$SUBSCRIBED,
                                           mode = 'everything'))
  
  cutoff <- 0.5
  pred_market2a <- ifelse(pred_market2_rl$yes > cutoff, 'yes', 'no')
  pred_market2a <- as.factor(pred_market2a)
  (mc_market2_rla <- caret::confusionMatrix(data = pred_market2a, positive = 'yes', 
                                            reference = market2_test$SUBSCRIBED,
                                            mode = 'everything'))
  
  # -------------------------------------------------------------------
  # Usando o Dataset Market3 (sem a variável CONS_PRICE_IDX)
  # ----- Regressão Logística -----
  mod_market3_rl <- train(SUBSCRIBED ~ .,
                          data = market3_train, 
                          method = "glm", 
                          metric = "ROC", 
                          trControl = cv)
  mod_market3_rl
  pred_market3_rl <- predict(mod_market3_rl, newdata = market3_test, type = 'prob')
  
  plotROC(actuals = as.integer(market3_test$SUBSCRIBED)-1, predictedScores = pred_market3_rl$yes)
  AUC(pred_market3_rl$yes, as.integer(market3_test$SUBSCRIBED)-1)
  LogLoss(pred_market3_rl$yes, as.integer(market3_test$SUBSCRIBED)-1)

  (cutoff <- optimalCutoff(actuals = as.integer(market3_test$SUBSCRIBED)-1, predictedScores = pred_market3_rl$yes))
  pred_market3 <- ifelse(pred_market3_rl$yes > cutoff, 'yes', 'no')
  pred_market3 <- as.factor(pred_market3)
  (mc_market3_rl <- caret::confusionMatrix(data = pred_market3, positive = 'yes', 
                                           reference = market3_test$SUBSCRIBED,
                                           mode = 'everything'))
  
  cutoff <- 0.5
  pred_market3a <- ifelse(pred_market3_rl$yes > cutoff, 'yes', 'no')
  pred_market3a <- as.factor(pred_market3a)
  (mc_market3_rla <- caret::confusionMatrix(data = pred_market3a, positive = 'yes',
                                            reference = market3_test$SUBSCRIBED,
                                            mode = 'everything'))
  
  # Parando o processamento paralelo
  stopCluster(cl)
}

# Setando o melhor Dataset para Market
dt_market_train <- market2_train
dt_market_test <- market2_test
# A retirada das linhas com CONS_PRICE_IDX com NA's se mostrou mais eficaz que a retirada de 
# toda a variável. O AUC da regressão com o primeiro Dataset (Market2) foi de 93,68% e
# o LogLoss foi de 0.2126905, otimizando o Cutoff foi possível obter:
# uma sensibilidade de 55,06%, 
# especificidade de 95,63%, 
# precisão de 62,87% e 
# F1 Score de 58,71%. 
#
# Com o segundo Dataset (Market3) que foi criado retirando toda a variável CONS_PRICE_IDX, 
# o AUC foi de 92,82%, um pouco inferior, e um LogLoss de 0,217033, superior ao modelo anterior.
# Otimizando o Cutoff foi possível obter:
# uma sensibilidade de 40,88%, 
# uma especificidade de 97,39%, 
# precisão de 66,55% e
# F1 Score de 50,64%.
#
# Nota-se um AUC ligeiramente maior do primeiro modelo e um LogLoss menor, evidenciando a
# capacidade de distinguir clientes que irão converter dos que não irão maior. 
# Outro ponto mostrado foi a métrica F1 Score, que no primeiro modelo ficou sensivelmente
# melhor que no segundo modelo. Como essa métrica é uma junção da sensibilidade (Recall) e
# especificidade, ela mostra a capacidade de distinguir clientes que irão converter e os
# que não irão converter de um modelo. Essa evidência do F1 Score se soma ao do AUC e LogLos
# que apontam que o melhor Data Set para Market é o utilizado no primeiro modelo, que é o 
# Data Set (Market2).


# ------------------------------------------------------------------------------------------------
# 4) Testar modelos de classificação para as campanhas de marketing:
#      4.1) Regressão Logística, Árvores de Decisão, SVM , Redes Neurais e Algoritmo Genético 
#           para featureselection.


# Instalando o H2O ------------------------------------------------------------------------------------------
# The following two commands remove any previously installed H2O packages for R.
if (!require(h2o)) {
  if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
  if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
  
  # Next, download packages that H2O depends on.
  pkgs <- c("RCurl","jsonlite")
  for (pkg in pkgs) {
    if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
  }
  
  # Download and install the H2O package for R.
  install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")))
}
require(h2o)
h2o.init()
# -----------------------------------------------------------------------------------------------------------

# Definindo a variável target
y <- "SUBSCRIBED"
x <- setdiff(names(dt_market_train), y)
h2o_market_train <- as.h2o(dt_market_train)
h2o_market_test <- as.h2o(dt_market_test)


# ----- Regressão Logística ------------------------------------------------------------------------------
# Treinando o modelo
mod_market_rl <- h2o.glm(x = x, 
                         y = y,
                         training_frame = h2o_market_train,
                         nfolds = 10,
                         fold_assignment = 'Random',
                         family = "binomial",
                         max_iterations = 100,
                         seed = 314)
mod_market_rl
h2o.varimp(mod_market_rl)
h2o.varimp_plot(mod_market_rl)

# Avaliando a performance
# Usando H2O
pred_market_rl <- h2o.predict(mod_market_rl, newdata = h2o_market_test)
(perf_market_rl <- h2o.performance(mod_market_rl, h2o_market_test))
perf_market_rl@metrics$logloss
perf_market_rl@metrics$AUC
perf_market_rl@metrics$AIC
# Com H2O: 
# O modelo gerado obteve:
# AUC de 93,71%, 
# LogLoss de 0,2123981 e 
# Akaike (AIC) de 4873,877.
#
# As 10 mais importantes variáveis na ordem de contribuição para o modelo descrito abaixo:
# 1: EMP_VAR_RATE, 
# 2: MONTH.mar,
# 3: DURATION, 
# 4: POUTCOME.success, 
# 5: MONTH.may, 
# 6: CONS_PRICE_IDX, 
# 7: POUTCOME.failure,
# 8: MONTH.aug,
# 9: MONTH.jun,
#10: JOB.blue-collar.

# Otimizando a linha de corte e usando CARET
# Usando o pacote InformationValue
plotROC(actuals = as.integer((as.data.frame(h2o_market_test))$SUBSCRIBED)-1, 
        predictedScores = (as.data.frame(pred_market_rl))$yes)
(cutoff <- optimalCutoff(actuals = as.integer((as.data.frame(h2o_market_test))$SUBSCRIBED)-1, 
                         predictedScores = (as.data.frame(pred_market_rl))$yes))
# Usando o pacote MLmetrics
AUC((as.data.frame(pred_market_rl))$yes, as.integer((as.data.frame(h2o_market_test))$SUBSCRIBED)-1)
LogLoss((as.data.frame(pred_market_rl))$yes, as.integer((as.data.frame(h2o_market_test))$SUBSCRIBED)-1)

pred_market <- ifelse(pred_market_rl$yes > cutoff, 'yes', 'no')
pred_market <- as.factor(pred_market)
(mc_market_rl <- caret::confusionMatrix(data = (as.data.frame(pred_market))$C1, 
                                        positive = 'yes', 
                                        reference = (as.data.frame(h2o_market_test))$SUBSCRIBED,
                                        mode = 'everything'))
# Com CARET: 
# O modelo gerado obteve um AUC de 93,71%, semelhante ao obtido através do H2O que foi de 93,71%.
# LogLoss de 0,2123981, mesmo que no H2O. 
# Otimizando a linha de corte, foi obtido o valor de 35,00%.
# Acurácia de 90,85%.
#
# Como temos uma Dataset desbalanceado, foi focado nas métricas de Precisão, Sensibilidade e 
# Especificidade:
# Sensibilidade foi de 58,88%,  
# Especificidade foi de 95,15% e 
# Precisão foi de 61,99%.
#
# Como métrica balizadora de performance, foi escolhido o F1 Score pois este leva em conta tanto 
# a Sensibilidade quanto a Precisão no seu valor. 
# O valor do F1 Score para esse modelo foi de 60,39%.


# ----- Random Forest -------------------------------------------------------------------------------------
# Treinando o modelo
mod_market_rf <- h2o.randomForest(x = x, 
                                  y = y,
                                  training_frame = h2o_market_train,
                                  nfolds = 10,
                                  fold_assignment = 'Random',
                                  ntrees = 200,
                                  max_depth = 30,
                                  binomial_double_trees = TRUE,
                                  seed = 314)
mod_market_rf
h2o.varimp(mod_market_rf)
h2o.varimp_plot(mod_market_rf)

# Avaliando a performance
# Usando H2O
pred_market_rf <- h2o.predict(mod_market_rf, newdata = h2o_market_test)
(perf_market_rf <- h2o.performance(mod_market_rf, h2o_market_test))
perf_market_rf@metrics$logloss
perf_market_rf@metrics$AUC
# Com H2O: 
# O modelo gerado obteve:
# AUC de 94,18% e
# LogLoss de 0,1871988.
#
# As 10 mais importantes variáveis na ordem de contribuição para o modelo descrito abaixo:
# 1: DURATION,
# 2: AGE,
# 3: JOB,
# 4: PDAYS,
# 5: MONTH,
# 6: DAY_OF_WEEK,
# 7: EDUCATION,
# 8: EMP_VAR_RATE,
# 9: CONS_CONF_IDX,
#10: POUTCOME.

# Otimizando a linha de corte e usando CARET
# Usando o pacote InformationValue
plotROC(actuals = as.integer((as.data.frame(h2o_market_test))$SUBSCRIBED)-1, 
        predictedScores = (as.data.frame(pred_market_rf))$yes)
(cutoff <- optimalCutoff(actuals = as.integer((as.data.frame(h2o_market_test))$SUBSCRIBED)-1, 
                         predictedScores = (as.data.frame(pred_market_rf))$yes))
# Usando o pacote MLmetrics
AUC((as.data.frame(pred_market_rf))$yes, as.integer((as.data.frame(h2o_market_test))$SUBSCRIBED)-1)
LogLoss((as.data.frame(pred_market_rf))$yes, as.integer((as.data.frame(h2o_market_test))$SUBSCRIBED)-1)

pred_market <- ifelse(pred_market_rf$yes > cutoff, 'yes', 'no')
pred_market <- as.factor(pred_market)
(mc_market_rf <- caret::confusionMatrix(data = (as.data.frame(pred_market))$C1, 
                                        positive = 'yes',
                                        reference = (as.data.frame(h2o_market_test))$SUBSCRIBED,
                                        mode = 'everything'))
# Com CARET: 
# O modelo gerado obteve um AUC de 94,17%, semelhante ao obtido através do H2O que foi de 94,18%.
# LogLoss de 0,1871988, mesmo valor que o H2O.
# Otimizando a linha de corte, foi obtido o valor de 43,13%.
# Acurácia de 91,09%.
#
# Como temos uma Dataset desbalanceado, foi focado nas métricas de Precisão, Sensibilidade e 
# Especificidade:
# Sensibilidade foi de 61,12%, 
# Especificidade foi de 95,12% e 
# Precisão foi de 62,72%, 
#
# Como métrica balizadora de performance, foi escolhido o F1 Score pois este leva em conta tanto 
# a Sensibilidade quanto a Precisão no seu valor.
# O valor do F1 Score para esse modelo foi de 61,91%.


# ----- eXtreme Gradient Boosting ------------------------------------------------------------------------
# Treinando o modelo
mod_market_xgb <- h2o.xgboost(x = x, 
                              y = y,
                              training_frame = h2o_market_train,
                              nfolds = 10,
                              fold_assignment = 'Random',
                              ntrees = 60,
                              max_depth = 7,
                              booster = 'dart',
                              stopping_rounds = 2,
                              stopping_metric = 'logloss',
                              stopping_tolerance = 0.01,
                              seed = 314)
mod_market_xgb
head(h2o.varimp(mod_market_xgb),10)
h2o.varimp_plot(mod_market_xgb)

# Avaliando a performance
# Usando H2O
pred_market_xgb <- h2o.predict(mod_market_xgb, newdata = h2o_market_test)
(perf_market_xgb <- h2o.performance(mod_market_xgb, h2o_market_test))
perf_market_xgb@metrics$logloss
perf_market_xgb@metrics$AUC
# Com H2O: 
# O modelo gerado obteve:
# AUC de 94,50% e
# LogLoss de 0,1859924.
#
# As 10 mais importantes variáveis na ordem de contribuição para o modelo descrito abaixo:
# 1: DURATION,
# 2: EMP_VAR_RATE,
# 3: PDAYS,
# 4: CONS_CONF_IDX,
# 5: AGE,
# 6: MONTH.may,
# 7: CONS_PRICE_IDX,
# 8: CAMPAIGN,
# 9: POUTCOME.failure,
#10: CONTACT.celular.

# Otimizando a linha de corte e usando CARET
# Usando o pacote InformationValue
plotROC(actuals = as.integer((as.data.frame(h2o_market_test))$SUBSCRIBED)-1, 
        predictedScores = (as.data.frame(pred_market_xgb))$yes)
(cutoff <- optimalCutoff(actuals = as.integer((as.data.frame(h2o_market_test))$SUBSCRIBED)-1, 
                         predictedScores = (as.data.frame(pred_market_xgb))$yes))
# Usando o pacote MLmetrics
AUC((as.data.frame(pred_market_xgb))$yes, as.integer((as.data.frame(h2o_market_test))$SUBSCRIBED)-1)
LogLoss((as.data.frame(pred_market_xgb))$yes, as.integer((as.data.frame(h2o_market_test))$SUBSCRIBED)-1)

pred_market <- ifelse(pred_market_xgb$yes > cutoff, 'yes', 'no')
pred_market <- as.factor(pred_market)
(mc_market_xgb <- caret::confusionMatrix(data = (as.data.frame(pred_market))$C1, 
                                         positive = 'yes', 
                                         reference = (as.data.frame(h2o_market_test))$SUBSCRIBED,
                                         mode = 'everything'))
# Com CARET: 
# O modelo gerado obteve um AUC de 94,50%, semelhante ao obtido através do H2O que foi de 94,50%.
# LogLoss de 0,1859924, mesmo valor que no H2O.
# Otimizando a linha de corte, foi obtido o valor de 43,73%.
# Acurácia de 91,18%.
#
# Como temos uma Dataset desbalanceado, foi focado nas métricas de Precisão, Sensibilidade e 
# Especificidade:
# Sensibilidade foi de 61,80%,
# Especificidade foi de 95,13% e 
# Precisão foi de 63,03%.
#
# Como métrica balizadora de performance, foi escolhido o F1 Score pois este leva em conta tanto 
# a Sensibilidade quanto a Precisão no seu valor.
# O valor do F1 Score para esse modelo foi de 62,41%.


# ----- Support Vector Machine (LINEAR) --------------------------------------------------------------------
# Ajustando o processamento paralelo em CORES (NÚCLEOS)
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

# Treinando o modelo
set.seed(314)
arquivo <- './model/model-Market-SVM-Linear.rds'
if (file.exists(arquivo)) {
  mod_market_svmLin <- readRDS(file = arquivo)
} else {
  cv <- trainControl(method = "cv", number = 10, savePredictions = TRUE,
                     summaryFunction = twoClassSummary, classProbs = TRUE)
  
  system.time(mod_market_svmLin <- train(SUBSCRIBED ~ ., 
                                         data = dt_market_train,
                                         method = "svmLinear", 
                                         metric = 'ROC',
                                         trControl = cv, 
                                         preProcess = c("center", "scale")))
  saveRDS(mod_market_svmLin, file = arquivo)  
}
mod_market_svmLin
(varImp_svmLin <- varImp(mod_market_svmLin))
plot(varImp_svmLin)

# Avaliando a performance
# Otimizando a linha de corte e usando CARET
pred_market_svmLin <- predict(mod_market_svmLin, newdata = dt_market_test, type = 'prob')

# Parando o processamento paralelo
stopCluster(cl)

# Usando o pacote InformationValue
plotROC(actuals = as.integer(dt_market_test$SUBSCRIBED)-1, predictedScores = pred_market_svmLin$yes)
(cutoff <- optimalCutoff(actuals = as.integer(dt_market_test$SUBSCRIBED)-1, 
                         predictedScores = pred_market_svmLin$yes))

# Usando o pacote MLmetrics
AUC(pred_market_svmLin$yes, as.integer(dt_market_test$SUBSCRIBED)-1)
LogLoss(pred_market_svmLin$yes, as.integer(dt_market_test$SUBSCRIBED)-1)

pred_market <- ifelse(pred_market_svmLin$yes > cutoff, 'yes', 'no')
pred_market <- as.factor(pred_market)
(mc_market_svmLin <- caret::confusionMatrix(data = pred_market, 
                                            positive = 'yes', 
                                            reference = dt_market_test$SUBSCRIBED,
                                            mode = 'everything'))
# Com CARET: 
# As 10 mais importantes variáveis na ordem de contribuição para o modelo descrito abaixo:
# 1: DURATION,
# 2: EMP_VAR_RATE,
# 3: CONTACT,
# 4: CONS_CONF_IDX,
# 5: PREVIOUS,
# 6: PDAYS,
# 7: DEFAULT,
# 8: POUTCOME,
# 9: CAMPAIGN,
#10: MARITAL_STATUS.
#
# O modelo gerado obteve um AUC de 93,65%.
# LogLoss de 0,2250672.
# Otimizando a linha de corte, foi obtido o valor de 27,00%.
# Acurácia de 90,61%.
#
# Como temos uma Dataset desbalanceado, foi focado nas métricas de Precisão, Sensibilidade e 
# Especificidade:
# Sensibilidade foi de 57,15%,
# Especificidade foi de 95,11% e
# Precisão foi de 61,09%.
#
# Como métrica balizadora de performance, foi escolhido o F1 Score pois este leva em conta tanto 
# a Sensibilidade quanto a Precisão no seu valor.
# O valor do F1 Score para esse modelo foi de 59,06%.


# ----- Support Vector Machine (RADIAL) --------------------------------------------------------------------
# Ajustando o processamento paralelo em CORES (NÚCLEOS)
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

# Treinando o modelo
set.seed(314)
arquivo <- './model/model-Market-SVM-Radial.rds'
if (file.exists(arquivo)) {
  mod_market_svmRad <- readRDS(file = arquivo)
} else {
  cv <- trainControl(method = "cv", number = 10, savePredictions = TRUE,
                     summaryFunction = twoClassSummary, classProbs = TRUE)
  
  system.time(mod_market_svmRad <- train(SUBSCRIBED ~ ., 
                                         data = dt_market_train,
                                         method = "svmRadial", 
                                         metric = 'ROC',
                                         trControl = cv, 
                                         preProcess = c("center", "scale")))
  saveRDS(mod_market_svmRad, file = arquivo)
}
mod_market_svmRad
(varImp_svmRad <- varImp(mod_market_svmRad))
plot(varImp_svmRad)

# Avaliando a performance
# Otimizando a linha de corte e usando CARET
pred_market_svmRad <- predict(mod_market_svmRad, newdata = dt_market_test, type = 'prob')

# Parando o processamento paralelo
stopCluster(cl)

# Usando o pacote InformationValue
plotROC(actuals = as.integer(dt_market_test$SUBSCRIBED)-1, predictedScores = pred_market_svmRad$yes)
(cutoff <- optimalCutoff(actuals = as.integer(dt_market_test$SUBSCRIBED)-1, 
                         predictedScores = pred_market_svmRad$yes))

# Usando o pacote MLmetrics
AUC(pred_market_svmRad$yes, as.integer(dt_market_test$SUBSCRIBED)-1)
LogLoss(pred_market_svmRad$yes, as.integer(dt_market_test$SUBSCRIBED)-1)

pred_market <- ifelse(pred_market_svmRad$yes > cutoff, 'yes', 'no')
pred_market <- as.factor(pred_market)
(mc_market_svmRad <- caret::confusionMatrix(data = pred_market, 
                                            positive = 'yes', 
                                            reference = dt_market_test$SUBSCRIBED,
                                            mode = 'everything'))
# Com CARET: 
# As 10 mais importantes variáveis na ordem de contribuição para o modelo descrito abaixo:
# 1: DURATION,
# 2: EMP_VAR_RATE,
# 3: CONTACT,
# 4: CONS_CONF_IDX,
# 5: PREVIOUS,
# 6: PDAYS,
# 7: DEFAULT,
# 8: POUTCOME,
# 9: CAMPAIGN,
#10: MARITAL_STATUS.
#
# O modelo gerado obteve um AUC de 93,49%.
# LogLoss de 0,2236074.
# Otimizando a linha de corte, foi obtido o valor de 33,79%.
# Acurácia de 90,59%.
#
# Como temos uma Dataset desbalanceado, foi focado nas métricas de Precisão, Sensibilidade e 
# Especificidade:
# Sensibilidade foi de 51,09%,
# Especificidade foi de 95,89% e
# Precisão foi de 62,57%.
#
# Como métrica balizadora de performance, foi escolhido o F1 Score pois este leva em conta tanto 
# a Sensibilidade quanto a Precisão no seu valor.
# O valor do F1 Score para esse modelo foi de 56,25%.


# ----- Support Vector Machine (POLINOMIAL) --------------------------------------------------------------------
# Ajustando o processamento paralelo em CORES (NÚCLEOS)
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

# Treinando o modelo
set.seed(314)
arquivo <- './model/model-Market-SVM-Poly.rds'
if (file.exists(arquivo)) {
  mod_market_svmPol <- readRDS(file = arquivo)
} else {
  cv <- trainControl(method = "cv", number = 10, savePredictions = TRUE,
                     summaryFunction = twoClassSummary, classProbs = TRUE)
  
  system.time(mod_market_svmPol <- train(SUBSCRIBED ~ ., 
                                         data = dt_market_train,
                                         method = "svmPoly", 
                                         metric = 'ROC',
                                         trControl = cv, 
                                         preProcess = c("center", "scale")))
  saveRDS(mod_market_svmPol, file = arquivo)
}
mod_market_svmPol
(varImp_svmPol <- varImp(mod_market_svmPol))
plot(varImp_svmPol)

# Avaliando a performance
# Otimizando a linha de corte e usando CARET
pred_market_svmPol <- predict(mod_market_svmPol, newdata = dt_market_test, type = 'prob')

# Parando o processamento paralelo
stopCluster(cl)

# Usando o pacote InformationValue
plotROC(actuals = as.integer(dt_market_test$SUBSCRIBED)-1, predictedScores = pred_market_svmPol$yes)
(cutoff <- optimalCutoff(actuals = as.integer(dt_market_test$SUBSCRIBED)-1, 
                         predictedScores = pred_market_svmPol$yes))

# Usando o pacote MLmetrics
AUC(pred_market_svmPol$yes, as.integer(dt_market_test$SUBSCRIBED)-1)
LogLoss(pred_market_svmPol$yes, as.integer(dt_market_test$SUBSCRIBED)-1)

pred_market <- ifelse(pred_market_svmPol$yes > cutoff, 'yes', 'no')
pred_market <- as.factor(pred_market)
(mc_market_svmPol <- caret::confusionMatrix(data = pred_market, 
                                            positive = 'yes', 
                                            reference = dt_market_test$SUBSCRIBED,
                                            mode = 'everything'))
# Com CARET: 
# As 10 mais importantes variáveis na ordem de contribuição para o modelo descrito abaixo:
# 1: DURATION,
# 2: EMP_VAR_RATE,
# 3: CONTACT,
# 4: CONS_CONF_IDX,
# 5: PREVIOUS,
# 6: PDAYS,
# 7: DEFAULT,
# 8: POUTCOME,
# 9: CAMPAIGN,
#10: MARITAL_STATUS.
#
# O modelo gerado obteve um AUC de 93,74%.
# LogLoss de 0,2472548.
# Otimizando a linha de corte, foi obtido o valor de 20,98%.
# Acurácia de 90,06%.
#
# Como temos uma Dataset desbalanceado, foi focado nas métricas de Precisão, Sensibilidade e 
# Especificidade:
# Sensibilidade foi de 55,88%,
# Especificidade foi de 95,27% e
# Precisão foi de 61,35%.
#
# Como métrica balizadora de performance, foi escolhido o F1 Score pois este leva em conta tanto 
# a Sensibilidade quanto a Precisão no seu valor.
# O valor do F1 Score para esse modelo foi de 58,49%.


# ----- Redes Neurais -----------------------------------------------------------------------------------
# Treinando o modelo
mod_market_rn <- h2o.deeplearning(x = x, 
                                  y = y,
                                  training_frame = h2o_market_train,
                                  hidden = c(128,128,128),
                                  epochs = 10,
                                  activation = "Tanh",
                                  stopping_metric = 'logloss',
                                  stopping_tolerance = 1e-2,
                                  stopping_rounds = 2,
                                  adaptive_rate = FALSE,
                                  rate = 0.01,
                                  rate_annealing = 2e-6,
                                  momentum_start = 0.2,
                                  momentum_stable = 0.4,
                                  momentum_ramp = 1e7,
                                  l1 = 1e-5,
                                  l2 = 1e-5,
                                  max_w2 = 10,
                                  reproducible = TRUE,
                                  seed = 314)
mod_market_rn
head(h2o.varimp(mod_market_rn),10)
h2o.varimp_plot(mod_market_rn)

# Avaliando a performance
# Usando H2O
pred_market_rn <- h2o.predict(mod_market_rn, newdata = h2o_market_test)
(perf_market_rn <- h2o.performance(mod_market_rn, h2o_market_test))
perf_market_rn@metrics$logloss
perf_market_rn@metrics$AUC
# Com H2O: 
# O modelo gerado obteve:
# AUC de 94,59% e 
# LogLoss de 0,183795.
#
# As 10 mais importantes variáveis na ordem de contribuição para o modelo descrito abaixo:
# 1: DURATION,
# 2: EMP_VAR_RATE,
# 3: MONTH.oct,
# 4: MONTH.apr,
# 5: MONTH.may,
# 6: POUTCOME.success,
# 7: MONTH.mar,
# 8: DEFAULT.unknown,
# 9: MONTH.jun,
#10: POUTCOME.failure.

# Otimizando a linha de corte e usando CARET
# Usando o pacote InformationValue
plotROC(actuals = as.integer((as.data.frame(h2o_market_test))$SUBSCRIBED)-1, 
        predictedScores = (as.data.frame(pred_market_rn))$yes)
(cutoff <- optimalCutoff(actuals = as.integer((as.data.frame(h2o_market_test))$SUBSCRIBED)-1, 
                         predictedScores = (as.data.frame(pred_market_rn))$yes))
# Usando o pacote MLmetrics
AUC((as.data.frame(pred_market_rn))$yes, as.integer((as.data.frame(h2o_market_test))$SUBSCRIBED)-1)
LogLoss((as.data.frame(pred_market_rn))$yes, as.integer((as.data.frame(h2o_market_test))$SUBSCRIBED)-1)

pred_market <- ifelse(pred_market_rn$yes > cutoff, 'yes', 'no')
pred_market <- as.factor(pred_market)
(mc_market_rn <- caret::confusionMatrix(data = (as.data.frame(pred_market))$C1, 
                                        positive = 'yes', 
                                        reference = (as.data.frame(h2o_market_test))$SUBSCRIBED,
                                        mode = 'everything'))
# Com CARET: 
# O modelo gerado obteve um AUC de 94,59%, semelhante ao obtido através do H2O que foi de 94,59%.
# LogLoss de 0,183795.
# Otimizando a linha de corte, foi obtido o valor de 36,74%.
# Acurácia de 91,17%.
#
# Como temos uma Dataset desbalanceado, foi focado nas métricas de Precisão, Sensibilidade e 
# Especificidade:
# Sensibilidade foi de 66,29%,
# Especificidade foi de 94,51%,
# Precisão foi de 61,89%.
#
# Como métrica balizadora de performance, foi escolhido o F1 Score pois este leva em conta tanto 
# a Sensibilidade quanto a Precisão no seu valor.
# O valor do F1 Score para esse modelo foi de 64,01%.


# ----- Avaliação da Performance dos modelos de Classificação para Marketing ----------------------------
#
#                                LogLoss       AUC      Precisão    F1 Score
#                                -------      ------    --------    --------
# Regressão Logística.........: 0,2123981     93,71%    61,99%      60,39%  
# Random Forest...............: 0,1871988     94,17%    62,72%      61,39%
# eXtreme Gradient Boosting...: 0,1859924     94,50%    63,03%      62,41%
# SVM Linear..................: 0,2250672     93,65%    61,09%      59,06%
# SVM Radial..................: 0,2236074     93,49%    62,57%      56,25%
# SVM Polinomial..............: 0,2472548     93,74%    61,35%      58,49%
# Rede Neural.................: 0,1837795     94,59%    61,89%      64,01%
#
# Os melhores modelos eleitos foram o de Regressão Logística, Random Forest, eXtreme Gradient Boosting e 
# Rede Neural.
#
# Regressão Logística, apesar de ter um LogLoss de 0,2123981, que mostra um erro na capacidade
# de identificar um cliente que irá contratar o plano de um que não, semelhante aos demais modelo 
# com performance mediana, apresenta uma capacidade de acertar os clientes que convertem daqueles 
# que não convertem superior aos demais, utilizando como métrica a F1 Score. Portanto esse modelo
# também foi eleito.
#
# Random Forest foi eleito por possuir um LogLoss superior a maioria aos modelos medianos (com 
# LogLoss maior que 0,2), acrescido de um F1 Score maior que 60%.
#
# eXtreme Gradient Boosting foi eleito por possuir um LogLoss que é o segundo melhor, perdendo
# somente para o modelo de Rede Neural e também por possuir um F1 Score maior que 60%.
#
# Rede Neural foi eleito porque possuir o melhor LogLoss e F1 Score dentro os modelos testados.


# ------------------------------------------------------------------------------------------------------
# Dentro os modelos eleitos, foi escolhido o de Regressão Logística e de Random Forest para aplicar
# uma feature selection e verificar o comportamento do modelo. Na Regressão Logística foi aplicado
# Step Wise e Genetic Algorithms, no Random Forest foi aplicado Genetic Algorithms e Recursive 
# Feature Elimination.

# ----- Regressão Logística com Stepwise usando AIC --------------------------------------------------------
# Ajustando o processamento paralelo em CORES (NÚCLEOS)
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

# Treinando o modelo
set.seed(314)
arquivo <- './model/model-Market-RL-StepAIC.rds'
if (file.exists(arquivo)) {
  mod_market_rlSW <- readRDS(file = arquivo)
} else {
  cv <- trainControl(method = "cv", number = 10, savePredictions = TRUE,
                     summaryFunction = twoClassSummary, classProbs = TRUE)
  
  system.time(mod_market_rlSW <- train(SUBSCRIBED ~ ., 
                                       data = dt_market_train,
                                       method = "glmStepAIC", 
                                       metric = 'ROC',
                                       trControl = cv))
  saveRDS(mod_market_rlSW, file = arquivo)
}
mod_market_rlSW
summary(mod_market_rlSW)
(varImp_rlSW <- varImp(mod_market_rlSW))
plot(varImp_rlSW)

# Avaliando a performance
# Otimizando a linha de corte e usando CARET
pred_market_rlSW <- predict(mod_market_rlSW, newdata = dt_market_test, type = 'prob')

# Parando o processamento paralelo
stopCluster(cl)

# Usando o pacote InformationValue
plotROC(actuals = as.integer(dt_market_test$SUBSCRIBED)-1, predictedScores = pred_market_rlSW$yes)
(cutoff <- optimalCutoff(actuals = as.integer(dt_market_test$SUBSCRIBED)-1, 
                         predictedScores = pred_market_rlSW$yes))

# Usando o pacote MLmetrics
AUC(pred_market_rlSW$yes, as.integer(dt_market_test$SUBSCRIBED)-1)
LogLoss(pred_market_rlSW$yes, as.integer(dt_market_test$SUBSCRIBED)-1)

pred_market <- ifelse(pred_market_rlSW$yes > cutoff, 'yes', 'no')
pred_market <- as.factor(pred_market)
(mc_market_rlSW <- caret::confusionMatrix(data = pred_market, 
                                          positive = 'yes', 
                                          reference = dt_market_test$SUBSCRIBED,
                                          mode = 'everything'))
# Com CARET: 
# As 10 mais importantes variáveis na ordem de contribuição para o modelo descrito abaixo:
# 1: DURATION,
# 2: EMP_VAR_RATE,
# 3: CONTACT,
# 4: CONS_CONF_IDX,
# 5: PREVIOUS,
# 6: PDAYS,
# 7: DEFAULT,
# 8: POUTCOME,
# 9: CAMPAIGN,
#10: MARITAL_STATUS.
#
# O modelo gerado obteve um AUC de 93,69%.
# LogLoss de 0,2127598.
# Otimizando a linha de corte, foi obtido o valor de 37,00%.
# Acurácia de 90,86%.
#
# Como temos uma Dataset desbalanceado, foi focado nas métricas de Precisão, Sensibilidade e 
# Especificidade:
# Sensibilidade foi de 56,33%,
# Especificidade foi de 95,50% e
# Precisão foi de 62,72%.
#
# Como métrica balizadora de performance, foi escolhido o F1 Score pois este leva em conta tanto 
# a Sensibilidade quanto a Precisão no seu valor.
# O valor do F1 Score para esse modelo foi de 59,35%.


# ----- Regressão Logística com Algoritmo Genético --------------------------------------------------------
# Ajustando o processamento paralelo em CORES (NÚCLEOS)
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

# Treinando o modelo
set.seed(314)
arquivo <- './model/model-Market-RL-GA.rds'
if (file.exists(arquivo)) {
  obj_rl_ga <- readRDS(file = arquivo)
} else {
  ga_ctrl <- gafsControl(functions = caretGA,
                         genParallel = TRUE,
                         allowParallel = TRUE,
                         number = 5,
                         method = "cv")

  obj_rl_ga <- gafs(x = dt_market_train[,-c('SUBSCRIBED','DEFAULT')],
                    y = dt_market_train$SUBSCRIBED,
                    iters = 3,
                    popSize = 5,
                    gafsControl = ga_ctrl,
                    method = "glm")

  saveRDS(obj_rl_ga, file = arquivo)
}
mod_market_rlGA <- obj_rl_ga$fit
summary(mod_market_rlGA)
(varImp_rlGA <- varImp(mod_market_rlGA))
plot(varImp_rlGA)

# Avaliando a performance
# Otimizando a linha de corte e usando CARET
pred_market_rlGA <- predict(mod_market_rlGA, newdata = dt_market_test, type = 'prob')

# Parando o processamento paralelo
stopCluster(cl)

# Usando o pacote InformationValue
plotROC(actuals = as.integer(dt_market_test$SUBSCRIBED)-1, predictedScores = pred_market_rlGA$yes)
(cutoff <- optimalCutoff(actuals = as.integer(dt_market_test$SUBSCRIBED)-1, 
                         predictedScores = pred_market_rlGA$yes))

# Usando o pacote MLmetrics
AUC(pred_market_rlGA$yes, as.integer(dt_market_test$SUBSCRIBED)-1)
LogLoss(pred_market_rlGA$yes, as.integer(dt_market_test$SUBSCRIBED)-1)

pred_market <- ifelse(pred_market_rlGA$yes > cutoff, 'yes', 'no')
pred_market <- as.factor(pred_market)
(mc_market_rlGA <- caret::confusionMatrix(data = pred_market, 
                                          positive = 'yes', 
                                          reference = dt_market_test$SUBSCRIBED,
                                          mode = 'everything'))
# Com CARET: 
# As 10 mais importantes variáveis na ordem de contribuição para o modelo descrito abaixo:
# 1: DURATION,
# 2: EMP_VAR_RATE,
# 3: CONS_PRICE_IDX,
# 4: POUTCOME.success,
# 5: MONTH.mar,
# 6: CONTACT.telephone,
# 7: MONTH.aug,
# 8: MONTH.may,
# 9: POUTCOME.nonexistent,
#10: CAMPAIGN.
#
# O modelo gerado obteve um AUC de 93,66%.
# LogLoss de 0,2130927.
# Otimizando a linha de corte, foi obtido o valor de 36,00%.
# Acurácia de 90,81%.
#
# Como temos uma Dataset desbalanceado, foi focado nas métricas de Precisão, Sensibilidade e 
# Especificidade:
# Sensibilidade foi de 57,23%,
# Especificidade foi de 95,32% e
# Precisão foi de 62,16%.
#
# Como métrica balizadora de performance, foi escolhido o F1 Score pois este leva em conta tanto 
# a Sensibilidade quanto a Precisão no seu valor.
# O valor do F1 Score para esse modelo foi de 59,59%.


# ----- Random Forest com Algoritmo Genético ------------------------------------------------------------------
# Ajustando o processamento paralelo em CORES (NÚCLEOS)
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

# Treinando o modelo
set.seed(314)
arquivo <- './model/model-Market-RF-GA.rds'
if (file.exists(arquivo)) {
  obj_rf_ga <- readRDS(file = arquivo)
} else {
  ga_ctrl <- gafsControl(functions = rfGA,
                         genParallel = TRUE,
                         allowParallel = TRUE,
                         number = 5,
                         method = "cv")
  
  obj_rf_ga <- gafs(x = dt_market_train[,-c('SUBSCRIBED')],
                    y = dt_market_train$SUBSCRIBED,
                    iters = 3,
                    popSize = 10,
                    gafsControl = ga_ctrl)
  
  saveRDS(obj_rf_ga, file = arquivo)
}
mod_market_rfGA <- obj_rf_ga$fit
summary(mod_market_rfGA)
(varImp_rfGA <- varImp(mod_market_rfGA))
plot(varImp_rfGA)

# Avaliando a performance
# Otimizando a linha de corte e usando CARET
pred_market_rfGA <- predict(mod_market_rfGA, newdata = dt_market_test, type = 'prob')

# Parando o processamento paralelo
stopCluster(cl)

# Usando o pacote InformationValue
plotROC(actuals = as.integer(dt_market_test$SUBSCRIBED)-1, 
        predictedScores = (as.data.frame(pred_market_rfGA))$yes)
(cutoff <- optimalCutoff(actuals = as.integer(dt_market_test$SUBSCRIBED)-1, 
                         predictedScores = (as.data.frame(pred_market_rfGA))$yes))

# Usando o pacote MLmetrics
AUC((as.data.frame(pred_market_rfGA))$yes, as.integer(dt_market_test$SUBSCRIBED)-1)
LogLoss((as.data.frame(pred_market_rfGA))$yes, as.integer(dt_market_test$SUBSCRIBED)-1)

pred_market <- ifelse((as.data.frame(pred_market_rfGA))$yes > cutoff, 'yes', 'no')
pred_market <- as.factor(pred_market)
(mc_market_rfGA <- caret::confusionMatrix(data = pred_market, 
                                          positive = 'yes', 
                                          reference = dt_market_test$SUBSCRIBED,
                                          mode = 'everything'))
# Com CARET: 
# As 10 mais importantes variáveis na ordem de contribuição para o modelo descrito abaixo:
# 1: AGE,
# 2: JOB,
# 3: MARITAL_STATUS,
# 4: EDUCATION,
# 5: DEFAULT,
# 6: HOUSING,
# 7: CONTACT,
# 8: MONTH,
# 9: DAY_OF_WEEK,
#10: DURATION.
#
# O modelo gerado obteve um AUC de 94,29%.
# LogLoss de 0,192436.
# Otimizando a linha de corte, foi obtido o valor de 44,20%.
# Acurácia de 91,11%.
#
# Como temos uma Dataset desbalanceado, foi focado nas métricas de Precisão, Sensibilidade e 
# Especificidade:
# Sensibilidade foi de 60,00%,
# Especificidade foi de 05,29% e
# Precisão foi de 63,12%.
#
# Como métrica balizadora de performance, foi escolhido o F1 Score pois este leva em conta tanto 
# a Sensibilidade quanto a Precisão no seu valor.
# O valor do F1 Score para esse modelo foi de 61,52%.


# ----- Random Forest com Recursive Feature Elimination ----------------------------------------------------
# Ajustando o processamento paralelo em CORES (NÚCLEOS)
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

# Treinando o modelo
set.seed(314)
arquivo <- './model/model-Market-RF-RFE.rds'
if (file.exists(arquivo)) {
  obj_rf_rfe <- readRDS(file = arquivo)
} else {
  subsets <- c(1:5,10,15,ncol(dt_market_train)-1)
  fStats <- function(...) c(twoClassSummary(...),
                            defaultSummary(...))
  rfFuncs_new <- rfFuncs
  rfFuncs_new$summary <- fStats
  rfe_ctrl <- rfeControl(functions = rfFuncs_new,
                         allowParallel = TRUE,
                         number = 10,
                         method = "cv")
  
  obj_rf_rfe <- rfe(x = dt_market_train[,-c('SUBSCRIBED')],
                    y = dt_market_train$SUBSCRIBED,
                    sizes = subsets,
                    metric = 'ROC',
                    rfeControl = rfe_ctrl,
                    ntree = 200)
  
  saveRDS(obj_rf_rfe, file = arquivo)
}
mod_market_rfRFE <- obj_rf_rfe$fit
summary(mod_market_rfRFE)
(varImp_rfRFE <- varImp(mod_market_rfRFE))
plot(varImp_rfRFE)

# Avaliando a performance
# Otimizando a linha de corte e usando CARET
pred_market_rfRFE <- predict(mod_market_rfRFE, newdata = dt_market_test, type = 'prob')
str(pred_market_rfRFE)

# Parando o processamento paralelo
stopCluster(cl)

# Usando o pacote InformationValue
plotROC(actuals = as.integer(dt_market_test$SUBSCRIBED)-1, 
        predictedScores = (as.data.frame(pred_market_rfRFE))$yes)
(cutoff <- optimalCutoff(actuals = as.integer(dt_market_test$SUBSCRIBED)-1, 
                         predictedScores = (as.data.frame(pred_market_rfRFE))$yes))

# Usando o pacote MLmetrics
AUC((as.data.frame(pred_market_rfRFE))$yes, as.integer(dt_market_test$SUBSCRIBED)-1)
LogLoss((as.data.frame(pred_market_rfRFE))$yes, as.integer(dt_market_test$SUBSCRIBED)-1)

pred_market <- ifelse((as.data.frame(pred_market_rfRFE))$yes > cutoff, 'yes', 'no')
pred_market <- as.factor(pred_market)
(mc_market_rfRFE <- caret::confusionMatrix(data = pred_market, 
                                          positive = 'yes', 
                                          reference = dt_market_test$SUBSCRIBED,
                                          mode = 'everything'))
# Com CARET: 
# As 10 mais importantes variáveis na ordem de contribuição para o modelo descrito abaixo:
# 1: DURATION,
# 2: EMP_VAR_RATE,
# 3: PDAYS,
# 4: MONTH,
# 5: POUTCOME,
# 6: CONS_CONF_IDX,
# 7: JOB,
# 8: CONTACT,
# 9: AGE,
#10: EDUCATION.
#
# O modelo gerado obteve um AUC de 94,43%.
# LogLoss de 0,1976715.
# Otimizando a linha de corte, foi obtido o valor de 44,00%.
# Acurácia de 91,17%.
#
# Como temos uma Dataset desbalanceado, foi focado nas métricas de Precisão, Sensibilidade e 
# Especificidade:
# Sensibilidade foi de 61,72%,
# Especificidade foi de 95,13% e
# Precisão foi de 63,00%.
#
# Como métrica balizadora de performance, foi escolhido o F1 Score pois este leva em conta tanto 
# a Sensibilidade quanto a Precisão no seu valor.
# O valor do F1 Score para esse modelo foi de 62,35%.


# ------------------------------------------------------------------------------------------------
# 5) Testar modelos de regressão para o valor de vendas das lojas:
#      5.1) Regressão Linear, Árvore de Decisão, e Redes Neurais.

if (!require(car)) install.packages('car')
require(car)

if (!require(nortest)) install.packages('nortest')
require(nortest)

if (!require(lmtest)) install.packages('lmtest')
require(lmtest)


# ----- Regressão Linear ---------------------------------------------------------------------------------
# Ajustando o processamento paralelo em CORES (NÚCLEOS)
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

# Treinando o modelo
set.seed(314)

mod_retail_rl <- lm(Weekly_Sales ~ ., 
                    data = dt_retail_train[,-c('Store')])
summary(mod_retail_rl)
# A princípio, foi obtido um modelo sem o ID da Loja. O resultado obtido foi um R2 muito baixo
# e um RSE demasiadamente alto.
# Optou-se em colocar a variável Store no treinamento do modelo, com o argumento que seria 
# uma variável da loja que não seria alterada e identificaria, univocamente, a loja e, 
# consequentemente, a região em que ela estaria operando. Isso traria para o modelo 
# relações geográficas que não estão disponíveis no Data Set.

mod_retail_rl <- lm(Weekly_Sales ~ ., 
                    data = dt_retail_train)
summary(mod_retail_rl)

# Retirando os Outliers para tentar melhorar o modelo
outliers <- cooks.distance(mod_retail_rl) > 4/nrow(dt_retail_train)
mod_retail_rl <- lm(Weekly_Sales ~ .,
                    data = dt_retail_train[!which(outliers),])
summary(mod_retail_rl)
# Houve uma melhora significativa do modelo, o R2 passou para o,435 e o RSE 337.300.

mod_retail_rl <- lm(Weekly_Sales ~ ., 
                    data = dt_retail_train[!which(outliers),-c('MarkDown4')])
summary(mod_retail_rl)

mod_retail_rl <- lm(Weekly_Sales ~ ., 
                    data = dt_retail_train[!which(outliers),-c('MarkDown4','Fuel_Price')])
summary(mod_retail_rl)
dt <- dt_retail_train[!which(outliers),-c('MarkDown4','Fuel_Price')]

# Como resultado final, foi obtido um modelo com todas as variáveis presentes significantes, 
# pois p-valor dos coeficientes delas passaram no teste t. Assim como o todo o modelo de 
# acordo com o teste F que obteve p-valor igual a 2.2e-16, apesar do R2 baixo de 0,435 
# e um RSE alto de 337.200.

# Verificando a contribuição das variáveis no modelo
(varImp_rl <- caret::varImp(mod_retail_rl, useModel = TRUE, scale = TRUE))
Imp_rl <- data.table()
Imp_rl[,':='(Variable = rownames(varImp_rl),
             Importance = varImp_rl$Overall)]
setorder(Imp_rl, Importance)
Imp_rl$Variable <- factor(Imp_rl$Variable, levels = Imp_rl$Variable)
Imp_rl
as.data.frame(Imp_rl) %>% ggplot(aes(y = Importance, x = Variable)) +
  geom_col(aes(fill = as.factor(Variable)), width = 0.2, show.legend = FALSE) +
  scale_fill_brewer(direction = 1, palette = 8,  type = 'div') +
  scale_y_continuous(limits = c(0,23), breaks = seq(1,23,2)) +
  labs(y = 'Grau de Importância', x = "Variável", title = "Escala de Importância das Variáveis") +
  coord_flip()
# As mais importantes variáveis na ordem de contribuição para o modelo:
# 1: Store,
# 2: MarkDown1,
# 3: MarkDown5,
# 4: CPI,
# 5: Unemployment,
# 6: MarkDown2,
# 7: MarkDown3,
# 8: IsHoliday.TRUE,
# 9: Date.
#10: Temperature,

# ----------------------------------------------
# Verificando a existência de Multicolinearidade 
# Avaliando graficamente se existe colinearidade entre as variáveis dependentes
corrplot(cor(dt[,-c('Date','IsHoliday')]),
         method = 'square',
         type = 'upper',
         diag = FALSE,
         title = 'Correlação',
         mar = c(1,1,1,1),
         addCoefasPercent = TRUE,
         addCoef.col = 'gray50',
         number.digits = 0)
# Não há índicios de alta correlação entre as variáveis dependentes (>70%) 

# Avaliando através de teste estatístico VIF (Variance Inflation Factor),
# em português, FIV (Fator Inflação da Variância).
vif(mod_retail_rl)
# Segundo uma regra prática, se o valor do VIF for maior que 10, existe forte
# colinearidade entre as variáveis.

# O teste VIF não apontou a existência de colinearidade. Isso ratifica o que foi mostrado pela
# matriz de correlação

# -----------------------------------------------------
# Verificando o pressuposto de normalidade dos resíduos
residuo <- rstandard(mod_retail_rl)

# Avaliando graficamente se os resíduos são distribuídos normalmente
ggplot() +
  geom_qq(aes(sample = residuo), color = 'blue') +
  geom_qq_line(aes(sample = residuo), color = 'red') +
  labs(x = "Quantidades Teóricas", y = "Resíduos Padronizados")
# Com exceção dos primeiros e últimos pontos que representam um descolamento da reta,
# o gráfico sugere que os resíduos são distribuídos normalmente.

# Avaliando a normalidade dos resíduos através do teste formal de Anderson-Darling
ad.test(residuo)
# Como p-valor é menor que 5%, rejeita-se a hipótese nula.
# Portanto, os resíduos não possuem distribuição normal, invalidando um dos pressupostos
# da Regressão Linear.

# Foi aplicado uma transformação nas variáveis independentes para buscar uma normalidade nos resíduos.
# No caso utilizou-se a função log.
mod_retail_rl2 <- lm(Weekly_Sales ~ log(Store) + log(MarkDown1) + log(MarkDown5) + log(CPI) +
                       log(Temperature) + log(Unemployment) + 1,
                     data = dt)
summary(mod_retail_rl2)
# Foi produzido um modelo com R2 mais baixo e com RSE mais alto.

residuo2 <- rstandard(mod_retail_rl2)
ggplot() +
  geom_qq(aes(sample = residuo2), color = 'blue') +
  geom_qq_line(aes(sample = residuo2), color = 'red') +
  labs(x = "Quantidades Teóricas", y = "Resíduos Padronizados")
ad.test(residuo2)
# Avaliando graficamente, os resíduos parecem estar distribuídos normalmente com exceção das extremidades,
# mas formalmente o teste ainda acusa falta de normalidade nos mesmos.

# Apesar do pressuposto de normalidade dos resíduos comprometer a qualidade do modelo
# obtido, processguiremos com sua avaliação nos demais pressupostos.

# --------------------------------------------------
# Verificando a existência de Auto Correlação Serial 
plot(residuo2)

# Avaliando através do teste formal de Durbin-Watson
dwtest(mod_retail_rl, alternative = 'two.sided')
dwtest(mod_retail_rl2, alternative = 'two.sided')
# Dado que p-valor é menor que 5%, rejeita-se a hipótese nula de Ausência de Correlação Serial nos dois
# modelos.
# Portanto, há indícios de Correlação Serial no modelo, quebrando mais um pressuposto.


# --------------------------------------------------------
# Avaliando graficamente a existência de Homocedasticidade
g1 <- ggplot() +
  geom_point(aes(x = fitted.values(mod_retail_rl), y = residuo), color = 'cyan4') +
  geom_hline(yintercept = 0, color = 'salmon3') +
  labs(x = "Satisfação Ajustado", y = 'Resíduos Padronizados', title = "Modelo sem transformação")
g2 <- ggplot() +
  geom_point(aes(x = fitted.values(mod_retail_rl2), y = residuo), color = 'cyan4') +
  geom_hline(yintercept = 0, color = 'salmon3') +
  labs(x = "Satisfação Ajustado", y = 'Resíduos Padronizados', title = "Modelo com transformação de LOG")
grid.arrange(g1, g2,
             ncol = 2, nrow = 1)
# Os pontos parecem apresentar um padrão no dois modelos, como a forma de um losango por exemplo, que caracterizaria 
# uma possível heterocedasticidade.

# Avaliando a Homocedasticidade através do teste Breusch-Pagan
bptest(mod_retail_rl)
bptest(mod_retail_rl2)
# Como o p-valor é menor que 5% nos dois modelos, rejeita-se a hipótese nula de que a variância dos resíduos 
# e, consequentemente, da variável depedente é constante. Assim existe heterocedasticidade 
# no modelo, portanto ele não atende a esse pressuposto da Regressão Linear.

# Como os testes de vários pressupostos falharam para o modelo obtido, ele não será utilizado
# para previsão do Faturamento das Lojas. Acrescenta-se a isso, ao baixo nível do R2 obtido,
# menor que 60%.
#
# Seguiremos com a avaliação de performance do modelo para fins didáticos. Optou-se por utilizar o 
# modelo sem transformação logarítmica, pois possui um R2 melhor

# Avaliando a performance
pred_retail_rl <- predict(mod_retail_rl, newdata = dt_retail_test)
R2_Score(y_pred = pred_retail_rl, y_true = dt_retail_test$Weekly_Sales)
RMSE(y_pred = pred_retail_rl, y_true = dt_retail_test$Weekly_Sales)

# Parando o processamento paralelo
stopCluster(cl)

# O modelo gerado obteve um R2 de 0,1728.
# RMSE de 506.072,8.



# ----- Random Forest  ----------------------------------------------------------------------
# Ajustando o processamento paralelo em CORES (NÚCLEOS)
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

# Treinando o modelo
set.seed(314)
arquivo <- './model/model-Retail-RF.rds'
if (file.exists(arquivo)) {
  mod_retail_rf <- readRDS(file = arquivo)
} else {
  cv <- trainControl(method = "cv", number = 10, savePredictions = TRUE,
                     summaryFunction = defaultSummary)
  
  mod_retail_rf <- train(Weekly_Sales ~ ., 
                         data = dt_retail_train,
                         method = "rf", 
                         metric = 'RMSE',
                         trControl = cv,
                         tuneLength = 5)
  saveRDS(mod_retail_rf, file = arquivo)
}
summary(mod_retail_rf)
(varImp_rf <- varImp(mod_retail_rf))
plot(varImp_rf)

# Avaliando a performance
# Otimizando a linha de corte e usando CARET
pred_retail_rf <- predict(mod_retail_rf, newdata = dt_retail_test)
R2_Score(y_pred = pred_retail_rf, y_true = dt_retail_test$Weekly_Sales)
RMSE(y_pred = pred_retail_rf, y_true = dt_retail_test$Weekly_Sales)

# Parando o processamento paralelo
stopCluster(cl)

# Com CARET: 
# As mais importantes variáveis na ordem de contribuição para o modelo:
# 1: Store,
# 2: CPI,
# 3: Unemployment,
# 4: Temperature,
# 5: MarkDown5,
# 6: MarkDown1,
# 7: MarkDown3,
# 8: Date,
# 9: Temperature,
#10: MarkDown4.
#
# O modelo gerado obteve um R2 de 0,9417.
# RMSE de 135.551,6.


# ----- Redes Neurais -----------------------------------------------------------------------------------
# Definindo a variável target
y <- "Weekly_Sales"
x <- setdiff(names(dt_retail_train), y)
h2o_retail_train <- as.h2o(dt_retail_train)
h2o_retail_test <- as.h2o(dt_retail_test)

# Treinando o modelo
mod_retail_rn <- h2o.deeplearning(x = x, 
                                  y = y,
                                  training_frame = h2o_retail_train,
                                  hidden = c(256,256,256,256,256),
                                  epochs = 30,
                                  activation = 'Rectifier',
                                  stopping_metric = 'RMSE',
                                  stopping_tolerance = 1e-5,
                                  stopping_rounds = 5,
                                  l1 = 1e-5,
                                  l2 = 1e-5,
                                  max_w2 = 10,
                                  reproducible = TRUE,
                                  seed = 314)
mod_retail_rn
head(h2o.varimp(mod_retail_rn),10)
h2o.varimp_plot(mod_retail_rn)

# Avaliando a performance
# Usando H2O
pred_retail_rn <- h2o.predict(mod_retail_rn, newdata = h2o_retail_test)
(perf_retail_rn <- h2o.performance(mod_retail_rn, h2o_retail_test))
perf_retail_rn@metrics$r2
# Com H2O: 
# O modelo gerado obteve:
# R2 de 83,97% e 
# RMSE de 222.770,7,
#
# As 10 mais importantes variáveis na ordem de contribuição para o modelo descrito abaixo:
# 1: Store,
# 2: Unemployment,
# 3: CPI,
# 4: MarkDown5,
# 5: Date,
# 6: MarkDown1,
# 7: IsHoliday.FALSE,
# 8: Temperature,
# 9: IsHoliday.TRUE,
#10: Feul_Price.



# Desconectando e desligando o pacote H2O.
h2o.shutdown(prompt = FALSE)

# ------------------------------------------------------------------------------------------------
# 6) Validar  a performance dos modelos (R2 & Matriz de Confusão).

# ------------------------------------------------------------------------------------------------
# 7) Fazer o “scoring” dos modelos para os dados nas respectivas bases de teste.
#

