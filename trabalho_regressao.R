#aluno: PRISCILA CASTILHO PALAZZO
#RA: 619202237
#e-mail: pripalazzo@gmail.com

getwd()

#carregando a biblioteca ggplot2
library(ggplot2)

  #Importando os dados de housing.csv
housing <- read.csv("housing.csv")

#tipo de arquivo: data frame
class(housing)

#conhecendo os dados
head(housing)
dim(housing)
summary(housing)
str(housing)


#carregando as bibliotecas: tidyverse / dplyr / MASS
library(tidyverse)

library(dplyr)

library(MASS)


#****************************EXERCÍCIOS*******************************************

# 1a) 
# Os novos nomes deverão ser: Neighborhood, Class, Units, YearBuilt, SqFt, Income, 
# IncomePerSqFt, Expense, ExpensePerSqFt, NetIncome, Value, ValuePerSqFt e Boro.
# Salve os nomes novos em um vetor de caracteres nomes_novos. Salve este vetor em 
# colnames(housing). renomeando os nomes das colunas

#Vetor de caracteres
nomes_novos <- c("Neighborhood", "Class", "Units", "YearBuilt",
                 "SqFt", "Income", "IncomePerSqFt", "Expense",
                 "ExpensePerSqFt", "NetIncome", "Value",
                 "ValuePerSqFt","Boro")

colnames(housing) <- nomes_novos

# 1b-1) 
# Faça um histograma colocando no eixo x ValuePerSqFt. Este histograma possui a 
# característica de bimodalidade. Pesquise sobre o que pode significar um histograma 
# bimodal e o que isso pode implicar para o Cientista de dados a respeito do conjunto 
# de dados.

#Histograma com eixo x ValuePerSqFt

ggplot(data = housing)+
  geom_histogram(mapping = aes(x = ValuePerSqFt),
                 colour = "black", fill = "blue", bins = 20)+
  ggtitle("Distribuição Value Per Square Foot")+
  theme(plot.title = element_text(hjust = 0.5))

### Análise: Percebe-se no histograma bimodal plotado com 2 picos (2 modas) e assimétrico. 
# Pode ser conveniente dividir a amostra em porções para uma melhor análise. Como recomendado 
# pelo excercício, dividiu-se a amostra por distritos, o que melhorou a análise dos dados,
# uma vez que cada região possui valores por pé quadrado.
# Histogramas assimétricos não possuem distribuição normal, o que faz que técnicas de ajustes 
# sejam empregadas na busca de um melhor grau de acurácia para as predições. 

# 1b-2) 
# Plote um histograma para cada um dos bairros: Manhatan, Brooklyn, Queens, Bronx e Staten
# Island. Produza uma breve análise sobre o que é possível concluir a respeito desses 
# histogramas. Os histogramas apresentam forte assimetria? Em que isto pode afetar a 
# regressão?
## Histogramas referem-se a valor por pé quadrado X quantidade de imóveis

# Manhatan
ggplot(data = housing%>%filter(Boro=="Manhattan"))+
  geom_histogram(mapping = aes(x = ValuePerSqFt),
                 colour = "black", fill = "yellow", bins = 15)+
  ggtitle("Manhattan")+
  theme(plot.title = element_text(hjust = 0.5))

## Observação Manhatan: boa distribuição de dados (forte simetria). É um histograma que apresenta 2 
## modas entre os valores de 175 e 225, com frequência superior nestes 2 picos a quantidade 
## de 275. Comparado as demais localizações, é um local bem populoso e o local mais bem avaliado.

# Brooklyn
ggplot(data = housing%>%filter(Boro=="Brooklyn"))+
  geom_histogram(mapping = aes(x = ValuePerSqFt),
                 colour = "black", fill = "gray", bins = 15)+
  ggtitle("Brooklyn")+
  theme(plot.title = element_text(hjust = 0.5))

## Observação Brooklin: forte assimetria na distribuição de dados. É um histograma que apresenta
## outliers acima dos valores de 175, o que pode interferir caso utilize-se a média em alguma 
## análise. Comparado as demais localizações, é segundo local mais bem avaliado e também o segundo mais populoso.

# Queens
ggplot(data = housing%>%filter(Boro=="Queens"))+
  geom_histogram(mapping = aes(x = ValuePerSqFt),
                 colour = "black", fill = "blue", bins = 15)+
  ggtitle("Queens")+
  theme(plot.title = element_text(hjust = 0.5))

## Observação Queens: forte assimetria na distribuição de dados, apresentando 2 picos. 
# Apresenta outlier no valor acima de 300.

# Bronx
ggplot(data = housing%>%filter(Boro=="Bronx"))+
  geom_histogram(mapping = aes(x = ValuePerSqFt),
                 colour = "black", fill = "red", bins = 10)+
  ggtitle("Bronx")+
  theme(plot.title = element_text(hjust = 0.5))

## Observação Bronx: forte assimetria na distribuição de dados, apresentando 2 picos (2 modas). 
# Os valores dos imóveis são baixo comparados as outras localizações.

# Staten Island
ggplot(data = housing%>%filter(Boro=="Staten Island"))+
  geom_histogram(mapping = aes(x = ValuePerSqFt),
                 colour = "black", fill = "pink")+
  ggtitle("Staten Island")+
  theme(plot.title = element_text(hjust = 1))

## observação Staten Island: forte assimetria nos dados. As 2 maiores frequências estão para
#os valores entre 20 a 40 (valor por pé quadrado). Quando analisado reduzindo o intervalo da 
#escala do eixo (x), pode-se notar uma curva com não assimétrica. 


# 1b-3) 
# Investigue NA´s (Not Available = missing data = dados ausentes)

#i) Faça summary(housing). Você identifica valores ausentes?

summary(housing)

# Existem 96 valores NA's na coluna YearBuilt

#ii) Faça sum(is.na(housing) para contar quantos são os valores ausentes.

sum(is.na(housing))

# Resultado: 96 valores

#iii) Eles estão espalhados por mais de uma variável?
#Os valores ausentes estão concentrados em uma variável que é YearBuilt.

#iv) Pesquise sobre os comandos na.omit e complete.cases.

dados_omit <- na.omit(housing)
summary(dados_omit)

#O comando na.omit removeu todos os casos incompletos de um objeto de dados (foi criada 
# uma variável chamada de dados_omint para observação do comando). 

complete.cases(housing)

# O comando complete.cases indica linhas que estão completas como TRUE e linhas incompletas 
# (NA´s) como FALSE.


#v) Faça a tabela de dupla entrada tabela_Boro_class<-xtabs (~Boro + Class, data = housing). 
#Algo chama a atenção?

tabela_Boro_class<-xtabs (~Boro + Class, data = housing)
tabela_Boro_class

# Na tabela de dupla entrada tabela_Boro_class o que me chamou a atenção foi a alta frequência
# para os dados R4-condominium em Manhattam e a baixa frequencia de dados para Staten Island, 
# de somente 26 imóveis concentrados também na classificação de R4-condominium.


#iv)Faça prop.table(tabela_Boro_class)

prop.table(tabela_Boro_class)
tabela_Boro_class

# Com o comando prop.table consegue-se verificar a distribuição das frequencias de cada 
# local e subdivisão considerando 100% dos dados (soma de 2626 conforme comando dim).
# É possível verificar a frequencia de distribuição como: Manhattan (R4-condominium) em 
# primeiro lugar, Brooklin (R4-condominium) em segundo lugar e Queens (R4-condominium) em 
# terceiro lugar


# Execução do "data Preprocessing" para correção de dados ausentes(NA´s), com a substituição pela média obtida pelo summary.


housing$YearBuilt[which(is.na(housing$YearBuilt))] = 1967


# Codificação de variáveis categóricas nas colunas Class e Boro
housing$Class <- factor(housing$Class, 
                        levels = c("R2-CONDOMINIUM", "R4-CONDOMINIUM",
                                   "R9-CONDOMINIUM", "RR-CONDOMINIUM"),
                        labels = c(1, 2, 3, 4))

housing$Boro <- factor(housing$Boro,
                       levels = c("Bronx", "Brooklyn", "Manhattan",
                                  "Queens", "Staten Island"),
                       labels = c(1, 2, 3, 4, 5))

#c) Construa os modelos de regressão 

#Modelo 1 - house1

house1 <- lm(formula = ValuePerSqFt ~ Units + SqFt + YearBuilt +
               Income + Boro, 
             data = housing)

summary(house1)

# O modelo house1 apresenta o R-Quadrado Ajustado em 66,79% (modelo explica a proporção da
# variação da variável dependente na taxa mencionada), e com variáveis independentes com 
# baixa significância estatística: Units(p=0.329) e Boro5 (p=0.220)
# O F-statistic como p < 2.2e-16, indica que o modelo possui boa qualidade.

#Modelo 2 - house2

house2 <- lm(formula = ValuePerSqFt ~ Units*SqFt + YearBuilt +
               IncomePerSqFt + Income + Boro, 
             data = housing)

summary(house2)

# O modelo house2 apresenta o R-Quadrado Ajustado em 93,26% (modelo explica a proporção da
# variação da variável dependente na taxa mencionada), mas apresenta apresenta variáveis 
# independentes com baixa significância estatística: Units(p=0.1706), SqFt(p=0.7995), Income
# (p=0.9506) e Boros.
# As variáveis quando multiplicadas Unit Sqft apresentam um baixo grau de relevância (p = 0.0536).
# O F-statistic como p < 2.2e-16, indica que o modelo possui boa qualidade.


#Modelo 3 - house3

house3 <- lm(formula = ValuePerSqFt ~ log(Units) + log(SqFt) + Boro, 
             data = housing)

summary(house3)

# O modelo house3 apresenta o R-Quadrado Ajustado em 59,6%  mas apresenta variáveis independentes
# com baixa significância estatística para os Boros.
# O F-statistic como p < 2.2e-16, indica que o modelo possui boa qualidade.

#Modelo 4 - house4

house4 <- lm(formula = log(ValuePerSqFt) ~ log(Units)+log(SqFt)+Boro, 
             data = housing)

summary(house4)

# O modelo house4 explica a variação da variável ValuePerSqFt em apro-
# ximadamente 60%, apresenta variáveis independentes com significância
# estatística. A única variável independente que possui baixa significância
# é a Boro5 com p = 22.36%.
# O F-statistic como p < 2.2e-16, indica que o modelo possui boa qualidade.

## Os 4 modelos apresentaram o mesmo F-statistic.


### Comparação dos modelos através do Akaike Information Criteria

AIC(house1, house2, house3, house4)

# Comparando modelos utilizando o Akaike Information Criteria (indica qual modelo está mais
# ajustado. Quanto menor o valor de AIC melhor é o modelo. Segue os valores dos modelos:

#house1 10 26771.382
#house2 12 22587.293
#house3  8 27285.516
#house4  8  2108.935

# Como o modelo house4 apresenta o menor valor para AIC, indico que o modelo house 4 é o 
# mais ajustado.


#d)	Apresente os gráficos de resíduos x fitted para cada um dos modelos. O que eles sugerem? 

# Modelo house1
# Gráfico Resíduos X Fitted

plot(fitted(house1), residuals(house1), 
     xlab = "fitted", ylab = "Resíduos",
     main = "house1 model", col = "blue")
abline(h=0, col = "red", lwd = 3)


# Modelo house2
# Gráfico Resíduos X Fitted 

plot(fitted(house2), residuals(house2), 
     xlab = "fitted", ylab = "Resíduos",
     main = "house2 model", col = 'red')
abline(h=0, col = "blue", lwd = 3)

# Modelo house3
# Gráfico Resíduos X Fitted 

plot(fitted(house3), residuals(house3), 
     xlab = "fitted", ylab = "Resíduos",
     main = "house3 model")
abline(h=0, col = "blue", lwd = 3)

# Modelo house4
# Gráfico Resíduos X Fitted 

plot(fitted(house4), residuals(house4), 
     xlab = "fitted", ylab = "Resíduos",
     main = "house4 model", col = "green")
abline(h=0, col = "blue", lwd = 3)


# Os gráficos de resíduos X fitted plotados apresentam caracteristicas de heterocedasticidade. 
# Os gráficos apresentam forte dispersão dos dados e variação da variância. Todos apresentaram
# outliers.
# O modelo que melhor representaria em relação a homocedasticidade/heterocedasticidade
# é o modelo house4. 


# e)	Faça o correspondente teste de Hipótese. Qual sua conclusão para cada modelo?

# Instalação da biblioteca car

library(car)

##Teste de Hipótese para Heterocedasticidade


# house1 

ncvTest(house1)

# house2 

ncvTest(house2)

# house3 

ncvTest(house3)

# house4

ncvTest(house4) 


# O Teste de Hipótese para Heterocedasticidade apresentou para todos os modelos p-value 
# abaixo no nível de significância alfa de 0.05.
# Os modelos house1, house2 e house3 apresentaram p = < 2.22e-16, devendo-se rejeitar H0


# f) Apresente os gráficos qqPlot para cada modelo. O que eles sugerem?


# house1
qqnorm(residuals(house1), ylab = "Resíduos house1", col = "red")
qqline(residuals(house1), lwd = 4, col = "blue")


# house2
qqnorm(residuals(house2), ylab = "Resíduos house2", col = "dark green")
qqline(residuals(house2), lwd = 4, col = "blue")


# house3
qqnorm(residuals(house3), ylab = "Resíduos house3", col = "purple")
qqline(residuals(house3), lwd = 4, col = "blue")

# house4
qqnorm(residuals(house4), ylab = "Resíduos house4", col = "orange")
qqline(residuals(house4), lwd = 3, col = "blue")

# Em todos os gráficos de resíduos não notou-se uma distribuição normal. 
# Os pontos apresentaram-se distantes em vários momentos da reta.


# g) Faça o correspondente teste de hipótese. Qual sua conclusão? 


# house1
shapiro.test(residuals(house1))

# house2
shapiro.test(residuals(house2))

# house3
shapiro.test(residuals(house3))

# house4
shapiro.test(residuals(house4))


# Com o teste shapiro.wilk todos osmodelos apresentaram os valores de p-value < 2.2e-16
# Em todos os modelos H0 foi descartado, ou seja, as amostras não seguiram uma distribuição 
# normal.


# h) Algum dos modelos apresenta problemas de multicolinearidade?  

library(rms)

# Teste Multicolinearidade house1
vif(house1)
sqrt(vif(house1)) > 2 

# Teste Multicolinearidade house2
vif(house2)
sqrt(vif(house2)) > 2 

# Teste Multicolinearidade house3
vif(house3)
sqrt(vif(house3)) > 2 

# Teste Multicolinearidade house4
vif(house4)
sqrt(vif(house4)) > 2 

# Todos os modelos analisados apresentaram problemas de multicolinearidade

# Todos os modelos trabalhados apresentaram problemas de heterocedasticidade, multicolinearidade 
# e distribuição não normal.