#Apresentação

Sumário teste
- 1. Preparar os dados
- 2. Normalidade
- 3. Variabilidade
- 4. Testes reunido (crd)

## 1. Preparar os dados
#### Informar o R
Aqui, é importante indicar qual pasta será usada para a análise. 
Primeiro precisamos indicar a pasta escrevendo o caminho dela dentro do seu computador. 
Algo como C://usuário.... dentro das aspas. Abaixo o meu:
  
  ```
getwd()
setwd("/home/user/Área de Trabalho/Bio/UFES/UFES_Outras_Pessoas/2023_mariana/R") #Mudança de diretório para pasta do projeto
```
#### Pacotes e entradas de arquivos
Vamos instalar logo vários pacotes que possivelmente utilizaremos para selecionar dados, fazer gráficos e tal.
```
if(!require(pacman, quietly = TRUE))(install.packages("pacman")) #agrupador de funções
pacman::p_load(magrittr,dplyr,reshape2) #magrittr para operações de pipe/dplyr para manipulador de dados
pacman::p_load(ggplot2, ggrepel, graphics,lubridate, gghighlight) #devtools, 
pacman::p_load(vegan)  #vegan para estatística ecológica/graphics para os gráficos
```
#### Caregar Planilha
```
pacman::p_load(openxlsx) 
caminho.do.arquivo <- "/home/user/Área de Trabalho/Bio/UFES/UFES_Outras_Pessoas/2023_mariana/2023_05_20_cayman_mari.xlsx"
planilhatotal <- read.xlsx(caminho.do.arquivo, #local do arquivo
                           sheet = 1, # em qual planilha estão os dados
                           colNames = T, # as colunas dos dados possuem nomes?
                           na.strings = "NA") # como estão identificados os dados omissos?

head(planilhatotal)
```

Vamos verificar se deu certo puxar a nossa tabela?
  ```  
summary(planilhatotal)
```
#### Corrigir planilha
Aqui vamos selecionar os dados. Primeiro vamos atribuir nossa tabela e denominar p2. 
```
p2 <- planilhatotal

```
Aqui tiramos as células NA ou vazias das colunas de dia, ordem e espécie.
```
p2 <- subset(p2, !is.na(tratamento))
#p2 <- subset(p2, !is.na(Altitude)) 
```
## 2. Normalidade
Primeiro o pacote para isso.
```
pacman::p_load(fitdistrplus)
```
Teste de normalidade por coluna para ver se a variável segue essa distribuição. 
No caso para media_serrap e é uma vairável normal. 
```
fit.MF.normal <- fitdist(p2$media_serrap, "norm") #gráfico de distribuição normal
plot(fit.MF.normal)
```
Podemos isolar o gráfico Q-Q Plot. Basicamente, um gráfico Q-Q é um gráfico de dispersão criado com a 
ajuda do uso de unidades de quantis em relação um ao outro. Se cada unidade de quantis veio de uma 
distribuição idêntica, temos que ver os fatores formando uma linha que é mais ou menos reta. 
Também e adicionar variáveis:
  ```  
ggplot(p2, aes(sample=dist_h2o, colour = tratamento)) +
  #geom_smooth(method = lm,se = FALSE, alpha = 0.6, aes(colour = tratamento)) +
  stat_qq() + 
  theme_bw()
```
Para garantir é fazendo um teste de Shapiro Wilk. Se p não for significativo a variável é normal.
```
shapiro.test(p2$media_serrap) 
```
Nesse caso fica claro que a variável é normal.

## 3. Variabilidade
```
dicalt <- lm(media_serrap ~ tratamento, data = p2) 
anova(dicalt) 

summary(dicalt)
```

Nesse teste a h0 significa não variável, o que foi estatisticamente não confirmado.
Note forma considerados cinco continentes e que o F é diferença das médias. 
Uma outra forma de ver é pelo summary. 

### Dados não paramétricos
Vamos carregando pacotes.
```
pacman::p_load(ggside, stringr) #, tidyverse,tidyquant)
```
Fazendo as tabelas.
```
p3 <- p2
nula <- p3 %>% filter(str_detect(tratamento, "nula"))
nula <- subset(nula, id_ninho!="Arclg2AN18")
nula <- subset(nula, id_ninho!="Arclg2AN19")

ninho <- p3 %>% filter(str_detect(tratamento, "ninho")) 
```

Rodar o teste
```
testeWilcox <- wilcox.test(ninho$dist_h2o, nula$dist_h2o,paired = T,alternative = "less", conf.level = 0.95)
testeWilcox
```
O significado:
H0: não são diferentes
H1: que são diferentes

#### Regressão
Chamando alguns pacotes
```
pacman::p_load(jtools, sandwich, lme4, ggstance, vegan, rpart) 
require(jtools,rpart,vegan)
``` 
Calcular se existe diferença.
```
summary(aov(p2$dist_h2o ~ factor(p2$tratamento)))
```
No caso existe diferença estatística, a hipotése nula de serem iguais foi negada. 
```
Destrinchando os dados.
```
a<-lm(p2$dist_h2o ~ factor(p2$tratamento)) 
summ(a)
```
#### 4. Testes reunidos (crd)

Agora, uma análise resumida de vaŕios testes

```
pacman::p_load(ExpDes)
crd(p2$tratamento, p2$elev, mcomp = "tukey", sigF = 0.01, sigT = 0.01)
```
E vamos gerar um gráfico:
```
ggplot(p2, aes(x = tratamento, y = dist_h2o)) + 
  geom_boxplot(aes(colour = tratamento))+
  theme_classic()
```