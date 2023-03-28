# Apresentação
Vou dividir em alguns tópicos pra gente entender melhor, certo.

- Preparar o dados;
- Acumulação;
- Estimativa de Riqueza;
- Diversidades das UCs;
- Gráficos laterais;
- Análise de variância.

## 1. Preparar os dados
#### Informar o R
Aqui, é importante indicar qual pasta será usada para a análise. Primeiro precisamos indicar a pasta escrevendo o caminho dela dentro do seu computador. Algo como C://usuário.... dentro das aspas. Abaixo o meu:
```
getwd()
setwd("/home/user/Área de Trabalho/Bio/UFES/UFES Outras Pessoas/2022_09_14_thais") #Mudança de diretório para pasta do projeto
```

#### Pacotes e entradas de arquivos
Vamos instalar logo vários pacotes que possivelmente utilizaremos para selecionar dados, afzer gráficos e tal.
```
if(!require(pacman, quietly = TRUE))(install.packages("pacman")) 
pacman::p_load(magrittr,dplyr) 
pacman::p_load(ggplot2, devtools, ggrepel, graphics) 
pacman::p_load(vegan,lubridate) 
```
#### Caregar Planilha
```
pacman::p_load(openxlsx) 
caminho.do.arquivo <- "/home/user/Área de Trabalho/Bio/UFES/UFES Outras Pessoas/2022_09_14_thais/01. Gráficos (ARTIGO).xlsx"
planilhatotal <- read.xlsx(caminho.do.arquivo, #local do arquivo
                           sheet = 1, # em qual planilha estão os dados
                           colNames = T, # as colunas dos dados possuem nomes?
                           na.strings = "NA") # como estão identificados os dados omissos?

head(planilhatotal)
```

Vamos verificar se deu certo puxar a nossa tabela?

`summary(planilhatotal)`

Agora o gŕafico.
```
p2 <- planilhatotal

ggplot(p2, aes(x = reorder(Tipos, +Order), y = Dist)) + 
  geom_boxplot(aes(colour = Tipos)) +
  xlab(" ") +
  ylab(" ") + 
  guides(color = FALSE, size = FALSE) +
  theme_classic()
  
#ggsave("dist.png",width = 10, height = 6, dpi = 300)

ggplot(p2, aes(x = Dist, fill = Tipos)) +
  geom_histogram() +
  facet_wrap(vars(Tipos)) +
  theme_classic()
  
  
```

## Análise de Variância
Primeiro a tabela
```
A Análise de Variância (ANOVA) trata-se de um método estatı́stico que permite realizar comparações simultâneas entre duas ou mais médias, ou seja, permite testar hipóteses sobre médias de distintas populações. Pressuposto: 
- Todas as observações devem ser independentes; 
- As observações em cada grupo devem possuir uma distribuição, aproximadamente normal; 
- As variâncias em cada grupo devem ser aproximadamente iguais.

Importante se atentar:
- Relação de espécie (variável dependente);
- #h0 = não variável/ F= diferença das médias / a variação de altitude média por espécie é de 1310m
```
pacman::p_load(tidyverse, FSA, emmeans)

dicalt <- lm(Dist ~ Tipos, data = p2) 
anova(dicalt) 
```
Dando valor de p siginificativo os dados são considerados variados. Para ver com mais detalhes:
```
summary(dicalt)
```
Onde R = explicação (mais perto de 1 melhor) e p significativo, pelo menos um dos anos se comporta de forma diferente. Para as variáveis de confiança:
```
confint(dicalt)
```
E a variãncia para a assembléia de espécies:
```
summary(aov(p2$Dist ~ factor(p2$Interações)))
```
Análise de variância por order
```
pacman::p_load(ExpDes)
crd(p2$Order, p2$Dist, mcomp = "tukey", sigF = 0.01, sigT = 0.01)
```
Regerssão linar

```
ggplot(p2,aes(x=Linha,y=Dist, colour = Tipos)) +
  geom_smooth(method="lm",alpha=0.2) +
  geom_point()+
  #facet_wrap(vars(Tipos)) +
  theme_classic()
```
Normalidade 
```
pacman::p_load(fitdistrplus)
fit.MF.normal <- fitdist(p2$Dist, "norm") #gráfico de distribuição normal
plot(fit.MF.normal)

ggplot(p2, aes(sample=Dist, colour = Tipos)) +
     stat_qq() + 
     theme_bw()
```