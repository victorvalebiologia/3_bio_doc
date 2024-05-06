# Apresentação

Oi Gabi. Aqui vai um script detalhado do que vamos fazer certinho para o cluster, blz. Precisando de mais coisa é só falar que eu vou atualziando o script. Vou dividir em alguns tópicos pra gente entender melhor, certo.

- 1. Preparar o dados;
- 2. Agrupamento de cluster de Jaccard;

## 1. Preparar os dados
#### Informar o R
Aqui, é importante indicar qual pasta será usada para a análise. Primeiro precisamos indicar a pasta escrevendo o caminho dela dentro do seu computador. Algo como C://usuário.... dentro das aspas. Abaixo o meu:
```
getwd()
setwd("/home/user/Área de Trabalho/Bio/projeto_publ/2020_santa_teresa/R") 
```
#### Pacotes e entradas de arquivos
Vamos instalar logo vários pacotes que possivelmente utilizaremos para selecionar dados, afzer gráficos e tal.
```
if(!require(pacman, quietly = TRUE))(install.packages("pacman")) #agrupador de funções
pacman::p_load(magrittr,dplyr,reshape2) #magrittr para operações de pipe/dplyr para manipulador de dados
pacman::p_load(ggplot2, ggrepel, graphics, lubridate, tidyquant, tidyverse, tidygraph, patchwork) 
pacman::p_load(vegan)  #vegan para estatística ecológica/graphics para os gráficos
pacman::p_load(forcats,iNEXT,tidyr,tibble,iNEXT) #hill,CRAN e riqueza estimada
```

#### Carregar Planilha
Vou carregar uma planilha xlsx, um excel. A planilha que eu vou usar é uma de dados de Santa Teresa, do specieslink que usei no meu cap 4. Cada linha um espécime, beleza.
```
pacman::p_load(openxlsx) 
caminho.do.arquivo <- "/home/user/Área de Trabalho/Bio/projeto_publ/2020_santa_teresa/2022_santateresa.xlsx"
planilhatotal <- read.xlsx(caminho.do.arquivo, 
                           sheet = 1,
                           colNames = T, 
                           na.strings = "NA") 
```                           

Vamos verificar se deu certo puxar a nossa tabela?

`summary(planilhatotal)`

#### Corrigir planilha
Aqui vamos selecionar os dados. Primeiro vamos atribuir nossa tabela e denominar p2 e deixar o planilhatotal como backup.

`p2 <- planilhatotal`

Depois vamos selecionar os dados que queremos. Vou deixar só os que estão no município de Santa Teresa. 

`p2 <- subset(p2, Município == "Santa Teresa")`

Aqui tiramos as células N/A ou vazias das colunas de dia, ordem e espécie.
```
p2 <- subset(p2, !is.na(Dia))
p2 <- subset(p2, !is.na(Ordem)) 
p2 <- subset(p2, !is.na(Espécie)) 
```

E vamos verificar mais uma vez.

`summary(p2)`

#### Teste de plots
Vamos dar uma olhada, mais fácil de reparar se a tabela está certa. Vou querer fazer uma similaridade entre as comunidades, então vamos fazer um gráfico. 
```
ggplot(p2, aes(x = Ano, y = Comunidade, colour = Ordem)) + 
  geom_point(size = 6, alpha = 0.7) +
  #geom_smooth(method = lm, se = TRUE) + 
  #scale_x_continuous(breaks = 0:1) +
  #scale_x_date(date_labels = "%Y %m %d") +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() 

```
Bom, alguns locais não possuem dados de Comunidade, então vamos tirar da tabela

`p2 <- subset(p2, !is.na(Comunidade))`

Rodei de novo e deu certo, então vamos seguir,

## 2. Similaridade
### Cluster para definir os grupos de comunidade

Cluster baseado em jaccard para definir os agrupamentos, conseidaramos aqui os dados binários. Primeiro vamos trazer o pacote, depois precisamos preparar uma tabela onde as linhas são as localidades e as colunas as espécies.
```
pacman::p_load("ade4")

local<-reshape2::dcast(p2, Comunidade ~ Espécie, value.var = "Abundância",fun.aggregate = sum)
local=data.frame(local, row.names=1)
```
Agora só preparar o cluster.
```

d <- dist.binary(local, method = 1, diag = FALSE, upper = FALSE) 
hc <- hclust(d)
plot(hc, labels = local$Altitude)
```
Deu certo, agora para exportar.
```

caminho_do_arquivo <- file.path("/home/user/Área de Trabalho/Bio/projeto_publ/2020_santa_teresa/R", "seu_grafico.png")

png(caminho_do_arquivo, width = 800, height = 600)  # Ajuste as dimensões conforme necessário
plot(hc, labels = local$Altitude)
dev.off()

```
