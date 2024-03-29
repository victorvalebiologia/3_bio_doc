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
setwd("/home/user/Área de Trabalho/Bio/projeto_publ/2020_santa_teresa/R") #Mudança de diretório para pasta do projeto
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
caminho.do.arquivo <- "/home/user/Área de Trabalho/Bio/projeto_publ/2020_santa_teresa/2022_santateresa.xlsx"
planilhatotal <- read.xlsx(caminho.do.arquivo, #local do arquivo
                           sheet = 1, # em qual planilha estão os dados
                           colNames = T, # as colunas dos dados possuem nomes?
                           na.strings = "NA") # como estão identificados os dados omissos?

head(planilhatotal)
```

Vamos verificar se deu certo puxar a nossa tabela?

`summary(planilhatotal)`

#### Corrigir planilha
Aqui vamos selecionar os dados. Primeiro vamos atribuir nossa tabela e denominar p2 e depois selecionar os dados se necessário:

```
p2 <- planilhatotal
p2 <- subset(p2, Ponto!="Fora") 
p2 <- subset(p2, OBS == "Dentro")
p2 <- subset(p2, !is.na(Espécie))
p2 <- subset(p2, !is.na(Ano))

p2 <- subset(p2,Distrito!="Identificar") 
p2 <- subset(p2,Ponto!="Fora") 
p2 <- subset(p2, OBS == "Dentro") 
```
Outras seleções:
```
p2 <- subset(p2,Coletor!="Yuri Luiz Reis Leite")
p2 <- subset(p2, !is.na(Ordem)) 
```
#### Teste de plots
Primeiro vamos configurar a data (alguns dados não tem dia e nem mês, não vai rolar).

```
p2 <- subset(p2, !is.na(Mês))
p2 <- subset(p2, !is.na(Dia))

p3 <- p2 %>% 
  select(Ano, Mês, Dia) %>% 
  mutate(Data = make_date(Ano, Mês, Dia))

Data <- data.frame(p2,p3)
 
```
Um gráfico um pouco mais elaborado. Vamos plotar as espécies no eixo y e as altitudes no eixo x, com cores vamos diferenciar as UCs. 

Agora o gŕafico.
```
ggplot(Data, aes(x = Data, y = Família, colour = Ordem)) + 
  geom_boxplot(alpha = 0.7) +
  #geom_smooth(method = lm, se = TRUE) + 
  #scale_x_continuous(breaks = 0:1) +
  #scale_x_date(date_labels = "%Y %m %d") +
  #facet_grid(Localitade~.) +
  theme_classic()
#ggsave("1.Família/Data.png",width = 10, height = 6, dpi = 300)
```

### Acumulação
Vamos cacular os principais índices de dievrsidade aqui. Primeiro vamos selecionar oa dados que podem ser:
- Gerais;
- Avifauna;
- Herpetofauna;
- Mastofauna.
Além de filtrar para apenas dados primários e espećies nativas.

Agora a tabela para trabalhar.
```
p2<-Data
p2 <- subset(p2, !is.na(Data))
p2 <- subset(p2, !is.na(Espécie))
acum<-reshape2::dcast(p2, Data ~ Espécie, value.var = "Abundância",fun.aggregate = sum)
acum=data.frame(acum, row.names=1)
```
E vamos gerar a curva:
```
acumplot<-specaccum(acum) #dados de acumulação
plot(acumplot) #curva simples
plot(acumplot,ci.type="poly",col="black",lwd=2,ci.lty=0,ci.col="lightgrey",ylab="Riqueza",
     xlab="Dias de amostragem",main="Curva de acumulação de registros",las=1,font=1.5,font.lab=1.5,cex.lab=1,cex.axis=1) #curva clássica
```
Podemos fazer alguns tipos de gráficos de acumulação, veremos a seguir em três etapas, primeiro selecionando a tabela.
```
acum<-reshape2::dcast(p2, Data ~ Espécie, value.var = "Abundância",fun.aggregate = sum)
acum <- subset(acum, !is.na(Data))
acum=data.frame(acum, row.names=1)
```
Segundo os cálculos
```
sp1<-specaccum(acum,method="rarefaction")
sp2<-specaccum(acum,method="exact")
sp3<-specaccum(acum,method="random")
sp4<-specaccum(acum,method="collector")
```
Por fim os gráficos:
```
#par(mgp=c(1,1,0)) #exportar a imagem
#png(filename="/home/user/Área de Trabalho/Bio/projeto_publ/2020_santa_teresa/R/acum2.png",width=800,height=600) #local e tmamanho
par(mfrow=c(2,2)) 
plot(sp1, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Dias de amostragem",ylab="Rarefação")
plot(sp2, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightgrey",xlab="Dias de amostragem",ylab="Riqueza Esperada")
plot(sp3, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="yellow",xlab="Dias de amostragem",ylab="Sítios Aleatórios")
plot(sp4, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Dias de amostragem",ylab="Curva do Coletor")
#par(mfrow=c(1,1)) #compilado de curvas
dev.off()
```
Podemos focar também a curva do coletor e adicionar a abundância por dia. Primeiro vamos aos cálculos de diversidade e abundância.
```
sp4<-specaccum(acum,method="collector")
spAbund<-rowSums(acum)
```
Agora preparamosa tabela:
```
acum<-reshape2::dcast(Data, Data ~ Espécie, value.var = "Abundância",fun.aggregate = sum)
acum <- subset(acum, !is.na(Data))
p3<-data.frame(spAbund,sp4$sites,sp4$richness,acum)
```
E plotamos:
```
ggplot(p3, aes(x = Data, y = sp4.richness)) + 
  geom_line(size=6, alpha=0.1) + #geom_line(aes(group = sp4.sites)) +
  geom_point(aes(size=spAbund), alpha=0.2) +
  scale_size_binned(range = c(.1, 18), name="Abundância de registros") +
  #geom_text(aes(label = a$sp4.richness),col = 'black',size = 5) +
  labs(title="Curva do coletor", subtitle="Riqueza e abundância por dia", y="Riqueza",x="Data", caption="",
       color = "Diversidade", size = "Abundância de registros") +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) + 
  scale_x_date(date_breaks = "130 months", date_labels = "%Y/%b") + 
  scale_y_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120)) +
  theme_classic() 
#ggsave("acum3.png",width = 14, height = 6, dpi = 300)
```
### Estimativa de riqueza
Vamos também estimar a riqueza. Vamos selecionar a tabela. Podem ter duas variáveis:
- Trilha;
- Localidade;
- Empresa;
- Grupo;

Primeiro vamos foltrar e atribuir as datas:

```
p2 <- Data
p2 <- subset(p2, !is.na(Data))
p2 <- subset(p2, !is.na(Espécie))
```
Vamos cacular os principais índices de dievrsidade aqui. Primeiro vamos selecionar oa dados que podem ser:
- Gerais;
- Avifauna;
- Herpetofauna;
- Mastofauna.
Além de filtrar para apenas dados primários e espećies nativas.

```
p3<-reshape2::dcast(Data, Data ~ Espécie, value.var = "Abundância",fun.aggregate = sum)
p3 <- subset(p3, !is.na(Data))
p3=data.frame(p3, row.names=1)
```
Agora vamos estimar a riqueza considerando a localidade toda:
```
pool<-specpool(p3)
pool
```
E um gráfico:
```
pool <- poolaccum(p3)
#summary(pool, display = "chao")
plot(pool)
```
Também podemos separar pelas varíaveis seguintes:
- Localidade;
- Trilha;
- Classe.

Precisamos fazer duas tabelas. 
- Uma indicando a abundância de espécies por dia, denominada de p3;
- Uma indicando a variável, denominada p4.
- Lembrar de conferir a variável
```
#p3 <- subset(Data, Empresa == "XXX")
p3<-reshape2::dcast(Data, Data + Distrito ~ Espécie, value.var = "Abundância",fun.aggregate = sum)
excluir <- c("Data", "Distrito")
p3 <- p3[,!(names(p3)%in% excluir)]
p4<-reshape2::dcast(Data, Data + Distrito ~ Classe, value.var = "Abundância",fun.aggregate = sum)
```
Agora a estimativa de riqueza por localidade.
```
pool<-specpool(p3, p4$Distrito) 
pool
plot(pool)
boxplot(pool$chao) 
```

### Diversidade
Aqui também podemos filtrar a tabela.
- Gerais;
- Avifauna;
- Herpetofauna;
- Mastofauna.
Além de filtrar para apenas dados primários e espećies nativas.

Agora vamos filtrar a tabela, ela pode ser por:
- Classe;
- Família;
- Influência;
- Empresa;
- Localidade.

Para considerar apenas os registros de data completa trocar p2 por Data.
```
p2 <- planilhatotal
p2 <- subset(p2, Ponto!="Fora") 
p2 <- subset(p2, OBS == "Dentro")
p2 <- subset(p2, !is.na(Espécie))
p2 <- subset(p2, !is.na(Ano))


p2 <- subset(p2,Distrito!="Identificar") 
p2 <- subset(p2,Ponto!="Fora") 
p2 <- subset(p2, OBS == "Dentro") 

p2 <- subset(p2, !is.na(Comunidade))

local<-reshape2::dcast(p2, Instituição ~ Espécie, value.var = "Abundância",fun.aggregate = sum)
local=data.frame(local,row.names=1)
```
E vamos aos cálculos;
```
S <- specnumber(local) 
spAbund <-rowSums(local) #abunância por faixa
shannon <- diversity(local)
J <- shannon/log(S) #Pielou
simp <- diversity(local, "simpson")
invsimp <- diversity(local, "inv")
```
E vamos plotar em gráfico, mas primeiro a tabela.
```
#p2 <- subset(p2,Comunidade!="Identificar") 
local<-reshape2::dcast(p2, Família + Ordem ~ Espécie,value.var = "Abundância",fun.aggregate = sum)
local<-data.frame(S, spAbund,local)
```
Gráfico é um de barras:
```
ggplot(local, aes(x = reorder(Família, S), y = S)) + 
  geom_col(aes(weight = S, fill = Ordem), alpha = 0.7) + 
  geom_point(aes(y = S, x = Família, size = spAbund, colour = Ordem)) +
  geom_text(aes(y = S, x = Família, label = S), size=4, alpha= 1) +
  labs(title="Riqueza e diversidade", subtitle="Diversidade", y="Riqueza", x="Família", caption="Dados primários",
       fill = "Ordem", colour = "Ordem", size = "Abundância") +
  scale_size_binned(range = c(.1, 18)) +
  #scale_y_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 14)) + 
        coord_flip() + theme_classic() 
#ggsave("fam.png",width = 9, height = 7, dpi = 600)
```
### Distribuição Temporal

Vamos ver a riqueza de dados ao passar dos anos. Essa riqueza pode ser de:

- Família;
- Distrito.

Primeiro vamos separar a tabela.
```
p2 <- planilhatotal
p2 <- subset(p2, Ponto!="Fora") 
p2 <- subset(p2, OBS == "Dentro")
p2 <- subset(p2, !is.na(Espécie))
p2 <- subset(p2, !is.na(Ano))

p2 <- subset(p2,Distrito!="Identificar") 
p2 <- subset(p2,Ponto!="Fora") 
p2 <- subset(p2, OBS == "Dentro") 

local<-reshape2::dcast(p2, Ano ~ Família) 
local=data.frame(local, row.names=1)
```
Agora vamos ver alguns índices simples, como abundância e riqueza:
```
abund<-rowSums(local) #abunância por faixa
S <- specnumber(local) 
```
E vamos plotar em gráfico:
```
local<-reshape2::dcast(p2, Ano ~ Família)

local<-data.frame(S,abund,local)
ggplot(local, aes(x = Ano, y = S)) + 
  geom_point(aes(size=abund, colour = S), alpha = 0.4) + 
  geom_line(aes(colour = S), size = 2, alpha = 0.35) +
  scale_size_binned(range = c(.1, 16), name = "Número de álbuns") +
  #scale_y_continuous(breaks = c(1900, 1920, 1940, 1960, 1980, 2000, 2200)) +
  #scale_x_continuous(breaks = c(10, 20, 40, 60, 80)) +
  geom_label_repel(aes(label = abund), size=4, alpha= 1,box.padding   = 0.35,point.padding = 0.75,segment.color = 'grey50') +
  labs(title="Família por ano", subtitle="Abundância de Registros",y="",x="Ano", caption="",color = "Riqueza por ano",size = "") +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("2.Ano_count.png",width = 17, height = 8, dpi = 600)
```
### Gráficos laterais
Vamos a outro gráfico lateral:
Primeiro o pacote. 
```
pacman::p_load(ggside, tidyverse,tidyquant,ggExtraPlot)
```
Agora o gráfico, considerando a abundância:
```
p2 <- planilhatotal
p2 <- subset(p2, Ponto!="Fora") 
p2 <- subset(p2, OBS == "Dentro")
p2 <- subset(p2, !is.na(Espécie))
p2 <- subset(p2, !is.na(Ano))


p2 <- subset(p2,Distrito!="Identificar") 
p2 <- subset(p2,Ponto!="Fora") 
p2 <- subset(p2, OBS == "Dentro") 

p2 <- subset(p2, !is.na(Coletor_tag))

p4 <- p2 %>%
  ggplot(aes(x = Ano, y = Instituição, colour = Coletor_tag)) +
  geom_boxplot() +
  geom_smooth(aes(color = NULL), se=TRUE) +
  geom_xsidedensity(aes(y = after_stat(scaled), 
    fill = Coletor_tag), size = 1, position = "stack") + #alpha = 0.5,
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  labs(title = "Boxplot temporal para o coletor", subtitle = "Acumulação",
       x = "Tempo", y = "instituição", colour = "Coletores", fill = "Coletores") +  theme(ggside.panel.scale.x = 0.2, ggside.panel.scale.y = 0.2)
plot(p4)
#ggsave("overlap.png",width = 12, height = 8, dpi = 600)
#geom_ysidedensity(aes(x = after_stat(density),
    #fill = Order),alpha = 0.5,size = 1, position = "stack") +
```
Agora um destque no gráfico lateral:
```
p2 <- planilhatotal
p2 <- subset(p2, !is.na(Espécie))
p2 <- subset(p2, !is.na(Ano))
p2 <- subset(p2, !is.na(Coletor_tag))
p2 <- subset(p2,Ordem!="Perissodactyla") 

p4 <- p2 %>%
  ggplot(aes(x = Ano, y = Classe, colour = Instituição)) +
  #geom_boxplot() +
  geom_smooth(aes(color = NULL), se=TRUE) +
  stat_density(aes(y = after_stat(scaled),
    fill = Instituição),alpha = 1,size = 1, position = "stack") +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  labs(title = "Boxplot temporal para o coletor", subtitle = "Acumulação",
       x = "Tempo", y = "Instituição") +  theme(ggside.panel.scale.x = 0.2, ggside.panel.scale.y = 0.2)
plot(p4)

#ggsave("overlap2.png",width = 12, height = 8, dpi = 600)
```
Agora com as famílias:

```
p2 <- planilhatotal
p2 <- subset(p2, Ponto!="Fora") 
p2 <- subset(p2, OBS == "Dentro")
p2 <- subset(p2, !is.na(Espécie))
p2 <- subset(p2, !is.na(Ano))


p2 <- subset(p2,Distrito!="Identificar") 
p2 <- subset(p2,Ponto!="Fora") 
p2 <- subset(p2, OBS == "Dentro") 

p2 <- subset(p2, !is.na(Coletor_tag))
p2 <- subset(p2,Ordem!="Perissodactyla") 

p4 <- p2 %>%
  ggplot(aes(x = Ano, y = Instituição, Colour = Ordem, fill = Ordem)) +
  geom_boxplot(aes(fill = Ordem, colour = Ordem), alpha = 0.6) +
  geom_xsidedensity(aes(y = after_stat(scaled), 
    fill = Ordem, colour = Ordem), size = 1, position = "stack") + #alpha = 0.5,
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  labs(title = "Boxplot temporal para o coletor", subtitle = "Acumulação",
       x = "Tempo", y = "Ordem") +  theme(ggside.panel.scale.x = 0.2, ggside.panel.scale.y = 0.2)
plot(p4)
#ggsave("ovfam.png",width = 17, height = 8, dpi = 600)
#geom_ysidedensity(aes(x = after_stat(density),
    #fill = Order),alpha = 0.5,size = 1, position = "stack") +
```
Agora um destque no gráfico lateral:
```
p2 <- planilhatotal
p2 <- subset(p2, !is.na(Espécie))
p2 <- subset(p2, !is.na(Ano))
p2 <- subset(p2, !is.na(Coletor_tag))
p2 <- subset(p2,Ordem!="Perissodactyla") 

p4 <- p2 %>%
  ggplot(aes(x = Ano, y = Classe, colour = Ordem)) +
  #geom_boxplot() +
  geom_smooth(aes(color = NULL), se=TRUE) +
  stat_density(aes(y = after_stat(scaled),
    fill = Ordem),alpha = 1,size = 1, position = "stack") +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  labs(title = "Boxplot temporal para o coletor", subtitle = "Acumulação",
       x = "Tempo", y = "Ordem") +  theme(ggside.panel.scale.x = 0.2, ggside.panel.scale.y = 0.2)
plot(p4)
#ggsave("ovfam2.png",width = 12, height = 8, dpi = 600)
#ggsave("overlap2.png",width = 12, height = 8, dpi = 600)
```

## Análise de Variância
Primeiro a tabela
```
p2 <- planilhatotal
p2 <- subset(p2, Ponto!="Fora") 
p2 <- subset(p2, OBS == "Dentro")
p2 <- subset(p2, !is.na(Espécie))
p2 <- subset(p2, !is.na(Ano))

p2 <- subset(p2,Distrito!="Identificar") 
p2 <- subset(p2,Ponto!="Fora") 
p2 <- subset(p2, OBS == "Dentro") 
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

dicalt <- lm(Data$Data ~ Data$Espécie, data = Data) 
anova(dicalt) 
```
Dando valor de p siginificativo os dados são considerados variados. Para ver com mais detalhes:

`summary(dicalt)`

Onde R = explicação (mais perto de 1 melhor) e p significativo, pelo menos um dos anos se comporta de forma diferente. Para as variáveis de confiança:

`confint(dicalt)`

E a variãncia para a assembléia de espécies:

`summary(aov(Data$Ano ~ factor(Data$Espécie)))`

Também vamos ver a relação deles com a altitude. Primeiro os pacotes:
```
require(jtools,rpart,vegan)
pacman::p_load(jtools, sandwich, lme4, ggstance)
```
Agora o gráfico para as espécies.
```
b<-glm(Ano ~ Ordem, data = Data)
jtools::plot_summs(b)
plot_summs(b, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)
```
E para os coletores (Coletor_tag) ou Comunidade/Distrito:
```
fit <- lm(Ano ~ Ordem, data = Data) #some os primeiros de cada grupo
summ(fit)
effect_plot(fit, pred = Order, interval = TRUE, plot.points = TRUE) #não faz sentido
plot_summs(fit, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)
```

### Normalidade
Teste de significância CRD (CV, Shapiro-Wilk, Homogenety, Tukey) com o pacote #Expdes - pacote de análise, normalidade, Turkey, shapiro-wilk e afins, onde as tabelas tem que ser iguais.
```
pacman::p_load(ExpDes, ExpDes.pt, fitdistrplus) 
crd(p2$Espécie, p2$Ano, mcomp = "tukey", sigF = 0.01, sigT = 0.01)
```
Não foi considerado normal,homocedástico,Tukey deu todos diferentes (ele separou as localidades, talvez usar). Vamos ver em um gráfico:
```
pacman::p_load(fitdistrplus) #análise de normalidade
shapiro.test(p2$Ano) #ho-normal / dados não normal
descdist(p2$Ano, boot = 500, discrete = F, graph = T) #distribuição normal
```
E os gráficos de QQ e outros:
```
fit.MF.normal <- fitdist(p2$Ano, "norm") #gráfico de distribuição normal / Altitude distribuição normal
plot(fit.MF.normal, demp = TRUE)
denscomp(fit.MF.normal, addlegend=TRUE)
#ppcomp(fit.MF.normal, addlegend=FALSE)
#qqcomp(fit.MF.normal, addlegend=FALSE)
```

############ 

### Gif

pacman::p_load(gganimat,ggplot2,dplyr,gapminder,ggthemes,gifski,readr,tidyr,cargo, rustc)
devtools::install_github('thomasp85/gganimate', force = TRUE)
### Baxixar cargo antes
devtools::install_github("r-rust/gifski", force = TRUE)
#install.packages("gifski", type = "source")

graph1 = p2 %>%
  ggplot(aes(x=Altitude, y=Species, color=Order)) +
  geom_point(alpha = 0.7, stroke = 0, size=8) +
  theme_fivethirtyeight() +
  scale_size(range=c(2,12), guide="none") +
  scale_x_log10() +
  labs(title = "Atitudinal range",
       x = "Altitude",
       y = "Species",
       color = "Order",
       caption = "Fonte: Planilahtotal") +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        legend.text=element_text(size=10)) +
  scale_color_brewer(palette = "Set2") + theme_classic()

graph1.animation = graph1 +
  transition_time(Altitude) +
  labs(subtitle = "Altitude: {frame_time}") +
  shadow_wake(wake_length = 0.1) 
  
animate(graph1.animation, height = 500, width = 800, fps = 30, duration = 20,
        end_pause = 60, res = 100, renderer = gifski_renderer()) 
#anim_save("11.aniamtion_faixa.gif") # ou criar em site como https://gifmaker.me/
#anim_save("out.gif",animation = graph1.animation)


### Teste de correlação,olhar depois:
Vamos ver a relação de riqueza, diversidade, abundância e esforço:
```
a<-ggplot(dados.clip, aes(x = Mean.Altitude, y = Richness)) + 
  geom_point(aes(colour = Factor)) + #
  geom_smooth(method = loess, se = TRUE, alpha = 0.2) + theme_classic()
  #geom_smooth(method = lm, se = TRUE, colour = "red", alpha = 0.2) + theme_classic() #correlação negativa

b<-ggplot(dados.clip, aes(x = Mean.Altitude, y = Shannon)) + 
  geom_point(aes(colour = Factor)) + #
  geom_smooth(method = loess, se = TRUE, alpha = 0.2) + theme_classic()
#geom_smooth(method = lm, se = TRUE, colour = "red", alpha = 0.2) + theme_classic() #correlação negativa

c<-ggplot(dados.clip, aes(x = Mean.Altitude, y = Abundance)) + 
  geom_point(aes(colour = Factor)) + #
  geom_smooth(method = loess, se = TRUE, alpha = 0.2) + theme_classic()
#geom_smooth(method = lm, se = TRUE, colour = "red", alpha = 0.2) + theme_classic() #correlação negativa

d<-ggplot(dados.clip, aes(x = Mean.Altitude, y = Sampling.Effort)) + 
  geom_point(aes(colour = Factor)) + #
  geom_smooth(method = loess, se = TRUE, alpha = 0.2) + theme_classic()
#geom_smooth(method = lm, se = TRUE, colour = "red", alpha = 0.2) + theme_classic() #correlação negativa

pacman::p_load(gridExtra) 
grid.arrange(a, b, c, d ,
             ncol=2, nrow=2)

#ggsave("loesst.png",width = 9, height = 5, dpi = 600)
```
E a relação da média de altitude com os fatores:
```
ggplot(dados.clip, aes(x = Richness, y = Altitude.range)) +
  geom_point(aes(size = Sampling.Effort, colour=Shannon), alpha=0.6) +
  geom_label_repel(aes(label = Locality), size=4, alpha= 0.7, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_smooth(method = lm, se = TRUE) +
  facet_grid(.~Factor) + theme_classic() 

#ggsave("tendencia.png",width = 9, height = 5, dpi = 600)
```

Teste de correlação, ver mais:
```
dados.clip %>% 
  subset(Richness > 0) %$% #escolher a partir dos dados (relação de alt com as anos)
  cor.test(Richness, Mean.Altitude) #tem significância de riqueza

dados.clip %>% 
  subset(Mean.Altitude > 1500) %$% #Dados acima de 1500 tem correlação negativa de riqueza
  cor.test(Richness, Mean.Altitude) #tem significância de riqueza

dados.clip %>% 
  subset(Shannon > 0) %$% #escolher a partir dos dados (relação de alt com as anos)
  cor.test(Shannon, Mean.Altitude) #não tem significância

dados.clip %>% 
  subset(Mean.Altitude > 1500) %$% #escolher a partir dos dados (relação de alt com as anos)
  cor.test(Shannon, Mean.Altitude) #tem significância
```

Significância entre altitude e espécie
```
pacman::p_load(jtools, sandwich, lme4, svyglm, ggstance) #plot lm
#os some A. cursos, acho que o (intecep é ele)
a<-summary(aov(dados.clip$Mean.Altitude ~ factor(dados.clip$Richness))) #signifcância da assembléia por altitude
a #não significativo
a<-summary(lm(dados.clip$Mean.Altitude ~ dados.clip$Richness)) #signifcância das espécies por altitude
a #não significativo
#anova(lm(dados.clip$Sampling.Effort ~ dados.clip$Shannon)) #signifcância das espécies por altitude
a<-lm(dados.clip$Mean.Altitude ~ dados.clip$Richness)
#plot(a, which=c(1,2,3,4,5,6))
summ(a) #não significativo
summ(a, robust = "HC1") #não significativo
#summ(a, scale = TRUE, n.sd = 2)
#summ(a, center = TRUE)
#summ(a, confint = TRUE, digits = 3)
#glm(planilhatotal$Altitude ~ planilhatotal$Species)
b<-glm(dados.clip$Mean.Altitude ~ dados.clip$Richness)
summ(b) #não significativo
#summ(b, exp = TRUE)
#c<-lmer(planilhatotal$Altitude ~ planilhatotal$Species) #randomização, não deu certo
#plot_summs(b)
#plot_summs(b, scale = TRUE)
#plot_summs(b, scale = TRUE, inner_ci_level = .9)
plot_summs(b, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)

fit <- lm(Mean.Altitude ~ Richness + Abundance + Shannon + Alpha + Pielou, data = dados.clip) 
summ(fit)
fit$coefficients
#effect_plot(fit, pred = Order, interval = TRUE, plot.points = TRUE) #não faz sentido
plot_summs(fit, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)

dados.clip %>% 
  subset(Richness >= 0) %$% #escolher a partir dos dados (relação de alt com as anos)
  cor.test(Richness, Sampling.Effort) # 0- baixa xorrelação / 1- alta correlação

```


#### Testes: Carregar e inserir imagem de fundo
pacman::p_load(png,ggpubr) #colocar a imagem
img <- readPNG("/home/victor/Área de Trabalho/Bio/projeto_publ/2020_altitude/montanha.png")
grid::grid.raster(img)

violin <- ggplot(planilhatotal, aes(x=Locality, y=Altitude)) + #violino, não deu o marginal
  background_image(img) +
  geom_violin() +
  geom_jitter(aes(color=Data) , size=2, width=0.3) +
  #geom_boxplot(width=0.1) +
  labs(title="Elevational Gradient",x="Locality", y = "Elevational")
violin + theme_classic()

violin <- ggplot(planilhatotal, aes(x=Locality, y=Altitude, color=Data)) + #violino, não deu o marginal
  background_image(img) +
  geom_point(alpha=0.5) +
  #geom_boxplot(width=0.1) +
  labs(title="Abundância por altitude",x="Ordem", y = "Altitude")
violin + theme_classic()

violin <- ggplot(planilhatotal, aes(x=Locality, y=Altitude, color=Group)) + #violino, não deu o marginal
  background_image(img) +
  geom_violin(alpha=0.5,
             size=6) +
  #geom_boxplot(width=0.1) +
  labs(title="Abundância por altitude",x="Ordem", y = "Altitude")
violin + theme_classic()

mean_wt <- data.frame(nome = c(1, 2, 3, 4, 5), faixa = c(500, 1000, 1500, 2000, 2500))

violin <- ggplot(planilhatotal, aes(x= Class, y=Altitude, color= Ecoregion, fill= Ecoregion)) + #violino, não deu o marginal
  geom_violin(alpha=0.5) +
  geom_hline(yintercept = 500, alpha=0.5) + geom_hline(yintercept = 1000, alpha=0.5) + geom_hline(yintercept = 1500, alpha=0.5) + 
  geom_hline(yintercept = 2000, alpha=0.5) +  geom_hline(yintercept = 2500, alpha=0.5) +
  facet_grid(.~Group) +
  labs(title="Elevational gradient for group, ecoregion and bands",x="Order", y = "Altitude")
violin + theme(legend.position="bottom") #trocar as espécies por números e colocar os nomes na legenda

#salvar imagem
#violin + theme_classic() +
#ggsave("violin2.png",width = 6, height = 4, dpi = 300)

###Plano Cartesiano
ggplot(planilhatotal, aes(x = Locality, y = Altitude, colour=Extrato)) + #plot de comparação catálogo com altitude
  geom_point(size=4)

ggplot(planilhatotal, aes(x = CodSp, y = Altitude, colour=Espécie)) + #escolher parâmetros
  geom_point(size=4,
             alpha=0.5) + #escolher tipo de plotage
  theme(legend.position = "bottom") + #tema do pacote
  facet_wrap(~Locality, ncol=3) #facetar o gráfico para finalmente plotar
#?theme
# Um scatterplot básico, onde a cor depende da Species
ggplot(planilhatotal, aes(x=CodSp, y=Altitude, color=Locality)) +
  geom_point(size=6)

# Vários
ggplot(planilhatotal, aes(x=CodSp, y=Altitude, 
                 shape=Family,
                 color=Espécie)) +
  geom_point(size=6,
             alpha=0.15)
             
### Pontos conectados
pacman::p_load("dplyr")

planilhatotal %>%
  arrange(desc(Altitude)) %>% #organizador
  ggplot(aes(x=Extrato, y=Altitude, colour = Order)) +
  geom_line(alpha=0.5) +
  geom_point(size=3) +
  ggtitle("Gradiente Altitudinal")

### Bolhas
# Size
ggplot(planilhatotal, aes(x=CodSp, y=Altitude, size=Espécie, color = Order)) +
  geom_point(alpha=0.7)

#options(scipen = 10) 
planilhatotal %>%
  arrange(order(Altitude)) %>%
  mutate(CatalogNumber = log(CatalogNumber)) %>% #Fazer testes com o vegan
  ggplot(aes(x = CodSp, y = Altitude, color = Species, shape = Locality, size = CodSp)) + #size colocar outro dado
  geom_point(alpha = 0.5) + #não está funcionando, precisa do mutate
  scale_size(range = c(.1, 18), name = "Population (M)")

#exemplo professor, problema com o tema library(hrbrthemes) não funciona
pacman::p_load(viridis, tidyverse)

planilhatotal %>%
  arrange(desc(Altitude)) %>%
  mutate(Altitude = factor(Altitude)) %>% #mais testes
  ggplot(aes(x=CodSp, y=Altitude, size=CatalogNumber, fill=Espécie)) +
  geom_point(alpha=0.2, shape=21, color="black") +
  scale_size(range = c(.1, 24), name="Altitude") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  #theme_ipsum() + #pacote não funciona
  theme(legend.position="bottom") +
  ylab("Faixa Altitudinal") +
  xlab("Altitude") + 
  theme(legend.position = "bottom") #pra ter legenda

#apontar outlines
pacman::p_load(ggrepel, quietly = T)

##### Plot Marginal ######
ggplot(data=planilhatotal, aes(x=CodSp, Altitude)) +
  geom_point(size=12, alpha=0.01) +
  geom_rug(col="steelblue",alpha=0.1, size=1.5)

##### Evitar o overploting #####
#diminuindo o ponto
planilhatotal %>%
  ggplot(aes(x=Genus, y=Altitude)) +
  geom_point(color="#69b3a2", size=0.02) +
  #theme_ipsum() +
  theme(legend.position="none")

#transparencia
planilhatotal %>%
  ggplot( aes(x=Extrato, y=Altitude)) +
  geom_point(color="#69b3a2", size=4, alpha=0.01) +
  #theme_ipsum() +
  theme(
    legend.position="none"
  )

#densidade 2D
ggplot(planilhatotal, aes(x=Extrato, y=Altitude) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis() +
  theme(legend.position='none')

#Amostragem
planilhatotal %>%
  sample_frac(0.05) %>% #5% da plotagem
  ggplot(aes(x=Extrato, y=Altitude)) +
  geom_point(color="#69b3a2", size=2) +
  #theme_ipsum() +
  theme(legend.position="none")

#Destacando um grupo
planilhatotal %>%
  ggplot(aes(x=Locality, y=Altitude)) +
  geom_point(color="grey", size=2) +
  geom_point(data = planilhatotal %>% filter(Order=="Didelphimorphia"), color="#69b3a2", size=2) +
  #theme_ipsum() +
  theme(legend.position="none", plot.title = element_text(size=12)) +
  ggtitle('Behavior of the group B')

planilhatotal %>%
  ggplot( aes(x=Locality, y=Altitude, color=Family)) +
  geom_point( size=2, alpha=0.1) +
  scale_color_viridis(discrete=TRUE) #+
#theme_ipsum()

#Facetagem NÃO CONSEGUI
data2 <- data %>% select(-group) #exemplo, não consegui
data2 <- data[,-3]

data2 <- planilhatotal %>% 
  select(Order="Rodentia") #escolhi só os Rodentia

planilhatotal %>%
  ggplot(aes(x=Extrato, y=Altitude)) +
  geom_point(data=data2, size=1, alpha=0.05, color="grey") +
  geom_point(aes(color=Locality) , size=2, alpha=0.1) +
  scale_color_viridis(discrete=TRUE) +
  #theme_ipsum() +
  theme(legend.position="none") +
  facet_wrap(~Locality)

#Tremer os dados (jitter)
p1 <- planilhatotal %>%
  ggplot(aes(x=Locality, y=Altitude)) +
  geom_point(aes(color=Altitude) , size=2, alpha=0.2) +
  scale_color_viridis() +
  #theme_ipsum() +
  theme(legend.position="none")
p1
# tREMIDA
p2 <- planilhatotal %>%
  ggplot(aes(x=Locality, y=Altitude)) +
  geom_jitter(aes(color=Extrato) , size=2, alpha=0.2, width=0.3) +
  scale_color_viridis() +
  #theme_ipsum() +
  theme(legend.position="none")
p2 

###### Outros ######
p<-poolaccum(altitude, permutations = 50)
p
p.plot<-plot(p, display = c("chao", "jack1", "jack2"))
p.plot
chao <- data.frame(summary(p)$chao,check.names = FALSE)
colnames(chao) <- c("N", "Chao", "lower2.5", "higher97.5", "std")
chao_melt <- melt(chao, id.vars = c("N","std"))
ggplot(data = chao_melt, aes(x = N, y = value, group = variable)) +
  geom_line(aes(color = variable))


#gráfico
pacman::p_load("agricolae")
anova_result <- aov(Locality ~ H, local)
summary(anova_result)
tukey_result <- HSD.test(anova_result, "Altitude", group = TRUE)
print(tukey_result)

group_data <- tukey_result$groups[order(rownames(tukey_result$groups)),]

ggplot(planilhatotal, aes(x = , y = Extrato1)) +
  geom_text(data = data.frame(),
            aes(x = rownames(group_data), y = max(planilhatotal$Extrato1) + 1, label = group_data$groups),
            col = 'black',
            size = 10) +
  geom_boxplot() +
  ggtitle("Alpha diversity") +
  xlab("Site") +
  ylab("Alpha diversity index")

###### Graphis paper ####

