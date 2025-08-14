# 1. Início
Primeiro, vamos indicar as pastas corretas.
- Esse diretório é do meu PV, Vitãp.
```
getwd()
setwd("/home/kaetes/Área de trabalho/PCSA/PCSA/Pessoas/2025_Victor Campos") 
```
Agora os principais pacotes utilizados:
```
# Verifica se o pacote pacman está instalado; se não, instala
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  magrittr, dplyr, reshape2,vegan,       # Manipulação de dados
  ggplot2, ggrepel, graphics, ggsave,  # Visualização de dados
  lubridate, stringr               # Manipulação de datas e strings
)
```

#2) Dados da Planilha

Como enviar dados para a panilha. Irei manter aqui o seu comando
```
dados <- read_excel("comparativo_kaetes.xlsx", sheet = 1)

```
Aqui vou colocar o meu comando de drive, blz
```
# Carregar pacotes necessários
pacman::p_load(googledrive, googlesheets4, readxl, dplyr)

# Autenticar no Google Drive
drive_auth()

# 1. Acessar a pasta pelo ID
pasta_id <- "1dgXN3Wl--Qdz751iyWxatY3OQgXTvui3"
pasta <- drive_get(as_id(pasta_id))

# 2. Listar os arquivos na pasta
arquivos_na_pasta <- drive_ls(pasta)

# 3. Filtrar pelo nome do arquivo
arquivo <- arquivos_na_pasta %>% filter(name == "COMPARATIVO RPPN KAETES - PEFG - PEPA.xlsx")

# 4. Verificar se encontrou exatamente um arquivo
if (nrow(arquivo) == 1) {
  # Baixar o arquivo
  drive_download(file = as_id(arquivo$id), path = "COMPARATIVO RPPN KAETES - PEFG - PEPA.xlsx", overwrite = TRUE)
  message("Arquivo baixado com sucesso!")

  # 5. Ler o arquivo Excel e corrigir nomes de colunas
  planilhatotal <- read_excel("COMPARATIVO RPPN KAETES - PEFG - PEPA.xlsx", .name_repair = "minimal")

  # Exibir os nomes das colunas para verificação
  print(names(planilhatotal))

  message("Arquivo lido com sucesso!")

} else if (nrow(arquivo) == 0) {
  stop("Erro: Arquivo 'COMPARATIVO RPPN KAETES - PEFG - PEPA.xlsx' não encontrado na pasta.")
} else {
  stop("Erro: Mais de um arquivo com o mesmo nome encontrado. Verifique manualmente.")
}

```
#3) Análise

#Vamos limpas

Agora vamos filtrar a tabela. Primeiro tirar os dias não amostrados e espécie exótica.
```
p1 <- subset(planilhatotal, !is.na(Espécie))
p2 <- p1
```
Vamos calcular, não temos data, então vou calcular a amostragem por Ordem
```
acum<-reshape2::dcast(p2, Ordem ~ Espécie)
acum=data.frame(acum, row.names=1)

acumplot<-specaccum(acum) #dados de acumulação
plot(acumplot) #curva simples
plot(acumplot,ci.type="poly",col="black",lwd=2,ci.lty=0,
     ci.col="lightgrey",ylab="Riqueza",
     xlab="Número de ordens registradas",main="Acumulado de espécies por Ordem",las=1,
     font=1.5,font.lab=1.5,cex.lab=1,cex.axis=1) #curva clássica

```
# 4. Diversidade
Vamos plotar uma imagem para a diversidade

```
local<-reshape2::dcast(p2, Família ~ Espécie)
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
local<-reshape2::dcast(p2, Família + Ordem ~ Espécie)
local<-data.frame(S, spAbund, shannon,J, local)
```
E agora o gráfico. Lembrar de verificar:
- Se a variável está em colour de geom_point;
- No subtitle de labs.
- No caso de família, trocar o y para S e x para Família.

Um outro exemplo de gráfico é um de barras:
```
ggplot(local, aes(x = reorder(Família, S), y = S)) + 
  geom_col(aes(weight = S, fill = Ordem), alpha = 0.7) + 
  #geom_point(aes(y = S, x = Família, size = spAbund, colour = Ordem), alpha = 0.7) +
  geom_label(aes(y = S, x = Família, label = S), size=4, alpha= 1) +
  #facet_grid(Ordem~., scales = "free_y", space = "free_y") + 
  labs(title="Riqueza e diversidade", subtitle="Diversidade", y="Riqueza", x="Família", caption="Dados primários", fill = "Ordem", colour = "Ordem", size = "Riqueza") +
  scale_size_binned(range = c(.1, 18)) +
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 14)) + 
        coord_flip() + 
        #scale_color_tq() + scale_fill_tq() +
        theme_tq()         
        
#ggsave("2025_diversidade.pdf",width = 10, height = 8, dpi = 600)
