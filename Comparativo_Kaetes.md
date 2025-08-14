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
library(dplyr)
library(reshape2)
library(vegan)

# 1. Criar a matriz de comunidade UC x Espécie
comm_matrix <- reshape2::dcast(p2, UC ~ Espécie, value.var = "Registro", fun.aggregate = sum)

# 2. Calcular riqueza total por UC (S_uc)
S_uc <- vegan::specnumber(comm_matrix[, -1])  # Exclui a coluna UC
names(S_uc) <- comm_matrix$UC  # Nomeia os valores com as UCs

# 3. Criar e calcular a riqueza por UC + Ordem
local_ordem <- reshape2::dcast(p2, UC + Ordem ~ Espécie, value.var = "Registro", fun.aggregate = sum)

S_ordem <- local_ordem %>% 
  group_by(UC, Ordem) %>%
  summarise(
    S = vegan::specnumber(across(where(is.numeric))),
    .groups = "drop"
  )

# 4. Juntar os dados
final_df <- S_ordem %>%
  left_join(
    data.frame(UC = names(S_uc), S_total = S_uc),
    by = "UC"
  )

# Verifique o resultado
head(final_df)
  

ggplot(final_df, aes(x = reorder(UC, S_total), y = S, fill = Ordem)) +  # Note que usamos "S" em vez de "S_por_Ordem"
  geom_col(position = "stack", width = 0.8) +
  geom_label(
    aes(label = ifelse(S > 0, S, "")),  # Aqui também usamos "S"
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.5
  ) +
  labs(
    x = "Unidade de Conservação",
    y = "Riqueza de Espécies (S)",
    fill = "Ordem Taxonômica",
    title = "Riqueza por UC e Ordem"
  ) +
  scale_fill_viridis_d() +
  theme_minimal() +
  coord_flip()
  
  
#ggsave("2025_diversidade.pdf",width = 10, height = 8, dpi = 600)
