# Carregar pacotes necessários
library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(ggplot2)
library(viridis) 
library(showtext)
library(wordcloud)
library(RColorBrewer)
library(snakecase)
library(patchwork)

# Limpar ambiente e definir diretório
rm(list = ls())
setwd(".")
font_add("Verdana", regular = "C:/Windows/Fonts/Verdana.ttf") 

# Ler o arquivo JSON
dados_brutos <- fromJSON("dados_spell.json")

dados <- dados_brutos %>%
  mutate(
    revista = str_trim(str_extract(fonte, "^[^,]+")),  # tudo até a primeira vírgula
    ano = str_extract(fonte, "\\b(19|20)\\d{2}\\b")     # primeiro ano de 4 dígitos que encontrar
  )

revista_ano_resumo <- dados %>%
  filter(!is.na(revista) & !is.na(ano)) %>%
  group_by(revista, ano) %>%
  summarise(qtd_artigos = n(), .groups = "drop") %>%
  arrange(desc(qtd_artigos), revista, ano)

revista_totais <- revista_ano_resumo %>%
  group_by(revista) %>%
  summarise(
    total_artigos = sum(qtd_artigos, na.rm = TRUE)
  ) %>%
  arrange(desc(total_artigos))

ano_totais <- revista_ano_resumo %>%
  group_by(ano) %>%
  summarise(
    total_artigos = sum(qtd_artigos, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    porcentagem = round(100 * total_artigos / sum(total_artigos), 2)
  ) %>%
  arrange(desc(ano))


head(dados %>% select(fonte, revista, ano), 20)


# 1. Selecionar as 10 revistas com maior número total de publicações
top10_revistas <- revista_ano_resumo %>%
  group_by(revista) %>%
  summarise(qtd_total = sum(qtd_artigos), .groups = "drop") %>%
  arrange(desc(qtd_total)) %>%
  slice_head(n = 5)

# 2. Filtrar os dados apenas para essas revistas
dados_top10 <- revista_ano_resumo %>%
  filter(revista %in% top10_revistas$revista)


dados_top10$revista <- str_wrap(dados_top10$revista, width = 20)


# 3. Gráfico de linhas ano a ano
grafico <- ggplot(dados_top10, aes(x = as.integer(ano), y = qtd_artigos, color = revista, group = revista)) +
  geom_smooth(method = "loess", span = 0.75, se = FALSE) +
  geom_point(size = 2) +
  geom_text(aes(label = qtd_artigos), vjust = -0.8, size = 3) +
  scale_color_brewer(palette = "Set2") +
  labs(
    x = "Ano de Publicação",
    y = "Quantidade de Artigos",
    color = "Revista",
    caption = "Fonte: Elaboração própria com base na base SPELL (2025)."
  ) +
  theme_minimal(base_family = "Arial") +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.spacing.y = unit(10, "pt"),
    legend.text = element_text(size = 9),
    plot.caption = element_text(hjust = 0, size = 10),
    plot.caption.position = "plot"
  )

plot_final <- wrap_elements(grafico) +
  plot_annotation(
    title = "Figura 1 – Evolução anual das 10 revistas com maior número de publicações sobre \nTransformação Digital (2010–2025)",
    theme = theme(
      plot.title = element_text(hjust = 0.5, family = "Times New Roman", size = 14, face = "bold")
    )
  )

plot_final


ggplot(dados_top10, aes(x = as.integer(ano), y = qtd_artigos, color = revista, group = revista)) +
  geom_smooth(method = "loess", span = 0.75, se = FALSE) +
  geom_point(size = 2) +
  geom_text(aes(label = qtd_artigos), vjust = -0.8, size = 3) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Figura 1 – Evolução anual das 10 revistas com maior número de \npublicações sobre Transformação Digital (2010–2025)",
    x = "Ano de Publicação",
    y = "Quantidade de Artigos",
    color = "Revista",
    caption = "Fonte: Elaboração própria com base na base SPELL (2025)."
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    legend.position = "right",
    legend.box = "vertical",
    legend.spacing.y = unit(12, "pt"),  # espaço vertical entre os itens
    legend.text = element_text(size = 9),
    plot.caption = element_text(hjust = 0, size = 10),
    plot.caption.position = "plot"
  )

list <- system_fonts()

distinct_revistas <- dados_top10 %>%
  distinct(revista)

###########
write.csv(revista_totais, "revista_totais.csv", row.names = TRUE)

install.packages("flextable")
library(flextable)
library(officer)

borda_preta <- fp_border(color = "black", width = 1)

ft <- flextable(head(revista_totais, 12)) %>%
  set_caption("Tabela 1: 12 revistas com mais publicações sobre Transformação Digital") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 12, part = "all") %>%
  align(align = "center", part = "all") %>%
  border_outer(border = borda_preta) %>%    # bordas externas
  border_inner(border = borda_preta) %>%    # bordas internas
  add_footer_lines("Fonte: Dados elaborados pelo autor (2025)") %>%
  align(align = "left", part = "footer")

doc <- read_docx() %>%
  body_add_par(" ") %>%
  body_add_flextable(ft)

print(doc, target = "revista_totais.docx")




