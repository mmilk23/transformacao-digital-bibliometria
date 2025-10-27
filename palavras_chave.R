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



# Limpar ambiente e definir diretório
rm(list = ls())
setwd("C:/users/Marcelo/Desktop/MUST/CAPSTONE/repo")
font_add("Verdana", regular = "C:/Windows/Fonts/verdana.ttf") 

# Ler o arquivo JSON
dados <- fromJSON("dados_spell.json")

# Separar e limpar palavras-chave
palavras <- dados %>%
  filter(palavras_chave != "") %>%
  mutate(id = as.character(id)) %>%
  separate_rows(palavras_chave, sep = ",\\s*") %>%
  mutate(
    palavra_limpa = palavras_chave %>%
      str_to_lower() %>%
      stri_trans_general("Latin-ASCII") %>%
      str_trim()
  ) %>%
  filter(
    palavra_limpa != "",
    !palavra_limpa %in% c("ver resumo", "verresumo", "ver", "resumo", "artigo", "portugues", "transformacao digital", "digital transformation", "0")
  )

# Criar dicionário de forma original para exibição
dicionario <- palavras %>%
  count(palavra_limpa, palavras_chave) %>%
  group_by(palavra_limpa) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(palavra_limpa, palavra_original = palavras_chave)

# Total de artigos com palavras-chave válidas
total_artigos_validos <- palavras %>%
  distinct(id) %>%
  nrow()

# Total de palavras-chave válidas
total_palavras_validas <- nrow(palavras)

# Agrupar, juntar com o dicionário e calcular métricas
resultado <- palavras %>%
  count(palavra_limpa, sort = TRUE) %>%
  left_join(dicionario, by = "palavra_limpa") %>%
  mutate(
    porcentagem_por_artigo = round(100 * n / total_artigos_validos, 2),
    porcentagem_por_palavra = round(100 * n / total_palavras_validas, 2)
  )

# Adicionar linha com totais
resumo_total <- resultado %>%
  summarise(
    palavra_limpa = "TOTAL",
    palavra_original = "TOTAL",
    n = sum(n),
    porcentagem_por_artigo = sum(porcentagem_por_artigo),
    porcentagem_por_palavra = sum(porcentagem_por_palavra)
  )

resultado_com_total <- bind_rows(resultado, resumo_total)

# Visualização (tabela)
print(resultado_com_total)

# Gráfico: 20 palavras-chave mais frequentes com acento restaurado
top_palavras <- resultado %>%
  arrange(desc(porcentagem_por_artigo), desc(n)) %>%
  head(15)

top_palavras <- top_palavras %>%
  mutate(palavra_label = to_title_case(palavra_original))

# Criar fator com base na porcentagem
top_palavras <- top_palavras %>%
  mutate(grupo_porcentagem = factor(porcentagem_por_artigo))


write.csv(top_palavras, "top_palavras.csv", row.names = TRUE)


##GRAFICO AZUL
ggplot(top_palavras, aes(x = reorder(palavra_label, porcentagem_por_artigo), y = porcentagem_por_artigo)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(porcentagem_por_artigo, "%")),
            hjust = -0.1,
            size = 4) +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "15 Palavras-chave mais presentes nos artigos (sem 'transformação digital')",
    x = "Palavra-chave",
    y = "Porcentagem de artigos"
  ) +
  theme_minimal()


write.csv(top_palavras, "top_palavras.csv", row.names = TRUE)


# Gráfico de barras colorido
ggplot(top_palavras, aes(
  x = reorder(palavra_label, porcentagem_por_artigo),
  y = porcentagem_por_artigo,
  fill = grupo_porcentagem     # cor com base na porcentagem!
)) +
  geom_col() +
  geom_text(aes(label = paste0(porcentagem_por_artigo, "%")),
            hjust = -0.1,
            size = 4,
            color = "black") +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.15))) +

  scale_fill_brewer(palette = "Set2", direction = -1) +

  labs(
    title = "Figura 2 – As 15 palavras-chave mais frequentes nos artigos sobre\nTransformação Digital (2010–2025)",
    x = "Palavra-chave",
    y = "Porcentagem de artigos",
    fill = "Porcentagem",
    caption = "Fonte: Elaboração própria com base na base SPELL (2025)."
  ) +
  theme_minimal() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, size = 13),
        legend.text = element_text(size = 9),
        plot.caption = element_text(hjust = 0, size = 10),
        plot.caption.position = "plot")


#nuvem de palavras

# Usar o mesmo dataframe `resultado`, mas com rótulos bonitos
palavras_freq <- resultado %>%
  filter(palavra_limpa != "total") %>%
  mutate(palavra_label = to_title_case(palavra_original)) %>%
  select(palavra_label, n)

# Gerar nuvem de palavras
wordcloud(
  words = palavras_freq$palavra_label,
  freq = palavras_freq$n,
  min.freq = 1,
  max.words = 15,
  random.order = FALSE,
  colors = brewer.pal(5, "Dark2"),
  scale = c(3.5, 0.8)
)


library(wordcloud2)
library(dplyr)
library(snakecase)

# Preparar os dados (sem "TOTAL")
dados_nuvem <- resultado %>%
  filter(palavra_limpa != "total") %>%
  mutate(palavra_label = to_title_case(palavra_original)) %>%
  arrange(desc(n)) %>%
  head(15) %>%
  select(word = palavra_label, freq = n)

# Gerar nuvem interativa
wordcloud2(data = dados_nuvem, size = 1.0, color = "random-dark", shape = "cardioid", elliptic = TRUE, fontWeight = "normal")

