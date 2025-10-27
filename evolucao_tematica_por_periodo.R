# ------------------------------------------------------------
# Evolução temática por períodos (2010–2025) – SPELL
# Entrada: resultado_spell_atualizado1025.json
# Saídas: PNG e CSVs auxiliares
# ------------------------------------------------------------

# Pacotes
required <- c("jsonlite","dplyr","tidyr","stringi","stringr","ggplot2","forcats","readr","RColorBrewer")
new_pkgs <- required[!(required %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)
lapply(required, library, character.only = TRUE)

# --------------------------
# Parâmetros
# --------------------------
INPUT_FILE <- "dados_spell.json"
TOP_N      <- 15
SAVE_CSV   <- TRUE
SAVE_PNG   <- TRUE

# --------------------------
# 1) Carrega e extrai ano
# --------------------------
dat <- jsonlite::fromJSON(INPUT_FILE, flatten = TRUE)

needed <- c("id","titulo","autoria","fonte","palavras_chave")
miss <- setdiff(needed, names(dat))
if (length(miss)) stop(paste("Faltam campos no JSON:", paste(miss, collapse=", ")))

extract_year <- function(x) {
  y <- stringr::str_extract(x, "(19|20)\\d{2}")
  suppressWarnings(as.integer(y))
}

dat <- dat %>%
  mutate(ano = extract_year(fonte)) %>%
  filter(!is.na(ano), ano >= 2010, ano <= 2025)

# --------------------------
# 2) Períodos
# --------------------------
periodo_breaks <- c(2010, 2014, 2018, 2022, 2026)
periodo_labels <- c("2010–2013","2014–2017","2018–2021","2022–2025")

dat <- dat %>%
  mutate(periodo = cut(ano, breaks = periodo_breaks, labels = periodo_labels, right = FALSE))

# --------------------------
# 3) Keywords: explode, normaliza e filtra termos
# --------------------------
keywords <- dat %>%
  select(id, periodo, palavras_chave) %>%
  mutate(palavras_chave = ifelse(is.na(palavras_chave), "", palavras_chave),
         palavras_chave = str_replace_all(palavras_chave, "\\s*;\\s*", ", ")) %>%
  separate_rows(palavras_chave, sep = "\\s*,\\s*") %>%
  mutate(term_raw = trimws(palavras_chave)) %>%
  filter(term_raw != "")

normalize_term <- function(term) {
  t <- tolower(term)
  t <- stringi::stri_trans_general(t, "Latin-ASCII")
  t <- str_squish(t)
  # normalizações úteis
  t <- str_replace_all(t, "^industria 4\\.0$", "industria 4.0")
  t <- str_replace_all(t, "^transformacao digital$", "transformação digital")
  t <- str_replace_all(t, "^gestao do conhecimento$", "gestao do conhecimento")
  t <- str_replace_all(t, "^mudanca organizacional$", "mudanca organizacional")
  t <- str_replace_all(t, "^tecnologia da informacao$|^ti$", "tecnologia da informacao")
  t <- str_replace_all(t, "^estrategia digital$", "estrategia digital")
  t <- str_replace_all(t, "^capacidade(s)? dinamica(s)?$", "capacidades dinamicas")
  t
}

# termos a excluir da análise (pedido do usuário)
banned_terms <- c("0", "bibliometria")

keywords <- keywords %>%
  mutate(term_norm = normalize_term(term_raw)) %>%
  filter(!is.na(term_norm), term_norm != "", !(term_norm %in% banned_terms))

# --------------------------
# 4) Frequências por período e Top N
# --------------------------
freq_periodo <- keywords %>%
  filter(!is.na(periodo)) %>%
  group_by(periodo, term_norm) %>%
  summarise(n = n(), .groups = "drop")

freq_total <- freq_periodo %>%
  group_by(term_norm) %>%
  summarise(n_total = sum(n), .groups = "drop") %>%
  arrange(desc(n_total))

top_terms <- freq_total %>% slice_head(n = TOP_N) %>% pull(term_norm)

freq_top <- freq_periodo %>%
  filter(term_norm %in% top_terms) %>%
  mutate(periodo = factor(periodo, levels = periodo_labels)) %>%
  arrange(term_norm, periodo)

# --------------------------
# 5) Gráfico com CORES (paleta Dark2)
# --------------------------
label_term <- function(t) {
  t <- gsub("\\b(4\\.0)\\b", "4.0", t)
  tools::toTitleCase(t)
}

freq_top_plot <- freq_top %>% mutate(term_label = label_term(term_norm))

p <- ggplot(freq_top_plot, aes(x = periodo, y = n, group = term_label, color = term_label)) +
  geom_line(size = 1, alpha = 0.85) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Dark2") +  # troque por "Set2" ou use viridis: scale_color_viridis_d()
  labs(
    title = "Evolução temática por períodos (palavras-chave, 2010–2025)",
    subtitle = paste0("Top ", TOP_N, " termos normalizados • Base: SPELL"),
    x = "Período",
    y = "Frequência (contagem por período)",
    caption = "Fonte: Elaboração própria com base na base SPELL (2025)."
  ) +
  facet_wrap(~ term_label, scales = "free_y") +
  theme_minimal(base_size = 11) +
  theme(
    strip.text  = element_text(face = "bold"),
    plot.title  = element_text(face = "bold"),
    legend.position = "none" # em facetas, a legenda não agrega — deixe TRUE se preferir
  )

print(p)

if (SAVE_PNG) {
  ggsave("evolucao_tematica_periodos_top_terms.png", p, width = 14, height = 9, dpi = 300)
}

# --------------------------
# 6) CSVs auxiliares
# --------------------------
if (SAVE_CSV) {
  readr::write_csv(freq_periodo, "freq_keywords_por_periodo.csv")
  readr::write_csv(freq_total,   "freq_keywords_total.csv")
  readr::write_csv(freq_top,     "freq_keywords_top_por_periodo.csv")
}

# --------------------------
# 7) Sumário no console
# --------------------------
message("\nResumo — Top termos (total no período):")
print(freq_top %>% group_by(term_norm) %>% summarise(total = sum(n)) %>% arrange(desc(total)) %>% head(10))
message("\nArquivos gerados:")
message("- evolucao_tematica_periodos_top_terms.png")
message("- freq_keywords_por_periodo.csv")
message("- freq_keywords_total.csv")
message("- freq_keywords_top_por_periodo.csv")
