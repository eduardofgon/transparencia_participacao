library(betareg)
library(broom)
library(purrr)
library(DT)
library(tidyverse)
library(gt)

# Dados

load("dados.RData")

options(scipen = 999)

############################

var_resp <- c("tx_comparecimento", "tx_votos_validos")

estr_preditores <- list(simples = "nota_transparencia",
                        intermediario = c("nota_transparencia", "area_log", "populacao_log",
                                          "idhm", "tx_idosos_60mais", "salario_medio",
                                          "tx_desocup", "anos_estudo", "anos_estudo_2"),
                        completo = c("nota_transparencia", "area_log", "populacao_log",
                                     "idhm", "tx_idosos_60mais", "salario_medio",
                                     "tx_desocup", "anos_estudo", "anos_estudo_2",
                                     "distancia", "competitividade", "tx_filiacao"))

fit_models <- function(formula, data) {
  list(ols = lm(formula, data = data),
       frm = glm(formula, family = quasibinomial(link = "probit"), data = data),
       beta = betareg::betareg(formula, data = data))}

formulas <- lapply(var_resp, function(resp) {
  lapply(estr_preditores, function(pred_set) {
    as.formula(paste(resp, "~", paste(pred_set, collapse = " + ")))}) %>%
      setNames(names(estr_preditores))}) %>%
      setNames(var_resp)

modelos <- lapply(formulas, function(resp_forms) {
  lapply(resp_forms, fit_models, data = dados)})

################################ TABELA COEFICIENTES

# Função extrair coeficientes e p-valores dos modelos
extrair_coefs <- function(model) {
  if (inherits(model, "lm") && !inherits(model, "glm")) {
    tidy(model) %>%
      mutate(p.value = ifelse(p.value < 0.001, "<0.001", as.character(round(p.value, 3))))
  } else if (inherits(model, "glm")) {
    tidy(model) %>%
      mutate(p.value = ifelse(p.value < 0.001, "<0.001", as.character(round(p.value, 3))))
  } else if (inherits(model, "betareg")) {
    tidy(model) %>%
      filter(component == "mean") %>%  # Foco nos coeficientes da média
      mutate(p.value = ifelse(p.value < 0.001, "<0.001", as.character(round(p.value, 3)))) %>%
      select(-component)}}

# Processar todos os modelos e criar uma tabela consolidada
tabela_result <- bind_rows(lapply(names(modelos), function(resp_var) {
  bind_rows(lapply(names(modelos[[resp_var]]), function(model_set) {
    bind_rows(lapply(names(modelos[[resp_var]][[model_set]]), function(model_type) {
      extrair_coefs(modelos[[resp_var]][[model_set]][[model_type]]) %>%
        mutate(resposta = resp_var,
               modelo = model_set,
               metodo = model_type,
               variaveis = term) %>%
        select(resposta, modelo, metodo, variaveis, estimate, p.value) %>%
        filter(variaveis != "(Intercept)")}))}))}))

# Criar tabela formatada
tabela_formatada <- tabela_result %>%
  mutate(Coef_Sig = ifelse(
         p.value == "<0.001",
         sprintf("%.5f***", estimate),
         ifelse(p.value < 0.01,
                sprintf("%.5f**", estimate),
                ifelse(p.value < 0.05,
                       sprintf("%.5f*", estimate),
                       sprintf("%.5f", estimate))))) %>%
  select(-estimate, -p.value) %>%
  pivot_wider(names_from = c(modelo, metodo),
              values_from = Coef_Sig,
              names_sep = ".")

# Criar tabela visual com gt
tabela_gt <- tabela_formatada %>%
  gt(groupname_col = "resposta") %>%
  tab_spanner_delim(delim = ".") %>%
  cols_label(variaveis = "Variável") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_row_groups()) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels()) %>%
  tab_options(table.width = pct(100),
              table.background.color = "white")

# Salvar como PNG

if (!dir.exists("imagens")) {
       dir.create("imagens")}

gtsave(tabela_gt,
       filename = "imagens/coef_modelos.png",
       path = getwd(),
       vwidth = 1600,
       vheight = 1200,
       zoom = 2)
