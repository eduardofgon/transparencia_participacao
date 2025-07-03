# Pacotes

library(tidyverse)
library(magrittr)
library(readxl)
library(stringr)
library(fuzzyjoin)
library(Hmisc)
library(skimr)

# Funções

normalizar_municipio_uf <- function(mun_col) {
    mun_col %>%
    str_trim() %>%
    str_replace_all("\\s*\\(([A-Za-z]{2})\\)", "/\\1") %>% # Parênteses (UF)
    str_replace_all("\\s*-\\s*([A-Za-z]{2})\\s*$", "/\\1") %>% # Hífens antes da UF
    str_replace_all("\\s*/\\s*", "/") %>% # Remove espaços em torno da barra
    toupper()
}

unificar_municipio_uf <- function(x, y, z) {
    resultado_unif <- stringdist_join(x,
                                      y,
                                      by = "municipio_uf",
                                      method = "jw",  # Jaro-Winkler
                                      max_dist = z,  # Intervalo de similaridade
                                      distance_col = "dist",
                                      mode = "left")

    tabela_controle <- resultado_unif %>%
        select(municipio_uf.x,
               municipio_uf.y,
               dist) %>%
        group_by(municipio_uf.x) %>%
        slice_min(dist, n = 1) %>%
        ungroup() %>%
        filter(dist != 0 | is.na(dist))

    resultado_unif %<>%
        group_by(municipio_uf.x) %>%
        slice_min(dist, n = 1) %>%
        ungroup() %>%
        select(-contains(".y") & -dist) %>%
        rename_with(~ gsub("\\.x", "", .),
                    .cols = contains(".x"))

    if (nrow(tabela_controle) > 0) {
        message("\033[31m",  # Inicia cor vermelha
                "    ---------ATENÇÃO: ",
                nrow(tabela_controle),
                " registros não exatos!---------\n",
                "---------Faça as modificações que achar necessário---------\n",
                "\033[0m")
        print(as.data.frame(tabela_controle))
    } else {
        message("\033[32m",  # Inicia cor verde
                "✓ Todos os registros foram exatos.\n",
                "\033[0m")
    }

    return(resultado_unif)
}

# Datasets

arquivo_area <- read_excel("Fontes/AR_BR_RG_UF_RGINT_RGIM_MES_MIC_MUN_2020.xls")

arquivo_idhm <- read_excel("Fontes/data.xlsx")

arquivo_ebt_360_2 <- read_excel("Fontes/EBT___Avaliacao_360____2__Edicao.xls")

arquivo_salario <- read_excel("Fontes/Tabela 1685.xlsx")

arquivo_desocup <- read_excel("Fontes/Tabela 4562.xlsx")

arquivo_pop <- read_excel("Fontes/Tabela 6579_2020.xlsx")

arquivo_anos_estudo <- read_excel("Fontes/Tabela 7126.xlsx")

arquivo_idosos <- read_excel("Fontes/Tabela 9514.xlsx")

arquivo_votos <- read.csv2("Fontes/detalhe_votacao_munzona_2020_BRASIL.csv",
                           fileEncoding = "latin1")

arquivo_cand <- read.csv2("Fontes/votacao_candidato.csv",
                          fileEncoding = "latin1")

arquivo_filiacao <- read.csv2("Fontes/filiacao-2020_pais.csv",
                              fileEncoding = "latin1")

# Manipulações

dados_area <- arquivo_area %>%
    select(3:7) %>%
    set_names(c("uf_nome", "uf_sigla", "cod_ibge", "municipio", "area")) %>%
    filter(!is.na(cod_ibge) & !cod_ibge %in% c("4300001", "4300002")) %>%
    mutate(municipio_uf = paste0(municipio, "/", uf_sigla))

dados_idhm <- arquivo_idhm %>%
    select(1, 3) %>%
    set_names(c("municipio_uf", "idhm")) %>%
    mutate(municipio_uf = normalizar_municipio_uf(municipio_uf))

dados_ebt_360_2 <- arquivo_ebt_360_2 %>%
    select(2:5) %>%
    set_names(c("nota_transparencia", "uf_nome", "municipio_uf", "cod_ibge")) %>%
    mutate(nota_transparencia = as.numeric(nota_transparencia)/10) %>%
    filter(!is.na(cod_ibge) & municipio_uf != "Brasília - DF")

dados_salario <- arquivo_salario %>%
    slice(c(4:5573)) %>%
    set_names(c("municipio_uf", "salario_medio")) %>%
    mutate(municipio_uf = normalizar_municipio_uf(municipio_uf))

dados_desocup <- arquivo_desocup %>%
    slice(c(4:30)) %>%
    set_names(c("uf_nome", "tx_desocup")) %>%
    mutate(tx_desocup = tx_desocup/100)

dados_pop <- arquivo_pop %>%
    slice(c(32:5601)) %>%
    set_names(c("municipio_uf", "populacao")) %>%
    mutate(municipio_uf = normalizar_municipio_uf(municipio_uf))

dados_anos_estudo <- arquivo_anos_estudo %>%
    slice(c(6:32)) %>%
    set_names(c("uf_nome", "anos_estudo"))

dados_idosos <- arquivo_idosos %>%
    slice(c(7:5576)) %>%
    mutate(across(c(2:11), ~if_else(. == "-", "0", .))) %>% 
    mutate(across(c(2:11), as.numeric)) %>%
    mutate(tx_idosos_60mais = rowSums(select(., 3:11)) / .[[2]]) %>%
    select(1, 12) %>%
    rename("municipio_uf" = 1) %>%
    mutate(municipio_uf = normalizar_municipio_uf(municipio_uf))

dados_votos <- arquivo_votos %>%
    rename("uf_sigla" = "SG_UF",
           "cargo" = "DS_CARGO",
           "municipio" = "NM_MUNICIPIO",
           "tipo_eleicao" = "NM_TIPO_ELEICAO",
           "turno" = "NR_TURNO",
           "aptos" = "QT_APTOS",
           "comparecimento" = "QT_COMPARECIMENTO",
           "votos_validos" = "QT_VOTOS_VALIDOS") %>%
    filter(cargo == "Prefeito") %>%
    filter(turno == 1) %>%
    filter(tipo_eleicao == "Eleição Ordinária") %>%
    group_by(uf_sigla, municipio) %>%
    summarise(aptos = sum(aptos),
              comparecimento = sum(comparecimento),
              votos_validos = sum(votos_validos)) %>%
    ungroup() %>%
    mutate(tx_comparecimento = comparecimento/aptos,
           tx_votos_validos = votos_validos/aptos,
           municipio_uf = paste0(municipio, "/", uf_sigla)) %>%
    select(-c(3:5))

dados_cand <- arquivo_cand %>%
    rename("uf_sigla" = "UF",
           "cargo" = "Cargo",
           "municipio" = "Município",
           "candidato" = "Nome.candidato",
           "situacao_tot" = "Situação.totalização",
           "turno" = "Turno",
           "votos_val" = "Votos.válidos",
           "votos_nom" = "Votos.nominais") %>%
    filter(cargo == "Prefeito") %>%
    filter(turno == 1) %>%
    group_by(uf_sigla, municipio, candidato) %>%
    summarise(votos_val = sum(votos_val),
              votos_nom = sum(votos_nom),
              situacao_tot = first(situacao_tot)) %>%
    ungroup() %>%
    mutate(tx_votos = votos_nom/votos_val) %>%
    group_by(uf_sigla, municipio) %>%
    arrange(desc(tx_votos), .by_group = TRUE)  %>%
    mutate(distancia = ifelse(row_number() == 1, tx_votos[1] - tx_votos[2], NA_real_),
           competitividade = ifelse(row_number() == 1, 1-tx_votos, NA_real_)) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    select(-c(3:7)) %>%
    mutate(municipio_uf = paste0(municipio, "/", uf_sigla)) %>%
    replace_na(list(distancia = 1))

dados_filiacao <- arquivo_filiacao %>%
    select(1:3) %>%
    set_names(c("uf_sigla", "municipio", "filiacao")) %>%
    filter(uf_sigla != "ZZ") %>%
    mutate(municipio_uf = paste0(municipio, "/", uf_sigla))

# Correções para unificações

dados_idhm$municipio_uf[dados_idhm$municipio_uf == "EMBU/SP"] <- "EMBU DAS ARTES/SP"

dados_cand$municipio_uf[dados_cand$municipio_uf == "ASSÚ/RN"] <- "AÇU/RN"

dados_filiacao$municipio_uf[dados_filiacao$municipio_uf == "ASSÚ/RN"] <- "AÇU/RN"

dados_votos$municipio_uf[dados_votos$municipio_uf == "ASSÚ/RN"] <- "AÇU/RN"

# Unificações

dados_area_ebt <- left_join(dados_ebt_360_2 %>% select(1, 4),
                            dados_area,
                            by = "cod_ibge")

dados_unif1 <- unificar_municipio_uf(dados_area_ebt, dados_filiacao, 0.05)

dados_unif2 <- unificar_municipio_uf(dados_unif1, dados_cand, 0.05)

dados_unif3 <- unificar_municipio_uf(dados_unif2, dados_idhm, 0.07)

dados_unif4 <- unificar_municipio_uf(dados_unif3, dados_idosos, 0.05)

dados_unif5 <- unificar_municipio_uf(dados_unif4, dados_pop, 0.05)

dados_unif6 <- unificar_municipio_uf(dados_unif5, dados_salario, 0.05)

dados_unif7 <- unificar_municipio_uf(dados_unif6, dados_votos, 0.08)

dados_estaduais <- inner_join(dados_anos_estudo,
                              dados_desocup,
                              by = "uf_nome") %>%
                   filter(uf_nome != "Distrito Federal")

dados <- stringdist_join(dados_estaduais,
                         dados_unif7,
                         by = "uf_nome",
                         method = "jw",  # Jaro-Winkler
                         max_dist = 0.08,  # Intervalo de similaridade
                         distance_col = "dist",
                         mode = "left") %>%
        select(-contains(".y") & -dist) %>%
        rename_with(~ gsub("\\.x", "", .),
                    .cols = contains(".x"))

# Criar últimas variáveis e selecionar dados

dados %<>% mutate(area_log = log(area),
                  populacao_log = log(populacao),
                  anos_estudo_2 = anos_estudo^2,
                  tx_filiacao = filiacao/populacao) %>%
           select(municipio_uf,
                  tx_comparecimento,
                  tx_votos_validos,
                  nota_transparencia,
                  area,
                  area_log,
                  populacao,
                  populacao_log,
                  idhm,
                  tx_idosos_60mais,
                  salario_medio,
                  tx_desocup,
                  anos_estudo,
                  anos_estudo_2,
                  distancia,
                  competitividade,
                  tx_filiacao)

# Verificar dataset

summary(dados)
describe(dados)
skim(dados)

# Salvar

save(dados, file = "dados.RData")
