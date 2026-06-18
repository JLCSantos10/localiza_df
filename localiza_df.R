# ============================================================================
# localiza_df.R  —  padronização de RA do DF (determinística + Machine Learning)
# ----------------------------------------------------------------------------
# Padroniza endereços/bairros (texto livre) para a Região Administrativa (RA)
# do Distrito Federal. Trabalha em DUAS camadas que se complementam:
#
#   1) MOTOR DETERMINÍSTICO  -> localiza_df()
#      match exato + regra por token + fuzzy (adist). Robusto e auditável.
#
#   2) CAMADA DE ML (opcional, aprende com o tempo)  -> localiza_df_ml()
#      Roda o motor determinístico e, nos casos incertos (precisa_revisao_manual),
#      aplica um classificador de texto treinado nas correções humanas
#      (n-gramas de caractere + regressão logística multinomial / glmnet).
#      Quanto mais você revisa e retreina, melhor ele fica ("aprende gradual").
#      Sem modelo treinado, localiza_df_ml() devolve exatamente o resultado
#      determinístico (degradação limpa).
#
# ARQUIVOS AUXILIARES NECESSÁRIOS (na MESMA pasta deste .R):
#   tabela_localizacao_codeplan_2020.csv  — tabela de localidades da CODEPLAN
#   CODQGIS_RA.xlsx                       — fonte canônica da grafia das RAs
#
# ARQUIVOS GERADOS PELO CICLO DE ML (criados automaticamente):
#   ra_treino_rotulos.csv  — base de rótulos (a "memória" do modelo)
#   modelo_ra.rds          — o modelo treinado
#
# DEPENDÊNCIAS:
#   Sempre:    dplyr, stringr, stringi, readr, tibble, readxl
#   Só p/ ML:  tidymodels, textrecipes, glmnet  (carregados de forma lazy)
#     install.packages(c("tidymodels","textrecipes","glmnet"))
#
# FUNÇÃO PRINCIPAL (recomendada): localiza_df_ml()
# Demais: localiza_df(), vincular_regiao_saude(),
#   auditoria_vinculo_regiao_administrativa(), diagnostico_vinculo_ra(),
#   bootstrap_rotulos_v2(), coletar_rotulos_para_revisao(),
#   registrar_rotulos_ra(), treinar_modelo_ra(), avaliar_modelo_ra(),
#   prever_ra_ml(), padronizar_ra_qgis(), carregar_ra_qgis().
#
# Ver o passo a passo prático no README.md.
# ============================================================================

# ----------------------------------------------------------------------------
# Helpers internos de normalização de texto
# ----------------------------------------------------------------------------

# Normalização: NA-safe, sem acento, maiúsculas, pontuação -> espaço, colapsa
# espaços. Mantém letras/dígitos (preserva QNN, SHIN, DF-097).
.norm_ra <- function(x) {
  x <- ifelse(is.na(x), NA_character_, as.character(x))
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- toupper(x)
  x <- stringr::str_replace_all(x, "[^A-Z0-9]+", " ")
  x <- stringr::str_squish(x)
  ifelse(is.na(x) | x == "", NA_character_, x)
}

# Chave de cruzamento: Latin-ASCII, minúsculas, sem espaços; mantém pontuação.
# Idêntica à de duplicar_coluna_para_cruzamento() para que regiao_adm_chave
# gerado por localiza_df() case com o restante do pipeline.
.chave_ra <- function(x) {
  y <- ifelse(is.na(x), NA_character_, as.character(x))
  y <- stringi::stri_trans_general(y, "Latin-ASCII")
  y <- tolower(y)
  ifelse(is.na(y), NA_character_, gsub("\\s+", "", y, perl = TRUE))
}

# ----------------------------------------------------------------------------
# Fonte canônica de RAs: CODQGIS_RA.xlsx
# ----------------------------------------------------------------------------

.cache_ra_qgis <- new.env(parent = emptyenv())

# Lê e memoiza a tabela CODQGIS_RA.xlsx (RA, COD_RA_QGIS, REGIAO_SAUDE).
# Toda a nomenclatura de RA do projeto deve se padronizar por ela.
carregar_ra_qgis <- function(caminho_qgis = "CODQGIS_RA.xlsx") {
  if (!is.null(.cache_ra_qgis[[caminho_qgis]])) return(.cache_ra_qgis[[caminho_qgis]])
  tab <- readxl::read_xlsx(caminho_qgis) |>
    dplyr::transmute(RA, COD_RA_QGIS, REGIAO_SAUDE, ra_chave = .chave_ra(RA))
  .cache_ra_qgis[[caminho_qgis]] <- tab
  tab
}

# Aliases de grafia -> chave do QGIS, para variantes estruturais que não casam
# por chave direta (ordem/separadores diferentes nos arquivos da CODEPLAN/SES).
.aliases_grafia_qgis <- c(
  "scia/estrutural"      = "estrutural(scia)",
  "sciaestrutural"       = "estrutural(scia)",
  "estruturalscia"       = "estrutural(scia)",
  "estrutural"           = "estrutural(scia)",
  "scia"                 = "estrutural(scia)",
  "pordosol/solnascente" = "solnascente/pordosol",
  "pordosolsolnascente"  = "solnascente/pordosol",
  "solnascente_pordosol" = "solnascente/pordosol",
  "sudoeste_octogonal"   = "sudoeste/octogonal",
  "sudoesteoctogonal"    = "sudoeste/octogonal",
  "arniqueiras"          = "arniqueira",
  "riachofundoi"         = "riachofundo",
  "riachofundo1"         = "riachofundo"
)

# Converte qualquer grafia de RA para a grafia EXATA do CODQGIS_RA.xlsx.
# Tenta chave direta; se falhar, usa .aliases_grafia_qgis.
padronizar_ra_qgis <- function(x, caminho_qgis = "CODQGIS_RA.xlsx") {
  mapa  <- carregar_ra_qgis(caminho_qgis)
  ch    <- .chave_ra(x)
  alvo  <- ifelse(ch %in% mapa$ra_chave, ch,
                  unname(.aliases_grafia_qgis[ch]))
  idx   <- match(alvo, mapa$ra_chave)
  ifelse(is.na(idx), as.character(x), mapa$RA[idx])
}

# ----------------------------------------------------------------------------
# Mapa canônico e dicionário de aliases (DF)
# ----------------------------------------------------------------------------

# Mapa regiao_administrativa (CODEPLAN bruto) -> RA canônica.
.mapa_ra_canonica <- function() {
  tibble::tribble(
    ~raw,                 ~regiao_adm,
    "ÁGUAS CLARAS",       "AGUAS CLARAS",
    "ARNIQUEIRA",         "ARNIQUEIRA",
    "BRAZLÂNDIA",         "BRAZLANDIA",
    "CANDANGOLÂNDIA",     "CANDANGOLANDIA",
    "CEILÂNDIA",          "CEILANDIA",
    "CRUZEIRO",           "CRUZEIRO",
    "ESTRUTURAL (SCIA)",  "ESTRUTURAL (SCIA)",
    "FERCAL",             "FERCAL",
    "GAMA",               "GAMA",
    "GUARÁ",              "GUARA",
    "ITAPOÃ",             "ITAPOA",
    "JARDIM BOTÂNICO",    "JARDIM BOTANICO",
    "LAGO NORTE",         "LAGO NORTE",
    "LAGO SUL",           "LAGO SUL",
    "NÚCLEO BANDEIRANTE", "NUCLEO BANDEIRANTE",
    "PARANOÁ",            "PARANOA",
    "PARK WAY",           "PARK WAY",
    "PLANALTINA",         "PLANALTINA",
    "PLANO PILOTO",       "PLANO PILOTO",
    "RECANTO DAS EMAS",   "RECANTO DAS EMAS",
    "RIACHO FUNDO I",     "RIACHO FUNDO",
    "RIACHO FUNDO II",    "RIACHO FUNDO II",
    "S I A",              "SIA",
    "SAMAMBAIA",          "SAMAMBAIA",
    "SANTA MARIA",        "SANTA MARIA",
    "SÃO SEBASTIÃO",      "SAO SEBASTIAO",
    "SOBRADINHO",         "SOBRADINHO",
    "SOBRADINHO II",      "SOBRADINHO II",
    "SUDOESTE/OCTOGONAL", "SUDOESTE/OCTOGONAL",
    "TAGUATINGA",         "TAGUATINGA",
    "VARJÃO",             "VARJAO",
    "VICENTE PIRES",      "VICENTE PIRES"
  )
}

.ra_canonicas <- function() {
  unique(c(.mapa_ra_canonica()$regiao_adm,
           "SOL NASCENTE / POR DO SOL", "ARAPOANGA", "AGUA QUENTE"))
}

# Dicionário determinístico de padrões do DF (alias -> RA canônica).
# padrao já normalizado (.norm_ra). peso = confiança base do método.
.dicionario_aliases_df <- function() {
  ra <- .ra_canonicas()
  base <- tibble::tibble(padrao = .norm_ra(ra), regiao_adm = ra,
                         tipo = "ra_nome", peso = 0.97)

  curados <- tibble::tribble(
    ~padrao,                      ~regiao_adm,                 ~peso,
    # Plano Piloto / Sudoeste
    "ASA NORTE",                  "PLANO PILOTO",              0.95,
    "ASA SUL",                    "PLANO PILOTO",              0.95,
    "SETOR NOROESTE",             "PLANO PILOTO",              0.95,
    "NOROESTE",                   "PLANO PILOTO",              0.90,
    "VILA PLANALTO",              "PLANO PILOTO",              0.95,
    "VILA TELEBRASILIA",          "PLANO PILOTO",              0.95,
    "SUDOESTE",                   "SUDOESTE/OCTOGONAL",        0.95,
    "OCTOGONAL",                  "SUDOESTE/OCTOGONAL",        0.95,
    # Ceilândia
    "CEILANDIA NORTE",            "CEILANDIA",                 0.96,
    "CEILANDIA SUL",              "CEILANDIA",                 0.96,
    "P SUL",                      "CEILANDIA",                 0.92,
    "P NORTE",                    "CEILANDIA",                 0.92,
    "SETOR O",                    "CEILANDIA",                 0.88,
    "SETOR P SUL",                "CEILANDIA",                 0.93,
    "SETOR P NORTE",              "CEILANDIA",                 0.93,
    "EXPANSAO DO SETOR O",        "CEILANDIA",                 0.93,
    "GUARIROBA",                  "CEILANDIA",                 0.90,
    "QNN",                        "CEILANDIA",                 0.90,
    "QNM",                        "CEILANDIA",                 0.90,
    "QNP",                        "CEILANDIA",                 0.90,
    "QNO",                        "CEILANDIA",                 0.90,
    "QNQ",                        "CEILANDIA",                 0.90,
    "QNR",                        "CEILANDIA",                 0.90,
    # Sol Nascente / Pôr do Sol
    "SOL NASCENTE",               "SOL NASCENTE / POR DO SOL", 0.95,
    "POR DO SOL",                 "SOL NASCENTE / POR DO SOL", 0.95,
    "P DO SOL",                   "SOL NASCENTE / POR DO SOL", 0.90,
    # Taguatinga
    "TAGUATINGA NORTE",           "TAGUATINGA",                0.96,
    "TAGUATINGA SUL",             "TAGUATINGA",                0.96,
    # Samambaia
    "SAMAMBAIA NORTE",            "SAMAMBAIA",                 0.96,
    "SAMAMBAIA SUL",              "SAMAMBAIA",                 0.96,
    "COLONIA AGRICOLA SAMAMBAIA", "SAMAMBAIA",                 0.95,
    "SAMABAIA",                   "SAMAMBAIA",                 0.88,
    "SAMAMBIA",                   "SAMAMBAIA",                 0.88,
    # Recanto das Emas
    "RECANTO DA EMAS",            "RECANTO DAS EMAS",          0.93,
    # Estrutural / SCIA
    "SCIA",                       "ESTRUTURAL (SCIA)",         0.95,
    "ESTRUTURAL",                 "ESTRUTURAL (SCIA)",         0.95,
    "VILA ESTRUTURAL",            "ESTRUTURAL (SCIA)",         0.96,
    "CIDADE DO AUTOMOVEL",        "ESTRUTURAL (SCIA)",         0.93,
    "CIDADE DOS AUTOMOVEIS",      "ESTRUTURAL (SCIA)",         0.93,
    # Guará
    "GUARA I",                    "GUARA",                     0.96,
    "GUARA II",                   "GUARA",                     0.96,
    "LUCIO COSTA",                "GUARA",                     0.92,
    "QUADRAS ECONOMICAS LUCIO COSTA", "GUARA",                 0.93,
    # Cruzeiro
    "CRUZEIRO NOVO",              "CRUZEIRO",                  0.95,
    "CRUZEIRO VELHO",             "CRUZEIRO",                  0.95,
    "SHCES",                      "CRUZEIRO",                  0.90,
    # Lago Norte
    "SHIN",                       "LAGO NORTE",                0.92,
    "MANSOES LAGO NORTE",         "LAGO NORTE",                0.95,
    "GRANJA DO TORTO",            "LAGO NORTE",                0.93,
    "TAQUARI",                    "LAGO NORTE",                0.88,
    "SETOR DE HABITACOES INDIVIDUAIS NORTE", "LAGO NORTE",     0.95,
    "SETOR HABITACOES INDIVIDUAIS NORTE",  "LAGO NORTE",       0.93,
    "HABITACOES INDIVIDUAIS NORTE",        "LAGO NORTE",       0.90,
    # Lago Sul
    "SHIS",                       "LAGO SUL",                  0.92,
    "SETOR DE HABITACOES INDIVIDUAIS SUL", "LAGO SUL",         0.95,
    "HABITACOES INDIVIDUAIS SUL",          "LAGO SUL",         0.90,
    # Núcleo Bandeirante
    "METROPOLITANA",              "NUCLEO BANDEIRANTE",        0.90,
    "VILA NOVA DIVINEIA",         "NUCLEO BANDEIRANTE",        0.92,
    # Paranoá / Itapoã
    "PARANOA PARQUE",             "PARANOA",                   0.95,
    "DEL LAGO",                   "ITAPOA",                    0.90,
    # Planaltina (Arapoanga virou RA própria)
    "VALE DO AMANHECER",          "PLANALTINA",                0.92,
    "MESTRE D ARMAS",             "PLANALTINA",                0.92,
    "ESTANCIA MESTRE D ARMAS",    "PLANALTINA",                0.93,
    "QUINTAS DO AMANHECER",       "PLANALTINA",                0.92,
    "CORREGO DO ATOLEIRO",        "PLANALTINA",                0.92,
    "ATOLEIRO",                   "PLANALTINA",                0.85,
    "PLANALTINA DF",              "PLANALTINA",                0.95,
    "SETOR RESIDENCIAL LESTE",    "PLANALTINA",                0.90,
    # Sobradinho / Sobradinho II
    "LAGO OESTE",                 "SOBRADINHO II",             0.90,
    "GRANDE COLORADO",            "SOBRADINHO II",             0.92,
    "NUCLEO RURAL LAGO OESTE",    "SOBRADINHO II",             0.93,
    "AREA RURAL DE SOBRADINHO",   "SOBRADINHO",                0.92,
    # Riacho Fundo I/II
    "RIACHO FUNDO",               "RIACHO FUNDO",              0.88,
    "RIACHO FUNDO 1",             "RIACHO FUNDO",              0.93,
    "RIACHO FUNDO 2",             "RIACHO FUNDO II",           0.93,
    # Santa Maria
    "CH MARTINS",                 "SANTA MARIA",               0.90,
    # Gama
    "GAMA LESTE",                 "GAMA",                      0.93,
    "GAMA OESTE",                 "GAMA",                      0.93,
    "SETOR CENTRAL GAMA",         "GAMA",                      0.93,
    # Brazlândia
    "INCRA 07 08",                "BRAZLANDIA",                0.90,
    "INCRA 7 8",                  "BRAZLANDIA",                0.90,
    # Jardim Botânico
    "JOAO CANDIDO",               "JARDIM BOTANICO",           0.90,
    "SETOR HABITACIONAL JARDIM BOTANICO", "JARDIM BOTANICO",   0.95,
    # Ceilândia — abreviações e-SUS/SINAN
    "CEIL SUL",                   "CEILANDIA",                 0.90,
    "CEIL NORTE",                 "CEILANDIA",                 0.90,
    "CEI NORTE",                  "CEILANDIA",                 0.88,
    "CEI SUL",                    "CEILANDIA",                 0.88,
    "PSUL",                       "CEILANDIA",                 0.90
  ) |>
    dplyr::mutate(padrao = .norm_ra(padrao), tipo = "alias_df")

  dplyr::bind_rows(base, curados) |>
    dplyr::filter(!is.na(padrao)) |>
    dplyr::distinct(padrao, regiao_adm, .keep_all = TRUE)
}

# ----------------------------------------------------------------------------
# Referência CODEPLAN v2
# ----------------------------------------------------------------------------

# Constrói referência limpa da CODEPLAN usando apenas colunas confiáveis
# (regiao_administrativa + localidade). Descarta colunas pré-computadas
# (ra_norm/texto_busca_norm), corrompidas por vírgulas em descricao_area_bairro.
construir_referencia_codeplan_v2 <- function(
    caminho_tabela = "tabela_localizacao_codeplan_2020.csv") {

  bruto <- suppressWarnings(readr::read_delim(
    caminho_tabela, delim = ",", show_col_types = FALSE,
    locale = readr::locale(encoding = "UTF-8"), trim_ws = TRUE,
    name_repair = "minimal"
  ))
  bruto <- bruto[, c("regiao_administrativa", "localidade")]
  names(bruto) <- c("regiao_administrativa", "localidade")

  ref <- bruto |>
    dplyr::filter(!is.na(regiao_administrativa)) |>
    dplyr::left_join(.mapa_ra_canonica(), by = c("regiao_administrativa" = "raw")) |>
    dplyr::filter(!is.na(regiao_adm)) |>
    dplyr::mutate(
      padrao_ra         = .norm_ra(regiao_adm),
      padrao_localidade = .norm_ra(localidade)
    )

  ent_ra <- ref |>
    dplyr::transmute(padrao = padrao_ra, regiao_adm,
                     tipo = "ra_nome", peso = 0.97)
  ent_loc <- ref |>
    dplyr::filter(!is.na(padrao_localidade), padrao_localidade != padrao_ra) |>
    dplyr::transmute(padrao = padrao_localidade, regiao_adm,
                     tipo = "localidade_codeplan", peso = 0.90)

  dplyr::bind_rows(.dicionario_aliases_df(), ent_ra, ent_loc) |>
    dplyr::filter(!is.na(padrao), nchar(padrao) >= 2) |>
    dplyr::group_by(padrao) |>
    dplyr::filter(peso == max(peso)) |>
    dplyr::filter(dplyr::n_distinct(regiao_adm) == 1) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
}

# ----------------------------------------------------------------------------
# Helpers de matching
# ----------------------------------------------------------------------------

# Contém termo como "palavra inteira" (bordas de espaço).
.contem_token <- function(texto, termo) {
  stringr::str_detect(paste0(" ", texto, " "),
                      stringr::fixed(paste0(" ", termo, " ")))
}

# Similaridade 0..1 por distância de edição (base R adist; sem dependências).
.sim_edit <- function(a, b) {
  if (is.na(a) || is.na(b) || a == "" || b == "") return(0)
  d <- as.integer(utils::adist(a, b))
  1 - d / max(nchar(a), nchar(b))
}

# ============================================================================
# FUNÇÃO PRINCIPAL: localiza_df()
# ----------------------------------------------------------------------------
# Vincula cada endereço (ID_BAIRRO ou outra coluna) à Região Administrativa
# do DF em 3 camadas: (1) match exato, (2) regra determinística (substring por
# token), (3) fuzzy (adist).
#
# Retorna o df original acrescido de:
#   endereco_original, endereco_padronizado
#   regiao_adm, regiao_adm_chave
#   metodo_vinculo, score_confianca
#   texto_usado_para_match, possiveis_matches
#   precisa_revisao_manual
#
# Parâmetros:
#   df             — data.frame de entrada
#   col_bairro     — nome da coluna de endereço (default "ID_BAIRRO")
#   caminho_tabela — tabela_localizacao_codeplan_2020.csv
#   caminho_qgis   — CODQGIS_RA.xlsx (fonte canônica)
#   limiar_fuzzy   — similaridade mínima para aceitar match fuzzy (default 0.86)
#   margem_minima  — margem entre 1º e 2º candidato para não marcar como ambíguo
#   max_janela     — tamanho máximo de janela de tokens no fuzzy
# ============================================================================
localiza_df <- function(
    df,
    col_bairro     = "ID_BAIRRO",
    caminho_tabela = "tabela_localizacao_codeplan_2020.csv",
    caminho_qgis   = "CODQGIS_RA.xlsx",
    limiar_fuzzy   = 0.86,
    margem_minima  = 0.05,
    max_janela     = 4) {

  if (!col_bairro %in% names(df)) {
    stop(sprintf("Coluna '%s' não encontrada em df.", col_bairro))
  }

  ref <- construir_referencia_codeplan_v2(caminho_tabela) |>
    dplyr::mutate(n = nchar(padrao)) |>
    dplyr::arrange(dplyr::desc(n))
  padroes     <- ref$padrao
  padroes_ra  <- ref$regiao_adm
  padroes_pes <- ref$peso
  padroes_n   <- ref$n

  enderecos_orig <- as.character(df[[col_bairro]])
  enderecos_norm <- .norm_ra(enderecos_orig)

  resolver_um <- function(addr) {
    if (is.na(addr)) {
      return(list(regiao_adm = NA_character_, metodo = "sem_correspondencia",
                  score = NA_real_, texto = NA_character_,
                  possiveis = NA_character_, revisao = TRUE))
    }

    # RA explícita entre parênteses tem prioridade
    par <- .norm_ra(stringr::str_match(addr, "\\(([^)]+)\\)")[, 2])

    # Camada 1: correspondência exata
    if (!is.na(par)) {
      i <- which(padroes == par)
      if (length(i) > 0) {
        i <- i[which.max(padroes_pes[i])]
        return(list(regiao_adm = padroes_ra[i], metodo = "match_exato",
                    score = 1.0, texto = par,
                    possiveis = NA_character_, revisao = FALSE))
      }
    }
    i <- which(padroes == addr)
    if (length(i) > 0) {
      i <- i[which.max(padroes_pes[i])]
      return(list(regiao_adm = padroes_ra[i], metodo = "match_exato",
                  score = 1.0, texto = addr,
                  possiveis = NA_character_, revisao = FALSE))
    }

    # Camada 2: regra determinística (substring por token).
    # Desempate por chave = comprimento + peso: padrão mais longo vence; em
    # comprimentos iguais, nome da RA (0.97) vence localidade genérica (0.90).
    hits <- which(vapply(padroes, function(p) .contem_token(addr, p), logical(1)))
    if (length(hits) > 0) {
      chave <- padroes_n[hits] + padroes_pes[hits]
      top   <- hits[chave == max(chave)]
      ras_top <- unique(padroes_ra[top])
      if (length(ras_top) == 1) {
        j <- top[1]
        return(list(regiao_adm = padroes_ra[j], metodo = "regra_deterministica",
                    score = padroes_pes[j], texto = padroes[j],
                    possiveis = NA_character_, revisao = padroes_pes[j] < 0.90))
      }
      return(list(regiao_adm = NA_character_, metodo = "regra_deterministica",
                  score = NA_real_, texto = paste(padroes[top], collapse = " | "),
                  possiveis = paste(ras_top, collapse = " | "), revisao = TRUE))
    }

    # Camada 3: fuzzy (adist) sobre janelas de 1..max_janela tokens
    toks <- strsplit(addr, " ", fixed = TRUE)[[1]]
    toks <- toks[nchar(toks) > 0]
    if (length(toks) == 0) {
      return(list(regiao_adm = NA_character_, metodo = "sem_correspondencia",
                  score = NA_real_, texto = addr,
                  possiveis = NA_character_, revisao = TRUE))
    }
    janelas <- character(0)
    for (w in 1:min(max_janela, length(toks))) {
      for (s in 1:(length(toks) - w + 1)) {
        janelas <- c(janelas, paste(toks[s:(s + w - 1)], collapse = " "))
      }
    }
    janelas <- unique(janelas)

    melhor_sim <- 0; melhor_ra <- NA_character_; melhor_txt <- NA_character_
    seg_sim <- 0; seg_ra <- NA_character_
    for (p in seq_along(padroes)) {
      sims <- vapply(janelas, .sim_edit, numeric(1), b = padroes[p])
      s <- max(sims)
      if (s > melhor_sim) {
        seg_sim <- melhor_sim; seg_ra <- melhor_ra
        melhor_sim <- s; melhor_ra <- padroes_ra[p]
        melhor_txt <- janelas[which.max(sims)]
      } else if (s > seg_sim && padroes_ra[p] != melhor_ra) {
        seg_sim <- s; seg_ra <- padroes_ra[p]
      }
    }

    if (melhor_sim >= limiar_fuzzy) {
      ambiguo <- !is.na(seg_ra) && seg_ra != melhor_ra &&
                 (melhor_sim - seg_sim) < margem_minima
      return(list(
        regiao_adm = if (ambiguo) NA_character_ else melhor_ra,
        metodo = "fuzzy_match",
        score = round(melhor_sim, 3), texto = melhor_txt,
        possiveis = if (ambiguo) paste(c(melhor_ra, seg_ra), collapse = " | ") else NA_character_,
        revisao = ambiguo || melhor_sim < 0.90
      ))
    }

    list(regiao_adm = NA_character_, metodo = "sem_correspondencia",
         score = round(melhor_sim, 3), texto = addr,
         possiveis = NA_character_, revisao = TRUE)
  }

  # Resolve uma vez por endereço normalizado único (eficiência)
  unicos <- unique(enderecos_norm)
  res_lista <- lapply(unicos, resolver_um)
  names(res_lista) <- ifelse(is.na(unicos), "__NA__", unicos)

  pega <- function(campo) {
    vapply(enderecos_norm, function(e) {
      r <- res_lista[[ if (is.na(e)) "__NA__" else e ]]
      v <- r[[campo]]
      if (is.null(v) || length(v) == 0) NA else v
    }, FUN.VALUE = if (campo == "score") numeric(1)
                   else if (campo == "revisao") logical(1)
                   else character(1))
  }

  df$endereco_original      <- enderecos_orig
  df$endereco_padronizado   <- enderecos_norm
  # Padroniza a grafia final pela fonte de verdade (CODQGIS_RA.xlsx)
  df$regiao_adm             <- padronizar_ra_qgis(pega("regiao_adm"), caminho_qgis)
  df$regiao_adm_chave       <- .chave_ra(df$regiao_adm)
  df$metodo_vinculo         <- pega("metodo")
  df$score_confianca        <- pega("score")
  df$texto_usado_para_match <- pega("texto")
  df$possiveis_matches      <- pega("possiveis")
  df$precisa_revisao_manual <- pega("revisao")
  df
}

# ============================================================================
# DICIONÁRIO RA -> REGIÃO DE SAÚDE (DF)
# ============================================================================

dicionario_ra_regiao_saude <- function() {
  tibble::tribble(
    ~regiao_adm,            ~regiao_saude,
    "AGUAS CLARAS",         "SUDOESTE",
    "ASA NORTE",            "CENTRAL",
    "ASA SUL",              "CENTRAL",
    "BRAZLANDIA",           "OESTE",
    "CANDANGOLANDIA",       "CENTRO-SUL",
    "CEILANDIA",            "OESTE",
    "CRUZEIRO",             "CENTRAL",
    "SCIA (ESTRUTURAL)",    "CENTRO-SUL",
    "FERCAL",               "NORTE",
    "GAMA",                 "SUL",
    "GUARA",                "CENTRO-SUL",
    "ITAPOA",               "LESTE",
    "JARDIM BOTANICO",      "LESTE",
    "LAGO NORTE",           "CENTRAL",
    "LAGO SUL",             "CENTRAL",
    "NUCLEO BANDEIRANTE",   "CENTRO-SUL",
    "PARANOA",              "LESTE",
    "PARK WAY",             "CENTRO-SUL",
    "PLANALTINA",           "NORTE",
    "PLANO PILOTO",         "CENTRAL",
    "RECANTO DAS EMAS",     "SUDOESTE",
    "RIACHO FUNDO I",       "CENTRO-SUL",
    "RIACHO FUNDO II",      "CENTRO-SUL",
    "SAMAMBAIA",            "SUDOESTE",
    "SANTA MARIA",          "SUL",
    "SAO SEBASTIAO",        "LESTE",
    "SIA",                  "CENTRO-SUL",
    "SOBRADINHO",           "NORTE",
    "SOBRADINHO II",        "NORTE",
    "SUDOESTE OCTOGONAL",   "CENTRAL",
    "TAGUATINGA",           "SUDOESTE",
    "VARJAO",               "CENTRAL",
    "VICENTE PIRES",        "SUDOESTE",
    "ARNIQUEIRAS",          "SUDOESTE",
    "SOL NASCENTE / POR DO SOL", "OESTE",
    "ARAPOANGA",            "NORTE",
    "AGUA QUENTE",          "SUDOESTE"
  )
}

# Adiciona coluna `regiao_saude` ao data.frame com base em `regiao_adm`.
# Usa o CODQGIS_RA.xlsx como fonte primária; dicionario_ra_regiao_saude() é
# mantido como fallback histórico.
vincular_regiao_saude <- function(df, col_ra = "regiao_adm",
                                  caminho_qgis = "CODQGIS_RA.xlsx") {
  if (!col_ra %in% names(df)) {
    stop(sprintf("Coluna '%s' não encontrada em df.", col_ra))
  }
  rs <- carregar_ra_qgis(caminho_qgis) |>
    dplyr::select(ra_chave, regiao_saude = REGIAO_SAUDE)

  df$.ra_chave_tmp <- .chave_ra(df[[col_ra]])
  out <- df |>
    dplyr::left_join(rs, by = c(".ra_chave_tmp" = "ra_chave")) |>
    dplyr::mutate(regiao_saude = dplyr::if_else(
      is.na(regiao_saude), "RS não identificada", regiao_saude
    )) |>
    dplyr::select(-.ra_chave_tmp)
  out
}

# ============================================================================
# AUDITORIA E DIAGNÓSTICO
# ============================================================================

# Tabela de casos que precisam de revisão manual (saída de localiza_df()).
auditoria_vinculo_regiao_administrativa <- function(df_result) {
  df_result |>
    dplyr::filter(precisa_revisao_manual %in% TRUE) |>
    dplyr::transmute(
      endereco_original, endereco_padronizado,
      ra_sugerida = regiao_adm, score_confianca, metodo_vinculo,
      possiveis_matches,
      motivo_revisao = dplyr::case_when(
        is.na(regiao_adm) & metodo_vinculo == "sem_correspondencia" ~ "sem correspondência",
        is.na(regiao_adm) & !is.na(possiveis_matches)               ~ "ambiguidade entre RAs",
        score_confianca < 0.90                                      ~ "confiança baixa",
        TRUE                                                        ~ "revisar"
      )
    ) |>
    dplyr::distinct()
}

# Diagnóstico de performance contra uma coluna de verdade (RA conhecida).
# Retorna acurácia, % classificado, % em revisão e erros por RA.
diagnostico_vinculo_ra <- function(df_result, col_verdade) {
  stopifnot(col_verdade %in% names(df_result))
  d <- df_result |>
    dplyr::mutate(verdade = .chave_ra(.data[[col_verdade]]),
                  pred    = .chave_ra(regiao_adm)) |>
    dplyr::filter(!is.na(verdade))
  aval <- d |> dplyr::filter(!is.na(pred))
  list(
    n_avaliados       = nrow(d),
    classificados_pct = round(mean(!is.na(d$pred)) * 100, 1),
    revisao_pct       = round(mean(d$precisa_revisao_manual %in% TRUE) * 100, 1),
    acuracia_pct      = if (nrow(aval) > 0) round(mean(aval$pred == aval$verdade) * 100, 1) else NA_real_,
    erros_por_ra      = aval |>
      dplyr::filter(pred != verdade) |>
      dplyr::count(verdade = .chave_ra(.data[[col_verdade]]), regiao_adm, sort = TRUE)
  )
}

# Testes de regressão: casos fáceis, abreviados, com typo e ambíguos.
testar_vinculo_ra_v2 <- function(
    caminho_tabela = "tabela_localizacao_codeplan_2020.csv") {
  casos <- tibble::tribble(
    ~endereco,                       ~esperado,
    "QNN 18 Ceilândia",              "CEILÂNDIA",
    "P Norte",                       "CEILÂNDIA",
    "Sol Nascente",                  "SOL NASCENTE/PÔR DO SOL",
    "Arapoanga",                     "ARAPOANGA",
    "Sobradinho II",                 "SOBRADINHO II",
    "Setor O",                       "CEILÂNDIA",
    "Vicente Pires",                 "VICENTE PIRES",
    "Colônia Agrícola Samambaia",    "SAMAMBAIA",
    "SCIA Estrutural",               "ESTRUTURAL (SCIA)",
    "SIA Trecho 3",                  "SIA",
    "Jardim Botânico",               "JARDIM BOTÂNICO",
    "Riacho Fundo II",               "RIACHO FUNDO II",
    "Park Way",                      "PARK WAY",
    "Núcleo Bandeirante",            "NÚCLEO BANDEIRANTE",
    "Lago Norte",                    "LAGO NORTE",
    "Lago Sul",                      "LAGO SUL",
    "Ceilanda Sul",                  "CEILÂNDIA",
    "Samambia Norte",                "SAMAMBAIA",
    "Endereço inexistente XYZ",      NA_character_
  )
  out <- localiza_df(
    casos, col_bairro = "endereco", caminho_tabela = caminho_tabela)
  out |>
    dplyr::transmute(
      endereco, esperado, obtido = regiao_adm,
      metodo = metodo_vinculo, score = score_confianca,
      ok = (is.na(esperado) & is.na(obtido)) |
           (!is.na(obtido) & obtido == esperado)
    )
}

# ============================================================================
# CAMADA DE MACHINE LEARNING (aprendizagem supervisionada incremental)
# ----------------------------------------------------------------------------
# COMPLEMENTA localiza_df() — não a substitui. Ciclo human-in-the-loop:
#   1) localiza_df() resolve o fácil; sobra um RESÍDUO incerto.
#   2) Um humano revisa o resíduo -> cada correção vira um RÓTULO.
#   3) Os rótulos acumulados treinam um classificador de texto.
#   4) Nas próximas execuções o modelo cobre parte do resíduo sozinho.
# A cada ciclo a base de rótulos cresce e o modelo MELHORA.
#
# Por que n-gramas de caractere? Endereços são curtos e cheios de abreviações/
# erros ("SAMABAIA", "CEIL SUL", "P NORTE"); n-gramas (2..4) são robustos a typos.
# ============================================================================

# Caminhos padrão dos artefatos de ML (na pasta deste .R).
.CAMINHO_ROTULOS_RA <- "ra_treino_rotulos.csv"
.CAMINHO_MODELO_RA  <- "modelo_ra.rds"

# Garante as dependências de ML; mensagem única e clara se faltar.
.checar_pacotes_ml <- function(extra = character(0)) {
  necessarios <- c("recipes", "parsnip", "workflows", "textrecipes",
                   "glmnet", "tibble", "dplyr", extra)
  faltam <- necessarios[!vapply(necessarios, requireNamespace,
                                logical(1), quietly = TRUE)]
  if (length(faltam) > 0) {
    stop("Pacotes de ML ausentes: ", paste(faltam, collapse = ", "),
         "\nInstale com: install.packages(c(",
         paste(sprintf('\"%s\"', faltam), collapse = ", "), "))")
  }
  invisible(TRUE)
}

# Esquema da base de rótulos. `endereco_padronizado` é a chave de treino
# (texto que o modelo enxerga); `regiao_adm_correta` é o alvo (label).
.colunas_rotulos_ra <- function() {
  c("endereco_original", "endereco_padronizado", "regiao_adm_correta",
    "fonte", "data_rotulo")
}

# ----------------------------------------------------------------------------
# 1. COLETA DE RÓTULOS
# ----------------------------------------------------------------------------

# Exporta os casos que precisam de revisão manual como CSV-template para um
# humano preencher a coluna `regiao_adm_correta`. df_result = saída de
# localiza_df() ou localiza_df_ml().
coletar_rotulos_para_revisao <- function(
    df_result,
    caminho_saida = "ra_para_revisar.csv") {
  obrig <- c("endereco_original", "endereco_padronizado",
             "regiao_adm", "metodo_vinculo", "score_confianca",
             "possiveis_matches", "precisa_revisao_manual")
  faltam <- setdiff(obrig, names(df_result))
  if (length(faltam) > 0) {
    stop("df_result não parece vir de localiza_df(); faltam colunas: ",
         paste(faltam, collapse = ", "))
  }

  template <- df_result |>
    dplyr::filter(.data$precisa_revisao_manual %in% TRUE) |>
    dplyr::distinct(.data$endereco_padronizado, .keep_all = TRUE) |>
    dplyr::transmute(
      endereco_original,
      endereco_padronizado,
      ra_sugerida        = regiao_adm,     # palpite do motor (pré-preenchido)
      regiao_adm_correta = NA_character_,  # <- HUMANO PREENCHE AQUI
      metodo_vinculo,
      score_confianca,
      possiveis_matches
    ) |>
    dplyr::arrange(score_confianca)

  dir.create(dirname(caminho_saida), recursive = TRUE, showWarnings = FALSE)
  readr::write_excel_csv(template, caminho_saida)
  message("Casos para revisão: ", nrow(template), " -> ", caminho_saida,
          "\nPreencha 'regiao_adm_correta' e chame registrar_rotulos_ra().")
  invisible(caminho_saida)
}

# Semeia a base de rótulos com acertos de ALTA confiança do motor (match exato /
# regra forte). Dá treino ao modelo desde o dia 1, sem esperar revisão manual.
bootstrap_rotulos_v2 <- function(
    df_result,
    caminho_store = .CAMINHO_ROTULOS_RA,
    limiar_confianca = 0.95) {
  conf <- df_result |>
    dplyr::filter(!is.na(.data$regiao_adm),
                  !(.data$precisa_revisao_manual %in% TRUE),
                  .data$score_confianca >= limiar_confianca) |>
    dplyr::distinct(.data$endereco_padronizado, .keep_all = TRUE) |>
    dplyr::transmute(
      endereco_original,
      endereco_padronizado,
      regiao_adm_correta = regiao_adm,
      fonte       = "bootstrap_v2",
      data_rotulo = as.character(Sys.Date())
    )
  .gravar_rotulos(conf, caminho_store)
}

# Lê um CSV revisado por humano (com `regiao_adm_correta` preenchida) e o
# incorpora à base de rótulos.
registrar_rotulos_ra <- function(
    caminho_revisado,
    caminho_store = .CAMINHO_ROTULOS_RA) {
  rev <- readr::read_csv(caminho_revisado, show_col_types = FALSE)
  if (!"regiao_adm_correta" %in% names(rev)) {
    stop("O arquivo revisado precisa ter a coluna 'regiao_adm_correta'.")
  }
  novos <- rev |>
    dplyr::filter(!is.na(.data$regiao_adm_correta),
                  nzchar(stringr::str_squish(.data$regiao_adm_correta))) |>
    dplyr::transmute(
      endereco_original = if ("endereco_original" %in% names(rev))
        .data$endereco_original else NA_character_,
      endereco_padronizado = .norm_ra(
        if ("endereco_padronizado" %in% names(rev))
          .data$endereco_padronizado else .data$endereco_original),
      regiao_adm_correta = stringr::str_squish(.data$regiao_adm_correta),
      fonte       = "revisao_manual",
      data_rotulo = as.character(Sys.Date())
    )
  if (nrow(novos) == 0) {
    warning("Nenhuma linha com 'regiao_adm_correta' preenchida; nada a registrar.")
    return(invisible(caminho_store))
  }
  .gravar_rotulos(novos, caminho_store)
}

# Append + dedup da base de rótulos. Grafia do alvo padronizada pelo
# CODQGIS_RA.xlsx. Dedup por endereco_padronizado: a entrada MAIS RECENTE vence
# (correções humanas sobrescrevem o bootstrap).
.gravar_rotulos <- function(novos, caminho_store) {
  novos <- novos |>
    dplyr::mutate(
      endereco_padronizado = .norm_ra(.data$endereco_padronizado),
      regiao_adm_correta   = padronizar_ra_qgis(.data$regiao_adm_correta)
    ) |>
    dplyr::filter(!is.na(.data$endereco_padronizado),
                  !is.na(.data$regiao_adm_correta))

  antigos <- if (file.exists(caminho_store)) {
    readr::read_csv(caminho_store, show_col_types = FALSE)
  } else {
    tibble::tibble()
  }

  combinado <- dplyr::bind_rows(antigos, novos) |>
    dplyr::mutate(.ordem = dplyr::row_number()) |>
    dplyr::group_by(.data$endereco_padronizado) |>
    dplyr::slice_max(.data$.ordem, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(-".ordem") |>
    dplyr::arrange(.data$regiao_adm_correta, .data$endereco_padronizado)

  dir.create(dirname(caminho_store), recursive = TRUE, showWarnings = FALSE)
  readr::write_excel_csv(combinado, caminho_store)
  message("Base de rótulos: ", nrow(combinado), " exemplos (",
          dplyr::n_distinct(combinado$regiao_adm_correta), " RAs) -> ",
          caminho_store)
  invisible(caminho_store)
}

# ----------------------------------------------------------------------------
# 2. TREINO DO MODELO
# ----------------------------------------------------------------------------

# Treina o classificador de RA a partir da base de rótulos e o salva em disco.
# Pipeline: recipe -> n-gramas de caractere (2..n) -> tf-idf -> glmnet multinom.
# Retorna o caminho do modelo salvo, ou NULL se não houver dados suficientes
# (nesse caso o vínculo segue só determinístico).
treinar_modelo_ra <- function(
    caminho_store  = .CAMINHO_ROTULOS_RA,
    caminho_modelo = .CAMINHO_MODELO_RA,
    penalty        = 0.01,
    n_gram         = 4L,
    max_tokens     = 2000L,
    min_exemplos   = 30L) {
  .checar_pacotes_ml()

  if (!file.exists(caminho_store)) {
    message("Sem base de rótulos (", caminho_store, "). ",
            "Gere rótulos (bootstrap_rotulos_v2 / registrar_rotulos_ra) antes.")
    return(invisible(NULL))
  }

  dados <- readr::read_csv(caminho_store, show_col_types = FALSE) |>
    dplyr::transmute(
      texto      = .norm_ra(.data$endereco_padronizado),
      regiao_adm = padronizar_ra_qgis(.data$regiao_adm_correta)
    ) |>
    dplyr::filter(!is.na(.data$texto), !is.na(.data$regiao_adm)) |>
    dplyr::distinct()

  n_classes <- dplyr::n_distinct(dados$regiao_adm)
  if (nrow(dados) < min_exemplos || n_classes < 2) {
    message("Rótulos insuficientes para treinar (", nrow(dados),
            " exemplos, ", n_classes, " RAs; mínimo ", min_exemplos,
            " e 2 RAs). Mantendo só o determinístico.")
    return(invisible(NULL))
  }

  dados$regiao_adm <- factor(dados$regiao_adm)

  rec <- recipes::recipe(regiao_adm ~ texto, data = dados) |>
    textrecipes::step_tokenize(
      texto, token = "character_shingle",
      options = list(n = n_gram, n_min = 2L)) |>
    textrecipes::step_tokenfilter(texto, max_tokens = max_tokens) |>
    textrecipes::step_tfidf(texto)

  mod <- parsnip::multinom_reg(penalty = penalty, mixture = 1) |>
    parsnip::set_engine("glmnet") |>
    parsnip::set_mode("classification")

  wf <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(mod)

  modelo <- tryCatch(
    parsnip::fit(wf, data = dados),
    error = function(e) {
      message("Falha ao treinar o modelo de RA: ", conditionMessage(e))
      NULL
    })
  if (is.null(modelo)) return(invisible(NULL))

  dir.create(dirname(caminho_modelo), recursive = TRUE, showWarnings = FALSE)
  saveRDS(
    list(workflow = modelo, classes = levels(dados$regiao_adm),
         n_exemplos = nrow(dados), penalty = penalty, n_gram = n_gram,
         treinado_em = Sys.time()),
    caminho_modelo)
  .cache_modelo_ra[[caminho_modelo]] <- NULL  # invalida cache
  message("Modelo de RA treinado: ", nrow(dados), " exemplos, ",
          n_classes, " RAs -> ", caminho_modelo)
  invisible(caminho_modelo)
}

# Carrega (e memoiza) o modelo salvo. NULL silencioso se não houver.
.cache_modelo_ra <- new.env(parent = emptyenv())
carregar_modelo_ra <- function(caminho_modelo = .CAMINHO_MODELO_RA) {
  if (!is.null(.cache_modelo_ra[[caminho_modelo]]))
    return(.cache_modelo_ra[[caminho_modelo]])
  if (!file.exists(caminho_modelo)) return(NULL)
  obj <- readRDS(caminho_modelo)
  .cache_modelo_ra[[caminho_modelo]] <- obj
  obj
}

# ----------------------------------------------------------------------------
# 3. PREDIÇÃO
# ----------------------------------------------------------------------------

# Prediz a RA para um vetor de endereços (texto livre): classe mais provável e
# sua probabilidade. Resolve uma vez por texto único.
prever_ra_ml <- function(enderecos, modelo = NULL,
                         caminho_modelo = .CAMINHO_MODELO_RA) {
  .checar_pacotes_ml()
  if (is.null(modelo)) modelo <- carregar_modelo_ra(caminho_modelo)
  n <- length(enderecos)
  if (is.null(modelo)) {
    return(tibble::tibble(regiao_adm_ml = rep(NA_character_, n),
                          prob_ml = rep(NA_real_, n)))
  }
  txt <- .norm_ra(enderecos)
  unicos <- unique(txt[!is.na(txt)])
  if (length(unicos) == 0) {
    return(tibble::tibble(regiao_adm_ml = rep(NA_character_, n),
                          prob_ml = rep(NA_real_, n)))
  }
  novo <- tibble::tibble(texto = unicos)
  cls  <- predict(modelo$workflow, novo, type = "class")$.pred_class
  prob <- predict(modelo$workflow, novo, type = "prob")
  prob_max <- apply(as.matrix(prob), 1, max)

  mapa <- tibble::tibble(texto = unicos,
                         regiao_adm_ml = as.character(cls),
                         prob_ml = prob_max)
  idx <- match(txt, mapa$texto)
  tibble::tibble(regiao_adm_ml = mapa$regiao_adm_ml[idx],
                 prob_ml = mapa$prob_ml[idx])
}

# ----------------------------------------------------------------------------
# 4. VÍNCULO HÍBRIDO (determinístico + ML no resíduo)  <- FUNÇÃO PRINCIPAL
# ----------------------------------------------------------------------------

# Roda localiza_df() e, nos casos marcados para revisão, aplica o modelo de ML
# quando a probabilidade >= limiar_ml. Grafia final padronizada pelo
# CODQGIS_RA.xlsx. Saída = colunas de localiza_df() + origem_classificacao + prob_ml.
localiza_df_ml <- function(
    df,
    col_bairro     = "ID_BAIRRO",
    caminho_tabela = "tabela_localizacao_codeplan_2020.csv",
    caminho_qgis   = "CODQGIS_RA.xlsx",
    caminho_modelo = .CAMINHO_MODELO_RA,
    limiar_ml      = 0.70,   # mínimo p/ aceitar predição do ML
    limiar_revisao = 0.85) { # abaixo disso, ainda marca p/ revisão humana

  # 1) Camada determinística (intacta, auditável).
  out <- localiza_df(df, col_bairro = col_bairro,
                     caminho_tabela = caminho_tabela, caminho_qgis = caminho_qgis)
  out$origem_classificacao <- ifelse(is.na(out$regiao_adm),
                                     NA_character_, "deterministica")
  out$prob_ml <- NA_real_

  modelo <- carregar_modelo_ra(caminho_modelo)
  if (is.null(modelo)) {
    message("Sem modelo de ML treinado; resultado é 100% determinístico. ",
            "Treine com treinar_modelo_ra() para ativar a camada de ML.")
    return(out)
  }

  # 2) Resíduo: o que o motor não resolveu com confiança.
  residuo <- which(out$precisa_revisao_manual %in% TRUE)
  if (length(residuo) == 0) return(out)

  pred <- prever_ra_ml(out$endereco_padronizado[residuo], modelo = modelo)
  aceita <- !is.na(pred$regiao_adm_ml) & pred$prob_ml >= limiar_ml
  alvo <- residuo[aceita]

  if (length(alvo) > 0) {
    ra_ml <- padronizar_ra_qgis(pred$regiao_adm_ml[aceita], caminho_qgis)
    out$regiao_adm[alvo]             <- ra_ml
    out$regiao_adm_chave[alvo]       <- .chave_ra(ra_ml)
    out$metodo_vinculo[alvo]         <- "ml_glmnet"
    out$score_confianca[alvo]        <- round(pred$prob_ml[aceita], 3)
    out$origem_classificacao[alvo]   <- "ml_glmnet"
    out$prob_ml[alvo]                <- round(pred$prob_ml[aceita], 3)
    out$texto_usado_para_match[alvo] <- out$endereco_padronizado[alvo]
    out$possiveis_matches[alvo]      <- NA_character_
    out$precisa_revisao_manual[alvo] <- pred$prob_ml[aceita] < limiar_revisao
  }
  out
}

# ----------------------------------------------------------------------------
# 5. AVALIAÇÃO (validação cruzada da base de rótulos)
# ----------------------------------------------------------------------------

# Estima a acurácia do modelo por validação cruzada k-fold sobre os rótulos.
# Útil para acompanhar a evolução do aprendizado a cada ciclo.
avaliar_modelo_ra <- function(
    caminho_store = .CAMINHO_ROTULOS_RA,
    k = 5L, penalty = 0.01, n_gram = 4L, max_tokens = 2000L) {
  .checar_pacotes_ml(extra = c("rsample", "yardstick", "tune"))
  if (!file.exists(caminho_store)) {
    message("Sem base de rótulos para avaliar."); return(invisible(NULL))
  }
  dados <- readr::read_csv(caminho_store, show_col_types = FALSE) |>
    dplyr::transmute(texto = .norm_ra(.data$endereco_padronizado),
                     regiao_adm = factor(padronizar_ra_qgis(.data$regiao_adm_correta))) |>
    dplyr::filter(!is.na(.data$texto)) |>
    dplyr::distinct()
  if (nrow(dados) < k * 2) {
    message("Poucos exemplos para CV de ", k, " folds."); return(invisible(NULL))
  }

  rec <- recipes::recipe(regiao_adm ~ texto, data = dados) |>
    textrecipes::step_tokenize(texto, token = "character_shingle",
                               options = list(n = n_gram, n_min = 2L)) |>
    textrecipes::step_tokenfilter(texto, max_tokens = max_tokens) |>
    textrecipes::step_tfidf(texto)
  mod <- parsnip::multinom_reg(penalty = penalty, mixture = 1) |>
    parsnip::set_engine("glmnet") |> parsnip::set_mode("classification")
  wf <- workflows::workflow() |>
    workflows::add_recipe(rec) |> workflows::add_model(mod)

  set.seed(1)
  folds <- rsample::vfold_cv(dados, v = k)
  res <- tune::fit_resamples(
    wf, folds, metrics = yardstick::metric_set(yardstick::accuracy),
    control = tune::control_resamples(save_pred = FALSE))
  tune::collect_metrics(res)
}

# ============================================================================
# FLUXO DE USO TÍPICO (a cada ciclo de aprendizagem) — ver README.md
# ----------------------------------------------------------------------------
# source("localiza_df.R")
# res <- localiza_df_ml(meu_df, col_bairro = "NM_BAIRRO")   # padroniza
# bootstrap_rotulos_v2(res)                                 # rótulos "de graça"
# coletar_rotulos_para_revisao(res, "ra_para_revisar.csv")  # exporta resíduo
# # (humano preenche regiao_adm_correta no CSV)
# registrar_rotulos_ra("ra_para_revisar.csv")               # incorpora
# treinar_modelo_ra()                                       # treina/salva .rds
# avaliar_modelo_ra()                                       # acurácia (CV)
# ============================================================================
