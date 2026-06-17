# ============================================================================
# localiza_df.R
# ----------------------------------------------------------------------------
# Padronização de endereços (ID_BAIRRO) para Região Administrativa (RA) do DF
# a partir da tabela da CODEPLAN.
#
# PLANILHAS AUXILIARES NECESSÁRIAS:
#   auxiliares/tabela_localizacao_codeplan_2020.csv  — tabela de localidades
#   auxiliares/CODQGIS_RA.xlsx                       — fonte canônica de RAs
#
# FUNÇÃO PRINCIPAL:
#   localiza_df()   →  antigo vincular_regiao_administrativa_codeplan_v2()
#
# OUTRAS FUNÇÕES EXPORTADAS:
#   vincular_regiao_saude()
#   auditoria_vinculo_regiao_administrativa()
#   diagnostico_vinculo_ra()
#   testar_vinculo_ra_v2()
#   dicionario_ra_regiao_saude()
#   padronizar_ra_qgis()
#   carregar_ra_qgis()
#   construir_referencia_codeplan_v2()
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
carregar_ra_qgis <- function(caminho_qgis = "auxiliares/CODQGIS_RA.xlsx") {
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
padronizar_ra_qgis <- function(x, caminho_qgis = "auxiliares/CODQGIS_RA.xlsx") {
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
    caminho_tabela = "auxiliares/tabela_localizacao_codeplan_2020.csv") {

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
    caminho_tabela = "auxiliares/tabela_localizacao_codeplan_2020.csv",
    caminho_qgis   = "auxiliares/CODQGIS_RA.xlsx",
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
                                  caminho_qgis = "auxiliares/CODQGIS_RA.xlsx") {
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
    caminho_tabela = "auxiliares/tabela_localizacao_codeplan_2020.csv") {
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
