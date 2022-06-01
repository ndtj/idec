

# download tjsp -----------------------------------------------------------

lex::tjsp_cjsg_download(
  '"sumula 597"',
  dir = "data-raw/tjsp_cjsg",
  classe = "198"
)

dados_tjsp <- fs::dir_ls("data-raw/tjsp_cjsg") |>
  purrr::map_dfr(lex::tjsp_cjsg_parse) |>
  dplyr::distinct(n_decisao, .keep_all = TRUE) |>
  dplyr::mutate(data_julgamento = lubridate::dmy(data_julgamento)) |>
  dplyr::filter(lubridate::year(data_julgamento) >= 2015)

writexl::write_xlsx(dados_tjsp, "data-raw/dados_tjsp.xlsx")



# download tjrs -----------------------------------------------------------


# lex::tjrs_cj
baixar_pagina <- function(pagina) {
  u0 <- "https://www.tjrs.jus.br/novo/buscas-solr/?aba=jurisprudencia&q=&conteudo_busca=ementa_completa"
  r0 <- httr::GET(u0)
  u <- "https://www.tjrs.jus.br/buscas/jurisprudencia/ajax.php"
  body <- list(
    "action" = "consultas_solr_ajax",
    "metodo" = "buscar_resultados",
    "parametros" = stringr::str_glue("aba=jurisprudencia&realizando_pesquisa=1&pagina_atual={pagina}&q_palavra_chave=%22s%C3%BAmula+597%22&conteudo_busca=ementa_completa_aspas&filtroComAExpressao=&filtroComQualquerPalavra=&filtroSemAsPalavras=&filtroTribunal=-1&filtroRelator=-1&filtroOrgaoJulgador=-1&filtroTipoProcesso=-1&filtroClasseCnj=-1&assuntoCnj=-1&filtroReferenciaLegislativa=&filtroJurisprudencia=&filtroComarcaOrigem=&filtroAssunto=&data_julgamento_de=&data_julgamento_ate=&filtroNumeroProcesso=&data_publicacao_de=&data_publicacao_ate=&filtroacordao=acordao&facet=on&facet.sort=index&facet.limit=index&wt=json&ordem=desc&facet_orgao_julgador=&facet_origem=&facet_relator_redator=&facet_ano_julgamento=&facet_nome_classe_cnj=&facet_nome_assunto_cnj=&facet_nome_tribunal=&facet_tipo_processo=&facet_mes_ano_publicacao=&start=0")
  )
  r <- httr::POST(
    u, body = body,
    encode = "form"
    # httr::add_headers("Content-Type" = "application/x-www-form-urlencoded; charset=UTF-8")
  )
  resultados <- httr::content(r, "text") |>
    jsonlite::fromJSON(simplifyDataFrame = TRUE)

  dados <- resultados[[2]]$docs |>
    tibble::as_tibble() |>
    dplyr::mutate(n_tot = resultados[[2]]$numFound)

  dados
}

n_paginas <- function() {
  baixar_pagina(1)$n_tot[1] %/% 10 + 1
}

npags <- n_paginas()

tjrs_raw <- purrr::map_dfr(seq_len(npags), baixar_pagina, .id = "pagina")

dados_tjrs <- tjrs_raw |>
  dplyr::transmute(
    n_processo = numero_processo,
    n_decisao = cod_ementa,
    id = pagina,
    classe_assunto = paste(nome_classe_cnj, nome_assunto_cnj, sep = " / "),
    comarca = origem,
    data_julgamento = data_julgamento,
    data_publicacao = data_publicacao,
    ementa = ementa_completa,
    orgao_julgador = orgao_julgador,
    relatora = nome_relator,
    assunto = assunto,
    outros_numeros = cod_documento
  ) |>
  tidyr::unnest(ementa) |>
  dplyr::mutate(data_julgamento = as.Date(lubridate::ymd_hms(data_julgamento))) |>
  dplyr::filter(lubridate::year(data_julgamento) >= 2015)

writexl::write_xlsx(dados_tjrs, "data-raw/dados_tjrs.xlsx")
