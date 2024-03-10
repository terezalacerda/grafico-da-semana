trata_dados <- function(dados, ano_atual) {
  dados |> 
  dplyr::filter(stringr::str_detect(drs_cod, "Total")) |> 
  dplyr::select(janeiro_notificados:total_confirmados.importados) |> 
  tidyr::pivot_longer(cols = dplyr::everything()) |> 
  dplyr::mutate(
    mes = stringr::str_extract(name, ".+_") |> stringr::str_remove("_"),
    tipo = stringr::str_extract(name, "_.+") |> stringr::str_remove("_") |> stringr::str_replace("\\.", "_"),
    ano = ano_atual
  ) |> 
  dplyr::rename(
    num = value
  ) |> 
  dplyr::select(-name)
}

nomes <- c("drs_cod", "drs_nome", "gve_cod", "gve_nome", 
           "regiao_cod", "regiao_nome", "muni_residencia", 
           "janeiro_notificados", "janeiro_confirmados.autoctones", "janeiro_confirmados.importados",
           "fevereiro_notificados", "fevereiro_confirmados.autoctones", "fevereiro_confirmados.importados",
           "marco_notificados", "marco_confirmados.autoctones", "marco_confirmados.importados",
           "abril_notificados", "abril_confirmados.autoctones", "abril_confirmados.importados",
           "maio_notificados", "maio_confirmados.autoctones", "maio_confirmados.importados",
           "junho_notificados", "junho_confirmados.autoctones", "junho_confirmados.importados",
           "julho_notificados", "julho_confirmados.autoctones", "julho_confirmados.importados",
           "agosto_notificados", "agosto_confirmados.autoctones", "agosto_confirmados.importados",
           "setembro_notificados", "setembro_confirmados.autoctones", "setembro_confirmados.importados",
           "outubro_notificados", "outubro_confirmados.autoctones", "outubro_confirmados.importados",
           "novembro_notificados", "novembro_confirmados.autoctones", "novembro_confirmados.importados",
           "dezembro_notificados", "dezembro_confirmados.autoctones", "dezembro_confirmados.importados",
           "total_notificados", "total_confirmados.autoctones", "total_confirmados.importados")

dados_raw_22 <- readxl::read_excel("data/dengue/dengue2022_mes.xlsx", skip = 2)

colnames(dados_raw_22) <- nomes

dados_22 <- dados_raw_22 |> 
  trata_dados(2022)

dados_raw_23 <- readxl::read_excel("data/dengue/dengue23_mes.xlsx", skip = 1)

colnames(dados_raw_23) <- nomes

dados_23 <- dados_raw_23 |> 
  trata_dados(2023)

dados_raw_24 <- readxl::read_excel("data/dengue/dengue24_mes.xlsx", skip = 1)

colnames(dados_raw_24) <- c("drs_cod", "drs_nome", "gve_cod", "gve_nome", 
                        "regiao_cod", "regiao_nome", "muni_residencia", 
                        "janeiro_notificados", "janeiro_confirmados.autoctones", "janeiro_confirmados.importados",
                        "total_notificados", "total_confirmados.autoctones", "total_confirmados.importados")

dados_24 <- dados_raw_24 |> 
  trata_dados(2024)

aux <- data.frame(
  mes = c("janeiro", "fevereiro", "marco", "abril", "maio", "junho",
          "julho", "agosto", "setembro", "outubro", "novembro", "dezembro"),
  numero = 1:12
)

dados <- dados_22 |> 
  dplyr::bind_rows(dados_23) |> 
  dplyr::bind_rows(dados_24) |> 
  dplyr::left_join(aux) |> 
  dplyr::mutate(
    data = lubridate::make_date(year = ano, month = numero)
  ) |> 
  dplyr::select(data, tipo_caso = tipo, num_casos = num, ano, mes = numero, mes_nome = mes) |> 
  dplyr::filter(mes != "total") |> 
  dplyr::mutate(
    num_casos = as.numeric(num_casos)
  ) |> 
  dplyr::mutate(
    tipo = dplyr::case_when(
      tipo_caso == "notificados" ~ "notificados",
      stringr::str_detect(tipo_caso, "confirmados") ~ "confirmados"
    )
  ) |> 
  dplyr::group_by(data, mes, mes_nome, ano, tipo) |> 
  dplyr::summarise(
    num_casos = sum(num_casos)
  )

readr::write_rds(dados, "data/dados_dengue.rds")
