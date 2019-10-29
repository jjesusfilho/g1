#' Lê as notícias baixadas pela função baixar_noticias_g1
#'
#' @param arquivos Vetor de arquivos. Se NULL, informar diretório
#' @param diretorio Informar diretório onde estão os arquivos
#'
#' @return tibbe
#' @export
#'
#' @examples
#' \dontrun{
#' df <- ler_noticias_g1()
#' }
ler_noticias_g1<-function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio,pattern="html$",full.names=TRUE)

  }

  purrr::map_dfr(arquivos,~{

    x<- xml2::read_html(.x)

    headline<- xml2::xml_find_first(x,'//*[@class="content-head__title"]') %>%
      xml2::xml_text()


    intro <- xml2::xml_find_first(x,'//*[@class="content-head__subtitle"]') %>%
      xml2::xml_text()

    autoria <- xml2::xml_find_first(x,'//*[@class="content-publication-data__from"]') %>%
      xml2::xml_text(trim=TRUE)

    data_publicacao <- xml2::xml_find_first(x,'//*[@itemprop="datePublished"]') %>%
      xml2::xml_attr("datetime") %>%
      lubridate::as_datetime()

    data_atualizacao <- xml2::xml_find_first(x,'//*[@itemprop="dateModified"]') %>%
      xml2::xml_attr("datetime") %>%
      as_datetime()

    corpo <- xml2::xml_find_all(x,'//p[@class="content-text__container "]') %>%
      xml2::xml_text() %>%
      stringr::str_c(collapse="\n")

    tibble::tibble(headline,intro,autoria,data_publicacao,data_atualizacao,corpo)

  })
}
