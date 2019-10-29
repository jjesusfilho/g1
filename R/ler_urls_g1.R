#' Lê as urls contidas nas páginas baixadas por baixar_paginas_g1
#'
#' @param arquivos Se NULL, informar diretório
#' @param diretorio Indicar diretório se não indicou arquivos
#'
#' @return character vector
#' @export
#'
#' @examples
#' \dontrun{
#' urls <- ler_urls_g1(diretorio=".")
#' }
ler_urls_g1<-function(arquivos=NULL, diretorio="."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio,pattern="html$",full.names=TRUE)
  }

  purrr::map(arquivos,~{

    x <- xml2::read_html(.x)

    xml2::xml_find_all(x,"//*[@class='_et']/a") %>%
      xml2::xml_attr("href")


  }) %>%
    unlist()

}
