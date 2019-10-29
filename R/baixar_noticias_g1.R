#' Baixar notícias do g1 a partir das urls obtidas pela função baixar_url_g1
#'
#'
#' @param urls Vetor de  urls
#' @param diretorio Local onde serão salvas as notícias
#'
#' @return html
#' @export
#'
#' @examples
#' \dontrun{
#' baixar_noticias_g1(urls = urls)
#' }
baixar_noticias_g1 <- function(urls, diretorio="."){

  purrr::walk2(urls,1:length(urls),purrr::possibly(~{

    arquivo <- file.path(diretorio,
                         paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_noticia_",.y,".html"))

    httr::GET(.x,httr::write_disk(arquivo,overwrite = TRUE))



  },NULL))


}
