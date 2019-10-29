#' Baixa páginas do G1
#'
#' @param numero Número de páginas a serem baixadas
#' @param diretorio Diretório onde salvar as páginas
#'
#' @return html
#' @export
#'
#' @examples
#' \dontrun{
#' baixar_paginas_g1()
#' }
baixar_paginas_g1 <- function(numero=10,diretorio="."){

  if (!is.numeric(numero)|numero<1){
    stop("Fornecer um inteiro positivo maior que zero")

  }
  url<-paste0("https://g1.globo.com/sp/sao-paulo/index/feed/pagina-",1:numero,".ghtml")

  purrr::walk2(url,1:numero,purrr::possibly(~{

    arquivo <- file.path(diretorio,
                         paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.y,".html"))
    httr::GET(.x,httr::write_disk(arquivo,overwrite = TRUE))

  },NULL))

}


