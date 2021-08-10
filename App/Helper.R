library(tidyverse)

createLink <- function(uniprotID, gene) {
  sprintf('<a href="https://www.phosphosite.org/uniprotAccAction?id=%s" 
            target="_blank" 
            class="btn btn-primary">%s</a>',uniprotID,gene)
}



