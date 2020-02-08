

## n = Total de daus a jugar
## daus = vector amb colors dels daus. Per defecte hi ha 13 daus, 6 verds, 4 grocs 3 vermells.
## daus = vector amb colors dels daus obligatoris 

jugar <- function(n = 3, daus = NULL, daus_obligatoris = NULL){
  ## daus que hi ha al cubilet
  if(is.null(daus)){
    daus <- c(rep("verd",6),rep("groc",4),rep("vermell",3))
  } 
  
  # mostra de 3 daus del cubilet sense reemplaçament
  if(is.null(daus_obligatoris)){
    d3_samp <- sample(daus,n, replace = F)
  }else{
    d3_samp <- c(daus_obligatoris, sample(daus, n - length(daus_obligatoris), replace = F))
  }
  
  
  
  # tirem els tres daus escollits
  res <- tirar_daus(d3_samp)
  
  colors <- unique(daus)
  daus_restants <- NULL
  for (i in seq_along(colors))  {
    if(   any(names(res$ndaus) == colors[i])   ){
      daus_restants <- c(daus_restants, daus[daus == colors[i]][- c(1:res$ndaus[colors[i]])])
    }else{
      daus_restants <- c(daus_restants, daus[daus == colors[i]])
    }
  } 
  
  # daus que hi ha a sobre de la taula
  TAULA <<- c(TAULA,res$resultat)
  sum_taula <- table(TAULA)
  
  # Comprovació de si pot seguir jugant 
  
  if (any(names(sum_taula) == "BANG!")) {if(sum_taula["BANG!"] >= 3) {
    warning("HAS PERDUT! \n",
            "Tirada actual: ", paste0(res$resultat,"(",names(res$resultat),")", collapse = ", "),
            "\nTAULA \n", paste0(names(sum_taula),": ", sum_taula, "\n"),call. = F)
  }}
  
  return(list(resultat = res$resultat, 
              passos = res$passos, 
              daus_restants = daus_restants, 
              dau_paint(res$resultat),
              sum_taula = sum_taula))
  
}




# daus3 vector caracter amb colors dels daus que es volen tirar

tirar_daus <- function(daus3){
  verd <- c(rep("Cereeebbrrooo",3),rep("--->",2), rep("BANG!",1))
  groc <- c(rep("Cereeebbrrooo",2),rep("--->",2), rep("BANG!",2))
  vermell <- c(rep("Cereeebbrrooo",1),rep("--->",2), rep("BANG!",3))
  res3 <- unlist(lapply(daus3, function(x) {switch(x,
                                                   "verd" = sample(verd,1),
                                                   "groc" = sample(groc,1),
                                                   "vermell" = sample(vermell,1))}))
  names(res3) <- daus3
  ndaus <- table(names(res3))
  
  
  # passos <<- names(which(res3 == "--->"))
  return(list(resultat = res3, passos = names(which(res3 == "--->")), 
              ndaus = ndaus ))
}


# res_daus vector de resultats. Ha de tenir com a nom el color de cada dau. 
# EXEMPLE
#  groc            groc            verd          
# "BANG!"         "BANG!"         "Cereeebbrrooo" 

dau_paint <- function(res_daus) {
  
  info_res <- NULL
  for(i in seq_along(res_daus)){
    info_res <- c(info_res, switch(names(res_daus)[i],
                                   "verd" = paste("green('",res_daus[i],"')") ,
                                   "groc" = paste("yellow('",res_daus[i],"')"),
                                   "vermell" = paste("red('",res_daus[i],"')"))    )
  }
  return(eval(parse(text =paste("cat(",paste(info_res,collapse = ","),")"))))
}




# 🔪
# 
# 👣

# Exemple d'una jugada
# jugar(daus = tots)



# 
# calcul_probabilitats <- function(n = 3, daus = NULL, daus_obligatoris = NULL){
#   if(is.null(daus)){
#     daus <- c(rep("verd",6),rep("groc",4),rep("vermell",3))
#   } 
#   p_col <- prop.table(table(daus))
#   cat("Probabilitat d'un dau:\n", paste(names(p_col),":", round(p_col,3), "\n ", collapse = "" ) )
#   cat("Probabilitat de 2 daus:\n", paste(names(p_col),":", round(p_col^2,3), "\n ", collapse = "" ) )
#   cat("Probabilitat de 3 daus:\n", paste(names(p_col),":", round(p_col^3,3), "\n ", collapse = "" ) )
# }


# # mostra de tres daus del cubilet
# d3 <- sample(tots,6, replace = F)
# 
# # tirar 3 daus ja seleccionats
# tdaus <- tirar_daus(d3)
# 
# dau_paint(tdaus$resultat)



