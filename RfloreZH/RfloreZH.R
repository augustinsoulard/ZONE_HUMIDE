# Definition du repertoire de fichiers
WD = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(WD)

# Chargement des packages
if(!require("foreign")){install.packages("foreign")} ; library("foreign")
if(!require("reshape2")){install.packages("reshape2")} ; library("reshape2")
if(!require("tidyverse")){install.packages("tidyverse")} ; library("tidyverse")
if(!require("xlsx")){install.packages("xlsx")} ; library("xlsx")

# Charger le fichier d'export des releves
DATAPHYTO = read.csv(choose.files())

#Decroiser le tableau
MELT_DATA <- melt(DATAPHYTO, id.vars = c("CD_NOM","NomComplet","RELEVE"), measure.vars = c("ARBO", "ARBU", "HERB", "MUCINALE"))


# Attribution d'un poid pour chaque classe phyto

MELT_DATA = MELT_DATA %>% mutate(pondvalue = case_when(
  value == "5" ~ 87.5,
  value == "4" ~ 62.5,
  value == "3" ~ 37.5,
  value == "2" ~ 15,
  value == "1" ~ 3,
  value == "\\+" ~ 1,
  value == "r" ~ 0.5,
  value == "i" ~ 0.25,
  TRUE ~ 0
))
colnames(MELT_DATA) = c("CD_NOM","NOM_VALIDE","RELEVE","STRATE","value","pondvalue")

MELT_DATA = MELT_DATA[,c("RELEVE","STRATE","NOM_VALIDE","pondvalue","CD_NOM")]

#Jointure de l'indication ZH
FloreZH <- read.csv("FloreZH.csv")
JOIN_ZH = left_join(MELT_DATA,FloreZH,by=c("CD_NOM"="CODE.FVF"))

# Construction du fichier à exporter
JOIN_ZH_EXPORT = JOIN_ZH %>% select("CD_NOM","RELEVE","STRATE","NOM_VALIDE","pondvalue","INDIC_ZH")
JOIN_ZH_EXPORT = JOIN_ZH_EXPORT[JOIN_ZH_EXPORT$pondvalue>0,]
JOIN_ZH_EXPORT = arrange(JOIN_ZH_EXPORT,RELEVE,STRATE,desc(pondvalue))
JOIN_ZH_EXPORT[is.na(JOIN_ZH_EXPORT$INDIC_ZH),]$INDIC_ZH="Non"


# Recouvrement cumulé
JOIN_ZH_CUMUL = JOIN_ZH_EXPORT %>% group_by(RELEVE,STRATE) %>% mutate(cumulative_sum = cumsum(pondvalue))
SLICE_CUMUL= slice(group_by(JOIN_ZH_CUMUL,RELEVE,STRATE),1)
JOIN_ZH_CUMUL = JOIN_ZH_CUMUL[unique(c(which(JOIN_ZH_CUMUL$cumulative_sum<=50),which(JOIN_ZH_CUMUL$cumulative_sum<=50) + 1)),]
# Rajout des dépassants 50% sur la première valeure

JOIN_ZH_CUMUL = rbind(JOIN_ZH_CUMUL,SLICE_CUMUL)
JOIN_ZH_CUMUL$ID = paste0(JOIN_ZH_CUMUL$CD_NOM,JOIN_ZH_CUMUL$RELEVE,JOIN_ZH_CUMUL$STRATE)

JOIN_ZH_CUMUL = JOIN_ZH_CUMUL[!duplicated(JOIN_ZH_CUMUL$ID),]

# Nettoyage
JOIN_ZH_CUMUL = JOIN_ZH_CUMUL[!is.na(JOIN_ZH_CUMUL$cumulative_sum),]
JOIN_ZH_CUMUL = arrange(JOIN_ZH_CUMUL,RELEVE,STRATE,desc(pondvalue))
JOIN_ZH_CUMUL = data.frame(JOIN_ZH_CUMUL)

# Export du fichier final
write.xlsx(JOIN_ZH_CUMUL,"Bilan_esp_ZH_RELEVE.xlsx")

################################################################################
###################CREATION DU FICHIER RMARKDOWN################################
################################################################################
if(!require("knitr")){install.packages("knitr")} ; library("knitr")
if(!require("flextable")){install.packages("flextable")} ; library("flextable")

# Fonction qui enregistre les sorties de R pour 
con <- file("Analyse_phyto.Rmd", open = "wt", encoding = "UTF-8")
sink(con,split=T)
cat("---
title: \"Analyse phytosociologique\"
date: \"`r Sys.Date()`\"
output:
  word_document: 
    reference_docx: word_styles_references.dotm
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
``` \n \n")
LIST_RELEVE = levels(as.factor(DATARELEVE_JOIN$RELEVE))
for(i in 1:length(LIST_RELEVE)){
  cat("### Relevé : ",LIST_RELEVE[i],"\n \n")
  JOIN_DATA[JOIN_DATA$RELEVE==LIST_RELEVE[i],]
  cat("Tableau des espèces : 
```{r ",paste0("ESP",LIST_RELEVE[i]),",echo=TRUE}
flextable(JOIN_DATA[JOIN_DATA$RELEVE==LIST_RELEVE[",i,"],])
``` 
\n \n")
  cat("Tableau des relevés : 
```{r", paste0("RELEVE",LIST_RELEVE[i]),",echo=TRUE}
knitr::kable(DATARELEVE_JOIN[DATARELEVE_JOIN$RELEVE==LIST_RELEVE[",i,"],])
``` 
\n \n")
  cat(paste0("![](",DATAPHYTO[DATAPHYTO$RELEVE == LIST_RELEVE[i],]$RP_PHOTO1[1],") \n \n"))
  
}
sink()
close(con)

#Test knit
rmarkdown::render("Analyse_phyto.Rmd",output_format = "word_document",output_file = "Analyse_phyto.docx")

