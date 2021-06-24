library(dplyr)
library(tidyr)
library(neonUtilities)

##EXample for loading invertebrate data from one site (MART)
invert<-
  loadByProduct(dpID="DP1.20120.001",
                site=c("MART"), 
                startdate ="2016-01",
                check.size = F)

#pull out the files you want
invert.data<- 
  invert[[4]] %>%            
  as_tibble()

invert.field<-
  invert[[2]]%>%
  as_tibble()

#combine everything into one tibble
invert.all<-
  inner_join(invert.data,invert.field,by="sampleID")

#Calculate the invertebrate density from the count data
  NEONFILE.CLEAN<-invert.all %>%
    select(sampleID,scientificName,estimatedTotalCount,benthicArea)%>%
    mutate(Sample=substr(sampleID,1,11))%>%
    group_by(Sample,scientificName)%>%
    summarize(inv.density=sum(estimatedTotalCount)/sum(benthicArea))
  
#this step is to convert the data to wide format
#this format is needed for vegan or functional diversity analyses
  NEON2<-spread(NEONFILE.CLEAN,scientificName,inv.density)
    NEON2[is.na(NEON2)]

    
  

