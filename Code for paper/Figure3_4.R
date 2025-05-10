
rm(list=ls())

library(maps)
library(mapdata)
library(sp)
library(ukgeog)
#library(maptools) # Not in use since Oct 2023
#library(rgdal) # Not in use since Oct 2023
library(ggmap)
library(ggplot2)
#library(rgeos) # Not in use since Oct 2023
library(broom)
library(plyr)
library(sf)
library(latex2exp)
library(dplyr)
library(magrittr)
library(ggrepel)

FileSource1 = "~/Desktop/Chapter 1_Forecast/results/" ##Results directory##
FileSource2 = "~/Desktop/Chapter 1_Forecast/data/" ##Data directory##

shpName = paste(FileSource2,"Local_Authority_Districts_May_2023_Boundaries_UK_BGC/LAD_MAY_2023_UK_BGC_V2.shp",sep = "")
shapefile<-read_sf(shpName)
dataName = paste(FileSource1,"ModelResiduals_ReMSPE_Individual_Lags1_PreTime1.rda",sep = "")

        load(file = dataName)
        l_effect <- ReMSPE_Individual_Map
 
    for (variable_shortName in colnames(l_effect)) { 
        
        variable_longName <- variable_shortName
        
        df_effect <- NULL
        
      
        df_effect <- as.matrix(l_effect[,variable_shortName]) 
        LADcodeName = paste(FileSource2,"Lad_Region_Code.csv",sep = "")
        regionalcode<-read.csv(file = LADcodeName,header = T)
        code<-as.matrix(regionalcode)
        Ladcode = code[,1]

        
        
        df_effect = data.frame(Ladcode = Ladcode,variable = df_effect)
        
        v_breaks1 <- c(-Inf, 1 , Inf) # (9,)
        
        df_effect %<>% mutate(var_inte = cut(variable, breaks = v_breaks1, labels = FALSE)) %>%
          mutate(var_char = as.character(var_inte)) 
        # merge with spatial coordinates
        df_sf_effect <- left_join(shapefile, df_effect, by = c("LAD23CD" = "Ladcode")) # (377,5)
        
        df_sf_effect %<>% mutate(var_char = if_else(is.na(var_char), "3", var_char))
        
        v_levels <- c("1", "2", "3")
        df_sf_effect %<>% mutate(var_fact = factor(var_char, levels = v_levels, ordered = TRUE))
        
        # maps
        map <- ggplot()
        

        map <- map + geom_sf(
          data    = df_sf_effect,
          mapping = aes(fill = var_fact),
          color   = "light grey", 
          size    = 0.1,
          inherit.aes = FALSE)
        
        v_values <- c(
          "1" = "#08519C",
          "2" = "#D55E00",
          "3" = "gray97")
        
        ### labels
        v_labels <- vector("character", length(v_values))
        v_labels[1] <- sprintf("<1")
        v_labels[2] <- sprintf(">1")
        v_labels[3] <- "No Data"
        
        
        map <- map + scale_discrete_manual(
          aesthetics = c("fill"),
          values     = v_values, 
          breaks     = v_levels, 
          labels     = v_labels, 
          drop       = FALSE 
        )
        
        map <- map + 
          theme(
            axis.text         = element_blank(),
            axis.ticks        = element_blank(),
            panel.background  = element_blank(),
            panel.grid        = element_blank(),
            legend.title      = element_text(size = 10), 
            legend.text       = element_text(size = 6),
          ) +
          labs(
            x    = NULL,
            y    = NULL,
            fill = sprintf("%s", variable_longName)
          )
        
       
        dev.new()
        plot(map)
        
       
        
        # save
          sub_filename <- sprintf("map_%s", variable_shortName)
          filename <- paste(FileSource1,sub_filename,sep = "")
          ggsave(filename = sprintf("%s.png", filename), plot     = map,  device   = "png",dpi      = 300)
          ggsave(filename = sprintf("%s.pdf", filename),plot     = map, device   = pdf)
         
    }


