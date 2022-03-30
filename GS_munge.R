rv<-qread("D:/FAMEshiny/results/RawFH_FireHistory_06-0pc_zones_2022to2050_r02_merged_LF_DISTRICT_with_PU_field_TFI_GS.qs")
gc()
TimeNames <- paste0("GS_", as.character(rv$FHAnalysis$TimeSpan))

GS_Summary_PU_LONG <-rv$GS_Summary$GS_Summary_wide%>%
  dplyr::select(-c(MIN_LO_TFI,
                   MIN_HI_TFI, MAX_TFI, Index, FH_ID, FIRE_REG, FIREFMZ,
                   DELWP)) %>%
  tidyr::pivot_longer(tidyselect::all_of(TimeNames),
                      names_to = "SEASON", values_to = "GS") %>% 
  dplyr::group_by(EFG, EFG_NAME, PLM, FIRE_FMZ_NAME, FIRE_FMZ_SHORT_NAME, 
                  FIRE_REGION_NAME,PU, DELWP_REGION, SEASON, GS) %>%
  dplyr::summarise(Pixels = sum(nPixel),
                   Hectares = sum(Hectares)) %>%
  dplyr::mutate(SEASON = gsub("GS_","",SEASON))
  
