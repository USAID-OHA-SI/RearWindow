#' Assemble various visuals for country MER performance review
#'
#' @param filepath_msd filepath to the OUxIM MSD (MSD must be saves as an rds) 
#' @param opunit operating unit to observe 
#'
#' @export
#' @importFrom dplyr %>%
#'
#' @examples
#' \dontrun{
#' rw_assemble_plots("~/Data/MER_Structured_Dataset_OU_IM_FY17-18_20180921_v2_2.rds", "Tanzania") }

rw_assemble_plots <- function(filepath_msd, opunit){
  
  #create a folder in the temp files to save all output viz
    tmp_fldr <- fs::path_temp()
    output_fldr <- fs::path(tmp_fldr, opunit) #create file path for OU
    if(dir.exists(output_fldr)) fs::dir_delete(output_fldr) #if the folder exists, delete it
    fs::dir_create(output_fldr) #create folder
  
  #print output directory location
    cat(paste0("\nOutput files can be found in this directory: \n", output_fldr))
  
  #setup data: filter to opunit and key ind and disaggs
    cat("\n\nread in data ... ")
    df_ou <- rw_compile(filepath_msd, opunit)
    cat(" complete")
    
  #identify the current period for saving purposes
    pd <- ICPIutilities::identifypd(df_ou) %>% toupper()
  
  #plot national achievement
    cat("\nplotting national achievement ... ")
    rw_plot_achv(df_ou) %>% 
      rw_save(output_fldr, opunit, pd, "1_Achievement")
    cat(" complete")
    
  #plot USAID achievement against national
    cat("\nplotting USAID achievement ...")
    rw_plot_achv_usaid(df_ou) %>% 
      rw_save(output_fldr, opunit, pd, "2_USAIDAchievement")
    cat(" complete")
    
  #plot agency comparison 
    cat("\nplotting agency comparison ...")
    rw_plot_achv_agency(df_ou) %>% 
      rw_save(output_fldr, opunit, pd, "3_AgencyComp")
    cat(" complete")
    
  #plot USAID HTS & HTS_POS modalities
    hts <- rw_plot_achv_hts(df_ou, "HTS_TST", "USAID")
    hts_pos <- rw_plot_achv_hts(df_ou, "HTS_TST_POS", "USAID")
    
    cat("\nplotting USAID HTS_TST by modality ...")
    rw_save(hts, output_fldr, opunit, pd, "4_HTS")
    cat(" complete")
    
    cat("\nplotting USAID HTS_TST_POS by modality ...")
    gridExtra::grid.arrange(hts, hts_pos, ncol = 2) %>% 
      rw_save(output_fldr, opunit, pd, "5_HTS_POS")
    cat(" complete")
    rm(hts, hts_pos)
   
  #plot USAID VMMC
    cat("\nplotting USAID VMMC by age ...")
    rw_plot_achv_age(df_ou, "VMMC_CIRC",  agency = "USAID") %>% 
      rw_save(output_fldr, opunit, pd, "6_VMMCage")
    cat(" complete")
  
  #mechanism table of indicator achievement
    cat("\nplotting USAID mechanism achievement table ...")
    rw_plot_mech_table(df_ou) %>% 
      rw_save(output_fldr, opunit, pd, "7_MechTable")
    cat(" complete")
    
  #identify mechanisms w/ indicators less than targeted achievement
    thres_med <- rw_addthresholds(df_ou)["med"] #identify threshold value
    mech_list <- rw_identify(df_ou, thres_med)
    rm(thres_med)
  #plot each mechanism
    cat("\nplotting mechanism trends for", nrow(mech_list)  ,"indicator/mechanism pairs ...")
    purrr::map2(.x = mech_list$indicator,
                .y = mech_list$mechanismid,
                .f = ~ rw_output_mech(df_ou, .x, .y, output_fldr))
    cat(" complete")
    
  #print output directory location
    cat(paste0("\n\nfin\nOutput files can be found in this directory: \n", output_fldr))
}