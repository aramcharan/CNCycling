#Clear lists
rm(list = ls())
#Set working Directory
setwd("C:/Users/amr418/Desktop/CYCLESOutput")

# Create shortcut root directory name
Root.dir<- c("C:/Users/amr418/Desktop/CYCLESOutput")


# Define folders to parse through
Location<-c("Lebanon","RockSpring","Beltsville");
Soil<-c("Soil1","Soil2");
Fertilizer<-c("Manure","SynFert");
Management<-c("Fallow","CC","AllRye","RyeStover","FertRye","FertRyeStover");

# Collect all seasonal data for a location in this list 
results <- list()

# For Loop that runs through every terminal node in folder
for (l in Location) {
  for (k in Soil) {
    for (j in Fertilizer) {
      for (i in Management) {
  
      work.dir.name<-file.path(Root.dir,l,k,j,i);
  
      #Grab season data from 2 season files output from 2 rotations (corn-soybean, soybean-corn)
      season_data_1<-read.table(file.path(work.dir.name,"season.dat"),header = TRUE, skip = 1);
      season_data_2<-read.table(file.path(work.dir.name,"season2.dat"),header = TRUE, skip = 1);
      season_data<- rbind(season_data_1, season_data_2);
  
  
  
      colnames(season_data) <- c("DATE","CROP","TOTAL_BIOMASS","ROOT_BIOMASS","GRAIN","FORAGE","RESIDUE","HI","POT_TRANS","ACT_TRANS","SOIL_EVAP","TOTAL_N","ROOT_N","GRAIN_N","FORAGE_N","N_STRESS","N_HARVEST","N_RESIDUE","%N_FORAGE");
  
      #### Calculate average and standard deviation in output for each crop
  
      subset_maize <- season_data[which(season_data[,2] == "Maize"),3:ncol(season_data)]
      maize_avg <- apply(subset_maize,2,mean,na.rm = TRUE)
      maize_sd <- apply(subset_maize,2,sd,na.rm=TRUE)
  
      subset_soybean <- season_data[which(season_data[,2] == "Soybean"),3:ncol(season_data)]
      soybean_avg <- apply(subset_soybean,2,mean,na.rm = TRUE)
      soybean_sd <- apply(subset_soybean,2,sd,na.rm=TRUE)
  
      subset_rye_bm <- season_data[which(season_data[,2] == "RyeBioenergyBM"),3:ncol(season_data)]
      rye_bm_avg <- apply(subset_rye_bm,2,mean,na.rm = TRUE)
      rye_bm_sd <- apply(subset_rye_bm,2,sd,na.rm = TRUE)
  
      subset_rye_bs <- season_data[which(season_data[,2] == "RyeBioenergyBS"),3:ncol(season_data)]
      rye_bs_avg <- apply(subset_rye_bs,2,mean,na.rm = TRUE)
      rye_bs_sd <- apply(subset_rye_bs,2,sd,na.rm = TRUE)
  
      subset_triticale <- season_data[which(season_data[,2] == "Triticale"),3:ncol(season_data)]
      triticale_avg <- apply(subset_triticale,2,mean,na.rm = TRUE)
      triticale_sd <- apply(subset_triticale,2,sd,na.rm = TRUE)
  
  
      crop_avg = data.frame(rbind(maize_avg,soybean_avg, rye_bm_avg, rye_bs_avg, triticale_avg,maize_sd,soybean_sd, rye_bm_sd, rye_bs_sd, triticale_sd, row.names = NULL));
  
      rownames(crop_avg) <- c("maize","soybean", "rye_bm","rye_bs","triticale","maize_sd","soybean_sd","rye_bm_sd","rye_bs_sd","triticale_sd");
  
      crop_avg$Location<-rep(paste(l),length(row.names(crop_avg)));
      crop_avg$Soil<-rep(paste(k),length(row.names(crop_avg)));
      crop_avg$Fertilizer<-rep(paste(j),length(row.names(crop_avg)));
      crop_avg$Management<-rep(paste(i),length(row.names(crop_avg)));
  
      results <- rbind(results,crop_avg)
  
      write.csv(results,"season_test.csv")
          }
     }
  }
}
 

#####
#SUMMARY
#####

# Collect all season data for a location in this list 
summary <- list()

# For Loop that runs through every terminal node in folder
for (l in Location) {
 for (k in Soil) {
    for (j in Fertilizer) {
      for (i in Management) {
        
        work.dir.name<-file.path(Root.dir,l,k,j,i);

        #Grab season data from 2 season files output from 2 rotations (corn-soybean, soybean-corn)
        summary_data_1<-read.table(file.path(work.dir.name,"summary.dat"), skip = 2,nrows = 1);
        summary_data_2<-read.table(file.path(work.dir.name,"summary2.dat"), skip = 2, nrows = 1);
        
        summary_data_3<-read.table(file.path(work.dir.name,"summary.dat"), skip = 6, nrows = 1);
        summary_data_4<-read.table(file.path(work.dir.name,"summary2.dat"), skip = 6, nrows = 1);
        
        # Combine rows from different rotations
        Row1 <- rbind(summary_data_1[1,],summary_data_2[1,])
        Row2 <- rbind(summary_data_3[1,],summary_data_4[1,])
        
        # Combine columns from differe rotations
        Row_all <-cbind(Row1,Row2)
        
        # Average and standard deviation of Data
        summary_avg <- apply(Row_all,2,mean,na.rm = TRUE)  
        summary_sd <- apply(Row_all,2,sd,na.rm = TRUE)
        
        crop_summary = data.frame(rbind(summary_avg,summary_sd))
        colnames(crop_summary) <- c("Init_Prof_C","Fin_Prof_C","Prof_C_Diff", "Res_C_Input", "Root_C_Input","Hum_C", "Resp_C", "Resp_Res_C", "Ret_Res","Prod_Root", "Soil_C_Ch_per_yr","Avg_Gross_N_Min","Avg_N_Imm", "Avg_Net_Min","Avg_NH4_Nitr","Avg_N20_Nitr", "Avg_NH3_Vol","Avg_NO3_Denit","Avg_N2O_Denit","Avg_Nit_Leach","Avg_Amm_Leach","Avg_Total_N20_Emm");
        
        crop_summary$Location<-rep(paste(l),length(row.names(crop_summary)));
        crop_summary$Soil<-rep(paste(k),length(row.names(crop_summary)));
        crop_summary$Fertilizer<-rep(paste(j),length(row.names(crop_summary)));
        crop_summary$Management<-rep(paste(i),length(row.names(crop_summary)));
        
        summary <- rbind(summary,crop_summary)
        
        write.csv(summary,"scenario_test.csv")
      }
    }
  }
}





