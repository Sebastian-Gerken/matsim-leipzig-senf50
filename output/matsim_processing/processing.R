library(tidyverse)
library(RColorBrewer)
library(viridis)

dirs <- list.dirs("../", recursive = FALSE)

#pattern to filter dirs
filter_string <- "350it"
#filtering dirs with pattern
dirs <- dirs[grepl(filter_string, dirs)]

data_list <- list()

file_patterns <- c("leipzig-1pct.modestats", "leipzig-1pct.ph_modestats", "leipzig-1pct.pkm_modestats", "leipzig-1pct.scorestats")

file_names <- c("modestat", "ph_modestats", "pkm_modestats", "scorestats")
x_vals <- c("modes", "mode_traveltime", "modes", "")
y_vals <- c("modal share [%]", "person hours", "pkm", "scores")

counter <- 0
#loop over file patterns ##pattern <- file_patterns[2]
for (pattern in file_patterns){
  
  #loop over all matching dirs ##dir <- dirs[1]
  counter  <- counter + 1
  file_name <- file_names[counter]
  x_val <- x_vals[counter]
  y_val <- y_vals[counter]
  
  for (dir in dirs){
    # Get all files in the current directory with the ".txt" extension
    files_txt <- list.files(path = dir, pattern = ".txt")
    
    # Find the file with a specific pattern (e.g., 'pattern')
    file <- files_txt[grepl(pattern, files_txt)]
    
    #name
    name <- str_split_1(dir,"-")
    
    # Read the file and add it to the data_list as a data frame with the specified name
    df_name <- name[length(name)] # Create a unique name for the DataFrame
    data_list[[df_name]] <- read.delim(file.path(dir, file))
  }
  
  #unpack files
  df_combined <- do.call(rbind, data_list)
  df_combined$df_name <- rep(names(data_list), sapply(data_list, nrow))

  last_rows <- seq(351, nrow(df_combined), by = 351)

  df_final_iteration <- subset(df_combined[last_rows,])
  
  if ("Iteration" %in% names(df_final_iteration)){
    df_final_iteration <- df_final_iteration %>%
      select(-Iteration)
  } else if ("ITERATION" %in% names(df_final_iteration)) {
    df_final_iteration <- df_final_iteration %>%
      select(-ITERATION)
  }
  
  #print(file_name)
  #print(df_final_iteration)
  
  if (pattern == "leipzig-1pct.ph_modestats"){
    df_final_iteration <- df_final_iteration %>%
      select(-bike_wait, -car_wait, -freight_wait, -ride_wait, -stageActivity_travel, -stageActivity_wait, -walk_wait)
  }
  
  
  #get names form df
  colls <- names(df_final_iteration)
  patterns_to_remove <- c("df_name", "ITERATION", "Iteration")
  colls <- colls[!grepl(paste(patterns_to_remove, collapse = "|"), colls)]
    
  #make long df format
  df_final_iteration <- df_final_iteration %>%
    pivot_longer(all_of(colls))
  
  #get value names
  
  ggplot(data = df_final_iteration, aes(x=name, y=value, fill = df_name))+
    geom_bar(stat = "identity", position = "dodge") +
    #geom_text(aes(label = round(value, 2)), position = position_dodge(0.9), vjust = -0.5)+
    xlab(x_val) +
    ylab(y_val) +
    scale_y_continuous(labels = function(x)format(x, scientific = FALSE)) +
    labs(fill = "Scenarios:") +
    theme_light()+
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Adjust the size to your preference
      axis.text.y = element_text(size = 10),  # Adjust the size to your preference
      axis.title.x = element_text(size = 12),  # Adjust the size to your preference
      axis.title.y = element_text(size = 12),  # Adjust the size to your preference
      plot.title = element_text(size = 14),  # Adjust the size to your preference
      legend.position = "bottom"
    ) +
    scale_fill_manual(values = brewer.pal(6, "Set2")) 
  width <- 9
  height <- 6
  
  
  ggsave(paste0("plots/",file_name, ".pdf"), width = width, height = height)
  ggsave(paste0("plots/",file_name, ".jpg"), width = width, height = height)
    
  #empty list
  data_list <- list()
}

