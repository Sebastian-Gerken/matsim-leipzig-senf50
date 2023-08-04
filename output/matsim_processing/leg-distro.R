library(tidyverse)
library(RColorBrewer)
library(viridis)

dirs <- list.dirs("../", recursive = FALSE)

#pattern to filter dirs
filter_string <- "350it"
#filtering dirs with pattern
dirs <- dirs[grepl(filter_string, dirs)]


for (i in seq(1, 4)){

modes <- c("car", "bike", "walk", "pt")

  


data_list <- list()

for (dir in dirs){
  name <- str_split_1(dir,"-")
  name <- name[length(name)]
  
  #list .csv files
  files_csv <- list.files(dir, pattern = ".csv")
  
  #find legs file
  file <- files_csv[grepl("legs", files_csv)]
  
  #read legs file into list
  df <-  read_csv2(file.path(dir, file))
  
  df <- df %>%
    filter(mode %in% modes[i])
  
  data <- df %>%
    mutate(dep_time = as.POSIXct(dep_time)) %>%
    mutate(value = 1)%>%
    mutate(hour = lubridate::floor_date(dep_time, unit = "hours")) %>%
    group_by(hour) %>%
    summarise(value = sum(value))
  
  #add name to list
  df_name <- name # Create a unique name for the DataFrame
  
  data_list[[df_name]]  <- data
    
}

  df <- do.call(rbind, data_list)
  df$df_name <- rep(names(data_list), sapply(data_list, nrow))
  
  if (modes[i] == "walk"){
    step <- 200
  } else {
    step <- 50
  }
  
  
  ggplot(data = df) +
    geom_line(aes(x = hour, y = value, color = df_name)) +
    scale_x_datetime(date_breaks = "1 hour", date_labels ="%H:%M", expand = c(0, 0), 
                     limits = as.POSIXct(c("1970-01-01 01:00:00", "1970-01-02 01:00:00"))) +
    scale_y_continuous(breaks = c(seq(0, 5000, by = step))) +
    xlab("time [hh:mm]") +
    ylab("traffic volume count") +
    labs(color = "Scenarios:") +
    theme_light()+
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Adjust the size to your preference
      axis.text.y = element_text(size = 10),  # Adjust the size to your preference
      axis.title.x = element_text(size = 12),  # Adjust the size to your preference
      axis.title.y = element_text(size = 12),  # Adjust the size to your preference
      plot.title = element_text(size = 14),  # Adjust the size to your preference
      legend.position = "bottom",
      panel.grid.minor.x = element_blank()
    ) + 
    scale_color_manual(values = brewer.pal(6, "Set2")) 
  
  ggsave(file.path("plots", paste0("tvc_", modes[i],"_summary.pdf")), width = 10, height = 6, dpi = 100)
}
