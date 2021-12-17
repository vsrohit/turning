library(tidyverse)
library(cowplot) #Built over ggplot2 provides grid plot and useful save plot function
library(moments) # For Kurtosis and skewness

# All the functions have use ilist as input and olist as output
# Data import --------
get_exp_data <- function(){
  # Input  - Nothing But make sure expi.csv files are present 
  # Output - Gives a list with each element a data frame 
  #          with data of the experiment
  # Job    - It reads data from csv and creates a new exp_data list
  
  exp_data <- list()
  
  for(i in 1:nExp){
    exp_data[[i]] <- read_csv(str_c("exp",i,".csv"), 
                              col_names = FALSE)
    names(exp_data[[i]]) <- c("t", "Fx", "Fy", "Fz", "Mx", "My", "Mz")
  }
  exp_data
}

get_component <- function(ilist, col_num){
  # Input  - ilist   -> Input list with all components
  #          col_num -> Column number of the component
  #          Note: Passing component name through function is not working with 
  #                Select
  # Output - Gives a list with (t, Component) data frames 
  # Job    - Selects (t, component) based on the column number given
  olist <- list()
  for(i in 1:nExp){
    olist[[i]] <- ilist[[i]]%>%
      select(t, col_num)
  }
  olist
}

# Data preparation -------------

add_frame_numbers <- function(ilist){
  # Input  - ilist   -> Input list
  # Output - Has an extra column with Frame_number name 
  # Job    - Add frame numbers to data
  
  olist <- list()
  for(i in 1:nExp){
    olist[[i]] <- ilist[[i]] %>%
      mutate(Frame_number = floor(t*sf/f+1)) %>%
      filter(Frame_number != max(Frame_number)) #Removes last frame number
  }
  olist
}

adjust_time <- function(ilist){
  # Add frame numbers to data
  # Input  - ilst -> Input list with time as first column
  # Output - List with modified time
  # Job    - The time is readjusted to make it easy for frame number calculation
  olist <- list()
  for(i in 1:nExp){
    t_minus <- as.numeric(ilist[[i]][1,1])
    olist[[i]] <- ilist[[i]] %>%
      mutate(t = t - t_minus)
  }
  olist
}

get_continuous_data <- function(ilist){
  # Input  - ilist -> Has to be raw data with time and components
  #          It also needs data about start_c and end_c which are the end result
  #          of first step
  # Output - Gives a list with each element a data frame 
  #          with data of the experiment
  # Job    - Gives continuous data
  
  olist <- list()
  for(i in 1:nExp){
    olist[[i]] <- ilist[[i]][start_c[i]:end_c[i],]
  }
  olist
}

get_entry_data <- function(ilist){
  # Input  - ilist -> Has to be raw data with time and components
  #          It also needs data about start_e and end_e which are the end result
  #          of first step
  # Output - Gives a list with each element a data frame 
  #          with data of the experiment
  # Job    - Gives entry data
  
  olist <- list()
  for(i in 1:nExp){
    olist[[i]] <- ilist[[i]][start_e[i]:end_e[i],]
  }
  olist
}

make_frames_as_factors <- function(ilist){
  # Input  - List with frame numbers and Component data
  # Output - Suitable for developing violin plots 
  # Job    - Arranges data as (Frame_number, Fz and then converts Frame_number
  #          into a factor)
  olist <- list()
  for(i in 1:nExp){
    olist[[i]] <- ilist[[i]] %>%
      select(Frame_number, Fz) %>%
      mutate(Frame_number = as.factor(Frame_number))
  }
  olist
}

# Statistical calculations ---------

frame_stat <- function(ilist, stat_func){
  # Input  - ilist     -> Input list with Frame number added
  #        - stat_func -> The function that you want to evaluate for each frame
  # Output - (Frame number, Stat) 
  # Job    - All the time data is summarized for each frame
  olist <- list()
  for(i in 1:nExp){
    olist[[i]] <- ilist[[i]] %>%
      group_by(Frame_number) %>%
      summarize(stat = stat_func(Fz)) %>%
      filter(!is.na(stat)) 
  }
  olist
}

trend <- function(ilist){
  #For the given list, it uses linear model to calculate the trend and returns
  #the value for all experiments
  # Input  - The list must have stat and Frame_Number 
  # Output - a vector of trend for all the elements of the list
  # Job    - Uses linear model and its coefficients to calculate the slope
  trendz <- vector()
  for(i in 1:nExp){
    x <- select(ilist[[i]], stat, Frame_number)
    k <- coef(lm(x))
    trendz[i] <- k[2]
  }
  trendz
}

full <- function(ilist, func){
  #Calculates statistical quantities for a given data frame that has
  # Fz as its column
  fullz <- vector()
  for (i in 1:nExp){
    fullz[i] <- func(ilist[[i]]$Fz)
  }
  fullz
}
# Plotting -------
find_range <- function(ilist,col_num){
  # Helper function for stat_plot
  rlist <- list(c(NA,NA))
  for(i in 1:nExp){
    rlist[[i]] <- range(ilist[[i]][,col_num])
  }
  range(rlist)
}

stat_plot <- function(ilist, xlabel, ylabel){
  #Plot all the graphs in a single page and save it 
  axis_name <- names(ilist[[1]])
  x_scale <- scale_x_continuous(limits = find_range(ilist, 1))
  y_scale <- scale_y_continuous(limits = find_range(ilist, 2))
  
  plot_list <- list()
  for(i in 1:nExp){
    plot_list[[i]]<- ggplot(ilist[[i]],
                            aes_string(x = axis_name[1], y = axis_name[2])) +
      geom_point()+
      xlab(xlabel)+ylab(ylabel)+
      x_scale +
      y_scale
  }
  p <- plot_grid(plotlist = plot_list, labels = "AUTO",ncol = 2)
  
  p
}

plot_violin <- function(ilist){
  
  axis_name <- names(ilist[[1]])
  y_scale <- scale_y_continuous(limits = find_range(ilist, 2))
  
  plot_list <- list()
  for(i in 1:nExp){
    p <- ggplot(ilist[[i]],aes(x = Frame_number, y = Fz))+
      geom_violin() +
      #geom_boxplot(width=.1, fill="black", outlier.colour=NA) +
      stat_summary(fun.y=median, geom="point", fill="white", shape=21, size=2.5)
    
    plot_list[[i]]<- p +
      xlab("Frame Number")+ylab(axis_name[2])+
      #   x_scale +
      y_scale
  }
  plot_grid(plotlist = plot_list, labels = "AUTO",ncol = 2)
}
