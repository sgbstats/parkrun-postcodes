# library(xml)
library(tidyverse)
library(rvest)
library(dplyr)

# Function to extract tables from HTML file
extract_html_tables <- function(file_path, table_index = NULL, clean_names = TRUE) {
  #' Extract tables from an HTML file
  #' 
  #' @param file_path Character string specifying the path to the HTML file
  #' @param table_index Integer specifying which table to extract (default: NULL for all tables)
  #' @param clean_names Logical indicating whether to clean column names (default: TRUE)
  #' @return A data frame (single table) or list of data frames (multiple tables)
  
  # Check if file exists
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  # Read the HTML file
  tryCatch({
    html_content <- read_html(file_path)
  }, error = function(e) {
    stop("Error reading HTML file: ", e$message)
  })
  
  # Extract all tables
  tables <- html_content %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  
  # Check if any tables were found
  if (length(tables) == 0) {
    warning("No tables found in the HTML file")
    return(NULL)
  }
  
  # Function to clean column names if requested
  clean_column_names <- function(df) {
    if (clean_names && ncol(df) > 0) {
      colnames(df) <- make.names(colnames(df), unique = TRUE)
      colnames(df) <- gsub("\\.", "_", colnames(df))
      colnames(df) <- gsub("_+", "_", colnames(df))
      colnames(df) <- gsub("_$", "", colnames(df))
      colnames(df) <- tolower(colnames(df))
    }
    return(df)
  }
  
  # Apply cleaning to all tables
  tables <- lapply(tables, clean_column_names)
  
  # Return specific table or all tables
  if (!is.null(table_index)) {
    if (table_index > length(tables) || table_index < 1) {
      stop("Table index out of range. Found ", length(tables), " tables.")
    }
    return(tables[[table_index]])
  } else {
    if (length(tables) == 1) {
      return(tables[[1]])
    } else {
      message("Found ", length(tables), " tables. Returning list of data frames.")
      return(tables)
    }
  }
}

# Helper function to preview table structure
preview_html_tables <- function(file_path) {
  #' Preview the structure of tables in an HTML file
  #' 
  #' @param file_path Character string specifying the path to the HTML file
  #' @return Prints summary information about tables found
  
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  html_content <- read_html(file_path)
  tables <- html_content %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  
  if (length(tables) == 0) {
    cat("No tables found in the HTML file.\n")
    return(invisible(NULL))
  }
  
  cat("Found", length(tables), "table(s) in the HTML file:\n\n")
  
  for (i in seq_along(tables)) {
    cat("Table", i, ":\n")
    cat("  Dimensions:", nrow(tables[[i]]), "rows x", ncol(tables[[i]]), "columns\n")
    if (ncol(tables[[i]]) > 0) {
      cat("  Column names:", paste(colnames(tables[[i]]), collapse = ", "), "\n")
    }
    cat("  Preview:\n")
    print(head(tables[[i]], 3))
    cat("\n")
  }
}


files=list.files("py/data", full.names = F)
all_events=tribble(~"eventname",~"event", ~"date", ~"finishers", ~"volunteers")
t=0
for(i in files){
  t=t+1
  new_name <- sub("_.*", "", i)
  svMisc::progress(t, length(files))
  
  tables <- extract_html_tables(paste0("py/data/",i))
  
  all_events=all_events %>% rbind(tables %>% mutate(date=as.Date(substr(date, 1,10)),
                                                    volunteers=str_extract(finishers, "\\d+") %>% as.numeric(),
                                                    finishers=str_extract(date_first_finishers, "\\d+") %>% as.numeric(),
                                                    eventname=new_name) %>% 
                                    dplyr::select(eventname,event, date, finishers, volunteers))
  
}

col=rep("grey", length(unique(all_events$eventname)))
names(col)=unique(all_events$eventname)


all_events %>%
  filter(finishers < 1500, volunteers > 1) %>%
  mutate(ratio = finishers / volunteers) %>%
  ggplot(aes(x = finishers, y = ratio)) +
  geom_smooth(se = FALSE, show.legend = FALSE) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 1000, 100), lim=c(0,1000)) +
  labs(x = "Finishers", y = "Finishers:Volunteers")


all_events %>% filter(finishers<1000) %>% 
  ggplot(aes(x=finishers))+
  geom_density()
