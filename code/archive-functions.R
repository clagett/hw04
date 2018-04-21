library(XML)
library(stringr)
library(ggplot2)

read_archive <- function(package){
     tbl_html <- readHTMLTable(paste0("http://cran.r-project.org/src/contrib/Archive/",package))
     tbl_html
}


clean_archive <- function(pkgtbl){
     split_names <- strsplit(as.character(pkgtbl[[1]]$Name), "_")
     names <- as.character(unlist(lapply(split_names, function(l) l[[1]])))
     version <- str_replace_all(unlist(lapply(split_names, function(l) l[2])), "\\.[:alpha:].*", "")
     
     split_date <- strsplit(as.character(pkgtbl[[1]]$`Last modified`), " ")
     date <- unlist(lapply(split_date, function(l) l[[1]]))
     
     change_size <- function(sizes){
          sizes <- as.character(sizes)
          sizes <- str_replace_all(sizes, "K", "")
          sizes[grep("M", sizes)] <- as.numeric(str_replace_all(sizes[grep("M", sizes)], "M", ""))*1000
          sizes
     }
     
     
     size <- change_size(pkgtbl[[1]]$Size)
     
     cleaned <- data.frame(head(cbind(names, version, date, size)[-c(1:2),], -1))
     cleaned$names <- as.character(cleaned$names)
     cleaned$version <- as.character(cleaned$version)
     cleaned$date <- as.Date(cleaned$date)
     cleaned$size <- as.numeric(as.character(cleaned$size), digits = 10)
     cleaned
}


plot_archive <- function(clean_data){
     ggplot(clean_data) + 
          geom_step(aes(x=date, y=size)) +
          ggtitle("stringr: timeline of version sizes")
}

