

# function to get list of .jpeg and .mp4 files to process
get_files <- function (input_dir) {
  # input directory
  setwd(input_dir)
  
  # load files from input directory one at a time
  file_list <- list.files(all.files = TRUE, recursive = TRUE)
  # print(file_list)
  
  return (file_list)
  
}

safe_dir_create <- function (input_dir) {
  # setwd(input_dir)
  
  ret <- ""
  
  if (!dir.exists(input_dir)) {
    ret <- dir.create(input_dir)
  }

  return (ret)
}


safe_move <- function (from, to) {
  # setwd(input_dir)
  
  ret <- FALSE
  
  # if file name ends with Thumbs.db or .THM or .info
  if (substr(from, nchar(from) - 8, nchar(from)) == "Thumbs.db") {
    ret <- file.remove(from)
    return (ret)
  }
  
  if (substr(from, nchar(from) - 3, nchar(from)) == ".THM") {
    ret <- file.remove(from)
    return (ret)
  }

  if (substr(from, nchar(from) - 4, nchar(from)) == ".info") {
    ret <- file.remove(from)
    return (ret)
  }
  
  is_from_file_movable <- (file.access(from, 2) != -1)
  is_file_exists <- file.exists(to)
  is_file_accessible <- (file.access(to) != -1)

  is_file_size_same <- FALSE
  if (is_file_exists & is_file_accessible) {
    is_file_size_same <- (file.size(from) == file.size(to))
  }
  
  # print(paste("from", from, "to", to, is_from_file_movable, is_file_exists, is_file_accessible, is_file_size_same, sep = " -- "))
  
  if (is_from_file_movable & is_file_exists & is_file_accessible & is_file_size_same) {
    # duplicate file
    ret <- file.remove(from)
    
    return (ret)
  }
  
  if (is_from_file_movable & is_file_exists & is_file_accessible & !is_file_size_same) {
    # file name is same but the file sizes are different
    # further randomizing the dup file name to prevent overwriting
    ret <- file.rename(from, paste(to, "-dup-", sample.int(10000, 1), sep = ""))
    
    return (ret)
  }
  
  if (is_from_file_movable & !is_file_exists & !is_file_accessible) {
    # file doesn't exist... move to destination
    ret <- file.rename(from, to)
    
    return (ret)
  }
  
  print(paste("WARNING!!! check this file manually: from", from, "to", to, is_from_file_movable, is_file_exists, is_file_accessible, is_file_size_same, sep = " -- "))
  return (ret)
}

process_file_match_1 <- function(input_file, output_dir) {
  # print(paste("processing ", input_file, sep = ""))
  
  ret <- FALSE
  
  is_match <- regexpr("(20[0-9][0-9][0-9][0-9][0-9][0-9]_[0-9]+)", input_file)
  # print(paste("      ---> match ", is_match, sep = ""))
  
  if (is_match > 0) {
    
    f <- substr(input_file, is_match, nchar(input_file))

    YYYY <- substr(input_file, is_match, is_match + 3)
    MM <- substr(input_file, is_match + 4, is_match + 5)
    DD <- substr(input_file, is_match + 6, is_match + 7)
    
    # print(paste("          ---> ", YYYY, "/", MM, "/", DD, sep = ""))
    
    safe_dir_create(paste(output_dir, YYYY, sep = "/"))
    safe_dir_create(paste(output_dir, YYYY, MM, sep = "/"))
    safe_dir_create(paste(output_dir, YYYY, MM, DD, sep = "/"))
    
    # move file to destination dir
    ret <- safe_move(input_file, paste(output_dir, YYYY, MM, DD, f, sep = "/"))
    
  }  
  
  return (ret)
}

process_file_match_2 <- function(input_file, output_dir) {
  # print(paste("processing ", input_file, sep = ""))
  
  ret <- FALSE
  
  # check for alternative match format
  # eg: IMG-20170903-WA0017.jpg
  is_match2 <- regexpr("([A-Z][A-Z][A-Z]\\-[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]\\-[a-zA-Z0-9]+)", input_file)
  
  if (is_match2 > 0) {
    # print(paste("      ---> match2 ", is_match2, sep = ""))
    
    f <- substr(input_file, is_match2, nchar(input_file))
    YYYY <- substr(input_file, is_match2 + 4, is_match2 + 7)
    MM <- substr(input_file, is_match2 + 8, is_match2 + 9)
    DD <- substr(input_file, is_match2 + 10, is_match2 + 11)
    
    # print(paste("          ---> ", YYYY, "/", MM, "/", DD, sep = ""))
    
    safe_dir_create(paste(output_dir, YYYY, sep = "/"))
    safe_dir_create(paste(output_dir, YYYY, MM, sep = "/"))
    safe_dir_create(paste(output_dir, YYYY, MM, DD, sep = "/"))
    
    # move file to destination dir
    ret <- safe_move(input_file, paste(output_dir, YYYY, MM, DD, f, sep = "/"))
    
  }
  
  return (ret)
}

process_file_match_3 <- function(input_file, output_dir) {
  # print(paste("processing ", input_file, sep = ""))
  
  ret <- FALSE
  
  # check for alternative match format
  # eg: 02-NOV-2004
  is_match <- regexpr("([0-9][0-9]\\-[A-Z][A-Z][A-Z]\\-20[0-9][0-9]+)", input_file)
  
  if (is_match > 0) {
    f <- substr(input_file, is_match, nchar(input_file))

    s <- substr(input_file, is_match, is_match + 11)
    date <- as.Date(s, format = "%d-%b-%Y")
    # print(paste("DATE is ", date, sep = ""))
    
    YYYY <- format(date, "%Y")
    MM <- format(date, "%m")
    DD <- format(date, "%d")

    # print(paste("          ---> ", YYYY, "/", MM, "/", DD, sep = ""))
    
    safe_dir_create(paste(output_dir, YYYY, sep = "/"))
    safe_dir_create(paste(output_dir, YYYY, MM, sep = "/"))
    safe_dir_create(paste(output_dir, YYYY, MM, DD, sep = "/"))
    
    # move file to destination dir
    ret <- safe_move(input_file, paste(output_dir, YYYY, MM, DD, f, sep = "/"))
    
  }
  
  return (ret)
}

# unused function... remove it later
# no_matches_move_to_unknown <- function(input_file, unknown_dir) {
#   # move file to unknown_dir
#   fl <- strsplit(input_file, "/")
#   f <- tail(fl[[1]], 1)
#   dup_f <- paste("dup-", sample.int(100000,1), "-",f, sep = "")
#   # print(paste("unknown file = ", f, sep = ""))
#   safe_move(input_file, paste(unknown_dir, f, sep = "/"), paste(unknown_dir, dup_f, sep = "/"))
#   
#   ret <- TRUE
#   return (ret)
# }


# input_dir <- "/Volumes/Seagate\ Backup\ Plus\ Drive/Memories_DO_NOT_ADD/Nov/"
input_dir <- "/Users/asaalam/Desktop/Baseline2/Code2/github/mem_app/input_dir/" # test dir

# makesure the ending / is not present in the output dir
# output_dir <- "/Volumes/Seagate\ Backup\ Plus\ Drive/memories"
output_dir <- "/Users/asaalam/Desktop/Baseline2/Code2/github/mem_app/output_dir" # test dir

safe_dir_create(output_dir)

#unknown_dir <- paste(output_dir, "unknown", sep = "/")
#safe_dir_create(unknown_dir)

raw_files <- get_files(input_dir)

print(paste("processing ", length(raw_files), " files", sep = ""))

for (i in 1:length(raw_files)) {
  # print(paste("processing ", raw_files[i], sep = ""))
  
  ret <- process_file_match_1(raw_files[i], output_dir)
  
  if (ret) {
    next
  }
  
  ret <- process_file_match_2(raw_files[i], output_dir)
  
  if (ret) {
    next
  }

  ret <- process_file_match_3(raw_files[i], output_dir)
  
  if (ret) {
    next
  }
  
  # no matches then don't move the file
  # ret <- no_matches_move_to_unknown(raw_files[i], unknown_dir)
}


print("DONE")