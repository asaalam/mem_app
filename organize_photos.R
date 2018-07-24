#!/usr/bin/Rscript

# Rscript organize_photos.R > out.txt

# --- Configuration START

# input_dir <- "/Volumes/Seagate\ Backup\ Plus\ Drive/Photos_TO_BACKUP/f_1/"
input_dir <- "/Users/asaalam/Desktop/Baseline2/Code2/github/mem_app/input_dir/" # test dir

# make sure the ending / is not present in the output dir
# output_dir <- "/Volumes/Seagate\ Backup\ Plus\ Drive/memories"
output_dir <- "/Users/asaalam/Desktop/Baseline2/Code2/github/mem_app/output_dir" # test dir

# --- Configuration END


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
  if (!dir.exists(input_dir)) {
    ret <- dir.create(input_dir)
    return (ret)
  }
  
  return (FALSE)
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
  
  if (substr(from, nchar(from) - 8, nchar(from)) == ".DS_Store") {
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
  
  if (is_from_file_movable &
      is_file_exists & is_file_accessible & is_file_size_same) {
    # duplicate file
    ret <- file.remove(from)
    
    return (ret)
  }
  
  if (is_from_file_movable &
      is_file_exists & is_file_accessible & !is_file_size_same) {
    # file name is same but the file sizes are different
    # further randomizing the dup file name to prevent overwriting
    
    ds <- strsplit(to, "\\.")
    dup_file <- paste(to, "_dup_", sample.int(10000, 1), ".", tail(ds[[1]], 1), sep = "")

    ret <-
      file.copy(from, dup_file)
    
    if (ret) {
      # remove the copied file
      ret_remove <- file.remove(from)
      
      if (! ret_remove) {
        print(paste("Unable to remove from = ", from, sep = ""))  
        
      }
    } else {
      print(paste("Unable to copy_1 from = ", from, ", to = ", dup_file, sep = ""))  
    }
    
    return (ret)
  }
  
  if (is_from_file_movable &
      !is_file_exists & !is_file_accessible) {
    # file doesn't exist... move to destination
    ret <- file.copy(from, to, overwrite = TRUE)
    
    if (ret) {
      # copy successful ... remove
      ret_remove <- file.remove(from)
      
      if (! ret_remove) {
        print(paste("Unable to remove from = ", from, sep = ""))  
      }
    } else {
      print(paste("Unable to copy_2 from = ", from, ", to = ", to, sep = ""))  
    }
    
    return (ret)
  }
  
  if (!is_from_file_movable) {
    # print(paste("file ", from, " mode = ", file.mode(from), sep = ""))
    
    # change permissions
    # Sys.chmod(from, "777")
    s <- system(paste("chflags nouchg ", shQuote(from), sep = ""), intern = TRUE)
    # print(s)
    
    is_from_file_movable <- (file.access(from, 2) != -1)
    
    # print(paste("file ", from, " mode = ", file.mode(from), ", is_from_file_movable = ", is_from_file_movable, sep = ""))
    
    if (is_from_file_movable) {
      # changed the from file permission to TRUE
      ret <- file.copy(from, to, overwrite = TRUE)
      
      if (ret) {
        
        ret_remove <- file.remove(from)
        
        if (! ret_remove) {
          print(paste("Unable to remove from = ", from, sep = ""))  
        }
      } else {
        print(paste("Unable to copy_3 from = ", from, ", to = ", to, sep = ""))  
      }
      
      return (ret)
    }
    
  }
  
  print(
    paste(
      "WARNING!!! check this file manually: from",
      from,
      "to",
      to,
      is_from_file_movable,
      is_file_exists,
      is_file_accessible,
      is_file_size_same,
      sep = " -- "
    )
  )
  return (ret)
}

process_file_match_1 <- function(input_file, output_dir) {
  # print(paste("processing ", input_file, sep = ""))
  
  ret <- FALSE
  
  is_match <-
    regexpr("(20[0-9][0-9][0-9][0-9][0-9][0-9]_[0-9]+)", input_file)
  # print(paste("      ---> match ", is_match, sep = ""))
  
  if (is_match > 0) {
    f <- substr(input_file, is_match, nchar(input_file))
    
    date <-
      as.Date(substr(input_file, is_match, is_match + 7), format = "%Y%m%d")
    
    YYYY <- format(date, "%Y")
    MM <- format(date, "%b")
    
    safe_dir_create(paste(output_dir, YYYY, sep = "/"))
    safe_dir_create(paste(output_dir, YYYY, MM, sep = "/"))
    
    # move file to destination dir
    ret <-
      safe_move(input_file, paste(output_dir, YYYY, MM, f, sep = "/"))
    
  }
  
  return (ret)
}

process_file_match_2 <- function(input_file, output_dir) {
  # print(paste("processing ", input_file, sep = ""))
  
  ret <- FALSE
  
  # check for alternative match format
  # eg: IMG-20170903-WA0017.jpg
  is_match2 <-
    regexpr(
      "([A-Z][A-Z][A-Z]\\-[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]\\-[a-zA-Z0-9]+)",
      input_file
    )
  
  if (is_match2 > 0) {
    # print(paste("      ---> match2 ", is_match2, sep = ""))
    
    f <- substr(input_file, is_match2, nchar(input_file))
    date <-
      as.Date(substr(input_file, is_match2 + 4, is_match2 + 11), format = "%Y%m%d")
    
    YYYY <- format(date, "%Y")
    MM <- format(date, "%b")
    
    safe_dir_create(paste(output_dir, YYYY, sep = "/"))
    safe_dir_create(paste(output_dir, YYYY, MM, sep = "/"))
    
    # move file to destination dir
    ret <-
      safe_move(input_file, paste(output_dir, YYYY, MM, f, sep = "/"))
    
  }
  
  return (ret)
}

process_file_match_3 <- function(input_file, output_dir) {
  # print(paste("processing ", input_file, sep = ""))
  
  ret <- FALSE
  
  # check for alternative match format
  # eg: 02-NOV-2004
  is_match <-
    regexpr("([0-9][0-9]\\-[A-Z][A-Z][A-Z]\\-20[0-9][0-9]+)",
            input_file)
  
  if (is_match > 0) {
    f <- substr(input_file, is_match, nchar(input_file))
    
    s <- substr(input_file, is_match, is_match + 11)
    date <- as.Date(s, format = "%d-%b-%Y")
    
    YYYY <- format(date, "%Y")
    MM <- format(date, "%b")
    
    safe_dir_create(paste(output_dir, YYYY, sep = "/"))
    safe_dir_create(paste(output_dir, YYYY, MM, sep = "/"))
    
    # move file to destination dir
    ret <-
      safe_move(input_file, paste(output_dir, YYYY, MM, f, sep = "/"))
    
  }
  
  return (ret)
}

process_file_match_4 <- function(input_file, output_dir) {
  # print(paste("processing ", input_file, sep = ""))
  
  ret <- FALSE
  
  # Example: 20151031153138.m2ts
  is_match <-
    regexpr(
      "(20[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]+)",
      input_file
    )
  # print(paste("      ---> match ", is_match, sep = ""))
  
  if (is_match > 0) {
    f <- substr(input_file, is_match, nchar(input_file))
    date <-
      as.Date(substr(input_file, is_match, is_match + 7), format = "%Y%m%d")
    
    YYYY <- format(date, "%Y")
    MM <- format(date, "%b")
    
    safe_dir_create(paste(output_dir, YYYY, sep = "/"))
    safe_dir_create(paste(output_dir, YYYY, MM, sep = "/"))
    
    # move file to destination dir
    ret <-
      safe_move(input_file, paste(output_dir, YYYY, MM, f, sep = "/"))
    
  }
  
  return (ret)
}

process_file_match_5 <- function(input_file, output_dir) {
  # print(paste("processing ", input_file, sep = ""))
  
  ret <- FALSE
  
  # Example: /mem_app/input_dir/2010_08_05/2010_10_14/IMG_6242.JPG
  is_match <-
    regexpr(
      "(20[0-9][0-9]_[0-9][0-9]_[0-9][0-9]+)",
      input_file
    )
  # print(paste("      ---> match ", is_match, sep = ""))
  
  if (is_match > 0) {
    # for file name start with end and get until you get first /
    l <- strsplit(input_file, "/")
    f <- tail(l[[1]], 1)

    date <-
      as.Date(substr(input_file, is_match, is_match + 9), format = "%Y_%m_%d")
    
    YYYY <- format(date, "%Y")
    MM <- format(date, "%b")
    
    safe_dir_create(paste(output_dir, YYYY, sep = "/"))
    safe_dir_create(paste(output_dir, YYYY, MM, sep = "/"))
    
    # move file to destination dir
    ret <-
      safe_move(input_file, paste(output_dir, YYYY, MM, f, sep = "/"))
    
  }
  
  return (ret)
}

process_file_match_6 <- function(input_file, output_dir) {
  # print(paste("processing ", input_file, sep = ""))
  
  ret <- FALSE
  
  # Example: /mem_app/input_dir/2010-08-05/IMG_6242.JPG
  is_match <-
    regexpr(
      "(20[0-9][0-9]-[0-9][0-9]-[0-9][0-9]+)",
      input_file
    )
  # print(paste("      ---> match ", is_match, sep = ""))
  
  if (is_match > 0) {
    # for file name start with end and get until you get first /
    l <- strsplit(input_file, "/")
    f <- tail(l[[1]], 1)
    
    date <-
      as.Date(substr(input_file, is_match, is_match + 9), format = "%Y-%m-%d")
    
    YYYY <- format(date, "%Y")
    MM <- format(date, "%b")
    
    safe_dir_create(paste(output_dir, YYYY, sep = "/"))
    safe_dir_create(paste(output_dir, YYYY, MM, sep = "/"))
    
    # move file to destination dir
    ret <-
      safe_move(input_file, paste(output_dir, YYYY, MM, f, sep = "/"))
    
  }
  
  return (ret)
}

process_file_match_7 <- function(input_file, output_dir) {
  # print(paste("processing ", input_file, sep = ""))
  
  ret <- FALSE
  
  # Example: /mem_app/input_dir/10-18-2014/IMG_6242.JPG
  is_match <-
    regexpr(
      "([0-9][0-9]\\-[0-9][0-9]\\-20[0-9][0-9]+)",
      input_file
    )
  # print(paste("      ---> match ", is_match, sep = ""))
  
  if (is_match > 0) {
    # for file name start with end and get until you get first /
    l <- strsplit(input_file, "/")
    f <- tail(l[[1]], 1)
    
    date <-
      as.Date(substr(input_file, is_match, is_match + 9), format = "%m-%d-%Y")
    
    YYYY <- format(date, "%Y")
    MM <- format(date, "%b")
    
    safe_dir_create(paste(output_dir, YYYY, sep = "/"))
    safe_dir_create(paste(output_dir, YYYY, MM, sep = "/"))
    
    # move file to destination dir
    ret <-
      safe_move(input_file, paste(output_dir, YYYY, MM, f, sep = "/"))
    
  }
  
  return (ret)
}

process_file_match_8 <- function(input_file, output_dir) {
  # print(paste("processing ", input_file, sep = ""))
  
  ret <- FALSE
  
  # Example: /mem_app/input_dir/1-1-2014/IMG_6242.JPG
  is_match <-
    regexpr(
      "([0-9]\\-[0-9]\\-20[0-9][0-9]+)",
      input_file
    )
  # print(paste("      ---> match ", is_match, sep = ""))
  
  if (is_match > 0) {
    # for file name start with end and get until you get first /
    l <- strsplit(input_file, "/")
    f <- tail(l[[1]], 1)
    
    date <-
      as.Date(substr(input_file, is_match, is_match + 7), format = "%m-%d-%Y")
    
    YYYY <- format(date, "%Y")
    MM <- format(date, "%b")
    
    safe_dir_create(paste(output_dir, YYYY, sep = "/"))
    safe_dir_create(paste(output_dir, YYYY, MM, sep = "/"))
    
    # move file to destination dir
    ret <-
      safe_move(input_file, paste(output_dir, YYYY, MM, f, sep = "/"))
    
  }
  
  return (ret)
}

process_file_match_9 <- function(input_file, output_dir) {
  # print(paste("processing ", input_file, sep = ""))
  
  ret <- FALSE
  
  # Example: /mem_app/input_dir/5-13-2014/IMG_6242.JPG
  is_match <-
    regexpr(
      "([0-9]\\-[0-9][0-9]\\-20[0-9][0-9]+)",
      input_file
    )
  # print(paste("      ---> match ", is_match, sep = ""))
  
  if (is_match > 0) {
    # for file name start with end and get until you get first /
    l <- strsplit(input_file, "/")
    f <- tail(l[[1]], 1)
    
    date <-
      as.Date(substr(input_file, is_match, is_match + 8), format = "%m-%d-%Y")
    
    YYYY <- format(date, "%Y")
    MM <- format(date, "%b")
    
    safe_dir_create(paste(output_dir, YYYY, sep = "/"))
    safe_dir_create(paste(output_dir, YYYY, MM, sep = "/"))
    
    # move file to destination dir
    ret <-
      safe_move(input_file, paste(output_dir, YYYY, MM, f, sep = "/"))
    
  }
  
  return (ret)
}

process_file_match_10 <- function(input_file, output_dir) {
  # print(paste("processing ", input_file, sep = ""))
  
  ret <- FALSE
  
  # Example: /mem_app/input_dir/11-2-2014/IMG_6242.JPG
  is_match <-
    regexpr(
      "([0-9][0-9]\\-[0-9]\\-20[0-9][0-9]+)",
      input_file
    )
  # print(paste("      ---> match ", is_match, sep = ""))
  
  if (is_match > 0) {
    # for file name start with end and get until you get first /
    l <- strsplit(input_file, "/")
    f <- tail(l[[1]], 1)
    
    date <-
      as.Date(substr(input_file, is_match, is_match + 8), format = "%m-%d-%Y")
    
    YYYY <- format(date, "%Y")
    MM <- format(date, "%b")
    
    safe_dir_create(paste(output_dir, YYYY, sep = "/"))
    safe_dir_create(paste(output_dir, YYYY, MM, sep = "/"))
    
    # move file to destination dir
    ret <-
      safe_move(input_file, paste(output_dir, YYYY, MM, f, sep = "/"))
    
  }
  
  return (ret)
}

process_file_ctime <- function(input_file, output_dir) {
  # print(paste("processing ", input_file, sep = ""))
  
  ret <- FALSE
  
  # Example: use the file creation time to move the file
  ctime <- file.info(input_file)$ctime
  # print(paste("      ---> ctime ", ctime, sep = ""))

  # for file name start with end and get until you get first /
  l <- strsplit(input_file, "/")
  f <- tail(l[[1]], 1)
  
  date <- as.Date(ctime, format = "%m-%d-%Y")
    
  YYYY <- format(date, "%Y")
  MM <- format(date, "%b")
    
  safe_dir_create(paste(output_dir, YYYY, sep = "/"))
  safe_dir_create(paste(output_dir, YYYY, MM, sep = "/"))

  # move file to destination dir
  ret <- safe_move(input_file, paste(output_dir, YYYY, MM, f, sep = "/"))
    
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


dir_create_ret <- safe_dir_create(output_dir)

raw_files <- get_files(input_dir)
print(
  paste(
    "processing ",
    length(raw_files),
    " files from [",
    input_dir,
    "] to [",
    output_dir,
    "]",
    sep = ""
  )
)

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
  
  ret <- process_file_match_4(raw_files[i], output_dir)
  
  if (ret) {
    next
  }
  
  ret <- process_file_match_5(raw_files[i], output_dir)
  
  if (ret) {
    next
  }
  
  ret <- process_file_match_6(raw_files[i], output_dir)
  
  if (ret) {
    next
  }
  
  ret <- process_file_match_7(raw_files[i], output_dir)
  
  if (ret) {
    next
  }
  
  ret <- process_file_match_8(raw_files[i], output_dir)
  
  if (ret) {
    next
  }
  
  ret <- process_file_match_9(raw_files[i], output_dir)
  
  if (ret) {
    next
  }

  ret <- process_file_match_10(raw_files[i], output_dir)
  
  if (ret) {
    next
  }
  
  # ret <- process_file_ctime(raw_files[i], output_dir)
  
  # if (ret) {
  #   next
  # }
  
  print(paste("No pattern match for file [", raw_files[i], "]", sep = ""))
  
}

warnings()

print("DONE")
