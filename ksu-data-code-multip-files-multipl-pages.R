############################

### The raw data are 45 AOI files from Tobii Pro Studio. ###
### Each AOI file is the recording of a participant.
### There are 8 different slides (media pages) for each recording. ###

### This code Converts 45 AOI files to 8 separate files,
### each of which has 45 sequences of AOIs ###
 

############################

### 2018/03/05           ###


##############################


##### Step 1 - read the 45 aoi files, which are exported from Tobii Pro Studio
setwd("C:/.../Tom KSU Data/txt")
### read all files for all the 8 slides 

# get all the 45 AOI file names
filenames <- list.files(full.names=TRUE)
length(filenames)
# 45
filenames[1]
# [1] "./recording 03 1023-01.txt"

##### overall function: file name as argument
get_all_aois <- function(file_name){
  
# set number of columns need to be read (midc) for each slide
# users need to know the number of AOIs for each slide (media page)

leftc <- c(2, 85, 157, 238, 319, 391, 462, 543)
midc <- c(80, 72, 81, 81, 72, 71, 81, 72)
rightc <- c(533, 458, 377, 296, 224, 153, 72, 0)

setwd("C:/.../Tom KSU Data/txt")

path_file <- function(a, b, c){
  read.table(file_name, header=TRUE, sep="\t", 
             colClasses=c(rep("NULL",a), rep("integer",b), rep("NULL", c)),
             blank.lines.skip = TRUE)
}

# apply function
pathf <- mapply(path_file, a=leftc, b=midc, c=rightc)



### remove empty rows

pathfx <- lapply(pathf, function(x) x[!apply(x, 1, function(x) any(is.na(x)|x=="")),]) 


### simplify AOIs

# characters in aois to be removed
a.rem <- c('AOI.*IMP|\\.', 'AOI.*hit|\\.', 'AOI.*hit|\\.', 'AOI.*hit|\\.',
           'AOI.*hit|\\.', 'AOI.*hit|\\.', 'AOI.*hit|\\.', 'AOI.*hit|\\.')

# define function
aoi_simple <- function(a, f){
  gsub(a, '', colnames(f), fixed = FALSE)
}

# apply function
aois <- mapply(aoi_simple, a=a.rem, f=pathfx)


### organize aoi sequences based on 1

# number of rows
n <- lapply(pathfx, function(x) nrow(x))

# number of aois in each slide
c <- midc

## number of 1's is less than the number of rows
m <- lapply(pathfx, function(x) sum(x == 1))


# initiate vectors
aoiSq <- lapply(m, function(x) vector(mode="character", length = x))

# assign aoi sequences for each slide based on 1
for (y in 1:8){
  k <- 1
  for (i in 1:n[[y]]){
    for (j in 1:c[[y]]){
      if (pathfx[[y]][i,j] == 1){
        aoiSq[[y]][k] <- aois[[y]][j]
        k <- k + 1
        break
      }
    }
  }
}

# write aoi sequences to 8 files
setwd("C:/.../Tom KSU Data/aois")

aoi.out.fn <- lapply(id, function(i) sprintf("aois_slide%02d", i))

for (i in 1:8){
  write(aoiSq[[i]], paste0(aoi.out.fn[[i]], '.txt'), sep='\t', append=TRUE, ncolumns=3000)
}

} # end of overall function

# apply overall function

lapply(filenames, function(x) get_all_aois(x))


##############################
###########
### read all files for 1st and 4th slides because above export 46 rows
###########

##### Step 1 - read the 45 aoi files, which are exported from Tobii Pro
setwd("C:/.../Tom KSU Data/txt")


# get all the 45 AOI file names
filenames <- list.files(full.names=TRUE)
length(filenames)
# 45
filenames[1]
# [1] "./recording 03 1023-01.txt"

##### overall function: file name as argument
get_all_aois <- function(file_name){
  
  # set number of columns need to be read (midc) for each slide
  
  leftc <- c(2, 238)
  midc <- c(80, 81)
  rightc <- c(533, 296)
  
  setwd("C:/Users/huitang/Desktop/fa18/Tom KSU Data/txt")
  
  path_file <- function(a, b, c){
    read.table(file_name, header=TRUE, sep="\t", 
               colClasses=c(rep("NULL",a), rep("integer",b), rep("NULL", c)),
               blank.lines.skip = TRUE)
  }
  
  # apply function
  pathf <- mapply(path_file, a=leftc, b=midc, c=rightc)
  
  
  
  ### remove empty rows
  
  pathfx <- lapply(pathf, function(x) x[!apply(x, 1, function(x) any(is.na(x)|x=="")),]) 
  
  
  ### simplify AOIs
  
  # characters in aois to be removed
  a.rem <- c('AOI.*IMP|\\.', 'AOI.*hit|\\.')
  
  # define function
  aoi_simple <- function(a, f){
    gsub(a, '', colnames(f), fixed = FALSE)
  }
  
  # apply function
  aois <- mapply(aoi_simple, a=a.rem, f=pathfx)
  
  
  ### organize aoi sequences based on 1
  
  # number of rows
  n <- lapply(pathfx, function(x) nrow(x))
  
  # number of aois in each slide
  c <- midc
  
  ## number of 1's is less than the number of rows
  m <- lapply(pathfx, function(x) sum(x == 1))
  
  
  # initiate vectors
  aoiSq <- lapply(m, function(x) vector(mode="character", length = x))
  
  # assign aoi sequences for each slide based on 1
  for (y in 1:2){
    k <- 1
    for (i in 1:n[[y]]){
      for (j in 1:c[[y]]){
        if (pathfx[[y]][i,j] == 1){
          aoiSq[[y]][k] <- aois[[y]][j]
          k <- k + 1
          break
        }
      }
    }
  }
  
  # write aoi sequences to 8 files
  setwd("C:/.../Tom KSU Data/aois1-4")
  
  id <- c(1, 4)
  aoi.out.fn <- lapply(id, function(i) sprintf("aois_slide%02d", i))
  
  ### ncolumn 3000 sometimes is not enough!
  for (i in 1:2){
    write(aoiSq[[i]], paste0(aoi.out.fn[[i]], '.txt'), sep='\t', append=TRUE, ncolumns=6000)
  }
  
} # end of overall function

# apply overall function

lapply(filenames, function(x) get_all_aois(x))



##############################
########### Same code with one more step: remove repeat AOIs before writing to file
###########

#####  read the 45 aoi files, which are exported from Tobii Pro
setwd("C:/.../Tom KSU Data/txt")
### read all files for all the 8 slides 

# get all the 45 AOI file names
filenames <- list.files(full.names=TRUE)
length(filenames)
# 45
filenames[1]
# [1] "./recording 03 1023-01.txt"

##### overall function: file name as argument
get_all_aois <- function(file_name){
  
  # set number of columns need to be read (midc) for each slide
  
  leftc <- c(2, 85, 157, 238, 319, 391, 462, 543)
  midc <- c(80, 72, 81, 81, 72, 71, 81, 72)
  rightc <- c(533, 458, 377, 296, 224, 153, 72, 0)
  
  setwd("C:/Users/huitang/Desktop/fa18/Tom KSU Data/txt")
  
  path_file <- function(a, b, c){
    read.table(file_name, header=TRUE, sep="\t", 
               colClasses=c(rep("NULL",a), rep("integer",b), rep("NULL", c)),
               blank.lines.skip = TRUE)
  }
  
  # apply function
  pathf <- mapply(path_file, a=leftc, b=midc, c=rightc)
  
  
  
  ### remove empty rows
  
  pathfx <- lapply(pathf, function(x) x[!apply(x, 1, function(x) any(is.na(x)|x=="")),]) 
  
  
  ### simplify AOIs
  
  # characters in aois to be removed
  a.rem <- c('AOI.*IMP|\\.', 'AOI.*hit|\\.', 'AOI.*hit|\\.', 'AOI.*hit|\\.',
             'AOI.*hit|\\.', 'AOI.*hit|\\.', 'AOI.*hit|\\.', 'AOI.*hit|\\.')
  
  # define function
  aoi_simple <- function(a, f){
    gsub(a, '', colnames(f), fixed = FALSE)
  }
  
  # apply function
  aois <- mapply(aoi_simple, a=a.rem, f=pathfx)
  
  
  ### organize aoi sequences based on 1
  
  # number of rows
  n <- lapply(pathfx, function(x) nrow(x))
  
  # number of aois in each slide
  c <- midc
  
  ## number of 1's is less than the number of rows
  m <- lapply(pathfx, function(x) sum(x == 1))
  
  
  # initiate vectors
  aoiSq <- lapply(m, function(x) vector(mode="character", length = x))
  
  # assign aoi sequences for each slide based on 1
  for (y in 1:8){
    k <- 1
    for (i in 1:n[[y]]){
      for (j in 1:c[[y]]){
        if (pathfx[[y]][i,j] == 1){
          aoiSq[[y]][k] <- aois[[y]][j]
          k <- k + 1
          break
        }
      }
    }
  }
  
  # remove consecutive duplicate AOIs from vector
  RmDup <- function(v){
    X <- rle(as.character(v))
    Y <- cumsum(c(1, X$lengths[-length(X$lengths)]))
    return(v[Y])
  }
  # apply
  aoiSqx <- lapply(aoiSq, function(x) RmDup(x))
  
  # write aoi sequences to 8 files
  setwd("C:/.../Tom KSU Data/aoisx")
  
  aoi.out.fn <- lapply(id, function(i) sprintf("aoisx_slide%02d", i))
  
  for (i in 1:8){
    write(aoiSqx[[i]], paste0(aoi.out.fn[[i]], '.txt'), sep='\t', append=TRUE, ncolumns=3000)
  }
  
} # end of overall function

# apply overall function

lapply(filenames, function(x) get_all_aois(x))

