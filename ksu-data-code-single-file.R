############################

### Convert one AOI file from Tobii Pro Studio to sequence of AOIs ###

### KSU data ###

### The result is the sequence of AOIs in one media/stimuli page. ###

############################

### 2018/03/05    ###


setwd("C:/Users/.../Tom KSU Data/txt")

### Step 1. read the first slide - pathway
# the 1st slide includes the first 80 columns, out of 533 columns
path_file1 <-  read.table("recording 03 1023-01.txt", header=TRUE, sep="\t", 
                          colClasses=c(rep("NULL",2), rep("integer",80), rep("NULL", 533)),
                          blank.lines.skip = TRUE)

dim(path_file1)
# [1] 25117    80

ncol(path_file1)
# [1] 80

colnames(path_file1)[1]
# [1] "AOI.hit..IMP.Pathway...A1." 

# check
path_file1[1, 1:3]

#  AOI.hit..IMP.Pathway...A1. AOI.hit..IMP.Pathway...A2. AOI.hit..IMP.Pathway...A3.
# 1

# # if using header=FALSE in read.table
#                           V3                         V4                         V5
# 1 AOI hit [IMP Pathway - A1] AOI hit [IMP Pathway - A2] AOI hit [IMP Pathway - A3]
# #

path_file1[1, 80]
# [1] AOI hit [IMP Pathway - T11]

      

### Step 2. remove empty rows

path_file1.1 <- path_file1[!apply(path_file1, 1, function(x) any(is.na(x)|x=="")),] 

dim(path_file1.1)
# [1] 2107    80
# check .xls file, correct


path_file1.1[1, 80]
# [1] 0

class(path_file1.1[1, 80])
# [1] "integer"


# check number of '1' in cells
sum(path_file1.1 == 1)

# [1] 425
# check in .xls. looks correct

### write file for backup
colnames(path_file1.1) <- aois
setwd("C:/.../Tom KSU Data/test")
write.table(path_file1.1, "rec01.txt", row.names = F)


### Step 3. simplify AOIs

aois <- gsub('AOI.*IMP|\\.', '', colnames(path_file1.1), fixed = FALSE)
aois
# looks ok


### Step 4. organize aoi sequences based on 1
n <- nrow(path_file1.1)
n
# [1] 2107

c <- length(aois)
c
# [1] 80


## number of 1's is less than the number of rows
m <- sum(path_file1.1 == 1)
aoiSq1 <- vector(mode="character", length = m)

k <- 1
for (i in 1:n){
  for (j in 1:c){
    if (path_file1.1[i,j] == 1){
      aoiSq1[k] <- aois[j]
      k <- k + 1
      break
    }
  }
}

length(aoiSq1)
# [1] 425

aoiSq1
# looks correct when view rec01.xlsx


