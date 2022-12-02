# Misc setup --------------------------------------------------------------
# required packages
pkgs <- c("dplyr", "stringr", "tidyr")
# install packages if required
installed_packages <- pkgs %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
 install.packages(pkgs[!installed_packages])
}
# load required packages:
invisible(lapply(pkgs, library, character.only = TRUE))
# load data
v_n_calorie <- read.csv(file = "data-raw/data.rtf")

# Data wrangling ----------------------------------------------------------
v_n_calorie <- v_n_calorie[8:nrow(v_n_calorie), ]
# remove forward slash
v_n_calorie_v2 <- gsub(pattern = "\s+", replacement = "", 
                       x = v_n_calorie, fixed = TRUE)
# transform to data frame
v_n_calorie_v2 <- as.data.frame(v_n_calorie_v2)
# create column ID
v_n_calorie_v2$tbl_ID <- cumsum(!nzchar(v_n_calorie_v2$v_n_calorie_v2))
# remove missing rows
v_n_calorie_v3 <- v_n_calorie_v2[nzchar(v_n_calorie_v2$v_n_calorie_v2), ]
# transform to numeric 
v_n_calorie_v3$v_n_calorie_v2 <- as.numeric(v_n_calorie_v3$v_n_calorie_v2)
# create column names
colnames(x = v_n_calorie_v3) <- c("Calories", "Elf")
# sum by Elf group and omit last NA value
v_total_calories <- aggregate(v_n_calorie_v3$Calories, 
                              by = list(Category = v_n_calorie_v3$Elf), 
                              FUN = sum) |> 
 na.omit(v_total_calories)
# find the max calories carried by an elf
max(v_total_calories)
# find the total calories of the top three elves
v_total_calories[order(v_total_calories$x, decreasing = TRUE)[1:3], 2] |> sum()

# End file ----------------------------------------------------------------