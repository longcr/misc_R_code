# Practicing SQL joins with Frank
# 2017-01-07

# diagrams showing SQL joins
# https://encrypted.google.com/search?q=sql+join&tbm=isch

# mostly 'dplyr' with a little 'merge'


# SITUATIONS COVERED ##########################################################

# sit_A = number columns per table in join
# sit_B = column names the same between tables

#	sit_A	sit_B	inner_	left_	  right_	outer_
#	1		  yes		y			  y			  y			  y
#	1		  no		y			  y			  y			  y
#	2		  yes		y			  y			  y			  y
#	2		  no		y			  y			  y			  y



# concepts:
# data normalization
# mutation
# Cartesian product



# options to keep (or not) columns from each of the merged/joined data frames




# LOAD PACKAGES ###############################################################

library(dplyr)
library(readr)



# JOIN USING ONE COLUMN IN EACH DATAFRAME #####################################
# http://stat545.com/bit001_dplyr-cheatsheet.html


superheroes <- "
name, alignment, gender,         publisher
Magneto,       bad,   male,            Marvel
Storm,      good, female,            Marvel
Mystique,       bad, female,            Marvel
Batman,      good,   male,                DC
Joker,       bad,   male,                DC
Catwoman,       bad, female,                DC
Hellboy,      good,   male, Dark Horse Comics
"

superheroes <- read_csv(superheroes, trim_ws = TRUE, skip = 1)


publishers <- "
publisher, yr_founded
DC,       1934
Marvel,       1939
Image,       1992
"

publishers <- read_csv(publishers, trim_ws = TRUE, skip = 1)


# inner join

(ijsp <- inner_join(superheroes, publishers))


# left join - with dplyr

left_join(superheroes, publishers)

# left join - with merge -- note the 'all.x = TRUE'

merge(superheroes, publishers, by = 'publisher', all.x = TRUE)


# anti join

(ajsp <- anti_join(superheroes, publishers))


# anti join
(ijps <- inner_join(publishers, superheroes))


# left join

(ljps <- left_join(publishers, superheroes))


# full join

(fjsp <- full_join(superheroes, publishers))



# JOIN USING MULTIPLE COLUMNS #################################################
# http://stackoverflow.com/questions/29212691/how-to-merge-two-dataframes-using-multiple-columns-as-key


# COLUMNS HAVE SAME NAME BETWEEN DF -------------------------------------------

# this is your DF1    
DF1 <- data.frame("col_1" = rep(c("A","B"), 18),
                  "col_2" = rep(c("C","D","E"), 12),
                  "NUM"= seq(from = 101, to = 136),
                  "TEST" = rep(NA,36))

?seq
#this is a DF2 i created, with unique A, B, VAL
DF2 <- data.frame("col_1" = rep(c("A","B"),3),
                  "col_2" = rep(c("C","D","E"),2),
                  "VAL" = rep(1:6))


# answer      
tmp1 <- merge(DF1, DF2, by = c("col_1", "col_2"), all.x = TRUE, all.y = FALSE)

DF1[4] <- tmp[5]



# COLUMNS HAVE DIFFERENT NAMES BETWEEN DF -------------------------------------

# col_1 in DF3 is related to col_3 in DF4
# col_2 in DF3 is related to col_4 in DF4


# this is your DF1    
DF3 <- data.frame("col_1" = rep(c("A","B"), 18),
                  "col_2" = rep(c("C","D","E"), 12),
                  "NUM"= seq(from = 101, to = 136),
                  "TEST" = rep(NA,36))


# this is a DF2 i created, with unique A, B, VAL
DF4 <- data.frame("col_3" = rep(c("A","B"),3),
                  "col_4" = rep(c("C","D","E"),2),
                  "VAL" = rep(1:6))


# answer      
tmp2 <- merge(DF3, DF4, by.x = c("col_1","col_2"), by.y = c("col_3","col_4"), all.x = TRUE, all.y = FALSE)

DF3[4] <- tmp2[5]





# END CODE ####################################################################