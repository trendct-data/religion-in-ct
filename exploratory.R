library(foreign)
library(dplyr)

counties <- read.spss("data/religions_counties.SAV", 
                      to.data.frame = TRUE)

counties$CNTYNAME <- as.character(counties$CNTYNAME)
counties$CNTYNAME <- sub("County", "", counties$CNTYNAME)
counties$CNTYNAME <- sub("\\s+$", "", counties$CNTYNAME)

counties_rates <- counties[ , grepl( "RATE" , names( counties ) ) ]
counties_rank <- counties_rates

counties_rank_names <- counties[c("STABBR", "CNTYNAME")]

counties_rank$STABBR <- NULL
counties_rank$CNTYNAME <- NULL

for (i in 1:ncol(counties_rank)) {


    counties_rank[,i] <- rank(-as.numeric(as.character(counties_rank[,i])), na.last=T, )
    print(colnames(counties_rank[i]))

  
}

counties_rank <- cbind(counties_rank_names, counties_rank)

filtered_rank_ct <- counties_rank %>%
  filter(STABBR=="CT")

filtered_rank_ct$STABBR <- NULL

rank_ct <- data.frame(t(filtered_rank_ct))
rank_ct$type <- rownames(rank_ct)
rownames(rank_ct) <- NULL
column_names <- filtered_rank_ct$CNTYNAME
column_names <- c(column_names, "type")
colnames(rank_ct) <- gsub(" ", "", column_names)
rank_ct <- subset(rank_ct, type!="CNTYNAME")

fairfield <- subset(rank_ct, as.numeric(as.character(Fairfield)) <=15)

# Fairfield is the 13th highest county for American Carpatho-Russian Orthodox Diocese (ACRORATE)

hartford <- subset(rank_ct, as.numeric(as.character(Hartford)) <=15)

# Fairfield is tied for 14th highest county for Armenian Apostolic Church of America (Catholicosate of Cilicia) (AACARATE)

hartford <- subset(rank_ct, as.numeric(as.character(Hartford)) <=15)

# Hartford is tied for 14th highest county for Armenian Apostolic Church of America (Catholicosate of Cilicia) (AACARATE)

litchfield <- subset(rank_ct, as.numeric(as.character(Litchfield)) <=15)

# Litchfield is third highest county for United Catholic Church, Inc. (UCTHRATE)

middlesex <- subset(rank_ct, as.numeric(as.character(Middlesex)) <=15)

# Middlesex is 13th for highest county for Congregational Christian Churches (CCRATE)

newhaven <- subset(rank_ct, as.numeric(as.character(NewHaven)) <=15)

# New Haven is 13th for highest county for United Catholic Church, Inc. (UCTHRATE)

windham <- subset(rank_ct, as.numeric(as.character(Windham)) <=15)

# Windham is fifth for highest county for Reconstructionist Judaism (RJUDRATE)

## OK MAP IT NOW SUCKERRRR


