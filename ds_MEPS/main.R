library(foreign) 
library(survey)
library(DBI)
library(tidyverse)
library(data.table)
library(ggplot2)


options(survey.lonely.psu='adjust')

setwd("C:/Users/RC3258/Documents/Rprojects/ds_MEPS")

folder <- getwd()
files <- c(
	"https://meps.ahrq.gov/mepsweb/data_files/pufs/h163ssp.zip",
	"https://meps.ahrq.gov/mepsweb/data_files/pufs/h171ssp.zip")

### download and save Meps data	
dl_meps <- function(x) {
	# download MEPS data
	fname <- basename(x)
	download.file(x, temp <- tempfile())
	unzipped_file = unzip(temp)
	df = read.xport(unzipped_file)
	write_csv(df, paste0(folder, "/", fname, ".csv"), na = "NA", append = FALSE)
	unlink(temp)  # delete temporary file
}
lapply(files, dl_meps)

### gather data into sqlite db
# interested in these variables
vars <- c(
"VARPSU", "VARSTR", "PERWT", #VARIANCE ESTIMATION PSU, VARIANCE ESTIMATION STRATUM, FINAL PERSON WEIGHT
"DUPERSID", "AGELAST", "CANCERDX", # patient demo
"OBVEXP", "OPTEXP", "ERTEXP", # ambulatory and office based
"HHAEXP", "HHNEXP", "VISEXP", "OTHEXP", # home and other
"IPTEXP", "RXEXP", "DVTEXP", # inpatient, drug, dental
"^CA", # all cancer types begin with CA
"TOTEXP"
)

# fread select columns otherwise whole db too large
read_cols <- function(file_name, selectcols) {
    header <- fread(file_name, nrows = 1, header = T)
	tomatch <- paste(selectcols, collapse="|")
	matchingnames <- names(header)[grep(tomatch, names(header))]
    #all_in_header <- all(selectcols %chin% unlist(header))
    #stopifnot(length(matchingnames)==length(selectcols))
    fread(file_name, header=TRUE, select=matchingnames, verbose=TRUE)
}

# create db
my_db <- src_sqlite("dbmeps2", create = TRUE) 

read_to_db <- function(x){
	df <- read_cols(x, vars)
	fname <- gsub(".zip.csv", "", basename(x))
	copy_to(my_db, df, name=fname, temporary = FALSE) # uploading
}

# read all csv to db as tables
meps_csv_files <- list.files(pattern=".*[.]csv$", path=folder, full.names=TRUE)
lapply(meps_csv_files, read_to_db)

# connect to db
db <- src_sqlite("dbmeps", create = FALSE) 
src_tbls(db)

dat2013 = tbl(db,"h163ssp") 
df <- as.data.frame(dat2013)

# Define the survey object:
mepsdsgn <- svydesign(
	id = ~VARPSU,
    strata = ~VARSTR,
    weights = ~PERWT13F,
    data = df,
    nest = TRUE)
	
# add more grouped variables
mepsdsgn <- update(mepsdsgn,
	ambexp13 = OBVEXP13 + OPTEXP13 + ERTEXP13,
	hhexp13  = HHAEXP13 + HHNEXP13 + VISEXP13 + OTHEXP13)
	
# Use svyratio to calculate percentage distribution of spending by type of service:
pct_TOS = svyratio(~IPTEXP13 + ambexp13 + RXEXP13 + DVTEXP13 + hhexp13,
	denominator = ~TOTEXP13,
	design = mepsdsgn)
# Now do the same thing by age group (<65, 65+), using the `subset` function.
pct_TOS_lt65 = svyratio(~IPTEXP13 + ambexp13 + RXEXP13 + DVTEXP13 + hhexp13,
	denominator = ~TOTEXP13,
	design = subset(mepsdsgn,AGELAST < 65))
pct_TOS_ge65 = svyratio(~IPTEXP13 + ambexp13 + RXEXP13 + DVTEXP13 + hhexp13,
	denominator = ~TOTEXP13,
	design = subset(mepsdsgn,AGELAST >= 65))

# Create output tables
pct_matrix = cbind(coef(pct_TOS),
	coef(pct_TOS_lt65),
	coef(pct_TOS_ge65))*100

rownames(pct_matrix) <- c("Hospital IP", "Ambulatory", "RX", "Dental", "HH and Other")
colnames(pct_matrix) <- c("Total","<65 years","65+ years")


pct_matrix %>% as.data.frame %>% mutate(type=row.names(.)) %>% gather(key,value,-type)
# or
long = melt(pct_matrix)

# define custom colors
my_blue <- rgb(0,115,189,maxColorValue = 255)
my_yellow <- rgb(255,197,0,maxColorValue = 255)
my_magenta <- rgb(99,16,99,maxColorValue=255)
my_darkblue <- rgb(0,0,173,maxColorValue = 255)

# create plot
ggplot(long2,aes(x=Var1,y=value,fill=Var2)) +
  geom_bar(position = "dodge",stat="identity") +                 # make bars side-by-side
  scale_fill_manual(values = c(my_blue,my_yellow,my_magenta))+   # change colors of bars
  labs(y = "Percentage",x="") +                                  # change axis labels
  geom_text(aes(x=Var1,y=value,label=round(value)),              # add data labels to end of bars
            position = position_dodge(width = 0.9),vjust = -0.25,
            colour = my_darkblue, fontface = "bold")+
  theme_classic()+                        # change themes (background color, line colors, etc.)
  theme(legend.position="top",
        legend.title = element_blank(),
        axis.line.x = element_line(colour="black"),
        axis.line.y = element_line(colour="black"),
        text = element_text(colour=my_darkblue,
                            face="bold"))+
  scale_y_continuous(expand = c(0,0),limits=c(0,max(long$value)+2))


 
 
# Now do the same by cancer type 

perc_dist_calc <- function(cantype) {
	string <- paste0(cantype, " == 1")
	dat <- df %>% filter_(string)
	stopifnot(nrow(dat) > 0)
	meps <- svydesign(
	id = ~VARPSU,
    strata = ~VARSTR,
    weights = ~PERWT13F,
    data = dat,
    nest = TRUE)
	
	# add more grouped variables
	meps <- update(meps,
		ambexp13 = OBVEXP13 + OPTEXP13 + ERTEXP13,
		hhexp13  = HHAEXP13 + HHNEXP13 + VISEXP13 + OTHEXP13)

	ra <- svyratio(~IPTEXP13 + ambexp13 + RXEXP13 + DVTEXP13 + hhexp13,
		denominator = ~TOTEXP13,
		design = meps)
		
	ra <- coef(ra)
	ra <- data.frame(ra)
	rownames(ra) <- c("Hospital IP", "Ambulatory", "RX", "Dental", "HH and Other")	
	names(ra) <- cantype
	return(ra)
}

cancertypes <- names(df)[grepl("^CA", names(df))]
removenames <- c("CANCERDX", "CARECO42", "CASHP13X")
cancertypes <- cancertypes[!cancertypes %in% removenames]

test <- lapply(cancertypes, perc_dist_calc)	
res <- do.call("cbind", test)
res$type <- row.names(res)
long <- res %>% gather(key,value, -type) %>%
	mutate(key=gsub("CA", "", key))

ggplot(long,aes(x=type,y=value)) +
  geom_bar(position = "dodge",stat="identity") +                 # make bars side-by-side
  #scale_fill_manual(values = c(my_blue,my_yellow,my_magenta))+   # change colors of bars
  labs(y = "Percentage of Total Exp",x="") +                                  # change axis labels
  geom_text(aes(x=type,y=value,label=round(value, 12)),              # add data labels to end of bars
            position = position_dodge(width = 0.9),vjust = -0.25,
            colour = my_darkblue, fontface = "bold")+
  theme_classic()+                        # change themes (background color, line colors, etc.)
  theme(legend.position="top",
        legend.title = element_blank(),
		axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.3, size=10),
        axis.line.x = element_line(colour="black"),
        axis.line.y = element_line(colour="black"),
        text = element_text(colour=my_darkblue,
                            face="bold"))+
  #scale_y_continuous(expand = c(0,0),limits=c(0,max(long$value)+2)) +
  facet_wrap( ~ key, ncol=4)

 
dbDisconnect(db$con)
dbDisconnect(my_db$con)
	
	
	
	
	
	
	
	
	
	
	
	
	
	