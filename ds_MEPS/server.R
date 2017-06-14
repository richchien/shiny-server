# server.R

# connect to db
sqlitePath <- "dbmeps"

# survey options
options(survey.lonely.psu='adjust')

perc_dist_calc <- function(vars) {
	
	string <- paste0(vars, " == 1")

	dat <- dat %>% 
		mutate(age65 = ifelse(AGELAST >=65, 1, 0)) %>%
		mutate(age65n = ifelse(AGELAST < 65, 1, 0)) %>%	
		filter_(string)
		
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
	names(ra) <- vars
	return(ra)
}
	

shinyServer(function(input, output) {

# input selections
pdat <- reactive({input$in_pdata})
yr <- reactive({input$in_year})
	
sqlgetdata <- reactive({

	con <- dbConnect(SQLite(), sqlitePath)
	querry <- paste0("SELECT * FROM ", yr(), ";")
	dat <- dbGetQuery(con, querry)
	dbDisconnect(con)
	dat
	
})

getcancertype <- reactive({
	dat <- sqlgetdata()
	cancertypes <- names(dat)[grepl("^CA", names(dat))]
	removenames <- c("CANCERDX", "CARECO", "CASHP")
	cancertypes <- cancertypes[!grepl(paste(removenames, collapse="|"), cancertypes)]
	cancertypes
})


calc_percent <- reactive({
	dat <- sqlgetdata()
	pdat <- as.character(pdat())
	if (pdat =="Cancer") {
		res <- lapply(getcancertype(), perc_dist_calc)	
	} else {
		res <- lapply(c("age65", "age65n"), perc_dist_calc)	
	}
	res <- do.call("cbind", res)
	res$type <- row.names(res)
	long <- res %>% 
		gather(key,value, -type) %>%
		mutate(key=gsub("CA", "", key))
	long
})


getplot <- reactive({

long <- calc_percent()

ggplot(long, aes(x=type,y=value)) +
  geom_bar(position = "dodge",stat="identity", fill="dodgerblue") +                 
  #scale_fill_manual(values = c(my_blue,my_yellow,my_magenta))+ 
  labs(y = "Percentage of Total Exp",x="") +                                 
  geom_text(aes(x=type,y=value,label=round(value, 2)),             
            position = position_dodge(width = 0.9),vjust = -0.25,
            colour = "black", fontface = "bold")+
  theme_classic()+                        
  theme(legend.position="top",
        legend.title = element_blank(),
		axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.3, size=10),
        axis.line.x = element_line(colour="black"),
        axis.line.y = element_line(colour="black"),
        text = element_text(colour="black",
                            face="bold"))+
  #scale_y_continuous(expand = c(0,0),limits=c(0,max(long$value)+2)) +
  facet_wrap( ~ key, ncol=4)

})

gettable <- reactive({
long <- calc_percent()
long %>% group_by(type) %>% summarize(mean=mean(value))
})

output$resTable <- renderTable(
gettable()
)

output$resTxt <- renderText(
pdat()
)

output$resPlot <- renderPlot(
getplot()
)


})