## ui.R ##


##
dashboardPage(


dashboardHeader(title = "Medical Expenditures"),

dashboardSidebar(
	fluidRow(
		#box(title = "Year selection",
			selectInput("in_year", "Year:", 
				c("2013" = "h163ssp",
				"2014" = "h171ssp"), 
				selected = "h163ssp")

		#)
	),
	fluidRow(
		#box(title = "Plot selection",
			selectInput("in_pdata", "Plot Data", 
				c("Age" = "Age",
				"Cancer" = "Cancer"),
				selected = "Cancer")
		#)
	)

),

dashboardBody(
	fluidRow(
		box(tableOutput("resTable"))
	),	
	fluidRow(
		box(plotOutput("resPlot"))
	),
	fluidRow(
		box(textOutput("resTxt"))
	)

) 

	
) ## 
