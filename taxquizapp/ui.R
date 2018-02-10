shinyUI(fluidPage(
	theme='font.css',
	titlePanel("Calculate your Trump Change"),
	HTML('<p> </p>'),
	sidebarPanel(
		numericInput('income','Annual income',min=0,value=0,step=10000),
		numericInput('dep','Number of dependents',min=0,step=1,value=0),
		selectInput('filingstat','Filing status',c('Single','Married Filing Separately','Married Filing Jointly','Head of Household','Qualifying Widow(er)')),
		checkboxGroupInput('causes','Causes you care about',c('Public health','Civil liberties','Wealth inequality','Racial inequality','LGBT+ rights','Immigration','Science and environment','Gender equality','TBH I just want to keep supporting Hilldawg')),
		actionButton('actionbutton','Calculate my Trump Change',icon("money"),style="backgound-color:#2CA74F; color:#3b224f")
		),
	mainPanel(
		uiOutput('message1'),
	    conditionalPanel(condition = "output.nocauseflag == 'FALSE'",
		uiOutput('message2'),
		uiOutput('charities')),
		conditionalPanel(condition = "output.nocauseflag == 'TRUE'",
		uiOutput('nocausemessage'))),
   tags$head(tags$style(
   	"#message2{color: #999999; font-size: 18px; font-family: 'Proxima Nova',sans-serif; line-height initial; margin-bottom:30px;
    font-style: italic; font-weight: 200}","#message1{font-size: 22px; font-weight: 600; margin-top: 30px}","#charities{font-family: proxima nova,sans-serif; font-size:14px}"))
))
