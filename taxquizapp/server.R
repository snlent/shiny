options(stringsAsFactors=F)
shinyServer(
	function(input,output) {
		calc.savings<-function(inc,stat,dep) {
			#Personal exemption phaseout
			PEP<-data.frame(incstart=c(155650,259400,285350,311300,311300),step=c(1250,2500,2500,2500,2500),abbrev16=c('MFS','S','HOH','M','M'),abbrevdt=c('S','S','S','M','M'))
			rownames(PEP)<-c('Married Filing Separately','Single','Head of Household','Married Filing Jointly','Qualifying Widow(er)')
			statshort16<-PEP[stat,'abbrev16']
			statshortdt<-PEP[stat,'abbrevdt']
			exemption<-4050*max(0,(1-0.02*max(ceiling((inc-PEP[stat,'incstart'])/PEP[stat,'step']),0)))
			taxable16<-max(inc-exemption*(dep+1+1*(statshort16=='M'))-6300*(statshort16 %in% c('S','MFS'))-12600*(statshort16=='M')-9300*(statshort16=='HOH'),0)
			taxabledt<-max(inc-30000*(statshortdt=='M')-15000*(statshortdt=='S'),0)
			rates16<-data.frame(rate=rep(c(0.1,0.15,0.25,0.28,0.33,0.35,0.396),4),min=c(0,9275,37650,91150,190150,413350,415050,0,9275,37650,75950,115725,206675,233475,0,18550,75300,151900,231450,413350,466950,0,13250,50400,130150,210800,413350,441000),stat=c(rep('S',7),rep('MFS',7),rep('M',7),rep('HOH',7)))
			rates16$add<-c(0,cumsum(rates16$rate[1:6]*(rates16$min[2:7]-rates16$min[1:6])),0,cumsum(rates16$rate[8:13]*(rates16$min[9:14]-rates16$min[8:13])),0,cumsum(rates16$rate[15:20]*(rates16$min[16:21]-rates16$min[15:20])),0,cumsum(rates16$rate[22:27]*(rates16$min[23:28]-rates16$min[22:27])))
			ratesdt<-data.frame(rate=rep(c(0.12,0.25,0.33),2),min=c(0,37500,112500,0,75000,225000),stat=c(rep('S',3),rep('M',3)))
			ratesdt$add<-c(0,cumsum(ratesdt$rate[1:2]*(ratesdt$min[2:3]-ratesdt$min[1:2])),0,cumsum(ratesdt$rate[4:5]*(ratesdt$min[5:6]-ratesdt$min[4:5])))
			index16<-max(which(taxable16>=rates16$min & rates16$stat %in% statshort16))
			indexdt<-max(which(taxabledt>=ratesdt$min & ratesdt$stat %in% statshortdt))
			savings<-(rates16$rate[index16]*(taxable16-rates16$min[index16])+rates16$add[index16])-(ratesdt$rate[indexdt]*(taxabledt-ratesdt$min[indexdt])+ratesdt$add[indexdt])
			return(round(savings/12))
		}

	results<-eventReactive(input$actionbutton,{
		#Calculate monthly savings
		monthly.savings<-calc.savings(input$income,input$filingstat,input$dep)
		#Read in list of charities
		df<-read.table('./data/charities.csv',sep=',',header=T)
		#Put in random order to ensure that different charities will come up on different quiz runs
		df<-df[sample(c(1:nrow(df)),nrow(df),replace=F),]
		df<-df[df$Category %in% input$causes,]
		nocauses<-(nrow(df)==0)
		if (length(input$causes)==2) {
			df<-rbind(head(df[df$Category==input$causes[1],],2),head(df[df$Category==input$causes[2],],2))
		} else if (length(input$causes)>2) {
			df<-df[!duplicated(df$Category),]
		}
		list(monthly.savings=monthly.savings,orgname=df$Name,category=df$Category,description=df$Description,website=df$Website,donatelink=df$DonationLink,cnlink=df$CN.Link,nocauseflag=nocauses)
	})

	output$nocauseflag<-renderText(results()$nocauseflag)
	outputOptions(output, "nocauseflag", suspendWhenHidden = FALSE)
	output$nocausemessage<-renderUI(HTML("<br style='margin-bottom:1000px'/> <h3 align='center'> <font color=#b81717> Select at least one cause to get charity recommendations based on your interests </font> </h3>"))

	output$charities<-renderUI(HTML(paste0("<h3 style='text-align: center; font-size=16px'> <b> ",gsub('"','',results()$orgname)," </b> </h3> <p align='left'> ",gsub('"','',results()$description)," </p>  <p align='center'> <button style='background-color:#3b224f; border:none; padding: 12px 20px' class='btn btn-success' onclick= window.open('",results()$website,"','_blank')> Visit the website</button> <button style='background-color:#3b224f; border:none; padding: 12px 20px' class='btn btn-success' onclick= window.open('",results()$donatelink,"','_blank')> Donate</button> </p> <p style='text-align: center; font-style: italic; color:#999999;' > Want to do more research? Check out the <a target='_blank' href=",results()$cnlink,"> Complete Charity Navigator Rating for ",gsub('"','',results()$orgname)," </a> </p> <hr margin-top:75px; margin-bottom:75px; ;width=75%>")))
		
    output$message1<-renderUI(HTML(ifelse(results()$monthly.savings<0,paste0("<p style='text-align: center; font-size=22px; font-weight=600'> Under Trump's proposed tax plan, you will owe $",abs(results()$monthly.savings)," more per month </p>"),paste0("<p align='center'> Your estimated savings under Trump's tax plan is <strong> $",abs(results()$monthly.savings)," </strong> per month </p>"))))
    output$message2<-renderUI(HTML(ifelse(results()$monthly.savings<0,"<p> Although Trump claimed he would lower taxes across the board during his campaign, some individuals &ndash; particularly single parents and people with many dependents &ndash; would pay more under his plan. However, if you are able to contribute even $1/month to any of the causes below, we urge you to consider doing so! </p> <hr margin-top:75px; margin-bottom:75px; width=75%>","<p> We recommend you make a recurring protest donation of this amount to one or more of the organizations below that fight for causes you care about. However, any recurring contribution, even $1/month, helps! </p> <hr margin-top:75px; margin-bottom:75px; width=75%>")))
 })
