

	filePath <- 'C:/dev/LossCurveFit/'
	setwd(filePath)
	
	#download real GDP data (chained 2007 dollars) from CANSIM table 379-0031 MANUALLY and save as .csv
	#data starting at Jan 1997, download 'for database loading' in .csv and save as 'gdp.csv'
	#url1 <- 'http://www5.statcan.gc.ca/cansim/results/cansim296894150450386967.csv'
	#download.file(url1, destfile='gdp.csv', method='libcurl')

	#download unemployment data from CANSIM table 282-0087 MANUALLY and save as .csv
	#data starting at Jan 1976, download unemployment and participation rate
	#url2 <- 'http://www5.statcan.gc.ca/cansim/results/cansim3942031476492388427.csv'
	#download.file(url2, destfile='employ.csv', method='libcurl')

	#prepare loss curves in matrix form, rows being losses over time and column being vintage, and save as .csv
	#first column should be date, day should also be on the '15th'

	
	#read GDP
	gdp <- read.csv('gdp.csv')
	gdp <- gdp[,-(2:5)]
	gdp[,1] <- as.character(gdp[,1])

	#fix Ref_Date column, set day to 15th for every month
	for (i in 1:(nrow(gdp)))
	{
		gdp[i,1] <- paste(gdp[i,1],'/15',sep='')
	}

	gdp[,1] <- as.Date(gdp[,1],'%Y/%m/%d')
	names(gdp)[2] <- 'realGDP'
		
	#calculate monthly % change in GDP
	gdpDelta <- NA
	for (j in 1:(nrow(gdp))) 
	{
		gdpDelta <- c(gdpDelta, gdp[j,2] / gdp[j-1,2] - 1)
	}	
	gdp2 <- cbind(gdp, gdpDelta)

	gdpMean <- mean(gdpDelta, na.rm=TRUE)
	gdpStdev <- sd(gdpDelta, na.rm=TRUE)	
	input <- NULL
	input <- c(input, gdpMean, gdpStdev, gdp2[nrow(gdp2),1], gdp2[nrow(gdp2),3])
	
	#calculate GDP Z score
	gdpZScore <- NA
	for (j in 2:(nrow(gdp)))
	{
		gdpZScore <- c(gdpZScore, (gdpDelta[j] - gdpMean) / gdpStdev)
	}
	gdp2 <- cbind(gdp2, gdpZScore)

	#read unemployment rate and participation rate
	employ <- read.csv('employ.csv')
	employ <- employ[,c(-2,-4,-5,-6)]
	library(reshape2)
	employ <- dcast(employ, Ref_Date ~ CHARACTERISTICS, value.var='Value')
	
	#fix Ref_Date column, set day to 15th for every month
	employ[,1] <- as.character(employ[,1])
	for (k in 1:(nrow(employ)))
	{
		employ[k,1] <- paste(employ[k,1],'/15',sep='')
	}

	employ[,1] <- as.Date(employ[,1],'%Y/%m/%d')

	#calculate avg and stdev of unemployment rate
	employMean <- mean(employ$'Unemployment rate (percent)', na.rm=TRUE)
	employStdev <- sd(employ$'Unemployment rate (percent)', na.rm=TRUE)
	input <- c(input, employMean, employStdev)

	#calculate employ Z score
	employZScore <- NA
	for (k in 2:(nrow(employ)))
	{
		#there has been structural shift in Canadian unemployment rate coming down from 
		#the 12% range in the early 1990s to 7% more recently.
		#the model hard codes 7% instead of employMean which is around 8.36%
		#another alternative is to truncate the data and discard data prior to 2000
		employZScore <- c(employZScore, (employ[k, 3] - 7) / employStdev)	
	}
	employ <- cbind(employ, employZScore)	

	#read loss curves
	loss <- read.csv('loss.csv', check.names=FALSE)

	names(loss) <- as.Date(names(loss), '%m/%d/%Y')
	loss[loss==0]<-NA

	#code below is for monthly (not quarterly) increments, if frequency is different adjust code accordingly
	#m is the number of loss curves we have by vintage
	trainData2 <- data.frame(gdp2[, 1])
	names(trainData2)[1] <- 'dates'
	for (m in 2:(ncol(loss)-1))
	{
		#attach the proper dates to each loss column
		mon <- format(as.POSIXct(names(loss)[m]), format = '%m')
		year <- format(as.POSIXct(names(loss)[m]), format = '%Y')
		dates <- as.Date(paste(mon,'/15/',year,sep=''), '%m/%d/%Y')

		for (l in 2:(nrow(loss)+1))
		{
			test = as.numeric(format(as.Date(dates[l-1]), '%m')) + 1
			if (test > 12)
			{
				mon <- as.numeric(format(as.Date(dates[l-1]), '%m')) + 1 - 12
				year <- as.numeric(format(as.Date(dates[l-1]), '%Y')) + 1
				dates <- c(dates, as.Date(paste(mon,'/15/',year,sep=''), '%m/%d/%Y'))
			}
			else
			{
				mon <- as.numeric(format(as.Date(dates[l-1]), '%m')) + 1 
				year <- as.numeric(format(as.Date(dates[l-1]), '%Y'))
				dates <- c(dates, as.Date(paste(mon,'/15/',year,sep=''), '%m/%d/%Y'))
			}
		}
			
		dates <- dates[-1]
		trainData <- cbind.data.frame(dates, loss[, m])
		names(trainData)[2] <- paste('loss', names(loss[m]))
		trainGDP <- gdp2[-c(2,3)]
		trainData <- merge(trainData, trainGDP, by.x= 'dates', by.y = 'Ref_Date', all=TRUE)
		trainEmploy <- employ[-c(2,3)]
		trainData <- merge(trainData, trainEmploy, by.x='dates', by.y='Ref_Date', all=TRUE)
		trainData2 <- merge(trainData2, trainData, by.x='dates', by.y='dates', all=TRUE)
	}

	#print(head(trainData2))
	write.csv(trainData2,file='trainData2.csv')

	hpi[,1] <- as.Date(hpi[,1],'%d-%b-%Y')
	
	#calculate seasonal adjustment for home sales volumn, HPI and home sales mean and stdev
	finalTable <- as.vector(NULL)
	
	sAdjSaleMean <- NULL
	sAdjSaleStdev <- NULL
	hpiMean <- NULL
	hpiStdev <- NULL
	saleMean <- NULL
	saleStdev <- NULL
	for (p in seq(3, ncol(hpi), 2))
	{
		if (length(hpi[,p]) == length(which(!is.na(hpi[,p]))))
		{
			index <- 0
		}
		else
		{
			index <- max(which(is.na(hpi[,p])))
		}

		seasonAdj <- NULL
		ratio <- NULL
		dates <- NULL
		z <- NA

		for (o in 1:(length(hpi[,p])-index-12))
		{
			x <- mean(hpi[,p][(index+o):(index+o+11)])
			y <- mean(hpi[,p][(index+o+1):(index+o+12)])
			seasonAdj <- c(seasonAdj, (x+y)/2)
			ratio <- c(ratio, hpi[,p][index+6+o]/((x+y)/2))
			dates <- c(dates, hpi$Transaction.Date[index+6+o])
			z <- c(z, seasonAdj[o]/seasonAdj[o-1]-1)
		}

		seasonTable <- data.frame(cbind(dates, seasonAdj, ratio))
		seasonTable$dates <- as.Date(seasonTable$dates, origin = "1970-01-01")

		seasonTable2 <- split(seasonTable, as.numeric(as.numeric(format(seasonTable$dates, '%m'))))
		seasonTable3 <- sapply(seasonTable2, function(elt) mean(elt[,3]))
		sums <- sum(seasonTable3)
		seasonTable4 <- seasonTable3 / sums * 12
		finalTable <- cbind(finalTable, seasonTable4)

		#calculate avg and stdev of seasonally adj. home sales
		sAdjSaleMean <- c(sAdjSaleMean, mean(z, na.rm=TRUE))
		sAdjSaleStdev <- c(sAdjSaleStdev, sd(z, na.rm=TRUE))

		colnames(finalTable)[(p-1)/2] <- names(hpi[p-1])
		
		#calculate avg and stdev of hpi and sales
		a <- NA
		b <- NA
		d <- NULL
		e <- NA
		for (q in 1:(length(hpi[,p-1])))	
		{
			a <- c(a, hpi[q,p-1]/hpi[q-1,p-1]-1)
			b <- c(b, hpi[q,p]/hpi[q-1,p]-1)
		
			#calculate seasonally adj. home sales
			if (format(hpi$Transaction.Date[q],'%m')=='01')
			{
				d <- c(d, hpi[q,p] / finalTable[1, (p-3)/2+1])
			}
			else if (format(hpi$Transaction.Date[q],'%m')=='02')
			{
				d <- c(d, hpi[q,p] / finalTable[2, (p-3)/2+1])
			}
			else if (format(hpi$Transaction.Date[q],'%m')=='03')
			{
				d <- c(d, hpi[q,p] / finalTable[3, (p-3)/2+1])
			}
			else if (format(hpi$Transaction.Date[q],'%m')=='04')
			{
				d <- c(d, hpi[q,p] / finalTable[4, (p-3)/2+1])
			}
			else if (format(hpi$Transaction.Date[q],'%m')=='05')
			{
				d <- c(d, hpi[q,p] / finalTable[5, (p-3)/2+1])
			}
			else if (format(hpi$Transaction.Date[q],'%m')=='06')
			{
				d <- c(d, hpi[q,p] / finalTable[6, (p-3)/2+1])
			}
			else if (format(hpi$Transaction.Date[q],'%m')=='07')
			{
				d <- c(d, hpi[q,p] / finalTable[7, (p-3)/2+1])
			}
			else if (format(hpi$Transaction.Date[q],'%m')=='08')
			{
				d <- c(d, hpi[q,p] / finalTable[8, (p-3)/2+1])
			}
			else if (format(hpi$Transaction.Date[q],'%m')=='09')
			{
				d <- c(d, hpi[q,p] / finalTable[9, (p-3)/2+1])
			}
			else if (format(hpi$Transaction.Date[q],'%m')=='10')
			{
				d <- c(d, hpi[q,p] / finalTable[10, (p-3)/2+1])
			}
			else if (format(hpi$Transaction.Date[q],'%m')=='11')
			{
				d <- c(d, hpi[q,p] / finalTable[11, (p-3)/2+1])
			}
			else if (format(hpi$Transaction.Date[q],'%m')=='12')
			{
				d <- c(d, hpi[q,p] / finalTable[12, (p-3)/2+1])
			}

			#calculate seasonally adj. home sales % change
			e <- c(e, d[q]/d[q-1]-1)

		}
		hpi <- cbind(hpi, a, b, d, e)
		names(hpi)[p] <- paste(names(hpi)[p-1],'Sale')
		names(hpi)[ncol(hpi)-3] <- paste(names(hpi)[p-1],'HPIDelta')
		names(hpi)[ncol(hpi)-2] <- paste(names(hpi)[p],'Delta')
		names(hpi)[ncol(hpi)-1] <- paste(names(hpi)[p],'sAdj')
		names(hpi)[ncol(hpi)] <- paste(names(hpi)[p],'sAdjDelta')
		
		hpiMean <- c(hpiMean, mean(a, na.rm=TRUE))
		hpiStdev <- c(hpiStdev, sd(a, na.rm=TRUE))
		saleMean <- c(saleMean, mean(b, na.rm=TRUE))	
		saleStdev <- c(saleStdev, sd(b, na.rm=TRUE))		
	}

	input <- c(input, hpiMean, hpiStdev, saleMean, saleStdev, sAdjSaleMean, sAdjSaleStdev)
	names(input) <- c('gdpMean', 'gdpStdev', 'lastGdpDate', 'lastGdp', 'employMean', 'employStdev',
				'Composite6HPIMean', 'Composite11HPIMean', 'VictoriaHPIMean', 'VancouverHPIMean',
				'CalgaryHPIMean', 'EdmontonHPIMean', 'WinnipegHPIMean', 'HamiltonHPIMean', 
				'TorontoHPIMean', 'OttawaHPIMean', 'MontrealHPIMean', 'QuebecHPIMean', 'HalifaxHPIMean',
				'Composite6HPIStdev', 'Composite11HPIStdev', 'VictoriaHPIStdev', 'VancouverHPIStdev',
				'CalgaryHPIStdev', 'EdmontonHPIStdev', 'WinnipegHPIStdev', 'HamiltonHPIStdev', 
				'TorontoHPIStdev', 'OttawaHPIStdev', 'MontrealHPIStdev', 'QuebecHPIStdev', 'HalifaxHPIStdev',
				'Composite6SaleMean', 'Composite11SaleMean', 'VictoriaSaleMean', 'VancouverSaleMean',
				'CalgarySaleMean', 'EdmontonSaleMean', 'WinnipegSaleMean', 'HamiltonSaleMean', 
				'TorontoSaleMean', 'OttawaSaleMean', 'MontrealSaleMean', 'QuebecSaleMean', 'HalifaxSaleMean',
				'Composite6SaleStdev', 'Composite11SaleStdev', 'VictoriaSaleStdev', 'VancouverSaleStdev',
				'CalgarySaleStdev', 'EdmontonSaleStdev', 'WinnipegSaleStdev', 'HamiltonSaleStdev', 
				'TorontoSaleStdev', 'OttawaSaleStdev', 'MontrealSaleStdev', 'QuebecSaleStdev', 'HalifaxSaleStdev',
				'Composite6sAdjSaleMean', 'Composite11sAdjSaleMean', 'VictoriasAdjSaleMean', 'VancouversAdjSaleMean',
				'CalgarysAdjSaleMean', 'EdmontonsAdjSaleMean', 'WinnipegsAdjSaleMean', 'HamiltonsAdjSaleMean', 
				'TorontosAdjSaleMean', 'OttawasAdjSaleMean', 'MontrealsAdjSaleMean', 'QuebecsAdjSaleMean', 'HalifaxsAdjSaleMean',
				'Composite6sAdjSaleStdev', 'Composite11sAdjSaleStdev', 'VictoriasAdjSaleStdev', 'VancouversAdjSaleStdev',
				'CalgarysAdjSaleStdev', 'EdmontonsAdjSaleStdev', 'WinnipegsAdjSaleStdev', 'HamiltonsAdjSaleStdev', 
				'TorontosAdjSaleStdev', 'OttawasAdjSaleStdev', 'MontrealsAdjSaleStdev', 'QuebecsAdjSaleStdev', 'HalifaxsAdjSaleStdev')

	#merge tables
	mergedData <- merge(gdp2, employ, by.x= 'Ref_Date', by.y = 'Ref_Date', all=TRUE)
	mergedData <- merge(mergedData, hpi, by.x= 'Ref_Date', by.y='Transaction.Date', all=TRUE)

	write.csv(input,file='input.csv')
	write.csv(finalTable,file='homeSales.csv')	
	write.csv(mergedData, file='output.csv')