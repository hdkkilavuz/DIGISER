install.packages('rworldmap',dependencies=TRUE) 
library(rworldmap)
install.packages("readxl")
library(readxl) # to read the excel file
data<-read_excel("C:\\Users\\Melis Y\\Desktop\\applied proje\\countries_excel.xlsx") # file with the country codes (ISO2 codes)

#colnames(data)
#help("joinCountryData2Map")

sPDF<- joinCountryData2Map(data, joinCode = "ISO2", nameJoinColumn = "CCode", nameCountryColumn = "country", suggestForFailedCodes = FALSE, mapResolution = "coarse", projection = NA, verbose = FALSE)
#CCode is the column name for the country codes in excel file.

par(mai=c(0.2,0,0.2,0),xaxs="i",yaxs="i")
plot<-mapCountryData( sPDF,
                     nameColumnToPlot="I1deneme",
                     mapRegion='Europe',addLegend = FALSE, 
                     colourPalette = 'heat',
                     oceanCol = 'lightblue',
                     borderCol = 'black',
                      lwd = 1.5,
                     missingCountryCol = 'grey',
                     mapTitle = 'Our title') 
#I1deneme is the variable name that we are using for coloring.We can try different variables according to the PCA. 
#If this does not work you can try changing ',' and '.'  according to your excel and rstudio. 
#help(mapCountryData)


do.call( addMapLegend, c(plot,legendLabels='all')) # This is for legend, however I cannot find a better to visualize legend.

# TO VISUALIZE THE LEGEND
#mapParams$legendText <- c('antarctic','africa','oceania'
#                          ,'americas','s.asia','eurasia')  


# To make the levels and naming them for legend. 
#cutVector <- quantile(sPDF@data[["I1deneme"]],na.rm=TRUE)
#sPDF@data[["I1deneme"]] <- cut( sPDF@data[["I1deneme"]]
#                                     , cutVector
#                                      , include.lowest=TRUE )

#levels(sPDF@data[['I1deneme']]) <- c('low','medium','higher medium','high')

