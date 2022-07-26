# Beyond DIGISER - An analysis of public sector digitalization in European cities and its correlates
This project has been designed by me and my team for the final research project of the Applied Statistics course of the Master's Degree in Computer Science and Engineering at Politecnico di Milano. The team was composed by me, Maite Siebel, William Rom, Hatice Deniz Temurtas and Melis Yilmaz.  

It is possible to visualize the code in the related folder, as well as the final poster of the results.  


## Motivations
Even in 2022, digitalization is not fully here yet.

Many european cities has an overall low level of digital skills in the public sector with respect to other cities.  

The aim of the project is helping stakeholders an policy makers in making decisions on best-possible actions to boost digital transformation of local public services in the European Union.
Moreover, using not only data about the public sector but also socioeconomics data, political data, and the distribution of individual digital skills, the research aims to find some influence of these factors with respect to the digitalization.  

## Data
### DIGISER Data
For each city, 31 indicators about the digitalization of the public sector, each reflecting a different sector.
### Political data
The dataset includes elections and cabinets in established democracies.
More specifically, the dataset includes democratic national lower houseelections and EP elections for all EU.
The dataset record all elections and succeeding cabinets after 1945 or after full democratization according to Boix ea. (2013).
We also include elections and cabinets before 1945. We record information after 1900 or after 
the last democratic transition (Boixea. 2013).

From the view_election, it is possible to see the details of the elections in the country. 
For the aim of the project, the only election that will be taken into consideration will be the most recent one,
since they will be used to explain the current political orientation of a country.

Moreover, only the party that in the most recent election won the maximum vote share will be taken into consideration.
Parties are classified into families by their position in an economic (state/market) and a cultural (liberty/authority) 
left/rightdimension.
The classification leads to eight party family categories: Communist/Socialist, Green/Ecologist, Social democracy, 
Liberal,Christian democracy, Agrarian, Conservative, Right-wing.
Parties that can not be classifi ed into the eight categories are coded as 'spec'.

Based on the characteristic and family of the max vote share party of a country, the latter will be associated to the
characteristics of the former.

At the end, each country will have a political orientation
Consequently, this information can be joint to the cities of the DIGISER dataset [1].

### Socioeconomic data
Data on European cities were collected in the Urban Audit and in the Large City Audit project. The projects'
ultimate goal is to contribute towards the improvement of the quality of urban life: it supports the exchange
of experience among European cities; it helps to identify best practices; it facilitates benchmarking at the
European level and provides information on the dynamics within the cities and with their surroundings.
At the city level, the Urban Audit contains more than 170 variables and more than 60 indicators. These
indicators are derived from the variables collected by the European Statistical System [2].  

### Individual digital skills
Dataset containing the percentage of individuals with basic or above basic overall digital skills for each country [3].


## Methods
The method used in the project are:
* Principal Component Analysis on public sector variables
* ANOVA on political variables
* Clustering on public sector/political data
* Correlation on socioeconomic data
* ANOVA on individual digital skills  

### PCA
Through Principal Component Analysis, the project deals with the interpretation of the sectors of the digitalization. After having found 8 PC, it is possible to project the cities in the PCs and to analyze how the various digital sector in the public sector are performing in that specific city.  

### ANOVA on political data
Studing the variance of each population of cities with different political orientation, it is possible to understand if the politics has an influence of the digitalization.  

### Clustering and comparation with politics
Clustering on bureaucratics variables has been made. The aim is finding any interesting connection between the performance of the bureaucracy with respect to the digitalization of the public sector and the current politic situation of a country.  

### Correlation between socioeconomics and digitalization
This section focuses in finding any interesting correlation between socioeconomic aspects of the countries and the digitalization in the public sector.  

### ANOVA on individual digital skills
Studing the variance of each population of cities with different number of individuals with basic digital skills, it is possible to understand if the the digital skills of the individuals has an influence of the digitalization of a country. 

## References
[1] 
* Döring, Holger. 2013. “The Collective Action of Data Collection: A Data Infrastructure on Parties, Elections and Cabinets.”European Union Politics 14(1): 161–78.
DOI:
10.1177/1465116512461189 (https://www.doi.org/10.1177/1465116512461189)
[ presentation of technical concept ]
* Döring, Holger. 2016. “Mapping Established Democracies: Integrated Data on Parties, Elections and Cabinets.” ElectoralStudies 44: 535–43.
DOI:
10.1016/j.electstud.2016.07.002 (https://www.doi.org/10.1016/j.electstud.2016.07.002)
[ presentation of methodological design and operationalization ]  

[2]
* Data: https://ec.europa.eu/eurostat/web/cities/data/database
* Manual: https://ec.europa.eu/eurostat/documents/3859598/8012444/KS-GQ-17-006-EN-N.pdf


[3] 
* Map: https://ec.europa.eu/eurostat/databrowser/view/ISOC_SK_DSKL_I21__custom_3013481/default/map?lang=en
* Data: https://ec.europa.eu/eurostat/databrowser/view/ISOC_SK_DSKL_I21__custom_3013481/default/table?lang=en

