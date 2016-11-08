gen.trac=c(1901, 2101, 2102, 9508, 9509,2201,2202, 2400,2301,2502, 2801, 2900, 3100,3200,2802,3500,3400,3301,9203,9201,9301, 9302, 5301, 5201, 4801, 4600, 8701, 8702, 9000, 4701, 4702, 10800, 10600, 8410, 8402, 8802, 8804, 8903, 9601, 8001, 7901, 7903, 7808, 9903, 9905, 8002, 10200, 6801, 6802, 11000, 6400, 7200, 7100, 10400)
gens = which(D[,2]%in%gen.trac)
dng = c(702, 9505, 9507, 9501, 2302, 9504, 9400, 9204, 9102, 11100, 8803, 8904, 9602, 7806, 7809, 7807, 7803, 7804, 9603, 9604, 9904, 9906, 9907, 7709, 7707, 9902, 7603, 7601, 7605, 7603, 7401, 7503, 7504, 7502 , 7407, 7406, 7408, 7403, 7409, 7304, 9700, 9804, 9803, 9801, 9802, 9807, 9810, 9811, 19000)
dngens = which(D[,2]%in%dng)
tracts = read.csv("tracts.csv")

#parsin xml list of stations
data <- xmlParse("http://feeds.capitalbikeshare.com/stations/stations.xml")
xml_data <- xmlToList(data)
station_list = matrix(0, ncol = 4, nrow=length(xml_data)-1)
station_to_tract = matrix(0, ncol = 2, nrow=length(xml_data)-1)

##getting the fips code for each lat/long pair and finding which tract goes with which fips code
for(i in 1:(length(xml_data)-1)){
  station_list[i,]=unlist(xml_data[i]$station[c(3,5,6,9)])
  query=paste(c("http://data.fcc.gov/api/block/find?format=json&latitude=",station_list[i,2],"&longitude=", station_list[i,3], "&showall=true" ), collapse="")
  r=GET(query)
  this.raw.content <- rawToChar(r$content)
  
  state=substring(strsplit(this.raw.content, "code")[[1]][2], 4,5)
  
  if(state=="DC"){
    fips = substring(strsplit(this.raw.content, "FIPS")[[1]][2], 4,14)
    tract=tracts[which(tracts[,3]==fips),2]
    station_to_tract[i,]= c(station_list[i,1], tract)
  }
  print(i)
}

###removing tracts in VA
rms = as.numeric(station_to_tract[,1])+as.numeric(station_to_tract[,2])
rms = which(rms ==0)
station_to_tract=station_to_tract[-rms,]
