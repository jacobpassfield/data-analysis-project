data = read.table("dna-data/data/human-phage.txt")

# NUMBER OF As

nAs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  nAs[i] = length(which(data[i,] == "A")) }
nAs

# NUMBER OF Ts

nTs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  nTs[i] = length(which(data[i,] == "T")) }
nTs

# NUMBER OF Gs

nGs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  nGs[i] = length(which(data[i,] == "G")) }
nGs

# NUMBER OF Cs

nCs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  nCs[i] = length(which(data[i,] == "C")) }
nCs

# NUMBER OF CONSECUTIVE As

nAAs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  for (j in 1:(ncol(data)-1)){
    nAAs[i] = nAAs[i] + ((data[i,j] == "A") & (data[i,j+1] == "A"))} 
}
nAAs

# NUMBER OF CONSECUTIVE Ts

nTTs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  for (j in 1:(ncol(data)-1)){
    nTTs[i] = nTTs[i] + ((data[i,j] == "T") & (data[i,j+1] == "T"))} 
}
nTTs

# NUMBER OF CONSECUTIVE Gs

nGGs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  for (j in 1:(ncol(data)-1)){
    nGGs[i] = nGGs[i] + ((data[i,j] == "G") & (data[i,j+1] == "G"))} 
}
nGGs

# NUMBER OF CONSECUTIVE Cs

nCCs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  for (j in 1:(ncol(data)-1)){
    nCCs[i] = nCCs[i] + ((data[i,j] == "C") & (data[i,j+1] == "C"))} 
}
nCCs

# NUMBER OF ATs

nATs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  for (j in 1:(ncol(data)-1)){
    nATs[i] = nATs[i] + ((data[i,j] == "A") & (data[i,j+1] == "T"))} 
}
nATs

# NUMBER OF TAs

nTAs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  for (j in 1:(ncol(data)-1)){
    nTAs[i] = nTAs[i] + ((data[i,j] == "T") & (data[i,j+1] == "A"))} 
}
nTAs

# NUMBER OF CGs

nCGs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  for (j in 1:(ncol(data)-1)){
    nCGs[i] = nCGs[i] + ((data[i,j] == "C") & (data[i,j+1] == "G"))} 
}
nCGs

# NUMBER OF GCs

nGCs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  for (j in 1:(ncol(data)-1)){
    nGCs[i] = nGCs[i] + ((data[i,j] == "G") & (data[i,j+1] == "C"))} 
}
nGCs

# NUMBER OF GAGs

nGAGs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  for (j in 1:(ncol(data)-2)){
    nGAGs[i] = nGAGs[i] + ((data[i,j] == "G") & (data[i,j+1] == "A") & (data[i,j+2] == "G"))} 
}
nGAGs

# NUMBER OF AGAs

nAGAs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  for (j in 1:(ncol(data)-2)){
    nAGAs[i] = nAGAs[i] + ((data[i,j] == "A") & (data[i,j+1] == "G") & (data[i,j+2] == "A"))} 
}
nAGAs

# PROPORTION OF C/G to A/T

pCGtoAT = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){ 
  pCGtoAT[i] = ( nCs[i] / nGs[i] ) / ( nAs[i] / nTs[i] )
}
pCGtoAT

# Adding to data

EFdata = data

EFdata$nAs = nAs
EFdata$nTs = nTs
EFdata$nGs = nGs
EFdata$nCs = nCs
EFdata$nAAs = nAAs
EFdata$nTTs = nTTs
EFdata$nGGs = nGGs
EFdata$nCCs = nCCs
EFdata$nATs = nATs
EFdata$nTAs = nTAs
EFdata$nCGs = nCGs
EFdata$nGCs = nGCs
EFdata$nGAGs = nGAGs
EFdata$nAGAs = nAGAs
EFdata$pCGtoAT = pCGtoAT

EFdata = EFdata[,c(1,102:116)]

# SAVE

save(EFdata, file = "dna-data/data/e-f-data.RData")
