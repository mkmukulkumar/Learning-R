#Get the protein sequence using DNA sequence 
rd<-scan(file = "DNA Sequence.txt", what = character())
#rd takes input of DNA sequence from user
rd<-toupper(rd)
protein=NULL
rna=NULL
#Convert string to vector with transcription
for (i in 1:nchar(rd)) {
  if ((substr(rd,i,i))=="T")
    rna<-append(rna,"A")
  if ((substr(rd,i,i))=="A")
    rna<-append(rna,"U")
  if ((substr(rd,i,i))=="G")
    rna<-append(rna,"C")
  if ((substr(rd,i,i))=="C")
    rna<-append(rna,"G")
}
#To show RNA sequence
y=NULL
for (i in 1:length(rna)) {
  y=  paste(y,rna[i],sep = "")
}
print("RNA sequence:")
print(y)

#find protein sequence
for (i in 1:nchar(y))
  { codon<-substr(y,i,i+2)
    if (codon=="AUG")
    { 
      protein<-append(protein,"Nterm-") 
      while(i<nchar(y)) 
        {
          codon<-substr(y,i,i+2)
          if(codon=="AUG")
           {protein<-append(protein,"M")}
          if(codon=="UGG")
            {protein<-append(protein,"W")}
          if(codon=="UAU"||codon=="UAC")
            {protein<-append(protein,"Y")}
          if(codon=="UGU"||codon=="UGC")
            {protein<-append(protein,"C")}
          if(codon=="AAU"||codon=="AAC")
            {protein<-append(protein,"N")}
          if(codon=="AAG"||codon=="AAA")
            {protein<-append(protein,"K")}
          if(codon=="GAU"||codon=="GAC")
            {protein<-append(protein,"D")}
          if(codon=="GAG"||codon=="GAA")
            {protein<-append(protein,"E")}
          if(codon=="CAU"||codon=="CAC")
            {protein<-append(protein,"H")}
          if(codon=="CAG"||codon=="CAA")
            {protein<-append(protein,"Q")}
          if(codon=="UUU"||codon=="UUC")
            {protein<-append(protein,"F")}
          if(codon=="CCU"||codon=="CCC"||codon=="CCA"||codon=="CCG")
            {protein<-append(protein,"P")}
          if(codon=="ACU"||codon=="ACC"||codon=="ACA"||codon=="ACG")
            {protein<-append(protein,"T")}
          if(codon=="GUU"||codon=="GUC"||codon=="GUA"||codon=="GUG")
            {protein<-append(protein,"V")}
          if(codon=="GCU"||codon=="GCC"||codon=="GCA"||codon=="GCG")
            {protein<-append(protein,"A")}
          if(codon=="GGU"||codon=="GGC"||codon=="GGA"||codon=="GGG")
            {protein<-append(protein,"G")}
          if(codon=="AUU"||codon=="AUC"||codon=="AUA")
            {protein<-append(protein,"I")}
          if(codon=="UAA"||codon=="UAG"||codon=="UGA")
            { protein<-append(protein,"-Cterm") 
          break; 
          }
          if(codon=="UUA"||codon=="UUG"||codon=="CUU"||codon=="CUC"||codon=="CUA"||codon=="CUG")
            {protein<-append(protein,"L")}
          if(codon=="UCU"||codon=="UCC"||codon=="UCA"||codon=="UCG"||codon=="AGU"||codon=="AGC")
            {protein<-append(protein,"S")}
          if(codon=="CGU"||codon=="CGC"||codon=="CGA"||codon=="CGG"||codon=="AGG"||codon=="AGA")
            {protein<-append(protein,"R")}
          i=i+3
        }
    }
  i=i+1
}
x=NULL
print("Protein Sequence:")
for (i in 1:length(protein)) {
  x=  paste(x,protein[i],sep = "")
}
print(x)