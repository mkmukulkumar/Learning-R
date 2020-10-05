#Get the protein sequence using DNA sequence 
a<-scan(file = "seq.txt", what = character() )
#Scan file of DNA sequence in same directory
protein=NULL
a<-toupper(a)
i=1
count=0
while (i<nchar(a))
  { x<-substr(a,i,i+2)
    if (x=="ATG")
    { 
      protein<-append(protein,"START") 
      while(i<nchar(a)) 
        {
          x<-substr(a,i,i+2)
          if(x=="TTT"||x=="TTC")
          {protein<-append(protein,"PHE")}
          if(x=="TTA"||x=="TTG")
          {protein<-append(protein,"LEU")}
          if(x=="TCT"||x=="TCC"||x=="TCA"||x=="TCG")
          {protein<-append(protein,"SER")}
          if(x=="TAT"||x=="TAC")
          {protein<-append(protein,"TYR")}
          if(x=="TAA"||x=="TAG"||x=="TGA")
          { protein<-append(protein,"STOP") 
            break;  }
          if(x=="TGT"||x=="TGC")
          {protein<-append(protein,"CYS")}
          if(x=="TGG")
          {protein<-append(protein,"TRP")}
          if(x=="CTT"||x=="CTC"||x=="CTA"||x=="CTG")
          {protein<-append(protein,"LEU")}
          if(x=="CCT"||x=="CCC"||x=="CCA"||x=="CCG")
          {protein<-append(protein,"PRO")}
          if(x=="CAT"||x=="CAC")
          {protein<-append(protein,"HIS")}
          if(x=="CAG"||x=="CAA")
          {protein<-append(protein,"GLN")}
          if(x=="CGT"||x=="CGC"||x=="CGA"||x=="CGG")
          {protein<-append(protein,"ARG")}
          if(x=="ATT"||x=="ATC"||x=="ATA")
          {protein<-append(protein,"ILE")}
          if(x=="ATG")
            {count=count+1
              if(count>1)  
              {protein<-append(protein,"MET")}
            }
          if(x=="ACT"||x=="ACC"||x=="ACA"||x=="ACG")
          {protein<-append(protein,"THR")}
          if(x=="AAT"||x=="AAC")
          {protein<-append(protein,"ASN")}
          if(x=="AAG"||x=="AAA")
          {protein<-append(protein,"LYS")}
          if(x=="AGT"||x=="AGC")
          {protein<-append(protein,"SER")}
          if(x=="AGG"||x=="AGA")
          {protein<-append(protein,"ARG")}
          if(x=="GTT"||x=="GTC"||x=="GTA"||x=="GTG")
          {protein<-append(protein,"VAL")}
          if(x=="GCT"||x=="GCC"||x=="GCA"||x=="GCG")
          {protein<-append(protein,"ALA")}
          if(x=="GAT"||x=="GAC")
          {protein<-append(protein,"ASP")}
          if(x=="GAG"||x=="GAA")
          {protein<-append(protein,"GLU")}
          if(x=="GGT"||x=="GGC"||x=="GGA"||x=="GGG")
          {protein<-append(protein,"GLY")}
          
          i=i+3
        }
    }
  i=i+1
}
print(protein)