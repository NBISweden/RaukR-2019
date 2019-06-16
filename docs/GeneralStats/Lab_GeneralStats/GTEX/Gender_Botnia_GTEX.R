library("DESeq2")
Path<-"C:/Bioinformatics/BotniaRNAseq"
setwd(Path)
df<-read.table("htseq_count_MergedFiles_DESeq_Normalized.txt",header=TRUE,check.names=FALSE,row.names=1,sep="\t")
df[,"82283_FHPlus_M3"]<-NULL
Males<-c("82420","82725","84105","84719","82019","82817","82839","83041","83250","84173","84353")
Females<-c("82066","82283","82292","84711","82407","82414","84149","84221")
Samples<-read.table("BotniaExerciseStudySamples.txt",header=FALSE,sep="\t")
datafile<-data.frame(matrix(nrow=dim(df)[1],ncol=0))
for(i in 1:dim(Samples)[1])
{
  Subset<-subset(df,select=colnames(df)[grep(as.character(Samples$V1[i]),colnames(df))])
  #SampleMedian<-apply(Subset,1,median)
  SampleMedian<-apply(Subset,1,mean)
  datafile[,substr(colnames(Subset)[1],1,nchar(colnames(Subset)[1])-3)]<-round(SampleMedian,0)
}
rownames(datafile)<-rownames(df)
Males_expr<-subset(datafile,select=colnames(datafile)[grep(paste(Males,collapse="|",sep=""),colnames(datafile))])
Females_expr<-subset(datafile,select=colnames(datafile)[grep(paste(Females,collapse="|",sep=""),colnames(datafile))])

datafile<-cbind(Males_expr,Females_expr)

ConditionVector<-c(colnames(Males_expr),colnames(Females_expr))
ConditionVector[grep(paste(Males,collapse="|",sep=""),ConditionVector)]<-"Males"
ConditionVector[grep(paste(Females,collapse="|",sep=""),ConditionVector)]<-"Females"

design_matrix<-data.frame(row.names=colnames(datafile),condition=ConditionVector)
condition<-factor(ConditionVector)

dds<-DESeqDataSetFromMatrix(countData=datafile,colData=design_matrix,design=~condition)
dds
dds<-DESeq(dds)
res<-results(dds)
res<-res[order(res$padj,res$pvalue),]
head(res)
annot<-read.table("C:/Bioinformatics/BotniaRNAseq/gene_pos_HTL_eQTL.txt",header=TRUE,sep="\t")
res$CHR<-annot$CHR[match(rownames(res),as.character(annot$GENE_NAME))]
write.table(res,file=paste(Path,"/DGE/","htseq_count_MergedFiles_DESeq_Normalized_DGE_DESeq2_Males_Females_Results_Without82283_FHPlus_M3_TechRepl_Mean.txt",sep=""),col.names=TRUE,row.names=TRUE,quote=FALSE,sep="\t")

FC<-vector()
Males_mean<-vector()
Females_mean<-vector()
for(i in 1:length(rownames(Males_expr)))
{
  FC<-append(FC,mean(as.numeric(Males_expr[i,]),na.rm=TRUE)/mean(as.numeric(Females_expr[i,]),na.rm=TRUE))
  Males_mean<-append(Males_mean,mean(as.numeric(Males_expr[i,]),na.rm=TRUE))
  Females_mean<-append(Females_mean,mean(as.numeric(Females_expr[i,]),na.rm=TRUE))
}
FoldChange<-data.frame(GENE=rownames(Males_expr),FC=FC,Males_mean=Males_mean,Females_mean=Females_mean)
res<-as.data.frame(res)
res$FC<-FoldChange$FC[match(rownames(res),as.character(FoldChange$GENE))]
res$Males_mean<-FoldChange$Males_mean[match(rownames(res),as.character(FoldChange$GENE))]
res$Females_mean<-FoldChange$Females_mean[match(rownames(res),as.character(FoldChange$GENE))]
res<-res[is.na(res$CHR)==FALSE,]
res<-res[as.character(res$CHR)!="chrX" & as.character(res$CHR)!="chrY",]
write.table(res,file=paste(Path,"/DGE/","htseq_count_MergedFiles_DESeq_Normalized_DGE_DESeq2_Males_Females_Results_Without82283_FHPlus_M3_TechRepl_Mean_Autosomes.txt",sep=""),col.names=TRUE,row.names=TRUE,quote=FALSE,sep="\t")

res<-res[order(-res$FC),]
res<-res[res$Males_mean>10 & res$Females_mean>10,]
res$padj<-p.adjust(res$pvalue,method="fdr")
res<-res[res$padj<0.05,]
write.table(res,file=paste(Path,"/DGE/","htseq_count_MergedFiles_DESeq_Normalized_DGE_DESeq2_Males_Females_Results_Without82283_FHPlus_M3_TechRepl_Mean_FDR5_FCMalesSorted.txt",sep=""),col.names=TRUE,row.names=TRUE,quote=FALSE,sep="\t")
res$InvFC<-1/res$FC
res<-res[order(-res$InvFC),]
write.table(res,file=paste(Path,"/DGE/","htseq_count_MergedFiles_DESeq_Normalized_DGE_DESeq2_Males_Females_Results_Without82283_FHPlus_M3_TechRepl_Mean_FDR5_FCFemalesSorted.txt",sep=""),col.names=TRUE,row.names=TRUE,quote=FALSE,sep="\t")



########################################### GTEX ######################################################################
library("DESeq2")
Path<-"C:/Bioinformatics/BotniaRNAseq"
setwd(Path)
df<-read.table("TMM_NormalizedCounts_157_Samples_Muscle.txt",header=TRUE,check.names=FALSE,row.names=1,sep="\t")
GTEX_annot<-read.table("GTEx_Data_2014-01-17_Annotations_SubjectPhenotypes_DS.txt",header=TRUE,row.names=1,sep="\t")
GTEX_annot_Males<-GTEX_annot[GTEX_annot$GENDER==1,]
GTEX_annot_Females<-GTEX_annot[GTEX_annot$GENDER==2,]
Males_expr<-subset(df,select=colnames(df)[grepl(paste(rownames(GTEX_annot_Males),collapse="|",sep=""),colnames(df))])
Females_expr<-subset(df,select=colnames(df)[grepl(paste(rownames(GTEX_annot_Females),collapse="|",sep=""),colnames(df))])

datafile<-cbind(Males_expr,Females_expr)

#datafile1<-datafile[1:10000,]
#X<-t(datafile1)
#lasso_fit <- cv.glmnet(X, Y, family="binomial")
#plot(lasso_fit)
#lasso_fit <- cv.glmnet(X, Y, family="binomial", alpha=0.5)
#plot(lasso_fit)
#result<-data.frame(GENE=names(as.matrix(coef(lasso_fit, s = "lambda.min"))[as.matrix(coef(lasso_fit, s = "lambda.min"))[,1]!=0,1])[-1],SCORE=as.numeric(as.matrix(coef(lasso_fit, s = "lambda.min"))[as.matrix(coef(lasso_fit, s = "lambda.min"))[,1]!=0,1])[-1])
#result<-result[order(-abs(result$SCORE)),]
#head(result)



ConditionVector<-c(colnames(Males_expr),colnames(Females_expr))
ConditionVector[grep(paste(colnames(Males_expr),collapse="|",sep=""),ConditionVector)]<-"Males"
ConditionVector[grep(paste(colnames(Females_expr),collapse="|",sep=""),ConditionVector)]<-"Females"

design_matrix<-data.frame(row.names=colnames(datafile),condition=ConditionVector)
condition<-factor(ConditionVector)

dds<-DESeqDataSetFromMatrix(countData=datafile,colData=design_matrix,design=~condition)
dds
dds<-DESeq(dds)
res<-results(dds)
#res<-res[order(res$padj,res$pvalue),]
head(res)
GTEX_reads<-read.delim("C:/Bioinformatics/GTex/Expression/GTEx_Analysis_2014-01-17_RNA-seq_RNA-SeQCv1.1.8_gene_reads.gct",header=TRUE,check.names=FALSE,skip=2,comment.char="#",sep="\t")
annot<-read.table("C:/Bioinformatics/BotniaRNAseq/gene_pos_HTL_eQTL.txt",header=TRUE,sep="\t")
res$GENE_SYMBOL<-GTEX_reads$Description
res$CHR<-annot$CHR[match(as.character(res$GENE_SYMBOL),as.character(annot$GENE_NAME))]
head(res)
write.table(res,file=paste(Path,"/DGE/","GTEX_TMM_Normalized_DGE_DESeq2_Males_Females.txt",sep=""),col.names=TRUE,row.names=TRUE,quote=FALSE,sep="\t")

FC<-vector()
Males_mean<-vector()
Females_mean<-vector()
for(i in 1:length(rownames(Males_expr)))
{
  FC<-append(FC,mean(as.numeric(Males_expr[i,]),na.rm=TRUE)/mean(as.numeric(Females_expr[i,]),na.rm=TRUE))
  Males_mean<-append(Males_mean,mean(as.numeric(Males_expr[i,]),na.rm=TRUE))
  Females_mean<-append(Females_mean,mean(as.numeric(Females_expr[i,]),na.rm=TRUE))
}
FoldChange<-data.frame(GENE=rownames(Males_expr),FC=FC,Males_mean=Males_mean,Females_mean=Females_mean)
res<-as.data.frame(res)
res$FC<-FoldChange$FC[match(rownames(res),as.character(FoldChange$GENE))]
res$Males_mean<-FoldChange$Males_mean[match(rownames(res),as.character(FoldChange$GENE))]
res$Females_mean<-FoldChange$Females_mean[match(rownames(res),as.character(FoldChange$GENE))]
res<-res[is.na(res$CHR)==FALSE,]
res<-res[as.character(res$CHR)!="chrX" & as.character(res$CHR)!="chrY",]
write.table(res,file=paste(Path,"/DGE/","GTEX_TMM_Normalized_DGE_DESeq2_Males_Females_Autosomes.txt",sep=""),col.names=TRUE,row.names=TRUE,quote=FALSE,sep="\t")

res<-res[order(-res$FC),]
res<-res[res$Males_mean>10 & res$Females_mean>10,]
res$padj<-p.adjust(res$pvalue,method="fdr")
res<-res[res$padj<0.05,]
write.table(res,file=paste(Path,"/DGE/","GTEX_TMM_Normalized_DGE_DESeq2_Males_Females_FDR5_FCMalesSorted.txt",sep=""),col.names=TRUE,row.names=TRUE,quote=FALSE,sep="\t")
res$InvFC<-1/res$FC
res<-res[order(-res$InvFC),]
head(res)
write.table(res,file=paste(Path,"/DGE/","GTEX_TMM_Normalized_DGE_DESeq2_Males_Females_FDR5_FCFemalesSorted.txt",sep=""),col.names=TRUE,row.names=TRUE,quote=FALSE,sep="\t")























#dge_gender<-read.table(paste(Path,"/DGE/","htseq_count_MergedFiles_DESeq_Normalized_DGE_DESeq2_Males_Females_Results_Without82283_FHPlus_M3_TechRepl_Mean_temp.txt",sep=""),header=TRUE,sep="\t")
dge_gender<-read.table("C:/Bioinformatics/GTex/Expression/ExpressionByTissue/TMM_NormalizedCounts_157_Samples_Muscle.txt",header=TRUE,row.names=1,check.names=FALSE,sep="\t")
GTEX_annot<-read.table("C:/Bioinformatics/GTex/Expression/GTEx_Data_2014-01-17_Annotations_SubjectPhenotypes_DS.txt",header=TRUE,row.names=1,check.names=FALSE,sep="\t")
GTEX_annot_Males<-GTEX_annot[GTEX_annot$GENDER==1,]
GTEX_annot_Females<-GTEX_annot[GTEX_annot$GENDER==2,]

Males_expr<-subset(dge_gender,select=colnames(dge_gender)[grepl(paste(rownames(GTEX_annot_Males),collapse="|",sep=""),colnames(dge_gender))])
Females_expr<-subset(dge_gender,select=colnames(dge_gender)[grepl(paste(rownames(GTEX_annot_Females),collapse="|",sep=""),colnames(dge_gender))])

FC<-vector()
Males_mean<-vector()
Females_mean<-vector()
for(i in 1:length(rownames(Males_expr)))
{
  FC<-append(FC,mean(as.numeric(Males_expr[i,]),na.rm=TRUE)/mean(as.numeric(Females_expr[i,]),na.rm=TRUE))
  Males_mean<-append(Males_mean,mean(as.numeric(Males_expr[i,]),na.rm=TRUE))
  Females_mean<-append(Females_mean,mean(as.numeric(Females_expr[i,]),na.rm=TRUE))
}
dge_gender<-data.frame(GENE=rownames(Males_expr),FC=FC,Males_mean=Males_mean,Females_mean=Females_mean)
setwd("C:/Bioinformatics/GTex/Expression")
df<-read.delim("GTEx_Analysis_2014-01-17_RNA-seq_RNA-SeQCv1.1.8_gene_reads.gct",header=TRUE,check.names=FALSE,skip=2,comment.char="#",sep="\t")
dge_gender$GENE_SYMBOL<-df$Description
#dge_gender$FC<-FC
#dge_gender$Males_mean<-Males_mean
#dge_gender$Females_mean<-Females_mean
annot<-read.table("C:/Bioinformatics/BotniaRNAseq/gene_pos_HTL_eQTL.txt",header=TRUE,sep="\t")
dge_gender$CHR<-annot$CHR[match(as.character(dge_gender$GENE_SYMBOL),as.character(annot$GENE_NAME))]
#dge_gender<-dge_gender[order(dge_gender$padj,dge_gender$pvalue),]

#dge_gender<-na.omit(dge_gender)
dge_gender<-dge_gender[as.character(dge_gender$CHR)!="chrX" & as.character(dge_gender$CHR)!="chrY",]
dge_gender<-dge_gender[as.character(dge_gender$FC)!="Inf",]
dge_gender<-dge_gender[order(-dge_gender$FC),]
dge_gender<-dge_gender[dge_gender$Males_mean>10 & dge_gender$Females_mean>10,]
dge_gender$padj<-p.adjust(dge_gender$pvalue,method="fdr")
dge_gender<-dge_gender[dge_gender$padj<0.05,]
head(dge_gender,20)









