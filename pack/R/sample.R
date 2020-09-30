
#' Using two data.frames to create sample sets
#'
#' @param x a data.frame or matrix
#' @param y a data.frame or matrix
#'
#' @return 2000 sample sets
#' @export
#'
#' @examples
#' my_sample(GSE85589,GSE59856)#GSE85589 is a data.frame,GSE59856 is also a data.frame

my_sample<-function(x,y){
  common_mir_inter<-intersect(row.names(x),row.names(y))
  x_common<-x[common_mir_inter,]
  y_common<-y[common_mir_inter,]
  write.table(x_common,"x_common.txt",sep="\t",quote=F,row.names=T,col.names=T)
  write.table(y_common,"y_common.txt",sep="\t",quote=F,row.names=T,col.names=T)


  lable_y<-y[1,]
  case_sample_1<- y_common[,which(lable_y=="1")]
  control_sample_1<-y_common[,which(lable_y=="0")]
  case_sample<-case_sample_1
  control_sample<-control_sample_1

  library(siggenes)
  library(gplots)
  library(multtest)
  mm <-list()


  for(k in 1:100){
    a1 <- sample(1:dim(case_sample)[2], 20)
    b1<- sample(1:dim(control_sample)[2], 30)
    d1_case <- case_sample[,a1]
    d1_control <- control_sample[,b1]

    nn1 <- setdiff(1:dim(case_sample)[2],a1)
    mm1 <- setdiff(1:dim(control_sample)[2],b1)
    a2 <- sample(nn1, 20)
    b2 <- sample(mm1, 30)
    d2_case <-  case_sample[,a2]
    d2_control <-control_sample[,b2]

    s1<-c(a1,a2)
    s2<-c(b1,b2)
    nn2<-setdiff(1:dim(case_sample)[2],s1)
    mm2<- setdiff(1:dim(control_sample)[2],s2)
    a3<-sample(nn2,20)
    b3<-sample(mm2,30)
    d3_case <-case_sample[,a3]
    d3_control <-control_sample[,b3]

    s3<-c(a1,a2,a3)
    s4<-c(b1,b2,b3)
    nn3<-setdiff(1:dim(case_sample)[2],s3)
    mm3<- setdiff(1:dim(control_sample)[2],s4)
    a4<-sample(nn3,20)
    b4<-sample(mm3,30)
    d4_case <-case_sample[,a4]
    d4_control <-control_sample[,b4]

    s5<-c(a1,a2,a3,a4)
    s6<-c(b1,b2,b3,b4)
    nn4<-setdiff(1:dim(case_sample)[2],s5)
    mm4<- setdiff(1:dim(control_sample)[2],s6)
    d5_case <-case_sample[,nn4]
    d5_control <-control_sample[,mm4]

    test1_case<-d1_case
    test1_control<-d1_control
    evaluate1_case<-d2_case
    evaluate1_control<-d2_control
    select1_case<-cbind(d3_case,d4_case,d5_case)
    select1_control<-cbind(d3_control,d4_control,d5_control)

    write.csv(test1_case,paste("test1_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test1_control,paste("test1_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate1_case,paste("evaluate1_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate1_control,paste("evaluate1_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select1_case,paste("select1_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select1_control,paste("select1_control[",k,"].csv",sep=""),quote=F,row.names=T)

    test2_case<-d1_case
    test2_control<-d1_control
    evaluate2_case<-d3_case
    evaluate2_control<-d3_control
    select2_case<-cbind(d2_case,d4_case,d5_case)
    select2_control<-cbind(d2_control,d4_control,d5_control)

    write.csv(test2_case,paste("test2_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test2_control,paste("test2_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate2_case,paste("evaluate2_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate2_control,paste("evaluate2_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select2_case,paste("select2_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select2_control,paste("select2_control[",k,"].csv",sep=""),quote=F,row.names=T)

    test3_case<-d1_case
    test3_control<-d1_control
    evaluate3_case<-d4_case
    evaluate3_control<-d4_control
    select3_case<-cbind(d2_case,d3_case,d5_case)
    select3_control<-cbind(d2_control,d3_control,d5_control)

    write.csv(test3_case,paste("test3_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test3_control,paste("test3_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate3_case,paste("evaluate3_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate3_control,paste("evaluate3_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select3_case,paste("select3_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select3_control,paste("select3_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test4_case<-d1_case
    test4_control<-d1_control
    evaluate4_case<-d5_case
    evaluate4_control<-d5_control
    select4_case<-cbind(d2_case,d3_case,d4_case)
    select4_control<-cbind(d2_control,d3_control,d4_control)

    write.csv(test4_case,paste("test4_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test4_control,paste("test4_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate4_case,paste("evaluate4_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate4_control,paste("evaluate4_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select4_case,paste("select4_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select4_control,paste("select4_control[",k,"].csv",sep=""),quote=F,row.names=T)

    test5_case<-d2_case
    test5_control<-d2_control
    evaluate5_case<-d1_case
    evaluate5_control<-d1_control
    select5_case<-cbind(d3_case,d4_case,d5_case)
    select5_control<-cbind(d3_control,d4_control,d5_control)

    write.csv(test5_case,paste("test5_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test5_control,paste("test5_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate5_case,paste("evaluate5_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate5_control,paste("evaluate5_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select5_case,paste("select5_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select5_control,paste("select5_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test6_case<-d2_case
    test6_control<-d2_control
    evaluate6_case<-d3_case
    evaluate6_control<-d3_control
    select6_case<-cbind(d1_case,d4_case,d5_case)
    select6_control<-cbind(d1_control,d4_control,d5_control)

    write.csv(test6_case,paste("test6_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test6_control,paste("test6_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate6_case,paste("evaluate6_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate6_control,paste("evaluate6_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select6_case,paste("select6_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select6_control,paste("select6_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test7_case<-d2_case
    test7_control<-d2_control
    evaluate7_case<-d4_case
    evaluate7_control<-d4_control
    select7_case<-cbind(d1_case,d3_case,d5_case)
    select7_control<-cbind(d1_control,d3_control,d5_control)

    write.csv(test7_case,paste("test7_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test7_control,paste("test7_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate7_case,paste("evaluate7_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate7_control,paste("evaluate7_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select7_case,paste("select7_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select7_control,paste("select7_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test8_case<-d2_case
    test8_control<-d2_control
    evaluate8_case<-d5_case
    evaluate8_control<-d5_control
    select8_case<-cbind(d1_case,d3_case,d4_case)
    select8_control<-cbind(d1_control,d3_control,d4_control)

    write.csv(test8_case,paste("test8_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test8_control,paste("test8_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate8_case,paste("evaluate8_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate8_control,paste("evaluate8_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select8_case,paste("select8_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select8_control,paste("select8_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test9_case<-d3_case
    test9_control<-d3_control
    evaluate9_case<-d1_case
    evaluate9_control<-d1_control
    select9_case<-cbind(d2_case,d4_case,d5_case)
    select9_control<-cbind(d2_control,d4_control,d5_control)

    write.csv(test9_case,paste("test9_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test9_control,paste("test9_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate9_case,paste("evaluate9_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate9_control,paste("evaluate9_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select9_case,paste("select9_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select9_control,paste("select9_control[",k,"].csv",sep=""),quote=F,row.names=T)



    test10_case<-d3_case
    test10_control<-d3_control
    evaluate10_case<-d2_case
    evaluate10_control<-d2_control
    select10_case<-cbind(d1_case,d4_case,d5_case)
    select10_control<-cbind(d1_control,d4_control,d5_control)

    write.csv(test10_case,paste("test10_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test10_control,paste("test10_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate10_case,paste("evaluate10_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate10_control,paste("evaluate10_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select10_case,paste("select10_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select10_control,paste("select10_control[",k,"].csv",sep=""),quote=F,row.names=T)




    test11_case<-d3_case
    test11_control<-d3_control
    evaluate11_case<-d4_case
    evaluate11_control<-d4_control
    select11_case<-cbind(d1_case,d2_case,d5_case)
    select11_control<-cbind(d1_control,d2_control,d5_control)

    write.csv(test11_case,paste("test11_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test11_control,paste("test11_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate11_case,paste("evaluate11_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate11_control,paste("evaluate11_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select11_case,paste("select11_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select11_control,paste("select11_control[",k,"].csv",sep=""),quote=F,row.names=T)

    test12_case<-d3_case
    test12_control<-d3_control
    evaluate12_case<-d5_case
    evaluate12_control<-d5_control
    select12_case<-cbind(d1_case,d2_case,d4_case)
    select12_control<-cbind(d1_control,d2_control,d4_control)

    write.csv(test12_case,paste("test12_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test12_control,paste("test12_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate12_case,paste("evaluate12_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate12_control,paste("evaluate12_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select12_case,paste("select12_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select12_control,paste("select12_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test13_case<-d4_case
    test13_control<-d4_control
    evaluate13_case<-d1_case
    evaluate13_control<-d1_control
    select13_case<-cbind(d2_case,d3_case,d5_case)
    select13_control<-cbind(d2_control,d3_control,d5_control)


    write.csv(test13_case,paste("test13_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test13_control,paste("test13_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate13_case,paste("evaluate13_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate13_control,paste("evaluate13_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select13_case,paste("select13_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select13_control,paste("select13_control[",k,"].csv",sep=""),quote=F,row.names=T)




    test14_case<-d4_case
    test14_control<-d4_control
    evaluate14_case<-d2_case
    evaluate14_control<-d2_control
    select14_case<-cbind(d1_case,d3_case,d5_case)
    select14_control<-cbind(d1_control,d3_control,d5_control)

    write.csv(test14_case,paste("test14_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test14_control,paste("test14_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate14_case,paste("evaluate14_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate14_control,paste("evaluate14_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select14_case,paste("select14_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select14_control,paste("select14_control[",k,"].csv",sep=""),quote=F,row.names=T)



    test15_case<-d4_case
    test15_control<-d4_control
    evaluate15_case<-d3_case
    evaluate15_control<-d3_control
    select15_case<-cbind(d1_case,d2_case,d5_case)
    select15_control<-cbind(d1_control,d2_control,d5_control)

    write.csv(test15_case,paste("test15_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test15_control,paste("test15_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate15_case,paste("evaluate15_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate15_control,paste("evaluate15_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select15_case,paste("select15_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select15_control,paste("select15_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test16_case<-d4_case
    test16_control<-d4_control
    evaluate16_case<-d5_case
    evaluate16_control<-d5_control
    select16_case<-cbind(d1_case,d2_case,d3_case)
    select16_control<-cbind(d1_control,d2_control,d3_control)

    write.csv(test16_case,paste("test16_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test16_control,paste("test16_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate16_case,paste("evaluate16_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate16_control,paste("evaluate16_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select16_case,paste("select16_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select16_control,paste("select16_control[",k,"].csv",sep=""),quote=F,row.names=T)




    test17_case<-d5_case
    test17_control<-d5_control
    evaluate17_case<-d1_case
    evaluate17_control<-d1_control
    select17_case<-cbind(d2_case,d3_case,d4_case)
    select17_control<-cbind(d2_control,d3_control,d4_control)

    write.csv(test17_case,paste("test17_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test17_control,paste("test17_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate17_case,paste("evaluate17_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate17_control,paste("evaluate17_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select17_case,paste("select17_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select17_control,paste("select17_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test18_case<-d5_case
    test18_control<-d5_control
    evaluate18_case<-d2_case
    evaluate18_control<-d2_control
    select18_case<-cbind(d1_case,d3_case,d4_case)
    select18_control<-cbind(d1_control,d3_control,d4_control)

    write.csv(test18_case,paste("test18_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test18_control,paste("test18_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate18_case,paste("evaluate18_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate18_control,paste("evaluate18_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select18_case,paste("select18_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select18_control,paste("select18_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test19_case<-d5_case
    test19_control<-d5_control
    evaluate19_case<-d3_case
    evaluate19_control<-d3_control
    select19_case<-cbind(d1_case,d2_case,d4_case)
    select19_control<-cbind(d1_control,d2_control,d4_control)

    write.csv(test19_case,paste("test19_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test19_control,paste("test19_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate19_case,paste("evaluate19_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate19_control,paste("evaluate19_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select19_case,paste("select19_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select19_control,paste("select19_control[",k,"].csv",sep=""),quote=F,row.names=T)



    test20_case<-d5_case
    test20_control<-d5_control
    evaluate20_case<-d4_case
    evaluate20_control<-d4_control
    select20_case<-cbind(d1_case,d2_case,d3_case)
    select20_control<-cbind(d1_control,d2_control,d3_control)

    write.csv(test20_case,paste("test20_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test20_control,paste("test20_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate20_case,paste("evaluate20_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate20_control,paste("evaluate20_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select20_case,paste("select20_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select20_control,paste("select20_control[",k,"].csv",sep=""),quote=F,row.names=T)
  }
  lable_x<-x[1,]
  case_sample_1<- x_common[,which(lable_x=="1")]
  control_sample_1<-x_common[,which(lable_x=="0")]
  case_sample<-case_sample_1
  control_sample<-control_sample_1
  for(k in 1:100){
    a1 <- sample(1:dim(case_sample)[2], 20)
    b1<- sample(1:dim(control_sample)[2], 4)
    d1_case <- case_sample[,a1]
    d1_control <- control_sample[,b1]

    nn1 <- setdiff(1:dim(case_sample)[2],a1)
    mm1 <- setdiff(1:dim(control_sample)[2],b1)
    a2 <- sample(nn1, 20)
    b2 <- sample(mm1, 4)
    d2_case <-  case_sample[,a2]
    d2_control <-control_sample[,b2]

    s1<-c(a1,a2)
    s2<-c(b1,b2)
    nn2<-setdiff(1:dim(case_sample)[2],s1)
    mm2<- setdiff(1:dim(control_sample)[2],s2)
    a3<-sample(nn2,20)
    b3<-sample(mm2,4)
    d3_case <-case_sample[,a3]
    d3_control <-control_sample[,b3]

    s3<-c(a1,a2,a3)
    s4<-c(b1,b2,b3)
    nn3<-setdiff(1:dim(case_sample)[2],s3)
    mm3<- setdiff(1:dim(control_sample)[2],s4)
    a4<-sample(nn3,20)
    b4<-sample(mm3,4)
    d4_case <-case_sample[,a4]
    d4_control <-control_sample[,b4]

    s5<-c(a1,a2,a3,a4)
    s6<-c(b1,b2,b3,b4)
    nn4<-setdiff(1:dim(case_sample)[2],s5)
    mm4<- setdiff(1:dim(control_sample)[2],s6)
    d5_case <-case_sample[,nn4]
    d5_control <-control_sample[,mm4]

    test1_case<-d1_case
    test1_control<-d1_control
    evaluate1_case<-d2_case
    evaluate1_control<-d2_control
    select1_case<-cbind(d3_case,d4_case,d5_case)
    select1_control<-cbind(d3_control,d4_control,d5_control)

    write.csv(test1_case,paste("test1_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test1_control,paste("test1_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate1_case,paste("evaluate1_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate1_control,paste("evaluate1_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select1_case,paste("select1_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select1_control,paste("select1_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test2_case<-d1_case
    test2_control<-d1_control
    evaluate2_case<-d3_case
    evaluate2_control<-d3_control
    select2_case<-cbind(d2_case,d4_case,d5_case)
    select2_control<-cbind(d2_control,d4_control,d5_control)

    write.csv(test2_case,paste("test2_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test2_control,paste("test2_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate2_case,paste("evaluate2_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate2_control,paste("evaluate2_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select2_case,paste("select2_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select2_control,paste("select2_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test3_case<-d1_case
    test3_control<-d1_control
    evaluate3_case<-d4_case
    evaluate3_control<-d4_control
    select3_case<-cbind(d2_case,d3_case,d5_case)
    select3_control<-cbind(d2_control,d3_control,d5_control)

    write.csv(test3_case,paste("test3_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test3_control,paste("test3_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate3_case,paste("evaluate3_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate3_control,paste("evaluate3_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select3_case,paste("select3_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select3_control,paste("select3_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test4_case<-d1_case
    test4_control<-d1_control
    evaluate4_case<-d5_case
    evaluate4_control<-d5_control
    select4_case<-cbind(d2_case,d3_case,d4_case)
    select4_control<-cbind(d2_control,d3_control,d4_control)

    write.csv(test4_case,paste("test4_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test4_control,paste("test4_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate4_case,paste("evaluate4_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate4_control,paste("evaluate4_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select4_case,paste("select4_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select4_control,paste("select4_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test5_case<-d2_case
    test5_control<-d2_control
    evaluate5_case<-d1_case
    evaluate5_control<-d1_control
    select5_case<-cbind(d3_case,d4_case,d5_case)
    select5_control<-cbind(d3_control,d4_control,d5_control)

    write.csv(test5_case,paste("test5_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test5_control,paste("test5_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate5_case,paste("evaluate5_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate5_control,paste("evaluate5_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select5_case,paste("select5_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select5_control,paste("select5_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test6_case<-d2_case
    test6_control<-d2_control
    evaluate6_case<-d3_case
    evaluate6_control<-d3_control
    select6_case<-cbind(d1_case,d4_case,d5_case)
    select6_control<-cbind(d1_control,d4_control,d5_control)

    write.csv(test6_case,paste("test6_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test6_control,paste("test6_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate6_case,paste("evaluate6_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate6_control,paste("evaluate6_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select6_case,paste("select6_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select6_control,paste("select6_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test7_case<-d2_case
    test7_control<-d2_control
    evaluate7_case<-d4_case
    evaluate7_control<-d4_control
    select7_case<-cbind(d1_case,d3_case,d5_case)
    select7_control<-cbind(d1_control,d3_control,d5_control)

    write.csv(test7_case,paste("test7_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test7_control,paste("test7_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate7_case,paste("evaluate7_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate7_control,paste("evaluate7_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select7_case,paste("select7_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select7_control,paste("select7_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test8_case<-d2_case
    test8_control<-d2_control
    evaluate8_case<-d5_case
    evaluate8_control<-d5_control
    select8_case<-cbind(d1_case,d3_case,d4_case)
    select8_control<-cbind(d1_control,d3_control,d4_control)

    write.csv(test8_case,paste("test8_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test8_control,paste("test8_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate8_case,paste("evaluate8_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate8_control,paste("evaluate8_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select8_case,paste("select8_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select8_control,paste("select8_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test9_case<-d3_case
    test9_control<-d3_control
    evaluate9_case<-d1_case
    evaluate9_control<-d1_control
    select9_case<-cbind(d2_case,d4_case,d5_case)
    select9_control<-cbind(d2_control,d4_control,d5_control)

    write.csv(test9_case,paste("test9_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test9_control,paste("test9_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate9_case,paste("evaluate9_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate9_control,paste("evaluate9_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select9_case,paste("select9_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select9_control,paste("select9_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test10_case<-d3_case
    test10_control<-d3_control
    evaluate10_case<-d2_case
    evaluate10_control<-d2_control
    select10_case<-cbind(d1_case,d4_case,d5_case)
    select10_control<-cbind(d1_control,d4_control,d5_control)

    write.csv(test10_case,paste("test10_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test10_control,paste("test10_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate10_case,paste("evaluate10_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate10_control,paste("evaluate10_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select10_case,paste("select10_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select10_control,paste("select10_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test11_case<-d3_case
    test11_control<-d3_control
    evaluate11_case<-d4_case
    evaluate11_control<-d4_control
    select11_case<-cbind(d1_case,d2_case,d5_case)
    select11_control<-cbind(d1_control,d2_control,d5_control)

    write.csv(test11_case,paste("test11_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test11_control,paste("test11_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate11_case,paste("evaluate11_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate11_control,paste("evaluate11_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select11_case,paste("select11_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select11_control,paste("select11_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test12_case<-d3_case
    test12_control<-d3_control
    evaluate12_case<-d5_case
    evaluate12_control<-d5_control
    select12_case<-cbind(d1_case,d2_case,d4_case)
    select12_control<-cbind(d1_control,d2_control,d4_control)

    write.csv(test12_case,paste("test12_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test12_control,paste("test12_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate12_case,paste("evaluate12_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate12_control,paste("evaluate12_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select12_case,paste("select12_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select12_control,paste("select12_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test13_case<-d4_case
    test13_control<-d4_control
    evaluate13_case<-d1_case
    evaluate13_control<-d1_control
    select13_case<-cbind(d2_case,d3_case,d5_case)
    select13_control<-cbind(d2_control,d3_control,d5_control)

    write.csv(test13_case,paste("test13_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test13_control,paste("test13_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate13_case,paste("evaluate13_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate13_control,paste("evaluate13_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select13_case,paste("select13_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select13_control,paste("select13_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test14_case<-d4_case
    test14_control<-d4_control
    evaluate14_case<-d2_case
    evaluate14_control<-d2_control
    select14_case<-cbind(d1_case,d3_case,d5_case)
    select14_control<-cbind(d1_control,d3_control,d5_control)

    write.csv(test14_case,paste("test14_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test14_control,paste("test14_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate14_case,paste("evaluate14_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate14_control,paste("evaluate14_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select14_case,paste("select14_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select14_control,paste("select14_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test15_case<-d4_case
    test15_control<-d4_control
    evaluate15_case<-d3_case
    evaluate15_control<-d3_control
    select15_case<-cbind(d1_case,d2_case,d5_case)
    select15_control<-cbind(d1_control,d2_control,d5_control)

    write.csv(test15_case,paste("test15_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test15_control,paste("test15_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate15_case,paste("evaluate15_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate15_control,paste("evaluate15_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select15_case,paste("select15_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select15_control,paste("select15_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test16_case<-d4_case
    test16_control<-d4_control
    evaluate16_case<-d5_case
    evaluate16_control<-d5_control
    select16_case<-cbind(d1_case,d2_case,d3_case)
    select16_control<-cbind(d1_control,d2_control,d3_control)

    write.csv(test16_case,paste("test16_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test16_control,paste("test16_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate16_case,paste("evaluate16_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate16_control,paste("evaluate16_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select16_case,paste("select16_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select16_control,paste("select16_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test17_case<-d5_case
    test17_control<-d5_control
    evaluate17_case<-d1_case
    evaluate17_control<-d1_control
    select17_case<-cbind(d2_case,d3_case,d4_case)
    select17_control<-cbind(d2_control,d3_control,d4_control)

    write.csv(test17_case,paste("test17_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test17_control,paste("test17_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate17_case,paste("evaluate17_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate17_control,paste("evaluate17_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select17_case,paste("select17_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select17_control,paste("select17_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test18_case<-d5_case
    test18_control<-d5_control
    evaluate18_case<-d2_case
    evaluate18_control<-d2_control
    select18_case<-cbind(d1_case,d3_case,d4_case)
    select18_control<-cbind(d1_control,d3_control,d4_control)

    write.csv(test18_case,paste("test18_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test18_control,paste("test18_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate18_case,paste("evaluate18_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate18_control,paste("evaluate18_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select18_case,paste("select18_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select18_control,paste("select18_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test19_case<-d5_case
    test19_control<-d5_control
    evaluate19_case<-d3_case
    evaluate19_control<-d3_control
    select19_case<-cbind(d1_case,d2_case,d4_case)
    select19_control<-cbind(d1_control,d2_control,d4_control)

    write.csv(test19_case,paste("test19_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test19_control,paste("test19_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate19_case,paste("evaluate19_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate19_control,paste("evaluate19_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select19_case,paste("select19_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select19_control,paste("select19_control[",k,"].csv",sep=""),quote=F,row.names=T)



    test20_case<-d5_case
    test20_control<-d5_control
    evaluate20_case<-d4_case
    evaluate20_control<-d4_control
    select20_case<-cbind(d1_case,d2_case,d3_case)
    select20_control<-cbind(d1_control,d2_control,d3_control)

    write.csv(test20_case,paste("test20_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test20_control,paste("test20_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate20_case,paste("evaluate20_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate20_control,paste("evaluate20_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select20_case,paste("select20_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select20_control,paste("select20_control[",k,"].csv",sep=""),quote=F,row.names=T)
  }
}
