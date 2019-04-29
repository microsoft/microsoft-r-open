--- 
 
# required metadata 
title: "RevoIOQ function (RevoIOQ) | Microsoft Docs" 
description: "Installation and operation qualification of Microsoft R Services" 
keywords: "(RevoIOQ), RevoIOQ, package" 
author: "heidisteen" 
manager: "jhubbard" 
ms.date: "09/13/2017" 
ms.topic: "reference" 
ms.prod: "microsoft-r" 
ms.service: "" 
ms.assetid: "" 
 
# optional metadata 
ROBOTS: "" 
audience: "" 
ms.devlang: "" 
ms.reviewer: "" 
ms.suite: "" 
ms.tgt_pltfrm: "" 
ms.technology: "r-server" 
ms.custom: "" 
 
--- 
 
 
 #RevoIOQ: Microsoft R Services Quality Assurance 
 ##Description
 Installation and operation qualification of Microsoft R Services 
 
 
 ##Usage

```   
  RevoIOQ(printText=TRUE, printHTML=TRUE, outdir=if (file.access(getwd(), mode=2)) file.path(tempdir(),"RevoIOQ") else file.path(getwd(),"RevoIOQ"), 
          basename=paste("Revo-IOQ-Report", format(Sys.time(), "%m-%d-%Y-%H-%M-%S"), sep="-"),
          view=TRUE, clean=TRUE, runTestFileInOwnProcess=TRUE, testLocal = FALSE, testScaleR = TRUE)
 
```
 
 ##Arguments

   
    
 ### `printText`
 logical flag. If `TRUE`, an RUnit test report in ASCII text format is produced. 
  
    
 ### `printHTML`
 logical flag. If `TRUE`, an RUnit test report in HTML format is produced. 
  
    
 ### `outdir`
 character string representing path to output report directory. 
  
    
 ### `basename`
 character string denoting the name of the output file (sans the extension). 
  
    
 ### `view`
 logical flag. If `TRUE` and either `printText=TRUE` or `printHTML=TRUE`, then  the resulting output file is shown, respectively. 
  
    
 ### `clean`
 logical flag. If `TRUE`, then graphics files created during the tests are removed. 
  
    
 ### `runTestFileInOwnProcess`
 logical flag. If `TRUE`, each test file  is run in a separate R process. 
  
    
 ### `testLocal`
 logical flag. If `TRUE`, a set of basic tests is created and run for all locally-installed packages. 
  
    
 ### `testScaleR`
 logical flag. If `TRUE`, the RevoScaleR unit tests are included in the test suite, if available. 
  
 
 
 
 ##Value
 
`TRUE` if all tests are successful, `FALSE` otherwise.
 
 
 ##Examples

 ```
   
  RevoIOQ()
 
```
 
 
 
