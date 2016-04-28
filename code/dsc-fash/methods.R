# Methods: 
sourceDir("methods")

addMethod(dsc_fash,name="ftest",fn=ftest.wrapper,outputtype="pval_output")
addMethod(dsc_fash,name="limma",fn=limma.wrapper,outputtype="pval_output")
addMethod(dsc_fash,name="fash",fn=fash.wrapper,outputtype="fash_output")
addMethod(dsc_fash,name="vash+fash",fn=vash_fash.wrapper,outputtype="fash_output")
