Siberia Reconstructions Scripts

These scripts provide a continuous workflow for reconstructions, given in ... All of them should be compiled one by one and ran completely. Scripts return:
	– code outputs (output/performance)
	– transfer functions performance metrics and p-values (by palaeoSig) in environment as 	"summary" table as well as in nice table format (output/summary)
	– scatterplots: fitted&observed, fitted&residuals for each model (plots)
	– correlation matrices between reconstructions (plots)

Code Contributor: Rodion Andreev
		  dorionio40@gmail.com

How to use:
	1. Start a RStudio project and compile scripts one by one
	2. Uncomment a "setwd(..." line in 01_data_loading.R, add your path and compile scripts 	one by one

System requirements: ...
