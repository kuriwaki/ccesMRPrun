** analyze data in Stata because it's easier to do cell means/  rowmeans

cd "~/Dropbox/ccesMRPrun/data-raw"

use cces_GA.dta, clear

egen  age_female = group(age female), label 
 
tab age_female educ if cd == "GA-01"
