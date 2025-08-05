## Logistic regression model ##
## Multiple imputation already performed##
cd " "			// set directory
****************************************************************************
** Define HCU covariates & outcomes **
**************************************
use " ", clear	// load mi dta
****************
** Covariates **
****************
** HCU before - quartiles **
summ cost_before, d

gen byte cost_category = .
replace cost_category = 1 if cost_before <= r(p25)
replace cost_category = 2 if cost_before > r(p25) & cost_before <= r(p50)
replace cost_category = 3 if cost_before > r(p50) & cost_before <= r(p75)
replace cost_category = 4 if cost_before > r(p75)

**************
** Outcomes **
**************
** Year 1 **
summ cost_after1, detail
scalar p75 = r(p75)
gen byte high_cost_after1 = cost_after1 >= p75
** After 2 **
summ cost_after2, detail
scalar p75 = r(p75)
gen byte high_cost_after2 = cost_after2 >= p75

** High cost year 1 **
gen byte high_cost_point=1 if high_cost_after1==1
recode high_cost_point .=0

** Persistent **
gen byte persistent=1 if high_cost_after1==1 & high_cost_after2==1
recode persistent .=0
*************************
tab high_cost_point 		// check distribution
tab persistent
************************
save cost_models_hunt4.dta, replace
*******************************************************************************
**************************
** Prepare for analysis **
**************************
use cost_models_hunt4, clear
* Form folds for 5-fold CV *
gen rand = runiform()  // Generate a random number for sorting
bysort high_cost_point (rand): gen fold = mod(_n, 5) + 1  // Create 5 folds stratified by outcome

* Form global for covariates * 
global covariates = "i.sex age i.maritalstat bmi pulse bpsyst bpdia waistcirc hipcirc i.education i.health i.satlife i.armpain i.neckpain i.backuppain i.lumbarpain i.hiplegpain i.painintens i.specconsult i.hospadmiss i.altcons i.smoking i.alcofreq i.headache actindex i.worktype i.workshift hadsdep hadsanx icpc_morbidity_index i.cost_category"

********************************************************************************
*					          	1) VALIDATION RUN							   *
********************************************************************************
cd " "
save "cost_models_hunt4.dta", replace 
* Convert to long format so predictions for each imputation are saved * 
mi convert flong
* New variable to store held out predictions (linear predictor) * 
gen cv_xb=.
************
* Run CV *
************ 
forval x = 1(1)5 {	                                  						   
	mi estimate, dots errorok saving(micv, replace): logistic high_cost_point $covariates if (fold!=`x')
	mi predict double xb if (fold==`x') using micv, xb storecomplete
	replace cv_xb = xb if (fold==`x')
	drop xb
	display `x'
}

* Fit the model on all folds
mi estimate, dots errorok saving(hunt4_high_cost_point_model, replace): logistic high_cost_point $covariates
parmest, list(,) saving(hunt4_high_cost_point_parmest.dta, replace) label for(estimate min95 max95) 

* Save updated dataset with predictions * 
save "cost_point_hunt4.dta", replace 

********************************************************************************
*	          3) PERFORMANCE ASSESSMENT                   *
********************************************************************************
use  cost_point_hunt4.dta, clear 
********************
** Pooled metrics **
********************
* Calibration slope overall * 
mi estimate, dots: logistic high_cost_point cv_xb		
* Calibration-in-the-large * 
mi estimate, dots: logistic high_cost_point, offset(cv_xb)
* Calibration plot *
gen int_proba = invlogit(cv_xb)	
save cost_point_hunt4.dta, replace		
mi convert wide							// Convert to wide, take avg. proba to fit with RR

pmcalplot int_proba high_cost_point, ci  ///
xtitle("Predicted event probability", size(medsmall) margin(medsmall)) ytitle("Observed probability", size(medsmall) margin(medsmall)) graphregion(color(white)) ///
title ("{bf:Logistic regression (development data)}", color(black) position(11) yoffset(1) size(medsmall)) legend(off) ///
name(high_cost_point_pmcalplot, replace) 

**************
use  cost_point_hunt4.dta, clear // Back to long format data

* Discrimination *
* Define eroctab to use all imputations *
mi xeq 0: roctab high_cost_point cv_xb
return list
cap program drop eroctab
program eroctab, eclass
        version 12.0

        args refvar classvar
        roctab `refvar' `classvar'

        tempname b V
        mat `b' = r(area)
        mat `V' = r(se)^2
        local N = r(N)

        mat colnames `b' = AUC
        mat colnames `V' = AUC
        mat rownames `V' = AUC

        ereturn post `b' `V', obs(`N')
        ereturn local cmd "eroctab"
        ereturn local title "ROC area"
end
mi estimate, cmdok dots: eroctab high_cost_point cv_xb

********************************************************************************
**								HCU POINT TROMSØ7							  **
********************************************************************************
cd "  "

** Define HCU covariates & outcomes **
**************************************
use "M:\p2124-bjbe\hunt_tromsø\analyses\costs\mice\tromsø7_costs_mi20", clear
****************
** Covariates **
****************
** HCU before - quartiles **
summ cost_before, d

gen byte cost_category = .
replace cost_category = 1 if cost_before <= r(p25)
replace cost_category = 2 if cost_before > r(p25) & cost_before <= r(p50)
replace cost_category = 3 if cost_before > r(p50) & cost_before <= r(p75)
replace cost_category = 4 if cost_before > r(p75)

**************
** Outcomes **
**************
** Year 1 **
summ cost_after1, detail
scalar p75 = r(p75)
gen byte high_cost_after1 = cost_after1 >= p75
** After 2 **
summ cost_after2, detail
scalar p75 = r(p75)
gen byte high_cost_after2 = cost_after2 >= p75

** High cost year 1 **
gen byte high_cost_point=1 if high_cost_after1==1
recode high_cost_point .=0

** Persistent **
gen byte persistent=1 if high_cost_after1==1 & high_cost_after2==1
recode persistent .=0
tab persistent
*************************
tab high_cost_point 
tab persistent
************************
save "cost_models_tromsø7.dta", replace
********************************************************************************
**************************
** Prepare for analysis **
**************************
use cost_models_tromsø7, clear
* Form folds for 5-fold CV *
gen rand = runiform()  // Generate a random number for sorting
bysort high_cost_point (rand): gen fold = mod(_n, 5) + 1  // Create 5 folds stratified by outcome
********************************************************************************
*					          	1) VALIDATION RUN							   *
********************************************************************************
cd "  "
save "cost_point_tromsø7.dta", replace 
* Convert to long format so that all predictions (i.e. for each imputation) are saved * 
mi convert flong

************
* Run validation *
************ 
* Predict outcomes using the developed model
mi predict double val_xb using hunt4_high_cost_point_model, xb storecomplete

sum val_xb, d

* Save updated dataset: save predictions, and then can use for the model evaluation* 
save "cost_point_tromsø7.dta", replace 
********************************************************************************
********************************************************************************
*	          3) PERFORMANCE ASSESSMENT                   *
********************************************************************************
use  cost_point_tromsø7.dta, clear 
************************************************
** Pooled metrics (without meta-analysis) *
************************************************
* Calibration slope overall *  
mi estimate, dots: logistic high_cost_point val_xb
* Calibration-in-the-large * 
mi estimate, dots: logistic high_cost_point, offset(val_xb)  
* Calibration plot *
gen val_proba = invlogit(val_xb)	
save cost_point_tromsø7.dta, replace		
mi convert wide							// Convert to wide, take avg. proba to fit with RR

pmcalplot val_proba high_cost_point, ci  ///
xtitle("Predicted event probability", size(medsmall) margin(medsmall)) ytitle("Observed probability", size(medsmall) margin(medsmall)) graphregion(color(white)) ///
title ("{bf:Logistic regression (external validation)}", color(black) position(11) yoffset(1) size(medsmall)) legend(off) ///
name(high_cost_point_val_pmcalplot, replace) 

***************
use  cost_point_tromsø7.dta, clear // Back to long format data

* Discrimination *
* Define eroctab to use all imputations *
mi xeq 0: roctab high_cost_point val_xb
return list
cap program drop eroctab
program eroctab, eclass
        version 12.0

        args refvar classvar
        roctab `refvar' `classvar'

        tempname b V
        mat `b' = r(area)
        mat `V' = r(se)^2
        local N = r(N)

        mat colnames `b' = AUC
        mat colnames `V' = AUC
        mat rownames `V' = AUC

        ereturn post `b' `V', obs(`N')
        ereturn local cmd "eroctab"
        ereturn local title "ROC area"
end
mi estimate, cmdok dots: eroctab high_cost_point val_xb 

********************************************************************************
********************************************************************************

**********************
** Reference models **
**********************
** Development
cd " "
use "cost_point_hunt4.dta", clear

** Survey model **
* Form global for covariates * 
global covariates = "i.sex age i.maritalstat bmi pulse bpsyst bpdia waistcirc hipcirc i.education i.health i.satlife i.armpain i.neckpain i.backuppain i.lumbarpain i.hiplegpain i.painintens i.specconsult i.hospadmiss i.altcons i.smoking i.alcofreq i.headache actindex i.worktype i.workshift hadsdep hadsanx"

* New variable to store held out predictions (linear predictor) * 
gen survey_xb=.
************
* Run CV *
************ 
forval x = 1(1)5 {	                                  						   
	mi estimate, dots errorok saving(survcv, replace): logistic high_cost_point $covariates if (fold!=`x')
	mi predict double xb if (fold==`x') using survcv, xb storecomplete
	replace survey_xb = xb if (fold==`x')
	drop xb
	display `x'
}

* Fit the model on all folds
mi estimate, dots errorok saving(hunt4_survey_high_cost_point_model, replace): logistic high_cost_point $covariates 
parmest, list(,) saving(hunt4_survey_high_cost_point_parmest.dta, replace) label for(estimate min95 max95) 

save cost_point_hunt4.dta, replace
************
* Performance assessment *
************ 
use  cost_point_hunt4.dta, clear 
********************
** Pooled metrics **
********************
* Calibration slope overall * 
* Regress linear predictor * 
mi estimate, dots: logistic high_cost_point survey_xb			
* Calibration-in-the-large * 
mi estimate, dots: logistic high_cost_point, offset(survey_xb)	

gen surv_proba = invlogit(survey_xb)	

* Discrimination *
* Define eroctab to use all imputations *
mi xeq 0: roctab high_cost_point survey_xb
return list
cap program drop eroctab
program eroctab, eclass
        version 12.0

        args refvar classvar
        roctab `refvar' `classvar'

        tempname b V
        mat `b' = r(area)
        mat `V' = r(se)^2
        local N = r(N)

        mat colnames `b' = AUC
        mat colnames `V' = AUC
        mat rownames `V' = AUC

        ereturn post `b' `V', obs(`N')
        ereturn local cmd "eroctab"
        ereturn local title "ROC area"
end
mi estimate, cmdok dots: eroctab high_cost_point survey_xb 

save cost_point_hunt4.dta, replace

****************
** Cost model **		// no missing - so not using mi estimate
****************
use cost_point_hunt4, clear
* Form global for covariates * 
global covariates = "i.sex age icpc_morbidity_index i.cost_category"

* New variable to store held out predictions (linear predictor) * 
gen cost_xb=.
************
* Run CV *
************ 
forval x = 1(1)5 {	                                  						   
	logistic high_cost_point $covariates if (fold!=`x' & _mi_m==1)
	predict double xb if (fold==`x' & _mi_m==1), xb
	replace cost_xb = xb if (fold==`x' & _mi_m==1)
	drop xb
	display `x'
}

logistic high_cost_point $covariates if _mi_m==1
estimates save hunt4_cost_high_cost_point_model, replace
parmest, list() saving(hunt4_cost_high_cost_point_parmest.dta, replace) label for(estimate min95 max95)

save cost_point_hunt4.dta, replace

************
* Performance assessment *
************ 
use  cost_point_hunt4.dta, clear 
********************
** Metrics **
********************
* Calibration slope overall * 
* Regress linear predictor * 
logistic high_cost_point cost_xb if _mi_m==1, coef
* Calibration-in-the-large * 
logistic high_cost_point if _mi_m==1, offset(cost_xb) coef

gen cost_proba = invlogit(cost_xb)	

* Discrimination *
roctab high_cost_point cost_proba

save cost_point_hunt4.dta, replace

********************************************************************************
********************************************************************************

**********************
** Reference models **
**********************
** Validation **
cd "M:\p2124-bjbe\hunt_tromsø\analyses\costs\modelling\tromsø\cost_point"
use "cost_point_tromsø7.dta", clear

** Survey model **
* Form global for covariates * 
global covariates = "sex age i.maritalstat bmi pulse bpsyst bpdia waistcirc hipcirc i.education i.health i.satlife armpain neckpain backuppain lumbarpain hiplegpain i.painintens specconsult hospadmiss altcons i.smoking i.alcofreq headache actindex i.worktype workshift hadsdep hadsanx"

* Predict outcomes using the developed model *
mi predict double survey_val_xb using hunt4_survey_high_cost_point_model, xb storecomplete

sum survey_val_xb, d

* Save updated dataset: save predictions, and then can use for the model evaluation* 
save "cost_point_tromsø7.dta", replace 

************
* Performance assessment *
************ 
use cost_point_tromsø7.dta, clear 
********************
** Pooled metrics **
********************
* Calibration slope overall * 
* Regress linear predictor * 
mi estimate, dots: logistic high_cost_point survey_val_xb
* Calibration-in-the-large * 
mi estimate, dots: logistic high_cost_point, offset(survey_val_xb)

gen surv_val_proba = invlogit(survey_val_xb)	

* Discrimination *
* Define eroctab to use all imputations *
mi xeq 0: roctab high_cost_point survey_val_xb
return list
cap program drop eroctab
program eroctab, eclass
        version 12.0

        args refvar classvar
        roctab `refvar' `classvar'

        tempname b V
        mat `b' = r(area)
        mat `V' = r(se)^2
        local N = r(N)

        mat colnames `b' = AUC
        mat colnames `V' = AUC
        mat rownames `V' = AUC

        ereturn post `b' `V', obs(`N')
        ereturn local cmd "eroctab"
        ereturn local title "ROC area"
end
mi estimate, cmdok dots: eroctab high_cost_point survey_val_xb
save cost_point_tromsø7.dta, replace
****************
** Cost model **
****************
use cost_point_tromsø7.dta, clear 
* Form global for covariates * 
global covariates = "sex age icpc_morbidity_index i.cost_category"

* Predict outcomes using the developed model *
estimates use hunt4_cost_high_cost_point_model
predict double cost_val_xb if _mi_m==1, xb

sum cost_val_xb, d

* Save updated dataset: save predictions, and then can use for the model evaluation* 
save "cost_point_tromsø7.dta", replace 

************
* Performance assessment *
************ 
use cost_point_tromsø7.dta, clear 
********************
** Metrics **
********************
* Calibration slope overall * 
* Regress linear predictor * 
logistic high_cost_point cost_val_xb if _mi_m==1, coef
* Calibration-in-the-large * 
logistic high_cost_point if _mi_m==1, offset(cost_val_xb) coef

gen cost_val_proba = invlogit(cost_val_xb)	
preserve
keep if _mi_m==1
pmcalplot cost_val_proba high_cost_point 
restore

* Discrimination *
roctab high_cost_point cost_val_proba

save "cost_point_tromsø7.dta", replace 

********************************************************************************
******************
** DeLongs test **
******************
** Development **
cd " "
use "cost_point_hunt4.dta", clear

roccomp high_cost_point int_proba cost_proba if _mi_m==1

** Validation **
cd "M:\p2124-bjbe\hunt_tromsø\analyses\costs\modelling\tromsø\cost_point"
use "cost_point_tromsø7.dta", clear

roccomp high_cost_point val_proba cost_val_proba if _mi_m==1

********************************************************************************
******************************* END ********************************************
********************************************************************************