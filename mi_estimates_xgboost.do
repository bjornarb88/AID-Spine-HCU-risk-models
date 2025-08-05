#MI estimate trick ML probability output
************************* Internal validation **********************************
*********************
** Point high-cost **
*********************
** Set directory **
cd " "

forval i = 1/20 {
    import delimited "pred_values_mi`i'.csv", clear	
	rename (predicted) (proba)
	gen patid= _n	
	gen m=`i'
	save "pred_values\pred_values_mi`i'.dta", replace
}

use "pred_values\pred_values_mi1.dta", clear
forval i = 2/20 {
	append using  "pred_values\pred_values_mi`i'.dta"
	save cost_point_cv_miset, replace
}

**
* Keep first imputation - complete case, m=0 
keep if m==1 
replace m=0 

* Generate mi style missing indicator - to later use Stata's mi machinery *
gen i_predictions = 1 
tab m 
replace proba = . if i_predictions==1 

append using cost_point_cv_miset
save cost_point_cv_miset_ready, replace
* Format as multiply imputed dataset * 
use cost_point_cv_miset_ready, clear
mi import flong, m(m) id(patid) imputed(proba)          // Importing imputations into this dataset 
save cost_point_cv_miset_ready, replace

gen cv_xb=log(proba / (1 - proba))
save cost_point_cv_miset_ready, replace

**************************
* PERFORMANCE ASSESSMENT *
**************************
use cost_point_cv_miset_ready, clear
sum cv_xb
*******************
** Pooled metrics *
*******************
* Calibration slope overall * 
mi estimate, dots: logistic true cv_xb
* Calibration-in-the-large * 
mi estimate, dots: logistic true, offset(cv_xb)

* Calibration plot *
save cost_point_cv_miset_ready.dta, replace		// Convert to wide, take avg. proba to fit with RR
mi convert wide
* Calculated 'predicted' probs - use linear predictors
pmcalplot _1_proba true, ci  ///
xtitle("Predicted event probability", size(medsmall) margin(medsmall)) ytitle("Observed probability", size(medsmall) margin(medsmall)) graphregion(color(white)) ///
title ("{bf:XGBoost (development data)}", color(black) position(11) yoffset(1) size(medsmall)) legend(off) ///
name(dev_xgb_pmcalplot, replace) 


* Discrimination *
use cost_point_cv_miset_ready, clear
* Define eroctab to use all imputations *
mi xeq 1: roctab true cv_xb
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
mi estimate, cmdok dots: eroctab true cv_xb
****
save cost_point_cv_miset_ready, replace
**************************************************

************************* External validation **********************************
*********************
** Point high-cost **
*********************
** Set directory **
cd "  "

forval i = 1/20 {
    import delimited "pred_values_mi`i'.csv", clear	
	gen patid= _n	
	gen m=`i'
	save "pred_values\pred_values_mi`i'.dta", replace
}

use "pred_values\pred_values_mi1.dta", clear
forval i = 2/20 {
	append using  "pred_values\pred_values_mi`i'.dta"
	save cost_point_val_miset, replace
}

**
* Keep first imputation - complete case, m=0 
keep if m==1 
replace m=0 

* Generate mi style missing indicator - to later use Stata's mi machinery *
gen i_predictions = 1 
tab m 
replace proba = . if i_predictions==1 

append using cost_point_val_miset
save cost_point_val_miset_ready, replace

* Format as multiply imputed dataset *
use cost_point_val_miset_ready, clear
mi import flong, m(m) id(patid) imputed(proba)          // Importing imputations into this dataset 
save cost_point_val_miset_ready, replace

sum proba
mdesc proba
gen val_xb=log(proba / (1 - proba))
save cost_point_val_miset_ready, replace

**************************
* PERFORMANCE ASSESSMENT *
**************************
use cost_point_val_miset_ready, clear
sum val_xb
*******************
** Pooled metrics *
*******************
* Calibration slope overall * 
* Regress linear predictor * 
mi estimate, dots: logistic true val_xb
* Calibration-in-the-large * 
mi estimate, dots: logistic true, offset(val_xb)

* Calibration plot *
save cost_point_val_miset_ready.dta, replace		// Convert to wide, take avg. proba to fit with RR
mi convert wide

pmcalplot _1_proba true, ci  ///
xtitle("Predicted event probability", size(medsmall) margin(medsmall)) ytitle("Observed probability", size(medsmall) margin(medsmall)) graphregion(color(white)) ///
title ("{bf:XGBoost (validation data)}", color(black) position(11) yoffset(1) size(medsmall)) legend(off) ///
name(val_xgb_pmcalplot, replace) 

* Discrimination *
use cost_point_val_miset_ready, clear
* Define eroctab to use all imputations *
mi xeq 1: roctab true val_xb
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
mi estimate, cmdok dots: eroctab true val_xb
****
save cost_point_val_miset_ready, replace
*********************

****************************** Reference models ********************************
********************************************************************************

**************************** Internal validation *******************************
**********************
** Point high-cost **
*********************
** Survey model
** Set directory **
cd "  "

forval i = 1/20 {
    import delimited "pred_values_surv_mi`i'.csv", clear	
	rename (predicted) (proba)
	gen patid= _n	
	gen m=`i'
	save "pred_values\pred_values_surv_mi`i'.dta", replace
}

use "pred_values\pred_values_surv_mi1.dta", clear
forval i = 2/20 {
	append using  "pred_values\pred_values_surv_mi`i'.dta"
	save cost_point_surv_cv_miset, replace
}

**
* Keep first imputation - complete case, m=0 
keep if m==1 
replace m=0 

* Generate mi style missing indicator - to later use Stata's mi machinery *gen i_predictions = 1 
tab m 
replace proba = . if i_predictions==1 

append using cost_point_surv_cv_miset
save cost_point_surv_cv_miset_ready, replace

* Format as multiply imputed dataset * 
use cost_point_surv_cv_miset_ready, clear
mi import flong, m(m) id(patid) imputed(proba)          // Importing imputations into this dataset 
save cost_point_surv_cv_miset_ready, replace

sum proba
mdesc proba
gen cv_xb=log(proba / (1 - proba))
save cost_point_surv_cv_miset_ready, replace

**************************
* PERFORMANCE ASSESSMENT *
**************************
use cost_point_surv_cv_miset_ready, clear
sum cv_xb
*******************
** Pooled metrics *
*******************
* Calibration slope overall * 
mi estimate, dots: logistic true cv_xb
* Calibration-in-the-large * 
mi estimate, dots: logistic true, offset(cv_xb)

* Discrimination *
use cost_point_surv_cv_miset_ready, clear
* Define eroctab to use all imputations *
mi xeq 1: roctab true cv_xb
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
mi estimate, cmdok dots: eroctab true cv_xb
****
save cost_point_surv_cv_miset_ready, replace
***************************

** Registry-only model 		// no missing - so not using mi estimate
** Set directory **
cd "  "

**************************
* PERFORMANCE ASSESSMENT *
**************************
use "pred_values\pred_values_cost_mi1.dta", clear

********************
** Metrics **
********************
* Calibration slope overall * 
logistic true cv_xb, coef
* Calibration-in-the-large * 
logistic true, offset(cv_xb) coef

gen cost_proba = invlogit(cv_xb)	
* Discrimination *
roctab true cost_proba

**************************** External validation *******************************
**********************
** Point high-cost **
*********************
** Survey model
** Set directory **
cd "  "

forval i = 1/20 {
    import delimited "pred_values_surv_mi`i'.csv", clear	
	gen patid= _n	
	gen m=`i'
	save "pred_values\pred_values_surv_mi`i'.dta", replace
}

use "pred_values\pred_values_surv_mi1.dta", clear
forval i = 2/20 {
	append using  "pred_values\pred_values_surv_mi`i'.dta"
	save cost_point_surv_val_miset, replace
}

**
* Keep first imputation - complete case, m=0 
keep if m==1 
replace m=0 

* Generate mi style missing indicator - to later use Stata's mi machinery *
gen i_predictions = 1 
tab m 
replace proba = . if i_predictions==1 

append using cost_point_surv_val_miset

save cost_point_surv_val_miset_ready, replace
* Format as multiply imputed dataset *
use cost_point_surv_val_miset_ready, clear
mi import flong, m(m) id(patid) imputed(proba)          // Importing imputations into this dataset 
save cost_point_surv_val_miset_ready, replace

sum proba
mdesc proba
gen val_xb=log(proba / (1 - proba))
save cost_point_surv_val_miset_ready, replace

**************************
* PERFORMANCE ASSESSMENT *
**************************
use cost_point_surv_val_miset_ready, clear
sum val_xb
******************
* Pooled metrics *
******************
* Calibration slope overall *  
mi estimate, dots: logistic true val_xb
* Calibration-in-the-large * 
mi estimate, dots: logistic true, offset(val_xb)

* Discrimination *
use cost_point_surv_val_miset_ready, clear
* Define eroctab to use all imputations *
mi xeq 1: roctab true val_xb
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
mi estimate, cmdok dots: eroctab true val_xb
****
save cost_point_surv_val_miset_ready, replace

************************
** Registry-only model 		// no missing - so not using mi estimate
** Set directory **
cd "  "

**************************
* PERFORMANCE ASSESSMENT *
**************************
use "pred_values\pred_values_cost_mi1.dta", clear

***********
* Metrics *
***********
* Calibration slope overall * 
logistic true val_xb, coef
* Calibration-in-the-large * 
logistic true, offset(val_xb) coef

gen cost_val_proba = invlogit(val_xb)	
* Discrimination *
roctab true cost_val_proba


********************************************************************************
********************************************************************************
******************
** DeLongs test **
******************
** Development **
cd "  "
use "cost_point_cv_miset_ready.dta", clear
sort id (_mi_m)
rename proba int_proba
merge 1:1 id _mi_m using cost_point_cost_cv_miset_ready.dta
rename proba cost_proba

roccomp true int_proba cost_proba if _mi_m==1

** Validation **
cd "M:\p2124-bjbe\hunt_tromsø\analyses\costs\modelling\tromsø\cost_point\xgboost\estimates\val"
use "cost_point_val_miset_ready.dta", clear
sort id (_mi_m)
rename proba val_proba
merge 1:1 id _mi_m using cost_point_cost_val_miset_ready.dta
rename proba cost_proba

roccomp true val_proba cost_proba if _mi_m==1

********************************************************************************
******************************* END ********************************************
********************************************************************************