/*This program is for loading the new PTC files into Stata, create the analytic data set, recode the ftpt code vars, 
remove unneccessary variables, and otherwise ready the data set for transport into R. */

*Load the data sets

local varlist "oira_student_id term_enrolled_date crdsem01 gpasem01 ftptcode_sem* grad_sem* ethnicity_imputed_code female enrolled_sem* ba* aa* entry_date cpi_units_total caa_total cas_sat_total_recntrd dependent pell_flag anyba anyaa pell_awd_fy01 nodelay entry_age need_any_remediation best_math best_writ best_read transferred_out last_college_code ever_transferred college_id_semR* degree* cas_regents*"

use `varlist' using "O:\!Policy\Projects\Policy Tracking Cohort\Public Use\Wallace Dissertation\20131125_Wallace_f99-s00.dta",clear
append using  "O:\!Policy\Projects\Policy Tracking Cohort\Public Use\Wallace Dissertation\20131125_Wallace_f00-s02.dta", keep(`varlist') force
append using "O:\!Policy\Projects\Policy Tracking Cohort\Public Use\Wallace Dissertation\20131125_Wallace_f02-s04.dta" , keep(`varlist') force

drop if inlist(term_enrolled_date,"9/1/2003","2/1/2004","2/1/2003")
count //127144

drop if degree_pursued_level_code==1
count //126172

gen spring=0
replace spring=1 if  entry_date=="2/1/2000":entry
replace spring=1 if  entry_date=="2/1/2001":entry
replace spring=1 if  entry_date=="2/1/2002":entry

tab entry_date spring,m

gen pell=0
replace pell=1 if pell_awd_fy01>0 & pell_awd_fy01!=.



*Create the recoded ftptcode variables
foreach i of numlist 1/9{
rename ftptcode_sem0`i' ftptcode_sem`i'
rename grad_sem0`i' grad_sem`i'
rename enrolled_sem0`i'  enrolled_sem`i' 
 
}

*Add the semester degree data
merge 1:1 oira_student_id using "O:\!Policy\Projects\Policy Tracking Cohort\Special Extracts\Degree By SEM\20131204_Degree_by_SEM.dta", gen(mdeg)
drop if mdeg==2 //those in later cohorts
count //126172

*create a dummy degree_earned_level_code_sem1
gen degree_earned_level_code_sem1=.

*there are values for the sem18 and sem20 variable above 3
replace degree_earned_level_code_sem18=. if degree_earned_level_code_sem18>3
replace degree_earned_level_code_sem20=. if degree_earned_level_code_sem20>3 
/*
foreach i of numlist 2/9 {
rename ftptcode_sem0`i' ftptcode_sem`i'
}
*/
foreach i of numlist 1/20 {
gen r_ftptcode_sem`i'=3 //s
replace r_ftptcode_sem`i'=1 if ftptcode_sem`i'==1 & grad_sem`i'==0 //f
replace r_ftptcode_sem`i'=2 if ftptcode_sem`i'==2 & grad_sem`i'==0 //p
replace r_ftptcode_sem`i'=5 if degree_earned_level_code_sem`i'==1 //Cert
replace r_ftptcode_sem`i'=6 if degree_earned_level_code_sem`i'==2 //AA
replace r_ftptcode_sem`i'=7 if degree_earned_level_code_sem`i'==3 //BA

}

foreach i of numlist 2/20 {
replace r_ftptcode_sem`i'=4 if college_id_semR`i'>30 & college_id_semR`i'!=. //t
}

foreach i of numlist 20/3 {
local j = `i' - 1
	while `j' >=2 {
		replace r_ftptcode_sem`i' = 8 if  r_ftptcode_sem`i'== 3 & inlist(r_ftptcode_sem`j',5, 6,7) //post-grad
		replace r_ftptcode_sem`i' = 4 if  r_ftptcode_sem`i'== 3 & r_ftptcode_sem`j'==4 & transferred_out==1 //transfer
		local j = `j' - 1
		}
}



egen pattern= concat(r_ftptcode_sem1 r_ftptcode_sem2 r_ftptcode_sem3 r_ftptcode_sem4 r_ftptcode_sem5 r_ftptcode_sem6 r_ftptcode_sem7 r_ftptcode_sem8 r_ftptcode_sem9 r_ftptcode_sem10 r_ftptcode_sem11 r_ftptcode_sem12 r_ftptcode_sem13 r_ftptcode_sem14 r_ftptcode_sem15 r_ftptcode_sem16 r_ftptcode_sem17 r_ftptcode_sem18 r_ftptcode_sem19 r_ftptcode_sem20)

edit r_ftpt* college_id_semR* ftpt* enrolled* pattern

/*What I need to do is to separate those who started at the AA level and stayed there from those who transferred up. 
*It might also be nice to include the kind of degree earned into the pattern. I can approximate this with grad_sem* and 
degree pursued, but ideally, I'd have degree_earned_level_code by semester. This means a special extract. Use the crd_sem*
sql syntax applied to DEGREE_FACTS joined to OIRA_ID_NUMBER_DIM and we should have it. 
*/

tab degree_pursued_level_code,m

gen last_deg_pursued= degree_pursued_level_code

foreach i of numlist 2/9{
rename degree_pursued_lvl_code_sem0`i' degree_pursued_lvl_code_sem`i'
}

foreach i of numlist 2/20 {
replace last_deg_pursued = degree_pursued_lvl_code_sem`i' if degree_pursued_lvl_code_sem`i'!=.
}

tab degree_pursued_level_code last_deg_pursued,m
/*This is an excellent first step, but it only tells me the last degree they pursued at CUNY. For the transfer students I 
need to know the level of the school they ended up at There should be a way to do this but it requires the original NSC data.*/

*next I need to find the variables needed to construct a last transfer level
/*
use "O:\!Policy\Projects\Policy Tracking Cohort\NSLC Match\20131120_NSC_Wide.dta", clear
keep oira* year4year*

*Create the target variable
gen trans_last_college_level=.

*Remove the "L" value and destring the underlying variables
foreach i of numlist 1/101{
replace year4year`i'="" if year4year`i'=="L"
destring year4year`i',replace
}
foreach i of numlist 1/101{
replace trans_last_college_level=year4year`i' if year4year`i'!=.
}


tab trans_last_college_level,m
*I think this looks good

keep oira* trans_last_college_level

save "C:\Users\awallace\Desktop\Andrew's Files\Dissertation\20151119_Transfer_Level.dta"

*/

merge 1:1 oira_student_id using "C:\Users\awallace\Desktop\Andrew's Files\Dissertation\20151119_Transfer_Level.dta", gen(m_trns_lvl)
drop if m_trns_lvl==2
count //126172

keep oira_student_id degree_pursued_level_code pattern trans_last_college_level last_deg_pursued term_enrolled_date crdsem01 ethnicity_imputed_code gpasem01 aa* ba* cpi_units_total caa_total cas_sat_total_recntrd cas_regents* pell entry_date dependent female nodelay entry_age best* need_any_remediation ever_transferred transferred_out last_college_code spring r_ftptcode_sem*

*To bring it in line with the PTC variables
recode trans_last_college_level (4=3)

tab degree_pursued_level_code trans_last_college_level

bys degree_pursued_level_code: tab last_deg_pursued trans_last_college_level if transferred_out==1

mdesc trans_last_college_level if transferred_out==1

gen last_deg_pursued_R = last_deg_pursued
replace last_deg_pursued_R = 2 if trans_last_college_level==2 & transferred_out==1
replace last_deg_pursued_R = 3 if trans_last_college_level==3 & transferred_out==1
tab last_deg_pursued_R last_deg_pursued,m

tab degree_pursued_level_code
tab degree_pursued_level_code last_deg_pursued_R,m


gen group=.
replace group=1 if degree_pursued_level_code==2 & last_deg_pursued_R==2 //terminal AA students
replace group=2 if degree_pursued_level_code==2 & last_deg_pursued_R==3 //Upward AA Students 
replace group=3 if degree_pursued_level_code==3 & last_deg_pursued_R==3 //Terminal BA Students
replace group=4 if degree_pursued_level_code==3 & last_deg_pursued_R==2 //Downward BA Students
replace group=5 if degree_pursued_level_code==3 & last_deg_pursued_R==1 //BA to Cert Students
replace group=5 if degree_pursued_level_code==2 & last_deg_pursued_R==1 //AA to Cert Students


tab group,m
*That said, it would appear that only 415 students are still missing
edit if group==. & last_deg_pursued==2
//The above are 21 transfer students within the 415 who stayed at the associate level. The rest were downward transfers.These were missing on last_c

order r_ftptcode_sem1 r_ftptcode_sem2 r_ftptcode_sem3 r_ftptcode_sem4 r_ftptcode_sem5 r_ftptcode_sem6 r_ftptcode_sem7 r_ftptcode_sem8 r_ftptcode_sem9 r_ftptcode_sem10 r_ftptcode_sem11 r_ftptcode_sem12 r_ftptcode_sem13 r_ftptcode_sem14 r_ftptcode_sem15 r_ftptcode_sem16 r_ftptcode_sem17 r_ftptcode_sem18 r_ftptcode_sem19 r_ftptcode_sem20, first

*save as single data set

drop if degree_pursued_level_code == 1
drop if last_deg_pursued_R == 1
drop if ethnicity_imputed_code == 6

saveold "C:\Users\awallace\Desktop\Andrew's Files\Dissertation\20151120_PTC_Stata12.dta"











