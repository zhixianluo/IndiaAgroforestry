clear all
set more off


use "Data_Commercial tree growing_HH_Vill_GIS.dta"


* 1 Generate variables needed for analysis

* 1.1 Classify trees by type: fruit, timber, bamboo

gen sp_hort=1 if mango==1 | lemon==1 | guava==1 | aawla==1 | mahua==1 | orange==1
* "aawla" is the spelling the survey firm used for "amla"
replace sp_hort=0 if sp_hort==.

gen sp_timb=1 if teak_sagon==1 | eucalyptus==1 | sheesham==1 | saal==1
replace sp_timb=0 if sp_timb==.

gen sp_bmbo=1 if bamboo==1
replace sp_bmbo=0 if sp_bmbo==.

* 1.2 Define tree-growing strategies
   
gen sp_choice=4 if sp_bmbo==1 & sp_timb==1 & sp_hort==1
replace sp_choice=4 if sp_timb==1 & sp_hort==1 & sp_choice==.
replace sp_choice=4 if sp_bmbo==1 & sp_hort==1 & sp_choice==.
replace sp_choice=3 if sp_choice==. & sp_bmbo==1 & sp_timb==1
replace sp_choice=3 if sp_choice==. & sp_timb==1
replace sp_choice=3 if sp_choice==. & sp_bmbo==1
replace sp_choice=2 if sp_choice==. & sp_hort==1
replace sp_choice=1 if sp_choice==.
tab sp_choice 

gen tree_grow_dummy=1 if sp_choice>1 & sp_choice!=. 
replace tree_grow_dummy=0 if tree_grow_dummy==.   

* 1.3 Number of trees: total and per acre

foreach i of numlist 1(1)10{
replace maxtree_oneplot_`i'=0 if maxtree_oneplot_`i'==.
}

egen tot_tree =rowtotal(maxtree_oneplot_1 maxtree_oneplot_2 maxtree_oneplot_3 maxtree_oneplot_4 maxtree_oneplot_5 maxtree_oneplot_6 maxtree_oneplot_7 maxtree_oneplot_8 maxtree_oneplot_9 maxtree_oneplot_10)

gen per_acre_tree= tot_tree/tot_land_operate_acre

* 1.4 Shares of trees by type: fruit, timber, bamboo

* 1.4.1 Fruit trees

foreach i of numlist 1(1)10{
gen hortsp_`i'= treenum_id_`i'  
}
destring hortsp_1 hortsp_2 hortsp_3 hortsp_4 hortsp_5 hortsp_6 hortsp_7 hortsp_8 hortsp_9 hortsp_10, replace

foreach i of numlist 1(1)10{
replace hortsp_`i'= 0 if hortsp_`i'==11 
replace hortsp_`i'= 1 if hortsp_`i'==1
replace hortsp_`i'= 1 if hortsp_`i'==2
replace hortsp_`i'= 1 if hortsp_`i'==3
replace hortsp_`i'= 1 if hortsp_`i'==4
replace hortsp_`i'= 1 if hortsp_`i'==5
replace hortsp_`i'= 1 if hortsp_`i'==6
replace hortsp_`i'= 0 if hortsp_`i'==97
replace hortsp_`i'= 0 if hortsp_`i'==7
replace hortsp_`i'= 0 if hortsp_`i'==8
replace hortsp_`i'= 0 if hortsp_`i'==9
replace hortsp_`i'= 0 if hortsp_`i'==10    
}

foreach i of numlist 1(1)10{
gen horttree_`i'= maxtree_oneplot_`i' if hortsp_`i'==1   
}

foreach i of numlist 1(1)10{
replace horttree_`i'= 0 if horttree_`i'==.   
}
egen tot_horttree =rowtotal (horttree_1 horttree_2 horttree_3 horttree_4 horttree_5 horttree_6 horttree_7 horttree_8 horttree_9 horttree_10)

gen ratio_hort_tot_tree= tot_horttree/tot_tree 

* Mango

foreach i of numlist 1(1)10{
gen MANGO_`i'= treenum_id_`i'  
}
destring MANGO_1 MANGO_2 MANGO_3 MANGO_4 MANGO_5 MANGO_6 MANGO_7 MANGO_8 MANGO_9 MANGO_10, replace

foreach i of numlist 1(1)10{
replace MANGO_`i'= 0 if MANGO_`i'==11 
replace MANGO_`i'= 1 if MANGO_`i'==1
replace MANGO_`i'= 0 if MANGO_`i'==2
replace MANGO_`i'= 0 if MANGO_`i'==3
replace MANGO_`i'= 0 if MANGO_`i'==4
replace MANGO_`i'= 0 if MANGO_`i'==5
replace MANGO_`i'= 0 if MANGO_`i'==6
replace MANGO_`i'= 0 if MANGO_`i'==97
replace MANGO_`i'= 0 if MANGO_`i'==7
replace MANGO_`i'= 0 if MANGO_`i'==8
replace MANGO_`i'= 0 if MANGO_`i'==9
replace MANGO_`i'= 0 if MANGO_`i'==10    
}

foreach i of numlist 1(1)10{
gen MANGOTREE_`i'= maxtree_oneplot_`i' if MANGO_`i'==1   
}

foreach i of numlist 1(1)10{
replace horttree_`i'= 0 if horttree_`i'==.   
}
egen NOOF_MANGOTREE =rowtotal (MANGOTREE_1 MANGOTREE_2 MANGOTREE_3 MANGOTREE_4 MANGOTREE_5 MANGOTREE_6 MANGOTREE_7 MANGOTREE_8 MANGOTREE_9 MANGOTREE_10)

total NOOF_MANGOTREE
total tot_tree

* 1.4.2 Timber trees

foreach i of numlist 1(1)10{
gen timbsp_`i'= treenum_id_`i'  
}
destring timbsp_1 timbsp_2 timbsp_3 timbsp_4 timbsp_5 timbsp_6 timbsp_7 timbsp_8 timbsp_9 timbsp_10, replace

foreach i of numlist 1(1)10{
replace timbsp_`i'= 0 if timbsp_`i'==11 
replace timbsp_`i'= 0 if timbsp_`i'==1
replace timbsp_`i'= 0 if timbsp_`i'==2
replace timbsp_`i'= 0 if timbsp_`i'==3
replace timbsp_`i'= 0 if timbsp_`i'==4
replace timbsp_`i'= 0 if timbsp_`i'==5
replace timbsp_`i'= 0 if timbsp_`i'==6
replace timbsp_`i'= 0 if timbsp_`i'==97
replace timbsp_`i'= 1 if timbsp_`i'==7
replace timbsp_`i'= 1 if timbsp_`i'==8
replace timbsp_`i'= 1 if timbsp_`i'==9
replace timbsp_`i'= 1 if timbsp_`i'==10    
}

foreach i of numlist 1(1)10{
gen timbtree_`i'= maxtree_oneplot_`i' if timbsp_`i'==1   
}

foreach i of numlist 1(1)10{
replace timbtree_`i'= 0 if timbtree_`i'==.   
}
egen tot_timbtree =rowtotal (timbtree_1 timbtree_2 timbtree_3 timbtree_4 ///
timbtree_5 timbtree_6 timbtree_7 timbtree_8 timbtree_9 timbtree_10)

gen ratio_timb_tot_tree= tot_timbtree/tot_tree 

* Teak

foreach i of numlist 1(1)10{
gen TEAK_`i'= treenum_id_`i'  
}
destring TEAK_1 TEAK_2 TEAK_3 TEAK_4 TEAK_5 TEAK_6 TEAK_7 TEAK_8 TEAK_9 TEAK_10, replace

foreach i of numlist 1(1)10{
replace TEAK_`i'= 0 if TEAK_`i'==11 
replace TEAK_`i'= 0 if TEAK_`i'==1
replace TEAK_`i'= 0 if TEAK_`i'==2
replace TEAK_`i'= 0 if TEAK_`i'==3
replace TEAK_`i'= 0 if TEAK_`i'==4
replace TEAK_`i'= 0 if TEAK_`i'==5
replace TEAK_`i'= 0 if TEAK_`i'==6
replace TEAK_`i'= 0 if TEAK_`i'==97
replace TEAK_`i'= 1 if TEAK_`i'==7
replace TEAK_`i'= 0 if TEAK_`i'==8
replace TEAK_`i'= 0 if TEAK_`i'==9
replace TEAK_`i'= 0 if TEAK_`i'==10    
}

foreach i of numlist 1(1)10{
gen TEAKTREE_`i'= maxtree_oneplot_`i' if TEAK_`i'==1   
}

foreach i of numlist 1(1)10{
replace TEAKTREE_`i'= 0 if TEAKTREE_`i'==.   
}
egen NOOF_TEAKTREE =rowtotal (TEAKTREE_1 TEAKTREE_2 TEAKTREE_3 TEAKTREE_4 ///
TEAKTREE_5 TEAKTREE_6 TEAKTREE_7 TEAKTREE_8 TEAKTREE_9 TEAKTREE_10)

total NOOF_TEAKTREE
total tot_tree

* 1.4.3 Bamboo plants

foreach i of numlist 1(1)10{
gen bmbosp_`i'= treenum_id_`i'  
}
destring bmbosp_1 bmbosp_2 bmbosp_3 bmbosp_4 bmbosp_5 bmbosp_6 bmbosp_7 bmbosp_8 bmbosp_9 bmbosp_10, replace

foreach i of numlist 1(1)10 {
replace bmbosp_`i'= 0 if bmbosp_`i'==1 
replace bmbosp_`i'= 0 if bmbosp_`i'==2
replace bmbosp_`i'= 0 if bmbosp_`i'==3
replace bmbosp_`i'= 0 if bmbosp_`i'==4
replace bmbosp_`i'= 0 if bmbosp_`i'==5
replace bmbosp_`i'= 0 if bmbosp_`i'==6
replace bmbosp_`i'= 0 if bmbosp_`i'==7
replace bmbosp_`i'= 0 if bmbosp_`i'==8
replace bmbosp_`i'= 0 if bmbosp_`i'==9
replace bmbosp_`i'= 0 if bmbosp_`i'==10
replace bmbosp_`i'= 0 if bmbosp_`i'==97
replace bmbosp_`i'= 1 if bmbosp_`i'==11
}

foreach i of numlist 1(1)10{
gen bmbo_`i'= maxtree_oneplot_`i' if bmbosp_`i'==1   
}

foreach i of numlist 1(1)10{
replace bmbo_`i'= 0 if bmbo_`i'==.   
}

egen tot_bmbotree =rowtotal (bmbo_1 bmbo_2 bmbo_3 bmbo_4 ///
bmbo_5 bmbo_6 bmbo_7 bmbo_8 bmbo_9 bmbo_10)

gen ratio_bmbo_tot_tree= tot_bmbotree/tot_tree

total tot_bmbotree 

* 1.4.4 Check that all trees are identified

gen check = ratio_hort_tot_tree + ratio_timb_tot_tree + ratio_bmbo_tot_tree
tab check, missing  // 1 obs = 0.88; rest = 1 or .
drop if check!=1 & check!=.
drop check

* 1.5 Locational dummies

* 1.5.1 District

tab district

tab district, generate (dist_dummy)
rename dist_dummy1 dist_Harda 
rename dist_dummy2 dist_Hoshangabad 
rename dist_dummy3 dist_Jabalpur 
rename dist_dummy4 dist_Sehore
rename dist_dummy5 dist_Shajapur

gen check = dist_Harda + dist_Hoshangabad + dist_Jabalpur + dist_Sehore + dist_Shajapur
tab check, missing  // All = 1
drop check

* 1.5.2 Ecoregion

tab ECO_NAME

gen agroeco_dummy_1=1 if ECO_NAME=="East Deccan moist deciduous forests"
replace agroeco_dummy_1=0 if agroeco_dummy_1==.

gen agroeco_dummy_2=1 if ECO_NAME=="Khathiar-Gir dry deciduous forests"
replace agroeco_dummy_2=0 if agroeco_dummy_2==.

gen agroeco_dummy_3=1 if ECO_NAME=="Narmada Valley dry deciduous forests"
replace agroeco_dummy_3=0 if agroeco_dummy_3==.

gen check = agroeco_dummy_1 + agroeco_dummy_2 + agroeco_dummy_3
tab check, missing  // All = 1
drop check

by ECO_NAME, sort : su tree_grow_dummy tot_tree per_acre_tree ratio_hort_tot_tree ratio_timb_tot_tree ratio_bmbo_tot_tree

* 1.6 Distance variables

tab puccaroad_invillage
replace puccaroad_distance = 0 if puccaroad_distance ==.

* 1.6.1 Markets

tab inputmarket_invillage
replace inputmarket_distance = 0 if inputmarket_distance==.

tab  cermarket_invillage
replace cermarket_distance = 0 if cermarket_distance==.

tab fruitmarket_invillage
replace fruitmarket_distance = 0 if fruitmarket_distance==.

tab pulsmarket_invillage
replace pulsmarket_distance = 0 if pulsmarket_distance==.

tab vegmarket_invillage
replace vegmarket_distance = 0 if vegmarket_distance==.

* Ignore wood market because many entries are -99 (don't know)

* 1.6.2 Government facilities

* Nurseries (including private)

su Distancetotheclosestgovernme Distancetotheclosestprivat
gen closest_nursery = min(Distancetotheclosestgovernme, Distancetotheclosestprivate) 

* Agriculture science centers (KVKs)

su kvk_distance

* Forestry range offices

su dist_rangeoffice

* Forest depots

su Distancetotheclosestforestry

* 1.7 Household and farm characteristics

egen village_altitude_mean = mean(gps_resp_housealtitud), by(district block village) 
replace gps_resp_housealtitude = village_altitude_mean if missing(gps_resp_housealtitude)
drop village_altitude_mean

tab hh_caste, generate(new_hh_caste)
rename new_hh_caste1 caste_gen
rename new_hh_caste2 caste_obc
rename new_hh_caste3 caste_sc
rename new_hh_caste4 caste_ST

gen check = caste_gen + caste_sc + caste_ST + caste_obc
tab check, missing  // All = 1
drop check

gen credit_aces= 1 if loan_able_cooperative==1 | loan_able_mfi==1 | loan_able_shg==1 | loan_able_flender==1 | loan_able_ngo==1
replace credit_aces=0 if credit_aces==. 

gen tot_land_operate_acre_2 = own_land_acre + coowned_land_acre

tab lplot_irrigation_source, missing  // 20 obs missing, which are unirrigated plots
drop if lplot_irrigation_source==.
gen dummy_irrigation_GW = 0 if lplot_irrigation_source!=.
replace dummy_irrigation_GW = 1 if lplot_irrigation_source==3 | lplot_irrigation_source==4


* 2 Data for Figure 1, Tree growing outcomes by landholding size: raw data

gen landhold=0 if tot_land_operate_acre<4.94
replace landhold=1 if landhold==.

by landhold, sort : tab tree_grow_dummy

by landhold, sort : su tot_tree per_acre_tree ratio_timb_tot_tree ratio_bmbo_tot_tree ratio_hort_tot_tree 

ttest tot_tree, by(landhold)
ttest per_acre_tree, by(landhold)
ttest tree_grow_dummy, by(landhold)
ttest ratio_hort_tot_tree, by(landhold)
ttest ratio_timb_tot_tree, by(landhold)
ttest ratio_bmbo_tot_tree, by(landhold)


* 3 Data for Figure 2, Tree growing outcomes by exposure to agroforestry programs: raw data

* 3.1 Generate two-level treatment variable
   
tab heard_govtsup_agroforest agroforest_parti_prog, missing

tab heard_govtsup_agroforest, gen(heard_SMAFF)
tab agroforest_parti_prog, gen(partcp_SMAFF)

by heard_SMAFF2, sort : tab tree_grow_dummy
by partcp_SMAFF2, sort : tab tree_grow_dummy

by heard_SMAFF2, sort : su tot_tree per_acre_tree ratio_timb_tot_tree ratio_bmbo_tot_tree
by partcp_SMAFF2, sort : su tot_tree per_acre_tree ratio_timb_tot_tree ratio_bmbo_tot_tree

gen ag_supp=2 if partcp_SMAFF2==1
replace ag_supp=1 if partcp_SMAFF2==0
replace ag_supp=0 if ag_supp==.

* 3.2 Test differences in means

ttest tot_tree if ag_supp<2, by(ag_supp)
ttest per_acre_tree if ag_supp<2, by(ag_supp)
ttest tree_grow_dummy if ag_supp<2, by(ag_supp)
ttest ratio_hort_tot_tree if ag_supp<2, by(ag_supp)
ttest ratio_timb_tot_tree if ag_supp<2, by(ag_supp)
ttest ratio_bmbo_tot_tree if ag_supp<2, by(ag_supp)

ttest tot_tree if ag_supp==0 | ag_supp==2, by(ag_supp)
ttest per_acre_tree if ag_supp==0 | ag_supp==2, by(ag_supp)
ttest tree_grow_dummy if ag_supp==0 | ag_supp==2, by(ag_supp)
ttest ratio_hort_tot_tree if ag_supp==0 | ag_supp==2, by(ag_supp)
ttest ratio_timb_tot_tree if ag_supp==0 | ag_supp==2, by(ag_supp)
ttest ratio_bmbo_tot_tree if ag_supp==0 | ag_supp==2, by(ag_supp)

* 3.3 Test differences in medians (Mann–Whitney test)

su tot_tree, detail

centile tot_tree
centile per_acre_tree

median tot_tree if ag_supp<2, by(ag_supp)
median per_acre_tree if ag_supp<2, by(ag_supp)

median tot_tree if ag_supp==0 | ag_supp==2, by(ag_supp)
median per_acre_tree if ag_supp==0 | ag_supp==2, by(ag_supp)


* 4 Data for Supplementary Figure 1, Village exposure to agroforestry programs
by village, sort : tabulate heard_SMAFF2, sort


* 5 Generate tree age variable

tab tree1, missing

gen treeplnt_year_1 = year(tree_plant_monthyr_1)
gen tree_age_1 = (2023 - treeplnt_year_1) 
 
count if treeplnt_year_1!=.  // 239 obs

histogram treeplnt_year_1

save TemporaryDataset, replace
clear all


* 5 Matching models: IPWRA

use TemporaryDataset

* 5.1 Outcome: tree_grow_dummy

* Determine min and max values of matching covariates by treatment group

global control1 caste_ST dist_Sehore Distancetotheclosestforestry  ///
dummy_irrigation_GW fruitmarket_distance kvk_distance prec_rabi_1995_2014 /// 
puccaroad_distance resp_schoolyr tmin_rabi_1995_2014 tot_land_operate_acre /// 
tot_land_operate_acre_2

tabstat $control1 

global cont1 Distancetotheclosestforestry  fruitmarket_distance kvk_distance ///
prec_rabi_1995_2014 puccaroad_distance resp_schoolyr tmin_rabi_1995_2014 ///
tot_land_operate_acre tot_land_operate_acre_2       

* Check common support in tails of continuous variables selected by CovSel

gen Support = 1

foreach var of varlist $cont1 {
	
	su `var' if heard_SMAFF2==0
	replace Support = 0 if `var'<0.95*r(min) & heard_SMAFF2==1
	replace Support = 0 if `var'>1.05*r(max) & heard_SMAFF2==1
	
	su `var' if heard_SMAFF2==1
	replace Support = 0 if `var'<0.95*r(min) & heard_SMAFF2==0
	replace Support = 0 if `var'>1.05*r(max) & heard_SMAFF2==0
	}

bysort heard_SMAFF2: tab Support

keep if Support==1  // Drop 7 observations without common support (i.e., beyond 5% of min and max values )
drop Support

* Check puccaroad_distance

bysort tree_grow_dummy ag_supp: count if puccaroad_distance==0
bysort tree_grow_dummy ag_supp: count if puccaroad_distance!=0
* Has too few nonzero values, so drop from this model and subsequent ones

global control1 caste_ST dist_Sehore Distancetotheclosestforestry  ///
dummy_irrigation_GW fruitmarket_distance kvk_distance prec_rabi_1995_2014 /// 
resp_schoolyr tmin_rabi_1995_2014 tot_land_operate_acre /// 
tot_land_operate_acre_2

* Check balance

quietly teffects ipwra (tree_grow_dummy $control1, logit) (ag_supp $control1), atet aeq 

tebalance summarize

* Austin (2009) bounds for variance ratios

di invFtail(138,138,0.975)
di invFtail(138,138,0.025)

di invFtail(82,82,0.975)
di invFtail(82,82,0.025)  

* Variance ratio for Distancetotheclosestforestry is outside the guideline for ag_supp==2, but retain this covariate because it is within both balance guidelines for ag_supp==1 and its SMD is nearly within the guideline for ag_supp==2

* Estimate ATET

teffects ipwra (tree_grow_dummy $control1, logit) (ag_supp $control1), atet aeq

bysort ag_supp: su tree_grow_dummy

clear all

* 5.2 Outcome: tot_tree 

use TemporaryDataset

* Determine min and max values of matching covariates by treatment group

global control2 agroeco_dummy_2 caste_obc caste_ST dist_Hoshangabad /// 
Distancetotheclosestforestry dummy_irrigation_GW fruitmarket_distance /// 
hh_mem_total inputmarket_distance pulsmarket_distance resp_schoolyr /// 
tmax_kharif_1995_2014 tmax_rabi_1995_2014 tmin_kharif_1995_2014 /// 
tot_land_operate_acre tot_land_operate_acre_2

tabstat $control2 

global cont2 Distancetotheclosestforestry fruitmarket_distance /// 
hh_mem_total inputmarket_distance pulsmarket_distance resp_schoolyr /// 
tmax_kharif_1995_2014 tmax_rabi_1995_2014 tmin_kharif_1995_2014 /// 
tot_land_operate_acre tot_land_operate_acre_2   

* Check common support in tails of continuous variables selected by CovSel

gen Support = 1

foreach var of varlist $cont2 {
	
	su `var' if heard_SMAFF2==0
	replace Support = 0 if `var'<0.95*r(min) & heard_SMAFF2==1
	replace Support = 0 if `var'>1.05*r(max) & heard_SMAFF2==1
	
	su `var' if heard_SMAFF2==1
	replace Support = 0 if `var'<0.95*r(min) & heard_SMAFF2==0
	replace Support = 0 if `var'>1.05*r(max) & heard_SMAFF2==0
	}

bysort heard_SMAFF2: tab Support

keep if Support==1  // Drop 16 observations without common support (i.e., beyond 5% of min and max values )
drop Support

su tot_tree
replace tot_tree = tot_tree / 1000

* Check balance

quietly teffects ipwra (tot_tree $control2) (ag_supp $control2), atet aeq 

tebalance summarize

* Austin (2009) bounds for variance ratios

di invFtail(138,138,0.975)
di invFtail(138,138,0.025)

di invFtail(82,82,0.975)
di invFtail(82,82,0.025)

* Retain all covariates because all satisfy both balance criteria for ag_supp==1, nearly all satisfy both for ag_supp==2, and all satisfy at least one criterion for ag_supp==2.

* Estimate ATET

teffects ipwra (tot_tree $control2) (ag_supp $control2), atet aeq 

bysort ag_supp: su tot_tree

clear all

* 5.3 Outcome: per_acre_tree

use TemporaryDataset

* Determine min and max values of matching covariates by treatment group

global control3 agroeco_dummy_1 agroeco_dummy_2 dist_Jabalpur ///
dist_rangeoffice Distancetotheclosestforestry dummy_irrigation_GW ///  
hh_mem_total resp_age tot_land_operate_acre /// 
tot_land_operate_acre_2
* Have dropped puccaroad_distance, per analysis in section 5.1

tabstat $control3 

global cont3 dist_rangeoffice Distancetotheclosestforestry ///  
hh_mem_total resp_age tot_land_operate_acre /// 
tot_land_operate_acre_2   

* Check common support in tails of continuous variables selected by CovSel

gen Support = 1

foreach var of varlist $cont3 {
	
	su `var' if heard_SMAFF2==0
	replace Support = 0 if `var'<0.95*r(min) & heard_SMAFF2==1
	replace Support = 0 if `var'>1.05*r(max) & heard_SMAFF2==1
	
	su `var' if heard_SMAFF2==1
	replace Support = 0 if `var'<0.95*r(min) & heard_SMAFF2==0
	replace Support = 0 if `var'>1.05*r(max) & heard_SMAFF2==0
	}

bysort heard_SMAFF2: tab Support

keep if Support==1  // Drop 17 observations without common support (i.e., beyond 5% of min and max values )
drop Support

* Check balance

quietly teffects ipwra (per_acre_tree $control3) (ag_supp $control3), atet aeq 
 
tebalance summarize  

* Austin (2009) bounds for variance ratios

di invFtail(138,138,0.975)
di invFtail(138,138,0.025)

di invFtail(83,83,0.975)
di invFtail(83,83,0.025)

* For reasons analogous to those given in section 5.2, retain all covariates.

* Estimate ATET

teffects ipwra (per_acre_tree $control3) (ag_supp $control3), atet aeq

bysort ag_supp: su per_acre_tree

clear all

* 5.4 Outcome: ratio_hort_tot_tree

use TemporaryDataset
* Restrict sample to farmers who grow trees
keep if tree_grow_dummy==1

* Determine min and max values of matching covariates by treatment group

global control4 agroeco_dummy_1 caste_obc dist_Jabalpur dist_rangeoffice /// 
Distancetotheclosestforestry dummy_irrigation_GW fruitmarket_distance /// 
gps_resp_housealtitude kvk_distance prec_rabi_1995_2014 pulsmarket_distance /// 
resp_schoolyr tmin_kharif_1995_2014 tot_land_operate_acre tot_land_operate_acre_2 ///
vegmarket_distance
   
tabstat $control4  

global cont4 dist_rangeoffice Distancetotheclosestforestry fruitmarket_distance /// 
gps_resp_housealtitude kvk_distance prec_rabi_1995_2014 pulsmarket_distance /// 
resp_schoolyr tmin_kharif_1995_2014 tot_land_operate_acre tot_land_operate_acre_2 ///
vegmarket_distance 

* Check common support in tails of continuous variables selected by CovSel

gen Support = 1

foreach var of varlist $cont4 {
	
	su `var' if heard_SMAFF2==0
	replace Support = 0 if `var'<0.95*r(min) & heard_SMAFF2==1
	replace Support = 0 if `var'>1.05*r(max) & heard_SMAFF2==1
	
	su `var' if heard_SMAFF2==1
	replace Support = 0 if `var'<0.95*r(min) & heard_SMAFF2==0
	replace Support = 0 if `var'>1.05*r(max) & heard_SMAFF2==0
	}

bysort heard_SMAFF2: tab Support

keep if Support==1  // Drop 6 observations without common support (i.e., beyond 5% of min and max values )
drop Support

* Check balance

quietly teffects ipwra (ratio_hort_tot_tree $control4) (ag_supp $control4), atet aeq 
 
tebalance summarize  

* Austin (2009) bounds for variance ratios

di invFtail(101,101,0.975)
di invFtail(101,101,0.025)

di invFtail(68,68,0.975)
di invFtail(68,68,0.025)

* All covariates are balanced according to both criteria

* Estimate ATET

teffects ipwra (ratio_hort_tot_tree $control4) (ag_supp $control4), atet aeq

bysort ag_supp: su ratio_hort_tot_tree

clear all

* 5.5 Outcome: ratio_timb_tot_tree 

use TemporaryDataset
* Restrict sample to farmers who grow trees
keep if tree_grow_dummy==1

* Determine min and max values of matching covariates by treatment group

global control5 caste_obc cermarket_distance Distancetotheclosestforestry /// 
dummy_irrigation_GW  kvk_distance  prec_kharif_1995_2014 resp_schoolyr /// 
tmin_kharif_1995_2014 tmin_rabi_1995_2014 tot_land_operate_acre ///  
tot_land_operate_acre_2 vegmarket_distance

tabstat $control5 

global cont5 cermarket_distance Distancetotheclosestforestry /// 
kvk_distance  prec_kharif_1995_2014 resp_schoolyr /// 
tmin_kharif_1995_2014 tmin_rabi_1995_2014 tot_land_operate_acre ///  
tot_land_operate_acre_2 vegmarket_distance
   
* Check common support in tails of continuous variables selected by CovSel

gen Support = 1

foreach var of varlist $cont5 {
	
	su `var' if heard_SMAFF2==0
	replace Support = 0 if `var'<0.95*r(min) & heard_SMAFF2==1
	replace Support = 0 if `var'>1.05*r(max) & heard_SMAFF2==1
	
	su `var' if heard_SMAFF2==1
	replace Support = 0 if `var'<0.95*r(min) & heard_SMAFF2==0
	replace Support = 0 if `var'>1.05*r(max) & heard_SMAFF2==0
	}

bysort heard_SMAFF2: tab Support

keep if Support==1  // Drop 5 observations without common support (i.e., beyond 5% of min and max values )
drop Support

* Check balance

quietly teffects ipwra (ratio_timb_tot_tree $control5) (ag_supp $control5), atet aeq 
 
tebalance summarize  

* Austin (2009) bounds for variance ratios

di invFtail(102,102,0.975)
di invFtail(102,102,0.025)

di invFtail(68,68,0.975)
di invFtail(68,68,0.025)

* All covariates are balanced according to both criteria

* Estimate ATET

teffects ipwra (ratio_timb_tot_tree $control5) (ag_supp $control5), atet aeq

bysort ag_supp: su ratio_timb_tot_tree

clear all

* 5.6 Outcome: ratio_bmbo_tot_tree

use TemporaryDataset
* Restrict sample to farmers who grow trees
keep if tree_grow_dummy==1

* Determine min and max values of matching covariates by treatment group

global control6 agroeco_dummy_1 cermarket_distance dummy_irrigation_GW ///  
gps_resp_housealtitude resp_age resp_schoolyr tmin_kharif_1995_2014 /// 
tot_land_operate_acre tot_land_operate_acre_2

tabstat $control6  

global cont6 cermarket_distance ///  
gps_resp_housealtitude resp_age resp_schoolyr tmin_kharif_1995_2014 /// 
tot_land_operate_acre tot_land_operate_acre_2  

* Check common support in tails of continuous variables selected by CovSel

gen Support = 1

foreach var of varlist $cont6 {
	
	su `var' if heard_SMAFF2==0
	replace Support = 0 if `var'<0.95*r(min) & heard_SMAFF2==1
	replace Support = 0 if `var'>1.05*r(max) & heard_SMAFF2==1
	
	su `var' if heard_SMAFF2==1
	replace Support = 0 if `var'<0.95*r(min) & heard_SMAFF2==0
	replace Support = 0 if `var'>1.05*r(max) & heard_SMAFF2==0
	}

bysort heard_SMAFF2: tab Support

keep if Support==1  // Drop 9 observations without common support (i.e., beyond 5% of min and max values )
drop Support

* Check balance

quietly teffects ipwra (ratio_bmbo_tot_tree $control5) (ag_supp $control5), atet aeq 
 
tebalance summarize  

* All covariates are balanced according to both criteria

* Austin (2009) bounds for variance ratios

di invFtail(101,101,0.975)
di invFtail(101,101,0.025)

di invFtail(68,68,0.975)
di invFtail(68,68,0.025)

* Estimate ATET

teffects ipwra (ratio_bmbo_tot_tree $control6) (ag_supp $control6), atet aeq

bysort ag_supp: su ratio_bmbo_tot_tree

clear all


* 6 Prediction models: lasso

use TemporaryDataset

* 6.1 Define groups of potential predictors

global distdummy dist_Harda dist_Hoshangabad dist_Jabalpur dist_Sehore dist_Shajapur 

global agroecocharec agroeco_dummy_1 agroeco_dummy_2 agroeco_dummy_3 tmin_rabi_1995_2014 tmax_rabi_1995_2014 prec_rabi_1995_2014 tmin_kharif_1995_2014 tmax_kharif_1995_2014 prec_kharif_1995_2014 gps_resp_housealtitude

global mktcharec puccaroad_distance inputmarket_distance cermarket_distance vegmarket_distance fruitmarket_distance pulsmarket_distance

global govtfclt kvk_distance Distancetotheclosestforestry Distancetotheclosestgovernme Distancetotheclosestprivate dist_rangeoffice

global hhcharec caste_gen caste_obc caste_sc caste_ST hh_mem_total resp_age resp_schoolyr tot_land_operate_acre tot_land_operate_acre_2 credit_aces dummy_irrigation_GW

* 6.2 Define sample (untreated farmers only)

drop if ag_supp==1 | ag_supp==2

* 6.3 Lasso models by outcome variable 
 
lasso logit tree_grow_dummy $govtfclt $hhcharec $distdummy $mktcharec $agroecocharec, cluster (village) rseed(151176)
lassogof
estimate store M1apl 
lassocoef M1apl, display(coef, standardized)
* x's show which controls were selected by lasso

lasso linear tot_tree $govtfclt $hhcharec $distdummy $mktcharec $agroecocharec, cluster (village) rseed(151176)
lassogof, postselection 
estimate store M1ap2 
lassocoef M1ap2, display(coef, standardized)

lasso linear per_acre_tree $govtfclt $hhcharec $distdummy $mktcharec $agroecocharec, cluster (village) rseed(151176)
lassogof, postselection 
estimate store M1ap3 
lassocoef M1ap3, display(coef, standardized)

* Estimate remaining models on sample that includes only farmers who grow trees
keep if tree_grow_dummy==1

lasso linear ratio_hort_tot_tree $govtfclt $hhcharec $distdummy $mktcharec $agroecocharec, cluster (village) rseed(151176)
lassogof, postselection  
estimate store M1ap4 
lassocoef M1ap4, display(coef, standardized)

lasso linear ratio_timb_tot_tree $govtfclt $hhcharec $distdummy $mktcharec $agroecocharec, cluster (village) rseed(151176)
lassogof, postselection
estimate store M1ap5 
lassocoef M1ap5, display(coef, standardized)

lasso linear ratio_bmbo_tot_tree $govtfclt $hhcharec $distdummy $mktcharec $agroecocharec, cluster (village) rseed(151176)
lassogof, postselection
estimate store M1ap6 
lassocoef M1ap6, display(coef, standardized)
clear all


* 7 Create datasets for CovSel

* 7.1 Dataset for Treatment 1

use TemporaryDataset.dta 

keep qre_uniqueid resp_age resp_schoolyr hh_mem_total tot_land_operate_acre ///
gps_resp_housealtitude puccaroad_distance inputmarket_distance cermarket_distance ///
vegmarket_distance fruitmarket_distance pulsmarket_distance kvk_distance ///
Distancetotheclosestgovernme Distancetotheclosestprivate Distancetotheclosestforestry ///
dist_rangeoffice tmin_rabi_1995_2014 tmax_rabi_1995_2014 prec_rabi_1995_2014 ///
tmin_kharif_1995_2014 tmax_kharif_1995_2014 prec_kharif_1995_2014 tree_grow_dummy tot_tree per_acre_tree ///
ratio_hort_tot_tree ratio_timb_tot_tree ratio_bmbo_tot_tree dist_Hoshangabad ///
dist_Jabalpur dist_Sehore dist_Shajapur agroeco_dummy_1 agroeco_dummy_2 caste_obc caste_sc caste_ST ///
credit_aces tot_land_operate_acre_2 dummy_irrigation_GW heard_SMAFF2 partcp_SMAFF2 ///
ag_supp

drop if ag_supp==2
drop ag_supp 
replace partcp_SMAFF2=0 if partcp_SMAFF2==.   

save "Data_Variables for COVSEL Treatment 1.dta", replace
clear all

* 7.2 Dataset for Treatment 2

use TemporaryDataset.dta 

keep qre_uniqueid resp_age resp_schoolyr hh_mem_total tot_land_operate_acre ///
gps_resp_housealtitude puccaroad_distance inputmarket_distance cermarket_distance ///
vegmarket_distance fruitmarket_distance pulsmarket_distance kvk_distance ///
Distancetotheclosestgovernme Distancetotheclosestprivate Distancetotheclosestforestry ///
dist_rangeoffice tmin_rabi_1995_2014 tmax_rabi_1995_2014 prec_rabi_1995_2014 ///
tmin_kharif_1995_2014 tmax_kharif_1995_2014 prec_kharif_1995_2014 tree_grow_dummy tot_tree per_acre_tree ///
ratio_hort_tot_tree ratio_timb_tot_tree ratio_bmbo_tot_tree dist_Hoshangabad ///
dist_Jabalpur dist_Sehore dist_Shajapur agroeco_dummy_1 agroeco_dummy_2 caste_obc caste_sc caste_ST ///
credit_aces tot_land_operate_acre_2 dummy_irrigation_GW heard_SMAFF2 partcp_SMAFF2 ///
ag_supp

drop if ag_supp==1
drop ag_supp 
replace partcp_SMAFF2=0 if partcp_SMAFF2==.   

save "Data_Variables for COVSEL Treatment 2.dta", replace
clear all


* 8 Create dataset for Supplementary Figure 3, Planting years for commercial tree species planted by farmers

use TemporaryDataset.dta 

keep qre_uniqueid treeplnt_year_1
drop if treeplnt_year_1==. 

save "Data_Tree Planting Year.dta", replace
clear all

erase TemporaryDataset.dta
clear all
