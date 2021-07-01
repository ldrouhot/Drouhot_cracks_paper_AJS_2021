// May 2021
// Lucas G. Drouhot, Max Planck Institute for the Study of Religious and Ethnic Diversity
// This is cleaning program for data preparation for analyses reproducing results in:

// Drouhot, Lucas G. 2021. "Cracks in the Melting Pot ? Religiosity & Assimilation Among the Diverse Muslim Population in France" American Journal of Sociology 126(4) 795-851.

// N.B. the code is the "omnibus" cleaning code I use for all TeO analyses and includes observations and annotations related to TeO data in general, not just this paper.
// Comments, questions, criticisms welcome at: drouhot@mmg.mpg.de. 
// More information at lucasdrouhot.com

**************************************************************
** Dataset used: "Trajectoires et Origines" survey (INED/INSEE), 2008. More info on dataset and data access here: http://bdq.quetelet.progedo.fr/fr/Details_d_une_enquete/1808
**************************************************************


* retrieve dataset
******************

* use "", clear *



{ // SUMMING, INITIAL INSPECTION OF KEY VARIABLES, AND TRIMMING DATASET
set more off 

// Dependent variables of interest:

// Dad and mom's religion
tab relmere1
tab relpere1

// Importance of religion in respondent's life, and respect of religious practices
sum r_impedu
sum r_culte
sum r_miam
sum r_ostent
tab regiontomb2
tab r_tombe

// Ego's religion:

tab relego1


// Religious signs:

tab signerel1
tab signerel2
tab signerel3

/////


// Let's find the independent variables of interest, especially relating to SES, plus other relevant things for multivariate analysis. 

rename agenq age

// Marital status
destring matrie, replace

// Mother tongue
sum lref_gr

// Respondent's survey weight (sum==total number of respondents)
sum poidsin

// Overall monthly salary (second variable has higher n)
sum p_salnet
sum p_salmens_ego

// Monthly family income (this obivously need to be controlled for number of persons in household)

sum a_treve

// Highest degree obtained
sum f_dip

// Highest degree obtained and also for parents (following international nomenclature)
tab f_isced
sum t_isced_p t_isced_m

// Parents' profession

sum isco_mere isco_pere

// Creating a dummy for high skilled/upper-middle class occupation:

destring cs42_ego, replace
gen middle_class=0 if cs42_ego!=.
replace middle_class=1 if cs42_ego>=23 & cs42_ego<=42


// age of naturalization
sum agenat

// Country or region of birth
tab regionnaise2
destring regionnaisp2 regionnaism2, replace

// Place of birth of parents (France, abroad, etc)

tab lieunaispr 
tab lieunaismr

// Parental conflict about religion, friends and love relationships:
sum t_crelig
sum t_camis
sum t_camour

// Number of brothers and sisters
sum t_frsoe

// Feeling French

sum  x_moifr x_oubli x_vufri x_apparf

// Percent immigrants in attended middle school
destring f_immcol, replace
sum f_immcol

// Situation at work

sum p_colleg
sum p_pressi
tab p_sa1com 
tab p_sa2com 
tab p_sa3com

// On welfare (think proportion of total income)
// Amount of income received on welfare
sum p_mtallo

// number of years and months unemployed
sum p_qdchom
sum p_qdchoa


// Spouse or partner's highest degree

sum c_diploc

// Relgious marriage and forced marriage

tab c_marec
tab c_forcec
tab c_forcep

// Partner's religion (and "premier conjoint")

tab c_religa
tab relca1
tab c_religp
tab  c_religc

// Respondent's geographic departement when 15 years old

tab j15dpt

// Type of housing when 15 years old:

tab  l_j15typ

// Colleagues and clients: origins

tab p_client
tab p_colleg

// origins of colleagues for first job upon arriving in France:

tab p_coll

// Unfair rejection at a job application:

tab p_drech

// Immigrants in neighborhood

tab l_immi

// Year of arrival in current housing

tab l_demen

// Having an currently open application for social housing (HLM)

tab  l_ddehlm

// Relationship with institutions and trust in institutions

sum  i_cnfjus i_cnfpol i_cnfss i_ecole i_contri i_control i_intcom

sum d_mairie d_prefec d_poste d_admini

// Opinion about racist discriminations:

tab  d_inegal

// Experience of discrimination

sum  d_qdraci d_racpot d_nom d_peau d_accent d_relig d_natio d_apphy d_racaut

// Other experiences and context of experience (qualitative)

tab  d_ouracp
tab  d_racq

// Friendship homogeneity: 

sum  a_retud a_rrelig a_rorig a_rhom a_rfem

// Neighboring and interactions with colleagues:

sum  a_rvoisi a_rcoleg

// Controling for quality of response and effort of interviewee:

sum  q_amelio q_accuei

// Characteristics chosen by respondents to define themselves:

sum  x_presa_a x_presa_b x_presa_c x_presa_d x_presa_e x_presa_f x_presa_g x_presa_h x_presa_i x_presa_j x_presa_k x_presa_l x_presa_m x_presa_n x_presa_o x_presa_nbrep x_presb_flag x_presb_a x_presb_b x_presb_c x_presb_d x_presb_e x_presb_f x_presb_g x_presb_h x_presb_i x_presb_j x_presb_k x_presb_l x_presb_m x_presb_n x_presb_o x_presb_nbrep f_sectpq_flag

// NOTE: I noticed some mistakes in there (the second skin color item for each fiche pair / impair is actually a question about hobbies.

// Perception of differential treatment at school:

sum f_dis_a f_dis_b f_dis_c f_dis_d f_dis_e f_dis_f f_dis_g f_dis_nbrep

// Perceptionof causes of difficulty at work (not sure how these three differ): 

sum  p_saqcom_c p_saqcom_d p_saqcom_e p_saqcom_f p_saqcom_g p_saqcom_nbrep p_aiqcom_a p_aiqcom_c p_aiqcom_d p_aiqcom_e p_aiqcom_f p_aiqcom_g
sum  p_nsqcom_c p_nsqcom_d p_nsqcom_e p_nsqcom_f p_nsqcom_nbrep
sum  p_aiqcom_c p_aiqcom_d p_aiqcom_e p_aiqcom_f p_aiqcom_g

// Perception of difficulties when looking for a job:

sum  p_chqcom_c p_chqcom_d p_chqcom_e p_chqcom_f p_chqcom_g p_chqcom_nbrep

// Reasons for not leaving parental home:

sum  l_tangpq_a l_tangpq_b l_tangpq_c l_tangpq_d l_tangpq_e l_tangpq_f l_tangpq_nbrep

// Perceptions of discirmination when renting apartment:

sum  l_dispq_a l_dispq_b l_dispq_c l_dispq_d l_dispq_e l_dispq_f l_dispq_g l_dispq_nbrep

// CHanging behavior in certain public spaces:

sum  d_compor_a d_compor_b d_compor_c d_compor_d d_compor_e d_compor_f d_compor_g d_compor_nbrep d_compor_flag

// Experience of racisme in different places:

sum  d_ouraci_a d_ouraci_b d_ouraci_c d_ouraci_d d_ouraci_e d_ouraci_f d_ouraci_g d_ouraci_h d_ouraci_i d_ouraci_j d_ouraci_k d_ouraci_nbrep d_ouraci_flag

// Controlling for interviewee's lack of interest or focus

sum  q_pqmal_c q_pqmal_b q_pqmal_a

// Parents' education level

sum  diplom diplop

// Housing type / type of built environment

tab typvois

// Job code:

tab cs42_ego
tab cs24_ego
tab isco_act

// Type of city

tab tu2008
destring tu2008, replace

// ZEP ou ZUS:

tab zfu2008
tab zus2008

// Region where ego has friendly or family contacts:

tab regionnew12 
tab regionnew22 
tab regionnew32




/// This trims the dataset down to the variables of interest for now.
keep ident sexee relmere1 relpere1 r_impedu r_culte r_miam r_ostent regiontomb2 r_tombe relego1 signerel1 signerel2 signerel3 age ///
matrie lref_gr poidsin poidsi p_salnet p_salmens_ego a_treve f_dip f_isced t_isced_p t_isced_m  isco_mere isco_pere agenat regionnaise2 ///
lieunaispr lieunaismr t_crelig t_camis t_camour t_frsoe x_moifr x_oubli x_vufri  x_apparf f_immcol p_colleg p_pressi p_sa1com ///
p_sa2com  p_sa3com p_mtallo p_qdchom p_qdchoa c_diploc c_marec c_forcec c_forcep c_religa relca1 c_religp c_religc j15dpt l_j15typ ///
p_client p_colleg p_coll p_drech l_immi l_demen l_ddehlm i_cnfjus  i_cnfpol  i_cnfss  i_ecole  i_contri i_control  i_intcom d_mairie ///
d_prefec d_poste  d_admini d_inegal d_qdraci  d_racpot  d_nom  d_peau d_accent  d_relig  d_natio  d_apphy  d_racaut d_ouracp d_racq ///
a_retud  a_rrelig  a_rorig  a_rhom  a_rfem a_rvoisi a_rcoleg q_amelio  q_accuei x_presa_a  x_presa_b  x_presa_c  x_presa_d  x_presa_e  x_presa_f ///
x_presa_g  x_presa_h  x_presa_i  x_presa_j  x_presa_k  x_presa_l  x_presa_m  x_presa_n  x_presa_o  x_presa_nbrep  x_presb_flag  x_presb_a  x_presb_b  x_presb_c x_presb_d ///
x_presb_e  x_presb_f  x_presb_g  x_presb_h  x_presb_i  x_presb_j  x_presb_k x_presb_l  x_presb_m  x_presb_n  x_presb_o  x_presb_nbrep  f_sectpq_flag f_dis_a ///
f_dis_b  f_dis_c  f_dis_d  f_dis_e  f_dis_f  f_dis_g  f_dis_nbrep p_saqcom_c p_saqcom_d p_saqcom_e p_saqcom_f p_saqcom_g p_saqcom_nbrep p_aiqcom_a p_aiqcom_c p_aiqcom_d p_aiqcom_e p_aiqcom_f p_aiqcom_g ///
p_nsqcom_c p_nsqcom_d p_nsqcom_e p_nsqcom_f p_nsqcom_nbrep p_aiqcom_c p_aiqcom_d p_aiqcom_e p_aiqcom_f p_aiqcom_g ///
p_chqcom_c p_chqcom_d p_chqcom_e p_chqcom_f p_chqcom_g p_chqcom_nbrep l_tangpq_a l_tangpq_b l_tangpq_c l_tangpq_d l_tangpq_e l_tangpq_f l_tangpq_nbrep ///
l_dispq_a l_dispq_b l_dispq_c l_dispq_d l_dispq_e l_dispq_f l_dispq_g l_dispq_nbrep d_compor_a d_compor_b d_compor_c d_compor_d d_compor_e d_compor_f d_compor_g d_compor_nbrep d_compor_flag ///
d_ouraci_a d_ouraci_b d_ouraci_c d_ouraci_d d_ouraci_e d_ouraci_f d_ouraci_g d_ouraci_h d_ouraci_i d_ouraci_j d_ouraci_k d_ouraci_nbrep  d_ouraci_flag q_pqmal_c  q_pqmal_b ///
q_pqmal_a diplom  diplop typvois cs42_ego cs24_ego isco_act tu2008 zfu2008 zus2008 regionnew12  regionnew22  regionnew32 lnaisp lnaism r_impvie p_auest d_discri lieunaiser d_racism situae ///
l_nbpiec npers nenfants couplee hc_act vadactiris_rfu vadactiris_rfu_code vadactcom_rfu vadantcom_rfu vadactcom_arrdt_rfu vadantcom_arrdt_rfu demrec mouvement demrec_qual adact_qual clpopc_demrec ///
clpopc_adact dadactiris_pct_m18 dadactiris_pct_60p dadactiris_pct_65p dadactiris_pct_act_atyp dadactiris_pct_act_cadr dadactiris_pct_act_comres dadactiris_pct_act_interim dadactiris_pct_act_nsala ///
dadactiris_pct_act_ouvr dadactiris_pct_act_tp dadactiris_pct_coupact dadactiris_pct_monop dadactiris_pct_dip_bacp dadactiris_pct_dip_prim dadactiris_pct_elev_ncom dadactiris_pct_men5p dadactiris_tx_occup ///
dadactiris_pct_hlm dadactiris_pct_lognord dadactiris_pct_maison dadactiris_pct_proprio dadactiris_pct_emrec dadactiris_pct_mobil dadactiris_pct_mob_com dadactiris_pct_sedent dadactiris_pct_imafs ///
dadactiris_pct_imagh dadactiris_pct_imeus dadactiris_pct_immi dadactiris_pct_imue27 dadactiris_pct_natur dadactiris_pct_etrang dadactiris_pct_prfam_dom dadactiris_pct_prfam_dtom dadactiris_pct_prfam_imm ///
dadactiris_tx_act1524 dadactiris_tx_act15p dadactiris_tx_actf15p dadactiris_tx_chom1524 dadactiris_tx_chom15p dadactiris_tx_chomf15p dadactiris_tx_chompan dadactiris_tx_chom_immi dadactiris_tx_emploi1524 ///
dadactiris_tx_emploif_1564 dadactiris_tx_emploi_1564 r_relsoi p_sanbw cprop a_avis t_cloisir p_activa regionnaism2 regionnaisp2 a_news m_ivtcol t_argent m_ivt  m_propri ///
regionnew12 regionnew22 regionnew32 regioninv2 regionprop2 regionver12 regionver22 regionver32 m_visp m_vism m_vismj m_vispj uc

//dadactcom_pct_m18 dadactcom_pct_60p dadactcom_pct_65p dadactcom_pct_act_atyp dadactcom_pct_act_cadr dadactcom_pct_act_comres dadactcom_pct_act_interim ///
//dadactcom_pct_act_nsala dadactcom_pct_act_ouvr dadactcom_pct_act_tp dadactcom_pct_coupact dadactcom_pct_monop dadactcom_pct_dip_bacp dadactcom_pct_dip_prim dadactcom_pct_elev_ncom dadactcom_pct_men5p ///
//dadactcom_tx_occup dadactcom_pct_hlm dadactcom_pct_lognord dadactcom_pct_maison dadactcom_pct_proprio dadactcom_pct_emrec dadactcom_pct_mobil dadactcom_pct_mob_com dadactcom_pct_sedent dadactcom_pct_imafs ///
//dadactcom_pct_imagh dadactcom_pct_imeus dadactcom_pct_immi dadactcom_pct_imue27 dadactcom_pct_natur dadactcom_pct_etrang dadactcom_pct_prfam_dom dadactcom_pct_prfam_dtom dadactcom_pct_prfam_imm dadactcom_tx_act1524 ///
//dadactcom_tx_act15p dadactcom_tx_actf15p dadactcom_tx_chom1524 dadactcom_tx_chom15p dadactcom_tx_chomf15p dadactcom_tx_chompan dadactcom_tx_chom_immi dadactcom_tx_emploi1524 dadactcom_tx_emploif_1564 dadactcom_tx_emploi_1564 ///
//dadactcom_arrdt_pct_m18 dadactcom_arrdt_pct_60p dadactcom_arrdt_pct_65p dadactcom_arrdt_pct_act_atyp dadactcom_arrdt_pct_act_cadr dadactcom_arrdt_pct_act_comres dadactcom_arrdt_pct_act_interim dadactcom_arrdt_pct_act_nsala ///
//dadactcom_arrdt_pct_act_ouvr dadactcom_arrdt_pct_act_tp dadactcom_arrdt_pct_coupact dadactcom_arrdt_pct_monop dadactcom_arrdt_pct_dip_bacp dadactcom_arrdt_pct_dip_prim dadactcom_arrdt_pct_elev_ncom dadactcom_arrdt_pct_men5p ///
//dadactcom_arrdt_tx_occup dadactcom_arrdt_pct_hlm dadactcom_arrdt_pct_lognord dadactcom_arrdt_pct_maison dadactcom_arrdt_pct_proprio dadactcom_arrdt_pct_emrec dadactcom_arrdt_pct_mobil dadactcom_arrdt_pct_mob_com ///
//dadactcom_arrdt_pct_sedent dadactcom_arrdt_pct_imafs dadactcom_arrdt_pct_imagh dadactcom_arrdt_pct_imeus dadactcom_arrdt_pct_immi dadactcom_arrdt_pct_imue27 dadactcom_arrdt_pct_natur dadactcom_arrdt_pct_etrang ///
//dadactcom_arrdt_pct_prfam_dom dadactcom_arrdt_pct_prfam_dtom dadactcom_arrdt_pct_prfam_imm dadactcom_arrdt_tx_act1524 dadactcom_arrdt_tx_act15p dadactcom_arrdt_tx_actf15p dadactcom_arrdt_tx_chom1524 dadactcom_arrdt_tx_chom15p ///
//dadactcom_arrdt_tx_chomf15p dadactcom_arrdt_tx_chompan dadactcom_arrdt_tx_chom_immi dadactcom_arrdt_tx_emploi1524 dadactcom_arrdt_tx_emploif_1564 dadactcom_arrdt_tx_emploi_1564

}


{ // RECODING AND OTHER ASPECTS OF DATA PREP

{ // Socio-demographic variables


// Oddly, the age variable starts at 17. It should be 18.

destring lnaisp, replace
destring lnaism, replace

replace age=age+1
gen age_sq=age*age

/// Making a categorical age variable

capture drop agecat
gen agecat=.
replace agecat=1 if age<=25
replace agecat=2 if age>25 & age<=30
replace agecat=3 if age>30 & age<=35
replace agecat=4 if age>35 & age<=40
replace agecat=5 if age>40 & age<=45
replace agecat=6 if age>45 & age<=50
replace agecat=7 if age>50 & age<=55
replace agecat=8 if age>55

label define agecat1 1 "Age 18-25" 2 "Age 26-30" 3 "Age 31-35" 4 "Age 36-40" 5 "Age 41-45" 6 "Age 46-50" 7 "Age 51-55" 8 "Age 55 and over" 
label values agecat agecat1
tab agecat

// Age: none is missing
// Gender: none is missing
destring sexee, replace
replace sexee=0 if sexee==2
label define sex 1 "Male" 0 "Female"
label values sexee sex

// Matrimonial situation: none is missing. Not sure I need t ocontrol for it though.
// Professional situation: none missing. 
// Agenat: none missing
// Citizenship: none missing
// Work suffer: none missing
// Situpro: none missing

gen citizenship=.
replace citizenship=1 if lieunaiser==2
replace citizenship=3 if lieunaiser==1
replace citizenship=2 if agenat!=. & age>28
label define cit 1 "Foreign" 2 "Naturalized" 3 "French by jus soli"
label values citizenship cit

gen naturalized=0
replace naturalized=1 if citizenship==2

}

{ // education, work, income

destring p_activa, replace
gen ever_worked=1
replace ever_worked=0 if p_activa==2

destring f_dip, replace

label define diplome 1 "No degree" 2 "Primary school" 3 "Middle school" 4 "Vocational training" 5 "Technical high school" 6 "General high school" 7 "Some college" 8 "Bachelor or more"
label values f_dip diplome


destring a_treve, replace
label define menage 0 "No income" 1 "Less than 400" 2 "400-600" 3 "600-800" 4 "800-1k" 5 "1-1.2k" 6 "1.2-1.5k" ///
7 "1.5-1.8k" 8 "1.8-2k"  9 "2-2.5k" 10 "2.5-3k" 11 "3-4k" 12 "4-6k" 13 "6-10k" 14 "10+k"
label values a_treve menage

replace a_treve=. if a_treve==98 | a_treve==99

gen h_income=.
replace h_income=0 if a_treve==0
replace h_income=0.2 if a_treve==1
replace h_income=0.5 if a_treve==2
replace h_income=0.7 if a_treve==3
replace h_income=0.9 if a_treve==4
replace h_income=1.1 if a_treve==5
replace h_income=1.35 if a_treve==6
replace h_income=1.65 if a_treve==7
replace h_income=1.9 if a_treve==8
replace h_income=2.25 if a_treve==9
replace h_income=2.75 if a_treve==10
replace h_income=3.5 if a_treve==11
replace h_income=5 if a_treve==12
replace h_income=8 if a_treve==13
replace h_income=10 if a_treve==14

replace h_income=((h_income)*12)/uc
label  var h_income "Yearly standardized income in k"

destring situae, replace
gen situpro=.
replace situpro=1 if situae==1
replace situpro=2 if situae==2
replace situpro=3 if situae==3
replace situpro=4 if situae==6
replace situpro=5 if situae==7
replace situpro=6 if situae==5
replace situpro=7 if situae==4
label define situation 1 "Working" 2 "Apprenticeship" 3 "Student" 4 "Homemaker" 5 "Other (handicapped, etc)" 6 "Retired" 7 "Unemployed"
label values situpro situation

// Here i recode those in apprenticeship as students. 

replace situpro=3 if situpro==2


// Unemployment: 2221 persons declared as being unemployed (not including students and retirees). Months of unemployment variable is nested with the years of unemployment, so years is what matters to compute long-term unemployment.

replace situpro=8 if situpro==7 & p_qdchoa>=1
label define situation1 1 "Working" 2 "Apprenticeship" 3 "Student" 4 "Homemaker" 5 "Other (handicapped, etc)" 6 "Retired" 7 "Unemployed" 8 "Long-term unemployed"
label values situpro situation1 

gen situpro2=.
replace situpro2=1 if situpro==1 | situpro==2 | situpro==3
replace situpro2=2 if situpro==4
replace situpro2=3 if situpro==5
replace situpro2=4 if situpro==6
replace situpro2=5 if situpro==7 | situpro==8

label define situation2 1 "Working or studying" 2 "Homemaker" 3 "Other (handicapped, etc)" 4 "Retired" 5 "Unemployed"
label values situpro2 situation2

destring couplee, replace
gen nmbadult=1
replace nmbadult=2 if couplee==1
gen nmbpeople=nmbadult+nenfants

gen ratiohabitat= nper/l_nbpiec
label var ratiohabitat "nmb of persons per room"

destring a_avis, replace
gen sub_hardship=.
replace sub_hardship=0 if a_avis==1 | a_avis==2
replace sub_hardship=1 if a_avis==3
replace sub_hardship=2 if a_avis==4
replace sub_hardship=3 if a_avis==5

gen sub_hardship_b=0 if a_avis<8
replace sub_hardship_b=1 if a_avis==4 | a_avis==5

// Growing up in a household having money issues:

destring t_argent, replace
gen young_hardship=0
replace young_hardship=1 if t_argent==1


// Diplome: some missing, 310 for Muslims. 

// Souffrance au travail: seulement pour les actifs.

destring p_sa1com, replace
destring p_sa2com, replace
destring p_sa3com, replace

gen work_suffer=0
replace work_suffer=1 if p_sa1com==1 | p_sa2com==1 | p_sa3com==1
label define suff 0 "No" 1 "Yes"
label values work_suffer suff 


// Parent's education:

destring diplop, replace
destring diplom, replace

gen f_dip_dad=.
replace f_dip_dad=. if diplop==99
replace f_dip_dad=1 if diplop==1 | diplop==11 | diplop==19 | diplop==15 | diplop==20
replace f_dip_dad=2 if diplop==12
replace f_dip_dad=3 if diplop==13 | diplop==14 | diplop==30
replace f_dip_dad=4 if diplop==4  | diplop==41 | diplop==42 | diplop==43 | diplop==49
replace f_dip_dad=5 if diplop==5  | diplop==51 | diplop==52 | diplop==53 | diplop==59
replace f_dip_dad=6 if diplop==60
replace f_dip_dad=7 if diplop==7  | diplop==71 | diplop==72 | diplop==73 | diplop==79 | diplop==8
replace f_dip_dad=8 if diplop==81 | diplop==82 | diplop==83 | diplop==84 | diplop==89
label values f_dip_dad diplome

label define diplome1 0 "Does not know" 1 "No degree" 2 "Primary school" 3 "Middle school" 4 "Vocational training" 5 "Technical high school" 6 "General high school" 7 "Some college" 8 "Bachelor or more"
label values f_dip_dad diplome1

gen f_dip_mum=.
replace f_dip_mum=. if diplom==99
replace f_dip_mum=1 if diplom==1 | diplom==11 | diplom==19 | diplom==15 | diplom==20
replace f_dip_mum=2 if diplom==12
replace f_dip_mum=3 if diplom==13 | diplom==14 | diplom==30
replace f_dip_mum=4 if diplom==4  | diplom==41 | diplom==42 | diplom==43 | diplom==49
replace f_dip_mum=5 if diplom==5  | diplom==51 | diplom==52 | diplom==53 | diplom==59
replace f_dip_mum=6 if diplom==60
replace f_dip_mum=7 if diplom==7  | diplom==71 | diplom==72 | diplom==73 | diplom==79 | diplom==8
replace f_dip_mum=8 if diplom==81 | diplom==82 | diplom==83 | diplom==84 | diplom==89
label values f_dip_mum diplome1

}

{ // Religious groups and religiosity measurements

destring relego1, replace
tab relego1

// Here I am generating another variable intended to preserve each group without mixing them up. 

gen relego2=relego1
replace relego2=20 if relego2==21
replace relego2=. if relego2==12 | relego2==14 | relego2==30 | relego2==41 | relego2==42 | relego2==43 | relego2==44  | relego2==88 | relego2==99 

label define relego 1 "No religion, atheist" 10 "Christian" 11 "Catholic" 13 "Protestant" 20 "Muslim"  40 "Buddhist" 
label values relego2 relego


/// Collapsing all Muslims in one religious category (there is a second one with very few respondents)

replace relego1   =20 if relego1==21
replace relego1=12 if relego1==13 | relego1==14
replace relego1=40 if relego1==41
replace relego1=42 if relego1==43 | relego1==99
replace relego1=. if relego1==44
drop if relego1   ==88 
drop if relego1==99
drop if relego1==42


label define relego3 1 "No religion, atheist" 10 "Christian" 11 "Catholic" 12 "Protestant, Orthodox" 20 "Muslim" 30 "Jewish" 40 "Buddhist, Sikh, Hinduist" 50 "Mixed" 42 "Don't know" 88 "Refuse to answer" 99 "Don't know"
label values relego1 relego3

// We thus have 5,706 Muslim respondents total, comprising 26.22% of the whole TeO sample. Once we remove respondents who has both parents born in France, we are left with 5,680 Muslim individuals.


tab r_impedu
tab r_culte
tab r_miam
tab r_ostent
tab regiontomb2
tab r_tombe
tab r_impvie
tab signerel1
tab signerel2
tab signerel3

destring r_impedu, replace
destring r_culte, replace
destring r_miam, replace
destring r_impvie, replace
destring r_ostent, replace
destring r_relsoi, replace


/// Recoding religiosity variables:


gen r_food=.
replace r_food=1 if r_miam==4
replace r_food=2 if r_miam==3
replace r_food=3 if r_miam==2
replace r_food=4 if r_miam==1
capture label define rmiam1 4 "Always" 3 "Sometimes" 2 "Never" 1 "There is none" 8 "Refuse to answer" 9 "Don't know"
label values r_food rmiam1
tab r_food

gen r_signe=.
replace r_signe=1 if r_ostent==4
replace r_signe=2 if r_ostent==3
replace r_signe=3 if r_ostent==2
replace r_signe=4 if r_ostent==1
capture label define r_ostent1 1 "Does not apply" 2 "Never" 3 "Sometimes" 4 "Always" 8 "Refuse to answer" 9 "Don't know"
label values r_signe r_ostent1
tab r_signe


capture label define r_importance 1 "Not important" 2 "Somewhat important" 3 "Quite important" 4 "Very important" 8 "Refuse to answer" 9 "Does not know"
label values r_impvie r_importance
label values r_impedu r_importance
replace r_impedu=. if r_impedu>4
label var r_impedu "Importance of religion in parental education"

//Aadding a religiosity measure where it gets "0" if r_impvie is missing due to the respondent declaring no religion.

gen r_attendance=.
replace r_attendance=1 if r_culte==5
replace r_attendance=2 if r_culte==4
replace r_attendance=3 if r_culte==3
replace r_attendance=4 if r_culte==2
replace r_attendance=5 if r_culte==1


capture label define rculte1 1 "At least once a week" 2 "Once or twice a month" 3 "For religious holidays" 4 "Only for important family events" 5 "Never"
label values r_culte rculte1

capture label define rattendance 1 "Never" 2 "Only for important family events" 3 "For religious holidays" 4 "Once or twice a month" 5 "At least once a week"
label values r_attendance rattendance

replace r_attendance=2 if r_attendance==3
replace r_attendance=3 if r_attendance==4
replace r_attendance=4 if r_attendance==5

// Last minute change in the religiosity measure (Sept. 30th, 2016) to make the scaling on the religiosity variables even/comparable:

replace r_signe=0 if r_signe==1 | r_signe==2
replace r_signe=2 if r_signe==3
replace r_signe=3 if r_signe==4
replace r_food=0 if r_food==1 | r_food==2
replace r_food=1 if r_food==3
replace r_food=2 if r_food==4

// Here I tried the whole analysis with 1-2 / and 2-3 and the results are the same. Muslims appear as slightly more heterodox in the 2-3 configuration. 

replace r_attendance=r_attendance-1
replace r_impvie=r_impvie-1

///// February 2019: as I revise, I realize my original index not well constructed. The 0-2-3 scale does not make much sense.
///// In addition, the different items have different scales. They all need to be on 0-1-2 scale, with a maximum of 8 in the combined scale so they have
///// equal weight. The downside of this is the need to recode religious attendance and subjective importance from 5 and 4-point scale
///// respectively to a 3-point scale. 

capture label define r_importance2 0 "Not important" 1 "Somewhat or quite important" 2 "Very important" 8 "Refuse to answer" 9 "Does not know"
capture label define r_attendance2 0 "Never" 1 "For specific occasions" 2 "Monthly or more" 8 "Refuse to answer" 9 "Does not know"
capture label define r_ostent2 0 "Never" 1 "Sometimes" 2 "Always" 8 "Refuse to answer" 9 "Don't know"
capture label define r_food2 0 "Never" 1 "Sometimes" 2 "Always" 8 "Refuse to answer" 9 "Don't know"

replace r_impvie=. if r_impvie>4
replace r_impvie=0 if r_relsoi==2 
replace r_impvie=1 if r_impvie==2
replace r_impvie=2 if r_impvie==3

replace r_attendance=. if r_culte>5
replace r_attendance=0 if r_relsoi==2
replace r_attendance=2 if r_attendance==3
replace r_food=. if r_miam>4
replace r_food=0 if r_relsoi==2
label values r_food r_food2
replace r_signe=. if r_ostent>4
replace r_signe=0 if r_relsoi==2
replace r_signe=1 if r_signe==2
replace r_signe=2 if r_signe==3
label values r_signe r_ostent2
label values r_impvie r_importance2
label values r_attendance r_attendance2



destring r_tombe, replace
label define sepulture 1 "France" 2 "French oversea territories" 3 "Another country" 4 "It does not matter" 9 "Don't know"
label values r_tombe sepulture
replace r_tombe=. if r_tombe==8

// Additive religiosity score:

capture drop religiositysum
gen religiositysum=.
replace religiositysum=(r_signe+r_food+r_attendance+r_impvie)

}

{ // Discrimination and identity


// Here is how the discrimination module works. 
// d_discri is for all respondents, and is a general question about the experience of discriminations in the last 5 years. 
// d_racism is for all respondents, and is a general question about the experience of discrimination throughout respondent's life.
// d_inegal is also general and asks about the respondent's general perception of discrimination in France. 
// d_racpot is about the likelihood of facing racism for those who said they've never encounetered it in d_racism. 

// The problem here is that the questions on discrimination in detail mix up people who faced and people who could potentially.

// Here we create specific variable measuring the experience of discrimination as perceived by respondent.

destring d_racism d_relig d_nom d_accent d_natio d_peau d_apphy, replace

gen xp_d_relig=0
gen xp_d_nom=0
gen xp_d_accent=0
gen xp_d_nat=0
gen xp_d_race=0
gen xp_d_phys=0

replace xp_d_relig=1 if d_racism==1 & d_relig==1
replace xp_d_nom=1 if d_racism==1 & d_nom==1
replace xp_d_accent=1 if d_racism==1 & d_accent==1
replace xp_d_nat=1 if d_racism==1 & d_natio==1
replace xp_d_race=1 if d_racism==1 & d_peau==1
replace xp_d_phys=1 if d_racism==1 & d_apphy==1

// Experience of discrimination at school, for those with schooling done in France - coded as 1 "Yes" if reporting perception of discrimination on ground of race, ethnicity, of way of dressing:

gen xp_d_school=0
replace xp_d_school=1 if f_dis_c==1 | f_dis_d==1 | f_dis_e==1

// Experience of discrimination in housing:

gen xp_d_housing=0
replace xp_d_housing=1 if l_dispq_c==1 | l_dispq_d==1 | l_dispq_c==1


// Unfair refusal of a job:
destring p_drech, replace
gen xp_unfair_job=0
replace xp_unfair_job=1 if p_drech==1

// I am recoding those who xp discrimination when searching for a job as all having had a job, even those who did not because otherwise htye will be excluded from interaction term "ever_worked*xp_unfair_job"

replace ever_worked=1 if xp_unfair_job==1

// Bad experience with institutions:
destring i_control d_poste d_mairie d_prefec p_drech, replace

gen xp_d_police=0
replace xp_d_police=1 if i_control==2

gen xp_d_post=0
replace xp_d_post=1 if d_poste==1

gen xp_d_townhall=0
replace xp_d_townhall=1 if d_mairie==1

gen xp_d_prefect=0
replace xp_d_prefect=1 if d_prefec==1

/// Here we generate the isntitutional discrimination dummy, adding also oeple who have been insulted or treated badly on racist grounds in hospitals, banks, police stations and administrations.
gen xp_d_instit=0
replace xp_d_instit=1 if xp_d_police==1 | xp_d_post==1 | xp_d_townhall==1 | xp_d_prefect==1 | d_ouraci_g==1 | d_ouraci_h==1 | d_ouraci_j==1 | d_ouraci_i==1
label define binary 0 "No" 1 "Yes"
label values xp_d_instit binary


// Creating a control for perception of discrimination.

gen d_perception=0
replace d_perception=1 if d_inegal==2
replace d_perception=2 if d_inegal==1

label define perception 0 "Low" 1 "Medium" 2 "High"
label values d_perception perception

destring a_rrelig, replace
replace a_rrelig=. if a_rrelig==8
gen r_homophily=.
replace r_homophily=1 if a_rrelig==1
replace r_homophily=2 if a_rrelig==3
replace r_homophily=3 if a_rrelig==2
replace r_homophily=4 if a_rrelig==9
label define r_homophily1 1 "More than half" 2 "About half" 3 "Less than half" 4"Don't know"
label values r_homophily r_homophily1 

gen r_homophily2=0
replace r_homophily2=1 if r_homophily==1

destring x_apparf, replace
label define x_apparf1 1 "Clearly agree" 2 "Somewhat agree" 3 "Don't really agree" 4 "Don't agree at all" 9 "Refuse to answer"
replace x_apparf=. if x_apparf==8
label values x_apparf x_apparf1

destring x_vufri, replace
replace x_vufri=. if x_vufri==8
label values x_vufri x_apparf1

destring x_moifr, replace
replace x_moifr=. if x_moifr==8
label values x_moifr x_apparf1

replace x_vufri=. if x_vufri==9
replace x_moifr=. if x_moifr==9

destring d_inegal, replace
replace d_inegal=. if d_inegal==8
label define discrimination 1 "Often" 2 "Sometimes" 3 "Never" 9 "Does not know"
label values d_inegal discrimination

destring d_discri, replace
replace d_discri=. if d_discri==8
label values d_discri discrimination

destring d_relig, replace
replace d_relig=2 if d_relig==.
replace d_relig=. if d_relig==8

label define disc_relig 1 "Yes" 2 "No" 9 "Don't know"
label values d_relig disc_relig

destring d_qdraci, replace
replace d_qdraci=2 if d_qdraci==.
replace d_qdraci=. if d_qdraci==9



destring i_cnfjus i_cnfpol i_cnfss i_ecole, replace


gen j_trust=.
replace j_trust=1 if i_cnfjus==4
replace j_trust=2 if i_cnfjus==3
replace j_trust=3 if i_cnfjus==2
replace j_trust=4 if i_cnfjus==1
label define institutional 1 "Not at all" 2 "Not really" 3 "Moderately" 4 "Completely"
label values j_trust institutional

gen p_trust=.
replace p_trust=1 if i_cnfpol==4
replace p_trust=2 if i_cnfpol==3
replace p_trust=3 if i_cnfpol==2
replace p_trust=4 if i_cnfpol==1
label values p_trust institutional

gen w_trust=.
replace w_trust=1 if i_cnfss==4
replace w_trust=2 if i_cnfss==3
replace w_trust=3 if i_cnfss==2
replace w_trust=4 if i_cnfss==1
label values w_trust institutional

gen s_trust=.
replace s_trust=1 if i_ecole==4
replace s_trust=2 if i_ecole==3
replace s_trust=3 if i_ecole==2
replace s_trust=4 if i_ecole==1
label values s_trust institutional

gen inst_trust=.
replace inst_trust= (j_trust+p_trust+w_trust+s_trust)-4

}

{// other generation and region/ethnic/religious origins variables

destring lieunaiser, replace
gen lieuborn=lieunaiser


replace lieunaiser=1 if lieunaiser==0

destring lieunaismr, replace
replace lieunaismr=1 if lieunaismr==0
destring lieunaispr, replace
replace lieunaispr=1 if lieunaispr==0




gen rel_gen=. // religion/generation categorical scheme

replace rel_gen=1 if relego1==1 & lieunaiser==2
replace rel_gen=2 if relego1==1 & lieunaiser==1 & ((lieunaismr==2 & lieunaispr==2) | (lieunaismr==2 | lieunaispr==2))
replace rel_gen=3 if relego1==11 & lieunaiser==2
replace rel_gen=4 if relego1==11 & lieunaiser==1 & ((lieunaismr==2 & lieunaispr==2) | (lieunaismr==2 | lieunaispr==2))
replace rel_gen=5 if relego1==20 & lieunaiser==2
replace rel_gen=6 if relego1==20 & lieunaiser==1 & ((lieunaismr==2 & lieunaispr==2) | (lieunaismr==2 | lieunaispr==2))
replace rel_gen=7 if lieunaiser==1 & lieunaismr==1 & lieunaispr==1

label define generation 1 "1st gen. no religion" 2 "2nd gen. no religion" 3 "1st gen. Catholic" 4 "2nd gen. Catholic" 5 "1st gen. Muslim" 6 "2nd gen. Muslim" 7 "Native"
label values rel_gen generation

gen generation=1 if rel_gen==1 | rel_gen==3 | rel_gen==5 
replace generation=2 if rel_gen==2 | rel_gen==4 | rel_gen==6

label define generation1 1 "1st generation (foreign born)" 2 "2nd generation (French born)"
label values generation generation1


// Geographic origin variables, with detail (regionnaise variables)
destring regionnaise, replace

label define region12 1 "France" 2 "Oversea territories" 3 "Western Europe" 4 "Southern Europe" 5 "Eastern Europe" 6 "Maghreb" 7 "Sahel" 8 "Central Africa" 9 "Turkey" 10 "Other in Middle East" 11 "Southeast Asia" 12 "South America" 13 "Other Africa" 14 "Other Asia" 15 "Other (North America, Oceania, other Europe)" 16 "Mixed"
gen region1=.
replace region1=1 if regionnaise==1101
replace region1=2 if lieuborn==1
replace region1=3 if regionnaise==4402 | regionnaise==4403 | regionnaise==4404 | regionnaise==4405 | regionnaise==4406 | regionnaise==4407 | regionnaise==4408 | regionnaise==4409 | regionnaise==4410 | regionnaise==4411 | regionnaise==4701
replace region1=4 if regionnaise==4101 | regionnaise==4201 | regionnaise==4301 | regionnaise==4401 | regionnaise==4502 | regionnaise==4507 
replace region1=5 if regionnaise==4501 | regionnaise==4503 | regionnaise==4504 | regionnaise==4505 | regionnaise==4506 | regionnaise==4508 | regionnaise==4509 | regionnaise==4510 | regionnaise==4511 | regionnaise==4512 
replace region1=6 if regionnaise==2101 | regionnaise==2201 | regionnaise==2301 
replace region1=7 if regionnaise==2401 | regionnaise==2402 | regionnaise==2403 | regionnaise==2404 | regionnaise==2405 | regionnaise==2406 | regionnaise==2407 | regionnaise==2408 | regionnaise==2409 
replace region1=8 if regionnaise==2501 | regionnaise==2502 | regionnaise==2503 | regionnaise==2504 | regionnaise==2505 | regionnaise==2506 | regionnaise==2507 | regionnaise==2508 | regionnaise==2509 | regionnaise==2510 | regionnaise==2511
replace region1=9 if regionnaise==3401
replace region1=10 if regionnaise==5106
replace region1=11 if regionnaise==3101 | regionnaise==3201 | regionnaise==3301 
replace region1=12 if regionnaise==5105 | regionnaise==5104
replace region1=13 if regionnaise==2601 
replace region1=14 if regionnaise==3501 
replace region1=15 if regionnaise==5103 | regionnaise==5107 | regionnaise==4801 | regionnaise==5102


gen region_1G=.
replace region_1G=1 if region1==6
replace region_1G=2 if region1==7
replace region_1G=3 if region1==9
replace region_1G=4 if region1==10
replace region_1G=5 if region1==8 | region1==13
replace region_1G=6 if region1==11 | region1==14
replace region_1G=7 if region1==1 | region1==2 | region1==3 | region1==4 | region1==5 | region1==12 | region1==15

label define region13 1 "Maghreb" 2 "Sahel" 3 "Turkey" 4 "Other in Middle East" 5 "Other in Subsaharan Africa" 6 "Asia" 7 "Other" 8 "Mixed"
label values region_1G region13

gen region_parents=. 
replace region_parents=1 if regionnaisp2==1101
replace region_parents=2 if regionnaisp2==1201 | regionnaisp2==1202 | regionnaisp2==1203 | regionnaisp2==1204 | regionnaisp2==1205
replace region_parents=3 if regionnaisp2==4402 | regionnaisp2==4403 | regionnaisp2==4404 | regionnaisp2==4405 | regionnaisp2==4406 | regionnaisp2==4407 | regionnaisp2==4408 | regionnaisp2==4409 | regionnaisp2==4410 | regionnaisp2==4411 | regionnaisp2==4701
replace region_parents=4 if regionnaisp2==4101 | regionnaisp2==4201 | regionnaisp2==4301 | regionnaisp2==4401 | regionnaisp2==4502 | regionnaisp2==4507 
replace region_parents=5 if regionnaisp2==4501 | regionnaisp2==4503 | regionnaisp2==4504 | regionnaisp2==4505 | regionnaisp2==4506 | regionnaisp2==4508 | regionnaisp2==4509 | regionnaisp2==4510 | regionnaisp2==4511 | regionnaisp2==4512 
replace region_parents=6 if regionnaisp2==2101 | regionnaisp2==2201 | regionnaisp2==2301 
replace region_parents=7 if regionnaisp2==2401 | regionnaisp2==2402 | regionnaisp2==2403 | regionnaisp2==2404 | regionnaisp2==2405 | regionnaisp2==2406 | regionnaisp2==2407 | regionnaisp2==2408 | regionnaisp2==2409 
replace region_parents=8 if regionnaisp2==2501 | regionnaisp2==2502 | regionnaisp2==2503 | regionnaisp2==2504 | regionnaisp2==2505 | regionnaisp2==2506 | regionnaisp2==2507 | regionnaisp2==2508 | regionnaisp2==2509 | regionnaisp2==2510 | regionnaisp2==2511
replace region_parents=9 if regionnaisp2==3401
replace region_parents=10 if regionnaisp2==5106
replace region_parents=11 if regionnaisp2==3101 | regionnaisp2==3201 | regionnaisp2==3301 
replace region_parents=12 if regionnaisp2==5105 | regionnaisp2==5104
replace region_parents=13 if regionnaisp2==2601 
replace region_parents=14 if regionnaisp2==3501 
replace region_parents=15 if regionnaisp2==5103 | regionnaisp2==5107 | regionnaisp2==4801 | regionnaisp2==5102
replace region_parents=16 if regionnaisp2!=regionnaism2


label values region1 region12
label values region_parents region12

gen region_parents2=.
replace region_parents2=1 if region_parents==6
replace region_parents2=2 if region_parents==7
replace region_parents2=3 if region_parents==9
replace region_parents2=4 if region_parents==10
replace region_parents2=5 if region_parents==8 | region_parents==13
replace region_parents2=6 if region_parents==11 | region_parents==14
replace region_parents2=7 if region_parents==1 | region_parents==2 | region_parents==3 | region_parents==4 | region_parents==5 | region_parents==12 | region_parents==15
replace region_parents2=8 if region_parents==16

label values region_parents2 region13

// New geographic variable across generations for Muslim respondents. 
gen regional_origin=.
replace regional_origin=region_1G if rel_gen==5
replace regional_origin=region_parents2 if rel_gen==6
label values regional_origin region13

// Getting rid of the "other in Middle East" category:

replace regional_origin=7 if regional_origin==4


// Count for original relego1: 
//6,291 are atheists. 
//2,006 Christians.
//6,578 are Catholic.
//245 Orthodox
//547 Protestant
//167 Jews
//579 Buddhists
//41 sikh

// label define generation2 1 "1G no religion n=1597" 2 "2G no religion n=2952" 3 "1G Christian n=355" 4 "2G Christian n=385" 5 "1G Catholic n=2183" 6 "2G Catholic n=2313 " ///
// 7 "1G Protestant n=334" 8 "2G Protestant n=118" 9 "1G Muslim 3308" 10 "2G Muslim n=2376" 11 "1G Buddhist n=418" 12 "2G Buddhist n=150"

gen rel_gen2=.

replace rel_gen2=1 if relego2==01 & lieunaiser==2
replace rel_gen2=2 if relego2==01 & lieunaiser==1 & ((lieunaismr==2 & lieunaispr==2) | (lieunaismr==2 | lieunaispr==2))
replace rel_gen2=3 if relego2==10 & lieunaiser==2
replace rel_gen2=4 if relego2==10 & lieunaiser==1 & ((lieunaismr==2 & lieunaispr==2) | (lieunaismr==2 | lieunaispr==2))
replace rel_gen2=5 if relego2==11 & lieunaiser==2
replace rel_gen2=6 if relego2==11 & lieunaiser==1 & ((lieunaismr==2 & lieunaispr==2) | (lieunaismr==2 | lieunaispr==2))
replace rel_gen2=7 if relego2==13 & lieunaiser==2
replace rel_gen2=8 if relego2==13 & lieunaiser==1 & ((lieunaismr==2 & lieunaispr==2) | (lieunaismr==2 | lieunaispr==2))
replace rel_gen2=9 if relego2==20 & lieunaiser==2
replace rel_gen2=10 if relego2==20 & lieunaiser==1 & ((lieunaismr==2 & lieunaispr==2) | (lieunaismr==2 | lieunaispr==2))

label define generation2 1 "1st gen no religion" 2 "2nd gen. no religion" 3 "1st gen. Christian" 4 "2nd gen. Christian" 5 "1st gen. Catholic" 6 "2nd gen. Catholic" ///
 7 "1st gen. Protestant" 8 "2nd gen. Protestant" 9 "1st gen. Muslim" 10 "2nd gen. Muslim" 11 "1st gen. Buddhist" 12 "2nd gen. Buddhist"

label values rel_gen2 generation2

gen edurel=.

replace edurel=1 if f_dip==1 & rel_gen==5 // Stratifiying Muslim respondents by education level
replace edurel=2 if f_dip==2 & rel_gen==5
replace edurel=3 if f_dip==3 & rel_gen==5
replace edurel=4 if f_dip==4 & rel_gen==5
replace edurel=5 if f_dip==5 & rel_gen==5
replace edurel=6 if f_dip==6 & rel_gen==5
replace edurel=7 if f_dip==7 & rel_gen==5
replace edurel=8 if f_dip==8 & rel_gen==5

label value edurel diplome

gen edurel2=.

replace edurel2=1 if f_dip==1 & rel_gen==6
replace edurel2=2 if f_dip==2 & rel_gen==6
replace edurel2=3 if f_dip==3 & rel_gen==6
replace edurel2=4 if f_dip==4 & rel_gen==6
replace edurel2=5 if f_dip==5 & rel_gen==6
replace edurel2=6 if f_dip==6 & rel_gen==6
replace edurel2=7 if f_dip==7 & rel_gen==6
replace edurel2=8 if f_dip==8 & rel_gen==6

label value edurel2 diplome

}

{// Other controls

destring cprop, replace
label values cprop binary
destring a_rvoisi, replace

replace a_rvoisi=. if a_rvoisi==9
replace a_rvoisi=0 if a_rvoisi==2

gen blocked_at_parents=.
replace blocked_at_parents=0 if cprop==1
replace blocked_at_parents=1 if l_tangpq_b==1
label values blocked_at_parents binary

gen waiting_to_marry=.
replace waiting_to_marry=0 if cprop==1
replace waiting_to_marry=1 if l_tangpq_d==1
label values waiting_to_marry binary

gen too_young=.
replace too_young=0 if cprop==1
replace too_young=1 if l_tangpq_a==1
label values too_young binary

gen no_housing=.
replace no_housing=0 if cprop==1
replace no_housing=1 if l_tangpq_c==1
label values no_housing binary

gen no_desire_to_leave=.
replace  no_desire_to_leave=0 if cprop==1
replace  no_desire_to_leave=1 if l_tangpq_e==1
label values no_desire_to_leave binary

gen other_reason_for_staying=.
replace other_reason_for_staying=0 if cprop==1
replace other_reason_for_staying=1 if l_tangpq_f==1
label values other_reason_for_staying binary

gen reason_parents=.
replace reason_parents=1 if l_tangpq_a==1
replace reason_parents=2 if l_tangpq_b==1
replace reason_parents=3 if l_tangpq_c==1
replace reason_parents=4 if l_tangpq_d==1
replace reason_parents=5 if l_tangpq_e==1
replace reason_parents=6 if l_tangpq_f==1

label define parents 1 "Too young" 2 "Not enough money" 3 "Have not found housing yet" 4 "Waiting to marry" 5 "Does not want to" 6 "Other"
label values reason_parents parents


// Non-family immigrant social closure:
destring f_immcol p_colleg l_immi, replace

// Middle school:
gen seg_school=0
replace seg_school=1 if f_immcol==1 | f_immcol==2

// Work:
gen seg_work=0
replace seg_work=1 if p_colleg==1 | p_colleg==2

// Neighborhood: 
gen seg_neigh=0
replace seg_neigh=1 if l_immi==1 | l_immi==2

// Parental control:
destring t_camour t_camis t_cloisir t_crelig, replace
gen argument_love=.
replace argument_love=1 if t_camour==3
replace argument_love=2 if t_camour==2
replace argument_love=3 if t_camour==4
replace argument_love=4 if t_camour==1

gen argument_friends=.
replace argument_friends=1 if t_camis==3
replace argument_friends=2 if t_camis==2
replace argument_friends=3 if t_camis==4
replace argument_friends=4 if t_camis==1

gen argument_leisure=.
replace argument_leisure=1 if t_cloisir==3
replace argument_leisure=2 if t_cloisir==2
replace argument_leisure=3 if t_cloisir==4
replace argument_leisure=4 if t_cloisir==1

gen argument_relig=.
replace argument_relig=1 if t_crelig==3
replace argument_relig=2 if t_crelig==2
replace argument_relig=3 if t_crelig==4
replace argument_relig=4 if t_crelig==1

label define argue 4 "Often" 2 "Rarely" 1 "Never" 3 "We avoided talking about it" 
label values argument_love argue
label values argument_friends argue
label values argument_leisure argue
label values argument_relig argue

destring relpere relmere, replace
replace relpere=20 if relpere==21
replace relpere=12 if relpere==13 | relpere==14
replace relpere=40 if relpere==41
replace relpere=42 if relpere==43 | relpere==99
replace relpere=. if relpere==44 | relpere==88 | relpere==99 | relpere==42

replace relmere=20 if relmere==21
replace relmere=12 if relmere==13 | relmere==14
replace relmere=40 if relmere==41
replace relmere=42 if relmere==43 | relmere==99
replace relmere=. if relmere==44 | relmere==88 | relmere==99 | relmere==42

gen rel_parents=relpere1
replace rel_parents=50 if relpere1!=relmere1
label values rel_parents relego3


label values relpere relego3
label values relmere relego3

gen non_muslim_parents=0 if relego1==20
replace non_muslim_parents=1 if relego1==20 & rel_parents!=20
gen mixed_rel_parents=0
replace mixed_rel_parents=1 if rel_parents==50


//Quality of the interview:

gen bad_interview=0
replace bad_interview=1 if q_pqmal_a==1 | q_pqmal_b==1 | q_pqmal_c==1

// Transnational ties

destring regionnew12 regionnew22 regionnew32 regioninv2 regionprop2 regionver12 regionver22 regionver32 m_vismj m_vispj, replace

gen trans_ties=0
replace trans_ties=1 if (regionnew12==regionnaise2 & regionnew12!=1101) | (regionnew12==regionnaism2 & regionnew12!=1101) | (regionnew12==regionnaisp2 & regionnew12!=1101)
gen trans_invest=0
replace trans_invest=1 if (regionprop2==regionnaise2 & regionprop2!=1101) | (regionprop2==regionnaism2 & regionprop2!=1101) | (regionprop2==regionnaisp2 & regionprop2!=1101) | (regioninv2==regionnaise2 & regioninv2!=1101) | (regioninv2==regionnaism2 & regioninv2!=1101) | (regioninv2==regionnaisp2 & regioninv2!=1101)
gen trans_remitt=0
replace trans_remitt=1 if (regionver12==regionnaise2 & regionver12!=1101) | (regionver12==regionnaism2 & regionver12!=1101) | (regionver12==regionnaisp2 & regionver12!=1101)
gen trans_visit_young=0
replace trans_visit_young=1 if m_vismj==1 |  m_vispj==1 

}

{ // Contextual variables

// Note for the contextual variable: you have census tract level (IRIS) which, when present, is a very fine geographical level. Also municipal and arrondissement level - which I am taking off the dataset for now (green bars) - I might use them when doing multilevel nalayses to see if things change according //
// how fine-grained to upper-levels in the models are. 
// I also realized that there is an error in coding for "communes irisables" I am correcting this here with the code below

destring a_rorig, replace
destring hc_act, replace
gen irisable=0
replace irisable=1 if hc_act==0

destring zfu zus, replace
capture drop urb
gen urb=0
replace urb=1 if zfu==1 | zus==1
label define urb1 0 "Not living in ZUS/ZFU" 1 "Living in ZUS/ZFU"
label values urb urb1

destring typvoisr, replace
label define housing 1 "Individual house (rural)" 2 "Individual house (urban)" 3 "Apartment building" 4 "Low-income housing project" 5 "Mixed housing"
label values typvoisr housing





// Create a dummy for two variables with important missingness. Then run a correlation matrix for missingness with all other variables. 

// Labelling contextual variables

label define socialhousing 2 "0%" 4 "0-1%" 5 "1-3%" 6 "3-5%" 7 "5-10%" 8 "10-25%" 9 "25+%"
label values dadactiris_pct_hlm socialhousing

label define percentmaghreb 1 "0%" 3 "0-2%" 4 "2-8%" 5 "8-13%" 6 "13-19%" 7 "19-27%" 8 "27-40%" 9 "40+%"
label values dadactiris_pct_imagh percentmaghreb

label define percentafrica 2 "0%" 4 "0-1%" 5 "1-4%" 6 "4-7%" 7 "7-11%" 8 "11-19%" 9 "19+%"
label values dadactiris_pct_imafs percentafrica

label define percentimm 0 "Less than 1%" 1 "1-1.5%" 2 "1.5-2%" 3 "2-2.5%" 4 "2.5-3.5%" 5 "3.5-4.5%" 6 "4.5-6%" 7 "6-8.5%" 8 "8.5-14%" 9 "14+%"
label values dadactiris_pct_immi percentimm

label define workers 0 "Less than 11%" 1 "11-15%" 2 "15-19%" 3 "19-22%" 4 "22-24%" 5 "24-27%" 6 "27-30%" 7 "30-33%" 8 "33-38%" 9 "38+%"
label values dadactiris_pct_act_ouvr workers

label define unemployment 0 "Less than 4%" 1 "4-6%" 2 "6-7%" 3 "7-8%" 4 "8-9%" 5 "9-10%" 6 "10-11%" 7 "11-13%" 8 "13-17%" 9 "17+%"
label values dadactiris_tx_chom15p unemployment

label define longterm 0 "Less than 20%" 1 "20-30%" 2 "30-35%" 3 "35-39%" 4 "39-42%" 5 "42-45%" 6 "45-49%" 7 "49-52%" 8 "52-60%" 9 "60+%"
label values dadactiris_tx_chompan longterm

label define recent 0 "Less than 12%" 1 "12-15%" 2 "15-18%" 3 "18-20%" 4 "20-22%" 5 "22-24%" 6 "24-27%" 7 "27-30%" 8 "30-35%" 9 "35+%"
label values dadactiris_pct_emrec recent

label define primary 0 "Less than 20%" 1 "20-24%" 2 "24-27%" 3 "27-29%" 4 "29-32%" 5 "32-35%" 6 "35-38%" 7 "38-42%" 8 "42-50%" 9 "50+%"
label values dadactiris_pct_dip_prim primary

label define family 0 "Less than 2%" 1 "2-3%" 2 "3-4%" 3 "4-5%" 4 "5-6%" 5 "6-6.5%" 6 "6.5-7%" 7 "7-8%" 8 "8-10%" 9 "10+%"
label values dadactiris_pct_men5p family

label define rfu 0 "<12" 1 "12-13" 2 "13-14" 3 "14-14.5" 4 "14.5-15" 5 "15-15.3" 6 "15.3-15.7" 7 "15.7-16" 8 "16-16.4" 9 "16.4-16.8" ///
10 "16.8-17.2" 11 "17.2-17.6" 12 "17.6-18" 13 "18-18.5" 14 "18.5-19" 15 "19-20" 16 "20-21" 17 "21-22" 18 "22-25" 19 ">25k"
label values vadactiris_rfu rfu


// Linearly re-scaling contextual variables in %:

//replace dadactiris_pct_imagh=0 if dadactiris_pct_imagh==1
//replace dadactiris_pct_imagh=1 if dadactiris_pct_imagh==3
//replace dadactiris_pct_imagh=5 if dadactiris_pct_imagh==4
//replace dadactiris_pct_imagh=10.5 if dadactiris_pct_imagh==5
//replace dadactiris_pct_imagh=16 if dadactiris_pct_imagh==6
//replace dadactiris_pct_imagh=23 if dadactiris_pct_imagh==7
//replace dadactiris_pct_imagh=33.5 if dadactiris_pct_imagh==8
//replace dadactiris_pct_imagh=48.5 if dadactiris_pct_imagh==9

//replace dadactiris_pct_imafs=0 if dadactiris_pct_imafs==1
//replace dadactiris_pct_imafs=0.5 if dadactiris_pct_imafs==4
//replace dadactiris_pct_imafs=5.5 if dadactiris_pct_imafs==6
//replace dadactiris_pct_imafs=9 if dadactiris_pct_imafs==7
//replace dadactiris_pct_imafs=15 if dadactiris_pct_imafs==8
//replace dadactiris_pct_imafs=26 if dadactiris_pct_imafs==9

//replace dadactiris_tx_chompan=10 if dadactiris_tx_chompan==0
//replace dadactiris_tx_chompan=25 if dadactiris_tx_chompan==1
//replace dadactiris_tx_chompan=32.5 if dadactiris_tx_chompan==2
//replace dadactiris_tx_chompan=37 if dadactiris_tx_chompan==3
//replace dadactiris_tx_chompan=40.5 if dadactiris_tx_chompan==4
//replace dadactiris_tx_chompan=43.5 if dadactiris_tx_chompan==5
//replace dadactiris_tx_chompan=47 if dadactiris_tx_chompan==6
//replace dadactiris_tx_chompan=50.5 if dadactiris_tx_chompan==7
//replace dadactiris_tx_chompan=56 if dadactiris_tx_chompan==8
//replace dadactiris_tx_chompan=61 if dadactiris_tx_chompan==9

//replace dadactiris_pct_dip_prim=10 if dadactiris_pct_dip_prim==0
//replace dadactiris_pct_dip_prim=22 if dadactiris_pct_dip_prim==1
//replace dadactiris_pct_dip_prim=25.5 if dadactiris_pct_dip_prim==2
//replace dadactiris_pct_dip_prim=28.5 if dadactiris_pct_dip_prim==3
//replace dadactiris_pct_dip_prim=30.5 if dadactiris_pct_dip_prim==4
//replace dadactiris_pct_dip_prim=33.5 if dadactiris_pct_dip_prim==5
//replace dadactiris_pct_dip_prim=36.5 if dadactiris_pct_dip_prim==6
//replace dadactiris_pct_dip_prim=40 if dadactiris_pct_dip_prim==7
//replace dadactiris_pct_dip_prim=46 if dadactiris_pct_dip_prim==8
//replace dadactiris_pct_dip_prim=56 if dadactiris_pct_dip_prim==9

// constructing a dummy for top decile in the context variables of interest:

gen t_d_neigh_maghreb=0
gen t_d_neigh_ss_africa=0
gen t_d_neigh_primary=0
gen t_d_neigh_long_term_unemp=0
gen t_d_neigh_social_h=0
gen t_d_neigh_families=0

replace t_d_neigh_maghreb=1 if dadactiris_pct_imagh==9
replace t_d_neigh_ss_africa=1 if dadactiris_pct_imafs==9
replace t_d_neigh_primary=1 if dadactiris_pct_dip_prim==9
replace t_d_neigh_long_term_unemp=1 if dadactiris_tx_chompan==9
replace t_d_neigh_social_h=1 if dadactiris_pct_hlm==9
replace t_d_neigh_families=1 if dadactiris_pct_men5p==9

}

}









