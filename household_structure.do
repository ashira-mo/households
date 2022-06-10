********************************************************************************
*         Household structure - size by age and sex 						   *
********************************************************************************

/*set working directory cd */

/* use Health and Demographic Surveillance Systems (HDSS) dataset as available through INDEPTH - https://www.indepth-ishare.org/index.php/catalog/181 */
use ConsolidatedData2018_analysis.dta, clear

clear
set maxvar 32767

/* 
*Note: Events are coded as:
1 ENU enumeration
2 BTH birth
3 IMG in-migration
4 OMG out-migration
5 EXT exit of household
6 ENT entry into hosuehold
7 DTH death
9 OBE end of observation
30 PER change in period at 1 January
*/

// some cleaning needed to work on the file with houeshold information

* include only HDSS sites with location (household) ID 
drop if LocationId==. // Malawi site has no LocationId

* generating unique location id (when using multiple countries)
cap drop socialgpid
egen socialgpid= concat(CountryId CentreId LocationId)  


// fix for in and out migrations on consecutive days (that could cause negative household size at later stage)

sort CountryId CentreId concat_IndividualId EventDate EventCode

*Transform dates in td format
foreach var of varlist  EventDate DoB{
gen double `var'_1 = dofc(`var')
format `var'_1 %td
drop `var'
rename `var'_1 `var'
}


//consistency of event records at the individual level

* first records of individual
capture drop firstrecord
sort CountryId CentreId concat_IndividualId EventDate EventCode
bysort CountryId CentreId concat_IndividualId (EventDate EventCode): gen firstrecord=_n==1

*for first events in HDSS that is ENT, convert to IMG
bys concat_IndividualId  : replace EventCode=3 if EventCode[1]==6

*Add an half day to the exit date (for cases where death is on same day as birth, or ext and ent on same day)
sort concat_IndividualId EventDate EventCode
by concat_IndividualId : gen b=1 if EventDate==EventDate[_n-1] 
replace EventDate=EventDate + 12*60*60*1000 if b==1

*check that first record isn't death, out-migration or exit
ta EventCode if firstrecord ==1 


// checking event consistency at the household level ///

* for cases where there are double ext (ext followed by ext- one in same hh as ent, drop the ext line that is in same hh as ent)
cap drop sameday
sort CountryId CentreId concat_IndividualId EventDate EventCode
gen sameday=1 if socialgpid[_n]==socialgpid[_n+1]  & EventCode[_n]==5 & EventCode[_n-1]==5 & EventCode[_n+1]==6 & EventDate[_n]==EventDate[_n+1] & concat_IndividualId[_n]== concat_IndividualId[_n-1] &  concat_IndividualId[_n]== concat_IndividualId[_n+1]
drop if sameday==1
drop sameday
*307 observations deleted

* for cases where there is ext and ent into different hh but following hh id are similar to those before ext ent pair - drop the ent ext lines
cap drop extent
sort CountryId CentreId concat_IndividualId EventDate EventCode
gen extent=1 if socialgpid[_n]==socialgpid[_n+1] & socialgpid[_n]!=socialgpid[_n-1] & socialgpid[_n-1]==socialgpid[_n+2]  & EventCode[_n]==5 & EventCode[_n+1]==6 & EventDate[_n]+1==EventDate[_n+1] & concat_IndividualId[_n]== concat_IndividualId[_n-1] &  concat_IndividualId[_n]== concat_IndividualId[_n+1] &  concat_IndividualId[_n]== concat_IndividualId[_n+2]
replace extent=1 if extent[_n-1]==1 & EventCode[_n]==6 & concat_IndividualId[_n]== concat_IndividualId[_n-1] & socialgpid[_n]==socialgpid[_n-1]
drop if extent==1
drop extent

*for cases where the ext ent pairs are proceeded by a per (eventcode 30), of same hh id as the pair, but different to previous and following records - drop 2 records, replace hhid for per
cap drop extentp
sort CountryId CentreId concat_IndividualId EventDate EventCode
gen extentp=1 if socialgpid[_n]==socialgpid[_n+1] & socialgpid[_n]==socialgpid[_n-1] & socialgpid[_n]!=socialgpid[_n-2] & socialgpid[_n-2]==socialgpid[_n+2]  & EventCode[_n]==5 & EventCode[_n+1]==6 & EventCode[_n-1]==30 & EventDate[_n]+1==EventDate[_n+1] & concat_IndividualId[_n]== concat_IndividualId[_n-1] &  concat_IndividualId[_n]== concat_IndividualId[_n+1] &  concat_IndividualId[_n]== concat_IndividualId[_n+2] &  concat_IndividualId[_n]== concat_IndividualId[_n-2]
replace extentp=1 if extentp[_n-1]==1 & EventCode[_n]==6 & concat_IndividualId[_n]== concat_IndividualId[_n-1] & socialgpid[_n]==socialgpid[_n-1]
replace extentp=2 if extentp[_n+1]==1 & EventCode[_n]==30 & concat_IndividualId[_n]== concat_IndividualId[_n+1] & socialgpid[_n]==socialgpid[_n+1]
drop if extentp==1
replace socialgpid=socialgpid[_n-1] if extentp==2
drop extentp


*for cases where there are ext then ent in same hh on consecutive days (and no change in hhid in ext-ent, or in events before or after) - drop ent ext pairs  
cap drop extent_hh
sort CountryId CentreId concat_IndividualId EventDate EventCode
gen extent_hh=1 if socialgpid[_n]==socialgpid[_n+1] & socialgpid[_n]==socialgpid[_n-1] & socialgpid[_n]==socialgpid[_n+2] & EventCode[_n]==5 & EventCode[_n+1]==6 & ((EventDate[_n]+1)==EventDate[_n+1] | (EventDate[_n])==EventDate[_n+1]) & concat_IndividualId[_n]== concat_IndividualId[_n-1] &  concat_IndividualId[_n]== concat_IndividualId[_n+1] &  concat_IndividualId[_n]== concat_IndividualId[_n+2]
replace extent_hh=1 if extent_hh[_n-1]==1 & EventCode[_n]==6 & concat_IndividualId[_n]== concat_IndividualId[_n-1] & socialgpid[_n]==socialgpid[_n-1]
drop if extent_hh==1
drop extent_hh

*for cases where EXT or OMG events following ENU are from different HH- change the hh id in ext/omg event
cap drop enuid
sort CountryId CentreId concat_IndividualId EventDate EventCode
gen enuid=1 if socialgpid[_n]!=socialgpid[_n+1] & EventCode[_n]==1 & (EventCode[_n+1]==4 | EventCode[_n+1]==5)  & concat_IndividualId[_n]== concat_IndividualId[_n+1]
replace socialgpid= socialgpid[_n+1] if enuid==1 & concat_IndividualId[_n]== concat_IndividualId[_n+1]
drop enuid

* first records in HH
capture drop firstrecordHH
sort CountryId CentreId socialgpid EventDate EventCode
bysort CountryId CentreId socialgpid (EventDate EventCode): gen firstrecordHH=_n==1

sort concat_IndividualId EventDate


// to have same OBE for all
sort EventDate
gen double last_obs = (_n == _N)
gen double last_record_date_1 = EventDate if last_obs==1
format last_record_date_1 %tc
sort last_record_date_1
replace last_record_date_1 = last_record_date_1[_n-1] if missing(last_record_date_1) & _n > 1 

display %20.0f date("01Jan 2017","DMY")
sort concat_IndividualId EventDate EventCode
expand=2 if concat_IndividualId!=concat_IndividualId[_n+1] & EventDate<last_record_date_1, gen(duplicate)

sort concat_IndividualId EventDate EventCode duplicate
by concat_IndividualId : replace EventDate=last_record_date_1  if duplicate==1
by concat_IndividualId : replace EventCode=9  if duplicate==1

drop duplicate

sort concat_IndividualId EventDate EventCode


*Transform dates back to tc format
foreach var of varlist  EventDate DoB{
gen double `var'_1 = cofd(`var')
format `var'_1 %tc
drop `var'
rename `var'_1 `var'
}


// splitting the data into desired age groups //

capture drop datebeg
bysort concat_IndividualId : gen  double datebeg=cond(_n==1,DoB,EventDate[_n-1])
format datebeg %tc

cap drop lastrecord
qui bys concat_IndividualId: gen lastrecord=_n==_N

* to split into age periods for each individual, first need to stset
stset EventDate , id(concat_IndividualId) failure(lastrecord==1) ///
		time0(datebeg) origin(time DoB) exit(time .)

cap drop censor_death
qui gen censor_death=(EventCode==7) 


capture drop group_age
display %20.0f (5*365.25*24*60*60*1000)+212000 /*5ans*/
*157788212000
display %20.0f (10*365.25*24*60*60*1000) /*10 ans*/
*315576000000
display %20.0f (15*365.25*24*60*60*1000) /*15 ans*/
*473364000000
display %20.0f (50*365.25*24*60*60*1000) /*50 ans*/
* 1577880000000
display %20.0f (65*365.25*24*60*60*1000) /*65 ans)*/
*2051244000000

stsplit group_age, at(0 157788212000 315576000000 473364000000 1577880000000 2051244000000) 

sort concat_IndividualId EventDate EventCode
drop lastrecord

recode group_age (0=0 "0-5ans") (157788212000=1 "5-10 yr") (315576000000=2 "10-15 yr") ///
                 (473364000000=3 "15-50 yr") (1577880000000=4 "50-65 yr") ///
				 (2051244000000=5 "65+ yr") (*=.), gen(group_age_bis)
replace group_age_bis=0 if EventCode==2 & group_age_bis==. 
// b/c birth not included in stset which starts at DoB, so need to add to correct age group

sort concat_IndividualId EventDate EventCode

***Correct the line with wrong value of EventCode (eventcode=21 indicates a change in age group) - b/c of the stsplit
sort  concat_IndividualId EventDate
forval i=1/9{
	bys  concat_IndividualId : replace EventCode=21 if EventCode==`i' & EventCode[_n+1]==`i' & (group_age!=group_age[_n+1])
}
 *shift in age groups is not important after out-mig or death so drop these lines
sort  concat_IndividualId EventDate
bys  concat_IndividualId : drop  if EventCode==21 & EventCode[_n-1]==4 
bys  concat_IndividualId : drop if EventCode==21 & EventCode[_n-1]==7
bys  concat_IndividualId : drop  if EventCode==21 & EventCode[_n-1]==4 
bys  concat_IndividualId : drop if EventCode==21 & EventCode[_n-1]==7

*shift in age groups is not important before joining hdss (enu/img) so drop these lines
*creating var taking date of enu
sort concat_IndividualId EventDate EventCode
gen enum=EventCode==1
replace enum=. if enum==0
gen Enum_date = EventDate if enum==1
sort  concat_IndividualId enum
bys  concat_IndividualId: replace Enum_date = Enum_date[_n-1] if missing(Enum_date) & _n > 1 
bys concat_IndividualId: replace Enum_date = Enum_date[_N] if missing(Enum_date)
count if Enum_date==.
format Enum_date %tc

bys  concat_IndividualId : replace EventCode=. if EventDate<Enum_date & Enum_date!=. & EventCode==21
drop if EventCode==.

*creating var taking date of img
sort concat_IndividualId EventDate EventCode
gen eimg=EventCode==3
replace eimg=. if eimg==0
gen Eimg_date = EventDate if eimg==1
sort  concat_IndividualId eimg
bys  concat_IndividualId: replace Eimg_date = Eimg_date[_n-1] if missing(Eimg_date) & _n > 1 
bys concat_IndividualId: replace Eimg_date = Eimg_date[_N] if missing(Eimg_date)
count if Eimg_date==.
format Eimg_date %tc

bys  concat_IndividualId : replace EventCode=. if EventDate<Eimg_date & Eimg_date!=. & EventCode==21
drop if EventCode==.


*recomputing censor variables afer split
cap drop censor_death
qui gen censor_death=(EventCode==7) 

capture drop datebeg
bysort concat_IndividualId : gen  double datebeg=cond(_n==1,DoB,EventDate[_n-1])
format datebeg %tc

capture drop firstrecordHH
sort CountryId CentreId socialgpid EventDate EventCode
bysort CountryId CentreId socialgpid (EventDate EventCode): gen firstrecordHH=_n==1
*not just for first of all records, but all events on first date recorded for hh
bys CountryId CentreId socialgpid (EventDate EventCode): gen firsts=1 if EventDate==EventDate[1]


********************************************************************************
*          Population Size
********************************************************************************

/// calculation of hh population (size) is based on population equation- taking events in minus events out
// The number of events is counted per date so that the HH composition in time-varying


rename Sex gender

** number of enumerations
sort socialgpid EventDate EventCode
gen enum=EventCode==1
replace out=. if out==0
capture drop total_enum
sort socialgpid EventDate concat_IndividualId
bys socialgpid (EventDate concat_IndividualId): gen temp=sum(enum==1)
egen total_enum = max(temp), by(socialgpid EventDate)
replace total_enum=0 if enum==1  //for enumeration at beginning we can replace all with 0 but not when other events!
drop temp
drop same


** number of out-migrations	  
sort socialgpid EventDate EventCode
gen out=EventCode==4
replace out=. if out==0

capture drop total_out
sort socialgpid EventDate concat_IndividualId
bys socialgpid (EventDate concat_IndividualId): gen temp=sum(out==1)
egen total_out = max(temp), by(socialgpid EventDate)
qui sum temp
local mx = r(max)
forval lmt=0/`mx'{
replace total_out=`lmt' if out==1 & temp[_n-1]==`lmt'
}
replace total_out=total_out[_n-1] if out==1 & out[_n-1]==1 &  EventDate[_n]==EventDate[_n-1] //for cases where event was on same day (out-migrated together)
replace total_out=0 if firsts==1 //first events in HH shouldn't be ext- instead of deleting these events/individuals/hhs, reset the ext to 0
drop temp


** number of exits from household (but remaining in site)
sort socialgpid EventDate EventCode
gen ext=EventCode==5
replace ext=. if ext==0

capture drop total_ext
sort socialgpid EventDate concat_IndividualId
bys socialgpid (EventDate concat_IndividualId): gen temp=sum(ext==1)
egen total_ext = max(temp), by(socialgpid EventDate)
qui sum temp
local mx = r(max)
forval lmt=0/`mx'{
replace total_ext=`lmt' if ext==1 & temp[_n-1]==`lmt'
}
replace total_ext=total_ext[_n-1] if ext==1 & ext[_n-1]==1 &  EventDate[_n]==EventDate[_n-1] //for cases where event was on same day 
replace total_ext=0 if firsts==1 //first events in HH shouldn't be ext- instead of deleting these events/individuals/hhs, reset the ext to 0
drop temp

***Nombre de personnes déménagées dans le ménage
		  
sort socialgpid EventDate EventCode
gen ent=(EventCode==6)
replace ent=. if ent==0

capture drop total_ent
sort socialgpid EventDate concat_IndividualId
bys socialgpid (EventDate concat_IndividualId): gen temp=sum(ent==1)
egen total_ent = max(temp), by(socialgpid EventDate)
qui sum temp
local mx = r(max)
forval lmt=0/`mx'{
replace total_ent=`lmt' if ent==1 & temp[_n-1]==`lmt'
}
replace total_ent=total_ent[_n-1] if ent==1 & ent[_n-1]==1 &  EventDate[_n]==EventDate[_n-1] //for cases where event was on same day 
drop temp


**number of immigrations		  
sort socialgpid EventDate EventCode
gen inm=EventCode==3
replace inm=. if inm==0

capture drop total_inm
sort socialgpid EventDate concat_IndividualId
bys socialgpid (EventDate concat_IndividualId): gen temp=sum(inm==1)
egen total_inm = max(temp), by(socialgpid EventDate)
qui sum temp
local mx = r(max)
forval lmt=0/`mx'{
replace total_inm=`lmt' if inm==1 & temp[_n-1]==`lmt' // to remove from the count the events themselves so that the hh size represents size before event
}
replace total_inm=0 if inm==1 & socialgpid[_n]!=socialgpid[_n-1] // for cases where this is first record (possible only with in-mig or enu)
sort socialgpid EventDate concat_IndividualId
bys socialgpid: replace total_inm=total_inm[_n-1] if inm==1 & inm[_n-1]==1 &  EventDate[_n]==EventDate[_n-1] & concat_IndividualId[_n]!= concat_IndividualId[_n-1]  //for cases where event was on same day (in-migrated together)
drop temp



**number of births		  
sort socialgpid EventDate EventCode
gen bth=EventCode==2
replace bth=. if bth==0

capture drop total_bth
sort socialgpid EventDate concat_IndividualId
bys socialgpid (EventDate concat_IndividualId): gen temp=sum(bth==1)
egen total_bth = max(temp), by(socialgpid EventDate)
qui sum temp
local mx = r(max)
forval lmt=0/`mx'{
replace total_bth=`lmt' if bth==1 & temp[_n-1]==`lmt'
}
replace total_bth=total_bth[_n-1] if bth==1 & bth[_n-1]==1 &  EventDate[_n]==EventDate[_n-1] & concat_IndividualId[_n]!= concat_IndividualId[_n-1] //for cases where event was on same day (born on same day -twins)
drop temp

**number of deaths 
sort socialgpid EventDate EventCode
gen dth=EventCode==7
replace dth=. if dth==0

capture drop total_dth
sort socialgpid EventDate concat_IndividualId
bys socialgpid (EventDate concat_IndividualId): gen temp=sum(dth==1)
egen total_dth = max(temp), by(socialgpid EventDate)
qui sum temp
local mx = r(max)
forval lmt=0/`mx'{
replace total_dth=`lmt' if dth==1 & temp[_n-1]==`lmt'
}
replace total_dth=total_dth[_n-1] if dth==1 & dth[_n-1]==1 &  EventDate[_n]==EventDate[_n-1] & concat_IndividualId[_n]!= concat_IndividualId[_n-1]  //for cases where event was on same day 
drop temp


*Population (household) size  : number enumerated + number of births + number of immigrants - number of emigrants - number of deaths
gen hh_size = total_enum + total_bth + total_inm + total_ent - total_out - total_dth - total_ext  

save consolidated_hh.dta, replace


	
*******************************************************************************************************
*	Repeated procedure but by age-sex groups to have number of members in household in specific groups
*******************************************************************************************************

//Note: Done in small loops here so as not to be too heavy with large dataset 

forval ag=0/5 {
forval sex=1/2 {

*ENU
sort socialgpid EventDate EventCode
gen x`sex'age`ag'_1=EventCode==1 & group_age_bis==`ag' & gender==`sex'
replace x`sex'age`ag'_1=. if x`sex'age`ag'_1==0

sort socialgpid EventDate concat_IndividualId
bys socialgpid (EventDate concat_IndividualId): gen temp_x`sex'age`ag'=sum(x`sex'age`ag'_1==1)
egen total_enum_x`sex'age`ag' = max(temp_x`sex'age`ag'), by(socialgpid EventDate)
egen same=sum(x`sex'age`ag'_1), by(socialgpid EventDate)
replace total_enum_x`sex'age`ag'=0 if same>0 // can only be the case if these are first events of household (like enu)
drop temp_x`sex'age`ag'
drop same 
	 }
 }

forval ag=0/5 {
forval sex=1/2 {
 
*IMG
sort socialgpid EventDate EventCode
gen x`sex'age`ag'_3=EventCode==3 & group_age_bis==`ag' & gender==`sex'
replace x`sex'age`ag'_3=. if x`sex'age`ag'_3==0

sort socialgpid EventDate concat_IndividualId
bys socialgpid (EventDate concat_IndividualId): gen temp_x`sex'age`ag'=sum(x`sex'age`ag'_3==1)
egen total_inm_x`sex'age`ag' = max(temp_x`sex'age`ag'), by(socialgpid EventDate)

egen same=sum(x`sex'age`ag'_3), by(socialgpid EventDate)
gen switch= 1 if same>0 & EventDate!=EventDate[_n-1] & same[_n-1]!=0

replace total_inm_x`sex'age`ag'=0 if /*same>0 &*/ (temp_x`sex'age`ag'==0 | socialgpid!=socialgpid[_n-1] ) // for first img events, otherwise it needs to pick up the previous total size (next line)
replace total_inm_x`sex'age`ag'=total_inm_x`sex'age`ag'[_n-1] if same>0 & socialgpid==socialgpid[_n-1] & switch==1
replace total_inm_x`sex'age`ag'=total_inm_x`sex'age`ag'[_n-1]  if same>0 & socialgpid==socialgpid[_n-1] & switch!=1

drop temp_x`sex'age`ag'
drop same switch
	 }
 }
save hh_structure_step1.dta, replace 
 
forval ag=0/5 {
forval sex=1/2 {

*ENT
sort socialgpid EventDate EventCode
gen x`sex'age`ag'_6=EventCode==6 & group_age_bis==`ag' & gender==`sex'
replace x`sex'age`ag'_6=. if x`sex'age`ag'_6==0

sort socialgpid EventDate concat_IndividualId
bys socialgpid (EventDate concat_IndividualId): gen temp_x`sex'age`ag'=sum(x`sex'age`ag'_6==1)
egen total_ent_x`sex'age`ag' = max(temp_x`sex'age`ag'), by(socialgpid EventDate)
egen same=sum(x`sex'age`ag'_6), by(socialgpid EventDate)
gen switch= 1 if same>0 & EventDate!=EventDate[_n-1] & same[_n-1]!=0

replace total_ent_x`sex'age`ag'=0 if (temp_x`sex'age`ag'==0 | socialgpid!=socialgpid[_n-1] ) // for first ent events, otherwise it needs to pick up the previous total size (next line)
replace total_ent_x`sex'age`ag'=total_ent_x`sex'age`ag'[_n-1] if same>0 & socialgpid==socialgpid[_n-1] & switch==1
replace total_ent_x`sex'age`ag'=total_ent_x`sex'age`ag'[_n-1] if same>0 & socialgpid==socialgpid[_n-1] & switch!=1

drop temp_x`sex'age`ag'
drop same switch
	 }
 }
 

forval ag=0/5 {
forval sex=1/2 {

*OMG

sort socialgpid EventDate EventCode
gen x`sex'age`ag'_4=EventCode==4 & group_age_bis==`ag' & gender==`sex'
replace x`sex'age`ag'_4=. if x`sex'age`ag'_4==0

sort socialgpid EventDate concat_IndividualId
bys socialgpid (EventDate concat_IndividualId): gen temp_x`sex'age`ag'=sum(x`sex'age`ag'_4==1)
egen total_omg_x`sex'age`ag' = max(temp_x`sex'age`ag'), by(socialgpid EventDate)
egen same=sum(x`sex'age`ag'_4), by(socialgpid EventDate)
 
replace total_omg_x`sex'age`ag'=total_omg_x`sex'age`ag'[_n-1] if same>1 & socialgpid==socialgpid[_n-1] 
replace total_omg_x`sex'age`ag'=total_omg_x`sex'age`ag' -1 if same==1 & socialgpid==socialgpid[_n-1] 
drop temp_x`sex'age`ag'
drop same 

	 }
 }

forval ag=0/5 {
forval sex=1/2 {
*EXT
sort socialgpid EventDate EventCode
gen x`sex'age`ag'_5=EventCode==5 & group_age_bis==`ag' & gender==`sex'
replace x`sex'age`ag'_5=. if x`sex'age`ag'_5==0

sort socialgpid EventDate concat_IndividualId
bys socialgpid (EventDate concat_IndividualId): gen temp_x`sex'age`ag'=sum(x`sex'age`ag'_5==1)
egen total_ext_x`sex'age`ag' = max(temp_x`sex'age`ag'), by(socialgpid EventDate)
egen same=sum(x`sex'age`ag'_5), by(socialgpid EventDate)

replace total_ext_x`sex'age`ag'=total_ext_x`sex'age`ag'[_n-1] if same>1 & socialgpid==socialgpid[_n-1] 
replace total_ext_x`sex'age`ag'=total_ext_x`sex'age`ag' -1 if same==1 & socialgpid==socialgpid[_n-1] 
drop temp_x`sex'age`ag'
drop same 
	 }
 }
 
 
forval ag=0/5 {
forval sex=1/2 {
*DTH

sort socialgpid EventDate EventCode
gen x`sex'age`ag'_7=EventCode==7 & group_age_bis==`ag' & gender==`sex'
replace x`sex'age`ag'_7=. if x`sex'age`ag'_7==0

sort socialgpid EventDate concat_IndividualId
bys socialgpid (EventDate concat_IndividualId): gen temp_x`sex'age`ag'=sum(x`sex'age`ag'_7==1)
egen total_dth_x`sex'age`ag' = max(temp_x`sex'age`ag'), by(socialgpid EventDate)
egen same=sum(x`sex'age`ag'_7), by(socialgpid EventDate)

replace total_dth_x`sex'age`ag'=total_dth_x`sex'age`ag'[_n-1] if same>1 & socialgpid==socialgpid[_n-1] //deaths on same day in hh 
replace total_dth_x`sex'age`ag'=total_dth_x`sex'age`ag' -1 if same==1

drop temp_x`sex'age`ag'
drop same 
	 }
 }

	 
*BTH	
* For under-5s also need to include births
forval sex=1/2 {
sort socialgpid EventDate EventCode
gen bth_child_x`sex'=EventCode==2 & group_age_bis==0 & gender==`sex'
replace bth_child_x`sex'=. if bth_child_x`sex'==0

sort socialgpid EventDate concat_IndividualId
bys socialgpid (EventDate concat_IndividualId): gen temp_x`sex'=sum(bth_child_x`sex'==1)
egen total_bth_x`sex' = max(temp_x`sex'), by(socialgpid EventDate)

egen same=sum(bth_child_x`sex'), by(socialgpid EventDate)
replace total_bth_x`sex'=total_bth_x`sex'[_n-1] if same>1 & socialgpid==socialgpid[_n-1] //twins
replace total_bth_x`sex'=total_bth_x`sex' -1 if same==1
drop temp_x`sex'
drop same
	}
	
drop x*	bth_child_x*


*** Population (number of household members) in each age group and by sex 

forval ag=1/5 {
forval sex=1/2 {

gen hh_x`sex'age`ag'_numb = total_enum_x`sex'age`ag' + total_inm_x`sex'age`ag'  + total_ent_x`sex'age`ag' /// 
- total_omg_x`sex'age`ag' - total_ext_x`sex'age`ag' - total_dth_x`sex'age`ag'

}
}

*for first age group (0-5) - include births
forval sex=1/2 {
gen hh_x`sex'age0_numb = total_enum_x`sex'age0  + total_bth_x`sex' + total_inm_x`sex'age0  + total_ent_x`sex'age0 /// 
- total_dth_x`sex'age0 - total_omg_x`sex'age0  - total_ext_x`sex'age0
}

**for whole household
*checking if the sum of the age group household membership is the same as the total size
egen hh_all = rowtotal(hh_x*)
sum hh_all
corr hh_all hh_size   

cap drop hh_males hh_females
egen hh_males = rowtotal(hh_x1*)
egen hh_females = rowtotal(hh_x2*)

sum hh_males hh_females

save hh_structure_final.dta, replace



