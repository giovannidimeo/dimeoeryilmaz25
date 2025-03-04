*____________________________________________________________________________________________________*
* The Impacts of Health Shocks on Household Labor Supply and Domestic Production
* Di Meo & Eryilmaz, 2025	 	 	 	 	 	 	 	 	 	   
*----------------------------------------------------------------------------------------------------*

*____________________________________________________________________________________________________*
* SETUP	 	 	 	 	 	 	 	 	 	 	   
*----------------------------------------------------------------------------------------------------*
version 16 
set more off 
clear all  
drop _all  
capture clear matrix 
capture log close   
set maxvar 25000  

 *____________________________________________________________________________________________________*
 * DEFINE PATHS 	 	 	 	 	 	 	 	 	 	 	   
 *----------------------------------------------------------------------------------------------------*
* IMPORTANT: 
di in red "You need to manually define the directory for the saved datasets the python code (line 75)"
di in red "This code takes a long time! (For us, it was nearly 10 hours)"
di in red "Ignore exit to continue"

exit


*----------------------------------------------------------------------------------------------------*

cd $saved
use pers_merged, clear

cap drop hpmax

* For now, take only couples with both partners in the data
keep if partner_pid > 0 & !mi(partner_pid) 

* For now, take only couples living together
*drop partner_indicator
*rename partner partner_indicator
keep if (partner_indicator == 1 | partner_indicator == 2)

* REMOVE VALUE LABELS FOR PYTHON SCRIPT, EXPORT FOR LATER USE
cd $work 
label save using c_labels.do, replace
label drop _all

* GENERATE GROUP-IDs
gsort pid partner_pid
gegen couple_id = group(pid partner_pid)

* Reduce dataframe for Python
keep hid pid syear partner_pid couple_id

cd $saved
compress
save pers_merged_junk.dta, replace
*----------------------------------------------------------------------------------------------------*

*____________________________________________________________________________________________________*
* PYTHON 	 	 	 	 	 	 	 	 	 	 	   
*----------------------------------------------------------------------------------------------------*


timer on 1

* Run python code
python

import os 
import pandas as pd
import numpy as np

os.chdir(r'C:\Users\gdimeo\polybox\Shared\16_DiMeo_Eryilmaz\giovanni\codes\stata\submission\datasets') 

df = pd.read_stata('pers_merged_junk.dta')

df = df.sort_values(['hid','pid', 'syear'],
              ascending = [True, True, True])

df = df.reset_index()    

df = df.drop(df.columns[0], axis=1)   
                               
df['nr'] = df.index

original_length = len(df)

df2 = df.merge(df, how='left', left_on = ['hid','syear','partner_pid'], 
               right_on = ['hid','syear','pid'], suffixes=('0', '1'), 
               indicator = True, validate = "1:1")

del df

df2.to_stata('pers_merged_junk2.dta')

df2 = df2[df2._merge == "both"]

df2 = df2.drop(columns=['_merge'])
 
df2['mark'] = 0

df2 = df2[['pid0', 'pid1', 'syear', 'nr1', 'mark', 'couple_id0']]

for n in range(0,original_length):
    print(n)
    try: 
        t = int(df2['nr1'][n]) #8
        df2.loc[t, 'mark'] = 1 
        df2['mark'] = df2.groupby(['pid0', 'pid1'])['mark'].transform(max)
        df2 = df2.loc[df2.mark != 1,]
        df2 = df2[df2['pid0'].notna()]
    except (KeyError, ValueError):
        pass


df2.to_stata('pers_merged_junk3.dta')

end


timer off 1
timer list 1 

exit
