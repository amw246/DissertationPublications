# Dissertation 

---

## Overview

My dissertation consisted of five chapters:

1. Introduction
2. Student Outcomes
3. Sequence Analysis
4. Graduation
5. Conclustion

Of the chapters, the three middle ones represent the best chance of publication
in journals or on the web. Ideally, I'd like something that could be published 
in a reasonable journal as well as put on the internet using knitR or some such
tool. 

Additionally, I'd like to bring in some nationally-representative data (ideally
that is publically available). The Post Baccalaureate Students Longitudinal 
Study (BPS), might be a good source of data. 

## Next Steps

Regardless of which chapter I choose to pull the analysis from, I need to go 
through my dissertation and figure out where the syntax that created each 
analysis came from. An important step will be recreating the underlying data 
file from scratch (to make sure that I know and am still okay with all of the
choices that went into creating that data set). 

Following that, I'd like to get the syntax organized and up onto Github so 
that I have a central record of it. Then I want to try recreating ALL of the 
analyses in R (i.e. bring it out of Stata). 

### Data Set Documentation

The data set I eventually used for the dissertation was a subset of the Policy
Tracking Cohort (PTC). Looking at the data section of my dissertation, on page
12 I indicate that the final total N is 125,515. 

Working backward from this, the following groups were dropped:
1. Native American Students
2. Those seeking certificate degrees at entry
3. Those seeking certificate degrees at exit

Further work will need to be done to include the semester-based state variables,
the Clearinghouse transfer data (maybe?), and to reshape the data from wide to 
long for the survival analysis (and possibly TraMineR as well). 

So, working from the dissertation, and the de-identified data prepared for my 
dissertation, I load three Stata file
+ 20131125_Wallace_f99-s00.dta
+ 20131125_Wallace_f00-s02.dta
+ 20131125_Wallace_f02-s04.dta

This gets me 172,067 records. Then I drop those records from fall 2003, spring 
2003, and spring 2004. This brings me to 127,144 records. Dropping the 
certificate students (972) gets me to 126,172. 

It would appear that in December, 2013 I did a special extract to get degree 
data by semester for the PTC. The SQL and data file is in the following 
location: 
O:\!Policy\Projects\Policy Tracking Cohort\Special Extracts\Degree By SEM

In the syntax (formerly 20131203_Create_Data_set.dta), there was syntax
to bring in NSC data for coding transfer students. I have recreated this
using the appropriate locations and data on CUNY computers. The problem is that
I am still at 126k. I need to get down to the 125k indicated in the 
dissertation.

Ahh. I think I may be able to get to the right number without the original 
syntax. If we drop those who pursued a certificate as their last degree, we get
rid of 454 more (leaving 125,178). It's important to note here that the last
degree variable I used to drop these students () is not the same as the original
version of the variable. It excludes those who transferred to a 2 or 4 year 
college in the NSC data (there are 49 of these students). 

If we then exclude the Native American students (203), we get to the N presented
in the dissertation: 125,515. 

Note that I saved this in Stata13 and ran into issues reading it into R. I used 
saveold to save it as a Stata12 file, opened it up in Stata11 and saveold'ed it
to Stata9. The name of the file is now 20151120_PTC_Stata9.dta. 

Now, having backed into the original number and read it into R, I would like to 
compare some descriptives to what I get in Stata to make sure there aren't any
differences. 

Variables to describe:
entry.date

Hmm. Is there a way to automate this? I can either look at the models to pick 
out certain variables to compare or compare all 87 variables in the data set. 
I also need to make sure eventually that I didn't add any variables later to 
the analysis that aren't in the data set currently. 




### Catalogue all analyses and find underlying syntax

### Bring data into R and QA to make sure it worked

### Recreate analyses in R