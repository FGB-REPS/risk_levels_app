# Data Risk Classification App for the VU Faculty of Behavioural and Movement Sciences

This **risk levels app** is designed to help researchers and research support staff working within the disciplines represented by the VU Faculty of Behavioural and Movements Sciences (FGB). 
The app is built using R Shiny and provides the user with insight into the **privacy risks** posed by the data they plan to use in their research. 
The risk levels are based on three primary concepts:

* **Re-identifiability risks**: the ease with which research participants could be re-identified from the data in use
* **Vulnerability risks**: the vulnerability of the research participant population, primarily based on their status in society or level of marginalization
* **Increased risk of harm**: whether the information within the data contribute additional risks to the participant population

The types of data and participants are based on those most commonly utilized by FGB research staff, allowing users to tailor the guidance and insights they receive to their specific research project.
Once users have completed the necessary response fields, they will receive a privacy risk categorization and specific guidance on how they should approve different data processing activities based on this categorization.

---------

## How to use

The primary end user should review the "How to use this tool" tab on the [application](https://risk_levels_app.rshiny.labs.vu.nl/).

Users wishing to modify and implement this app for their own purposes can fork this repository and adjust the contents as needed. To modify the app for another discipline the user should begin with the contents of:

* data/
* mdFiles/

The risk categorizations are ultimately defined in the server section of app.R. A new user should assess these categorizations and modify as needed to suit their own use-case. 
  
  
----------

## Application structure

* app.R - primary application R code
* helper_fxs.R - source code for functions developed for the app
* data/
  * rskData.RData - source data for calculating risk levels
* mdFiles/
  * addGuidance.Rmd - content for *Additional Guidance* tab
  * archivingBLUE.Rmd - content on archiving when risk categorization is blue
  * archivingGREEN.Rmd - content on archiving when risk categorization is green
  * archivingORANGE.Rmd - content on archiving when risk categorization is orange
  * archivingRED.Rmd - content on archiving when risk categorization is red
  * archivingYELLOW.Rmd - content on archiving when risk categorization is yellow
  * genRec.Rmd - general recommendations at top of *Risk level recommendations* tab regardless of risk categorization
  * instructions.Rmd - content for *How to use this tool* tab
  * reuseBLUE.Rmd - content on reuse when risk categorization is blue
  * reuseGREEN.Rmd - content on reuse when risk categorization is green
  * reuseORANGE.Rmd - content on reuse when risk categorization is orange
  * reuseRED.Rmd - content on reuse when risk categorization is red
  * reuseYELLOW.Rmd - content on reuse when risk categorization is yellow
  * storageBLUE.Rmd - content on storage when risk categorization is blue
  * storageGREEN.Rmd - content on storage when risk categorization is green
  * storageORANGE.Rmd - content on storage when risk categorization is orange
  * storageRED.Rmd - content on storage when risk categorization is red
  * storageYELLOW.Rmd - content on storage when risk categorization is yellow
  * studentsBLUE.Rmd - content on student use of data when risk categorization is blue
  * studentsGREEN.Rmd - content on student use of data when risk categorization is green
  * studentsORANGE.Rmd - content on student use of data when risk categorization is orange
  * studentsRED.Rmd - content on student use of data when risk categorization is red
  * studentsYELLOW.Rmd - content on student use of data when risk categorization is yellow
  * transferBLUE.Rmd - content on digital transfer of data when risk categorization is blue
  * transferGREEN.Rmd - content on digital transfer of data when risk categorization is green
  * transferORANGE.Rmd - content on digital transfer of data when risk categorization is orange
  * transferRED.Rmd - content on digital transfer of data when risk categorization is red
  * transferYELLOW.Rmd - content on digital transfer of data when risk categorization is yellow
* www/
  * styles.css - css file for additional styling beyond was is provided by bootstrap via bslib
  
----------

Creator:

* [Jessica Hrudey](https://github.com/jhrudey)

Maintainer(s):

* [Jessica Hrudey](https://github.com/jhrudey)
  
