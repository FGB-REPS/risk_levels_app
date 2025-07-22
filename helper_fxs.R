# helper functions for support app.R to operate properly

# typeINXfx - creates a subset of the selected dataframe (determined by the user) that only includes the variables
#  ExcepToNoINX, INXincrDir, INXincrHRindir, INXincrLRindir and INXdecrLRindir
# this step is necessary to carry out adjustINX (the following function) because adjustINX will iterate through
# the same steps for each of the INX variables

typeINXfx <- function(df) {
  typeINXdf <- df[stringr::str_detect(names(df), "INX")][-stringr::str_detect(names(df), "NoINX")]
}

# adjustINX - function to identify if there is a combination of risk variables that alter the normal risk level calculation
# e.g. if a VarName has other VarNames listed under ExcepToNoINX, INXincrDir, INXincrHRindir, INXincrLRindir or INXdecrLRindir
# *and* one of those VarNames listed is also in the included subset, then the combination between the two VarNames will be different
# depending the type of interaction. Basically need to identify if a modifying interaction between VarNames is
# present in the dataset and then adjust the type of identifier (DirectID, HRIndirID, LRIndirID, NoINX) that the VarName is so that the 
# in the calculation step (lvlCalcFx), the total calculation will be modified as needed
# Essentially this process accounts for certain VarNames interacting more or less strongly with each other than other VarName types.

# create vector of variable names for resultINX to use

resultINX <- c("resYesINX", "resIncrDir", "resIncrHR", "resIncrLR", "resDecrLR")

adjustINX <- function(df, typeINX, resultINX) {
  
  for (i in 1:length(typeINX)) {
    
    typeINXvar <- typeINX[, i] # set the specific vector we are dealing with for the first iteration
    resultINXvar <- resultINX[i] 
    # set the specific element from resultINX that will correspond to the name of the result interaction variable
    # also need to use [[]] format so that I can pass a name from the function argument to the function and have
    # the function use the name I've given in the arguments
    
    if (sum(!is.na(typeINXvar)) > 0) { # only to iteration if the selection of records in the df
      # contains information of interacting variables in one of the INX variables
      
      df[[resultINXvar]] <- 0 # need to use [[]] format again here for same reason as above
      # add the selected resultINX variable name to the overall data frame and give it a default of 0
      
      inxVar <- df$VarName
      # create a vector of all of the VarName present within in the df to compare against the 
      # VarNames listed within the INX variable
      
      for (j in 1:nrow(df)) {
        if (!is.na(typeINXvar[j])) { 
          # need this to ensure that checkINX only includes the info from cases where text is present in this INX variable
          checkINX <- typeINXvar[j]
          for (k in 1:length(inxVar)) {
            result <- stringr::str_detect(checkINX, inxVar[k])
            if (result == TRUE) {
              df[[resultINXvar]][j] <- 1 # need to use [[]] format again here for same reason as above
            }
          }
        }
      }
    }
  }
  
  # adjustments to any variables identified with resNoIncr will be dealt with separately outside of this function
  
  if(resultINX[1] %in% names(df)) { # identify is INX variable was added to the data frame in previous iterations
    # if there are any interactions between variables that cause a NoINX variable to interact
    # then change NoINX from 1 (yes) to 0 (no)
    df <- df %>% dplyr::mutate(NoINX = dplyr::case_when(resYesINX == 1 ~ 0,
                                          TRUE ~ NoINX))
  }
  
  if(resultINX[2] %in% names(df)) { # identify is INX variable was added to the data frame in previous iterations
    # if there are any interactions that cause a HR/LR indirect identifier to cause a DirectID increase in LvlCalc
    # then change the ID values for DirectID to 1 (yes) and HR/LRIndirID to 0 (no)
    df <- df %>% dplyr::mutate(DirectID = dplyr::case_when(resIncrDir == 1 ~ 1,
                                             TRUE ~ DirectID),
                        HRIndirID = dplyr::case_when(resIncrDir == 1 ~ 0,
                                              TRUE ~ HRIndirID),
                        LRIndirID = dplyr::case_when(resIncrDir == 1 ~ 0,
                                              TRUE ~ LRIndirID))
    
  }
  
  if(resultINX[3] %in% names(df)) { # identify is INX variable was added to the data frame in previous iterations
    # if there are any interactions that cause a LR indirect identifier to cause an HR indirect ID increase LvlCalc
    # then change the ID values for HRIndirID to 1 (yes) and LRIndirID to 0 (no)
    df <- df %>% dplyr::mutate(LRIndirID = dplyr::case_when(resIncrHR == 1 ~ 0,
                                              TRUE ~ LRIndirID),
                        HRIndirID = dplyr::case_when(resIncrHR == 1 ~ 1,
                                              TRUE ~ HRIndirID))
    
  }
  
  if(resultINX[4] %in% names(df)) { # identify is INX variable was added to the data frame in previous iterations
    # if there are any interactions that cause a variable with no identifier status to cause an LR indirect ID increase in LvlCalc
    # then change ID values for LRIndirID to 1 (yes)
    df <- df %>% dplyr::mutate(LRIndirID = dplyr::case_when(resIncrLR == 1 ~ 1,
                                              TRUE ~ LRIndirID))
  }
  
  if(resultINX[5] %in% names(df)) { # identify is INX variable was added to the data frame in previous iterations
    # if there are any interactions that cause an HR indirect identifier or DirectID to cause a LR indirect ID increase in LvlCalc
    # then change the ID values for LRIndirID to 1 (yes) and DirectID and HRIndirID to 0 (no)
    
    df <- df %>% dplyr::mutate(LRIndirID = dplyr::case_when(resDecrLR == 1 ~ 1,
                                              TRUE ~ LRIndirID),
                        HRIndirID = dplyr::case_when(resDecrLR == 1 ~ 0,
                                              TRUE ~ HRIndirID),
                        DirectID = dplyr::case_when(resDecrLR == 1 ~ 0,
                                             TRUE ~ DirectID))
    
  }
  return(df)
}

# function for checking and making note of combinations of VarNames where ExcepToIncr applies
# in this situation the adjustINX function can't be applied because we need to identify the VarNames
# that the adjustments apply to differently

excepToIncrFx <- function(df) {
  df$resNoIncr <- 0 # create variable for identifying the need for adjustments
  if(sum(!is.na(df$ExcepToIncr)) > 0) {
  excepToIncrVect <- str_remove(df$ExcepToIncr[!is.na(df$ExcepToIncr)], ";") # create vector of each 
  # VarName listed under ExcepToIncr, but remove all missings and remove the ; that is included in the raw data
  # note, this function only works if there is only one VarName listed in ExcepToIncr
  # if that changes in the future we will need a different function here
  for (i in 1:nrow(df)) { #iterate through row of df
    for (j in 1:length(excepToIncrVect)) { #iterate through each value of the VarNames included in the vector
      if (df$VarName[i] == excepToIncrVect[j]) { # if a VarName written in ExcepToIncr is present in the dataset
        # that VarName gets highlighted with resNoIncr so that that VarName does not add to the overall calculations
        df$resNoIncr[i] <- 1
      }
    }
  }
  }
  return(df)
}


# function for calculating over all data reidentification risk level


lvlCalcFx <- function(df) {
  newDF <- df %>%
    # identify all the records with a max Lvl in the section
    # as well as all the duplicates after the
    # first occurence of the max Lvl 
    # arrange by occurences of NoINX so that first occurence of max(Lvl) should be one where 
    # NoINX == 0
    arrange(NoINX) %>%
    mutate(duplMax = case_when(Lvl %in% max(Lvl) & duplicated(Lvl) ~ 1,
                               TRUE ~ 0))
  
  newDF <- if ("resNoIncr" %in% names(newDF)) { # check if resNoIncr variable present (after excepToIncrFx has been run) 
    #before doing calculation accounting for an ExcepToIncr interaction
    newDF %>%
      # calculate overall new Lvl value taking into consideration
      # the duplicated max values and the impact of different
      # identifier combo's
      mutate(lvlCalc = case_when(Lvl == max(Lvl) & duplMax == 0 ~ Lvl,
                                 resNoIncr == 1 ~ 0,
                                 NoINX == 1 ~ 0,
                                 DirectID == 1 ~ 0.3,
                                 HRIndirID == 1 ~ 0.2,
                                 LRIndirID == 1 ~ 0.1,
                                 DirectID == 1 & duplMax == 1 ~ 0.3,
                                 HRIndirID == 1 & duplMax == 1 ~ 0.2,
                                 LRIndirID == 1 & duplMax == 1 ~ 0.1,
                                 TRUE ~ 0)) 
  } else { # carry out basic calculation without any other modifications
    newDF %>%
      # calculate overall new Lvl value taking into consideration
      # the duplicated max values and the impact of different
      # identifier combo's
      mutate(lvlCalc = case_when(Lvl == max(Lvl) & duplMax == 0 ~ Lvl,
                                 NoINX == 1 ~ 0,
                                 DirectID == 1 ~ 0.3,
                                 HRIndirID == 1 ~ 0.2,
                                 LRIndirID == 1 ~ 0.1,
                                 DirectID == 1 & duplMax == 1 ~ 0.3,
                                 HRIndirID == 1 & duplMax == 1 ~ 0.2,
                                 LRIndirID == 1 & duplMax == 1 ~ 0.1,
                                 TRUE ~ 0)) 
  }
  
  newDF %>%
    summarise(maxX = sum(lvlCalc)) %>%
    # if total maxLvl is greater than 5, recalculate it to a max of 5
    mutate(maxX = ifelse(maxX > 5, 5, maxX))
}