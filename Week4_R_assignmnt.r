#Week 4: Dplyr Package

#Task: Write the function to get a dataset from Base R: Titanic
#and give the dataframe a new name of your choice
#(hint: you will want your data to be a dataframe. Use the function: as.data.frame(Titanic))

tit_detls <- function()
  
  {
     kkr_titanic <- as.data.frame(Titanic)
     colnames(kkr_titanic)
     return(kkr_titanic)
  }


#See the top rows of the data
#TASK: Write the function to see the top rows of the data

   top_rows <- function(df, n=6)
     
      {
         head(df, n)
      }

    
#Install and call the package dplyr
#TASK: Write the functions to install and call dplyr

in_dplyr <- function() 
  
  {
      if(!require(dplyr)) 
        {
           install.packages("dplyr")
           library(dplyr)
        } 
  
  else 
      {
           library(dplyr)
      }
  }


#Let's just see the Survived and Sex columns
#Task: Write the function to 'select' the Survived and Sex columns 
#(hint: use the 'select' function)

sel_col <- function(df) 
  
    {
      select(df, Survived, Sex)
    }


#Let's name the dataset with just the two columns, Survived and Sex
#TASK: Write the function to save the two columns as one new dataset
#and give it a name

save_datst <- function(df)
  
     {
       nw_datst <- sel_col(df)
       return(nw_datst)
     }


#Let's get rid of the Sex column in the new dataset created above
#TASK: Write the function that deselects the sex column
#(hint: use the 'select' function to not select a -column)

desel_col <- function(df) 
  
  {
    select(df, -Sex)
  }


#Let's rename a column name
#TASK: Write the function that renames 'Sex' to 'Gender'

col_rename <- function(df) 
  
   {
     nw_col <- rename(df, Gender = Sex)
     return(nw_col)
   }


#Let's make a new dataframe with the new column name
#TASK: Write the function that names a new dataset that includes the 'gender' column

nw_col_datst <- function(df) 
  
  {
    nw_col_datst <- col_rename(df)
    return(nw_col_datst)
  }


#Let's 'filter' just the males from our dataset
#TASK: Write the function that includes only rows that are 'male'

sel_m_gndr <- function(df) 
  
  {
    m <- filter(df, Gender == "male")
    return(m)
  }


#Let's 'arrange' our data by gender (not the data you just filtered)
#TASK: Write the function to group the data by gender (hint: arrange())

grp_gndr_arr <- function(df)
  
  {
    grp_gndr_arr <- arrange(df, Gender)
    return(grp_gndr_arr)
  }


#Let's see how many people were examined in the dataset (total the frequency in the original dataframe)
#TASK: Sum the Freq column
#TASK: After you run it, write the total here:____

tot_freq <- function(df) 
  
  {
    freqTot <- sum(df$Freq)
    return(freqTot)
  }


#Since we have a males dataset, let's make a females dataset
#TASK: Write the function that includes only rows that are 'female'

sel_fm_gndr <- function(df) 
  
    {
      fm <- filter(df, Gender == "female")
      return(fm)
    }


#And now let's join the males and females
#TASK: Write the function that joins the male and female rows 
#(hint: try using 'union' or 'bind_rows')

jn_gndr <- function(m, fm) 
  
   {
     all_gndr <- bind_rows(m, fm)
     return(all_gndr)
   }


#Optional Task: add any of the other functions 
#you learned about from the dplyr package

grp_surv_gndr <- function(df) 
  
  {
    grpd_df <- group_by(df, Survived)
    return(grpd_df)
  }


#Here I'm calling the above
# Calling Main Functionalities

#Here d is dataset
  d <- tit_detls()
  d

#Here for top rows
  top_rows(d)

#Installing dplyr packgs
  in_dplyr()

#Adding new column
  nw_col <- sel_col(d)
  nw_col

#Storing in the new  data
   nw_datst <- save_new_dataset(nw_col)
   nw_datstk <- nw_datst
   nw_datstk

#For deslecting the fields
   desel_col(nw_datst)

#For Renaming
   nw_colk <- col_rename(nw_datst)
   nw_colk


# new dataframe with the new column name
   nw_col_datst <-nw_col_datst(nw_datstk)
   nw_col_datst


#Selecting male data
   m <- sel_m_gndr(nw_col_datst)
   m

#Grouping all gender data 
   grp_gndr <- grp_gndr_arr(nw_col_datst)
   grp_gndr

#Frequency Total
   freqTot <- tot_freq(d)
   freqTot

#Female  data
   fm <- sel_fm_gndr(nw_col_datst)
   fm

#All gender combined survival Data
   all_gndr <- grp_surv_gndr(d)
   all_gndr