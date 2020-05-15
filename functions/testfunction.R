###########################################################
# Test functionality for practise app                     #
#                                                         #
# Auteurs: Lilith Kramer                                  #
#          Willem Stolte                                  #
#                                                         #
#                                                         #
# Datum: 15 May 2020                                      #
# Bedrijf:                                                #
# Licentie: GNU General Public License                    #
#                                                         #           
# Contact: XYZ                                            #
# Email: blabla@xyz.nl                                    #
#                                                         #
########################################################### 

read_file <- function(filename){
  
  library(tidyverse)
  opened_file <- read_delim(filename, delim = ";")
  
}