#% VIPR-GS Project 3.3
# Hannah Stewart (IMPLEMENTED IN R BY WILLIAM FISHER)
# 9/14/2021 (WILLIAM FISHER BEGAN EDITING 9/2/2024)

# The purpose of the following code is to calculate four different 
# fundamental parameters (back deck overhang, running gear contact patch 
# area, SMET FCC length, and SMET FCC curb-to-curb turning diameter) given 
# certain input data.

response_simulator <- function(vehicle_attributes){
  #vehicle_attributes: This is a vector of vehicle attributes as given in the
  #file "FinalDataCleaned.csv". See this file for attribute names.
  
  #The first component of Input_Constants (-0.0) is a placeholder, do not use! It is
  #there because the original coder of this simulator had a "#" as the first component
  #in the list of input constants.
  Input_Constants <- c(-0.0,999.99,2.99,48,910.85,70,72,331,90,55.8,26000,35,2000,0.93,
                       4.07,13.44,24.25,10.63,129,-39,4.99,7.25,5,24,300,16,5000,0.99,
                       8.19,8.19,33,8.19,25000,0.2,6.99,6.7,170,0.032,5.29,5000,0.99,
                       8.19,8.19,33,8.19,45,20,24,10,68.66,22,104,84,2,9,18,24,15,720,
                       0.9,36,764,99.99,394,0.0002,1,0,13.5,3.5,0.02,13,11.1,6.4,0.0083,
                       9000,540,52.8,8.34,20,3.1)
  
  #Preallocate variables
  
  Axle_Separation_in = 0
  
  Back_Deck_Overhang_in = 0
  
  Front_Deck_Overhang_in = 0
  
  Inner_Turning_Radius = 0
  
  Lower_Cargo_Storage_Volume_in3 = 0
  
  Lower_Cargo_Width_in = 0
  
  Max_Ackerman_Angle_deg = 0
  
  Offload_Generator_Volume_in3 = 0
  
  Running_Gear_Contact_Patch_Area = 0
  
  SMET_FCC_CtoC_Turning_Diameter_in = 0
  
  SMET_FCC_Length_in = 0
  
  SMET_FCC_Width_in = 0
  
  SMET_Platform_Length_in = 0
  
  SMET_Platform_Width_in = 0
  
  Total_FCC_Load_Volume_in3 = 0
  
  #Calculate Back Deck Overhang
  
  Back_Deck_Overhang_in = max(c(0,vehicle_attributes[13] - vehicle_attributes[2]))
  
  #Calculate Running Gear Contact Patch Area
  
  ##!!! After Eight-Wheeled, there was a mistake where the formulas for Mattrack and Oval
  ##were exchanged. I made a change to fix this problem (09/04/2024.
  if(vehicle_attributes[12] == 1){ #Four-wheeled
    Running_Gear_Contact_Patch_Area <- 4*2*pi*vehicle_attributes[26]*
      vehicle_attributes[25]/360*vehicle_attributes[27]
  }else if(vehicle_attributes[12] == 2){ #Six-wheeled
    Running_Gear_Contact_Patch_Area <- 6*2*pi*vehicle_attributes[26]*
      vehicle_attributes[25]/360*vehicle_attributes[27]
  }else if(vehicle_attributes[12] == 3){ #Eight-wheeled
    Running_Gear_Contact_Patch_Area <- 8*2*pi*vehicle_attributes[26]*
      vehicle_attributes[25]/360*vehicle_attributes[27]
  }else if(vehicle_attributes[12] == 4){ #Oval track
    Running_Gear_Contact_Patch_Area <- 2*(vehicle_attributes[6]-2*vehicle_attributes[2])*vehicle_attributes[18]
  }else if(vehicle_attributes[12] == 5){ #Trapezoid track
    if(vehicle_attributes[15]>=vehicle_attributes[23]){
      Running_Gear_Contact_Patch_Area <- 2*(vehicle_attributes[6]-2*vehicle_attributes[2] -
                  3.414*vehicle_attributes[23] - 0.586*vehicle_attributes[15])*vehicle_attributes[18]
    }else{
      Running_Gear_Contact_Patch_Area <- 2*(vehicle_attributes[6]-2*vehicle_attributes[2]-
                  5.414*vehicle_attributes[23] + 1.414*vehicle_attributes[15])*vehicle_attributes[18]
    }
  }else if (vehicle_attributes[12] == 6){ #Parallelogram track
      Running_Gear_Contact_Patch_Area <- 2*(vehicle_attributes[6]-2*vehicle_attributes[2]-
                  1.414*vehicle_attributes[23] + 1.414*vehicle_attributes[15])*vehicle_attributes[18]
  }else if(vehicle_attributes[12] == 7){ #Mattrack
      Running_Gear_Contact_Patch_Area <- 4*vehicle_attributes[19]*vehicle_attributes[22]
  }
    
  #}#else if(vehicle_attributes[12] == 4){ #Mattrack
    #Running_Gear_Contact_Patch_Area <- 4*vehicle_attributes[19]*vehicle_attributes[22]
  #}else if(vehicle_attributes[12] == 5){ #Oval track
    #Running_Gear_Contact_Patch_Area <- 2*(vehicle_attributes[6]-2*vehicle_attributes[2])*vehicle_attributes[18]
  #}else if(vehicle_attributes[12] == 6){ #Trapezoid track
    #if(vehicle_attributes[15]>=vehicle_attributes[23]){
      #Running_Gear_Contact_Patch_Area <- 2*(vehicle_attributes[6]-2*vehicle_attributes[2] -
                                             #3.414*vehicle_attributes[23] - 0.586*vehicle_attributes[15])*vehicle_attributes[18]
    #}else{
      #Running_Gear_Contact_Patch_Area <- 2*(vehicle_attributes[6]-2*vehicle_attributes[2]-
                                              #5.414*vehicle_attributes[23] + 1.414*vehicle_attributes[15])*vehicle_attributes[18]
    #}
  #}else if (vehicle_attributes[12] == 7){ #Parallelogram
   # Running_Gear_Contact_Patch_Area <- 2*(vehicle_attributes[6]-2*vehicle_attributes[2]-
                                            #1.414*vehicle_attributes[23] + 1.414*vehicle_attributes[15])*vehicle_attributes[18]
  #}
  
  #Calculate SMET FCC Length
  a <- max(c(0,vehicle_attributes[14]-vehicle_attributes[2]))
  b <- max(c(0,vehicle_attributes[30]))
  Front_Deck_Overhang_in = max(c(a,b))
  
  SMET_FCC_Length_in <- vehicle_attributes[6] + Front_Deck_Overhang_in + Back_Deck_Overhang_in
  
  #Calculate SMET FCC Curb-to-Curb Turning Diameter
  Total_FCC_Load_Volume_in3 <-(9*Input_Constants[51]*Input_Constants[48]*Input_Constants[49]+
                                 Input_Constants[57]*153+Input_Constants[54]*Input_Constants[76]+
                                 Input_Constants[55]*Input_Constants[77]+Input_Constants[56]*231.001)/Input_Constants[60]
  ##Lower Cargo Storage Volume
  Offload_Generator_Volume_in3 <- vehicle_attributes[9]*vehicle_attributes[10]*vehicle_attributes[11]
  
  SMET_Platform_Width_in <- vehicle_attributes[7] + 2*vehicle_attributes[18]
  
  SMET_Platform_Length_in <- vehicle_attributes[6] + Front_Deck_Overhang_in + Back_Deck_Overhang_in
  
  #THE BELOW CODE BLOCK IS INCORRECT!!! WILL CHANGE AFTER CONFIRMING RESULTS MATCH
  ##!!!!!!!!!!!!!!!!!##
  
  #THIS IS THE FIX! (09/03/2024) (ACTUALLY DON'T NEED TO NECESSARILY FIX THIS, BUT IT HAS A BAD FORM)
  #if(vehicle_attributes[3] == 1){ #Flush with frame
    #Lower_Cargo_Width_in <- SMET_Platform_Width_in
  #}else if(vehicle_attributes[3] == 2){ #Hanging over frame
    #Lower_Cargo_Width_in <- SMET_Platform_Width_in + 2*Input_Constants[49]
  
  if(vehicle_attributes[5] == 1){ #Box
    Lower_Cargo_Storage_Volume_in3 <- 0
  }else if(vehicle_attributes[5] == 2){ #MUTT
    Lower_Cargo_Storage_Volume_in3 <- SMET_Platform_Length_in*vehicle_attributes[7]*vehicle_attributes[4]
  }else if(vehicle_attributes[3] == 1){ #Flush with frame  
    Lower_Cargo_Width_in <- SMET_Platform_Width_in 
  }else if(vehicle_attributes[3] == 2){ #Hanging over frame 
    Lower_Cargo_Width_in <- SMET_Platform_Width_in + 2*Input_Constants[49] #Get rid of later
  }else if(vehicle_attributes[5] == 3){ #Protector
    Lower_Cargo_Storage_Volume_in3 <- SMET_Platform_Length_in - 0.65*vehicle_attributes[6] -
      Front_Deck_Overhang_in*Lower_Cargo_Width_in*vehicle_attributes[4]
  }else if(vehicle_attributes[5] == 4){ #SMSS
    Lower_Cargo_Storage_Volume_in3 <- SMET_Platform_Length_in - 0.35*vehicle_attributes[6] - 
      Front_Deck_Overhang_in*Lower_Cargo_Width_in*vehicle_attributes[4] +
      2*0.35*vehicle_attributes[6]*(Lower_Cargo_Width_in - vehicle_attributes[7])*vehicle_attributes[4]
  }
  ###!!!!!!!!!!!!!!!########
  
  #SMET FCC Width
  if(vehicle_attributes[5] == 2){ #MUTT
    if(Lower_Cargo_Storage_Volume_in3 >= Total_FCC_Load_Volume_in3 + Offload_Generator_Volume_in3){
      SMET_FCC_Width_in <- SMET_Platform_Width_in
      }
  }else if(vehicle_attributes[3] == 1){ #Flush with Frame
    SMET_FCC_Width_in <- SMET_Platform_Width_in
  }else if(vehicle_attributes[3] == 2){ #Hanging over frame
    SMET_FCC_Width_in <- SMET_Platform_Width_in + 2*Input_Constants[49]
  }
  
  #SMET FCC Curb-to-Curb Turning Diameter
  Axle_Separation_in <- vehicle_attributes[6] - 2*vehicle_attributes[2]
  
  if(vehicle_attributes[28] == 1){ #Pivot
    SMET_FCC_CtoC_Turning_Diameter_in <- sqrt((SMET_FCC_Length_in)^2 + (SMET_FCC_Width_in)^2)
  }else if(vehicle_attributes[28] == 2){ #Skid
    SMET_FCC_CtoC_Turning_Diameter_in <- 2*sqrt((SMET_FCC_Length_in/2)^2 + ((SMET_FCC_Width_in + SMET_Platform_Width_in - vehicle_attributes[18])/2)^2)
  }else{
    if(vehicle_attributes[12] == 1){ #Four-wheeled
      Max_Ackerman_Angle_deg <- Input_Constants[47]
    }else if(vehicle_attributes[12] == 2){ #Six-wheeled
      Max_Ackerman_Angle_deg <- Input_Constants[47]
    }else if(vehicle_attributes[12] == 3){ #Eight-wheeled
      Max_Ackerman_Angle_deg <- Input_Constants[47]
    }else if(vehicle_attributes[12] == 4){ #Oval track
      Max_Ackerman_Angle_deg <- Input_Constants[46]
    }else if(vehicle_attributes[12] == 5){ #Trapezoidal Track
      Max_Ackerman_Angle_deg <- Input_Constants[46]
    }else if(vehicle_attributes[12] == 6){ #Parallelogram Track
      Max_Ackerman_Angle_deg <- Input_Constants[46]
    }else if(vehicle_attributes[12] == 7){ #Mattrack
      Max_Ackerman_Angle_deg <- Input_Constants[46]
    }
    
    if(vehicle_attributes[28] == 3){ #Single Ackerman
      Inner_Turning_Radius <- Axle_Separation_in/tan(Max_Ackerman_Angle_deg)
    }else if(vehicle_attributes[28] == 4){ #Dual Ackerman
      Inner_Turning_Radius <- 0.5*Axle_Separation_in/tan(Max_Ackerman_Angle_deg)
    }
    
    SMET_FCC_CtoC_Turning_Diameter_in <- SMET_FCC_Width_in + SMET_Platform_Width_in - 
      vehicle_attributes[18] + 2*Inner_Turning_Radius
  }
  
  
  return(c(Back_Deck_Overhang_in,Running_Gear_Contact_Patch_Area,SMET_FCC_Length_in,SMET_FCC_CtoC_Turning_Diameter_in))
           
  
}

#Here we test the correctness of the code to see if it matches the original output before
#making any correction to the code.

#simulated_data_resp_sim <- read.csv("FinalDataCleaned.csv")

#n <- nrow(simulated_data_resp_sim)

#for(i in 1:n){
  #vehicle_attr_vec_i <- as.numeric(simulated_data_resp_sim[i,1:30])
  
  #resp_i <- response_simulator(vehicle_attr_vec_i)
  
  #simulated_data_resp_sim[i,"New_Resp_1"] <- resp_i[1]
  #simulated_data_resp_sim[i,"New_Resp_2"] <- resp_i[2]
  #simulated_data_resp_sim[i,"New_Resp_3"] <- resp_i[3]
  #simulated_data_resp_sim[i,"New_Resp_4"] <- resp_i[4]
  
#}

#all(abs(as.numeric(simulated_data_resp_sim[,"New_Resp_1"])-as.numeric(simulated_data_resp_sim[,"Back.Deck.Overhang.in"])) < 0.000000001) #accurate to 9 digits
#all(abs(as.numeric(simulated_data_resp_sim[,"New_Resp_2"])-as.numeric(simulated_data_resp_sim[,"Running.Gear.Contact.Patch.Area.in2"])) < 0.0000001) #accurate to 7 digits
#all(abs(as.numeric(simulated_data_resp_sim[,"New_Resp_3"])-as.numeric(simulated_data_resp_sim[,"SMET.FCC.Length.in"])) < 0.000000001) #accurate to 9 digits
#all(abs(as.numeric(simulated_data_resp_sim[,"New_Resp_4"])-as.numeric(simulated_data_resp_sim[,"SMET.FCC.CtoC.Turning.Diameter.in"])) < 0.0000001) #accurate to 7 digits

