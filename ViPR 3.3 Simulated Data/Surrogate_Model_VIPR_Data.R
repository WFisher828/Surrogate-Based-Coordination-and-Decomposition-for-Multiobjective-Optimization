#Model Building for Simulated VIPR Dataset.
#8/26/2024
#William Fisher

#Load libraries
library(car) #For using variance inflation factor.
library(MASS) #For using Box-cox


#Different users may need to change working directory.
simulated_data <- read.csv("FinalDataCleaned.csv")


#We first perform some data cleaning:
#1. Get rid of rows where responses have value less than or equal 0. I saw a response 
#   which was negative. This indicates something is wrong with the original
#   simulator.
#2. Get rid of the column "Running.Gear.Wheel.Width" because for some reason
#   it only contains 1s. 
#3. Convert some variables to factors since they are categorical/binary.
#4. Add a small constant since some responses have a value of 0.
#5. Investigate aliasing in the categorical variables related to
#   Running Gear Contact Patch Area in2
#   a. Running.Gear.Wheel.Contact.Patch.Arc
#   b. Running.Gear.Mattrack.Ground.Contract.Length
#   c. Running.Gear.Mattrack.Track.Width
#   d. Running.Gear.Track.Width
#   e. Running.Gear.Drive.Gear.Radius
#   f. Running.Gear.Road.Wheel.Radius


#Task 1.
simulated_data <- subset(simulated_data, Back.Deck.Overhang.in >= 0 &
                         Running.Gear.Contact.Patch.Area.in2 >= 0 &
                         SMET.FCC.Length.in >= 0 &
                         SMET.FCC.CtoC.Turning.Diameter.in >= 0)

#We removed 5 data entries, which had negative responses in 
#Running.Gear.Contact.Patch.Area.in2

#Task 2.
#We get rid of the column having all 1s for some reason.
simulated_data <- subset(simulated_data, select = -c(Running.Gear.Wheel.Width))

#Task 3.
#This is a list of categorical and binary variables, which we will convert to
#factors in simulated data.
factor_list <- c("Frame.FCC.Loading.Style","Frame.Shape",
                 "Offload.Generator.Present","Running.Gear.Type",
                 "Steering.Type.Name","Winch.Present")
#Old factors:
#"Running.Gear.Drive.Gear.Radius","Running.Gear.Drive.Gear.Width",
#"Running.Gear.Track.Thickness",
#"Running.Gear.Track.Width","Running.Gear.Mattrack.Ground.Contract.Length",
#"Running.Gear.Mattrack.Inset.Length","Running.Gear.Mattrack.Length",
#"Running.Gear.Mattrack.Track.Width","Running.Gear.Road.Wheel.Radius",
#"Running.Gear.Road.Wheel.Width","Running.Gear.Wheel.Contact.Patch.Arc"

#Some factors appear like they would be ordered.
#ordered_factor_list <- c("Running.Gear.Drive.Gear.Radius","Running.Gear.Track.Thickness",
                         #"Running.Gear.Track.Width","Running.Gear.Mattrack.Ground.Contract.Length",
                         #"Running.Gear.Road.Wheel.Radius")

#Factor conversion.
for(i in factor_list){
  #if(i %in% ordered_factor_list){
    #simulated_data[,i] <- factor(simulated_data[,i],ordered = TRUE)
  #} else{
  #simulated_data[,i] <- factor(simulated_data[,i])
  #}
  simulated_data[,i] <- factor(simulated_data[,i])
}

#Task 4. Add a small positive constant to Back.Deck.Overhang.in and
#Running.Gear.Contact.Patch.Area.in2 for Box-Cox transformations
small_const <- 0.000001
simulated_data[,"Back.Deck.Overhang.in"] <- simulated_data[,"Back.Deck.Overhang.in"] + small_const
simulated_data[,"Running.Gear.Contact.Patch.Area.in2"] <- simulated_data[,"Running.Gear.Contact.Patch.Area.in2"] + small_const

#Task 5. Checking aliasing with categorical variables related to Running Gear Contact Patch Area in2
aliased_variables_patch_area <- simulated_data[,c("Running.Gear.Type","Running.Gear.Wheel.Contact.Patch.Arc",
                                                  "Running.Gear.Mattrack.Ground.Contract.Length",
                                                  "Running.Gear.Mattrack.Track.Width",
                                                  "Running.Gear.Track.Width",
                                                  "Running.Gear.Drive.Gear.Radius",
                                                  "Running.Gear.Road.Wheel.Radius")]

################################################################################
###################*****************************************####################
################################################################################


#Certain responses depend only on certain variables. For modeling, we consider
#only main effects and quadratics for the continuous terms, and only main effects
#for the categorical terms.

#Back Deck Overhang in. depends on: 
#     Running Gear Back Mount Inset 
#     Frame Axle Mount Inset 

#We define the null model.
back_deck_null <- lm(Back.Deck.Overhang.in ~ 1, data = simulated_data)

summary(back_deck_null)

#We define the full model
back_deck_full <- lm(Back.Deck.Overhang.in ~ Running.Gear.Back.Mount.Inset + 
                       Frame.Axle.Mount.Inset + I(Running.Gear.Back.Mount.Inset^2) +
                       I(Frame.Axle.Mount.Inset^2), data = simulated_data)

summary(back_deck_full)

#Look for model using backward elimination.
backward_back_deck <- step(back_deck_full, direction = 'backward', scope = formula(back_deck_full),
                           k = log(length(simulated_data$DataPoint..)))
backward_back_deck$anova
backward_back_deck$coefficients

#Look for model using forward method.
forward_back_deck <- step(back_deck_null, direction = 'forward', scope = formula(back_deck_full),
                          k = log(length(simulated_data$DataPoint..)))
forward_back_deck$anova
forward_back_deck$coefficients

#It looks like both methods converged to the full model.

#Check the vif. Mean centering the data could reduce the effect in this case if needed.
vif(back_deck_full)

#Plot results related to linear fit to assess model.
plot(back_deck_full)

#Next we perform a Box-Cox transformation on the selected model:

box_back_deck <- boxcox(back_deck_full)

# Exact lambda for back_deck_full
lambda_back_deck <- box_back_deck$x[which.max(box_back_deck$y)]

simulated_data[,"Box.Back.Deck.Overhang.in"] <- (simulated_data[,"Back.Deck.Overhang.in"]^lambda_back_deck - 1)/lambda_back_deck

#Fit the model to the box-cox transformed data.
box_back_deck_full_model <- lm(Box.Back.Deck.Overhang.in ~ Running.Gear.Back.Mount.Inset + 
                                 Frame.Axle.Mount.Inset + I(Running.Gear.Back.Mount.Inset^2) +
                                 I(Frame.Axle.Mount.Inset^2), data = simulated_data)

plot(box_back_deck_full_model)
summary(box_back_deck_full_model) #The box-cox transformation resulted in a poorer fit!
#Compare R2 0.9277 of box-cox model and R2 0.9756 of untransformed model.
vif(box_back_deck_full_model)
###############################################################################
########************************************************************###########
###############################################################################

#Running Gear Contact Patch Area in2 depends on:
#     Running Gear Type (cat)
#     Running Gear Wheel Radius
#     Running Gear Wheel Contact Patch Arc (cat)
#     Running Gear Wheel Width (DONT USE THIS!!!)
#     Running Gear Mattrack Ground Contract Length (Cat)
#     Running Gear Mattrack Track Width (Cat)
#     Frame Underbody Length 
#     Frame Axle Mount Inset
#     Running Gear Track Width (Cat)
#     Running Gear Drive Gear Radius (Cat)
#     Running Gear Road Wheel Radius (Cat)

contact_patch_area_null <- lm(Running.Gear.Contact.Patch.Area.in2 ~ 1, data = simulated_data)

summary(contact_patch_area_null)

contact_patch_area_full <- lm(Running.Gear.Contact.Patch.Area.in2 ~ Running.Gear.Wheel.Radius +
                                I(Running.Gear.Wheel.Radius^2) + Frame.Underbody.Length +
                                I(Frame.Underbody.Length^2) + Frame.Axle.Mount.Inset +
                                I(Frame.Axle.Mount.Inset^2) + 
                                Running.Gear.Drive.Gear.Radius + I(Running.Gear.Drive.Gear.Radius^2) + 
                                Running.Gear.Road.Wheel.Radius + I(Running.Gear.Road.Wheel.Radius^2) +
                                Running.Gear.Type + Running.Gear.Track.Width +
                                Running.Gear.Mattrack.Ground.Contract.Length, data = simulated_data)

summary(contact_patch_area_full)

vif(contact_patch_area_full)

#Look for model using backward's elimination.
backward_patch_area <- step(contact_patch_area_full, direction = 'backward', scope = formula(contact_patch_area_full),
                           k = log(length(simulated_data$DataPoint..)))
backward_patch_area$anova
backward_patch_area$coefficients

#Look for model using forward method.
forward_patch_area <- step(contact_patch_area_null, direction = 'forward', scope = formula(contact_patch_area_full),
                          k = log(length(simulated_data$DataPoint..)))
forward_patch_area$anova
forward_patch_area$coefficients

#It appears that the forward and backward method produce the same result.

patch_area_final_mod <- lm(Running.Gear.Contact.Patch.Area.in2 ~ Running.Gear.Type +
                             Frame.Underbody.Length + Frame.Axle.Mount.Inset + 
                             I(Running.Gear.Road.Wheel.Radius^2), data = simulated_data)

summary(patch_area_final_mod)

plot(patch_area_final_mod)

vif(patch_area_final_mod)

#Perform Box-Cox Transformation
box_patch_area <- boxcox(patch_area_final_mod)

# Exact lambda for patch_area_final_mod
lambda_patch_area <- box_patch_area$x[which.max(box_patch_area$y)]

simulated_data[,"Box.Running.Gear.Contact.Patch.Area.in2"] <- (simulated_data[,"Running.Gear.Contact.Patch.Area.in2"]^lambda_patch_area - 1)/lambda_patch_area

box_patch_area_final_mod <- lm(Box.Running.Gear.Contact.Patch.Area.in2 ~ Running.Gear.Type +
                                 Frame.Underbody.Length + Frame.Axle.Mount.Inset + 
                                 I(Running.Gear.Road.Wheel.Radius^2), data = simulated_data)

summary(box_patch_area_final_mod) #In the Box-Cox transformed model, the continuous predictors
#are no longer significant. R2 = 1 for the Box-Cox transformed model. 

plot(box_patch_area_final_mod)

vif(box_patch_area_final_mod)

################################################################################
######****************************************************************##########
################################################################################

#SMET FCC Length in. depends on: 
#     Back Deck Overhang in. (THIS IS THE FIRST RESPONSE! WE INCLUDE THE VARIABLES THIS DEPENDS
#     ON BELOW AS WELL)
#     Frame Axle Mount Inset 
#     Running Gear Back Mount Inset
#     Running Gear Front Mount Inset
#     Frame Underbody Length 
#     Winch Stowed Width 

fcc_length_mod_null <- lm(SMET.FCC.Length.in ~ 1, data = simulated_data)

summary(fcc_length_mod_null)

fcc_length_mod_full <- lm(SMET.FCC.Length.in ~ Frame.Axle.Mount.Inset + I(Frame.Axle.Mount.Inset^2) +
                            Running.Gear.Back.Mount.Inset + I(Running.Gear.Back.Mount.Inset^2) + 
                            Running.Gear.Front.Mount.Inset + I(Running.Gear.Front.Mount.Inset^2) +
                            Frame.Underbody.Length + I(Frame.Underbody.Length^2) + 
                            Winch.Stowed.Width + I(Winch.Stowed.Width^2), data = simulated_data)

summary(fcc_length_mod_full)

#Look for model using backward elimination.
backward_fcc_length <- step(fcc_length_mod_full, direction = 'backward', scope = formula(fcc_length_mod_full),
                           k = log(length(simulated_data$DataPoint..)))
backward_fcc_length$anova
backward_fcc_length$coefficients
#Selected variables include: (BIC = 2211)
#Intercept, Frame.Axle.Mount.Inset, I(Frame.Axle.Mount.Inset^2), Running.Gear.Back.Mount.Inset
#I(Running.Gear.Back.Mount.Inset^2), I(Running.Gear.Front.Mount.Inset^2),
#Frame.Underbody.Length, I(Winch.Stowed.Width^2)

#Look for model using forward method.
forward_fcc_length <- step(fcc_length_mod_null, direction = 'forward', scope = formula(fcc_length_mod_full),
                            k = log(length(simulated_data$DataPoint..)))
forward_fcc_length$anova
forward_fcc_length$coefficients
#Selected Variables Include: (BIC = 2212.99)
#Intercept, Frame.Underbody.Length, I(Running.Gear.Back.Mount.Inset^2), Frame.Axle.Mount.Inset,
#Running.Gear.Front.Mount.Inset, I(Winch.Stowed.Width^2), I(Frame.Axle.Mount.Inset^2),
#Running.Gear.Back.Mount.Inset

#Models share same variables, except the backward one has I(Running.Gear.Front.Mount.Inset^2) and
#the forward one has Running.Gear.Front.Mount.Inset. BIC of the Backward model is lower,
#but the difference is small (less than 2)

backward_fcc_length_model <- lm(SMET.FCC.Length.in ~ Frame.Axle.Mount.Inset + I(Frame.Axle.Mount.Inset^2) +
                                  Running.Gear.Back.Mount.Inset + I(Running.Gear.Back.Mount.Inset^2) + 
                                  I(Running.Gear.Front.Mount.Inset^2) +
                                  Frame.Underbody.Length + 
                                  I(Winch.Stowed.Width^2), data = simulated_data)
summary(backward_fcc_length_model)
plot(backward_fcc_length_model)

vif(backward_fcc_length_model)

forward_fcc_length_model <- lm(SMET.FCC.Length.in ~ Frame.Axle.Mount.Inset + I(Frame.Axle.Mount.Inset^2) +
                                  Running.Gear.Back.Mount.Inset + I(Running.Gear.Back.Mount.Inset^2) + 
                                  Running.Gear.Front.Mount.Inset +
                                  Frame.Underbody.Length + 
                                  I(Winch.Stowed.Width^2), data = simulated_data)
summary(forward_fcc_length_model)
plot(forward_fcc_length_model)

vif(forward_fcc_length_model)

#These two models appear comparable to one another using both the BIC and adjusted R^2 criterion.
#Should we consider model selection using AIC?...
#Should we even consider Box-Cox Transformation with such a high R^2 value?

################################################################################
######****************************************************************##########
################################################################################

#SMET FCC CtoC Turning Diameter in depends on:
#     Offload Generator Width
#     Offload Generator Length
#     Offload Generator Height
#     Frame Underbody Width
#     Running Gear Track Width (Cat) (NO LONGER CAT)
#     Running Gear Front Mount Inset
#     Winch Stowed Width
#     Frame Axle Mount Inset
#     Running Gear Back Mount Inset
#     Frame Underbody Length
#     Frame Shape (Cat)
#     Frame Overbody Height
#     Frame FCC Loading Style (Cat)
#     Steering Type Name (Cat)
#     Running Gear Type (cat)

fcc_turn_mod_null <- lm(SMET.FCC.CtoC.Turning.Diameter.in ~ 1, data = simulated_data)

summary(fcc_turn_mod_null)

#Running.Gear.Type appears to be aliased with Running.Gear.Track.Width and Running.Gear.Track.Width^2
#So we removed Running.Gear.Track.Width^2 from the model.
fcc_turn_mod_full <- lm(SMET.FCC.CtoC.Turning.Diameter.in ~ Offload.Generator.Width + I(Offload.Generator.Width^2) +
                          Offload.Generator.Length + I(Offload.Generator.Length^2) +
                          Offload.Generator.Height + I(Offload.Generator.Height^2) +
                          Frame.Underbody.Width + I(Frame.Underbody.Width^2) + 
                          Running.Gear.Track.Width + #I(Running.Gear.Track.Width^2) +
                          Running.Gear.Front.Mount.Inset + I(Running.Gear.Front.Mount.Inset^2) +
                          Winch.Stowed.Width + I(Winch.Stowed.Width^2) +
                          Frame.Axle.Mount.Inset + I(Frame.Axle.Mount.Inset^2) +
                          Running.Gear.Back.Mount.Inset + I(Running.Gear.Back.Mount.Inset^2) +
                          Frame.Underbody.Length + I(Frame.Underbody.Length^2) +
                          Frame.Overbody.Height + I(Frame.Overbody.Height^2) +
                          Frame.Shape + Frame.FCC.Loading.Style + Steering.Type.Name +
                          Running.Gear.Type, data = simulated_data)
summary(fcc_turn_mod_full) 
alias(fcc_turn_mod_full)

#Look for model using backward elimination.
backward_fcc_turn <- step(fcc_turn_mod_full, direction = 'backward', scope = formula(fcc_turn_mod_full),
                            k = log(length(simulated_data$DataPoint..)))
backward_fcc_turn$anova
backward_fcc_turn$coefficients
#Selected variables include: (BIC = 6066.97)
#Intercept, Frame.Underbody.Width, Running.Gear.Track.Width, Frame.Axle.Mount.Inset
#I(Running.Gear.Back.Mount.Inset^2), Frame.Underbody.Length, Frame.Overbody.Height,
#Frame.Shape, Frame.FCC.Loading.Style, Steering.Type.Name

#Look for model using forward method.
forward_fcc_turn <- step(fcc_turn_mod_null, direction = 'forward', scope = formula(fcc_turn_mod_full),
                           k = log(length(simulated_data$DataPoint..)))
forward_fcc_turn$anova
forward_fcc_turn$coefficients
#Selected Variables Include: (BIC = 6098.98)

#BIC for model selected using backward method is lower.

fcc_turn_final_mod <- lm(SMET.FCC.CtoC.Turning.Diameter.in ~ Frame.Underbody.Width +  Running.Gear.Track.Width +
                           +Frame.Axle.Mount.Inset + I(Running.Gear.Back.Mount.Inset^2) + Frame.Underbody.Length +
                           Frame.Overbody.Height + Frame.Shape + Frame.FCC.Loading.Style + Steering.Type.Name, data = simulated_data)
summary(fcc_turn_final_mod)
plot(fcc_turn_final_mod)

vif(fcc_turn_final_mod)
#Let's see if a box-cox transformation can help improve our model.
box_fcc_turn <- boxcox(fcc_turn_final_mod)

# Exact lambda for fcc_turn_final_mod
lambda_fcc_turn <- box_fcc_turn$x[which.max(box_fcc_turn$y)]

simulated_data[,"Box.SMET.FCC.CtoC.Turning.Diameter.in"] <- (simulated_data[,"SMET.FCC.CtoC.Turning.Diameter.in"]^lambda_fcc_turn - 1)/lambda_fcc_turn

#Make the model for the box-cox transformed data
box_fcc_turn_final_mod <- lm(Box.SMET.FCC.CtoC.Turning.Diameter.in ~ Frame.Underbody.Width +  Running.Gear.Track.Width +
                               +Frame.Axle.Mount.Inset + I(Running.Gear.Back.Mount.Inset^2) + Frame.Underbody.Length +
                               Frame.Overbody.Height + Frame.Shape + Frame.FCC.Loading.Style + Steering.Type.Name, data = simulated_data)
summary(box_fcc_turn_final_mod)
plot(box_fcc_turn_final_mod)

vif(box_fcc_turn_final_mod)


################################################################################
###################***********************************##########################
################################################################################

#Dr. Zhang Meeting Notes(8/29/2024)
table(simulated_data$Running.Gear.Type)

main_eff_resp_1 <- lm(Back.Deck.Overhang.in ~ Running.Gear.Back.Mount.Inset + 
                        Frame.Axle.Mount.Inset, data = simulated_data)

summary(main_eff_resp_1)

cor(simulated_data[,c("Running.Gear.Back.Mount.Inset","Frame.Axle.Mount.Inset")])

main_eff_resp_3 <- lm(SMET.FCC.Length.in ~ Frame.Axle.Mount.Inset  +
                        Running.Gear.Back.Mount.Inset + 
                        Running.Gear.Front.Mount.Inset + 
                        Frame.Underbody.Length + 
                        Winch.Stowed.Width, data = simulated_data)

summary(main_eff_resp_3)

cor(simulated_data[,c("Frame.Axle.Mount.Inset","Running.Gear.Back.Mount.Inset",
                      "Running.Gear.Front.Mount.Inset", "Frame.Underbody.Length",
                      "Winch.Stowed.Width")])

main_effect_resp_4_cont <- lm(SMET.FCC.CtoC.Turning.Diameter.in ~ Frame.Underbody.Width +  Running.Gear.Track.Width +
                                +Frame.Axle.Mount.Inset +  Frame.Underbody.Length +
                                Frame.Overbody.Height, data = simulated_data)
summary(main_effect_resp_4_cont)

main_effect_resp_2_cont <- lm(Running.Gear.Contact.Patch.Area.in2 ~ Running.Gear.Wheel.Radius, data = simulated_data[simulated_data$Running.Gear.Type == 1,])
summary(main_effect_resp_2_cont)

cor(simulated_data[,c("Running.Gear.Wheel.Contact.Patch.Arc","Running.Gear.Wheel.Radius")])

fcc_turn_test <- lm(SMET.FCC.CtoC.Turning.Diameter.in ~ Frame.Underbody.Width +  Running.Gear.Track.Width +
                           +Frame.Axle.Mount.Inset + Frame.Underbody.Length +
                           Frame.Overbody.Height + Frame.Shape + Frame.FCC.Loading.Style + Steering.Type.Name, data = simulated_data)
summary(fcc_turn_test)

fcc_turn_test2 <- lm(SMET.FCC.CtoC.Turning.Diameter.in ~ Frame.Underbody.Width +  Running.Gear.Track.Width +
                      +Frame.Axle.Mount.Inset + Frame.Underbody.Length +
                      Frame.Overbody.Height + Frame.FCC.Loading.Style + Steering.Type.Name, data = simulated_data)
summary(fcc_turn_test2)

fcc_turn_test3 <- lm(SMET.FCC.CtoC.Turning.Diameter.in ~ Frame.Underbody.Width +  Running.Gear.Track.Width +
                      +Frame.Axle.Mount.Inset + Frame.Underbody.Length +
                      Frame.Overbody.Height + Frame.Shape + Steering.Type.Name, data = simulated_data)
summary(fcc_turn_test3)

fcc_turn_test4 <- lm(SMET.FCC.CtoC.Turning.Diameter.in ~ Frame.Underbody.Width +  Running.Gear.Track.Width +
                      +Frame.Axle.Mount.Inset + Frame.Underbody.Length +
                      Frame.Overbody.Height + Frame.Shape + Frame.FCC.Loading.Style, data = simulated_data)
summary(fcc_turn_test4)

fcc_turn_test5 <- lm(SMET.FCC.CtoC.Turning.Diameter.in ~ Frame.Underbody.Width +  Running.Gear.Track.Width +
                       +Frame.Axle.Mount.Inset + Frame.Underbody.Length +
                       Frame.Overbody.Height + Frame.Shape + Steering.Type.Name +
                       Frame.Underbody.Width:Frame.Shape +  Running.Gear.Track.Width:Frame.Shape +
                       +Frame.Axle.Mount.Inset:Frame.Shape + Frame.Underbody.Length:Frame.Shape +
                       Frame.Overbody.Height:Frame.Shape, data = simulated_data)
summary(fcc_turn_test5)

fcc_turn_test6 <- lm(SMET.FCC.CtoC.Turning.Diameter.in ~ Frame.Underbody.Width +  Running.Gear.Track.Width +
                       +Frame.Axle.Mount.Inset + Frame.Underbody.Length +
                       Frame.Overbody.Height + Frame.Shape + Steering.Type.Name +
                       Frame.Underbody.Width:Steering.Type.Name +  Running.Gear.Track.Width:Steering.Type.Name +
                       +Frame.Axle.Mount.Inset:Steering.Type.Name + Frame.Underbody.Length:Steering.Type.Name +
                       Frame.Overbody.Height:Steering.Type.Name, data = simulated_data)
summary(fcc_turn_test6)

################################################################################
###################***********************************##########################
################################################################################

#Let's look at relationship between true categorical variables.
table(simulated_data[,c("Frame.FCC.Loading.Style","Frame.Shape",
                        "Offload.Generator.Present","Running.Gear.Type",
                        "Steering.Type.Name","Winch.Present")])
table(simulated_data[,c("Frame.FCC.Loading.Style","Frame.Shape",
                        "Running.Gear.Type",
                        "Steering.Type.Name")])
table(simulated_data[,c("Frame.Shape",
                        "Running.Gear.Type",
                        "Steering.Type.Name","Offload.Generator.Present")])
table(simulated_data[,c("Frame.Shape",
                        "Running.Gear.Type",
                        "Steering.Type.Name","Winch.Present")])
