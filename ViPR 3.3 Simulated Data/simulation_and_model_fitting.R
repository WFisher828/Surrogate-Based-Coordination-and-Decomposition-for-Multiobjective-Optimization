library(dplyr)

#Need to create a dataset of the runs.

#cleaned_data <- read.csv("FinalDataCleaned.csv")

num_data <- 1000
set.seed(100)
frame_axle_mount_inset <- sample.int(15,size = num_data, replace = TRUE)
frame_fcc <- rep(1,num_data)#sample.int(2,size = num_data, replace = TRUE)
frame_overbody_height <- rep(0,num_data)
frame_shape <- rep(1,num_data)
frame_underbody_length <- sample.int(99,size=num_data, replace = TRUE) + 33
frame_underbody_width <- sample.int(43,size=num_data, replace = TRUE) + 15
offload_generator_present <- rep(1,num_data)
offload_generator_height <- runif(num_data,min = 10,max = 40)
offload_generator_length <- runif(num_data,min = 10,max = 40)
offload_generator_width <- runif(num_data,min = 10,max = 40)
running_gear_type <- rep(6,num_data)
running_gear_back_mount_inset <- sample.int(16,size = num_data, replace = TRUE) + 4
running_gear_front_mount_inset <- sample.int(16,size = num_data, replace = TRUE) + 4
running_gear_drive_gear_radius <- sample.int(3,size = num_data,replace = TRUE)
running_gear_drive_gear_width <- rep(13,num_data)
running_gear_track_thickness <- sample.int(2, size=num_data,replace = TRUE) #Not used
running_gear_track_width <- sample.int(2,size=num_data,replace = TRUE) + 13
running_gear_mattrack_ground_contract_length <- rep(0,num_data)
running_gear_mattrack_inset_length <- rep(0,num_data)
running_gear_mattrack_length <- rep(0,num_data)
running_gear_mattrack_track_width <- rep(0,num_data)
running_gear_road_wheel_radius <- sample.int(5,size = num_data,replace = TRUE)
running_gear_road_wheel_width <- rep(0,num_data)
running_gear_wheel_contact_patch_arc <- rep(0,num_data)
running_gear_wheel_radius <- sample.int(10,size = num_data,replace = TRUE) + 10
running_gear_wheel_width <- rep(1,num_data)
steering_type_name <- rep(3,num_data)
winch_present <- rep(1,num_data)
winch_stowed_width <- runif(num_data, min = 2, max = 9)

new_simulated_data <- data.frame(data_point = 1:num_data,frame_axle_mount_inset = frame_axle_mount_inset,
                                frame_fcc = frame_fcc,
                                frame_overbody_height = frame_overbody_height,
                                frame_shape = frame_shape,
                                frame_underbody_length = frame_underbody_length,
                                frame_underbody_width = frame_underbody_width,
                                offload_generator_present = offload_generator_present,
                                offload_generator_height = offload_generator_height,
                                offload_generator_length = offload_generator_length,
                                offload_generator_width = offload_generator_width,
                                running_gear_type = running_gear_type,
                                running_gear_back_mount_inset = running_gear_back_mount_inset,
                                running_gear_front_mount_inset = running_gear_front_mount_inset,
                                running_gear_drive_gear_radius = running_gear_drive_gear_radius,
                                running_gear_drive_gear_width = running_gear_drive_gear_width,
                                running_gear_track_thickness = running_gear_track_thickness,
                                running_gear_track_width = running_gear_track_width,
                                running_gear_mattrack_ground_contract_length = running_gear_mattrack_ground_contract_length,
                                running_gear_mattrack_inset_length = running_gear_mattrack_inset_length,
                                running_gear_mattrack_length = running_gear_mattrack_length,
                                running_gear_mattrack_track_width = running_gear_mattrack_track_width,
                                running_gear_road_wheel_radius = running_gear_road_wheel_radius,
                                running_gear_road_wheel_width = running_gear_road_wheel_width,
                                running_gear_wheel_contact_patch_arc = running_gear_wheel_contact_patch_arc,
                                running_gear_wheel_radius = running_gear_wheel_radius,
                                running_gear_wheel_width = running_gear_wheel_width,
                                steering_type_name = steering_type_name,
                                winch_present = winch_present,
                                winch_stowed_width = winch_stowed_width)

back_deck_overhang_in <- c()
running_gear_contact_patch_area_in2 <- c()
smet_fcc_length_in <- c()
smet_fcc_ctoc_turning_diameter_in <- c()

for (i in 1:num_data){
  response <- response_simulator(as.numeric(new_simulated_data[i,]))
  back_deck_overhang_in <- append(back_deck_overhang_in,c(response[1]))
  running_gear_contact_patch_area_in2 <- append(running_gear_contact_patch_area_in2,c(response[2]))
  smet_fcc_length_in <- append(smet_fcc_length_in,c(response[3]))
  smet_fcc_ctoc_turning_diameter_in <- append(smet_fcc_ctoc_turning_diameter_in,c(response[4]))
}

#new_simulated_data$back_deck_overhang_in <- back_deck_overhang_in
#new_simulated_data$running_gear_contact_patch_area_in2 <- running_gear_contact_patch_area_in2
#new_simulated_data$smet_fcc_length_in <- smet_fcc_length_in
#new_simulated_data$smet_fcc_ctoc_turning_diameter_in <- smet_fcc_ctoc_turning_diameter_in

#Create a dataframe for analysis purpose.

analysis_df <- data.frame(frame_axle_mount_inset = frame_axle_mount_inset,
                          frame_underbody_length = frame_underbody_length,
                          frame_underbody_width = frame_underbody_width,
                          offload_generator_height = offload_generator_height,
                          offload_generator_length = offload_generator_length,
                          offload_generator_width = offload_generator_width,
                          running_gear_back_mount_inset = running_gear_back_mount_inset,
                          running_gear_front_mount_inset = running_gear_front_mount_inset,
                          running_gear_drive_gear_radius = running_gear_drive_gear_radius,
                          #running_gear_track_thickness = running_gear_track_thickness,
                          running_gear_track_width = running_gear_track_width,
                          running_gear_road_wheel_radius = running_gear_road_wheel_radius,
                          running_gear_wheel_radius = running_gear_wheel_radius,
                          winch_stowed_width = winch_stowed_width,
                          back_deck_overhang_in = back_deck_overhang_in,
                          running_gear_contact_patch_area_in2 = running_gear_contact_patch_area_in2,
                          smet_fcc_length_in = smet_fcc_length_in,
                          smet_fcc_ctoc_turning_diameter_in = smet_fcc_ctoc_turning_diameter_in)

#Don't want I(running_gear_track_width^2) because there
#are only two levels

standardize_analysis_df <- analysis_df %>% mutate_all(~(scale(.) %>% as.vector))



back_deck_mod <- lm(back_deck_overhang_in ~ .^2 + I(frame_axle_mount_inset^2) + I(frame_underbody_length^2) +
                      I(frame_underbody_width^2) + I(offload_generator_height^2) + I(offload_generator_length^2)+
                      I(offload_generator_width^2) + I(running_gear_back_mount_inset^2) + 
                      I(running_gear_front_mount_inset^2) + I(running_gear_drive_gear_radius^2) +
                      I(running_gear_road_wheel_radius^2) + I(running_gear_wheel_radius^2) +
                      I(winch_stowed_width^2), data = standardize_analysis_df[,-c(15,16,17)])

summary(back_deck_mod)


running_gear_mod <- lm(running_gear_contact_patch_area_in2 ~ .^2 + I(frame_axle_mount_inset^2) + I(frame_underbody_length^2) +
                      I(frame_underbody_width^2) + I(offload_generator_height^2) + I(offload_generator_length^2)+
                      I(offload_generator_width^2) + I(running_gear_back_mount_inset^2) + 
                      I(running_gear_front_mount_inset^2) + I(running_gear_drive_gear_radius^2) +
                      I(running_gear_road_wheel_radius^2) + I(running_gear_wheel_radius^2) +
                      I(winch_stowed_width^2), data = standardize_analysis_df[,-c(14,16,17)])


summary(running_gear_mod)

smet_fcc_length_mod <- lm(smet_fcc_length_in ~ .^2 + I(frame_axle_mount_inset^2) + I(frame_underbody_length^2) +
                      I(frame_underbody_width^2) + I(offload_generator_height^2) + I(offload_generator_length^2)+
                      I(offload_generator_width^2) + I(running_gear_back_mount_inset^2) + 
                      I(running_gear_front_mount_inset^2) + I(running_gear_drive_gear_radius^2) +
                      I(running_gear_road_wheel_radius^2) + I(running_gear_wheel_radius^2) +
                      I(winch_stowed_width^2), data = standardize_analysis_df[,-c(14,15,17)])

summary(smet_fcc_length_mod)

smet_fcc_turning_mod <- lm(smet_fcc_ctoc_turning_diameter_in ~ .^2 + I(frame_axle_mount_inset^2) + I(frame_underbody_length^2) +
                      I(frame_underbody_width^2) + I(offload_generator_height^2) + I(offload_generator_length^2)+
                      I(offload_generator_width^2) + I(running_gear_back_mount_inset^2) + 
                      I(running_gear_front_mount_inset^2) + I(running_gear_drive_gear_radius^2) +
                      I(running_gear_road_wheel_radius^2) + I(running_gear_wheel_radius^2) +
                      I(winch_stowed_width^2), data = standardize_analysis_df[,-c(14,15,16)])

summary(smet_fcc_turning_mod)

#Check correlation between variables, correlations should be close to 0.
cor(standardize_analysis_df[,-c(14,15,16,17)])
