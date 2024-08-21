%% Initial Model - V2
% VIPR-GS Project 3.3
% Hannah Stewart
% 9/14/2021

% The purpose of the following code is to calculate four different 
% fundamental parameters (back deck overhang, running gear contact patch 
% area, SMET FCC length, and SMET FCC curb-to-curb turning diameter) given 
% certain input data.

% Note: This is an initial code meant to only calculate four parameters.  
% The final code will begin with the user identifying a functional 
% objective (FO) they would like to calculate, and the program will 
% identify and request that the user input relevant input constants with 
% which the program will calculate parameters.


clear;
clc;


Fund_Calc_Parameters = readtable('VirtualTradespaceModelVariables.xlsx','Sheet','Fund Calc Parameters','VariableNamingRule','preserve');
Data = table2array(readtable('1k-DataSet.csv','VariableNamingRule','Preserve'));
Input_Constants = readcell('VirtualTradespaceModelVariables.xlsx','Sheet','Input Constants Definition');

n = height(Data);
w = width(Data);


%% Preallocate Empty Arrays

Axle_Separation_in = zeros(n,1);

Back_Deck_Overhang_in = zeros(n,1);

Front_Deck_Overhang_in = zeros(n,1);

Inner_Turning_Radius = zeros(n,1);

Lower_Cargo_Storage_Volume_in3 = zeros(n,1);

Lower_Cargo_Width_in = zeros(n,1);

Max_Ackerman_Angle_deg = zeros(n,1);

Offload_Generator_Volume_in3 = zeros(n,1);

Running_Gear_Contact_Patch_Area = zeros(n,1);

SMET_FCC_CtoC_Turning_Diameter_in = zeros(n,1);

SMET_FCC_Length_in = zeros(n,1);

SMET_FCC_Width_in = zeros(n,1);

SMET_Platform_Length_in = zeros(n,1);

SMET_Platform_Width_in = zeros(n,1);

Total_FCC_Load_Volume_in3 = zeros(n,1);


%% Back Deck Overhang

for i = 1:n
    
    Back_Deck_Overhang_in(i,1) = max(0,Data(i,26)-Data(i,2));
    
end


%% Running Gear Contact Patch Area
    
for i = 1:n
    
    if Data(i,18) == 1 % Four-Wheeled
    
        Running_Gear_Contact_Patch_Area_in2(i,1) = 4*2*pi*Data(i,47)*Data(i,46)/360*Data(i,48);
    
    elseif Data(i,18) == 2 % Six-Wheeled
    
        Running_Gear_Contact_Patch_Area_in2(i,1) = 6*2*pi*Data(i,47)*Data(i,46)/360*Data(i,48);

    elseif Data(i,18) == 3 % Eight-Wheeled
    
        Running_Gear_Contact_Patch_Area_in2(i,1) = 8*2*pi*Data(i,47)*Data(i,46)/360*Data(i,48);

    elseif Data(i,18) == 4 % Mattracks
        
        Running_Gear_Contact_Patch_Area_in2(i,1) = 4*Data(i,35)*Data(i,38);
    
    elseif Data(i,18) == 5 % Oval Track
        
        Running_Gear_Contact_Patch_Area_in2(i,1) = 2*(Data(i,12)-2*Data(i,2))*Data(i,34);

    elseif Data(i,18) == 6 % Trapezoid Track
        
        if Data(i,28)>=Data(i,39)
            
            Running_Gear_Contact_Patch_Area_in2(i,1) = 2*(Data(i,12)-2*Data(i,2)-3.414*Data(i,39)-0.586*Data(i,28))*Data(i,34);
            
        else
            
            Running_Gear_Contact_Patch_Area_in2(i,1) = 2*(Data(i,12)-2*Data(i,2)-5.414*Data(i,39)+1.414*Data(i,28))*Data(i,34);
            
        end

    elseif Data(i,18) == 7 % Parallelogram Track
        
        Running_Gear_Contact_Patch_Area_in2(i,1) = 2*(Data(i,12)-2*Data(i,2)-1.414*Data(i,39)+1.414*Data(i,28))*Data(i,34);

    else
    end

end


%% SMET FCC Length

for i = 1:n

% Front Deck Overhang

    a = max(0,(Data(i,27)-Data(i,2)));
    b = max(0,Data(i,55));
    Front_Deck_Overhang_in(i,1) = max(a,b);

    
SMET_FCC_Length_in(i,1) = Data(i,12)+Front_Deck_Overhang_in(i,1)+Back_Deck_Overhang_in(i,1);

end


%% SMET FCC Curb-to-Curb Turning Diameter

for i = 1:n

    Total_FCC_Load_Volume_in3(i,1) = (9*Input_Constants{51,3}*Input_Constants{48,3}*Input_Constants{49,3}+Input_Constants{57,3}*153+Input_Constants{54,3}*Input_Constants{76,3}+Input_Constants{55,3}*Input_Constants{77,3}+Input_Constants{56,3}*231.001)/Input_Constants{60,3};

% Lower Cargo Storage Volume

    Offload_Generator_Volume_in3(i,1) = Data(i,16)*Data(i,17)*Data(i,15);
    
    SMET_Platform_Width_in(i,1) = Data(i,13)+2*Data(i,34);
    
    SMET_Platform_Length_in(i,1) = Data(i,12)+Front_Deck_Overhang_in(i,1)+Back_Deck_Overhang_in(i,1);
    
    if Data(i,8) == 1 % Box
        
        Lower_Cargo_Storage_Volume_in3(i,1) = 0;
        
    elseif Data(i,9) == 1 % MUTT
        
        Lower_Cargo_Storage_Volume_in3(i,1) = SMET_Platform_Length_in(i,1)*Data(i,13)*Data(i,6);
    
    elseif Data(i,4) == 1 % Flush with Frame
        
        Lower_Cargo_Width_in(i,1) = SMET_Platform_Width_in(i,1);
        
    elseif Data(i,5) == 1 % Hanging over Frame
        
        Lower_Cargo_Width_in(i,1) = SMET_Platform_Width_in(i,1)+2*Input_Constants{49,3};
    
    elseif Data(i,10) == 1 % Protector
        
        Lower_Cargo_Storage_Volume_in3(i,1) = SMET_Platform_Length_in(i,1)-0.65*Data(i,12)-Front_Deck_Overhang_in(i,1)*Lower_Cargo_Width_in(i,1)*Data(i,6);
    
    elseif Data(i,11) == 1 % SMSS
    
        Lower_Cargo_Storage_Volume_in3(i,1) = SMET_Platform_Length_in(i,1)-0.35*Data(i,12)-Front_Deck_Overhang_in(i,1)*Lower_Cargo_Width_in(i,1)*Data(i,6)+2*0.35*Data(i,12)*(Lower_Cargo_Width_in(i,1)-Data(i,13))*Data(i,6);
    else
    end
        
% SMET FCC Width        
    
    if Data(i,9) == 1 % MUTT
        
        if Lower_Cargo_Storage_Volume_in3(i,1) >= Total_FCC_Load_Volume_in3(i,1)+Offload_Generator_Volume_in3(i,1)

            SMET_FCC_Width_in(i,1) = SMET_Platform_Width_in(i,1);
            
        else
        end
            
    elseif Data(i,4) == 1 % Flush with Frame
        
        SMET_FCC_Width_in(i,1) = SMET_Platform_Width_in(i,1);
        
    elseif Data(i,5) == 1 % Hanging over Frame
        
        SMET_FCC_Width_in(i,1) = SMET_Platform_Width_in(i,1)+2*Input_Constants{49,3};
        
    else
    end
    
% SMET FCC Curb-to-Curb Turning Diameter

    Axle_Separation_in(i,1) = Data(i,12)-2*Data(i,2);

    if Data(i,50) == 1 % Pivot
        
        SMET_FCC_CtoC_Turning_Diameter_in(i,1) = sqrt((SMET_FCC_Length_in(i,1))^2+(SMET_FCC_Width_in(i,1))^2);

    elseif Data(i,51) == 1 % Skid
        
        SMET_FCC_CtoC_Turning_Diameter_in(i,1) = 2*sqrt((SMET_FCC_Length_in(i,1)/2)^2+((SMET_FCC_Width_in(i,1)+SMET_Platform_Width_in(i,1)-Data(i,34))/2)^2);

    else
        
        if Data(i,19) == 1 % Four-Wheeled
        
            Max_Ackerman_Angle_deg(i,1) = Input_Constants{47,3};
    
        elseif Data(i,20) == 1 % Six-Wheeled
        
            Max_Ackerman_Angle_deg(i,1) = Input_Constants{47,3};
            
        elseif Data(i,21) == 1 % Eight-Wheeled
        
            Max_Ackerman_Angle_deg(i,1) = Input_Constants{47,3};
            
        elseif Data(i,22) == 1 % Oval Track
        
            Max_Ackerman_Angle_deg(i,1) = Input_Constants{46,3};
            
        elseif Data(i,23) == 1 % Trapezoidal Track
        
            Max_Ackerman_Angle_deg(i,1) = Input_Constants{46,3};
            
        elseif Data(i,24) == 1 % Parallelogram Track
        
            Max_Ackerman_Angle_deg(i,1) = Input_Constants{46,3};
        
        elseif Data(i,25) == 1 % Mattrack
        
            Max_Ackerman_Angle_deg(i,1) = Input_Constants{46,3};
        
        else
        end
        
        if Data(i,52) == 1 % Single Ackerman
            
            Inner_Turning_Radius_in(i,1) = Axle_Separation_in(i,1)/tan(Max_Ackerman_Angle_deg(i,1));
            
        elseif Data(i,53) == 1 % Dual Ackerman
            
            Inner_Turning_Radius_in(i,1) = 0.5*Axle_Separation_in(i,1)/tan(Max_Ackerman_Angle_deg(i,1));
            
        else
        end
        
    SMET_FCC_CtoC_Turning_Diameter_in(i,1) = SMET_FCC_Width_in(i,1)+SMET_Platform_Width_in(i,1)-Data(i,34)+2*Inner_Turning_Radius_in(i,1);
        
    end   

end


%% Export to Excel

Data(:,w+1) = Back_Deck_Overhang_in;

Data(:,w+2) = Running_Gear_Contact_Patch_Area_in2;

Data(:,w+3) = SMET_FCC_Length_in;

Data(:,w+4) = SMET_FCC_CtoC_Turning_Diameter_in;

Title_New = {'Back Deck Overhang (in)','Running Gear Contact Patch Area (in2)','SMET FCC Length (in)','SMET FCC Curb-to-Curb Turning Diameter (in)'};

writematrix(Data,'FinalData.csv');
