%%  Plot and explore tradespace.
%   ViPR-GS Project 3.3
%   Author: Philip de Castro
%   Fall 2021 - Spring 2022

%   In this code, we import the produced simulated data and perform one
%   step of the Decomposition & Coordination scheme, where we decompose
%   into two subproblems: SP1 and SP2. Note that in this problem, we have
%   the following structure:
%       Vars in SP1: x_global
%       Vars in SP2: x_global & x_local.
%   Since SP1 only has global variables, we let SP1 be the preferred
%   subproblem for our coordination.

clc; clear all; close all;

%%  Initialize

% Load in the simulated data
run 'Initial_Model_V2.m'

%   Define SP1
%   Round each function value to 5 significant digits
Back_Deck_Overhang = round(Data(:,w+1), 5, 'significant');
SMET_FCC_Length = round(Data(:,w+3), 5, 'significant');

%   Define SP2
%   Round each function value to 5 significant digits.
Running_Gear_Contact_Patch_Area = -round(Data(:,w+2), 5, 'significant');
SMET_FCC_CtoC_Turning_Diameter = round(Data(:,w+4), 5, 'significant');

%%  Plot ONLY OUTPUTS

%   SP1
subplot(1,2,1);
scatter(Back_Deck_Overhang, SMET_FCC_Length, 200,'filled', 'MarkerFaceColor','#999999');
title('\textbf{Subproblem 1}','Interpreter','latex','FontSize',25);
xlabel('\textbf{Back Deck Overhang} $\mathbf{(f_1)}$','Interpreter','latex','FontSize',20); 
ylabel('\textbf{SMET FCC Length} $\mathbf{(f_3)}$','Interpreter','latex','FontSize',20);

%   SP2
subplot(1,2,2);
scatter(Running_Gear_Contact_Patch_Area, SMET_FCC_CtoC_Turning_Diameter, 200, '*', 'MarkerEdgeColor', '#999999');
title('\textbf{Subproblem 2}','Interpreter','latex','FontSize',25);
xlabel('\textbf{-Running Gear Contact Patch Area} $\mathbf{(-f_2)}$','Interpreter','latex','FontSize',20);
ylabel('\textbf{SMET FCC CtoC Turning Diameter} $\mathbf{(f_4)}$','Interpreter','latex','FontSize',20);


%% Relaxation and Coordination: SP1 is preferred.
%   The preferred point is (0, 35)

%   This is how we find the preimage of a point in the outcome space.
%   Matlab's "find", as set up below, will find the row index of
%   Back_Deck_Overhang and SMET_FCC_Length which corresponds to the values
%   of 0 and 35, respectively.
    [preferred_index, ~] = find(Back_Deck_Overhang == 0 & ...
        SMET_FCC_Length == 35);
%   Don't worry about the code below this point.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Define the preferred point
    f1_pref = 0;
    f3_pref = 35;
    % Define the relaxation
    f1_relax = 4;
    f3_relax = 15;
    % Find all of the points within the relaxation
    [row,~] = find(Back_Deck_Overhang >= f1_pref &...
        Back_Deck_Overhang <= f1_pref+f1_relax &...
        SMET_FCC_Length >= f3_pref &...
        SMET_FCC_Length <= f3_pref + f3_relax);
    % This is the preimage of all points in the relaxation region.
    preimage = Data(row,:);
    % These are the column indices which correspond to the global
    % variables.
    global_indices = [2 12 15 47 55 26 27 36 18 28 19 20 21 22 23 24 25 29 30 31 54];
    % Go through and save all of the solutions that have a common global
    % variable
    coord = zeros(1,w+4);
    row_num = 1;
    for i = 1:n
        for j = 1:length(row)
            if Data(i,global_indices) == preimage(j,global_indices)
                coord(row_num,:) = Data(i,:);
                row_num = row_num + 1;
            end
        end
    end
%%  Plot the results from the coordination.
    %   SP1
    subplot(1,2,1);
    hold on;
    scatter(coord(:,w+1), coord(:,w+3),200, 'k', 'LineWidth',3);
    hold on;
    quiver(f1_pref,f3_pref,f1_relax,0,'m','LineWidth',2,'AutoScale','off');
    hold on;
    quiver(f1_pref,f3_pref,0,f3_relax,'m','LineWidth',2,'AutoScale','off');
    hold on;
    scatter(f1_pref, f3_pref, 200,'g', 'LineWidth', 3);
    %   SP2
    subplot(1,2,2);
    hold on;
    scatter(-coord(:,w+2), coord(:,w+4), 200, 'k', 'LineWidth', 3);
    hold on;
    scatter(-Data(preferred_index,w+2), Data(preferred_index, w+4), 200, 'g', 'LineWidth', 3);
    
%%  Let's do the next step in coordination: 
%   Consider 5 pts in SP2 and see how they perform in SP1 to compare 
%   the relaxations.
    
    % These are the points of interest (POIs) from SP2
    [selected1,~] = find(-Data(:,w+2) == -870 & Data(:,w+4) == 43);
    [selected2,~] = find(-Data(:,w+2) == -952 & Data(:,w+4) == 49);
    [selected3,~] = find(-Data(:,w+2) == -952 & Data(:,w+4) == 75.244763272934820);
    [selected4,~] = find(-Data(:,w+2) == -1036 & Data(:,w+4) == 48.5);
    
    % We will plot each POI in a different color.
    points = {selected1 selected2 selected3 selected4};
    colors = {'#deff18', '#dd6f30', '#75007c','#31dde1','#009894'};
    for i = 1:4
       % Plot POI in SP2
       subplot(1,2,2);
       hold on;
       scatter(-Data(points{i},w+2), Data(points{i},w+4), 200, 'MarkerEdgeColor',colors{i}, 'LineWidth',3);
       % Plot POI in SP1
       subplot(1,2,1);
       hold on;
       scatter(Data(points{i},w+1), Data(points{i}, w+3), 200, 'MarkerEdgeColor',colors{i}, 'LineWidth',3);
    end










