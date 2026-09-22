% Build MOO objects for Smets-Wouters (2007) plus a curated set of Pfeifer
% DSGE_mod NK models - see README.md in this folder for full setup steps.
%
% Prerequisite: clone the Pfeifer DSGE_mod collection into this folder as
% "DSGE_mod":
%   git clone https://github.com/JohannesPfeifer/DSGE_mod.git DSGE_mod
% (run from dashboard/DSGEModDash - the folder name must be exactly
% "DSGE_mod", matching what a fresh `git clone` of that repo produces).
%
% Runs each replication .mod file directly through Dynare and exports the
% resulting M_/oo_ objects to JSON for ezdyn::read_dynare(). Smets-Wouters
% (Smets_Wouters_2007_45.mod) already has mode_compute=0, mh_replic=0 baked
% into its own `estimation(...)` call, so it just evaluates the
% likelihood/smoother at the supplied posterior mode (no optimization or
% MCMC) rather than actually estimating. The other 7 models are calibrated
% (no `estimation` command), solved via stoch_simul(order=1).
%
% Two of the seven curated NK models leave their FINAL decision rule in a
% non-baseline parameterization after running the paper's own multi-scenario
% script (Gali_2015_chapter_6 ends in a flexible-price case;
% Ascari_Sbordone_2014 ends at 6% trend inflation) - for those we reset the
% relevant parameter(s) and call stoch_simul() once more (with no variable
% list, which still recomputes oo_.dr.ghx/ghu for ALL endogenous variables)
% before export.
%
% Run from the repo root: matlab.exe -batch "run('dashboard/DSGEModDash/build_new_models.m')"

script_dir = fileparts(mfilename('fullpath'));
repo_root = fileparts(fileparts(script_dir));

addpath(genpath(fullfile(repo_root, 'functions')));
addpath('C:\Program Files (x86)\dynare 6.4\matlab\');

output_dir = fullfile(script_dir, 'inputs');
if ~exist(output_dir, 'dir')
    mkdir(output_dir);
end

toolbox_dir = fullfile(script_dir, 'DSGE_mod');
if ~exist(toolbox_dir, 'dir')
    error(['build_new_models: ' toolbox_dir ' not found - clone ' ...
        'https://github.com/JohannesPfeifer/DSGE_mod.git into this folder ' ...
        'as "DSGE_mod" first (see README.md).']);
end

%% Smets_Wouters_2007 - flagship estimated NK model (Smets & Wouters, 2007)
try
    clearvars -except script_dir repo_root output_dir toolbox_dir
    model_dir = fullfile(toolbox_dir, 'Smets_Wouters_2007');
    cd(model_dir);
    dynare Smets_Wouters_2007_45.mod nograph nostrict
    cd(script_dir);
    export_model_obj_json(M_, oo_, fullfile(output_dir, 'SW_2007_45'));
    disp("build_new_models: wrote " + fullfile(output_dir, 'SW_2007_45.json'));

    % Also export the real historical estimation sample (usmodel_data.mat,
    % the same varobs series used above) to CSV, so usa_dashboard.R can use
    % it as the dashboard's real baseline history instead of a synthetic
    % series. This is the standard Dynare/Pfeifer Smets-Wouters (2007) US
    % dataset, 1947Q3-2004Q4 (230 quarters), fed to the model with no
    % prefiltering (prefilter=0), so the raw values are already on the same
    % scale as the model's own observables (dy, dc, dinve, labobs, pinfobs,
    % dw, robs).
    historical_data = load(fullfile(model_dir, 'usmodel_data.mat'));
    historical_table = table( ...
        historical_data.dy, historical_data.dc, historical_data.dinve, ...
        historical_data.labobs, historical_data.pinfobs, historical_data.dw, historical_data.robs, ...
        'VariableNames', {'dy', 'dc', 'dinve', 'labobs', 'pinfobs', 'dw', 'robs'});
    writetable(historical_table, fullfile(output_dir, 'usmodel_data.csv'));
    disp("build_new_models: wrote " + fullfile(output_dir, 'usmodel_data.csv'));
catch ME
    cd(script_dir);
    fprintf(2, "FAILED SW_2007_45: %s\n", ME.message);
end

%% Gali_2015_chapter_3 - flagship baseline 3-equation NK model
try
    clearvars -except script_dir repo_root output_dir toolbox_dir
    cd(fullfile(toolbox_dir, 'Gali_2015'));
    dynare Gali_2015_chapter_3.mod nograph nostrict
    cd(script_dir);
    export_model_obj_json(M_, oo_, fullfile(output_dir, 'Gali2015Ch3'));
    disp("build_new_models: wrote " + fullfile(output_dir, 'Gali2015Ch3.json'));
catch ME
    cd(script_dir);
    fprintf(2, "FAILED Gali2015Ch3: %s\n", ME.message);
end

%% Gali_2015_chapter_6 - NK model with price AND wage rigidities
try
    clearvars -except script_dir repo_root output_dir toolbox_dir
    cd(fullfile(toolbox_dir, 'Gali_2015'));
    dynare Gali_2015_chapter_6.mod nograph nostrict
    % The file's own script ends in the flexible-price case (theta_p~0).
    % Reset to the baseline sticky price+wage calibration and recompute the
    % decision rule before export. stoch_simul must be called via its raw
    % MATLAB signature here (not Dynare's preprocessor-only name=value form).
    set_param_value('theta_w', 3/4);
    set_param_value('theta_p', 3/4);
    options_.order = 1;
    options_.irf = 0;
    options_.nograph = 1;
    options_.noprint = 1;
    [info, oo_, options_] = stoch_simul(M_, options_, oo_, {});
    cd(script_dir);
    export_model_obj_json(M_, oo_, fullfile(output_dir, 'Gali2015Ch6'));
    disp("build_new_models: wrote " + fullfile(output_dir, 'Gali2015Ch6.json'));
catch ME
    cd(script_dir);
    fprintf(2, "FAILED Gali2015Ch6: %s\n", ME.message);
end

%% Gali_2015_chapter_8 - small open economy NK model (FDIT Taylor rule, default)
try
    clearvars -except script_dir repo_root output_dir toolbox_dir
    cd(fullfile(toolbox_dir, 'Gali_2015'));
    dynare Gali_2015_chapter_8.mod nograph nostrict
    cd(script_dir);
    export_model_obj_json(M_, oo_, fullfile(output_dir, 'Gali2015Ch8'));
    disp("build_new_models: wrote " + fullfile(output_dir, 'Gali2015Ch8.json'));
catch ME
    cd(script_dir);
    fprintf(2, "FAILED Gali2015Ch8: %s\n", ME.message);
end

%% Gali_2010 - sticky wage model with unemployment (Handbook of Monetary Economics ch.10)
try
    clearvars -except script_dir repo_root output_dir toolbox_dir
    cd(fullfile(toolbox_dir, 'Gali_2010'));
    dynare Gali_2010.mod nograph nostrict
    cd(script_dir);
    export_model_obj_json(M_, oo_, fullfile(output_dir, 'Gali2010'));
    disp("build_new_models: wrote " + fullfile(output_dir, 'Gali2010.json'));
catch ME
    cd(script_dir);
    fprintf(2, "FAILED Gali2010: %s\n", ME.message);
end

%% Ireland_2004 - estimated NK model (calibrated at post-1980 posterior values)
try
    clearvars -except script_dir repo_root output_dir toolbox_dir
    cd(fullfile(toolbox_dir, 'Ireland_2004'));
    dynare Ireland_2004.mod nograph nostrict
    cd(script_dir);
    export_model_obj_json(M_, oo_, fullfile(output_dir, 'Ireland2004'));
    disp("build_new_models: wrote " + fullfile(output_dir, 'Ireland2004.json'));
catch ME
    cd(script_dir);
    fprintf(2, "FAILED Ireland2004: %s\n", ME.message);
end

%% Ascari_Sbordone_2014 - trend inflation NK model
% The original file's own post-`check;` script runs several slow scenario
% loops (IRF figures at multiple trend-inflation values, a business-cycle
% moments comparison, and a dense determinacy-region grid search) that are
% unnecessary for a single baseline MOO export and were observed to take a
% very long time. Build a trimmed copy that keeps everything up to (and
% including) `check;` and then adds a single, fast baseline stoch_simul call
% of our own.
try
    clearvars -except script_dir repo_root output_dir toolbox_dir
    model_dir = fullfile(toolbox_dir, 'Ascari_Sbordone_2014');
    cd(model_dir);
    raw = fileread('Ascari_Sbordone_2014.mod');
    marker = 'options_.qz_criterium = 1+1e-6; //make sure the option is set';
    idx = strfind(raw, marker);
    if isempty(idx)
        error('Could not find truncation marker in Ascari_Sbordone_2014.mod - source file may have changed.');
    end
    trimmed = raw(1:idx(1)+length(marker)-1);
    trimmed = [trimmed newline ...
        'steady;' newline ...
        'check;' newline ...
        'shocks;' newline ...
        '    var e_v; stderr 1;' newline ...
        '    var e_a; stderr 0;' newline ...
        '    var e_zeta; stderr 0;' newline ...
        'end;' newline ...
        'stoch_simul(order=1,irf=0,nograph,noprint) y pi real_interest s i v;' newline];
    trimmed_fname = 'Ascari_Sbordone_2014_dashboard.mod';
    fid = fopen(trimmed_fname, 'w');
    fprintf(fid, '%s', trimmed);
    fclose(fid);

    dynare Ascari_Sbordone_2014_dashboard.mod nograph nostrict
    cd(script_dir);
    export_model_obj_json(M_, oo_, fullfile(output_dir, 'AscariSbordone2014'));
    disp("build_new_models: wrote " + fullfile(output_dir, 'AscariSbordone2014.json'));
catch ME
    cd(script_dir);
    fprintf(2, "FAILED AscariSbordone2014: %s\n", ME.message);
end

%% Born_Pfeifer_2018_MP - Calvo (default) vs Rotemberg wage-setting NK model
try
    clearvars -except script_dir repo_root output_dir toolbox_dir
    cd(fullfile(toolbox_dir, 'Born_Pfeifer_2018', 'Monetary_Policy_IRFs'));
    dynare Born_Pfeifer_2018_MP.mod nograph nostrict
    cd(script_dir);
    export_model_obj_json(M_, oo_, fullfile(output_dir, 'BornPfeifer2018MP'));
    disp("build_new_models: wrote " + fullfile(output_dir, 'BornPfeifer2018MP.json'));
catch ME
    cd(script_dir);
    fprintf(2, "FAILED BornPfeifer2018MP: %s\n", ME.message);
end

cd(script_dir);
disp("build_new_models: done.");
