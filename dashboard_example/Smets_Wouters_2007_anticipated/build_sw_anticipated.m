% Build the dashboard's Smets-Wouters (2007) model object, with anticipated
% monetary policy shocks - see ../README.md.
%
% Runs Smets_Wouters_2007_45_anticipated.mod (in this folder) through Dynare
% and exports the resulting M_/oo_ objects to JSON for ezdyn::read_dynare(),
% as ../inputs/SW_2007_45.json (the file usa_dashboard.R reads). Also exports
% the model's estimation sample (usmodel_data.mat) to ../inputs/usmodel_data.csv.
%
% The mod file adds anticipated monetary policy shocks em_1 to em_60 to
% Johannes Pfeifer's DSGE_mod replication of Smets and Wouters (2007): em_<i+1>
% is announced i quarters ahead. They have zero variance, so the estimation
% step gives the same results as the original. Like the original, the
% estimation command has mode_compute=0 and mh_replic=0: it evaluates the
% likelihood and runs the smoother at the posterior mode in usmodel_mode.mat,
% rather than re-estimating. Everything it needs is in this folder; it does
% not need the DSGE_mod repository.
%
% Then rebuild the metadata workbooks (Rscript build_model_metadata.R, from
% dashboard_example/), which read this JSON for SW's parameter values.
%
% Run from the repo root:
%   matlab -batch "run('dashboard_example/Smets_Wouters_2007_anticipated/build_sw_anticipated.m')"

script_dir = fileparts(mfilename('fullpath'));
dashboard_dir = fileparts(script_dir);
repo_root = fileparts(dashboard_dir);

% export_model_obj_json() lives in the repo's ezDynare folder.
addpath(fullfile(repo_root, 'ezDynare'));

% Dynare: use the one already on the MATLAB path, otherwise this folder.
% Change this to match your Dynare installation.
dynare_matlab_dir = 'C:\Program Files (x86)\dynare 6.4\matlab\';
if isempty(which('dynare'))
    if ~exist(dynare_matlab_dir, 'dir')
        error(['build_sw_anticipated: Dynare not found - add it to the MATLAB ' ...
            'path, or set dynare_matlab_dir at the top of this script.']);
    end
    addpath(dynare_matlab_dir);
end

output_dir = fullfile(dashboard_dir, 'inputs');
if ~exist(output_dir, 'dir')
    mkdir(output_dir);
end

% Dynare reads usmodel_data.mat and usmodel_mode.mat from the current folder.
cd(script_dir);
dynare Smets_Wouters_2007_45_anticipated.mod nograph nostrict

% Check the anticipated shocks follow ezdyn's naming (em_1, em_2, ...).
anticipated = regexp(M_.exo_names, '^em_\d+$', 'match', 'once');
n_anticipated = nnz(~cellfun(@isempty, anticipated));
anticipated_names = compose('em_%d', 1:n_anticipated);
if n_anticipated == 0 || ~all(ismember(anticipated_names, M_.exo_names))
    error('build_sw_anticipated: expected anticipated shocks em_1, em_2, ... in M_.exo_names.');
end

% Check the estimation and smoother don't use the anticipated shocks: they
% must have zero variance (so the smoother, whose shock covariance is
% M_.Sigma_e, attributes nothing to them) and zero smoothed values. Then the
% smoothed shocks and variables, and so the historical shock decomposition,
% are the same as the original model's.
anticipated_index = find(ismember(M_.exo_names, anticipated_names));
if any(M_.Sigma_e(anticipated_index, :) ~= 0, 'all') || any(M_.Sigma_e(:, anticipated_index) ~= 0, 'all')
    error('build_sw_anticipated: the anticipated shocks must have zero variance (see the shocks block).');
end
for name = anticipated_names
    if any(oo_.SmoothedShocks.(name{1}) ~= 0)
        error('build_sw_anticipated: smoothed %s is not zero - the anticipated shocks must not enter the estimation.', name{1});
    end
end

export_model_obj_json(M_, oo_, fullfile(output_dir, 'SW_2007_45'));
disp("build_sw_anticipated: wrote " + fullfile(output_dir, 'SW_2007_45.json') + ...
    " (" + n_anticipated + " anticipated monetary policy shocks)");

% The estimation sample, 1947Q3-2004Q4 (230 quarters), on the same scale as
% the model's observables (it is used with prefilter=0).
historical_data = load(fullfile(script_dir, 'usmodel_data.mat'));
historical_table = table( ...
    historical_data.dy, historical_data.dc, historical_data.dinve, ...
    historical_data.labobs, historical_data.pinfobs, historical_data.dw, historical_data.robs, ...
    'VariableNames', {'dy', 'dc', 'dinve', 'labobs', 'pinfobs', 'dw', 'robs'});
writetable(historical_table, fullfile(output_dir, 'usmodel_data.csv'));
disp("build_sw_anticipated: wrote " + fullfile(output_dir, 'usmodel_data.csv'));
