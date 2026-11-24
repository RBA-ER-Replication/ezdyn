function build_gali_anticipated(source_file)
% Build the baseline sticky-price/sticky-wage Gali model with policy news.
% Run build_gali_anticipated() to rebuild the bundled generated mod file.
% Pass a local DSGE_mod Gali_2015_chapter_6.mod path to regenerate that file.
% News eps_nu_1 is immediate; eps_nu_60 arrives 59 quarters after announcement.
% The source header is retained; unrelated upstream scenario/plot code is omitted.

script_dir = fileparts(mfilename('fullpath'));
dashboard_dir = fileparts(fileparts(script_dir));
model_file = fullfile(script_dir, 'Gali2015Ch6_anticipated.mod');
original_dir = pwd;
restore_dir = onCleanup(@() cd(original_dir));
addpath(fullfile(dashboard_dir, '..', '..', 'ezDynare'));
if isempty(which('dynare'))
    addpath('C:\Program Files (x86)\dynare 7.2\matlab');
end

if nargin > 0
    source = fileread(source_file);
    source = strrep(source, sprintf('\r\n'), newline);
    marker = 'stoch_simul(order = 1,irf=15,nograph,noprint)';
    position = strfind(source, marker);
    assert(~isempty(position), 'Expected baseline simulation command not found.');
    source = source(1:position(1)-1);
    original_equation = 'nu=rho_nu*nu(-1)+eps_nu;';
    assert(numel(strfind(source, original_equation)) == 1, 'Expected one monetary shock equation.');
    assert(numel(strfind(source, 'model(linear);')) == 1, 'Expected one linear model block.');
    declarations = [
        "@#define n_anticipated = 60"
        "var"
        "@#for horizon in 1:n_anticipated-1"
        "    nu_news_@{horizon}"
        "@#endfor"
        ";"
        "varexo"
        "@#for horizon in 1:n_anticipated"
        "    eps_nu_@{horizon}"
        "@#endfor"
        ";"
        "model(linear);"
    ];
    equations = [
        "nu=rho_nu*nu(-1)+eps_nu+eps_nu_1+nu_news_1(-1);"
        "@#for horizon in 1:n_anticipated-1"
        "@#if horizon < n_anticipated-1"
        "nu_news_@{horizon}=nu_news_@{horizon+1}(-1)+eps_nu_@{horizon+1};"
        "@#else"
        "nu_news_@{horizon}=eps_nu_@{horizon+1};"
        "@#endif"
        "@#endfor"
    ];
    source = strrep(source, 'model(linear);', char(join(declarations, newline)));
    source = strrep(source, original_equation, char(join(equations, newline)));
    ending = [
        "shocks;"
        "@#for horizon in 1:n_anticipated"
        "var eps_nu_@{horizon}; stderr 0;"
        "@#endfor"
        "end;"
        "stoch_simul(order=1,irf=0,nograph,noprint);"
    ];
    source = [source newline char(join(ending, newline)) newline];
    file_id = fopen(model_file, 'w');
    assert(file_id >= 0, 'Cannot write generated model.');
    fprintf(file_id, '%s', source);
    fclose(file_id);
end

assert(isfile(model_file), 'Supply the local upstream mod path for the first build.');
cd(script_dir);
dynare Gali2015Ch6_anticipated.mod nograph nostrict
results = load(fullfile(script_dir, 'Gali2015Ch6_anticipated', 'Output', 'Gali2015Ch6_anticipated_results.mat'), 'M_', 'oo_');
M_ = results.M_;
oo_ = results.oo_;
assert(M_.params(strcmp(M_.param_names, 'theta_p')) == 0.75);
assert(M_.params(strcmp(M_.param_names, 'theta_w')) == 0.75);
news_index = find(startsWith(M_.exo_names, 'eps_nu_'));
assert(numel(news_index) == 60 && all(M_.Sigma_e(news_index, :) == 0, 'all'));
surprise_index = find(strcmp(M_.exo_names, 'eps_nu'));
immediate_index = find(strcmp(M_.exo_names, 'eps_nu_1'));
assert(max(abs(oo_.dr.ghu(:, surprise_index) - oo_.dr.ghu(:, immediate_index))) < 1e-10);
if ~isfield(oo_.dr, 'kstate')
    assert(M_.maximum_lag == 1);
    state_positions = oo_.dr.inv_order_var(oo_.dr.state_var);
    oo_.dr.kstate = [state_positions(:), repmat(M_.maximum_lag + 1, numel(state_positions), 1)];
end
export_model_obj_json(M_, oo_, string(fullfile(dashboard_dir, 'inputs', 'Gali2015Ch6_anticipated')));
disp('Built Gali2015Ch6_anticipated.json: 60 policy shocks, unchanged sticky-price/wage calibration.');
end