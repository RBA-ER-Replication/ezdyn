
function cmd = build_dynare_cmd(base_cmd, macro_args)
% build_dynare_cmd Generates command to run dynare with predefined
% macroprocessor variables. 
% Hamish Sullivan, 2025
% See https://www.dynare.org/assets/team-presentations/macroprocessor.pdf
%
% Input Arguments:
% - base_cmd (string) - 
%   Basic dynare command to run. e.g. "main.mod" or "main.mod 'nostrict'
%   'noclearall'.
% - macro_args (struct)
%   A structure containing any you want to define. 
% Usage:
%   cmd = build_dynare_cmd("main.mod 'nostrict'", ...
%       {'DATAFILE','dynare_data_matrix_test', ...
%        'XLS_SHEET','DynareInput', ...
%        'XLS_RANGE','a1:ag113'});
%   returns: dynare test.mod -DDATAFILE="dynare_data_matrix_test" -DXLS_SHEET="DynareInput" -DXLS_RANGE="a1:ag113"

    parts = ["dynare", string(base_cmd)];

    % Iterate over struct
    keys = fieldnames(macro_args);
    for i = 1:numel(keys)
        key = keys{i};
        val = macro_args.(key);
        if isempty(val)
            parts(end+1) = "-D" + key; % flag only
        else
            parts(end+1) = "-D" + key + "=" + '"' + string(val) + '"';
        end
    end

    % Join into final command
    cmd = strjoin(parts, ' ');
end
