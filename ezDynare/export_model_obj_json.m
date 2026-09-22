function export_model_obj_json(M_, oo_, fpath)
% EXPORT_MODEL_OBJ_JSON Export a Dynare model object to JSON.
% This is used for reading the data into R, as there are issues 
% with the R.matlab readMat package, and workarounds using mat v7.3 
% HDFS were unwieldly. 
% Note: oo_.dr eigenvalues is removed, because JSON export cannot
% handle complex numbers. 
%
%
% Input Arguments:
% - M_ (struct) - 
%   M_ dynare object. 
% - oo_ (struct) -
%   oo_ dynare object.
% - fpath (string) - 
%   File path to save to. 

    % Remove eigenvalues
    oo_.dr.eigval = [];
    % Determine file path
    save_name_json = fpath + ".json";
    out_json = jsonencode(struct(M_=M_, oo_=oo_));
    writelines(out_json, save_name_json)
end