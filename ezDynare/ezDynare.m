function ezDynare(base_cmd, xls, macro_args, ...
     save_name, start_date, end_date, save_dir, xls_sheet)
% EZDYNARE 
% Run dynare, with macro args and other arguments predetermined by xls
% sheet
% Input Arguments:
% - save_name (string) - location to save to (without a file extension).
% Will save both .mat and .json files.
    arguments 
        % Set requirements for input arguments and defaults
        base_cmd (1,1) string = "main.mod";
        xls (1,1) string = "dynare_data_matrix";
        macro_args (1,1) struct = struct();
        save_name (1,1) string = "";
        start_date (1,1) datetime = NaT;
        end_date (1,1) datetime = NaT;
        save_dir (1,1) string = "./intermediates/model_objects";
        xls_sheet (1,1) string = "";
    end
    % Set default savename as the current time if no savename is provided.
    if save_name == ""
            save_name = string(datetime(), "yyyy_MM_dd_HH_mm_ss");
    end
    
    [range, sheet_name] = get_excel_range(xls + ".xlsx", start_date, end_date, xls_sheet);
    macro_args.DATAFILE = xls;
    macro_args.XLS_SHEET = sheet_name;
    macro_args.XLS_RANGE = range
    cmd = build_dynare_cmd(base_cmd, macro_args);
    disp("ezDynare: Running " + cmd);
    eval(cmd);
    % Save outputs to file 
    fpath = save_dir + "/" + save_name; 
    disp("ezDynare: Saving resulting M_,  oo_, and var_list_ objects to " + fpath+".mat");
    evalin('base', sprintf("save('%s.mat', 'M_', 'oo_', 'var_list_');", fpath))
    disp("ezDynare: Saving resulting M_,  oo_ objects to " + fpath +".json");
    evalin('base', sprintf("export_model_obj_json(M_, oo_, '%s');", fpath))
end 