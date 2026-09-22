function [range, sheet] = get_excel_range(xls, start_date, end_date, sheet)
% GET_EXCEL_RANGE Retrieve excel range (e.g. A1:AA123) for Dynare input.
%
%  Note: Using any other start date other than default or the first row in
%  the sheet may not work. TODO: Work out how to fix this. 
%
% Syntax: 
%   [range, sheet] = get_excel_range(xls) Reads all dates in the excel
%   file, and assumes the first sheet in the workbook is to be used. 
% 
%   [range, sheet] = get_excel_range(xls, start_date) Restricts the sample
%   range to the first row in the sheet onwards, and assumes first sheet in
%   the workbook is to be used. 
% 
% Input Arguments:
% - xls (string) - 
%   path to an excel workbook containing the dynare data. 
% - start_date (datetime) -
%   First date in the excel sheet that you want to be included in the excel
%   range. It is inclusive.
% - end_date (datetime) - 
%   Last date in the excel sheet that you want to be included in the excel
%   range. If not provided, assumes that you don't want to restrict the end
%   date. It is inclusive. 
% - sheet (string) Which excel sheet to read from in the workbook. Defaults
%   to the first sheet in the workbook.

    arguments 
        % Set requirements for input arguments and defaults
        xls (1,1) string = "dynare_data_matrix";
        start_date (1,1) datetime = NaT;
        end_date (1,1) datetime = NaT;
        sheet (1,1) string = ""
    end; 

    if sheet == "" 
       sheets= sheetnames(xls);
       sheet = sheets(1);
    end 

    t = readtable(xls);

    % Extract year and quarter with simple string ops
    if ismember('Var1', t.Properties.VariableNames)
        date_column = t.Var1;
    elseif ismember('date', t.Properties.VariableNames)
        date_column = t.date;
    else
        error("Input data must contain a 'Var1' or 'date' date column.")
    end
    if isdatetime(date_column)
        date_column = string(year(date_column)) + "Q" + string(quarter(date_column));
    elseif isnumeric(date_column)
        date_column = datetime(date_column, 'ConvertFrom', 'excel');
        date_column = string(year(date_column)) + "Q" + string(quarter(date_column));
    else
        date_column = string(date_column);
    end
    yearStr = extractBetween(date_column, 1, 4);
    qStr    = extractAfter(date_column, 'Q');

    % Convert to numeric; invalid become NaN
    Y = str2double(yearStr);
    Q = str2double(qStr);

    % Validate quarter (1..4); mark invalids as NaT
    valid = ~isnan(Y) & ismember(Q, 1:4);

    % Compute month for start of quarter: 1,4,7,10
    month = (Q - 1) * 3 + 1;

    % Build datetime; invalid rows yield NaT
    dt = datetime(Y, month, 1);
    dt(~valid) = NaT;

    % Get list of indexes which are within date range
    if isnat(start_date) 
       range_start = 1;
    else 
       ix = find(dt >= start_date);
       range_start = min(ix);
       % TODO: This probably won't work if anything except for the first
       % row. Need to work out how to get dynare to allow truncation of
       % rows. 
    end

    if isnat(end_date) 
        range_end = height(dt) + 1;
    else
        ix = find(dt <= end_date);
        range_end = max(ix) + 1;
    end
    
    % Get column range
    n_cols = width(t);
    letters = 'A':'Z';
    colName = "";
    while n_cols > 0
        rem = mod(n_cols-1, 26);
        colName = letters(rem+1) + colName;
        n_cols = floor((n_cols-1)/26);
    end
    % Return the sheet range.
    range = "A"+range_start + ":" + colName + range_end;
end