program HelloMf6;

// {$LIBRARYPATH /c/Users/mjreno/Documents/dev/richard/winston}
{$LINKLIB libmf6}

uses
  ctypes, SysUtils;

const
  LENERRMESSAGE = 1024; //< max length for the error message
  BMI_LENERRMESSAGE = LENERRMESSAGE + 1; // max. length for the (exported) C-style error message

type
   ppcint = ^cint;
   ppcchar = ^pcchar;
   TErrorMessage = Array[0..BMI_LENERRMESSAGE-1] of AnsiChar;
   PErrorMessage = ^TErrorMessage;

function initialize () : cint;cdecl;external;
function update () : cint;cdecl;external;
function finalize () : cint;cdecl;external;
function get_version (mf_version : pchar) : cint;cdecl;external;
function get_component_name(name : pchar) : cint;cdecl;external;
function get_output_item_count(count : pcint) : cint;cdecl;external;
function get_value_int(c_var_address : pchar; c_arr_ptr: ppcint) : cint;cdecl;external;
function set_value_int(c_var_address : pchar; c_arr_ptr: ppcint) : cint;cdecl;external;
function get_value_string(c_var_address : pchar; c_arr_ptr: ppcchar) : cint;cdecl;external;
function get_last_bmi_error(c_error : pchar) : cint;cdecl;external;
function get_var_type(c_var_address: pchar; c_var_type: pchar): cint; cdecl; external;


Procedure GetAndWriteStringVariable(Name: string);
var
  VariableType: array[0..255] of char;
  VariableValue : array[0..511] of char;
  pVar : pcchar;
  ppVar : ppcchar;
  ErrorMessage: TErrorMessage;
  procedure InitializeVariableType;
  var
    Index: Integer;
  begin
    for Index := 0 to 255 do
    begin
      VariableType[Index] := #0
    end;
  end;
begin
  InitializeVariableType;
  Writeln;
  if get_var_type(Pchar(Name), PChar(VariableType)) = 0 then
  begin
    // If you get here, the variable exists.
    Write('Variable type for ', Name, ': ');
    WriteLn(VariableType);
    pVar := @VariableValue;
    ppVar := @pVar;
    if get_value_string(Pchar(Name), ppVar) = 0 then
    begin
      //write(Name, ': ');
      // Here we sometimes get an empty string!
      if Trim(string(VariableValue)) = '' then
      begin
        WriteLn('EMPTY VALUE!');
      end;
      writeln('Value for ', Name, ' = "', VariableValue, '".');
    end
    else
    begin
      WriteLn('failed to get variable value for "', Name, '".');
      get_last_bmi_error(@ErrorMessage);
      Writeln(ErrorMessage);
    end;
  end
  else
  begin
    Writeln('Failed to get variable type for "', Name, '".');
    get_last_bmi_error(@ErrorMessage);
    Writeln(ErrorMessage);
  end;

end;

var
  version : pchar;
  vstr : array[0..255] of char;
  component : pchar;
  cname : array[0..255] of char;
  address : pchar;
  modevar : array[0..13] of char = 'SIM/ISIM_MODE';
  mode : pcint;
  pmode : ppcint;
  m : cint;
  err : array[0..255] of char;
  tdis6_varaddress : array[0..23] of char = '__INPUT__/SIM/NAM/TDIS6';
  tdis6_fname : array[0..301] of char;
  ptdis : pcchar;
  pptdis : ppcchar;
  Sto6_VarAddress : array[0..23] of char = 'MODFLOW/STO/INPUT_FNAME';
  Sto6_fname : array[0..255] of char;
  psto : pcchar;
  ppsto : ppcchar;
  VariableType: array[0..255] of char;
  Index: Integer;
begin
  version := @vstr; 
  component := @cname;
  address := @modevar;
  m := 0;

  writeln ('Hello, mf6 world.');

  // initialize
  initialize();

  // mode
  mode := @m;
  pmode := @mode;
  set_value_int(address, pmode);
  get_value_int(address, pmode);
  write('MF6 mode: ');
  writeln(m);

  // version
  get_version(version);
  get_component_name(component);
  writeln('MF6 DLL version: ' + version);
  writeln('MF6 DLL component: ' + component);

  // update
  update();

  // get_value_string
  ptdis := @tdis6_fname;
  pptdis := @ptdis;
  get_value_string(tdis6_varaddress, pptdis);
  write('TDIS6 FNAME: ');
  writeln(tdis6_fname);

  // this works.
  GetAndWriteStringVariable('__INPUT__/SIM/NAM/TDIS6');
  // These return empty strings.
  GetAndWriteStringVariable('MODFLOW/STO/INPUT_FNAME');
  GetAndWriteStringVariable('MODFLOW/IC/INPUT_FNAME');
  GetAndWriteStringVariable('MODFLOW/CHD-1/INPUT_FNAME');

  // The following doesn't get a value at all.
  GetAndWriteStringVariable('MODFLOW/CHD-1/AUXNAME');
  // This however does work.
  GetAndWriteStringVariable('MODFLOW/CHD-1/AUXNAME_CST');

  // The following doesn't get a value at all.
  GetAndWriteStringVariable('MODFLOW/CHD-1/BOUNDNAME');
  // This gets 1 value but it should probably get two values.
  GetAndWriteStringVariable('MODFLOW/CHD-1/BOUNDNAME_CST');


  // finalize
  finalize();

  Writeln('Press any key to close');
  Readln;

end.
