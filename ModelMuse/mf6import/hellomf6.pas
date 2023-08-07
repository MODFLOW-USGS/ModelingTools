program HelloMf6;

// {$LIBRARYPATH /c/Users/mjreno/Documents/dev/richard/winston}
// You may need to change the following to the debug version of the dll.
{$LINKLIB libmf6}

uses
  ctypes;

function initialize () : cint;cdecl;external;
function update () : cint;cdecl;external;
function finalize () : cint;cdecl;external;
function get_version (mf_version : pansichar) : cint;cdecl;external;
function get_component_name(name : pansichar) : cint;cdecl;external;
function get_output_item_count(count : pcint) : cint;cdecl;external;
function get_var_rank(c_var_address: PChar; Var c_var_rank: Integer): cint; cdecl; external;
function get_var_type(c_var_address: PChar; c_var_type: PChar): cint; cdecl; external;
function get_value_string(c_var_address: PChar; c_arr_ptr: PChar): cint; cdecl; external;

var
  version : pchar;
  vstr : array[0..255] of char;
  component : pchar;
  cname : array[0..255] of char;
  count : pcint;
  cnt : cint;
  VarName: array[0..255] of char;
  VariableType: array[0..255] of char;
  PackageType: array[0..255] of char;
  Rank: cint;
  Index: Integer;
begin
  version := @vstr; 
  component := @cname;
  writeln ('Hello, mf6 world.');
  initialize();
  get_version(version);
  get_component_name(component);
  writeln('MF6 DLL version: ' + version);
  writeln('MF6 DLL component: ' + component);
  count := @cnt;
  get_output_item_count(count);
  write('MF6 output item count: ');
  Writeln(cnt);
  VarName := 'MODFLOW/NPF/PACKAGE_TYPE';
  Rank := -1;
  if get_var_rank(PChar(VarName), Rank) = 0 then
  begin
    Writeln('Rank: ', Rank);
  end
  else
  begin
    Writeln('failure getting rank');
  end;

  VariableType := '';
  if get_var_type(PChar(VarName), @VariableType) = 0 then
  begin
    Writeln(String(VarName), ': ', String(VariableType));
  end;

  PackageType := '';
  if get_value_string(PChar(VarName), @PackageType) = 0 then
  begin
    Writeln(VarName, ': ', PackageType);
  end;

  update();
  finalize();
end.
