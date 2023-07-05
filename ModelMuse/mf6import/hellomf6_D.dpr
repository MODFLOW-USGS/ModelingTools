program HelloMf6;

{$APPTYPE CONSOLE}

uses madExcept, madLinkDisAsm, madListHardware, madListProcesses, madListModules,
// {$LIBRARYPATH /c/Users/mjreno/Documents/dev/richard/winston}
//{$LINKLIB libmf6d}

  System.SysUtils, Classes;
//  ctype

type
  cint32                 = LongInt;
  cint                   = cint32;
  pcint                  = ^cint;
  TGridShape = array[0..2] of cint;
  PGridShape = ^TGridShape;
  TMfName = array[0..255] of AnsiChar;
  PMfName = ^TMfName;
  TMfNames = array of TMfName;
  PMfNames = ^TMfNames;
  cintArray = array of cint;
  PcintArray = ^cintArray;
  TErrorMessage = Array[0..1024] of AnsiChar;
  PErrorMessage = ^TErrorMessage;

function initialize () : cint;cdecl;external 'libmf6.dll';
function update () : cint;cdecl;external 'libmf6.dll';
function finalize () : cint;cdecl;external 'libmf6.dll';
function get_version (mf_version : PAnsiChar) : cint;cdecl;external 'libmf6.dll';
function get_component_name(name : PAnsiChar) : cint;cdecl;external 'libmf6.dll';
function get_output_item_count(count : pcint) : cint;cdecl;external 'libmf6.dll';

// grid_id start at 1.
function get_grid_type(grid_id : pcint; GridType: PAnsiChar) : cint;cdecl;external 'libmf6.dll';
function get_grid_shape(grid_id : pcint; GridShape: PGridShape): cint;cdecl;external 'libmf6.dll';
function get_output_var_names(Names: PAnsiChar): cint;cdecl;external 'libmf6.dll';
function get_value_int(Variablename: PAnsiChar; AnArray: pcint): cint;cdecl;external 'libmf6.dll';
function get_last_bmi_error(Error: PErrorMessage): cint;cdecl;external 'libmf6.dll';
//function BMI_LENVARTYPE : cint;cdecl;external 'libmf6.dll';

var
  version : PAnsiChar;
  vstr : array[0..255] of AnsiChar;
  component : PAnsiChar;
  cname : array[0..255] of AnsiChar;
  GridType : array[0..17] of AnsiChar;
  count : pcint;
  cnt : cint;
  PGridType : PAnsiChar;
  GridID: cint;
  GridShape: TGridShape;
  GridShapeP: PGridShape;
  Names: array of AnsiChar;
  NamesP: PMfNames;
  NameIndex: Integer;
  NameBuilder: TStringBuilder;
  NameList: TStringList;
  CharIndex: Integer;
  AChar: AnsiChar;
  AName: string;
  NperID: array[0..255] of AnsiChar;
  NperIDP: PAnsiChar;
  NperIDArray: cintArray;

  ErrorMessage: TErrorMessage;
//  LenVarType: cint;
begin
  version := @vstr;
  component := @cname;
  PGridType := @GridType;
  writeln ('Hello, mf6 world.');
  initialize();
  get_version(version);
  get_component_name(component);
  writeln('MF6 DLL version: ' + version);
  writeln('MF6 DLL component: ' + component);
  count := @cnt;
  get_output_item_count(count);
  write('MF6 output item count: ');
  writeln(cnt);

  SetLength(Names, (cnt+1)*256);

  NamesP := @Names;
  get_output_var_names(Addr(Names[0]));

  NameBuilder := TStringBuilder.Create;
  NameList := TStringList.Create;
  try
    for CharIndex := 0 to Length(Names) - 1 do
    begin
      AChar := Names[CharIndex];
      if Ord(AChar) = 0 then
      begin
        AName := NameBuilder.ToString;
        if AName <> '' then
        begin
          NameList.Add(AName);
          NameBuilder.Clear;
        end;
      end
      else
      begin
        NameBuilder.Append(AChar);
      end;
    end;
    for NameIndex := 0 to NameList.Count - 1 do
    begin
      Writeln(NameList[NameIndex]);
    end;
  finally
    NameList.Free;
    NameBuilder.Free;
  end;

  SetLength(NperIDArray, 1);
  NperID := 'TDIS/NPER';
  NperIDP := @NperID;
  if get_value_int(NperIDP, @NperIDArray) = 0 then
  begin
    Writeln('Success');
  end
  else
  begin
    get_last_bmi_error(@ErrorMessage);
    Writeln(ErrorMessage);
  end;
  writeln('NPER: ', NperIDArray[0]);

  GridID := 1;
  get_grid_type(@GridID, PGridType);
  writeln('grid type: ' + GridType);

  GridShape[0] := -1;
  GridShape[1] := -1;
  GridShape[2] := -1;

  GridShapeP := @GridShape;
  get_grid_shape(@GridID, GridShapeP);

//  LenVarType := BMI_LENVARTYPE;
//  WriteLn(' BMI_LENVARTYPE: ', LenVarType);

  WriteLn('GridShape[0, NLAY]: ', GridShape[0]);
  WriteLn('GridShape[1], NROW: ', GridShape[1]);
  WriteLn('GridShape[2], NCOL: ', GridShape[2]);

  update();
  finalize();
  Readln;
end.
