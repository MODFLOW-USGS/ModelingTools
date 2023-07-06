program HelloMf6;

{$APPTYPE CONSOLE}

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  System.SysUtils,
  Classes,
  Modflow6ConstantsUnit in 'Modflow6ConstantsUnit.pas';

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
  TGridType = Array[0..16] of AnsiChar;

function initialize () : cint;cdecl;external 'libmf6.dll';
function update () : cint;cdecl;external 'libmf6.dll';
function finalize () : cint;cdecl;external 'libmf6.dll';
//function get_version (mf_version : PAnsiChar) : cint;cdecl;external 'libmf6.dll';
function get_version (var mf_version : TMfName) : cint;cdecl;external 'libmf6.dll';
//function get_component_name(name : PAnsiChar) : cint;cdecl;external 'libmf6.dll';
function get_component_name(var name : TMfName) : cint;cdecl;external 'libmf6.dll';
function get_output_item_count(count : pcint) : cint;cdecl;external 'libmf6.dll';

// grid_id start at 1.
//function get_grid_type(grid_id : pcint; GridType: PAnsiChar) : cint;cdecl;external 'libmf6.dll';
function get_grid_type(grid_id : pcint; var GridType: TGridType) : cint;cdecl;external 'libmf6.dll';

function get_grid_shape(grid_id : pcint; var GridShape: TGridShape): cint;cdecl;external 'libmf6.dll';
function get_output_var_names(Names: PAnsiChar): cint;cdecl;external 'libmf6.dll';
function get_value_int(Variablename: PAnsiChar; AnArray: pcint): cint;cdecl;external 'libmf6.dll';
function get_last_bmi_error(Error: PErrorMessage): cint;cdecl;external 'libmf6.dll';

// Delphi does not support importing variables from DLLs or shared objects. :-(
// var
// BMI_LENVARTYPE : cint; cdecl;external 'libmf6.dll';

var
  version : PAnsiChar;
  vstr : TMfName;
  component : PAnsiChar;
  cname : TMfName;
  GridType : TGridType;
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
  NameList2: TStringList;
  CharIndex: Integer;
  AChar: AnsiChar;
  AName: string;
  NperID: array[0..255] of AnsiChar;
  NperIDP: PAnsiChar;
  NperIDArray: cintArray;

  ErrorMessage: TErrorMessage;
  NameStart: Integer;
//  LenVarType: cint;
begin
  version := @vstr;
  component := @cname;
  PGridType := @GridType;
  writeln ('Hello, mf6 world.');
  initialize();
  get_version(vstr);
  get_component_name(cname);
  writeln('MF6 DLL version: ' + version);
  writeln('MF6 DLL component: ' + component);
  count := @cnt;
  get_output_item_count(count);
  write('MF6 output item count: ');
  writeln(cnt);

  SetLength(Names, (cnt+1)*BMI_LENVARADDRESS);

  NamesP := @Names;
  get_output_var_names(Addr(Names[0]));

  NameBuilder := TStringBuilder.Create;
  NameList := TStringList.Create;
  NameList2 := TStringList.Create;
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
    for NameIndex := 0 to cnt - 1 do
    begin
      NameStart := NameIndex * BMI_LENVARADDRESS;
      AName := string(PAnsiChar(Addr(Names[NameStart])));
      NameList2.Add(AName);
    end;
    Assert(NameList.Count = NameList2.Count);
    Assert(NameList.CommaText = NameList2.CommaText);
  finally
    NameList2.Free;
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
  if get_grid_type(@GridID, GridType) = 0 then
  begin
    writeln('grid type: ' + GridType);
  end
  else
  begin
    writeln('Failure');
  end;

  GridShape[0] := -1;
  GridShape[1] := -1;
  GridShape[2] := -1;

  GridShapeP := @GridShape;
  if get_grid_shape(@GridID, GridShape) = 0 then

//  LenVarType := BMI_LENVARTYPE;
//  WriteLn(' BMI_LENVARTYPE: ', LenVarType);
  begin
  WriteLn('GridShape[0], NLAY: ', GridShape[0]);
  WriteLn('GridShape[1], NROW: ', GridShape[1]);
  WriteLn('GridShape[2], NCOL: ', GridShape[2]);
  end
  else
  begin
    writeln('Failure');
  end;

  update();
  finalize();
//  Readln;
end.
