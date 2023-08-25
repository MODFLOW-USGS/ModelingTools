program HelloMf6;

// {$LIBRARYPATH /c/Users/mjreno/Documents/dev/richard/winston}
// You may need to change the following to the debug version of the dll.
{$LINKLIB libmf6}

uses
  ctypes, SysUtils, Math, Classes;

function initialize () : cint;cdecl;external;
function update () : cint;cdecl;external;
function finalize () : cint;cdecl;external;
function get_version (mf_version : pansichar) : cint;cdecl;external;
function get_component_name(name : pansichar) : cint;cdecl;external;
function get_output_item_count(count : pcint) : cint;cdecl;external;
function get_var_rank(c_var_address: PChar; Var c_var_rank: Integer): cint; cdecl; external;
function get_var_type(c_var_address: PChar; c_var_type: PChar): cint; cdecl; external;
function get_value_string(c_var_address: PChar; c_arr_ptr: PPChar): cint; cdecl; external;

//type
//  TAnsiStringArray = Array of AnsiString;
//
//const
//  LENMEMTYPE = 50; //< maximum length of a memory manager type
//  BMI_LENVARTYPE = LENMEMTYPE + 1; //< max. length for variable type C-strings
//  StrLEN: Ansistring = 'LEN=';
//
//
//procedure InitializeVariableType(out VariableType: AnsiString);
//var
//  CharIndex: Integer;
//begin
//  SetLength(VariableType, BMI_LENVARTYPE);
//  for CharIndex := 1 to BMI_LENVARTYPE do
//  begin
//    VariableType[CharIndex] := #0;
//  end;
//end;
//
//procedure ExtractStrArray(AString: AnsiString; out Strings: TAnsiStringArray);
//var
//  StringList: TStringList;
//  index: Integer;
//  StartIndex: Integer;
//  CharIndex: Integer;
//  NewString: AnsiString;
//begin
//  StringList := TStringList.Create;
//  try
//    StartIndex := 1;
//    for CharIndex := StartIndex to Length(AString) do
//    begin
//      if AString[CharIndex] = #0 then
//      begin
//        NewString := Trim(Copy(AString,StartIndex, CharIndex - StartIndex));
//        if NewString <> '' then
//        begin
//          StringList.Add(NewString);
//        end;
//        StartIndex := CharIndex + 1;
//      end;
//    end;
//    SetLength(Strings, StringList.Count);
//    for index := 0 to StringList.Count - 1 do
//    begin
//      Strings[index] := StringList[index];
//    end;
//  finally
//    StringList.Free;
//  end;
//end;
//
//
//
//procedure GetStringVariable(VarName: AnsiString; var NameTypes: TAnsiStringArray);
//var
//  VariableType: AnsiString;
//  CharIndex: Integer;
//  StrLengthPos: Integer;
//  ParenPos: Integer;
//  VarLength: string;
//  StringLength: Integer;
//  Rank: Integer;
//  NIndex: Integer;
//  SavedVarType: AnsiString;
//  VariableValues: AnsiString;
//  PVarVal: PAnsiChar;
//  PPVarVal: PPAnsiChar;
//begin
//  InitializeVariableType(VariableType);
//  if get_var_type(PAnsiChar(VarName), PAnsiChar(VariableType)) = 0 then
//  begin
//    if AnsiPos('STRING', VariableType) = 1 then
//    begin
//      StrLengthPos := AnsiPos(StrLEN, VariableType);
//      if StrLengthPos > 0 then
//      begin
//        Inc(StrLengthPos, Length(StrLEN));
//        SavedVarType := VariableType;
//        VariableType := Trim(Copy(VariableType, StrLengthPos, MAXINT));
//        ParenPos := AnsiPos('(', VariableType);
//        if ParenPos > 0 then
//        begin
//          VarLength := Trim(Copy(VariableType,1,ParenPos-1));
//          VariableType := Copy(VariableType, ParenPos+1, MAXINT);
//          ParenPos := AnsiPos(')', VariableType);
//          Assert(ParenPos > 0);
//          VariableType := Trim(Copy(VariableType,1,ParenPos-1));
//          Rank := StrToInt(VariableType);
//          if Rank = 0 then
//          begin
//            WriteLn('  empty');
//            Exit;
//          end;
//        end
//        else
//        begin
//          VarLength := VariableType;
//          Rank := 1;
//        end;
//        StringLength := Max(StrToInt(VarLength), 255);
//      end
//      else
//      begin
//        StringLength := BMI_LENVARTYPE;
//        if get_var_rank(PAnsiChar(VarName), Rank) = 0 then
//        begin
//          Writeln('Rank: ', Rank);
//        end
//        else
//        begin
//          WriteLn('Failed to get Rank for "', VarName, '".');
//        end;
//      end;
//        SetLength(VariableValues, (StringLength+1)*Rank);
//      begin
//        for CharIndex := 1 to Length(VariableValues) do
//        begin
//          VariableValues[CharIndex] := #0;
//        end;
//      end;
//      PVarVal := PAnsiChar(VariableValues);
//      PPVarVal := @PVarVal;
//      if get_value_string(PAnsiChar(VarName), PPVarVal) = 0 then
//      begin
//        begin
//        try
//          ExtractStrArray(VariableValues, NameTypes);
//        except
//          Writeln(VarName);
//          Exit;
//        end;
////            ExtractStrArray(TypeNames[RankIndex], NameTypes);
//          if Length(NameTypes) > 0 then
//          begin
//            for NIndex := 0 to Length(NameTypes) - 1 do
//            begin
//              WriteLn('  ', Trim(NameTypes[NIndex]));
//            end;
//          end
//          else
//          begin
//            WriteLn('  no values extracted from "', VarName, '."');
//          end;
//        end;
//      end
//      else
//      begin
//        WriteLn('Failed to get values for "', VarName, '."');
//      end;
//    end;
//  end
//  else
//  begin
//    WriteLn;
//    WriteLn('Failed to get variable type for "', VarName, '".');
//  end;
//end;

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

  //PackageType := '';
  //if get_value_string(PChar(VarName), @PackageType) = 0 then
  //begin
  //  Writeln(VarName, ': ', PackageType);
  //end;

  update();
  finalize();
end.
