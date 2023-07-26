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
  Modflow6ConstantsUnit in 'Modflow6ConstantsUnit.pas',
  Mf6Variables in 'Mf6Variables.pas',
  Mf6Functions in 'Mf6Functions.pas';

//  ctype

//type
//  cint32                 = LongInt;
//  cint                   = cint32;
//  pcint                  = ^cint;
//  TGridShape = array[0..2] of cint;
//  PGridShape = ^TGridShape;
//  TMfName = array[0..255] of AnsiChar;
//  PMfName = ^TMfName;
//  TMfNames = array of TMfName;
//  PMfNames = ^TMfNames;
//  cintArray = array of cint;
//  PcintArray = ^cintArray;
//  TErrorMessage = Array[0..1024] of AnsiChar;
//  PErrorMessage = ^TErrorMessage;
//  TGridType = Array[0..16] of AnsiChar;
//  TVanName = array[0..BMI_LENVARADDRESS-1] of AnsiChar;
//  TVarNames = array of TVanName;

//function initialize () : cint;cdecl;external 'libmf6.dll';
//function update () : cint;cdecl;external 'libmf6.dll';
//function finalize () : cint;cdecl;external 'libmf6.dll';
////function get_version (mf_version : PAnsiChar) : cint;cdecl;external 'libmf6.dll';
//function get_version (var mf_version : TMfName) : cint;cdecl;external 'libmf6.dll';
////function get_component_name(name : PAnsiChar) : cint;cdecl;external 'libmf6.dll';
//function get_component_name(var name : TMfName) : cint;cdecl;external 'libmf6.dll';
//function get_output_item_count(count : pcint) : cint;cdecl;external 'libmf6.dll';
//
//// grid_id start at 1.
////function get_grid_type(grid_id : pcint; GridType: PAnsiChar) : cint;cdecl;external 'libmf6.dll';
//function get_grid_type(grid_id : pcint; var GridType: TGridType) : cint;cdecl;external 'libmf6.dll';
//
//function get_grid_shape(grid_id : pcint; var GridShape: TGridShape): cint;cdecl;external 'libmf6.dll';
//function get_output_var_names(Names: PAnsiChar): cint;cdecl;external 'libmf6.dll';
////function get_output_var_names(var Names: TVarNames): cint;cdecl;external 'libmf6.dll';
//function get_value_int(Variablename: PAnsiChar; AnArray: pcint): cint;cdecl;external 'libmf6.dll';
//function get_last_bmi_error(Error: PErrorMessage): cint;cdecl;external 'libmf6.dll';

// Delphi does not support importing variables from DLLs or shared objects. :-(
// var
// BMI_LENVARTYPE : cint; cdecl;external 'libmf6.dll';

var
  version : PAnsiChar;
  vstr : TMfName;
  component : PAnsiChar;
  cname : TMfComponentName;
  GridType : TGridType;
//  count : pcint;
  cnt : cint;
//  PGridType : PAnsiChar;
  GridID: cint;
  GridShape: TGridShape;
//  GridShapeP: PGridShape;
  Names: array of AnsiChar;
//  NamesP: PMfNames;
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
//  varName: TVanName;
  varNames: TVarNames;

  ErrorMessage: TErrorMessage;
  NameStart: Integer;
  currentTime: double;
  rank: cint;
  GridSize: cint;
  Index: Integer;
  InnerIndex: Integer;
  X, Y: array of double;
  NVertString: AnsiString;
  NCPL_String: AnsiString;
  NVERT: cint;
  NCPL: cint;
  DISV_NCVERT: AnsiString;
  DISV_ICVERT: AnsiString;
  DISV_XC: AnsiString;
  DISV_YC: AnsiString;
  MODEL_SHAPE: AnsiString;
  DIS_TOP: AnsiString;
  DIS_BOT: AnsiString;
  DIS_LENUNI: AnsiString;
  NPF_K: AnsiString;
  VertPerCell: array of cint;
  Verticies: array of cint;
  VertLength: integer;
  VertIndex: Integer;
  ModelShapeArray: array of cint;
  DimCount: Integer;
  Mf6GridType: TMf6GridType;
  ModelTop: array of Double;
  ModelBottom: array of Double;
  ArrayLength: Integer;
  LenUni: cint;
  LenUniArray: array of cint;
  k: array of Double;
  Mode: array of cint;
  ModeString: AnsiString;
begin
  version := @vstr;
  component := @cname;
//  PGridType := @GridType;
  writeln ('Hello, mf6 world.');
  initialize();
  get_version(vstr);
  get_component_name(cname);
  writeln('MF6 DLL version: ' + version);
  writeln('MF6 DLL component: ' + component);
//  count := @cnt;
  get_output_item_count(cnt);
  write('MF6 output item count: ');
  writeln(cnt);

  SetLength(Names, (cnt+1)*BMI_LENVARADDRESS);
  SetLength(varNames,  cnt+1);

//  NamesP := @Names;
  get_output_var_names(Addr(Names[0]));

  NVertString := '';
  NCPL_String := '';
  DISV_NCVERT := '';
  DISV_ICVERT := '';
  DISV_XC := '';
  DISV_YC := '';
  MODEL_SHAPE := '';
  DIS_TOP := '';
  DIS_BOT := '';
  DIS_LENUNI := '';
  NPF_K := '';
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
    if (NameList.Count > 0) and (NameList[0] = 'SIM/ISIM_MODE') then
    begin
      SetLength(Mode, 1);
      ModeString := NameList[0];
      if get_value_int(PAnsiChar(ModeString), @Mode) = 0 then
      begin
        Writeln('Mode: ', Mode[0]);
        Mode[0] := 0;
        if set_value_int(PAnsiChar(ModeString), @Mode) = 0 then
        begin
          Writeln('success Setting Mode: ');
        end
        else
        begin
          Writeln('Error Setting Mode: ');
          get_last_bmi_error(@ErrorMessage);
          Writeln(ErrorMessage);
        end;
      end
      else
      begin
        Writeln('Error Getting Mode: ');
        get_last_bmi_error(@ErrorMessage);
        Writeln(ErrorMessage);
      end;
    end;
    for NameIndex := 0 to cnt - 1 do
    begin
      NameStart := NameIndex * BMI_LENVARADDRESS;
      AName := string(PAnsiChar(Addr(Names[NameStart])));
      if Pos('/DISV/NVERT', AName) > 0 then
      begin
        NVertString := AnsiString(AName);
      end;
      if Pos('/DISV/NCPL', AName) > 0 then
      begin
        NCPL_String := AnsiString(AName);
      end;
      if Pos('/DISV/NCVERT', AName) > 0 then
      begin
        DISV_NCVERT := AnsiString(AName);
      end;
      if Pos('/DISV/ICVERT', AName) > 0 then
      begin
        DISV_ICVERT := AnsiString(AName);
      end;
      if Pos('/DISV/XC', AName) > 0 then
      begin
        DISV_XC := AnsiString(AName);
      end;
      if Pos('/DISV/YC', AName) > 0 then
      begin
        DISV_YC := AnsiString(AName);
      end;
      if Pos('MODEL_SHAPE', AName) > 0 then
      begin
        MODEL_SHAPE := AnsiString(AName);
      end;
      if Pos('/DIS/TOP', AName) > 0 then
      begin
        DIS_TOP := AnsiString(AName);
      end;
      if Pos('/DIS/BOT', AName) > 0 then
      begin
        DIS_BOT := AnsiString(AName);
      end;
      if Pos('/DIS/LENUNI', AName) > 0 then
      begin
        DIS_LENUNI := AnsiString(AName);
      end;
      if (Pos('/NPF/K', AName) > 0) and (AName[Length(AName)] = 'K') then
      begin
        NPF_K := AnsiString(AName);
      end;



      NameList2.Add(AName);
    end;
    Assert(NameList.Count = NameList2.Count);
    Assert(NameList.CommaText = NameList2.CommaText);
  finally
    NameList2.Free;
    NameList.Free;
    NameBuilder.Free;
  end;

  if NPF_K <> '' then
  begin
    SetLength(ModelShapeArray,MAXMEMRANK);
    for Index := 0 to Length(ModelShapeArray) - 1 do
    begin
      ModelShapeArray[Index] := Mf6Undefined;
    end;
    ArrayLength := 1;
    if get_var_shape(PAnsiChar(NPF_K), @ModelShapeArray[0]) = 0 then
    begin
      ArrayLength := 1;
      for Index := 0 to Length(ModelShapeArray) - 1 do
      begin
        if ModelShapeArray[Index] <> Mf6Undefined then
        begin
          Writeln('k dimension: ', Index, ' ', ModelShapeArray[Index]);
          ArrayLength := ArrayLength * ModelShapeArray[Index]
        end;
      end;
      SetLength(K, ArrayLength);
      if get_value_double(PAnsiChar(NPF_K), @K) = 0 then
      begin
        for Index := 0 to Length(K) - 1 do
        begin
          WriteLn(Index, ' ', K[Index]);
        end;
      end;
    end;
  end;

  if DIS_LENUNI <> '' then
  begin
    SetLength(LenUniArray, 1);
    if get_value_int(PAnsiChar(DIS_LENUNI), @LenUniArray) = 0 then
    begin
      Writeln('LENUNI: ', LenUniArray[0]);
    end;
  end;


  if DIS_TOP <> '' then
  begin
    SetLength(ModelShapeArray,MAXMEMRANK);
    for Index := 0 to Length(ModelShapeArray) - 1 do
    begin
      ModelShapeArray[Index] := Mf6Undefined;
    end;
    ArrayLength := 1;
    if get_var_shape(PAnsiChar(DIS_TOP), @ModelShapeArray[0]) = 0 then
    begin
      ArrayLength := 1;
      for Index := 0 to Length(ModelShapeArray) - 1 do
      begin
        if ModelShapeArray[Index] <> Mf6Undefined then
        begin
          Writeln('Top dimension: ', Index, ' ', ModelShapeArray[Index]);
          ArrayLength := ArrayLength * ModelShapeArray[Index]
        end;
      end;

      SetLength(ModelTop, ArrayLength);
      if get_value_double(PAnsiChar(DIS_TOP), @ModelTop) = 0 then
      begin
//        for Index := 0 to Length(ModelTop) - 1 do
//        begin
//          WriteLn(Index, ' ', ModelTop[Index]);
//        end;
      end;
    end;
  end;

  if DIS_BOT <> '' then
  begin
    SetLength(ModelShapeArray,MAXMEMRANK);
    for Index := 0 to Length(ModelShapeArray) - 1 do
    begin
      ModelShapeArray[Index] := Mf6Undefined;
    end;
    if get_var_shape(PAnsiChar(DIS_BOT), @ModelShapeArray[0]) = 0 then
    begin
      ArrayLength := 1;
      for Index := 0 to Length(ModelShapeArray) - 1 do
      begin
        if ModelShapeArray[Index] <> Mf6Undefined then
        begin
          Writeln('Botm dimension: ', Index, ' ', ModelShapeArray[Index]);
          ArrayLength := ArrayLength * ModelShapeArray[Index]
        end;
      end;
      SetLength(ModelBottom, ArrayLength);
      if get_value_double(PAnsiChar(DIS_BOT), @ModelBottom) = 0 then
      begin
        for Index := 0 to Length(ModelBottom) - 1 do
        begin
          WriteLn(Index, ' ', ModelBottom[Index]);
        end;
      end;
    end;
  end;

  DimCount := 0;
  if MODEL_SHAPE <> '' then
  begin
    SetLength(ModelShapeArray,3);
    for Index := 0 to Length(ModelShapeArray) - 1 do
    begin
      ModelShapeArray[Index] := Mf6Undefined;
    end;
    if get_value_int(PAnsiChar(MODEL_SHAPE), @ModelShapeArray) = 0 then
    begin
      for Index := 0 to Length(ModelShapeArray) - 1 do
      begin
        if ModelShapeArray[Index] <> Mf6Undefined then
        begin
          Inc(DimCount);
        end;
      end;
    end;
    Mf6GridType := TMf6GridType(DimCount-1);
  end;

  NVERT := 0;
  SetLength(NperIDArray, 1);
  if NVertString <> '' then
  begin
    if get_value_int(PAnsiChar(NVertString), @NperIDArray) = 0 then
    begin
      Writeln('Success');
      NVERT := NperIDArray[0];
    end
  end;

  NCPL := 0;
  if NCPL_String <> '' then
  begin
    if get_value_int(PAnsiChar(NCPL_String), @NperIDArray) = 0 then
    begin
      Writeln('Success');
      NCPL := NperIDArray[0];
    end
  end;

  if (NCPL > 0) and (DISV_NCVERT <> '') and (DISV_ICVERT <> '')
    and (DISV_XC <> '') and (DISV_YC <> '') then
  begin
    SetLength(VertPerCell, NCPL);
    SetLength(X, NCPL);
    SetLength(Y, NCPL);
    if (get_value_int(PAnsiChar(DISV_NCVERT), @VertPerCell) = 0)
      and (get_value_double(PAnsiChar(DISV_XC), @X) = 0)
      and (get_value_double(PAnsiChar(DISV_YC), @Y) = 0) then
    begin
      Writeln('Success');
      VertLength := 0;
      for Index := 0 to Length(VertPerCell) - 1 do
      begin
        VertLength := VertLength + VertPerCell[Index];
      end;
      SetLength(Verticies, VertLength);
      if get_value_int(PAnsiChar(DISV_ICVERT), @Verticies) = 0 then
      begin
        Writeln('Success');
        VertIndex := 0;
        for Index := 0 to Length(VertPerCell) - 1 do
        begin
          Write(Index,' ', VertPerCell[Index], ' ', X[Index], ' ', Y[Index]);
          for InnerIndex := 0 to VertPerCell[Index] - 1 do
          begin
            Write(' ', Verticies[VertIndex]);
            Inc(VertIndex);
          end;
          Writeln;
        end;
      end;
    end
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

  if get_grid_face_count(@GridID, cnt) = 0 then
  begin
  writeln('face count: ', cnt);
  end
  else
  begin
    writeln('Failure');
  end;

  GridShape[0] := Mf6Undefined;
  GridShape[1] := Mf6Undefined;
  GridShape[2] := Mf6Undefined;

//  GridShapeP := @GridShape;
  if get_grid_shape(@GridID, GridShape) = 0 then
  begin
    WriteLn('GridShape[0], NLAY: ', GridShape[0]);
    WriteLn('GridShape[1], NROW: ', GridShape[1]);
    WriteLn('GridShape[2], NCOL: ', GridShape[2]);
    GridSize := 1;
    for Index := 0 to Length(GridShape) - 1 do
    begin
      if GridShape[Index] <> Mf6Undefined then
      begin
        GridSize := GridSize * GridShape[Index]
      end;
    end;
    writeln('GridSize: ', GridSize);
  end
  else
  begin
    writeln('Failure');
  end;
  get_current_time(currentTime);
  Writeln('Current time: ',  currentTime);
  get_end_time(currentTime);
  Writeln('end time: ',  currentTime);

  SetLength(X, NVERT);
  SetLength(Y, NVERT);
  for Index := 0 to Length(X) - 1 do
  begin
    X[Index] := Mf6Undefined;
    Y[Index] := Mf6Undefined;
  end;

  GridID := 1;
  if (NVERT > 0) and (get_grid_x(@GridID, Addr(X[0])) = 0)
    and (get_grid_y(@GridID, Addr(Y[0])) = 0) then
  begin
    for Index := 0 to Length(X) - 1 do
    begin
      if X[Index] = Mf6Undefined then
      begin
        Assert(False)
      end;
      if Y[Index] = Mf6Undefined then
      begin
        Assert(False)
      end;
      writeln(Index, X[Index], Y[Index]);
    end;
  end
  else
  begin
    writeln('Failure get_grid_x');
  end;

  GridID := 1;
  GridSize := 0;
  if get_grid_size(@GridID, GridSize) = 0 then
  begin
    // grid size should not be zero.
    writeln('grid size: ', GridSize);
  end
  else
  begin
    writeln('Failure get_grid_size');
  end;

  update();

  get_output_item_count(cnt);
  write('MF6 output item count: ');
  writeln(cnt);

  get_current_time(currentTime);
  Writeln('Current time: ',  currentTime);

  finalize();
  Writeln('Press any key to close');
  Readln;
end.
