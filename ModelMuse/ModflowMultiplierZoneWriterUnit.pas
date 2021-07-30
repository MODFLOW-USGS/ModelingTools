unit ModflowMultiplierZoneWriterUnit;

interface

uses LayerStructureUnit, Classes, PhastModelUnit, SysUtils, HufDefinition,
  CustomModflowWriterUnit, ModflowParameterUnit, DataSetUnit, GoPhastTypes,
  ModflowPackageSelectionUnit;

type
  TCustomMultZoneWriter = class(TCustomModflowWriter)
//  private
//    FNameOfFile: string;
  protected
    FFileUnit: integer;
    FFileType: string;
    procedure WriteDataSet0; virtual; abstract;
    procedure WriteDataSet1; virtual; abstract;
    function UseSteadyParameter(Param: TModflowSteadyParameter): boolean;
      virtual;
    function GetDataArray(Param: TModflowSteadyParameter): TDataArray;
      virtual; abstract;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function GetArrayName(Param: TModflowSteadyParameter;
      LayerIndex: integer; AModel: TBaseModel): string; virtual; abstract;
    function ArrayUsed(const ArrayName: string): boolean; virtual; abstract;
    function DataSet2Comment: string; virtual; abstract;
    procedure WriteDataSets2And3;
    function NumberOfArrays: integer; virtual; abstract;
    property FileType : string read FFileType;
    property FileUnit : integer read FFileUnit;
    function UsesHufParam(UsedParam: THufUsedParameter;
      var ArrayName: string; var DataArray: TDataArray): boolean; virtual;
    procedure WriteTransientPackageArrays(TransPackage: TCustomTransientLayerPackageSelection); virtual; abstract;
    procedure WriteTransientArrays;
  public
    class function ArrayType: string; virtual; abstract;
    function WriteFile(const AFileName: string): boolean; virtual;
  end;

  TModflowZoneWriter = class(TCustomMultZoneWriter)
  private
    procedure CheckOverlappingZones;
  protected
    procedure WriteDataSet0; override;
    procedure WriteDataSet1; override;
    function UseSteadyParameter(Param: TModflowSteadyParameter): boolean;
      override;
    function GetDataArray(Param: TModflowSteadyParameter): TDataArray;
      override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function GetArrayName(Param: TModflowSteadyParameter;
      LayerIndex: integer; AModel: TBaseModel): string; override;
    function ArrayUsed(const ArrayName: string): boolean; override;
    function DataSet2Comment: string; override;
    class function Extension: string; override;
    function NumberOfArrays: integer; override;
    function UsesHufParam(UsedParam: THufUsedParameter;
      var ArrayName: string; var DataArray: TDataArray): boolean; override;
    procedure WriteTransientPackageArrays(
      TransPackage: TCustomTransientLayerPackageSelection); override;
  public
    class function ArrayType: string; override;
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); override;
    function WriteFile(const AFileName: string): boolean; override;
  end;

  TModflowMultiplierWriter = class(TCustomMultZoneWriter)
  protected
    procedure WriteTransientPackageArrays(TransPackage: TCustomTransientLayerPackageSelection); override;
    procedure WriteDataSet0; override;
    procedure WriteDataSet1; override;
    function UseSteadyParameter(Param: TModflowSteadyParameter): boolean;
      override;
    function GetDataArray(Param: TModflowSteadyParameter): TDataArray;
      override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function GetArrayName(Param: TModflowSteadyParameter;
      LayerIndex: integer; AModel: TBaseModel): string; override;
    function ArrayUsed(const ArrayName: string): boolean; override;
    function DataSet2Comment: string; override;
    class function Extension: string; override;
    function NumberOfArrays: integer; override;
    function UsesHufParam(UsedParam: THufUsedParameter;
      var ArrayName: string; var DataArray: TDataArray): boolean; override;
  public
    class function ArrayType: string; override;
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); override;
    function WriteFile(const AFileName: string): boolean; override;
  end;

implementation

uses OrderedCollectionUnit, ModflowUnitNumbers, frmProgressUnit, Forms,
  frmGoPhastUnit, frmErrorsAndWarningsUnit, System.IOUtils,
  PestParamRoots;

resourcestring
  StrWritingSPackageI = 'Writing %s Package input.';
  StrMultiple0sParame = 'Multiple %0:s parameters apply to the following cel' +
  'ls. The value assigned to such cells will be the sum of the values applied '
  + 'by the individual parameters. If you do not intend parameter zones to '
  + 'overlap, this is an error.';
  StrMissingZoneArrayF = 'Missing Zone Array file(s). The following files th' +
  'at define zone arrays do not exist. Such files are normally created when ' +
  'exporting the RCH, EVT, or ETS package. If you have suppressed generation ' +
  'of one of these packages by including it in the MODFLOW Name file dialog ' +
  'box, try removing it from the MODFLOW Name file dialog box and exporting again.';
  StrMissingMultArrayF = 'Missing Multiplier Array file(s). The following files th' +
  'at define zone arrays do not exist. Such files are normally created when ' +
  'exporting the RCH, EVT, or ETS package. If you have suppressed generation ' +
  'of one of these packages by including it in the MODFLOW Name file dialog ' +
  'box, try removing it from the MODFLOW Name file dialog box and exporting again.';
//  StrWritingDataSet0 = '  Writing Data Set 0.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
//  StrWritingDataSets2and3 = '  Writing Data Sets 2 and 3.';

{ TCustomMultZoneWriter }

function TCustomMultZoneWriter.UsesHufParam(UsedParam: THufUsedParameter;
  var ArrayName: string; var DataArray: TDataArray): boolean;
begin
  result := False;
  case UsedParam.Parameter.ParameterType of
    ptUndefined, ptLPF_HK, ptLPF_HANI, ptLPF_VK, ptLPF_VANI, ptLPF_SS,
    ptLPF_SY, ptLPF_VKCB, ptRCH, ptEVT, ptETS, ptCHD, ptGHB, ptQ, ptRIV,
    ptDRN, ptDRT, ptSFR, ptHFB, ptSTR, ptQMAX:
      begin
        Assert(False);
      end;
    ptHUF_HK, ptHUF_HANI, ptHUF_VK, ptHUF_VANI, ptHUF_KDEP, ptHUF_LVDA:
      begin
        result := Model.ModflowPackages.HufPackage.IsSelected;
      end;
    ptHUF_SS, ptHUF_SY, ptHUF_SYTP:
      begin
        result := Model.ModflowPackages.HufPackage.IsSelected
          and Model.ModflowFullStressPeriods.TransientModel;
      end;
    else Assert(False);
  end;
end;

function TCustomMultZoneWriter.UseSteadyParameter(
  Param: TModflowSteadyParameter): boolean;
begin
  result := False;
  case Param.ParameterType of
    ptLPF_HK, ptLPF_HANI, ptLPF_VK, ptLPF_VANI, ptLPF_VKCB:
      begin
        result := Model.ModflowPackages.LpfPackage.IsSelected
          or Model.ModflowPackages.UpwPackage.IsSelected;
      end;
    ptLPF_SS, ptLPF_SY:
      begin
        result := (Model.ModflowPackages.LpfPackage.IsSelected
          or Model.ModflowPackages.UpwPackage.IsSelected)
          and Model.ModflowFullStressPeriods.TransientModel;
      end;
    ptHUF_LVDA:
      begin
        result := Model.ModflowPackages.HufPackage.IsSelected;
      end;
    ptHUF_SYTP:
      begin
        result := Model.ModflowPackages.HufPackage.IsSelected
          and Model.ModflowFullStressPeriods.TransientModel;
      end;
    ptHFB:
      begin
        result := False;
      end;
    ptUndefined, ptRCH, ptEVT, ptETS, ptCHD, ptGHB, ptQ,
    ptRIV, ptDRN, ptDRT, ptSFR,
    ptHUF_HK, ptHUF_HANI, ptHUF_VK, ptHUF_VANI, ptHUF_SS, ptHUF_SY,
    ptHUF_KDEP, ptSTR, ptQMAX:
      begin
        Assert(False);
      end;
  end;
end;

procedure TCustomMultZoneWriter.WriteDataSets2And3;
var
  LayerCount: Integer;
  ArrayIndex: Integer;
  ArrayName: string;
  LayerIndex: Integer;
  DataArray: TDataArray;
  Param: TModflowSteadyParameter;
  ParamIndex: Integer;
  HGU: THydrogeologicUnit;
  UnitIndex: Integer;
  UsedParam: THufUsedParameter;
  Description: string;
  MIndex: Integer;
begin
  LayerCount := Model.ModflowLayerCount;
  MIndex := 1;
  for ParamIndex := 0 to Model.ModflowSteadyParameters.Count - 1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    Param := Model.ModflowSteadyParameters.Items[ParamIndex];
    if UseSteadyParameter(Param) then
    begin
      DataArray := GetDataArray(Param);
      for LayerIndex := 1 to LayerCount do
      begin
        ArrayName := GetArrayName(Param, LayerIndex, Model);
        if ArrayUsed(ArrayName) then
        begin
          Assert(ArrayName <> '');
          While Length(ArrayName) < 10 do
          begin
            ArrayName := ArrayName + ' ';
          end;
          // Data set 2;
          Assert(Length(ArrayName) = 10);
          WriteString(ArrayName);
          WriteString(DataSet2Comment);
          NewLine;

          // Data set 3
          ArrayIndex := Model.
            ModflowLayerToDataSetLayer(LayerIndex);
          if Param.ParameterType = ptLPF_VKCB then
          begin
            Inc(ArrayIndex);
          end;
          WriteArray(DataArray, ArrayIndex, ArrayType + ' array for '
            + Param.ParameterName + ' in '
            + Model.ModflowLayerBottomDescription(ArrayIndex),
            StrNoValueAssigned, ArrayName);
        end;
      end;
      WritePestZones(DataArray, FInputFileName, Format('M%d', [MIndex]));
      Inc(MIndex);
      Model.DataArrayManager.CacheDataArrays;
      DataArray.CacheData;
    end;
  end;

  if Model.ModflowPackages.HufPackage.IsSelected then
  begin
    for UnitIndex := 0 to Model.HydrogeologicUnits.Count - 1 do
    begin
      HGU := Model.HydrogeologicUnits[UnitIndex];
      for ParamIndex := 0 to HGU.HufUsedParameters.Count - 1 do
      begin
        UsedParam := HGU.HufUsedParameters[ParamIndex];
        if UsesHufParam(UsedParam, ArrayName, DataArray) then
        begin
          // Data set 2;
          Assert(ArrayName <> '');
          Assert(Length(ArrayName) <= 10);
          WriteString(ArrayName);
          WriteString('          ');
          WriteString(DataSet2Comment);
          NewLine;

          // Data set 3
          Description := UsedParam.Description;
          WriteArray(DataArray, 0, Description, StrNoValueAssigned, ArrayName);
          WritePestZones(DataArray, FInputFileName, Format('M%d', [MIndex]));
          Inc(MIndex);

          Model.DataArrayManager.CacheDataArrays;
          DataArray.CacheData;
        end;
      end;
    end;
  end;

  WriteTransientArrays;

end;

function TCustomMultZoneWriter.WriteFile(const AFileName: string): boolean;
begin
  result := NumberOfArrays > 0;
  if NumberOfArrays = 0 then Exit;
  if Model.PackageGeneratedExternally(FileType) then
  begin
    Exit;
  end;

  FNameOfFile := FileName(AFileName);
  FInputFileName := FNameOfFile;
  WriteToNameFile(FileType, FileUnit, FNameOfFile, foInput, Model);
  OpenFile(FileName(AFileName));
  try
    frmProgressMM.AddMessage(Format(StrWritingSPackageI, [FileType]));
    frmProgressMM.AddMessage(StrWritingDataSet0);
    WriteDataSet0;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet1);
    WriteDataSet1;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSets2and3);
    WriteDataSets2And3;
  finally
    CloseFile;
  end;
end;

procedure TCustomMultZoneWriter.WriteTransientArrays;
begin
  WriteTransientPackageArrays(Model.ModflowPackages.RchPackage);
  WriteTransientPackageArrays(Model.ModflowPackages.EvtPackage);
  WriteTransientPackageArrays(Model.ModflowPackages.EtsPackage);
end;

{ TModflowZoneWriter }

class function TModflowZoneWriter.ArrayType: string;
begin
  result := 'Zone';
end;

function TModflowZoneWriter.ArrayUsed(const ArrayName: string): boolean;
begin
  result := UsedZoneArrayNames.IndexOf(ArrayName) >= 0
end;

procedure TModflowZoneWriter.CheckOverlappingZones;
var
  ParamIndex: Integer;
  Param: TModflowSteadyParameter;
  DataArray: TDataArray;
  ActiveDataArray: TDataArray;
  CheckArrays: array[TParameterType] of T3DIntegerDataSet;
  CheckArray: T3DIntegerDataSet;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Paramtype: TParameterType;
  PARTYP: string;
  ErrorMessage: string;
//  Paramtype: Integer;
begin
  ActiveDataArray := Model.DataArrayManager.GetDataSetByName(rsActive);
  if Model.ModflowPackages.LpfPackage.IsSelected
    or Model.ModflowPackages.UpwPackage.IsSelected then
  begin
    for ParamIndex := 0 to Model.ModflowSteadyParameters.Count - 1 do
    begin
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      Param := Model.ModflowSteadyParameters.Items[ParamIndex];
      if UseSteadyParameter(Param) then
      begin
        CheckArray := CheckArrays[Param.ParameterType];
        if Length(CheckArray) = 0 then
        begin
          SetLength(CheckArray, ActiveDataArray.LayerCount,
            ActiveDataArray.RowCount, ActiveDataArray.ColumnCount);

          for LayerIndex := 0 to ActiveDataArray.LayerCount - 1 do
          begin
            for RowIndex := 0 to ActiveDataArray.RowCount - 1 do
            begin
              for ColIndex := 0 to ActiveDataArray.ColumnCount - 1 do
              begin
                if ActiveDataArray.BooleanData[LayerIndex, RowIndex, ColIndex] then
                begin
                  CheckArray[LayerIndex, RowIndex, ColIndex] := 0;
                end
                else
                begin
                  CheckArray[LayerIndex, RowIndex, ColIndex] := -1;
                end;
              end;
            end;
          end;
        end;

        if Param.UseZone then
        begin
          DataArray := GetDataArray(Param);
          for LayerIndex := 0 to ActiveDataArray.LayerCount - 1 do
          begin
            for RowIndex := 0 to ActiveDataArray.RowCount - 1 do
            begin
              for ColIndex := 0 to ActiveDataArray.ColumnCount - 1 do
              begin
                if (CheckArray[LayerIndex, RowIndex, ColIndex] >= 0)
                  and DataArray.BooleanData[LayerIndex, RowIndex, ColIndex] then
                begin
                  Inc(CheckArray[LayerIndex, RowIndex, ColIndex])
                end;
              end;
            end;
          end;
          Model.DataArrayManager.CacheDataArrays;
          DataArray.CacheData;
        end
        else
        begin
          for LayerIndex := 0 to ActiveDataArray.LayerCount - 1 do
          begin
            for RowIndex := 0 to ActiveDataArray.RowCount - 1 do
            begin
              for ColIndex := 0 to ActiveDataArray.ColumnCount - 1 do
              begin
                if CheckArray[LayerIndex, RowIndex, ColIndex] >= 0 then
                begin
                  Inc(CheckArray[LayerIndex, RowIndex, ColIndex])
                end;
              end;
            end;
          end;
        end;

        CheckArrays[Param.ParameterType] := CheckArray;
      end;
    end;
  end;

  for Paramtype := Low(TParameterType) to High(TParameterType) do
  begin


    CheckArray := CheckArrays[Paramtype];
    if Length(CheckArray) > 0 then
    begin
      case Paramtype of
        ptLPF_HK: PARTYP := 'HK';
        ptLPF_HANI: PARTYP := 'HANI';
        ptLPF_VK: PARTYP := 'VK';
        ptLPF_VANI: PARTYP := 'VANI';
        ptLPF_SS: PARTYP := 'SS';
        ptLPF_SY: PARTYP := 'SY';
        ptLPF_VKCB: PARTYP := 'VKCB';
        else Assert(False);
      end;
      ErrorMessage := Format(StrMultiple0sParame, [PARTYP]);
      frmErrorsAndWarnings.RemoveWarningGroup(Model, ErrorMessage);
      for LayerIndex := 0 to ActiveDataArray.LayerCount - 1 do
      begin
        for RowIndex := 0 to ActiveDataArray.RowCount - 1 do
        begin
          for ColIndex := 0 to ActiveDataArray.ColumnCount - 1 do
          begin
            if CheckArray[LayerIndex,RowIndex, ColIndex] > 1 then
            begin
              frmErrorsAndWarnings.AddWarning(Model, ErrorMessage,
                Format(StrLayerRowCol, [LayerIndex+1,RowIndex+1, ColIndex+1]));
            end;
          end;
        end;
      end;
    end;
  end;
end;

constructor TModflowZoneWriter.Create(AModel: TCustomModel; EvaluationType: TEvaluationType);
begin
  inherited;
  FFileUnit := Model.UnitNumbers.UnitNumber(StrZONE);
  FFileType := 'ZONE';
end;

procedure TModflowZoneWriter.WriteTransientPackageArrays(TransPackage: TCustomTransientLayerPackageSelection);
var
  ArrayIndex: Integer;
  Item: TTransientZoneItem;
  ZONNAM: string;
  Lines: TStringList;
  LineIndex: Integer;
begin
  if TransPackage.IsSelected then
  begin
    for ArrayIndex := 0 to TransPackage.ZoneArrayNames.Count - 1 do
    begin
      Item := TransPackage.ZoneArrayNames[ArrayIndex];
      ZONNAM := Item.ArrayName;
      while Length(ZONNAM) < 11 do
      begin
        ZONNAM := ZONNAM + ' ';
      end;
      WriteString(ZONNAM);
      WriteString(DataSet2Comment);
      NewLine;
      if Item.Uniform then
      begin
        WriteConstantU2DINT(Item.ArrayName, Item.UniformValue, matStructured, ZONNAM);
      end
      else
      begin
        WriteU2DINTHeader(DataSet2Comment, matStructured, Item.ArrayName);
        if TFile.Exists(Item.FileName) then
        begin
          Lines := TStringList.Create;
          try
            Lines.LoadFromFile(Item.FileName);
            for LineIndex := 0 to Lines.Count - 1 do
            begin
              WriteString(Lines[LineIndex]);
              NewLine;
            end;
          finally
            Lines.Free;
          end;
        end
        else
        begin
          frmErrorsAndWarnings.AddError(Model, StrMissingZoneArrayF,
            Item.FileName);
        end;
      end;
    end;
  end;
end;

function TModflowZoneWriter.DataSet2Comment: string;
begin
  result := ' # ZONNAM';
end;

class function TModflowZoneWriter.Extension: string;
begin
  result := '.zon';
end;

function TModflowZoneWriter.GetArrayName(Param: TModflowSteadyParameter;
  LayerIndex: integer; AModel: TBaseModel): string;
begin
  result := Param.ZoneArrayName(LayerIndex, AModel);
end;

function TModflowZoneWriter.GetDataArray(
  Param: TModflowSteadyParameter): TDataArray;
begin
  result := Model.DataArrayManager.GetDataSetByName(Param.ZoneName);
end;

function TModflowZoneWriter.NumberOfArrays: integer;
//var
//  List: TList;
//  Index: Integer;
//  DataArray: TDataArray;
  function CountZoneArrays(Package: TCustomTransientLayerPackageSelection): integer;
  begin
    result := 0;
    if Package.IsSelected then
    begin
      result := Package.ZoneArrayNames.Count;
    end;
  end;
begin
  result := UsedZoneArrayNames.Count;
  result := result + CountZoneArrays(Model.ModflowPackages.RchPackage);
  result := result + CountZoneArrays(Model.ModflowPackages.EvtPackage);
  result := result + CountZoneArrays(Model.ModflowPackages.EtsPackage);
//  List := TransientArrayList;
//  for Index := 0 to List.Count - 1 do
//  begin
//    DataArray := List[Index];
//    if DataArray.LayerCount = 1 then
//    begin
//      Inc(result);
//    end
//    else
//    begin
//      Assert(False);
//    end;
//  end;
end;

//function TModflowZoneWriter.TransientArrayList: TList;
//begin
//  result := Model.TransientZoneArrays;
//end;

function TModflowZoneWriter.UsesHufParam(UsedParam: THufUsedParameter;
  var ArrayName: string; var DataArray: TDataArray): boolean;
begin
  result := inherited UsesHufParam(UsedParam, ArrayName, DataArray);
  if result then
  begin
    result := UsedParam.UseZone;
    if result then
    begin
      ArrayName := UsedParam.ZoneArrayName;
      if ArrayName = '' then
      begin
        UsedParam.GenerateZoneArrayName;
        ArrayName := UsedParam.ZoneArrayName;
        UsedZoneArrayNames.Add(ArrayName);
      end;
      DataArray := Model.DataArrayManager.GetDataSetByName(UsedParam.ZoneDataSetName);
    end;
  end;
end;

function TModflowZoneWriter.UseSteadyParameter(
  Param: TModflowSteadyParameter): boolean;
begin
  result := inherited UseSteadyParameter(Param);
  if result then
  begin
    result := Param.UseZone;
  end;
end;

procedure TModflowZoneWriter.WriteDataSet0;
begin
  WriteCommentLine('Zone (ZONE) File created on '
    + DateToStr(Now) + ' by ' + Model.ProgramName
    + ' version ' + IModelVersion + '.');
  if WritingTemplate then
  begin
    WriteCommentLine('(and then modified by a parameter estimation program.)');
  end;
end;

procedure TModflowZoneWriter.WriteDataSet1;
var
  UnitIndex: Integer;
  HGU: THydrogeologicUnit;
  ParamIndex: Integer;
  UsedParam: THufUsedParameter;
  ArrayName: string;
  DataArray: TDataArray;
begin
  //  If required, update UsedZoneArrayNames
  if Model.ModflowPackages.HufPackage.IsSelected then
  begin
    for UnitIndex := 0 to Model.HydrogeologicUnits.Count - 1 do
    begin
      HGU := Model.HydrogeologicUnits[UnitIndex];
      for ParamIndex := 0 to HGU.HufUsedParameters.Count - 1 do
      begin
        UsedParam := HGU.HufUsedParameters[ParamIndex];
        UsesHufParam(UsedParam, ArrayName, DataArray)
      end;
    end;
  end;

  WriteInteger(NumberOfArrays);
  WriteString(' # NZN');
  NewLine;
end;

function TModflowZoneWriter.WriteFile(const AFileName: string): boolean;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrMissingZoneArrayF);
  result := inherited;
  CheckOverlappingZones;
end;

{ TModflowMultiplierWriter }

class function TModflowMultiplierWriter.ArrayType: string;
begin
  result := 'Multiplier';
end;

function TModflowMultiplierWriter.ArrayUsed(const ArrayName: string): boolean;
begin
  result := UsedMultiplierArrayNames.IndexOf(ArrayName) >= 0
end;

constructor TModflowMultiplierWriter.Create(AModel: TCustomModel; EvaluationType: TEvaluationType);
begin
  inherited;
  FFileUnit := Model.UnitNumbers.UnitNumber(StrMULT);
  FFileType := 'MULT';
end;

function TModflowMultiplierWriter.DataSet2Comment: string;
begin
  result := ' # MLTNAM';
end;

class function TModflowMultiplierWriter.Extension: string;
begin
  result := '.mlt';
end;

function TModflowMultiplierWriter.GetArrayName(Param: TModflowSteadyParameter;
  LayerIndex: integer; AModel: TBaseModel): string;
begin
  result := Param.MultiplierArrayName(LayerIndex, AModel);
end;

function TModflowMultiplierWriter.GetDataArray(
  Param: TModflowSteadyParameter): TDataArray;
begin
  result := Model.DataArrayManager.GetDataSetByName(Param.MultiplierName);
end;

function TModflowMultiplierWriter.NumberOfArrays: integer;
//var
//  List: TList;
//  Index: Integer;
//  DataArray: TDataArray;
  function CountMultiplierArrays(Package: TCustomTransientLayerPackageSelection): integer;
  begin
    result := 0;
    if Package.IsSelected then
    begin
      result := Package.MultiplierArrayNames.Count;
    end;
  end;
begin
  result := UsedMultiplierArrayNames.Count;
  result := result + CountMultiplierArrays(Model.ModflowPackages.RchPackage);
  result := result + CountMultiplierArrays(Model.ModflowPackages.EvtPackage);
  result := result + CountMultiplierArrays(Model.ModflowPackages.EtsPackage);
//  List := TransientArrayList;
//  for Index := 0 to List.Count - 1 do
//  begin
//    DataArray := List[Index];
//    if DataArray.LayerCount = 1 then
//    begin
//      Inc(result);
//    end
//    else
//    begin
//      Assert(False);
//    end;
//  end;
end;

//function TModflowMultiplierWriter.TransientArrayList: TList;
//begin
//  result := Model.TransientMultiplierArrays;
//end;

function TModflowMultiplierWriter.UsesHufParam(UsedParam: THufUsedParameter;
  var ArrayName: string; var DataArray: TDataArray): boolean;
begin
  result := inherited UsesHufParam(UsedParam, ArrayName, DataArray);
  if result then
  begin
    result := UsedParam.UseMultiplier;
    if result then
    begin
      ArrayName := UsedParam.MultiplierArrayName;
      if ArrayName = '' then
      begin
        UsedParam.GenerateMultiplierArrayName;
        ArrayName := UsedParam.MultiplierArrayName;
        UsedMultiplierArrayNames.Add(ArrayName);
      end;
      DataArray := Model.DataArrayManager.GetDataSetByName(UsedParam.MultiplierDataSetName);
    end;
  end;
end;

function TModflowMultiplierWriter.UseSteadyParameter(
  Param: TModflowSteadyParameter): boolean;
begin
  result := inherited UseSteadyParameter(Param);
  if result then
  begin
    result := Param.UseMultiplier;
  end;
end;

procedure TModflowMultiplierWriter.WriteDataSet0;
begin
  WriteCommentLine('Multiplier (MULT) File created on '
    + DateToStr(Now) + ' by ' + Model.ProgramName
    + ' version ' + IModelVersion + '.');
  if WritingTemplate then
  begin
    WriteCommentLine('(and then modified by a parameter estimation program.)');
  end;
end;

procedure TModflowMultiplierWriter.WriteDataSet1;
var
  UnitIndex: Integer;
  HGU: THydrogeologicUnit;
  ParamIndex: Integer;
  UsedParam: THufUsedParameter;
  ArrayName: string;
  DataArray: TDataArray;
begin
  //  If required, update UsedMultiplierArrayNames
  if Model.ModflowPackages.HufPackage.IsSelected then
  begin
    for UnitIndex := 0 to Model.HydrogeologicUnits.Count - 1 do
    begin
      HGU := Model.HydrogeologicUnits[UnitIndex];
      for ParamIndex := 0 to HGU.HufUsedParameters.Count - 1 do
      begin
        UsedParam := HGU.HufUsedParameters[ParamIndex];
        UsesHufParam(UsedParam, ArrayName, DataArray)
      end;
    end;
  end;
  
  WriteInteger(NumberOfArrays);
  WriteString(' # NML');
  NewLine;
end;

function TModflowMultiplierWriter.WriteFile(const AFileName: string): boolean;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrMissingMultArrayF);
  result := inherited;
end;

procedure TModflowMultiplierWriter.WriteTransientPackageArrays(
  TransPackage: TCustomTransientLayerPackageSelection);
var
  ArrayIndex: Integer;
  Item: TTransientMultItem;
  MLTNAM: string;
  Lines: TStringList;
  LineIndex: Integer;
begin
  if TransPackage.IsSelected then
  begin
    for ArrayIndex := 0 to TransPackage.MultiplierArrayNames.Count - 1 do
    begin
      Item := TransPackage.MultiplierArrayNames[ArrayIndex];
      MLTNAM := Item.ArrayName;
      while Length(MLTNAM) < 11 do
      begin
        MLTNAM := MLTNAM + ' ';
      end;
      WriteString(MLTNAM);
      WriteString(DataSet2Comment);
      NewLine;
      if Item.Uniform then
      begin
        WriteConstantU2DREL(Item.ArrayName, Item.UniformValue, matStructured, Item.ArrayName);
      end
      else
      begin
        WriteU2DRELHeader(DataSet2Comment, matStructured, Item.ArrayName);
        Lines := TStringList.Create;
        if TFile.Exists(Item.FileName) then
        begin
          try
            Lines.LoadFromFile(Item.FileName);
            for LineIndex := 0 to Lines.Count - 1 do
            begin
              WriteString(Lines[LineIndex]);
              NewLine;
            end;
          finally
            Lines.Free;
          end;
        end
        else
        begin
          frmErrorsAndWarnings.AddError(Model, StrMissingMultArrayF,
            Item.FileName);
        end;
      end;
    end;
  end;
end;

end.
