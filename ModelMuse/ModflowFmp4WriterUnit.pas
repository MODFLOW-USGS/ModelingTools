{@name writes the FMP input file for MODFLOW-OWHM version 2.}
unit ModflowFmp4WriterUnit;

interface

uses
  System.Classes, System.Contnrs, Vcl.Forms, System.SysUtils,
  CustomModflowWriterUnit, PhastModelUnit, ModflowFmpWellUnit, ModflowCellUnit,
  ModflowPackageSelectionUnit, ScreenObjectUnit, ModflowBoundaryUnit, RbwParser,
  DataSetUnit, OrderedCollectionUnit, GoPhastTypes, ModflowBoundaryDisplayUnit;

type
  TWriteLocation = (wlMain, wlOpenClose, wlOFE, wlCID, wlFID, wlRoot, wlCropUse, wlETR,
    wlEtFrac, wlSwLosses, wlPFLX, wlCropFunc, wlWaterCost, wlDeliveries,
    wlSemiRouteDeliv, wlSemiRouteReturn, wlCall, wlEfficiency,
    wlEfficiencyImprovement, wlBareRunoffFraction,
    wlBarePrecipitationConsumptionFraction);

  TWriteTransientData = procedure (WriteLocation: TWriteLocation) of object;

  TCheckDataSet = procedure (IntegerArray: TDataArray;
    const ErrorMessage: string) of object;

  TTransientDataUsed = function (Sender: TObject): Boolean of object;

  TEvaluateProcedure = procedure of object;

  TUpdateRequirements = record
    EvaluateProcedure: TEvaluateProcedure;
    TransientDataUsed: TTransientDataUsed;
    WriteLocation: TWriteLocation;
    TimeLists: TModflowBoundListOfTimeLists;
  end;

  TRequiredValues = record
    WriteLocation: TWriteLocation;
    Comment: string;
    DataTypeIndex: Integer;
    DataType: TRbwDataType;
    DefaultValue: Double;
    ErrorID: string;
    ID: string;
    StaticDataName: string;
    WriteTransientData: Boolean;
    CheckProcedure: TCheckDataSet;
    CheckError: string;
  end;

  TModflowFmp4Writer = class(TCustomListWriter)
  private
    FFarmProcess4: TFarmProcess4;
    FClimatePackage: TFarmProcess4Climate;
    FLandUse: TFarmProcess4LandUse;
    FACtiveSurfaceCells: array of array of boolean;
    FWriteLocation: TWriteLocation;

    FFarmIDs: TList;
    FRefEts: TList;
    FPrecip: TList;
    FCropIDs: TList;
    FEfficiencies: TList;
    FEfficiencyImprovements: TList;
    FBareRunoffFractions: TList;
    FBarePrecipitationConsumptionFractions: TList;

    FFarmWellID: Integer;
    FBaseName: string;

    FFID_FileStream: TFileStream;
    FOpenCloseFileStream: TFileStream;
    FPFLX_FileStream: TFileStream;
    FETR_FileStream: TFileStream;
    FCID_FileStream: TFileStream;
    FEFFICIENCY_FileStream: TFileStream;
    FEFFICIENCY_IMPROVEMENT_FileStream: TFileStream;
    FBARE_RUNOFF_FRACTION_FileStream: TFileStream;
    FBarePrecipitationConsumptionFractionFileStream: TFileStream;
    procedure WriteGobalDimension;
    procedure WriteOutput;
    procedure WriteWaterBalanceSubregion;
    procedure WriteSoil;
    procedure WriteClimate;
    procedure WriteSurfaceWater;
    procedure WriteSupplyWell;
    procedure WriteAllotments;
    procedure WriteLandUse;
    procedure WriteSalinityFlush;
    procedure WriteSurfaceWaterIrrigation;
    procedure WriteFileInternal;
    procedure EvaluateAll;

    procedure EvaluateFarmID;
    procedure WriteFarmLocation;

    procedure EvaluateCropID;
    procedure WriteLandUseLocation;

    procedure EvaluateReferenceET;
    procedure WriteRefET;

    procedure EvaluatePrecip;
    procedure WritePrecipitation;

    procedure EvaluateEfficiency;
    procedure WriteEfficiency;

    procedure EvaluateEfficiencyImprovement;
    procedure WriteEfficiencyImprovement;

    procedure EvaluateBareRunoffFraction;
    procedure WriteBareRunoffFraction;

    procedure EvaluateBarePrecipitationConsumptionFraction;
    procedure WriteBarePrecipitationConsumptionFraction;

    procedure FreeFileStreams;
    // wbs location
//    procedure WriteDataSet26(TimeIndex: Integer);
    procedure CheckDataSetZeroOrPositive(IntegerArray: TDataArray;
      const ErrorMessage: string);
    procedure CheckDataSetBetweenZeroAndOne(RealArray: TDataArray;
      const ErrorMessage: string);
    procedure EvaluateActiveCells;
    procedure RemoveErrorAndWarningMessages;
    procedure WriteTransientFmpArrayData(RequiredValues: TRequiredValues);
    function GetTransientList(WriteLocation: TWriteLocation): TList;
    function GetFileStreamName(WriteLocation: TWriteLocation): string;
    function GetFmpBoundary(ScreenObject: TScreenObject;
      WriteLocation: TWriteLocation): TModflowBoundary;
    procedure EvaluateTransientArrayData(WriteLocation: TWriteLocation);
    procedure WriteFmpArrayData(AFileName: string; RequiredValues: TRequiredValues);
    procedure UpdateDisplay(UpdateRequirements: TUpdateRequirements);
    function TransientCropUsed(Sender: TObject): Boolean;
    function TransientRefEtUsed(Sender: TObject): Boolean;
  protected
    class function Extension: string; override;
    function Package: TModflowPackageSelection; override;
    function CellType: TValueCellType; override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowBoundary;
      override;
    procedure WriteCell(Cell: TValueCell;
      const DataSetIdentifier, VariableIdentifiers: string); override;
    function ParameterType: TParameterType; override;
    procedure WriteParameterCells(CellList: TValueCellList; NLST: Integer;
      const VariableIdentifiers, DataSetIdentifier: string;
      AssignmentMethod: TUpdateMethod;
      MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection); override;
  public
    // @name creates and instance of @classname.
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
    procedure WriteString(const Value: AnsiString); overload; override;
    procedure WriteConstantU2DINT(const Comment: string;
      const Value: integer; ArrayType: TModflowArrayType;
      const MF6_ArrayName: string); override;
    procedure WriteConstantU2DREL(const Comment: string;
      const Value: double; ArrayType: TModflowArrayType;
      const MF6_ArrayName: string); override;
    procedure UpdateFarmIDDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateRefEtDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdatePrecipDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateCropIDDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateEfficiencyDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateEfficiencyImprovementDisplay(
      TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateBareRunoffFractionDisplay(
      TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateBarePrecipitationConsumptionFractionDisplay(
      TimeLists: TModflowBoundListOfTimeLists);
  end;


implementation

uses
  frmErrorsAndWarningsUnit, ModflowFmpWriterUnit,
  ModflowFmpFarmIdUnit, ModflowUnitNumbers, frmProgressUnit, ModflowFmpEvapUnit,
  ModflowFmpPrecipitationUnit;

resourcestring
  StrUndefinedError = 'Undefined %s in one or more stress periods';
  StrInvalidEfficiencyV = 'Invalid Efficiency value';
  StrInvalidEfficiencyI = 'Invalid Efficiency Improvement value';
  StrInvalidBareRunoff = 'Invalid Bare Runoff Fraction value';

{ TModflowFmp4Writer }

function TModflowFmp4Writer.CellType: TValueCellType;
begin
  result := TFmpWell_Cell;
end;

procedure TModflowFmp4Writer.CheckDataSetBetweenZeroAndOne(
  RealArray: TDataArray; const ErrorMessage: string);
var
  ColIndex: Integer;
  RowIndex: Integer;
begin
  Assert(RealArray <> nil);
  for RowIndex := 0 to Model.Grid.RowCount - 1 do
  begin
    for ColIndex := 0 to Model.Grid.ColumnCount - 1 do
    begin
      if FACtiveSurfaceCells[RowIndex, ColIndex] then
      begin
        if RealArray.IsValue[0, RowIndex, ColIndex] then
        begin
          if RealArray.DataType = rdtInteger then
          begin
            if (RealArray.IntegerData[0, RowIndex, ColIndex] < 0)
              or (RealArray.IntegerData[0, RowIndex, ColIndex] > 1) then
            begin
              frmErrorsAndWarnings.AddError(Model, ErrorMessage,
                Format(StrRow0dColumn, [RowIndex + 1, ColIndex + 1]));
            end;
          end
          else
          begin
            if (RealArray.RealData[0, RowIndex, ColIndex] < 0)
              or (RealArray.RealData[0, RowIndex, ColIndex] > 1) then
            begin
              frmErrorsAndWarnings.AddError(Model, ErrorMessage,
                Format(StrRow0dColumn, [RowIndex + 1, ColIndex + 1]));
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.CheckDataSetZeroOrPositive(IntegerArray: TDataArray;
  const ErrorMessage: string);
var
  ColIndex: Integer;
  RowIndex: Integer;
begin
  Assert(IntegerArray <> nil);
  for RowIndex := 0 to Model.Grid.RowCount - 1 do
  begin
    for ColIndex := 0 to Model.Grid.ColumnCount - 1 do
    begin
      if FACtiveSurfaceCells[RowIndex, ColIndex] then
      begin
        if IntegerArray.IsValue[0, RowIndex, ColIndex] then
        begin
          if IntegerArray.DataType = rdtInteger then
          begin
            if (IntegerArray.IntegerData[0, RowIndex, ColIndex] < 0) then
            begin
              frmErrorsAndWarnings.AddError(Model, ErrorMessage,
                Format(StrRow0dColumn, [RowIndex + 1, ColIndex + 1]));
            end;
          end
          else
          begin
            if (IntegerArray.RealData[0, RowIndex, ColIndex] < 0) then
            begin
              frmErrorsAndWarnings.AddError(Model, ErrorMessage,
                Format(StrRow0dColumn, [RowIndex + 1, ColIndex + 1]));
            end;
          end;
        end;
      end;
    end;
  end;
end;

constructor TModflowFmp4Writer.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FFarmIDs := TObjectList.Create;
  FRefEts := TObjectList.Create;
  FPrecip := TObjectList.Create;
  FCropIDs := TObjectList.Create;
  FEfficiencies := TObjectList.Create;
  FEfficiencyImprovements := TObjectList.Create;
  FBareRunoffFractions := TObjectList.Create;
  FBarePrecipitationConsumptionFractions := TObjectList.Create;

  FFarmProcess4 := Package as TFarmProcess4;
  FClimatePackage := Model.ModflowPackages.FarmClimate4;
  FLandUse := Model.ModflowPackages.FarmLandUse;
end;

destructor TModflowFmp4Writer.Destroy;
begin
  FreeFileStreams;
  FBarePrecipitationConsumptionFractions.Free;
  FBareRunoffFractions.Free;
  FEfficiencyImprovements.Free;
  FEfficiencies.Free;
  FCropIDs.Free;
  FPrecip.Free;
  FRefEts.Free;
  FFarmIDs.Free;
  inherited;
end;

procedure TModflowFmp4Writer.EvaluateActiveCells;
var
  RowIndex: Integer;
  LayerIndex: Integer;
  ActiveDataSet: TDataArray;
  ColumnIndex: Integer;
begin
  SetLength(FACtiveSurfaceCells, Model.Grid.RowCount, Model.Grid.ColumnCount);
  ActiveDataSet := Model.DataArrayManager.GetDataSetByName(rsActive);
  for RowIndex := 0 to Model.Grid.RowCount - 1 do
  begin
    for ColumnIndex := 0 to Model.Grid.ColumnCount - 1 do
    begin
      FACtiveSurfaceCells[RowIndex,ColumnIndex] := False;
      for LayerIndex := 0 to Model.Grid.LayerCount - 1 do
      begin
        FACtiveSurfaceCells[RowIndex,ColumnIndex] :=
          ActiveDataSet.BooleanData[LayerIndex, RowIndex, ColumnIndex];
        if FACtiveSurfaceCells[RowIndex,ColumnIndex] then
        begin
          break;
        end;
      end;
    end;
  end;
end;

procedure TModflowFmp4Writer.EvaluateAll;
begin
  EvaluateActiveCells;
  EvaluateFarmID;
  EvaluateReferenceET;
  EvaluatePrecip;
  EvaluateCropID;
  EvaluateEfficiency;
  EvaluateEfficiencyImprovement;
  EvaluateBareRunoffFraction;
  EvaluateBarePrecipitationConsumptionFraction
end;

procedure TModflowFmp4Writer.EvaluateBarePrecipitationConsumptionFraction;
begin
  if FFarmProcess4.TransientArrayBarePrecipitationConsumptionFractionDisplayUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidBareRunoff);
    EvaluateTransientArrayData(wlBarePrecipitationConsumptionFraction);
  end;
end;

procedure TModflowFmp4Writer.EvaluateBareRunoffFraction;
begin
  if FFarmProcess4.TransientArrayBareRunoffFractionDisplayUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidBareRunoff);
    EvaluateTransientArrayData(wlBareRunoffFraction);
  end;
end;

procedure TModflowFmp4Writer.EvaluateCropID;
begin
  if FLandUse.CropLocation = rstTransient then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidCropIDInF);
    EvaluateTransientArrayData(wlCID);
  end;
end;

procedure TModflowFmp4Writer.EvaluateEfficiency;
begin
  if (FFarmProcess4.EfficiencyOptions.ArrayList = alArray)
    and (FFarmProcess4.EfficiencyOptions.FarmOption = foTransient) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidEfficiencyV);
    EvaluateTransientArrayData(wlEfficiency);
  end;

end;

procedure TModflowFmp4Writer.EvaluateEfficiencyImprovement;
begin
  if FFarmProcess4.TransientArrayEfficiencyImprovementUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidEfficiencyI);
    EvaluateTransientArrayData(wlEfficiencyImprovement);
  end;
end;

procedure TModflowFmp4Writer.EvaluateFarmID;
begin
  if FFarmProcess4.Farms.FarmOption = foTransient then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidFarmID);
    EvaluateTransientArrayData(wlFID);
  end;
end;

procedure TModflowFmp4Writer.EvaluatePrecip;
begin
  if FClimatePackage.TransientPrecipUsed(Self) then
  begin
    EvaluateTransientArrayData(wlPFLX);
  end;
end;

procedure TModflowFmp4Writer.EvaluateReferenceET;
begin
  if FClimatePackage.TransientPrecipUsed(Self) then
  begin
    EvaluateTransientArrayData(wlETR);
  end;
end;

class function TModflowFmp4Writer.Extension: string;
begin
  Result := '.fmp';
end;

procedure TModflowFmp4Writer.FreeFileStreams;
begin
  FreeAndNil(FFID_FileStream);
  FreeAndNil(FPFLX_FileStream);
  FreeAndNil(FETR_FileStream);
  FreeAndNil(FCID_FileStream);
  FreeAndNil(FEFFICIENCY_FileStream);
  FreeAndNil(FEFFICIENCY_IMPROVEMENT_FileStream);
  FreeAndNil(FBARE_RUNOFF_FRACTION_FileStream);

end;

function TModflowFmp4Writer.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.ModflowFmpWellBoundary;
end;

function TModflowFmp4Writer.GetFileStreamName(WriteLocation: TWriteLocation): string;
begin
  case WriteLocation of
    wlMain:
      begin
      end;
    wlOpenClose:
      begin
      end;
    wlOFE:
      begin
      end;
    wlCID:
      begin
        RESULT := ChangeFileExt(FBaseName, '.CID');
        if FCID_FileStream = nil then
        begin
          FCID_FileStream := TFileStream.Create(RESULT,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlFID:
      begin
        RESULT := ChangeFileExt(FBaseName, '.FID');
        if FFID_FileStream = nil then
        begin
          FFID_FileStream := TFileStream.Create(RESULT,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlRoot:
      begin
      end;
    wlCropUse:
      begin
      end;
    wlETR:
      begin
        result := ChangeFileExt(FBaseName, '.ETR');
        if FETR_FileStream = nil then
        begin
          FETR_FileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlEtFrac:
      begin
      end;
    wlSwLosses:
      begin
      end;
    wlPFLX:
      begin
        result := ChangeFileExt(FBaseName, '.PFLX');
        if FPFLX_FileStream = nil then
        begin
          FPFLX_FileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlCropFunc:
      begin
      end;
    wlWaterCost:
      begin
      end;
    wlDeliveries:
      begin
      end;
    wlSemiRouteDeliv:
      begin
      end;
    wlSemiRouteReturn:
      begin
      end;
    wlCall:
      begin
      end;
    wlEfficiency:
      begin
        result := ChangeFileExt(FBaseName, '.EFFICIENCY');
        if FEFFICIENCY_FileStream = nil then
        begin
          FEFFICIENCY_FileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlEfficiencyImprovement:
      begin
        result := ChangeFileExt(FBaseName, '.EFFICIENCY_IMPROVEMENT');
        if FEFFICIENCY_IMPROVEMENT_FileStream = nil then
        begin
          FEFFICIENCY_IMPROVEMENT_FileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlBareRunoffFraction:
      begin
        result := ChangeFileExt(FBaseName, '.BARE_RUNOFF_FRACTION');
        if FBARE_RUNOFF_FRACTION_FileStream = nil then
        begin
          FBARE_RUNOFF_FRACTION_FileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlBarePrecipitationConsumptionFraction:
      begin
        result := ChangeFileExt(FBaseName, '.BARE_PRECIPITATION_CONSUMPTION_FRACTION');
        if FBarePrecipitationConsumptionFractionFileStream = nil then
        begin
          FBarePrecipitationConsumptionFractionFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    else Assert(False);
  end;
end;

function TModflowFmp4Writer.GetFmpBoundary(ScreenObject: TScreenObject;
  WriteLocation: TWriteLocation): TModflowBoundary;
begin
  result := nil;
  case WriteLocation of
    wlMain: ;
    wlOpenClose: ;
    wlOFE: ;
    wlCID: result := ScreenObject.ModflowFmpCropID;
    wlFID: result := ScreenObject.ModflowFmpFarmID;
    wlRoot: ;
    wlCropUse: ;
    wlETR: result := ScreenObject.ModflowFmpRefEvap;
    wlEtFrac: ;
    wlSwLosses: ;
    wlPFLX: result := ScreenObject.ModflowFmpPrecip;
    wlCropFunc: ;
    wlWaterCost: ;
    wlDeliveries: ;
    wlSemiRouteDeliv: ;
    wlSemiRouteReturn: ;
    wlCall: ;
    wlEfficiency: result := ScreenObject.Fmp4EfficiencyBoundary;
    wlEfficiencyImprovement: result := ScreenObject.Fmp4EfficiencyImprovementBoundary;
    wlBareRunoffFraction: result := ScreenObject.Fmp4BareRunoffFractionBoundary;
    wlBarePrecipitationConsumptionFraction: result := ScreenObject.Fmp4BarePrecipitationConsumptionFractionBoundary;
    else Assert(False);
  end;
end;

function TModflowFmp4Writer.GetTransientList(
  WriteLocation: TWriteLocation): TList;
begin
  result := nil;
  case WriteLocation of
    wlMain: ;
    wlOpenClose: ;
    wlOFE: ;
    wlCID: result := FCropIDs;
    wlFID: result := FFarmIDs;
    wlRoot: ;
    wlCropUse: ;
    wlETR: result := FRefEts;
    wlEtFrac: ;
    wlSwLosses: ;
    wlPFLX: result := FPrecip;
    wlCropFunc: ;
    wlWaterCost: ;
    wlDeliveries: ;
    wlSemiRouteDeliv: ;
    wlSemiRouteReturn: ;
    wlCall: ;
    wlEfficiency: result := FEfficiencies;
    wlEfficiencyImprovement: result := FEfficiencyImprovements;
    wlBareRunoffFraction: result := FBareRunoffFractions;
    wlBarePrecipitationConsumptionFraction: result := FBarePrecipitationConsumptionFractions;
    else Assert(False)
  end;
end;

function TModflowFmp4Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.FarmProcess4;
end;

function TModflowFmp4Writer.ParameterType: TParameterType;
begin
  result := ptUndefined;
  Assert(False);
end;

procedure TModflowFmp4Writer.RemoveErrorAndWarningMessages;
begin

end;

procedure TModflowFmp4Writer.UpdateBarePrecipitationConsumptionFractionDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateBarePrecipitationConsumptionFraction;
  UpdateRequirements.TransientDataUsed := (Package as TFarmProcess4).TransientArrayBarePrecipitationConsumptionFractionDisplayUsed;
  UpdateRequirements.WriteLocation := wlBarePrecipitationConsumptionFraction;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateBareRunoffFractionDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
//var
//  DataSets: TList;
//  BareRunoffFraction: TModflowBoundaryDisplayTimeList;
//  TimeIndex: integer;
//  TimeListIndex: integer;
//  List: TValueCellList;
//  TimeList: TModflowBoundaryDisplayTimeList;
//  DataArray: TDataArray;
//  EfficiencyIndex: integer;
//  DataSetIndex: integer;
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateBareRunoffFraction;
  UpdateRequirements.TransientDataUsed := (Package as TFarmProcess4).TransientArrayBareRunoffFractionDisplayUsed;
  UpdateRequirements.WriteLocation := wlBareRunoffFraction;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

function TModflowFmp4Writer.TransientCropUsed(Sender: TObject): Boolean;
begin
  result := FLandUse.CropLocation = rstTransient;
end;

procedure TModflowFmp4Writer.UpdateCropIDDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateCropID;
  UpdateRequirements.TransientDataUsed := TransientCropUsed;
  UpdateRequirements.WriteLocation := wlCID;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateDisplay(
  UpdateRequirements: TUpdateRequirements);
var
  DataSets: TList;
  Efficiency: TModflowBoundaryDisplayTimeList;
  TimeIndex: integer;
  TimeListIndex: integer;
  List: TValueCellList;
  TimeList: TModflowBoundaryDisplayTimeList;
  DataArray: TDataArray;
  EfficiencyIndex: integer;
  DataSetIndex: integer;
  BoundaryLists: TList;
begin
  EvaluateActiveCells;
  frmErrorsAndWarnings.BeginUpdate;
  try
    RemoveErrorAndWarningMessages;
    if not UpdateRequirements.TransientDataUsed(self) then
    begin
      UpdateNotUsedDisplay(UpdateRequirements.TimeLists);
      Exit;
    end;
    DataSets := TList.Create;
    try
      UpdateRequirements.EvaluateProcedure;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      Efficiency := UpdateRequirements.TimeLists[0];
      for TimeIndex := 0 to Efficiency.Count - 1 do
      begin
        DataSets.Clear;

        for TimeListIndex := 0 to UpdateRequirements.TimeLists.Count - 1 do
        begin
          TimeList := UpdateRequirements.TimeLists[TimeListIndex];
          DataArray := TimeList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataSets.Add(DataArray);
        end;

        BoundaryLists := GetTransientList(UpdateRequirements.WriteLocation);
        for EfficiencyIndex := 0 to BoundaryLists.Count - 1 do
        begin
          List := BoundaryLists[EfficiencyIndex];
          UpdateCellDisplay(List, DataSets, [], nil, [0]);
          List.Cache;
        end;
        for DataSetIndex := 0 to DataSets.Count - 1 do
        begin
          DataArray := DataSets[DataSetIndex];
          DataArray.UpToDate := True;
//          CheckDataSetZeroOrPositive(DataArray, StrInvalidFarmID);
          DataArray.CacheData;
        end;
      end;

      SetTimeListsUpToDate(UpdateRequirements.TimeLists);
    finally
      DataSets.Free;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowFmp4Writer.UpdateEfficiencyDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateEfficiency;
  UpdateRequirements.TransientDataUsed := (Package as TFarmProcess4).
    FarmTransientArrayEfficiencyUsed;
  UpdateRequirements.WriteLocation := wlEfficiency;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateEfficiencyImprovementDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateEfficiencyImprovement;
  UpdateRequirements.TransientDataUsed := (Package as TFarmProcess4).
    TransientArrayEfficiencyImprovementUsed;
  UpdateRequirements.WriteLocation := wlEfficiencyImprovement;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateFarmIDDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateFarmID;
  UpdateRequirements.TransientDataUsed := (Package as TFarmProcess4).
    TransientFarmIdUsed;
  UpdateRequirements.WriteLocation := wlFID;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdatePrecipDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluatePrecip;
  UpdateRequirements.TransientDataUsed := FClimatePackage.TransientPrecipUsed;
  UpdateRequirements.WriteLocation := wlPFLX;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.WriteFmpArrayData(AFileName: string; RequiredValues: TRequiredValues);
var
  DataArray: TDataArray;
begin
  WriteString('  ');
  WriteString(RequiredValues.ID);
  WriteString(' ');
  if RequiredValues.WriteTransientData then
  begin
    WriteString('TRANSIENT ARRAY DATAFILE ');
    WriteString(ExtractFileName(AFileName));
    NewLine;
    WriteTransientFmpArrayData(RequiredValues);
  end
  else
  begin
    WriteString('STATIC ARRAY DATAFILE ');
    WriteString(ExtractFileName(AFileName));
    NewLine;
    FWriteLocation := RequiredValues.WriteLocation;
    try
      DataArray := Model.DataArrayManager.GetDataSetByName(RequiredValues.StaticDataName);
      WriteArray(DataArray, 0, RequiredValues.ErrorID, '', RequiredValues.ID, False, False);
      if Assigned(RequiredValues.CheckProcedure) then
      begin
        RequiredValues.CheckProcedure(DataArray, RequiredValues.CheckError);
      end;
    finally
      FWriteLocation := wlMain;
    end;
  end;
end;

procedure TModflowFmp4Writer.EvaluateTransientArrayData(WriteLocation: TWriteLocation);
var
  TransList: TList;
  EmptyParamList: TStringList;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TModflowBoundary;
begin
  TransList := GetTransientList(WriteLocation);
  frmErrorsAndWarnings.BeginUpdate;
  EmptyParamList := TStringList.Create;
  try
    for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
    begin
      ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      if not ScreenObject.UsedModels.UsesModel(Model) then
      begin
        Continue;
      end;
      Boundary := GetFmpBoundary(ScreenObject, WriteLocation);
      if Boundary <> nil then
      begin
        Boundary.GetCellValues(TransList, EmptyParamList, Model, self);
      end;
    end;
  finally
    EmptyParamList.Free;
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

function TModflowFmp4Writer.TransientRefEtUsed(Sender: TObject): Boolean;
begin
  result := FClimatePackage.ReferenceET.FarmOption = foTransient;
end;

procedure TModflowFmp4Writer.UpdateRefEtDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateReferenceET;
  UpdateRequirements.TransientDataUsed := TransientRefEtUsed;
  UpdateRequirements.WriteLocation := wlETR;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.WriteAllotments;
begin

end;

procedure TModflowFmp4Writer.WriteBarePrecipitationConsumptionFraction;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FFarmProcess4.Bare_Runoff_Fraction.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  AFileName := GetFileStreamName(wlBarePrecipitationConsumptionFraction);

  if (FFarmProcess4.Bare_Runoff_Fraction.ArrayList = alArray) then
  begin
    RequiredValues.WriteLocation := wlBarePrecipitationConsumptionFraction;
    RequiredValues.DefaultValue := 0;
    RequiredValues.DataType := rdtDouble;
    RequiredValues.DataTypeIndex := 0;
    RequiredValues.Comment := 'FMP WBS: BARE_RUNOFF_FRACTION';
    RequiredValues.ErrorID := 'FMP WBS: BARE_RUNOFF_FRACTION';
    RequiredValues.ID := 'BARE_RUNOFF_FRACTION';
    RequiredValues.StaticDataName := KBarePrecipitationConsumptionFraction;
    RequiredValues.WriteTransientData :=
      (FFarmProcess4.Bare_Runoff_Fraction.FarmOption = foTransient);
    RequiredValues.CheckProcedure := CheckDataSetBetweenZeroAndOne;
    RequiredValues.CheckError := StrInvalidBareRunoff;

    WriteFmpArrayData(AFileName, RequiredValues);
  end
  else
  begin

  end;
end;

procedure TModflowFmp4Writer.WriteBareRunoffFraction;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FFarmProcess4.Bare_Runoff_Fraction.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  AFileName := GetFileStreamName(wlBareRunoffFraction);

  if (FFarmProcess4.Bare_Runoff_Fraction.ArrayList = alArray) then
  begin
    RequiredValues.WriteLocation := wlBareRunoffFraction;
    RequiredValues.DefaultValue := 0;
    RequiredValues.DataType := rdtDouble;
    RequiredValues.DataTypeIndex := 0;
    RequiredValues.Comment := 'FMP WBS: BARE_RUNOFF_FRACTION';
    RequiredValues.ErrorID := 'FMP WBS: BARE_RUNOFF_FRACTION';
    RequiredValues.ID := 'BARE_RUNOFF_FRACTION';
    RequiredValues.StaticDataName := KBareRunoffFraction;
    RequiredValues.WriteTransientData :=
      (FFarmProcess4.Bare_Runoff_Fraction.FarmOption = foTransient);
    RequiredValues.CheckProcedure := CheckDataSetBetweenZeroAndOne;
    RequiredValues.CheckError := StrInvalidBareRunoff;

    WriteFmpArrayData(AFileName, RequiredValues);
  end
  else
  begin

  end;
end;

procedure TModflowFmp4Writer.WriteCell(Cell: TValueCell;
  const DataSetIdentifier, VariableIdentifiers: string);
begin
  Assert(False);
end;

procedure TModflowFmp4Writer.WriteClimate;
begin
  if FClimatePackage.IsSelected then
  begin
    WriteString('BEGIN CLIMATE');
    NewLine;

    WritePrecipitation;
    WriteRefET;

    WriteString('END CLIMATE');
    NewLine;
    NewLine;
  end;
end;

procedure TModflowFmp4Writer.WriteConstantU2DINT(const Comment: string;
  const Value: integer; ArrayType: TModflowArrayType;
  const MF6_ArrayName: string);
var
  OldLocation: TWriteLocation;
begin
  case FWriteLocation of
    wlMain: inherited;
    wlOpenClose:
      begin
        OldLocation := FWriteLocation;
        FWriteLocation := wlMain;
        try
          inherited;
        finally
          FWriteLocation := OldLocation
        end;
      end;
    wlCID, wlFID:
      begin
        inherited;
      end
    else
      Assert(False);
  end;
end;

procedure TModflowFmp4Writer.WriteConstantU2DREL(const Comment: string;
  const Value: double; ArrayType: TModflowArrayType;
  const MF6_ArrayName: string);
begin
  inherited;

end;

procedure TModflowFmp4Writer.WriteEfficiency;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FFarmProcess4.EfficiencyOptions.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  AFileName := GetFileStreamName(wlEfficiency);

  if (FFarmProcess4.EfficiencyOptions.ArrayList = alArray) then
  begin
    RequiredValues.WriteLocation := wlEfficiency;
    RequiredValues.DefaultValue := 0;
    RequiredValues.DataType := rdtDouble;
    RequiredValues.DataTypeIndex := 0;
    RequiredValues.Comment := 'FMP WBS: Efficiency';
    RequiredValues.ErrorID := 'FMP WBS: Efficiency';
    RequiredValues.ID := 'EFFICIENCY';
    RequiredValues.StaticDataName := KEfficiency;
    RequiredValues.WriteTransientData :=
      (FFarmProcess4.EfficiencyOptions.FarmOption = foTransient);
    RequiredValues.CheckProcedure := CheckDataSetBetweenZeroAndOne;
    RequiredValues.CheckError := StrInvalidEfficiencyV;

    WriteFmpArrayData(AFileName, RequiredValues);
  end
  else
  begin

  end;

end;

procedure TModflowFmp4Writer.WriteEfficiencyImprovement;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FFarmProcess4.EfficiencyImprovement.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  AFileName := GetFileStreamName(wlEfficiencyImprovement);

  if (FFarmProcess4.EfficiencyImprovement.ArrayList = alArray) then
  begin
    RequiredValues.WriteLocation := wlEfficiencyImprovement;
    RequiredValues.DefaultValue := 0;
    RequiredValues.DataType := rdtDouble;
    RequiredValues.DataTypeIndex := 0;
    RequiredValues.Comment := 'FMP WBS: Efficiency Improvement';
    RequiredValues.ErrorID := 'FMP WBS: Efficiency Improvement';
    RequiredValues.ID := 'EFFICIENCY_IMPROVEMENT';
    RequiredValues.StaticDataName := KEfficiencyImprovement;
    RequiredValues.WriteTransientData :=
      (FFarmProcess4.EfficiencyImprovement.FarmOption = foTransient);
    RequiredValues.CheckProcedure := CheckDataSetBetweenZeroAndOne;
    RequiredValues.CheckError := StrInvalidEfficiencyI;

    WriteFmpArrayData(AFileName, RequiredValues);
  end
  else
  begin

  end;
end;

procedure TModflowFmp4Writer.WriteLandUseLocation;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  AFileName := GetFileStreamName(wlCID);

  RequiredValues.WriteLocation := wlCID;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtInteger;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.Comment := 'FMP LAND_USE: Location';
  RequiredValues.ErrorID := 'FMP LAND_USE: Location';
  RequiredValues.ID := 'LOCATION';
  RequiredValues.StaticDataName := KLand_Use_ID;
  RequiredValues.WriteTransientData := FLandUse.CropLocation = rstTransient;
  RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
  RequiredValues.CheckError := StrInvalidCropIDInF;

  WriteFmpArrayData(AFileName, RequiredValues);
end;

procedure TModflowFmp4Writer.WriteTransientFmpArrayData(RequiredValues: TRequiredValues);
var
  ValueCellList: TValueCellList;
  Dummy: TDataArray;
  TransList: TList;
  StressPeriodIndex: Integer;
  CheckAssigned: Boolean;
begin
  TransList := GetTransientList(RequiredValues.WriteLocation);

  FWriteLocation := RequiredValues.WriteLocation;
  try
    for StressPeriodIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
    begin
      if TransList.Count <= StressPeriodIndex then
      begin
        frmErrorsAndWarnings.AddError(Model,
          Format(StrUndefinedError, [RequiredValues.ErrorID]),
          IntToStr(StressPeriodIndex+1));
        Exit;
      end;
      ValueCellList := TransList[StressPeriodIndex];
      CheckAssigned := Assigned(RequiredValues.CheckProcedure);
      Dummy := nil;
      try
        WriteCommentLine(Format('Stress Period %d', [StressPeriodIndex+1]));
        WriteTransient2DArray(RequiredValues.Comment, RequiredValues.DataTypeIndex,
          RequiredValues.DataType, RequiredValues.DefaultValue,
          ValueCellList, umAssign, False, Dummy, RequiredValues.ID,
          (not CheckAssigned), False);
        if CheckAssigned then
        begin
          RequiredValues.CheckProcedure(Dummy, RequiredValues.CheckError);
        end;
      finally
        Dummy.Free;
      end;
    end;
  finally
    FWriteLocation := wlMain;
  end;
end;

procedure TModflowFmp4Writer.WriteFarmLocation;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FFarmProcess4.Farms.FarmOption = foNotUsed then
  begin
    Assert(False);
  end;

  AFileName := GetFileStreamName(wlFID);

  RequiredValues.WriteLocation := wlFID;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtInteger;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.Comment := 'FMP WBS: Location';
  RequiredValues.ErrorID := 'FMP WBS: Location';
  RequiredValues.ID := 'LOCATION';
  RequiredValues.StaticDataName := KFarmID;
  RequiredValues.WriteTransientData := FFarmProcess4.Farms.FarmOption = foTransient;
  RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
  RequiredValues.CheckError := StrInvalidFarmID;

  WriteFmpArrayData(AFileName, RequiredValues);
end;

procedure TModflowFmp4Writer.WriteFile(const AFileName: string);
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidFarmProcess);
  if (not FFarmProcess4.IsSelected)
{$IFDEF OWHMV2}
  or (Model.ModelSelection <> msModflowOwhm2)
{$ENDIF}
  then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrFMP) then
  begin
    Exit;
  end;
  if Model is TChildModel then
  begin
    if not TChildModel(Model).ParentModel.ModflowPackages.FarmProcess4.IsSelected then
    begin
      frmErrorsAndWarnings.AddError(Model, StrInvalidFarmProcess, StrIfTheFarmProcess);
    end;
  end;
  FNameOfFile := FileName(AFileName);
  FInputFileName := FNameOfFile;
  FBaseName := ChangeFileExt(FNameOfFile, '');

  WriteToNameFile(StrFMP, Model.UnitNumbers.UnitNumber(StrFMP),
    FNameOfFile, foInput, Model);

  EvaluateAll;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  FPestParamUsed := False;

  WriteFileInternal;

  if  Model.PestUsed and FPestParamUsed then
  begin
    frmErrorsAndWarnings.BeginUpdate;
    try
      FreeFileStreams;
      FNameOfFile := FNameOfFile + '.tpl';
      WritePestTemplateLine(FNameOfFile);
      WritingTemplate := True;
      WriteFileInternal;

    finally
      frmErrorsAndWarnings.EndUpdate;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteFileInternal;
begin
  ClearTimeLists(Model);
  FFarmWellID := 1;
  OpenFile(FNameOfFile);
  try
    FWriteLocation := wlMain;
    frmProgressMM.AddMessage('Writing FMP3 Package input.');
    frmProgressMM.AddMessage(StrWritingDataSet0);

    WriteTemplateHeader;

    WriteDataSet0;
    WriteGobalDimension;
    WriteOutput;
    WriteWaterBalanceSubregion;
    WriteSoil;
    WriteClimate;
    WriteSurfaceWater;
    WriteSupplyWell;
    WriteAllotments;
    WriteLandUse;
    WriteSalinityFlush;
    WriteSurfaceWaterIrrigation;
  finally
    CloseFile;
  end;
end;

procedure TModflowFmp4Writer.WriteGobalDimension;
begin
  WriteString('BEGIN GLOBAL DIMENSION');
  NewLine;

  WriteString('  NWBS');
  WriteInteger(1);
  NewLine;

  WriteString('  NCROP');
  WriteInteger(0);
  NewLine;

  WriteString('  NSOIL');
  if Model.ModflowPackages.FarmSoil4.IsSelected then
  begin
    WriteInteger(0);
  end
  else
  begin
    WriteInteger(0);
  end;
  NewLine;

  WriteString('END');
  NewLine;
  NewLine;
end;

procedure TModflowFmp4Writer.WriteLandUse;
begin
  if FLandUse.IsSelected then
  begin
    WriteString('BEGIN LAND_USE');
    NewLine;

    // remove this.
    WriteString('  SINGLE_LAND_USE_PER_CELL');
    NewLine;

    WriteLandUseLocation;

    // remove this.
    WriteString('  ROOT_DEPTH STATIC CONSTANT 0.00001');
    NewLine;

    WriteString('END LAND_USE');
    NewLine;
    NewLine;
  end;
end;

procedure TModflowFmp4Writer.WriteOutput;
begin
  WriteString('BEGIN OUTPUT');
  NewLine;


  if fpWbs_Water_Use in FFarmProcess4.FarmPrints then
  begin
    WriteString('  WBS_WATER_USE ');
    WriteString(ExtractFileName(ChangeFileExt(FBaseName, '.WBS_WATER_USE')));
    NewLine;
  end;

  WriteString('END OUTPUT');
  NewLine;
  NewLine;
end;

procedure TModflowFmp4Writer.WriteParameterCells(CellList: TValueCellList;
  NLST: Integer; const VariableIdentifiers, DataSetIdentifier: string;
  AssignmentMethod: TUpdateMethod;
  MultiplierArrayNames: TTransientMultCollection;
  ZoneArrayNames: TTransientZoneCollection);
begin
  Assert(False);

end;

procedure TModflowFmp4Writer.WritePrecipitation;
var
  AFileName: string;
//  DataArray: TDataArray;
  RequiredValues: TRequiredValues;
begin
  if FClimatePackage.Precipitation.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  AFileName := GetFileStreamName(wlPFLX);

  RequiredValues.WriteLocation := wlPFLX;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.Comment := 'Data Set 33: PFLX';
  RequiredValues.ErrorID := 'FMP CLIMATE: PRECIPITATION';
  RequiredValues.ID := 'PRECIPITATION';
  RequiredValues.StaticDataName := KPrecipitation;
  RequiredValues.WriteTransientData := FClimatePackage.TransientPrecipUsed(Self);
  RequiredValues.CheckProcedure := nil;
  RequiredValues.CheckError := '';

  WriteFmpArrayData(AFileName, RequiredValues);
end;

procedure TModflowFmp4Writer.WriteRefET;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FClimatePackage.ReferenceET.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  AFileName := GetFileStreamName(wlETR);

  RequiredValues.WriteLocation := wlETR;
  RequiredValues.DefaultValue := 0;
  RequiredValues.DataType := rdtDouble;
  RequiredValues.DataTypeIndex := 0;
  RequiredValues.Comment := 'Data Set 30b: ETR';
  RequiredValues.ErrorID := 'FMP CLIMATE: REFERENCE_ET';
  RequiredValues.ID := 'REFERENCE_ET';
  RequiredValues.StaticDataName := KRefET;
  RequiredValues.WriteTransientData := FClimatePackage.TransientEvapUsed(Self);
  RequiredValues.CheckProcedure := nil;
  RequiredValues.CheckError := '';

  WriteFmpArrayData(AFileName, RequiredValues);
end;

procedure TModflowFmp4Writer.WriteSalinityFlush;
begin

end;

procedure TModflowFmp4Writer.WriteSoil;
begin
  WriteString('BEGIN SOIL');
  NewLine;

  WriteString('END SOIL');
  NewLine;
  NewLine;
end;

procedure TModflowFmp4Writer.WriteString(const Value: AnsiString);
begin
  if Length(Value) > 0 then
  begin
    case FWriteLocation of
      wlMain: inherited;
      wlOpenClose:
        begin
          Assert(FOpenCloseFileStream <> nil);
          FOpenCloseFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlOFE:
        begin
          Assert(False);
//          Assert(FOFE_FileStream <> nil);
//          FOFE_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlCID:
        begin
          Assert(FCID_FileStream <> nil);
          FCID_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlFID:
        begin
          Assert(FFID_FileStream <> nil);
          FFID_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlRoot:
        begin
          Assert(False);
//          Assert(FRoot_FileStream <> nil);
//          FRoot_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlCropUse:
        begin
          Assert(False);
//          Assert(FCropUse_FileStream <> nil);
//          FCropUse_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlETR:
        begin
          Assert(FETR_FileStream <> nil);
          FETR_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlEtFrac:
        begin
          Assert(False);
//          Assert(FET_Frac_FileStream <> nil);
//          FET_Frac_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlSwLosses:
        begin
          Assert(False);
//          Assert(FSW_Losses_FileStream <> nil);
//          FSW_Losses_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlPFLX:
        begin
          Assert(FPFLX_FileStream <> nil);
          FPFLX_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlCropFunc:
        begin
          Assert(False);
//          Assert(FCropFunc_FileStream <> nil);
//          FCropFunc_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlWaterCost:
        begin
          Assert(False);
//          Assert(FWaterCost_FileStream <> nil);
//          FWaterCost_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlDeliveries:
        begin
          Assert(False);
//          Assert(FDeliveries_FileStream <> nil);
//          FDeliveries_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlSemiRouteDeliv:
        begin
          Assert(False);
//          Assert(FSemiDeliveries_FileStream <> nil);
//          FSemiDeliveries_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlSemiRouteReturn:
        begin
          Assert(False);
//          Assert(FSemiReturn_FileStream <> nil);
//          FSemiReturn_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlCall:
        begin
          Assert(False);
//          Assert(FCall_FileStream <> nil);
//          FCall_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlEfficiency:
        begin
          Assert(FEFFICIENCY_FileStream <> nil);
          FEFFICIENCY_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlEfficiencyImprovement:
        begin
          Assert(FEFFICIENCY_IMPROVEMENT_FileStream <> nil);
          FEFFICIENCY_IMPROVEMENT_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlBareRunoffFraction:
        begin
          Assert(FBARE_RUNOFF_FRACTION_FileStream <> nil);
          FBARE_RUNOFF_FRACTION_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlBarePrecipitationConsumptionFraction:
        begin
          Assert(FBarePrecipitationConsumptionFractionFileStream <> nil);
          FBarePrecipitationConsumptionFractionFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      else
        Assert(False);
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteSupplyWell;
begin

end;

procedure TModflowFmp4Writer.WriteSurfaceWater;
begin

end;

procedure TModflowFmp4Writer.WriteSurfaceWaterIrrigation;
begin

end;

procedure TModflowFmp4Writer.WriteWaterBalanceSubregion;
begin
  WriteString('BEGIN WATER_BALANCE_SUBREGION');
  NewLine;

  WriteFarmLocation;
  WriteEfficiency;
  WriteEfficiencyImprovement;

  if (FFarmProcess4.DeficiencyScenario.FarmOption <> foNotUsed) then
  begin
    WriteString('  PRORATE_DEFICIENCY ');
    case FFarmProcess4.ProrateDeficiency of
      pdoByDemand:
        begin
          WriteString('ByDEMAND')
        end;
      pdoAverage:
        begin
          WriteString('ByAVERAGE')
        end;
      else
        begin
          Assert(False);
        end;
    end;
    NewLine;
  end;

  WriteBareRunoffFraction;
  WriteBarePrecipitationConsumptionFraction;

  WriteString('END WATER_BALANCE_SUBREGION');
  NewLine;
  NewLine;
end;

end.
