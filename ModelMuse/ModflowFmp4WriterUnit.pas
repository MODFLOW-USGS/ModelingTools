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
    wlBarePrecipitationConsumptionFraction, wlCapillaryFringe, wlSoilID,
    wlSurfaceK, wlBareEvap, wlDirectRecharge, wlPrecipPotConsumption,
    wlNrdInfilLoc, wlLandUseAreaFraction, wlCropCoefficient);

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
    Option: string;
    FarmProperty: TFarmProperty;
    LandUseStaticFileNames: TStringList;
  end;

  TModflowFmp4Writer = class(TCustomListWriter)
  private
    FFarmProcess4: TFarmProcess4;
    FClimatePackage: TFarmProcess4Climate;
    FLandUse: TFarmProcess4LandUse;
    FSurfaceWater4: TFarmProcess4SurfaceWater;

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
    FEvapBare: TList;
    FDirectRecharge: TList;
    FPrecipPotConsumption: TList;
    FNrdInfilLocation: TList;
    FLandUseAreaFraction: TList;
    FCropCoefficient: TList;

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
    FSoil4: TFarmProcess4Soil;
    FCapillaryFringeFileStream: TFileStream;
    FSoilIdStream: TFileStream;
    FSurfaceKFileStream: TFileStream;
    FEvapBareFileStream: TFileStream;
    FDirectRechargeFileStream: TFileStream;
    FPrecipPotConsumptionFileStream: TFileStream;
    FNrdInfilLocationFileStream: TFileStream;
    FCropcoefficientFileStream: TFileStream;
    FLandUseAreaFractionFileStream: TFileStream;
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

    procedure EvaluateBareEvap;
    procedure WriteBareEvap;

    procedure EvaluateDirectRecharge;
    procedure WriteDirectRecharge;

    procedure EvaluatePrecipPotConsumption;
    procedure WritePrecipPotConsumption;

    procedure EvaluateNrdInfilLocation;
    procedure WriteNrdInfilLocation;

    procedure EvaluateLandUseAreaFraction;
    procedure WriteLandUseAreaFraction;

    procedure EvaluateCropCoefficient;
    procedure WriteCropCoefficient;

    procedure WriteCapillaryFringe;
    procedure WriteSoilID;
    procedure WriteSurfaceK;

    procedure FreeFileStreams;
    // wbs location
//    procedure WriteDataSet26(TimeIndex: Integer);
    procedure CheckDataSetZeroOrPositive(IntegerArray: TDataArray;
      const ErrorMessage: string);
    procedure CheckDataSetZeroOrGETen(IntegerArray: TDataArray;
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
    procedure WriteLandUseArrayData(AFileName: string; RequiredValues: TRequiredValues);
    procedure UpdateDisplay(UpdateRequirements: TUpdateRequirements);
    function TransientCropUsed(Sender: TObject): Boolean;
    function TransientRefEtUsed(Sender: TObject): Boolean;
    procedure GetScaleFactorsAndExternalFile(RequiredValues: TRequiredValues; var UnitConversionScaleFactor: string; var ExternalFileName: string; var ExternalScaleFileName: string);
    procedure WriteScaleFactorsAndID(RequiredValues: TRequiredValues; UnitConversionScaleFactor: string; ExternalScaleFileName: string);
    procedure WriteLandUseOption;
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
    procedure UpdateEvapBareDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateDirectRechargeDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdatePrecipPotConsumptionDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateNrdInfilLocationDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateLandUseAreaFractionDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateCropCoefficentDisplay(TimeLists: TModflowBoundListOfTimeLists);
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
  StrInvalidPrecipConsumpRunoff = 'Invalid Bare Precipitation Consumption Fraction value';
  StrInvalidCapillaryFringe = 'Invalid Capillary Fringe value';
  StrInvalidPotentialEv = 'Invalid Potential Evaporation Bare value';

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

procedure TModflowFmp4Writer.CheckDataSetZeroOrGETen(IntegerArray: TDataArray;
  const ErrorMessage: string);
var
  ColIndex: Integer;
  RowIndex: Integer;
  Value: Integer;
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
            Value := IntegerArray.IntegerData[0, RowIndex, ColIndex];
            if not ((Value = 0) or (Value >= 10)) then
            begin
              frmErrorsAndWarnings.AddError(Model, ErrorMessage,
                Format(StrRow0dColumn, [RowIndex + 1, ColIndex + 1]));
            end;
          end
          else
          begin
            Assert(False);
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
  FEvapBare := TObjectList.Create;
  FDirectRecharge := TObjectList.Create;
  FPrecipPotConsumption := TObjectList.Create;
  FNrdInfilLocation := TObjectList.Create;
  FLandUseAreaFraction := TObjectList.Create;
  FCropCoefficient := TObjectList.Create;

  FFarmProcess4 := Package as TFarmProcess4;
  FClimatePackage := Model.ModflowPackages.FarmClimate4;
  FLandUse := Model.ModflowPackages.FarmLandUse;
  FSoil4 := Model.ModflowPackages.FarmSoil4;
  FSurfaceWater4 := Model.ModflowPackages.FarmSurfaceWater4;
end;

destructor TModflowFmp4Writer.Destroy;
begin
  FreeFileStreams;

  FCropCoefficient.Free;
  FLandUseAreaFraction.Free;
  FNrdInfilLocation.Free;
  FPrecipPotConsumption.Free;
  FDirectRecharge.Free;
  FEvapBare.Free;
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
  EvaluateBarePrecipitationConsumptionFraction;
  EvaluateBareEvap;
  EvaluateDirectRecharge;
  EvaluatePrecipPotConsumption;
  EvaluateNrdInfilLocation;
  EvaluateLandUseAreaFraction;
  EvaluateCropCoefficient;
end;

procedure TModflowFmp4Writer.EvaluateBareEvap;
begin
  if FClimatePackage.TransientBareEvapUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidPotentialEv);
    EvaluateTransientArrayData(wlBareEvap);
  end;
end;

procedure TModflowFmp4Writer.EvaluateBarePrecipitationConsumptionFraction;
begin
  if FFarmProcess4.TransientArrayBarePrecipitationConsumptionFractionDisplayUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidPrecipConsumpRunoff);
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

procedure TModflowFmp4Writer.EvaluateCropCoefficient;
begin
  if FLandUse.TransientCropCoefficientarrayUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, 'Invalid Crop Coefficient value');
    EvaluateTransientArrayData(wlCropCoefficient);
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

procedure TModflowFmp4Writer.EvaluateDirectRecharge;
begin
  if FClimatePackage.TransientDirectRechargeUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, 'Invalid Direct Recharge value');
    EvaluateTransientArrayData(wlDirectRecharge);
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
  if FFarmProcess4.TransientFarmIdUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidFarmID);
    EvaluateTransientArrayData(wlFID);
  end;
end;

procedure TModflowFmp4Writer.EvaluateLandUseAreaFraction;
begin
  if FLandUse.TransientLandUseAreaFractionarrayUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, 'Invalid Land Use Area Fraction');
    EvaluateTransientArrayData(wlLandUseAreaFraction);
  end;
end;

procedure TModflowFmp4Writer.EvaluateNrdInfilLocation;
begin
  if FSurfaceWater4.TransientNrdInfilLocationUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, 'Invalid Non-Routed Delivery Infiltration Location in Farm Process');
    EvaluateTransientArrayData(wlNrdInfilLoc);
  end;
end;

procedure TModflowFmp4Writer.EvaluatePrecip;
begin
  if FClimatePackage.TransientPrecipUsed(Self) then
  begin
    EvaluateTransientArrayData(wlPFLX);
  end;
end;

procedure TModflowFmp4Writer.EvaluatePrecipPotConsumption;
begin
  if FClimatePackage.TransientPrecipPotConsumptionUsed(nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, 'Invalid Precipitation Potential Consumption value');
    EvaluateTransientArrayData(wlPrecipPotConsumption);
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
  FreeAndNil(FOpenCloseFileStream);
  FreeAndNil(FPFLX_FileStream);
  FreeAndNil(FETR_FileStream);
  FreeAndNil(FCID_FileStream);
  FreeAndNil(FEFFICIENCY_FileStream);
  FreeAndNil(FEFFICIENCY_IMPROVEMENT_FileStream);
  FreeAndNil(FBARE_RUNOFF_FRACTION_FileStream);
  FreeAndNil(FBarePrecipitationConsumptionFractionFileStream);
  FreeAndNil(FCapillaryFringeFileStream);
  FreeAndNil(FSoilIdStream);
  FreeAndNil(FSurfaceKFileStream);
  FreeAndNil(FEvapBareFileStream);
  FreeAndNil(FDirectRechargeFileStream);
  FreeAndNil(FPrecipPotConsumptionFileStream);
  FreeAndNil(FNrdInfilLocationFileStream);
  FreeAndNil(FLandUseAreaFractionFileStream);
  FreeAndNil(FCropcoefficientFileStream);

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
    wlCapillaryFringe:
      begin
        result := ChangeFileExt(FBaseName, '.CAPILLARY_FRINGE');
        if FCapillaryFringeFileStream = nil then
        begin
          FCapillaryFringeFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlSoilID:
      begin
        result := ChangeFileExt(FBaseName, '.SOIL_ID');
        if FSoilIdStream = nil then
        begin
          FSoilIdStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlSurfaceK:
      begin
        result := ChangeFileExt(FBaseName, '.SURFACE_VERTICAL_HYDRAULIC_CONDUCTIVITY');
        if FSurfaceKFileStream = nil then
        begin
          FSurfaceKFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlBareEvap:
      begin
        result := ChangeFileExt(FBaseName, '.POTENTIAL_EVAPORATION_BARE');
        if FEvapBareFileStream = nil then
        begin
          FEvapBareFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlDirectRecharge:
      begin
        result := ChangeFileExt(FBaseName, '.DIRECT_RECHARGE');
        if FDirectRechargeFileStream = nil then
        begin
          FDirectRechargeFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlPrecipPotConsumption:
      begin
        result := ChangeFileExt(FBaseName, '.PRECIPITATION_POTENTIAL_CONSUMPTION');
        if FPrecipPotConsumptionFileStream = nil then
        begin
          FPrecipPotConsumptionFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlNrdInfilLoc:
      begin
        result := ChangeFileExt(FBaseName, '.NRD_INFILTRATION_LOCATION');
        if FNrdInfilLocationFileStream = nil then
        begin
          FNrdInfilLocationFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlLandUseAreaFraction:
      begin
        result := ChangeFileExt(FBaseName, '.LAND_USE_AREA_FRACTION');
        if FLandUseAreaFractionFileStream = nil then
        begin
          FLandUseAreaFractionFileStream := TFileStream.Create(result,
            fmCreate or fmShareDenyWrite);
        end;
      end;
    wlCropCoefficient:
      begin
        result := ChangeFileExt(FBaseName, '.CROP_COEFFICIENT');
        if FCropcoefficientFileStream = nil then
        begin
          FCropcoefficientFileStream := TFileStream.Create(result,
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
    wlBarePrecipitationConsumptionFraction: result :=
      ScreenObject.Fmp4BarePrecipitationConsumptionFractionBoundary;
    wlCapillaryFringe: ;
    wlSoilID: ;
    wlSurfaceK: ;
    wlBareEvap: result := ScreenObject.ModflowFmpBareEvap;
    wlDirectRecharge: result := ScreenObject.ModflowFmpDirectRecharge;
    wlPrecipPotConsumption: result := ScreenObject.ModflowFmpPrecipPotConsumption;
    wlNrdInfilLoc: result := ScreenObject.ModflowFmp4NrdInfilLocationBoundary;
    wlLandUseAreaFraction: result := ScreenObject.ModflowFmp4LandUseAreaFraction;
    wlCropCoefficient: result := ScreenObject.ModflowFmp4CropCoefficient;
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
    wlCapillaryFringe: ;
    wlSoilID: ;
    wlSurfaceK: ;
    wlBareEvap: result := FEvapBare;
    wlDirectRecharge: result := FDirectRecharge;
    wlPrecipPotConsumption: result := FPrecipPotConsumption;
    wlNrdInfilLoc: result := FNrdInfilLocation;
    wlLandUseAreaFraction: result := FLandUseAreaFraction;
    wlCropCoefficient: result := FCropCoefficient;
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

procedure TModflowFmp4Writer.UpdateCropCoefficentDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateCropCoefficient;
  UpdateRequirements.TransientDataUsed := FLandUse.
    TransientCropCoefficientarrayUsed;
  UpdateRequirements.WriteLocation := wlCropCoefficient;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.WriteLandUseOption;
begin
  case FLandUse.LandUseOption of
    luoSingle:
      begin
        WriteString('  SINGLE_LAND_USE_PER_CELL');
      end;
    luoMultiple:
      begin
        WriteString('  MULTIPLE_LAND_USE_PER_CELL');
      end;
  end;
  NewLine;
end;

procedure TModflowFmp4Writer.WriteScaleFactorsAndID(RequiredValues: TRequiredValues; UnitConversionScaleFactor: string; ExternalScaleFileName: string);
begin
  if UnitConversionScaleFactor <> '' then
  begin
    WriteString('  INTERNAL SF ');
    WriteString(UnitConversionScaleFactor);
    NewLine;
  end;
  if ExternalScaleFileName <> '' then
  begin
    WriteString('  SFAC OPEN/CLOSE ');
    WriteString(ExternalScaleFileName);
    NewLine;
  end;
  WriteString('  ');
  WriteString(RequiredValues.ID);
  WriteString(' ');
end;

procedure TModflowFmp4Writer.GetScaleFactorsAndExternalFile(RequiredValues: TRequiredValues; var UnitConversionScaleFactor: string; var ExternalFileName: string; var ExternalScaleFileName: string);
begin
  if RequiredValues.FarmProperty <> nil then
  begin
    UnitConversionScaleFactor := RequiredValues.FarmProperty.UnitConversionScaleFactor;
    ExternalFileName := RequiredValues.FarmProperty.ExternalFileName;
    ExternalScaleFileName := RequiredValues.FarmProperty.ExternalScaleFileName;
  end
  else
  begin
    UnitConversionScaleFactor := '';
    ExternalFileName := '';
    ExternalScaleFileName := '';
  end;
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

procedure TModflowFmp4Writer.UpdateDirectRechargeDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateDirectRecharge;
  UpdateRequirements.TransientDataUsed := FClimatePackage.
    TransientDirectRechargeUsed;
  UpdateRequirements.WriteLocation := wlDirectRecharge;
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

procedure TModflowFmp4Writer.UpdateEvapBareDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateBareEvap;
  UpdateRequirements.TransientDataUsed := FClimatePackage.
    TransientBareEvapUsed;
  UpdateRequirements.WriteLocation := wlBareEvap;
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

procedure TModflowFmp4Writer.UpdateLandUseAreaFractionDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateLandUseAreaFraction;
  UpdateRequirements.TransientDataUsed := FLandUse.
    TransientLandUseAreaFractionarrayUsed;
  UpdateRequirements.WriteLocation := wlLandUseAreaFraction;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.UpdateNrdInfilLocationDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluateNrdInfilLocation;
  UpdateRequirements.TransientDataUsed := FSurfaceWater4.
    TransientNrdInfilLocationUsed;
  UpdateRequirements.WriteLocation := wlNrdInfilLoc;
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

procedure TModflowFmp4Writer.UpdatePrecipPotConsumptionDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  UpdateRequirements: TUpdateRequirements;
begin
  UpdateRequirements.EvaluateProcedure := EvaluatePrecipPotConsumption;
  UpdateRequirements.TransientDataUsed := FClimatePackage.
    TransientPrecipPotConsumptionUsed;
  UpdateRequirements.WriteLocation := wlPrecipPotConsumption;
  UpdateRequirements.TimeLists := TimeLists;
  UpdateDisplay(UpdateRequirements);
end;

procedure TModflowFmp4Writer.WriteFmpArrayData(AFileName: string; RequiredValues: TRequiredValues);
var
  DataArray: TDataArray;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  UnitConversionScaleFactor: string;
begin
  GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
    ExternalFileName, ExternalScaleFileName);

  if RequiredValues.WriteTransientData then
  begin
    WriteString('  ');
    WriteString(RequiredValues.ID);
    WriteString(' ');
    if RequiredValues.Option <> '' then
    begin
      WriteString(RequiredValues.Option + ' ');
    end;
    WriteString('TRANSIENT ARRAY DATAFILE ');
    if ExternalFileName <> '' then
    begin
      WriteString(ExternalFileName);
      NewLine;
    end
    else
    begin
      WriteString(ExtractFileName(AFileName));
      NewLine;
      WriteTransientFmpArrayData(RequiredValues);
    end;
  end
  else
  begin
    WriteScaleFactorsAndID(RequiredValues,
      UnitConversionScaleFactor, ExternalScaleFileName);

    if RequiredValues.Option <> '' then
    begin
      WriteString(RequiredValues.Option + ' ');
    end;
    DataArray := nil;
    if ExternalFileName = '' then
    begin
      DataArray := Model.DataArrayManager.GetDataSetByName(RequiredValues.StaticDataName);
      Assert(DataArray <> nil);
    end;
    if (DataArray <> nil) then
    begin
      DataArray.Initialize;
    end;
    if (DataArray <> nil) and (DataArray.IsUniform = iuTrue) then
    begin
      WriteString('STATIC CONSTANT ');
      if DataArray.DataType = rdtDouble then
      begin
        WriteFloat(DataArray.RealData[0,0,0]);
      end
      else
      begin
        WriteInteger(DataArray.IntegerData[0,0,0]);
      end;
      NewLine;
    end
    else
    begin
      WriteString('STATIC ARRAY DATAFILE ');
      if ExternalFileName <> '' then
      begin
        WriteString(ExternalFileName);
        NewLine;
      end
      else
      begin
        WriteString(ExtractFileName(AFileName));
        NewLine;
        FWriteLocation := RequiredValues.WriteLocation;
        try
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

procedure TModflowFmp4Writer.WriteBareEvap;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FClimatePackage.Potential_Evaporation_Bare.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  AFileName := GetFileStreamName(wlBareEvap);

  if (FClimatePackage.Potential_Evaporation_Bare.ArrayList = alArray) then
  begin
    RequiredValues.WriteLocation := wlBareEvap;
    RequiredValues.DefaultValue := 0;
    RequiredValues.DataType := rdtDouble;
    RequiredValues.DataTypeIndex := 0;
    RequiredValues.Comment := 'FMP CLIMATE: POTENTIAL_EVAPORATION_BARE';
    RequiredValues.ErrorID := 'FMP CLIMATE: POTENTIAL_EVAPORATION_BARE';
    RequiredValues.ID := 'POTENTIAL_EVAPORATION_BARE';
    RequiredValues.StaticDataName := KPotential_Evap_Bare;
    RequiredValues.WriteTransientData :=
      (FClimatePackage.Potential_Evaporation_Bare.FarmOption = foTransient);
    RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
    RequiredValues.CheckError := StrInvalidPotentialEv;
    RequiredValues.Option := '';
    RequiredValues.FarmProperty := FClimatePackage.Potential_Evaporation_Bare;

    WriteFmpArrayData(AFileName, RequiredValues);
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TModflowFmp4Writer.WriteBarePrecipitationConsumptionFraction;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FFarmProcess4.Bare_Precipitation_Consumption_Fraction.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  AFileName := GetFileStreamName(wlBarePrecipitationConsumptionFraction);

  if (FFarmProcess4.Bare_Precipitation_Consumption_Fraction.ArrayList = alArray) then
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
      (FFarmProcess4.Bare_Precipitation_Consumption_Fraction.FarmOption = foTransient);
    RequiredValues.CheckProcedure := CheckDataSetBetweenZeroAndOne;
    RequiredValues.CheckError := StrInvalidPrecipConsumpRunoff;
    RequiredValues.Option := '';
    RequiredValues.FarmProperty := FFarmProcess4.Bare_Precipitation_Consumption_Fraction;

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
    RequiredValues.Option := '';
    RequiredValues.FarmProperty := FFarmProcess4.Bare_Runoff_Fraction;

    WriteFmpArrayData(AFileName, RequiredValues);
  end
  else
  begin

  end;
end;

procedure TModflowFmp4Writer.WriteCapillaryFringe;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FSoil4.CapFringe.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  AFileName := GetFileStreamName(wlCapillaryFringe);

  if (FSoil4.CapFringe.ArrayList = alArray) then
  begin
    RequiredValues.WriteLocation := wlCapillaryFringe;
    RequiredValues.DefaultValue := 0;
    RequiredValues.DataType := rdtDouble;
    RequiredValues.DataTypeIndex := 0;
    RequiredValues.Comment := 'FMP SOIL: CAPILLARY_FRINGE';
    RequiredValues.ErrorID := 'FMP SOIL: CAPILLARY_FRINGE';
    RequiredValues.ID := 'CAPILLARY_FRINGE';
    RequiredValues.StaticDataName := KCapillary_Fringe;
    RequiredValues.WriteTransientData := False;
    RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
    RequiredValues.CheckError := StrInvalidCapillaryFringe;
    RequiredValues.Option := '';
    RequiredValues.FarmProperty := FSoil4.CapFringe;

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
    WriteBareEvap;

    WriteString('  REFERENCE_ET_TO_BARE');
    WriteFloat(FClimatePackage.RefEtToBare);
    NewLine;

    WriteDirectRecharge;
    WritePrecipPotConsumption;

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
    wlCID, wlFID, wlSoilID, wlNrdInfilLoc:
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

procedure TModflowFmp4Writer.WriteCropCoefficient;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  DataArrayNames: TStringList;
  CropIndex: Integer;
begin
  if FLandUse.CropCoeff.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  AFileName := GetFileStreamName(wlCropCoefficient);

  if (FLandUse.CropCoeff.ArrayList = alArray) then
  begin
    RequiredValues.WriteLocation := wlCropCoefficient;
    RequiredValues.DefaultValue := 0;
    RequiredValues.DataType := rdtDouble;
    RequiredValues.DataTypeIndex := 0;
    RequiredValues.Comment := 'FMP LAND_USE: CROP_COEFFICIENT';
    RequiredValues.ErrorID := 'FMP LAND_USE: CROP_COEFFICIENT';
    RequiredValues.ID := 'CROP_COEFFICIENT';
    RequiredValues.StaticDataName := KCropCoefficient;
    RequiredValues.WriteTransientData :=
      (FLandUse.CropCoeff.FarmOption = foTransient);
    RequiredValues.CheckError :=  'Invalid Crop Coefficient value';
    RequiredValues.CheckProcedure := CheckDataSetBetweenZeroAndOne;
    RequiredValues.Option := '';
    RequiredValues.FarmProperty := FLandUse.CropCoeff;

    if FLandUse.LandUseOption = luoSingle then
    begin
      WriteFmpArrayData(AFileName, RequiredValues);
    end
    else
    begin
      DataArrayNames := TStringList.Create;
      try
        for CropIndex := 0 to Model.FmpCrops.Count - 1 do
        begin
          DataArrayNames.Add(
            Model.FmpCrops[CropIndex].CropCoefficientDataArrayName);
        end;
        RequiredValues.LandUseStaticFileNames := DataArrayNames;
        WriteLandUseArrayData(AFileName, RequiredValues);
      finally
        DataArrayNames.Free;
      end;
    end;
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TModflowFmp4Writer.WriteDirectRecharge;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FClimatePackage.Direct_Recharge.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  AFileName := GetFileStreamName(wlDirectRecharge);

  if (FClimatePackage.Direct_Recharge.ArrayList = alArray) then
  begin
    RequiredValues.WriteLocation := wlDirectRecharge;
    RequiredValues.DefaultValue := 0;
    RequiredValues.DataType := rdtDouble;
    RequiredValues.DataTypeIndex := 0;
    RequiredValues.Comment := 'FMP CLIMATE: DIRECT_RECHARGE';
    RequiredValues.ErrorID := 'FMP CLIMATE: DIRECT_RECHARGE';
    RequiredValues.ID := 'DIRECT_RECHARGE';
    RequiredValues.StaticDataName := KDirectRecharge;
    RequiredValues.WriteTransientData :=
      (FClimatePackage.Direct_Recharge.FarmOption = foTransient);
    RequiredValues.CheckProcedure := nil;
    RequiredValues.CheckError :=  'Invalid Direct Recharge value';
    case FClimatePackage.DirectRechargeOption of
      droFlux: RequiredValues.Option := 'FLUX';
      droRate: RequiredValues.Option := 'RATE';
    end;
    RequiredValues.FarmProperty := FClimatePackage.Direct_Recharge;

    WriteFmpArrayData(AFileName, RequiredValues);
  end
  else
  begin
    Assert(False);
  end;
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
    RequiredValues.Option := '';
    RequiredValues.FarmProperty := FFarmProcess4.EfficiencyOptions;

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
    RequiredValues.Option := '';
    RequiredValues.FarmProperty := FFarmProcess4.EfficiencyImprovement;

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

  if FLandUse.LandUseOption = luoSingle then
  begin
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
    RequiredValues.Option := '';
    RequiredValues.FarmProperty := nil;

    WriteFmpArrayData(AFileName, RequiredValues);
  end;
end;

procedure TModflowFmp4Writer.WriteNrdInfilLocation;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FSurfaceWater4.Nrd_Infiltration_Location.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  AFileName := GetFileStreamName(wlNrdInfilLoc);

  if (FSurfaceWater4.Nrd_Infiltration_Location.ArrayList = alArray) then
  begin
    RequiredValues.WriteLocation := wlNrdInfilLoc;
    RequiredValues.DefaultValue := 0;
    RequiredValues.DataType := rdtInteger;
    RequiredValues.DataTypeIndex := 0;
    RequiredValues.Comment := 'FMP SURFACE_WATER: NRD_INFILTRATION_LOCATION';
    RequiredValues.ErrorID := 'FMP SURFACE_WATER: NRD_INFILTRATION_LOCATION';
    RequiredValues.ID := 'NRD_INFILTRATION_LOCATION';
    RequiredValues.StaticDataName := KNRD_Infiltration_Location;
    RequiredValues.WriteTransientData :=
      (FSurfaceWater4.Nrd_Infiltration_Location.FarmOption = foTransient);
    RequiredValues.CheckError :=  'Invalid Non-Routed Delivery Infiltration Location value';
    RequiredValues.CheckProcedure := CheckDataSetZeroOrGETen;
    RequiredValues.Option := '';
    RequiredValues.FarmProperty := FSurfaceWater4.Nrd_Infiltration_Location;

    WriteFmpArrayData(AFileName, RequiredValues);
  end
  else
  begin
    Assert(False);
  end;
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
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := nil;

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
  WriteInteger(Model.FmpCrops.Count);
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

    WriteLandUseOption;

    WriteLandUseLocation;
    WriteLandUseAreaFraction;
    WriteCropCoefficient;

    // remove this.
    WriteString('  ROOT_DEPTH STATIC CONSTANT 0.00001');
    NewLine;

    WriteString('END LAND_USE');
    NewLine;
    NewLine;
  end;
end;

procedure TModflowFmp4Writer.WriteLandUseAreaFraction;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
  DataArrayNames: TStringList;
  CropIndex: Integer;
begin
  if FLandUse.LandUseFraction.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  AFileName := GetFileStreamName(wlLandUseAreaFraction);

  if (FLandUse.LandUseFraction.ArrayList = alArray) then
  begin
    RequiredValues.WriteLocation := wlLandUseAreaFraction;
    RequiredValues.DefaultValue := 0;
    RequiredValues.DataType := rdtDouble;
    RequiredValues.DataTypeIndex := 0;
    RequiredValues.Comment := 'FMP LAND_USE: LAND_USE_AREA_FRACTION';
    RequiredValues.ErrorID := 'FMP LAND_USE: LAND_USE_AREA_FRACTION';
    RequiredValues.ID := 'LAND_USE_AREA_FRACTION';
    RequiredValues.StaticDataName := KLandUseAreaFraction;
    RequiredValues.WriteTransientData :=
      (FLandUse.LandUseFraction.FarmOption = foTransient);
    RequiredValues.CheckError :=  'Invalid Land Use Area Fraction value';
    RequiredValues.CheckProcedure := CheckDataSetBetweenZeroAndOne;
    RequiredValues.Option := '';
    RequiredValues.FarmProperty := FLandUse.LandUseFraction;
    if FLandUse.LandUseOption = luoSingle then
    begin
      WriteFmpArrayData(AFileName, RequiredValues);
    end
    else
    begin
      DataArrayNames := TStringList.Create;
      try
        for CropIndex := 0 to Model.FmpCrops.Count - 1 do
        begin
          DataArrayNames.Add(
            Model.FmpCrops[CropIndex].LandUseAreaFractionDataArrayName);
        end;
        RequiredValues.LandUseStaticFileNames := DataArrayNames;
        WriteLandUseArrayData(AFileName, RequiredValues);
      finally
        DataArrayNames.Free;
      end;
    end;
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TModflowFmp4Writer.WriteLandUseArrayData(AFileName: string;
  RequiredValues: TRequiredValues);
var
  UnitConversionScaleFactor: string;
  ExternalFileName: string;
  ExternalScaleFileName: string;
  DataSetList: TList;
  IsConstant: Boolean;
  index: Integer;
  DataArray: TDataArray;
  ConstantValue: double;
begin
  GetScaleFactorsAndExternalFile(RequiredValues, UnitConversionScaleFactor,
    ExternalFileName, ExternalScaleFileName);

  if RequiredValues.WriteTransientData then
  begin
    Assert(False)
  end
  else
  begin
    WriteScaleFactorsAndID(RequiredValues,
      UnitConversionScaleFactor, ExternalScaleFileName);

    Assert(RequiredValues.Option = '');

    if ExternalFileName <> '' then
    begin
      WriteString('STATIC ARRAY DATAFILE ');
      WriteString(ExternalFileName);
      NewLine;
    end
    else
    begin
      Assert(RequiredValues.LandUseStaticFileNames <> nil);
      DataSetList := TList.Create;
      try
        IsConstant := True;
        ConstantValue := 0.0;
        for index := 0 to RequiredValues.LandUseStaticFileNames.Count - 1 do
        begin
          DataArray := Model.DataArrayManager.GetDataSetByName(
            RequiredValues.LandUseStaticFileNames[index]);
          DataSetList.Add(DataArray);
          Assert(DataArray <> nil);
          DataArray.Initialize;
          if DataArray.IsUniform <> iuTrue then
          begin
            IsConstant := False;
          end;
          if IsConstant then
          begin
            if (index = 0) then
            begin
              ConstantValue := DataArray.RealData[0,0,0];
            end
            else
            begin
              IsConstant := ConstantValue = DataArray.RealData[0,0,0];
            end;
          end;
        end;

        if IsConstant then
        begin
          WriteString('STATIC CONSTANT ');
          WriteFloat(ConstantValue);
          NewLine;
        end
        else
        begin
          WriteString('STATIC ARRAY DATAFILE ');
          WriteString(ExtractFileName(AFileName));
          NewLine;
          FWriteLocation := RequiredValues.WriteLocation;
          try
            for Index := 0 to DataSetList.Count -1 do
            begin
              DataArray := DataSetList[Index];
              WriteArray(DataArray, 0, RequiredValues.ErrorID, '',
                RequiredValues.ID, False, False, True);
              if Assigned(RequiredValues.CheckProcedure) then
              begin
                RequiredValues.CheckProcedure(DataArray, RequiredValues.CheckError);
              end;
            end;
          finally
            FWriteLocation := wlMain;
          end;
        end;
      finally
        DataSetList.Free;
      end;
    end;
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
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := FClimatePackage.Precipitation;

  WriteFmpArrayData(AFileName, RequiredValues);
end;

procedure TModflowFmp4Writer.WritePrecipPotConsumption;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FClimatePackage.Precipitation_Potential_Consumption.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  AFileName := GetFileStreamName(wlPrecipPotConsumption);

  if (FClimatePackage.Precipitation_Potential_Consumption.ArrayList = alArray) then
  begin
    RequiredValues.WriteLocation := wlPrecipPotConsumption;
    RequiredValues.DefaultValue := 0;
    RequiredValues.DataType := rdtDouble;
    RequiredValues.DataTypeIndex := 0;
    RequiredValues.Comment := 'FMP CLIMATE: PRECIPITATION_POTENTIAL_CONSUMPTION';
    RequiredValues.ErrorID := 'FMP CLIMATE: PRECIPITATION_POTENTIAL_CONSUMPTION';
    RequiredValues.ID := 'PRECIPITATION_POTENTIAL_CONSUMPTION';
    RequiredValues.StaticDataName := KPrecipPotConsumption;
    RequiredValues.WriteTransientData :=
      (FClimatePackage.Precipitation_Potential_Consumption.FarmOption = foTransient);
    RequiredValues.CheckError :=  'Invalid Precipitation Potential Consumption value';
    case FClimatePackage.PrecipPotConsum of
      ppcLength:
      begin
        RequiredValues.CheckProcedure := nil;
        RequiredValues.Option := 'BY_LENGTH';
      end;
      ppcFraction:
        begin
          RequiredValues.CheckProcedure := CheckDataSetBetweenZeroAndOne;
          RequiredValues.Option := 'BY_FRACTION';
        end;
    end;
    RequiredValues.FarmProperty := FClimatePackage.Precipitation_Potential_Consumption;

    WriteFmpArrayData(AFileName, RequiredValues);
  end
  else
  begin
    Assert(False);
  end;
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
  RequiredValues.Option := '';
  RequiredValues.FarmProperty := nil;

  WriteFmpArrayData(AFileName, RequiredValues);
end;

procedure TModflowFmp4Writer.WriteSalinityFlush;
begin

end;

procedure TModflowFmp4Writer.WriteSoil;
begin
  if not FSoil4.IsSelected then
  begin
    Exit;
  end;
  WriteString('BEGIN SOIL');
  NewLine;

  WriteCapillaryFringe;
  WriteSoilID;
  WriteSurfaceK;

  WriteString('END SOIL');
  NewLine;
  NewLine;
end;

procedure TModflowFmp4Writer.WriteSoilID;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FSoil4.CapFringe.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  AFileName := GetFileStreamName(wlSoilID);

  if (FFarmProcess4.Bare_Runoff_Fraction.ArrayList = alArray) then
  begin
    RequiredValues.WriteLocation := wlSoilID;
    RequiredValues.DefaultValue := 0;
    RequiredValues.DataType := rdtDouble;
    RequiredValues.DataTypeIndex := 0;
    RequiredValues.Comment := 'FMP SOIL: SOIL_ID ';
    RequiredValues.ErrorID := 'FMP SOIL: SOIL_ID ';
    RequiredValues.ID := 'SOIL_ID ';
    RequiredValues.StaticDataName := KSoilID;
    RequiredValues.WriteTransientData := False;
    RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
    RequiredValues.CheckError := 'Invalid Soil ID value';
    RequiredValues.Option := '';
    RequiredValues.FarmProperty := FFarmProcess4.Bare_Runoff_Fraction;

    WriteFmpArrayData(AFileName, RequiredValues);
  end
  else
  begin

  end;
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
      wlCapillaryFringe:
        begin
          Assert(FCapillaryFringeFileStream <> nil);
          FCapillaryFringeFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlSoilID:
        begin
          Assert(FSurfaceKFileStream <> nil);
          FSurfaceKFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlSurfaceK:
        begin
          Assert(FSurfaceKFileStream <> nil);
          FSurfaceKFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlBareEvap:
        begin
          Assert(FEvapBareFileStream <> nil);
          FEvapBareFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlDirectRecharge:
        begin
          Assert(FDirectRechargeFileStream <> nil);
          FDirectRechargeFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlPrecipPotConsumption:
        begin
          Assert(FPrecipPotConsumptionFileStream <> nil);
          FPrecipPotConsumptionFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlNrdInfilLoc:
        begin
          Assert(FNrdInfilLocationFileStream <> nil);
          FNrdInfilLocationFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlLandUseAreaFraction:
        begin
          Assert(FLandUseAreaFractionFileStream <> nil);
          FLandUseAreaFractionFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlCropCoefficient:
        begin
          Assert(FCropcoefficientFileStream <> nil);
          FCropcoefficientFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      else
        Assert(False);
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteSupplyWell;
begin

end;

procedure TModflowFmp4Writer.WriteSurfaceK;
var
  AFileName: string;
  RequiredValues: TRequiredValues;
begin
  if FSoil4.SurfVertK.FarmOption = foNotUsed then
  begin
    Exit;
  end;

  AFileName := GetFileStreamName(wlSurfaceK);

  if (FSoil4.SurfVertK.ArrayList = alArray) then
  begin
    RequiredValues.WriteLocation := wlSurfaceK;
    RequiredValues.DefaultValue := 0;
    RequiredValues.DataType := rdtDouble;
    RequiredValues.DataTypeIndex := 0;
    RequiredValues.Comment := 'FMP SOIL: SURFACE_VERTICAL_HYDRAULIC_CONDUCTIVITY';
    RequiredValues.ErrorID := 'FMP SOIL: SURFACE_VERTICAL_HYDRAULIC_CONDUCTIVITY';
    RequiredValues.ID := 'SURFACE_VERTICAL_HYDRAULIC_CONDUCTIVITY';
    RequiredValues.StaticDataName := KSurfaceK;
    RequiredValues.WriteTransientData := False;
    RequiredValues.CheckProcedure := CheckDataSetZeroOrPositive;
    RequiredValues.CheckError := 'Invalid Surface Vertical K value';
    RequiredValues.Option := '';
    RequiredValues.FarmProperty := FSoil4.SurfVertK;

    WriteFmpArrayData(AFileName, RequiredValues);
  end
  else
  begin

  end;
end;

procedure TModflowFmp4Writer.WriteSurfaceWater;
begin
  if FSurfaceWater4.IsSelected then
  begin
    WriteString('BEGIN SURFACE_WATER');
    NewLine;

    WriteNrdInfilLocation;

    WriteString('END SURFACE_WATER');
    NewLine;
    NewLine;
  end;
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
