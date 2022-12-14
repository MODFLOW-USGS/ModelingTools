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
    wlSemiRouteDeliv, wlSemiRouteReturn, wlCall);

  TModflowFmp4Writer = class(TCustomListWriter)
  private
    FFarmProcess4: TFarmProcess4;
    FWriteLocation: TWriteLocation;
    FFarmIDs: TList;
    FFID_FileStream: TFileStream;
    FACtiveSurfaceCells: array of array of boolean;
    FFarmWellID: Integer;
    FOpenCloseFileStream: TFileStream;
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
    procedure EvaluateFarmID;
    procedure EvaluateAll;
    procedure FreeFileStreams;
    procedure WriteFarmLocation;
    procedure WriteDataSet26(TimeIndex: Integer);
    procedure CheckIntegerDataSet(IntegerArray: TDataArray;
      const ErrorMessage: string);
    procedure EvaluateActiveCells;
    procedure RemoveErrorAndWarningMessages;
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
    procedure UpdateFarmIDDisplay(TimeLists: TModflowBoundListOfTimeLists);
  end;


implementation

uses
  frmErrorsAndWarningsUnit, ModflowFmpWriterUnit,
  ModflowFmpFarmIdUnit, ModflowUnitNumbers, frmProgressUnit;

{ TModflowFmp4Writer }

function TModflowFmp4Writer.CellType: TValueCellType;
begin
  result := TFmpWell_Cell;
end;

procedure TModflowFmp4Writer.CheckIntegerDataSet(IntegerArray: TDataArray;
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

end;

destructor TModflowFmp4Writer.Destroy;
begin
  FreeFileStreams;
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
end;

procedure TModflowFmp4Writer.EvaluateFarmID;
var
  EmptyParamList: TStringList;
  ScreenObject: TScreenObject;
  ScreenObjectIndex: Integer;
  Boundary: TFmpFarmIDBoundary;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidFarmID);
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
      Boundary := ScreenObject.ModflowFmpFarmID;
      if Boundary <> nil then
      begin
        Boundary.GetCellValues(FFarmIDs, EmptyParamList, Model, self);
      end;
    end;
  finally
    EmptyParamList.Free;
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

class function TModflowFmp4Writer.Extension: string;
begin
  Result := '.fmp';
end;

procedure TModflowFmp4Writer.FreeFileStreams;
begin
 FreeAndNil(FFID_FileStream);
end;

function TModflowFmp4Writer.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.ModflowFmpWellBoundary;
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

procedure TModflowFmp4Writer.UpdateFarmIDDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  DataSets: TList;
  FarmID: TModflowBoundaryDisplayTimeList;
  TimeIndex: integer;
  TimeListIndex: integer;
  List: TValueCellList;
  TimeList: TModflowBoundaryDisplayTimeList;
  DataArray: TDataArray;
  FarmIDIndex: integer;
  DataSetIndex: integer;
begin
  EvaluateActiveCells;
  frmErrorsAndWarnings.BeginUpdate;
  try
    RemoveErrorAndWarningMessages;
    if not (Package as TFarmProcess4).FarmIdUsed(self) then
    begin
      UpdateNotUsedDisplay(TimeLists);
      Exit;
    end;
    DataSets := TList.Create;
    try
      EvaluateFarmID;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      FarmID := TimeLists[0];
      for TimeIndex := 0 to FarmID.Count - 1 do
      begin
        DataSets.Clear;

        for TimeListIndex := 0 to TimeLists.Count - 1 do
        begin
          TimeList := TimeLists[TimeListIndex];
          DataArray := TimeList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataSets.Add(DataArray);
        end;

        for FarmIDIndex := 0 to FFarmIDs.Count - 1 do
        begin
          List := FFarmIDs[FarmIDIndex];
          UpdateCellDisplay(List, DataSets, [], nil, [0]);
          List.Cache;
        end;
        for DataSetIndex := 0 to DataSets.Count - 1 do
        begin
          DataArray := DataSets[DataSetIndex];
          DataArray.UpToDate := True;
          CheckIntegerDataSet(DataArray, StrInvalidFarmID);
          DataArray.CacheData;
        end;
      end;

      SetTimeListsUpToDate(TimeLists);
    finally
      DataSets.Free;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowFmp4Writer.WriteAllotments;
begin

end;

procedure TModflowFmp4Writer.WriteCell(Cell: TValueCell;
  const DataSetIdentifier, VariableIdentifiers: string);
begin
  Assert(False);
end;

procedure TModflowFmp4Writer.WriteClimate;
begin

end;

procedure TModflowFmp4Writer.WriteConstantU2DINT(const Comment: string;
  const Value: integer; ArrayType: TModflowArrayType;
  const MF6_ArrayName: string);
var
  OldLocation: TWriteLocation;
begin
  case FWriteLocation of
    wlMain: inherited;
    wlOpenClose, wlCID, wlFID:
      begin
        OldLocation := FWriteLocation;
        FWriteLocation := wlMain;
        try
          inherited;
        finally
          FWriteLocation := OldLocation
        end;
      end
    else
      Assert(False);
  end;
end;

procedure TModflowFmp4Writer.WriteDataSet26(TimeIndex: Integer);
var
  FarmIDList: TValueCellList;
  Comment: string;
  DataTypeIndex: Integer;
  DataType: TRbwDataType;
  DefaultValue: Double;
//  AFileName: string;
  FarmIdArray: TDataArray;
//  BaseName: string;
begin
  if FFarmIDs.Count <= TimeIndex then
  begin
    frmErrorsAndWarnings.AddError(Model, StrFMPFarmsNotDefine, IntToStr(TimeIndex+1));
    Exit;
  end;
  FarmIDList := FFarmIDs[TimeIndex];
  DefaultValue := 0;
  DataType := rdtInteger;
  DataTypeIndex := 0;
  Comment := 'Data Set 26: FID';

  FarmIdArray := nil;
  FWriteLocation := wlFID;
  try
    WriteCommentLine(Format('Stress Period %d', [TimeIndex+1]));
    WriteTransient2DArray(Comment, DataTypeIndex, DataType, DefaultValue,
      FarmIDList, umAssign, False, FarmIdArray, 'FID', False, False);
    CheckIntegerDataSet(FarmIdArray, StrInvalidFarmID);
  finally
    FWriteLocation := wlMain;
    FarmIdArray.Free;
  end;
end;

procedure TModflowFmp4Writer.WriteFarmLocation;
var
  StressPeriodIndex: Integer;
  BaseName: string;
  AFileName: string;
  DataArray: TDataArray;
begin
  BaseName := ChangeFileExt(FNameOfFile, '');
  AFileName := ChangeFileExt(BaseName, '.FID');
  if FFID_FileStream = nil then
  begin
//    if not WritingTemplate then
//    begin
////        WriteToNameFile(StrDATA, Model.UnitNumbers.UnitNumber(StrFmpFID),
////              AFileName, foInput, Model);
//    end;
    FFID_FileStream := TFileStream.Create(AFileName,
      fmCreate or fmShareDenyWrite);
  end;

  WriteString('  LOCATION ');

  if FFarmProcess4.Farms.FarmOption = foTransient then
  begin
    WriteString('TRANSIENT ARRAY DATAFILE ');
    WriteString(ExtractFileName(AFileName));
    NewLine;

    for StressPeriodIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
    begin
      WriteDataSet26(StressPeriodIndex);
      NewLine;
    end;
  end
  else
  begin
    WriteString('STATIC ARRAY DATAFILE ');
    WriteString(ExtractFileName(AFileName));
    NewLine;
    FWriteLocation := wlFID;
    try
      DataArray := Model.DataArrayManager.GetDataSetByName(KFarmID);
      WriteArray(DataArray, 0, 'WBS: LOCATION', '', 'LOCATION', False, False);
    finally
      FWriteLocation := wlMain;
    end;
  end;
end;

procedure TModflowFmp4Writer.WriteFile(const AFileName: string);
begin
  FFarmProcess4 := Package as TFarmProcess4;
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

end;

procedure TModflowFmp4Writer.WriteOutput;
begin

end;

procedure TModflowFmp4Writer.WriteParameterCells(CellList: TValueCellList;
  NLST: Integer; const VariableIdentifiers, DataSetIdentifier: string;
  AssignmentMethod: TUpdateMethod;
  MultiplierArrayNames: TTransientMultCollection;
  ZoneArrayNames: TTransientZoneCollection);
begin
  Assert(False);

end;

procedure TModflowFmp4Writer.WriteSalinityFlush;
begin

end;

procedure TModflowFmp4Writer.WriteSoil;
begin

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
//          Assert(FOFE_FileStream <> nil);
//          FOFE_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlCID:
        begin
//          Assert(FCID_FileStream <> nil);
//          FCID_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlFID:
        begin
          Assert(FFID_FileStream <> nil);
          FFID_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlRoot:
        begin
//          Assert(FRoot_FileStream <> nil);
//          FRoot_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlCropUse:
        begin
//          Assert(FCropUse_FileStream <> nil);
//          FCropUse_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlETR:
        begin
//          Assert(FETR_FileStream <> nil);
//          FETR_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlEtFrac:
        begin
//          Assert(FET_Frac_FileStream <> nil);
//          FET_Frac_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlSwLosses:
        begin
//          Assert(FSW_Losses_FileStream <> nil);
//          FSW_Losses_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlPFLX:
        begin
//          Assert(FPFLX_FileStream <> nil);
//          FPFLX_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlCropFunc:
        begin
//          Assert(FCropFunc_FileStream <> nil);
//          FCropFunc_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlWaterCost:
        begin
//          Assert(FWaterCost_FileStream <> nil);
//          FWaterCost_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlDeliveries:
        begin
//          Assert(FDeliveries_FileStream <> nil);
//          FDeliveries_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlSemiRouteDeliv:
        begin
//          Assert(FSemiDeliveries_FileStream <> nil);
//          FSemiDeliveries_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlSemiRouteReturn:
        begin
//          Assert(FSemiReturn_FileStream <> nil);
//          FSemiReturn_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
        end;
      wlCall:
        begin
//          Assert(FCall_FileStream <> nil);
//          FCall_FileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
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
  NewLine;

  WriteFarmLocation;

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

  WriteString('END WATER_BALANCE_SUBREGION');
  NewLine;
end;

end.
