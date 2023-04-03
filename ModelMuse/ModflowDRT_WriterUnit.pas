unit ModflowDRT_WriterUnit;

interface

uses SysUtils, Classes, CustomModflowWriterUnit, ModflowDrtUnit,
  PhastModelUnit, ScreenObjectUnit, ModflowBoundaryUnit, ModflowCellUnit,
  ModflowPackageSelectionUnit, GoPhastTypes;

type
  TModflowDRT_Writer = class(TCustomListWriter)
  private
    NPDRT: integer;
    MXL: integer;
    FCells: array of array of TDrt_Cell;
    MXADRT: integer;
//    FPestParamUsed: Boolean;
    procedure WriteDataSet1;
    procedure WriteDataSets2And3;
    procedure WriteDataSets4To6;
    procedure InitializeCells;
    procedure WriteFileInternal;
  protected
    function CellType: TValueCellType; override;
    class function Extension: string; override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowBoundary;
      override;
    procedure GetOption(var Option: string); override;
    function Package: TModflowPackageSelection; override;
    function ParameterType: TParameterType; override;
    procedure WriteCell(Cell: TValueCell;
      const DataSetIdentifier, VariableIdentifiers: string); override;
    procedure WriteParameterCells(CellList: TValueCellList; NLST: Integer;
      const VariableIdentifiers, DataSetIdentifier: string;
      AssignmentMethod: TUpdateMethod; MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection); override;
    procedure CheckCell(ValueCell: TValueCell; const PackageName: string); override;
    procedure DoBeforeWriteCells; override;
    procedure Evaluate; override;
  public
    procedure WriteFile(const AFileName: string);
  end;


implementation

uses frmErrorsAndWarningsUnit,
  ModflowUnitNumbers, frmProgressUnit, Forms,
  DataSetUnit, FastGEO;

resourcestring
  StrWritingDRNPackage = 'Writing DRN Package input.';
//  StrWritingDataSet0 = '  Writing Data Set 0.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
//  StrWritingDataSets2and3 = '  Writing Data Sets 2 and 3.';
  StrWritingDataSets4to6 = '  Writing Data Sets 4 to 6.';
  StrDrainElevationDRT_IsB = 'Drain elevation in the DRT package is below '
  + 'the bottom of the cell at the following locations.';
  StrLargeDRTDrainElevDetailed = 'Large DRT drain elevation gradient between' +
  ' %0:s and %1:s. Gradient: %2:g';
  StrLargeDRTDrainElev = 'Large DRT drain elevation gradient';
  StrHighDRTDrainCondu = 'High DRT Drain conductance compared to the cell-to' +
  '-cell conductance may cause numerical difficulties';
  StrNegativeOrZeroDrainRtConductance = 'Transmissivity is negative or zero in cell containing a Drain Return boundary';
  StrNoDrainReturnCell = 'No Drain Return cells specified';
  StrBecauseNoDrainRet = 'Because no Drain Return cells have been specified,' +
  ' the DRT package will not be included in the model';

{ TModflowDRT_Writer }

function TModflowDRT_Writer.CellType: TValueCellType;
begin
  result := TDrt_Cell;
end;

procedure TModflowDRT_Writer.CheckCell(ValueCell: TValueCell;
  const PackageName: string);
const
  HighConductanceContrast = 1E6;
var
  Drt_Cell: TDrt_Cell;
  ActiveDataArray: TDataArray;
  ScreenObject: TScreenObject;
  CellBottomElevation: Real;
  OtherCell: TDrt_Cell;
  AqCond: Double;
  Ratio: Extended;
  Delta: double;
  procedure CheckGradient;
  var
    DeltaDrnElevation: double;
    WarningMessage: string;
//    OtherCellBottomElevation: Real;
//    DeltaCellElevation: Real;
    Cell1: string;
    Cell2: string;
    Point1: TPoint2D;
    Point2: TPoint2D;
    Gradient: Extended;
  begin
    if OtherCell <> nil then
    begin
      DeltaDrnElevation := Abs(Drt_Cell.Elevation - OtherCell.Elevation);
      Point1 := Model.Grid.TwoDElementCenter(Drt_Cell.Column, Drt_Cell.Row);
      Point2 := Model.Grid.TwoDElementCenter(OtherCell.Column, OtherCell.Row);
      Gradient := DeltaDrnElevation/Distance(Point1, Point2);
//      OtherCellBottomElevation := Model.Grid.CellElevation[
//        OtherCell.Column, OtherCell.Row, OtherCell.Layer+1];
//      DeltaCellElevation := Abs(OtherCellBottomElevation - CellBottomElevation);
      if Gradient > HighGradient then
      begin
        ScreenObject := Drt_Cell.ScreenObject as TScreenObject;
        Cell1 := Format(StrLayerRowColObject, [
          Drt_Cell.Layer+1, Drt_Cell.Row+1, Drt_Cell.Column+1, ScreenObject.Name]);
        ScreenObject := OtherCell.ScreenObject as TScreenObject;
        Cell2 := Format(StrLayerRowColObject, [
          OtherCell.Layer+1, OtherCell.Row+1, OtherCell.Column+1, ScreenObject.Name]);
        WarningMessage := Format(StrLargeDRTDrainElevDetailed,
          [Cell1, Cell2, Gradient]);
        frmErrorsAndWarnings.AddWarning(Model, StrLargeDRTDrainElev,
          WarningMessage, ScreenObject);
      end;
    end;
  end;
begin
  inherited;
  Drt_Cell := ValueCell as TDrt_Cell;
  if Length(FCells) = 0 then
  begin
    InitializeCells;
  end;
  FCells[Drt_Cell.Row, Drt_Cell.Column] := Drt_Cell;
  ActiveDataArray := Model.DataArrayManager.GetDataSetByName(rsActive);
  Assert(ActiveDataArray <> nil);
  CellBottomElevation := Model.DiscretiztionElevation[
    Drt_Cell.Column, Drt_Cell.Row, Drt_Cell.Layer+1];
  if ActiveDataArray.BooleanData[Drt_Cell.Layer, Drt_Cell.Row, Drt_Cell.Column]
    then
  begin
    if (Drt_Cell.Elevation < CellBottomElevation) then
    begin
      ScreenObject := Drt_Cell.ScreenObject as TScreenObject;
      Delta := CellBottomElevation- Drt_Cell.Elevation;
      if Model.ModelSelection = msModflowNWT then
      begin
        frmErrorsAndWarnings.AddError(Model, StrDrainElevationDRT_IsB,
          Format(StrLayerRowColObjectAmount, [
          Drt_Cell.Layer+1, Drt_Cell.Row+1, Drt_Cell.Column+1, ScreenObject.Name, Delta]),
          ScreenObject);
      end
      else
      begin
        frmErrorsAndWarnings.AddWarning(Model, StrDrainElevationDRT_IsB,
          Format(StrLayerRowColObjectAmount, [
          Drt_Cell.Layer+1, Drt_Cell.Row+1, Drt_Cell.Column+1, ScreenObject.Name, Delta]),
          ScreenObject);
      end;
    end;
    AqCond := AquiferConductance(Drt_Cell.Layer, Drt_Cell.Row, Drt_Cell.Column);
    if AqCond > 0 then
    begin
      Ratio := Drt_Cell.Conductance/AqCond;
      if Ratio > HighConductanceContrast then
      begin
        ScreenObject := Drt_Cell.ScreenObject as TScreenObject;
        frmErrorsAndWarnings.AddWarning(Model,StrHighDRTDrainCondu,
          Format(StrLayerRowColObjectamount, [
          Drt_Cell.Layer+1, Drt_Cell.Row+1, Drt_Cell.Column+1, ScreenObject.Name, Ratio]),
          ScreenObject);
      end;
    end
    else
    begin
      ScreenObject := Drt_Cell.ScreenObject as TScreenObject;
      frmErrorsAndWarnings.AddWarning(Model,StrNegativeOrZeroDrainRtConductance,
        Format(StrLayerRowColObject, [
        Drt_Cell.Layer+1, Drt_Cell.Row+1, Drt_Cell.Column+1, ScreenObject.Name]),
        ScreenObject);
    end;
  end;
  if not Model.DisvUsed then
  begin
    if Drt_Cell.Row > 0 then
    begin
      OtherCell := FCells[Drt_Cell.Row-1,Drt_Cell.Column];
      CheckGradient;
    end;
    if Drt_Cell.Column > 0 then
    begin
      OtherCell := FCells[Drt_Cell.Row,Drt_Cell.Column-1];
      CheckGradient;
    end;
    if Drt_Cell.Row < Model.Grid.RowCount-1 then
    begin
      OtherCell := FCells[Drt_Cell.Row+1,Drt_Cell.Column];
      CheckGradient;
    end;
    if Drt_Cell.Column < Model.Grid.ColumnCount-1 then
    begin
      OtherCell := FCells[Drt_Cell.Row,Drt_Cell.Column+1];
      CheckGradient;
    end;
  end;
end;

procedure TModflowDRT_Writer.DoBeforeWriteCells;
begin
  inherited;
  InitializeCells;
end;

procedure TModflowDRT_Writer.Evaluate;
begin
    inherited;
end;

class function TModflowDRT_Writer.Extension: string;
begin
  result := '.drt';
end;

function TModflowDRT_Writer.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.ModflowDrtBoundary;
end;

procedure TModflowDRT_Writer.GetOption(var Option: string);
begin
  inherited;
  Option := Option + ' RETURNFLOW';
end;

function TModflowDRT_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.DrtPackage;
end;

function TModflowDRT_Writer.ParameterType: TParameterType;
begin
  result := ptDRT;
end;

procedure TModflowDRT_Writer.WriteCell(Cell: TValueCell; const DataSetIdentifier,
  VariableIdentifiers: string);
var
  Drt_Cell: TDrt_Cell;
  LocalLayer: integer;
  Limit: Integer;
//  DataArray: TDataArray;
begin
  Drt_Cell := Cell as TDrt_Cell;
  LocalLayer := Model.
    DataSetLayerToModflowLayer(Drt_Cell.Layer);
  WriteInteger(LocalLayer);
  WriteInteger(Drt_Cell.Row+1);
  WriteInteger(Drt_Cell.Column+1);

  if (Drt_Cell.ElevationPest <> '')
    or (Drt_Cell.ConductancePest <> '')
    or (Drt_Cell.ReturnFractionPest <> '')
    or (Drt_Cell.ElevationPestSeries <> '')
    or (Drt_Cell.ConductancePestSeries <> '')
    or (Drt_Cell.ReturnFractionPestSeries <> '') then
  begin
    FPestParamUsed := True;
  end;

  WriteValueOrFormula(Drt_Cell, DrtElevationPosition);

  WriteValueOrFormula(Drt_Cell, DrtConductancePosition);

//  WriteFloat(Drt_Cell.Elevation);
//  WriteFloat(Drt_Cell.Conductance);
  WriteInteger(Drt_Cell.ReturnCell.Layer);
  if Model.ModelSelection in [msModflowFmp
          {$IFDEF OWHMV2}
          , msModflowOwhm2
          {$ENDIF}
  ] then
  begin
    Limit := -2;
  end
  else
  begin
    Limit := 1;
  end;

  if Drt_Cell.ReturnCell.Layer >= Limit then
  begin
    WriteInteger(Drt_Cell.ReturnCell.Row);
    WriteInteger(Drt_Cell.ReturnCell.Column);

    WriteValueOrFormula(Drt_Cell, DrtReturnPosition);

//    WriteFloat(Drt_Cell.ReturnFraction);
  end
  else
  begin
    WriteInteger(0);
    WriteInteger(0);
    WriteFloat(0.0);
  end;
  WriteIface(Drt_Cell.IFace);
  WriteString(' # ' + DataSetIdentifier + ' Layer Row Column Elevation '
    + VariableIdentifiers + ' LayR');
  WriteString(' RowR ColR Rfprop IFACE');
  NewLine;
end;

procedure TModflowDRT_Writer.WriteDataSet1;
var
  Option: String;
  IDRTCB: Integer;
begin
  CountParametersAndParameterCells(NPDRT, MXL);

  CountCells(MXADRT);
  GetFlowUnitNumber(IDRTCB);
  GetOption(Option);

  WriteInteger(MXADRT);
  WriteInteger(IDRTCB);
  WriteInteger(NPDRT);
  WriteInteger(MXL);
  WriteString(Option);
  WriteString(' # DataSet 1: MXADRT IDRTCB NPDRT MXL');
  if Option <> '' then
  begin
    WriteString(' Option');
  end;
  NewLine
end;

procedure TModflowDRT_Writer.WriteDataSets2And3;
const
//  ErrorRoot = 'One or more %s parameters have been eliminated '
//    + 'because there are no cells associated with them.';
  DS2 = ' # Data Set 2: PARNAM PARTYP Parval NLST';
  DS2Instances = ' INSTANCES NUMINST';
  DS3A = ' # Data Set 3a: INSTNAM';
  DataSetIdentifier = 'Data Set 3b:';
  VariableIdentifiers = 'Condfact IFACE';
begin
  WriteParameterDefinitions(DS2, DS2Instances, DS3A, DataSetIdentifier,
    VariableIdentifiers, StrOneOrMoreSParam, umAssign, nil, nil);
end;

procedure TModflowDRT_Writer.WriteDataSets4To6;
const
  D6PName =      ' # Data Set 6: PARNAM';
  D6PNameIname = ' # Data Set 6: PARNAM Iname';
  DS4 = ' # Data Set 4: ITMP NP';
  DataSetIdentifier = 'Data Set 4:';
  VariableIdentifiers = 'Cond ';
begin
  WriteStressPeriods(VariableIdentifiers, DataSetIdentifier, DS4,
    D6PNameIname, D6PName);
end;

procedure TModflowDRT_Writer.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrDrainElevationDRT_IsB);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrDrainElevationDRT_IsB);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrLargeDRTDrainElev);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrHighDRTDrainCondu);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNegativeOrZeroDrainRtConductance);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNoDrainReturnCell);

    if not Model.ModflowPackages.DrtPackage.IsSelected then
    begin
      Exit
    end;
    if Model.PackageGeneratedExternally(StrDRT) then
    begin
      Exit;
    end;
    if Model.ModelSelection = msModflow2015 then
    begin
      Exit;
    end;
    FPestParamUsed := False;
    NameOfFile := FileName(AFileName);
    FInputFileName := NameOfFile;
    Evaluate;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
//      Exit;
    end;
    ClearTimeLists(Model);
    FNameOfFile := FileName(AFileName);

    WriteFileInternal;

    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    if Model.PestUsed and FPestParamUsed then
    begin
      FNameOfFile := FNameOfFile + '.tpl';
      WritePestTemplateLine(FNameOfFile);
      WritingTemplate := True;
      WriteFileInternal;
    end;

  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowDRT_Writer.WriteFileInternal;
begin
  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingDRNPackage);
    frmProgressMM.AddMessage(StrWritingDataSet0);

    WriteTemplateHeader;

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

    if MXADRT = 0 then
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrNoDrainReturnCell,
        StrBecauseNoDrainRet);
    end;

    if not WritingTemplate then
    begin
      WriteToNameFile(StrDRT, Model.UnitNumbers.UnitNumber(StrDRT), FNameOfFile,
        foInput, Model);
    end;

    frmProgressMM.AddMessage(StrWritingDataSets2and3);
    WriteDataSets2And3;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    frmProgressMM.AddMessage(StrWritingDataSets4to6);
    WriteDataSets4To6;
  finally
    CloseFile;
  end;
end;

procedure TModflowDRT_Writer.InitializeCells;
var
  RowIndex: Integer;
  ColIndex: Integer;
  RowCount: Integer;
  ColumnCount: Integer;
begin
  if Model.Grid <> nil then
  begin
    RowCount := Model.Grid.RowCount;
    ColumnCount := Model.Grid.ColumnCount;
  end
  else
  begin
    RowCount := Model.DisvGrid.RowCount;
    ColumnCount := Model.DisvGrid.ColumnCount;
  end;
  SetLength(FCells, RowCount, ColumnCount);
  for RowIndex := 0 to RowCount - 1 do
  begin
    for ColIndex := 0 to ColumnCount - 1 do
    begin
      FCells[RowIndex, ColIndex] := nil;
    end;
  end;
end;

procedure TModflowDRT_Writer.WriteParameterCells(CellList: TValueCellList;
  NLST: Integer; const VariableIdentifiers, DataSetIdentifier: string;
  AssignmentMethod: TUpdateMethod; MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection);
var
  Cell: TDrt_Cell;
  CellIndex: Integer;
begin
  // Data set 3b
  InitializeCells;
  for CellIndex := 0 to CellList.Count - 1 do
  begin
    Cell := CellList[CellIndex] as TDrt_Cell;
    WriteCell(Cell, DataSetIdentifier, VariableIdentifiers);
    CheckCell(Cell, 'DRT');
  end;
  // Dummy inactive cells to fill out data set 3b.
  // Each instance of a parameter is required to have the same
  // number of cells.  This introduces dummy boundaries to fill
  // out the list.  because Condfact is set equal to zero, the
  // dummy boundaries have no effect.
  for CellIndex := CellList.Count to NLST - 1 do
  begin
    WriteInteger(1);
    WriteInteger(1);
    WriteInteger(1);
    WriteFloat(0);
    WriteFloat(0);
    WriteInteger(0);
    WriteInteger(0);
    WriteInteger(0);
    WriteInteger(0);
    WriteFloat(0);
    WriteString(
      ' # Data Set 3b: Layer Row Column Stage Condfact LayR  RowR ColR Rfprop IFACE (Dummy boundary)');
    NewLine;
  end;
end;

end.
