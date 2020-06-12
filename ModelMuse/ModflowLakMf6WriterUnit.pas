unit ModflowLakMf6WriterUnit;

interface

uses SysUtils, Classes, PhastModelUnit, CustomModflowWriterUnit,
  ModflowPackageSelectionUnit, ModflowTimeUnit, ModflowLakUnit,
  ScreenObjectUnit, System.Generics.Collections, ModflowCellUnit,
  ModflowLakMf6Unit;

type
  TLakObservation = record
    FName: string;
    FBoundName: string;
    FObsTypes: TLakObs;
  end;
  TLakObservationList = TList<TLakObservation>;

  TLakeType = (ltVert, ltHoriz, ltVertEmbed, ltHorizEmbed);

  TLakeCell = class(TObject)
    Cell: TCellLocation;
    LakeCell: TCellLocation;
    LakeType: TLakeType;
    BedLeakance: double;
    BedLeakanceAnnotation: String;
    BedThickness: double;
    BedThicknessAnnotation: String;
    BotElev: double;
    BotElevAnnotation: String;
    TopElev: double;
    TopElevAnnotation: String;
    ConnLength: double;
    ConnLengthAnnotation: String;
    ConnWidth: double;
    ConnWidthAnnotation: String;
  end;

  TLakeCellList = TObjectList<TLakeCell>;

  TLakeTableRow = class(TObject)
    // stage
    Stage: double;
    // volume
    Volume: double;
    // sarea
    SurfaceArea: double;
    // barea
    ExchangeArea: double;
  end;

  TLakeTable = class(TObjectList<TLakeTableRow>)
    FFileName: string;
  end;

  TOutletSetting = class(TObject)
    StartTime: double;
    EndTime: double;
    Rate: double;
    Invert: double;
    Width: double;
    Slope: double;
    Rough: double;
  end;

  TLakeOutletMf6 = class(TObjectList<TOutletSetting>)
    OutletType: TLakeOutletType;
    // @name starts at zero.
    LakeOutNumber: integer;
    FStartingTimeIndex: Integer;
  end;

  TLakeOutlets = TObjectList<TLakeOutletMf6>;

  TLakeSetting = class(TObject)
    StartTime: double;
    EndTime: double;
    Status: TLakeStatus;
    Stage: double;
    Rainfall: double;
    Evaporation: double;
    Runoff: double;
    Inflow: double;
    Withdrawal: double;
  end;

  TLakeSettings = TObjectList<TLakeSetting>;

  TLake = class (TObject)
  private
    // @name contains cells intersected or enclosed by the @link(FScreenObject).
    FCellList: TCellAssignmentList;
    FScreenObject: TScreenObject;
    // @name is the list of cells connected to the lake and the lake-related
    // properties of those cells.
    FLakeCellList: TLakeCellList;
    FLakeTable: TLakeTable;
    FLakeOutlets: TLakeOutlets;
    FLakeSettings: TLakeSettings;
    FStartingStage: double;
    FStartingTimeIndex: Integer;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TLakeList = TObjectList<TLake>;

  TModflowLAKMf6Writer = class(TCustomPackageWriter)
  private
    FLakeScreenObjects: TScreenObjectList;
    FLakMf6Package: TLakeMf6PackageSelection;
    FFileName: string;
    FLakes: TLakeList;
    noutlets: Integer;
    ntables: Integer;
    FObservations: TLakObservationList;
    FDirectObsLines: TStrings;
    FFileNameLines: TStrings;
    FCalculatedObsLines: TStrings;
    procedure Evaluate;
    procedure IdentifyLakesAndLakeCells;
    procedure SetLakeCellProperties;
    procedure SetLakeTables;
    procedure SetLakeOutlets;
    procedure SetLakeTimeProperties;
    function EvaluateFormula(Formula: string; const DataIdentifier,
      ScreenObjectName: string): double;
    procedure WriteOptions;
    procedure WriteDimensions;
    procedure WritePackageData;
    procedure WriteConnectionData;
    procedure WriteLakeTables;
    procedure WriteLakeTableNames;
    procedure WriteOutlets;
    procedure WritePeriods;
    function IsMf6Observation(AScreenObject: TScreenObject): Boolean; //override;
//    function IsMf6ToMvrObservation(AScreenObject: TScreenObject): Boolean;
    class function ObservationExtension: string; //override;
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
    procedure UpdateDisplay;
    property DirectObsLines: TStrings read FDirectObsLines write FDirectObsLines;
    property CalculatedObsLines: TStrings read FCalculatedObsLines write FCalculatedObsLines;
    property FileNameLines: TStrings read FFileNameLines write FFileNameLines;
  end;


implementation

uses
  ModflowOptionsUnit, frmProgressUnit, SparseDataSets,
  GoPhastTypes, SparseArrayUnit, frmErrorsAndWarningsUnit, DataSetUnit,
  ModflowIrregularMeshUnit, AbstractGridUnit, RealListUnit, RbwParser,
  frmFormulaErrorsUnit, System.Math, Modflow6ObsUnit, Modflow6ObsWriterUnit,
  FastGEO, ModflowBoundaryDisplayUnit, ModflowMvrWriterUnit, ModflowMvrUnit;

resourcestring
  StrTheFollowingObject = 'The following objects are supposed to define lake' +
  's that are connected to exactly one cell. However, the object is connected' +
  ' to either zero or more than one cell. The flag to treat the lake as ' +
  'embedded within a single cell will be ignored.';
  StrTheLakesDefinedBy = 'The lakes defined by the following objects are sup' +
  'posed to define lakes that are connected to exactly one cell. However, the' +
  ' object do not intersect any cells.';
  StrLakeStage = 'Lake Stage';
  StrLakeVolume = 'Lake Volume';
  StrLakeSurfaceArea = 'Lake Surface Area';
  StrLakeExchangeArea = 'Lake Exchange Area';
  StrLakeOutletIncorrec = 'Lake outlet incorrectly defined.';
  StrOutlet0dForThe = 'Outlet %0:d for the lake defined by the object %1:s, ' +
  'is incorrectly defined.';
  StrTheFollowingObjectNoCells = 'The following objects define lakes but are not co' +
  'nnected to any active cells.';
  StrWritingLAK6Package = 'Writing LAK6 Package input.';
  StrLakeBottomElevatio = 'Lake Bottom Elevation';
  StrLakeTopElevation = 'Lake Top Elevation';
  StrLakebedHydraulicCo = 'Lakebed Hydraulic Conductivity';
  StrLakebedThickness = 'Lakebed Thickness';
  StrLakeConnectionLeng = 'Lake Connection Length';
  StrLakeConnectionWidt = 'Lake Connection Width';
  StrInvalidLakeExtout = 'Invalid lake ext-outflow observation in lake witho' +
  'ut any outlets.';
  StrInvalidLakeOutflow = 'Invalid lake outflow observation in lake without ' +
  'any outlets.';
  StrSDefinesALakeEx = '%s defines a lake ext-outflow observation but has no' +
  ' outlets. The ext-outflow observation will be skipped.';
  StrSDefinesALakeOu = '%s defines a lake outflow observation but has no out' +
  'lets. The outflow observation will be skipped.';
  StrNoOutletLakeDefin = 'No outlet lake defined';
  StrInTheLakeDefined = 'In the lake defined by the object %0:s, no outlet l' +
  'ake is defined for outlet %1:d. Flow through this outlet will either exit ' +
  'the model or it can be transferred to another boundary via the MVR package.';
  StrLakeTopIsAboveTh = 'Lake top is above the top of the model in a horizontal' +
  'ly connected lake cell';
  StrTheLakeDefinedByGrid = 'The lake defined by %0:s has a higher top eleva' +
  'tion than the top of the model at Row %1:d, Column %2:d';
  StrTheLakeDefinedByDisv = 'The lake defined by %0:s has a higher top eleva' +
  'tion than the top of the model at Cell %1:d';
  StrDistanceBetweenCelRowCol = 'Distance between cells (%0:d, %1:d) and (%2' +
  ':d, %3:d) [Row, Column]';
  StrDistanceBetweenCelNum = 'Distance between cells %0:d and %1:d [Cell Num' +
  'ber]';
  StrConnectionWidthIs = 'Connection width is only specified for horizontal ' +
  'lake connections';
  StrWidthOfColumnD = 'Width of column %d';
  StrWidthOfRowD = 'Width of row %d';
  StrWidthOfCellFaceB = 'Width of cell face between cells %0:d, and %1:d';
  StrLakeOutletRateIn = 'Lake Outlet Rate in Outlet %0:d at %1:g';
  StrLakeOutletInvertI = 'Lake Outlet Invert in Outlet %0:d at %1:g';
  StrLakeOutletWidthIn = 'Lake Outlet Width in Outlet %0:d at %1:g';
  StrLakeOutletSlopeIn = 'Lake Outlet Slope in Outlet %0:d at %1:g';
  StrLakeOutletRoughnes = 'Lake Outlet Roughness in Outlet %0:d at %1:g';
  StrLakeStageAt0g = 'Lake Stage at %0:g';
  StrLakeRainfallAt0 = 'Lake Rainfall at %0:g';
  StrLakeEvaporationAt = 'Lake Evaporation at %0:g';
  StrLakeRunoffAt0g = 'Lake Runoff at %0:g';
  StrLakeWithdrawalAt = 'Lake Withdrawal at %0:g';
  StrLakeStartingStage = 'Lake Starting Stage';
  StrLakeInflowAt0g = 'Lake Inflow at %0:g';

{ TModflowLAKMf6Writer }

constructor TModflowLAKMf6Writer.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FLakeScreenObjects := TScreenObjectList.Create;
  FLakes := TLakeList.Create;
  FObservations := TLakObservationList.Create;
end;

destructor TModflowLAKMf6Writer.Destroy;
begin
  FObservations.Free;
  FLakes.Free;
  FLakeScreenObjects.Free;
  inherited;
end;

procedure TModflowLAKMf6Writer.Evaluate;
begin
  frmErrorsAndWarnings.RemoveWarningGroup(Model,StrTheFollowingObject);
  frmErrorsAndWarnings.RemoveWarningGroup(Model,StrTheLakesDefinedBy);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrLakeOutletIncorrec);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrInvalidLakeExtout);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrInvalidLakeOutflow);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheFollowingObjectNoCells);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNoOutletLakeDefin);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrLakeTopIsAboveTh);

  IdentifyLakesAndLakeCells;
  SetLakeCellProperties;
  SetLakeTables;
  SetLakeOutlets;
  SetLakeTimeProperties;
end;

class function TModflowLAKMf6Writer.Extension: string;
begin
  result := '.lak6';
end;

function TModflowLAKMf6Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.LakMf6Package;

end;

procedure TModflowLAKMf6Writer.WriteConnectionData;
var
  LakeIndex: Integer;
  ALake: TLake;
  CellIndex: Integer;
  ACell: TLakeCell;
  DisvUsed: Boolean;
begin
  DisvUsed := Model.DisvUsed;
  WriteBeginConnectionData;
  WriteString('# lakeno iconn cellid(ncelldim)   claktype    bedleak               belev                 telev                 connlen               connwidth');
  NewLine;
  for LakeIndex := 0 to FLakes.Count - 1 do
  begin
    ALake := FLakes[LakeIndex];

    for CellIndex := 0 to ALake.FLakeCellList.Count - 1 do
    begin
      ACell := ALake.FLakeCellList[CellIndex];
      WriteString('  ');
      WriteInteger(LakeIndex+1);
      WriteInteger(CellIndex+1);
      WriteInteger(ACell.Cell.Layer+1);
      if not DisvUsed then
      begin
        WriteInteger(ACell.Cell.Row+1);
      end;
      WriteInteger(ACell.Cell.Column+1);
      case ACell.LakeType of
        ltVert:
          begin
            WriteString('   VERTICAL');
          end;
        ltHoriz:
          begin
            WriteString(' HORIZONTAL');
          end;
        ltVertEmbed:
          begin
            WriteString('  EMBEDDEDV');
          end;
        ltHorizEmbed:
          begin
            WriteString('  EMBEDDEDH');
          end;
        else
          Assert(False);
      end;
      if ACell.BedThickness = 0 then
      begin
        WriteString(' NONE');
      end
      else
      begin
        WriteFloat(ACell.BedLeakance);
      end;
      WriteFloat(ACell.BotElev);
      WriteFloat(ACell.TopElev);
      WriteFloat(ACell.ConnLength);
      WriteFloat(ACell.ConnWidth);
      NewLine;

{  TLakeCell = class(TObject)
    Cell: TCellLocation;
    LakeType: TLakeType;
    BedLeakance: double;
    BotElev: double;
    TopElev: double;
    ConnLength: double;
    CondWidth: double;
  end;
}
    end;

  end;
  WriteEndConnectionData;
end;

procedure TModflowLAKMf6Writer.WriteDimensions;
var
  ObjectIndex: Integer;
  ALake: TLakeMf6;
begin
  WriteBeginDimensions;

  WriteString('  NLAKES ');
  WriteInteger(FLakeScreenObjects.Count);
  NewLine;

  noutlets := 0;
  ntables := 0;

  for ObjectIndex := 0 to FLakeScreenObjects.Count - 1 do
  begin
    ALake := FLakeScreenObjects[ObjectIndex].ModflowLak6;
    Inc(noutlets, ALake.Outlets.Count);
    if (ALake.LakeTable.Count > 0) then
    begin
      Inc(ntables);
    end;
  end;

  WriteString('  NOUTLETS ');
  WriteInteger(noutlets);
  NewLine;

  WriteString('  NTABLES ');
  WriteInteger(ntables);
  NewLine;

  WriteEndDimensions;
end;

procedure TModflowLAKMf6Writer.WriteFile(const AFileName: string);
var
  ObsWriter: TLakObsWriter;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally('LAK6') then
  begin
    Exit;
  end;
  if Model.ModelSelection <> msModflow2015 then
  begin
    Exit;
  end;

  FLakMf6Package := Package as TLakeMf6PackageSelection;

  Evaluate;

  FFileName := FileName(AFileName);
  WriteToNameFile('LAK6', 0, FFileName, foInput, Model);

  WriteLakeTables;

  OpenFile(FFileName);
  try
    frmProgressMM.AddMessage(StrWritingLAK6Package);
    frmProgressMM.AddMessage(StrWritingDataSet0);
    WriteDataSet0;

    WriteOptions;

    WriteDimensions;

    WritePackageData;

    WriteConnectionData;

    WriteLakeTableNames;

    WriteOutlets;

    WritePeriods;
  finally
    CloseFile;
  end;

  if FObservations.Count > 0 then
  begin
    ObsWriter := TLakObsWriter.Create(Model, etExport, FObservations);
    try
      ObsWriter.WriteFile(ChangeFileExt(FFileName, ObservationExtension));
    finally
      ObsWriter.Free;
    end;
  end;


end;

procedure TModflowLAKMf6Writer.WriteLakeTableNames;
var
  LakeIndex: Integer;
  ALake: TLake;
begin
  if ntables > 0 then
  begin
    WriteString('BEGIN TABLES');
    NewLine;

    for LakeIndex := 0 to FLakes.Count - 1 do
    begin
      ALake := FLakes[LakeIndex];
      if ALake.FLakeTable.FFileName <> '' then
      begin
        WriteString('  ');
        WriteInteger(LakeIndex+1);
        WriteString(' TAB6 FILEIN ');
        WriteString(ExtractFileName(ALake.FLakeTable.FFileName));
        NewLine;
      end;
    end;

    WriteString('END TABLES');
    NewLine;
    NewLine;
  end;
end;

procedure TModflowLAKMf6Writer.WriteLakeTables;
var
  LakeIndex: Integer;
  ALake: TLake;
  Extension: string;
  FormatString: string;
  ACell: TLakeCell;
  Embeded: Boolean;
  RowIndex: Integer;
  TableRow: TLakeTableRow;
begin
  for LakeIndex := 0 to FLakes.Count - 1 do
  begin
    ALake := FLakes[LakeIndex];
    if ALake.FLakeCellList.Count > 0 then
    begin
      if ALake.FLakeTable.Count > 0 then
      begin
        FormatString := '.%.' + Floor(Log10(FLakes.Count)+1).ToString + 'd.laktab';
        Extension := Format(FormatString, [LakeIndex+1]);
        ALake.FLakeTable.FFileName := ChangeFileExt(FFileName,Extension);
        OpenFile(ALake.FLakeTable.FFileName);
        try
          frmProgressMM.AddMessage('Writing LAK6 Table input.');

          WriteBeginDimensions;
          WriteString('  NROW');
          WriteInteger(ALake.FLakeTable.Count);
          NewLine;

          WriteString('  NCOL');
          ACell := ALake.FLakeCellList[0];
          Embeded := ACell.LakeType in [ltVertEmbed, ltHorizEmbed];
          if Embeded then
          begin
            WriteInteger(4);
          end
          else
          begin
            WriteInteger(3);
          end;
          NewLine;
          WriteEndDimensions;

          WriteString('BEGIN TABLE');
          NewLine;
          for RowIndex := 0 to ALake.FLakeTable.Count - 1 do
          begin
            TableRow := ALake.FLakeTable[RowIndex];
            WriteFloat(TableRow.Stage);
            WriteFloat(TableRow.Volume);
            WriteFloat(TableRow.SurfaceArea);
            if Embeded then
            begin
              WriteFloat(TableRow.ExchangeArea);
            end;
            NewLine;
          end;
          WriteString('END TABLE');
          NewLine;
          NewLine;
        finally
          CloseFile;
        end;
        Model.AddModelInputFile(ALake.FLakeTable.FFileName);
      end;
    end
    else
    begin
      frmErrorsAndWarnings.AddError(Model, StrTheFollowingObjectNoCells,
        ALake.FScreenObject.Name, ALake.FScreenObject);
    end;
  end;
end;

procedure TModflowLAKMf6Writer.SetLakeCellProperties;
var
  CellList: TCellLocationList;
  Results: TRealList;
  LakeIndex: Integer;
  ALake: TLake;
  CellIndex: Integer;
  ScreenObject: TScreenObject;
  LakeBoundary: TLakeMf6;
  BottomElevation: string;
  TopElevation: string;
  BedK: string;
  BedThickness: string;
  ConnectionLength: string;
//  ConnectionWidth: string;
  Grid: TCustomModelGrid;
  Disv: TModflowDisvGrid;
  LakeCell: TLakeCell;
  LakeCenter: TPoint2D;
  LakeConnectionCenter: TPoint2D;
//  LakeConnectionElevation: Real;
//  LakeCenterElevation: Real;
  DisvConnectionCell: TModflowIrregularCell2D;
  DisvLakeCell: TModflowIrregularCell2D;
  Annotation: string;
  BedKAnnotation: string;
  ThickZeroAnnotation: string;
  LakeDistanceAnnotation: string;
  ConnectionWidthAnnotation: string;
  NonHorizConnWidth: string;
  ACell: TLakeCell;
  CellTop: Real;
begin
  Grid := Model.Grid;
  Disv := Model.DisvGrid;
  CellList := TCellLocationList.Create;
  try
    Results := TRealList.Create;
    try
      for LakeIndex := 0 to FLakes.Count - 1 do
      begin
        ALake := FLakes[LakeIndex];
        CellList.Clear;
        for CellIndex := 0 to ALake.FLakeCellList.Count - 1 do
        begin
          CellList.Add(ALake.FLakeCellList[CellIndex].Cell);
        end;
        ScreenObject := ALake.FScreenObject;
        LakeBoundary := ScreenObject.ModflowLak6;

        BottomElevation := LakeBoundary.BottomElevation;
        ScreenObject.AssignValuesWithCellList(BottomElevation, Model, CellList,
          Results, Annotation, StrLakeBottomElevatio);
        Assert(ALake.FLakeCellList.Count = Results.Count);
        for CellIndex := 0 to ALake.FLakeCellList.Count - 1 do
        begin
          ALake.FLakeCellList[CellIndex].BotElev := Results[CellIndex];
          ALake.FLakeCellList[CellIndex].BotElevAnnotation := Annotation;
        end;

        TopElevation := LakeBoundary.TopElevation;
        ScreenObject.AssignValuesWithCellList(TopElevation, Model, CellList,
          Results, Annotation, StrLakeTopElevation);
        Assert(ALake.FLakeCellList.Count = Results.Count);
        for CellIndex := 0 to ALake.FLakeCellList.Count - 1 do
        begin
          ALake.FLakeCellList[CellIndex].TopElev := Results[CellIndex];
          ALake.FLakeCellList[CellIndex].TopElevAnnotation := Annotation;

          if ALake.FLakeCellList[CellIndex].LakeType = ltHoriz then
          begin
            ACell := ALake.FLakeCellList[CellIndex];
            if Grid <> nil then
            begin
              CellTop := Grid.CellElevation[ACell.Cell.Column, ACell.Cell.Row, 0]
            end
            else
            begin
              CellTop := Disv.LayerPosition(0, ACell.Cell.Column);
            end;
            if CellTop < ALake.FLakeCellList[CellIndex].TopElev then
            begin
              if Grid <> nil then
              begin
                frmErrorsAndWarnings.AddError(Model, StrLakeTopIsAboveTh,
                  Format(StrTheLakeDefinedByGrid, [ScreenObject.Name, ACell.Cell.Row+1, ACell.Cell.Column+1]), ScreenObject);
              end
              else
              begin
                frmErrorsAndWarnings.AddError(Model, StrLakeTopIsAboveTh,
                  Format(StrTheLakeDefinedByDisv, [ScreenObject.Name, ACell.Cell.Column+1]), ScreenObject);
              end;
            end;
          end;
        end;

        BedK := LakeBoundary.BedK;
        ScreenObject.AssignValuesWithCellList(BedK, Model, CellList, Results,
          Annotation, StrLakebedHydraulicCo);
        BedKAnnotation := Annotation;
        Assert(ALake.FLakeCellList.Count = Results.Count);
        for CellIndex := 0 to ALake.FLakeCellList.Count - 1 do
        begin
          ALake.FLakeCellList[CellIndex].BedLeakance := Results[CellIndex];
        end;

        BedThickness := LakeBoundary.BedThickness;
        ScreenObject.AssignValuesWithCellList(BedThickness, Model, CellList,
          Results, Annotation, StrLakebedThickness);
        BedKAnnotation := Format('(%0:s / %1:s)', [BedKAnnotation, Annotation]);
        ThickZeroAnnotation := Format('Leakance is zero because bedthickness is zero as set by %s', [Annotation]);
        Assert(ALake.FLakeCellList.Count = Results.Count);
        for CellIndex := 0 to ALake.FLakeCellList.Count - 1 do
        begin
          if Results[CellIndex] <> 0 then
          begin
            ALake.FLakeCellList[CellIndex].BedLeakance :=
              ALake.FLakeCellList[CellIndex].BedLeakance / Results[CellIndex];
            ALake.FLakeCellList[CellIndex].BedLeakanceAnnotation := BedKAnnotation;
          end
          else
          begin
            ALake.FLakeCellList[CellIndex].BedLeakance := 0;
            ALake.FLakeCellList[CellIndex].BedLeakanceAnnotation := ThickZeroAnnotation;
          end;
          ALake.FLakeCellList[CellIndex].BedThickness := Results[CellIndex];
        end;

        ConnectionLength := LakeBoundary.ConnectionLength;
        ScreenObject.AssignValuesWithCellList(ConnectionLength, Model,
          CellList, Results, Annotation, StrLakeConnectionLeng);
        Assert(ALake.FLakeCellList.Count = Results.Count);
        for CellIndex := 0 to ALake.FLakeCellList.Count - 1 do
        begin
          LakeCell := ALake.FLakeCellList[CellIndex];
          if LakeCell.LakeType = ltHoriz then
          begin
            if Grid <> nil then
            begin
              LakeConnectionCenter := Grid.TwoDElementCenter(LakeCell.Cell.Column, LakeCell.Cell.Row);
              LakeCenter := Grid.TwoDElementCenter(LakeCell.LakeCell.Column, LakeCell.LakeCell.Row);
              LakeDistanceAnnotation := Format(StrDistanceBetweenCelRowCol,
                [LakeCell.Cell.Row+1, LakeCell.Cell.Column+1, LakeCell.LakeCell.Row+1, LakeCell.LakeCell.Column+1]);
            end
            else
            begin
              LakeConnectionCenter := Disv.TwoDGrid.Cells[LakeCell.Cell.Column].Location;
              LakeCenter := Disv.TwoDGrid.Cells[LakeCell.LakeCell.Column].Location;
              LakeDistanceAnnotation := Format(StrDistanceBetweenCelNum,
                [LakeCell.Cell.Column+1, LakeCell.LakeCell.Column+1]);
            end;
            ALake.FLakeCellList[CellIndex].ConnLength := Distance(LakeConnectionCenter,LakeCenter);
            ALake.FLakeCellList[CellIndex].ConnLengthAnnotation := LakeDistanceAnnotation;
          end
          else
          begin
            ALake.FLakeCellList[CellIndex].ConnLength := Results[CellIndex];
            ALake.FLakeCellList[CellIndex].ConnLengthAnnotation := Annotation;
          end;
        end;

//        ConnectionWidth := LakeBoundary.ConnectionWidth;
//        ScreenObject.AssignValuesWithCellList(ConnectionWidth, Model, CellList,
//          Results, StrLakeConnectionWidt);
//        Assert(ALake.FLakeCellList.Count = Results.Count);
        NonHorizConnWidth := StrConnectionWidthIs;
        for CellIndex := 0 to ALake.FLakeCellList.Count - 1 do
        begin
          LakeCell := ALake.FLakeCellList[CellIndex];
          if LakeCell.LakeType = ltHoriz then
          begin
            if Grid <> nil then
            begin
              if LakeCell.Cell.Column = LakeCell.LakeCell.Column then
              begin
                ALake.FLakeCellList[CellIndex].ConnWidth := Grid.ColumnWidth[LakeCell.Cell.Column];
                ConnectionWidthAnnotation := Format(StrWidthOfColumnD, [LakeCell.Cell.Column+1]);
              end
              else
              begin
                ALake.FLakeCellList[CellIndex].ConnWidth := Grid.RowWidth[LakeCell.Cell.Row];
                ConnectionWidthAnnotation := Format(StrWidthOfRowD, [LakeCell.Cell.Row+1]);
              end;
            end
            else
            begin
              DisvConnectionCell := Disv.TwoDGrid.Cells[LakeCell.Cell.Column];
              DisvLakeCell := Disv.TwoDGrid.Cells[LakeCell.LakeCell.Column];
              ALake.FLakeCellList[CellIndex].ConnWidth := DisvLakeCell.SharedWidth(DisvConnectionCell);
              ConnectionWidthAnnotation := Format(StrWidthOfCellFaceB, [LakeCell.Cell.Column+1, LakeCell.LakeCell.Column+1]);
            end;
            ALake.FLakeCellList[CellIndex].ConnWidthAnnotation := ConnectionWidthAnnotation;
          end
          else
          begin
            ALake.FLakeCellList[CellIndex].ConnWidth := 0;
            ALake.FLakeCellList[CellIndex].ConnWidthAnnotation := NonHorizConnWidth;
          end;
        end;
      end;
    finally
      Results.Free;
    end;
  finally
    CellList.Free;
  end;
end;

procedure TModflowLAKMf6Writer.SetLakeOutlets;
var
  LakeIndex: Integer;
  ALake: TLake;
  LakeBoundary: TLakeMf6;
  OutletIndex: Integer;
  AnOutlet: TLakeOutlet;
  OutletObject: TObject;
  AnotherLake: TLake;
  Outlet: TLakeOutletMf6;
  InnerLakeIndex: Integer;
  TimeIndex: Integer;
  OutletTimeItem: TLakeOutletTimeItem;
  OutletSetting: TOutletSetting;
begin
  for LakeIndex := 0 to FLakes.Count - 1 do
  begin
    ALake := FLakes[LakeIndex];
    LakeBoundary := ALake.FScreenObject.ModflowLak6;
    for OutletIndex := 0 to LakeBoundary.Outlets.Count - 1 do
    begin
      AnOutlet := LakeBoundary.Outlets[OutletIndex].Outlet;
      OutletObject := AnOutlet.OutletObject;
//      if OutletObject <> nil then
      begin
        Outlet := TLakeOutletMf6.Create;
        ALake.FLakeOutlets.Add(Outlet);
        Outlet.OutletType := AnOutlet.OutletType;
        Outlet.LakeOutNumber := -1;
        if OutletObject <> nil then
        begin
          for InnerLakeIndex := 0 to FLakes.Count - 1 do
          begin
            AnotherLake := FLakes[InnerLakeIndex];
            if OutletObject = AnotherLake.FScreenObject then
            begin
              Outlet.LakeOutNumber := InnerLakeIndex;
              break;
            end;
          end;
          if Outlet.LakeOutNumber < 0 then
          begin
            frmErrorsAndWarnings.AddWarning(Model, StrLakeOutletIncorrec,
              Format(StrOutlet0dForThe, [OutletIndex+1, ALake.FScreenObject.Name]),
              ALake.FScreenObject);
          end;
        end;
        for TimeIndex := 0 to AnOutlet.LakeTimes.Count - 1 do
        begin
          OutletTimeItem := AnOutlet.LakeTimes[TimeIndex];
          OutletSetting := TOutletSetting.Create;
          Outlet.Add(OutletSetting);
          OutletSetting.StartTime := OutletTimeItem.StartTime;
          OutletSetting.EndTime := OutletTimeItem.EndTime;
          OutletSetting.Rate := EvaluateFormula(OutletTimeItem.Rate,
            Format(StrLakeOutletRateIn,
            [OutletIndex +1, OutletSetting.StartTime]),
            ALake.FScreenObject.Name);
          OutletSetting.Invert := EvaluateFormula(OutletTimeItem.Invert,
            Format(StrLakeOutletInvertI,
            [OutletIndex +1, OutletSetting.StartTime]),
            ALake.FScreenObject.Name);
          OutletSetting.Width := EvaluateFormula(OutletTimeItem.Width,
            Format(StrLakeOutletWidthIn,
            [OutletIndex +1, OutletSetting.StartTime]),
            ALake.FScreenObject.Name);
          OutletSetting.Slope := EvaluateFormula(OutletTimeItem.Slope,
            Format(StrLakeOutletSlopeIn,
            [OutletIndex +1, OutletSetting.StartTime]),
            ALake.FScreenObject.Name);
          OutletSetting.Rough := EvaluateFormula(OutletTimeItem.Roughness,
            Format(StrLakeOutletRoughnes,
            [OutletIndex +1, OutletSetting.StartTime]),
            ALake.FScreenObject.Name);
        end;
//      end
//      else
//      begin
//        frmErrorsAndWarnings.AddWarning(Model, StrLakeOutletIncorrec,
//          Format(StrOutlet0dForThe, [OutletIndex+1, ALake.FScreenObject.Name]),
//          ALake.FScreenObject);
      end;
    end;
  end;
end;

function TModflowLAKMf6Writer.EvaluateFormula(Formula: string;
  const DataIdentifier, ScreenObjectName: string): double;
var
  Compiler: TRbwParser;
  ErrorFunction: string;
  Expression: TExpression;
  ErrorMessage: string;
  ResultTypeOK: Boolean;
  procedure HandleError(E: Exception);
  var
    ErrorMessage: string;
    Formula: string;
  begin
    ErrorMessage := Format(StrErrorInTheFormula, [DataIdentifier, E.message]);
    frmFormulaErrors.AddFormulaError(ScreenObjectName, DataIdentifier,
      Expression.Decompile, ErrorMessage);
    Formula := '0';
    Compiler.Compile(Formula);
    Expression := Compiler.CurrentExpression;
    Expression.Evaluate;
  end;
begin
  Compiler := Model.rpThreeDFormulaCompiler;
  ErrorFunction := Formula;
  try
    Compiler.Compile(Formula);
  except on E: ERbwParserError do
    begin
      ErrorMessage := Format(StrErrorInTheFormula, [DataIdentifier, E.message]);
      frmFormulaErrors.AddFormulaError(ScreenObjectName,
        DataIdentifier, Formula, ErrorMessage);
      Formula := '0';
      Compiler.Compile(Formula);
    end;
  end;

  Expression := Compiler.CurrentExpression;
  ResultTypeOK := Expression.ResultType in [rdtInteger, rdtDouble];
  if not ResultTypeOK then
  begin
    raise EInvalidDataType.Create(StrInvalidDataType, Formula);
  end;

  try
    Expression.Evaluate;
  except
    on E: ERbwParserError do
    begin
      HandleError(E);
    end;
    on E: EInvalidOp do
    begin
      HandleError(E);
    end;
    on E: EDivByZero do
    begin
      HandleError(E);
    end;
    on E: EZeroDivide do
    begin
      HandleError(E);
    end;
  end;

  result := Expression.DoubleResult;

end;


procedure TModflowLAKMf6Writer.SetLakeTables;
var
  LakeIndex: Integer;
  ALake: TLake;
  LakeBoundary: TLakeMf6;
  TableRowIndex: Integer;
  TableItem: TLakeTableItemMf6;
  Formula: string;
  DataIdentifier: string;
  ALakeTableRow: TLakeTableRow;
begin
  for LakeIndex := 0 to FLakes.Count - 1 do
  begin
    ALake := FLakes[LakeIndex];
    LakeBoundary := ALake.FScreenObject.ModflowLak6;
    for TableRowIndex := 0 to LakeBoundary.LakeTable.Count - 1 do
    begin
      TableItem := LakeBoundary.LakeTable[TableRowIndex];
      ALakeTableRow := TLakeTableRow.Create;
      ALake.FLakeTable.Add(ALakeTableRow);

      Formula := TableItem.Stage;
      DataIdentifier := StrLakeStage;
      ALakeTableRow.Stage := EvaluateFormula(Formula, DataIdentifier,
        ALake.FScreenObject.Name);

      Formula := TableItem.Volume;
      DataIdentifier := StrLakeVolume;
      ALakeTableRow.Volume := EvaluateFormula(Formula, DataIdentifier,
        ALake.FScreenObject.Name);

      Formula := TableItem.SurfaceArea;
      DataIdentifier := StrLakeSurfaceArea;
      ALakeTableRow.SurfaceArea := EvaluateFormula(Formula, DataIdentifier,
        ALake.FScreenObject.Name);

      Formula := TableItem.ExchangeArea;
      if Formula = '' then
      begin
        ALakeTableRow.ExchangeArea := 0;
      end
      else
      begin
        DataIdentifier := StrLakeExchangeArea;
        ALakeTableRow.ExchangeArea := EvaluateFormula(Formula, DataIdentifier,
          ALake.FScreenObject.Name);
      end;
    end;
  end;
end;

procedure TModflowLAKMf6Writer.SetLakeTimeProperties;
var
  LakeIndex: Integer;
  ALake: TLake;
  LakeBoundary: TLakeMf6;
  TimeIndex: Integer;
  LakeItem: TLakeTimeItem;
  LakeSetting: TLakeSetting;
begin
  for LakeIndex := 0 to FLakes.Count - 1 do
  begin
    ALake := FLakes[LakeIndex];
    LakeBoundary := ALake.FScreenObject.ModflowLak6;
    for TimeIndex := 0 to LakeBoundary.Values.Count - 1 do
    begin
      LakeItem := LakeBoundary.Values[TimeIndex] as TLakeTimeItem;
      LakeSetting := TLakeSetting.Create;
      ALake.FLakeSettings.Add(LakeSetting);
      LakeSetting.StartTime := LakeItem.StartTime;
      LakeSetting.EndTime := LakeItem.EndTime;
      LakeSetting.Status := LakeItem.Status;

      LakeSetting.Stage := EvaluateFormula(LakeItem.Stage,
        Format(StrLakeStageAt0g,
        [LakeSetting.StartTime]),
        ALake.FScreenObject.Name);

      LakeSetting.Rainfall := EvaluateFormula(LakeItem.Rainfall,
        Format(StrLakeRainfallAt0,
        [LakeSetting.StartTime]),
        ALake.FScreenObject.Name);

      LakeSetting.Evaporation := EvaluateFormula(LakeItem.Evaporation,
        Format(StrLakeEvaporationAt,
        [LakeSetting.StartTime]),
        ALake.FScreenObject.Name);

      LakeSetting.Runoff := EvaluateFormula(LakeItem.Runoff,
        Format(StrLakeRunoffAt0g,
        [LakeSetting.StartTime]),
        ALake.FScreenObject.Name);

      LakeSetting.Inflow := EvaluateFormula(LakeItem.Inflow,
        Format( StrLakeInflowAt0g,
        [LakeSetting.StartTime]),
        ALake.FScreenObject.Name);

      LakeSetting.Withdrawal := EvaluateFormula(LakeItem.Withdrawal,
        Format(StrLakeWithdrawalAt,
        [LakeSetting.StartTime]),
        ALake.FScreenObject.Name);
    end;
  end;
end;

procedure TModflowLAKMf6Writer.UpdateDisplay;
var
  LakeDataArray: TModflowBoundaryDisplayDataArray;
  LakeIndex: Integer;
  ALake: TLake;
  CellIndex: Integer;
  ALakeCell: TLakeCell;
  Annotation: string;
begin
  frmErrorsAndWarnings.RemoveWarningGroup(Model,StrTheFollowingObject);
  frmErrorsAndWarnings.RemoveWarningGroup(Model,StrTheLakesDefinedBy);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrLakeOutletIncorrec);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrInvalidLakeExtout);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrInvalidLakeOutflow);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheFollowingObjectNoCells);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNoOutletLakeDefin);

  IdentifyLakesAndLakeCells;
  SetLakeCellProperties;

  LakeDataArray := Model.DataArrayManager.GetDataSetByName(
    KLake_Bed_Leakance) as TModflowBoundaryDisplayDataArray;
  Assert(LakeDataArray <> nil);
  LakeDataArray.Clear;

  Annotation := '';
  for LakeIndex := 0 to FLakes.Count - 1 do
  begin
    ALake := FLakes[LakeIndex];
    for CellIndex := 0 to ALake.FLakeCellList.Count - 1 do
    begin
      ALakeCell := ALake.FLakeCellList[CellIndex];
      if ALakeCell.BedLeakanceAnnotation <> Annotation then
      begin
        Annotation := ALakeCell.BedLeakanceAnnotation;
      end;
      LakeDataArray.AddDataValue(Annotation, ALakeCell.BedLeakance,
        ALakeCell.Cell.Column, ALakeCell.Cell.Row, ALakeCell.Cell.Layer);
    end;
  end;
  LakeDataArray.ComputeAverage;
  LakeDataArray.UpToDate := True;

  LakeDataArray := Model.DataArrayManager.GetDataSetByName(
    KLake_Bed_Thickness) as TModflowBoundaryDisplayDataArray;
  Assert(LakeDataArray <> nil);
  LakeDataArray.Clear;

  Annotation := '';
  for LakeIndex := 0 to FLakes.Count - 1 do
  begin
    ALake := FLakes[LakeIndex];
    for CellIndex := 0 to ALake.FLakeCellList.Count - 1 do
    begin
      ALakeCell := ALake.FLakeCellList[CellIndex];
      if ALakeCell.BedThicknessAnnotation <> Annotation then
      begin
        Annotation := ALakeCell.BedThicknessAnnotation;
      end;
      LakeDataArray.AddDataValue(Annotation, ALakeCell.BedThickness,
        ALakeCell.Cell.Column, ALakeCell.Cell.Row, ALakeCell.Cell.Layer);
    end;
  end;
  LakeDataArray.ComputeAverage;
  LakeDataArray.UpToDate := True;

  LakeDataArray := Model.DataArrayManager.GetDataSetByName(
    KLake_Bottom_Elevation) as TModflowBoundaryDisplayDataArray;
  Assert(LakeDataArray <> nil);
  LakeDataArray.Clear;

  Annotation := '';
  for LakeIndex := 0 to FLakes.Count - 1 do
  begin
    ALake := FLakes[LakeIndex];
    for CellIndex := 0 to ALake.FLakeCellList.Count - 1 do
    begin
      ALakeCell := ALake.FLakeCellList[CellIndex];
      if ALakeCell.BotElevAnnotation <> Annotation then
      begin
        Annotation := ALakeCell.BotElevAnnotation;
      end;
      LakeDataArray.AddDataValue(Annotation, ALakeCell.BotElev,
        ALakeCell.Cell.Column, ALakeCell.Cell.Row, ALakeCell.Cell.Layer);
    end;
  end;
  LakeDataArray.ComputeAverage;
  LakeDataArray.UpToDate := True;

  LakeDataArray := Model.DataArrayManager.GetDataSetByName(
    KLake_Top_Elevation) as TModflowBoundaryDisplayDataArray;
  Assert(LakeDataArray <> nil);
  LakeDataArray.Clear;

  Annotation := '';
  for LakeIndex := 0 to FLakes.Count - 1 do
  begin
    ALake := FLakes[LakeIndex];
    for CellIndex := 0 to ALake.FLakeCellList.Count - 1 do
    begin
      ALakeCell := ALake.FLakeCellList[CellIndex];
      if ALakeCell.TopElevAnnotation <> Annotation then
      begin
        Annotation := ALakeCell.TopElevAnnotation;
      end;
      LakeDataArray.AddDataValue(Annotation, ALakeCell.TopElev,
        ALakeCell.Cell.Column, ALakeCell.Cell.Row, ALakeCell.Cell.Layer);
    end;
  end;
  LakeDataArray.ComputeAverage;
  LakeDataArray.UpToDate := True;

  LakeDataArray := Model.DataArrayManager.GetDataSetByName(
    KLake_Connection_Length) as TModflowBoundaryDisplayDataArray;
  Assert(LakeDataArray <> nil);
  LakeDataArray.Clear;

  Annotation := '';
  for LakeIndex := 0 to FLakes.Count - 1 do
  begin
    ALake := FLakes[LakeIndex];
    for CellIndex := 0 to ALake.FLakeCellList.Count - 1 do
    begin
      ALakeCell := ALake.FLakeCellList[CellIndex];
      if ALakeCell.ConnLengthAnnotation <> Annotation then
      begin
        Annotation := ALakeCell.ConnLengthAnnotation;
      end;
      LakeDataArray.AddDataValue(Annotation, ALakeCell.ConnLength,
        ALakeCell.Cell.Column, ALakeCell.Cell.Row, ALakeCell.Cell.Layer);
    end;
  end;
  LakeDataArray.ComputeAverage;
  LakeDataArray.UpToDate := True;

  LakeDataArray := Model.DataArrayManager.GetDataSetByName(
    KLake_Connection_Width) as TModflowBoundaryDisplayDataArray;
  Assert(LakeDataArray <> nil);
  LakeDataArray.Clear;

  Annotation := '';
  for LakeIndex := 0 to FLakes.Count - 1 do
  begin
    ALake := FLakes[LakeIndex];
    for CellIndex := 0 to ALake.FLakeCellList.Count - 1 do
    begin
      ALakeCell := ALake.FLakeCellList[CellIndex];
      if ALakeCell.ConnWidthAnnotation <> Annotation then
      begin
        Annotation := ALakeCell.ConnWidthAnnotation;
      end;
      LakeDataArray.AddDataValue(Annotation, ALakeCell.ConnWidth,
        ALakeCell.Cell.Column, ALakeCell.Cell.Row, ALakeCell.Cell.Layer);
    end;
  end;
  LakeDataArray.ComputeAverage;
  LakeDataArray.UpToDate := True;
end;

procedure TModflowLAKMf6Writer.IdentifyLakesAndLakeCells;
var
  DisvGrid: TModflowDisvGrid;
  Grid: TCustomModelGrid;
  IDomainArray: TDataArray;
  CellList: TCellLocationList;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  LakeBoundary: TLakeMf6;
  ALake: TLake;
  ACell: TCellAssignment;
  ALakeCell: TLakeCell;
  CellIndex: Integer;
  NeighborIndex: Integer;
  NeighborCell: TCellLocation;
  Obs: TLakObservation;
  MfObs: TModflow6Obs;
begin
  if Model.DisvUsed then
  begin
    DisvGrid := Model.DisvGrid;
    Grid := nil;
  end
  else
  begin
    DisvGrid := nil;
    Grid := Model.Grid;
  end;
  IDomainArray := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);
  CellList := TCellLocationList.Create;
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
      if (ScreenObject.ModflowLak6 <> nil) and ScreenObject.ModflowLak6.Used then
      begin
        LakeBoundary := ScreenObject.ModflowLak6;
        FLakeScreenObjects.Add(ScreenObject);
        ALake := TLake.Create;
        FLakes.Add(ALake);
        ALake.FScreenObject := ScreenObject;

        ALake.FStartingStage := EvaluateFormula(LakeBoundary.StartingStage,
          StrLakeStartingStage, ALake.FScreenObject.Name);

        ScreenObject.GetCellsToAssign('0', nil, nil, ALake.FCellList, alAll, Model);
        if LakeBoundary.Embedded and (ALake.FCellList.Count <> 1) then
        begin
          frmErrorsAndWarnings.AddWarning(Model, StrTheFollowingObject,
            ALake.FScreenObject.Name, ALake.FScreenObject);
        end;
        if (ALake.FCellList.Count = 0) then
        begin
          frmErrorsAndWarnings.AddWarning(Model, StrTheLakesDefinedBy,
            ALake.FScreenObject.Name, ALake.FScreenObject);
        end;
        if LakeBoundary.Embedded and (ALake.FCellList.Count = 1) then
        begin
          ACell := ALake.FCellList[0];
          if lctHorizontal in LakeBoundary.LakeConnections then
          begin
            ALakeCell := TLakeCell.Create;
            ALake.FLakeCellList.Add(ALakeCell);
            ALakeCell.Cell := ACell.Cell;
            ALakeCell.LakeType := ltHorizEmbed;
            ALakeCell.LakeCell := ACell.Cell;
          end;
          if lctVertical in LakeBoundary.LakeConnections then
          begin
            ALakeCell := TLakeCell.Create;
            ALake.FLakeCellList.Add(ALakeCell);
            ALakeCell.Cell := ACell.Cell;
            ALakeCell.LakeType := ltVertEmbed;
            ALakeCell.LakeCell := ACell.Cell;
          end;
        end
        else
        begin
          for CellIndex := 0 to ALake.FCellList.Count - 1 do
          begin
            ACell := ALake.FCellList[CellIndex];
            if lctHorizontal in LakeBoundary.LakeConnections then
            begin
              CellList.Clear;
              if Grid = nil then
              begin
                DisvGrid.GetHorizontalNeighbors(ACell.Layer, ACell.Row, ACell.Column, CellList);
              end
              else
              begin
                Grid.GetHorizontalNeighbors(ACell.Layer, ACell.Row, ACell.Column, CellList);
              end;
              for NeighborIndex := 0 to CellList.Count - 1 do
              begin
                NeighborCell := CellList[NeighborIndex];
                if IDomainArray.IntegerData[NeighborCell.Layer, NeighborCell.Row, NeighborCell.Column] > 0 then
                begin
                  ALakeCell := TLakeCell.Create;
                  ALake.FLakeCellList.Add(ALakeCell);
                  ALakeCell.Cell := NeighborCell;
                  ALakeCell.LakeType := ltHoriz;
                  ALakeCell.LakeCell := ACell.Cell;
                end;
              end;
            end;
            if (lctVertical in LakeBoundary.LakeConnections) and (ACell.Layer + 1 < Model.LayerCount) then
            begin
              if lctHorizontal in LakeBoundary.LakeConnections then
              begin
                if IDomainArray.IntegerData[ACell.Layer + 1, ACell.Row, ACell.Column] > 0 then
                begin
                  ALakeCell := TLakeCell.Create;
                  ALake.FLakeCellList.Add(ALakeCell);
                  ALakeCell.Cell := ACell.Cell;
                  ALakeCell.Cell.Layer := ALakeCell.Cell.Layer + 1;
                  ALakeCell.LakeType := ltVert;
                  ALakeCell.LakeCell := ACell.Cell;
                end;
              end
              else
              begin
                if IDomainArray.IntegerData[ACell.Layer, ACell.Row, ACell.Column] > 0 then
                begin
                  ALakeCell := TLakeCell.Create;
                  ALake.FLakeCellList.Add(ALakeCell);
                  ALakeCell.Cell := ACell.Cell;
                  ALakeCell.LakeType := ltVert;
                  ALakeCell.LakeCell := ACell.Cell;
                end;
              end;
            end;
          end;
        end;

        if IsMf6Observation(ScreenObject) then
        begin
          MfObs := ScreenObject.Modflow6Obs;
          Obs.FName := MfObs.Name;
          Obs.FBoundName := ScreenObject.Name;
          Obs.FObsTypes := MfObs.LakObs;
          if LakeBoundary.Outlets.Count = 0 then
          begin
            if loExternalOutflow in Obs.FObsTypes then
            begin
              frmErrorsAndWarnings.AddWarning(Model, StrInvalidLakeExtout,
                Format(StrSDefinesALakeEx, [ScreenObject.Name]), ScreenObject);
              Exclude(Obs.FObsTypes, loExternalOutflow);
            end;
            if loOutlet in Obs.FObsTypes then
            begin
              frmErrorsAndWarnings.AddWarning(Model, StrInvalidLakeOutflow,
                Format(StrSDefinesALakeOu, [ScreenObject.Name]), ScreenObject);
              Exclude(Obs.FObsTypes, loOutlet);
            end;
          end;
          FObservations.Add(Obs);
        end;
      end;
    end;
  finally
    CellList.Free;
  end;
end;

function TModflowLAKMf6Writer.IsMf6Observation(
  AScreenObject: TScreenObject): Boolean;
var
  MfObs: TModflow6Obs;
begin
  MfObs := AScreenObject.Modflow6Obs;
  Result := (MfObs <> nil) and MfObs.Used and (MfObs.LakObs <> []);
end;

class function TModflowLAKMf6Writer.ObservationExtension: string;
begin
  result := '.ob_lak';
end;

procedure TModflowLAKMf6Writer.WriteOptions;
var
  StageFileName: string;
  BudgetFileName: string;
  ModflowOptions: TModflowOptions;
  time_conversion: double;
  length_conversion: double;
  NameOfFile: string;
  procedure WriteSingleQuoteMF6;
  begin
    WriteString('''');
  end;
begin
  WriteBeginOptions;
  // [AUXILIARY <auxiliary(naux)>]
  WriteBoundNamesOption;
  PrintListInputOption;

  if FLakMf6Package.PrintStage then
  begin
    WriteString('  PRINT_STAGE');
    NewLine;
  end;

  PrintFlowsOption;
  WriteSaveFlowsOption;

  if FLakMf6Package.SaveStage then
  begin
    StageFileName := ChangeFileExt(FFileName, '.lk_stg');
    Model.AddModelOutputFile(StageFileName);
    StageFileName := ExtractFileName(StageFileName);
    WriteString('  STAGE FILEOUT ');
    WriteSingleQuoteMF6;
    WriteString(StageFileName);
    WriteSingleQuoteMF6;
    NewLine;
  end;

//  [BUDGET FILEOUT <budgetfile>]
  if FLakMf6Package.SaveBudget then
  begin
    BudgetFileName := ChangeFileExt(FFileName, '.lk_bud');
    Model.AddModelOutputFile(BudgetFileName);
    BudgetFileName := ExtractFileName(BudgetFileName);
    WriteString('  BUDGET FILEOUT ');
    WriteString(BudgetFileName);
    NewLine;
  end;

//  [TS6 FILEIN <ts6_filename>]

  if FObservations.Count > 0 then
  begin
    WriteString('    OBS6 FILEIN ');
    NameOfFile := ChangeFileExt(FFileName, ObservationExtension);
    Model.AddModelInputFile(NameOfFile);
    NameOfFile := ExtractFileName(NameOfFile);
    WriteString(NameOfFile);
    NewLine;
  end;



//  [MOVER]
//  [SURFDEP <surfdep>]

  WriteString('  SURFDEP ');
  WriteFloat(FLakMf6Package.SurfDepDepth);
  NewLine;

  ModflowOptions := Model.ModflowOptions;
  time_conversion := 1.0;
  case ModflowOptions.TimeUnit of
    0:
      begin
        time_conversion := 1.0;
      end;
    1:
      begin
        time_conversion := 1.0;
      end;
    2:
      begin
        time_conversion := 60.0;
      end;
    3:
      begin
        time_conversion := 3600.0;
      end;
    4:
      begin
        time_conversion := 86400.0;
      end;
    5:
      begin
        time_conversion := 31557600.0;
      end;
    else
      Assert(False);
  end;

  WriteString('  TIME_CONVERSION ');
  WriteFloat(time_conversion);
  NewLine;

  length_conversion := 1.0;
  case ModflowOptions.LengthUnit of
    0:
      begin
        length_conversion := 1.0;
      end;
    1:
      begin
        length_conversion := 3.28081
      end;
    2:
      begin
        length_conversion := 1.0;
      end;
    3:
      begin
        length_conversion := 100.0;
      end;
    else
      Assert(False);
  end;

  WriteString('  LENGTH_CONVERSION ');
  WriteFloat(length_conversion);
  NewLine;

  if (MvrWriter <> nil) then
  begin
    if spcLak in TModflowMvrWriter(MvrWriter).UsedPackages then
    begin
      WriteString('  MOVER');
      NewLine
    end;
  end;


  WriteEndOptions
end;

procedure TModflowLAKMf6Writer.WriteOutlets;
var
  LakeIndex: Integer;
  ALake: TLake;
  OutletIndex: Integer;
  AnOutlet: TLakeOutletMf6;
  OutletNumber: Integer;
  ASetting: TOutletSetting;
begin
  if noutlets > 0 then
  begin
    WriteString('BEGIN OUTLETS');
    NewLine;

    OutletNumber := 0;
    for LakeIndex := 0 to FLakes.Count - 1 do
    begin
      ALake := FLakes[LakeIndex];
      for OutletIndex := 0 to ALake.FLakeOutlets.Count - 1 do
      begin
        AnOutlet := ALake.FLakeOutlets[OutletIndex];
        if AnOutlet.LakeOutNumber < 0 then
        begin
          frmErrorsAndWarnings.AddWarning(Model, StrNoOutletLakeDefin,
            Format(StrInTheLakeDefined,
            [ALake.FScreenObject.Name, OutletIndex+1]), ALake.FScreenObject);
        end;

        Inc(OutletNumber);

        WriteString('  ');
        WriteInteger(OutletNumber);
        WriteInteger(LakeIndex+1);
        WriteInteger(AnOutlet.LakeOutNumber+1);
        case AnOutlet.OutletType of
          lotSpecified:
            begin
              WriteString(' SPECIFIED');
            end;
          lotManning:
            begin
              WriteString(' MANNING');
            end;
          lotWeir:
            begin
              WriteString(' WEIR');
            end;
          else
            Assert(False);
        end;

        ASetting := AnOutlet[0];
        WriteFloat(ASetting.Invert);
        WriteFloat(ASetting.Width);
        WriteFloat(ASetting.Rough);
        WriteFloat(ASetting.Slope);
        NewLine;
      end;
    end;
    WriteString('END OUTLETS');
    NewLine;
    NewLine;
  end;
end;

procedure TModflowLAKMf6Writer.WritePackageData;
var
  LakeIndex: Integer;
  ALake: TLake;
  BoundName: string;
begin
  WriteBeginPackageData;
  for LakeIndex := 0 to FLakes.Count - 1 do
  begin
    ALake := FLakes[LakeIndex];
    WriteString('  ');
    WriteInteger(LakeIndex+1);
    WriteFloat(ALake.FStartingStage);
    WriteInteger(ALake.FLakeCellList.Count);
    // aux
    BoundName := Copy(ALake.FScreenObject.Name, 1, 40);
    BoundName := ' ''' + BoundName + ''' ';
    WriteString(BoundName);
    NewLine;
  end;
  WriteEndPackageData
end;

procedure TModflowLAKMf6Writer.WritePeriods;
var
  AllTimes: TRealList;
  StressPeriodStartTimes: TRealList;
  LakeIndex: Integer;
  ALake: TLake;
  OutletIndex: Integer;
  AnOutlet: TLakeOutletMf6;
  TimeIndex: Integer;
  ALakeSetting: TLakeSetting;
  OutletSetting: TOutletSetting;
  ATime: Double;
  StressPeriodIndex: Integer;
  OutletNumber: Integer;
  FirstTime: Double;
  LastTime: Double;
  MoverWriter: TModflowMvrWriter;
  MvrReceiver: TMvrReceiver;
  MvrRegSourceKey: TMvrRegisterKey;
  LocalScreenObject: TScreenObject;
  MvrUsed: Boolean;
  UsedOutlets: TList<Integer>;
  ReceiverItem: TReceiverItem;
begin

  if MvrWriter <> nil then
  begin
    MoverWriter := MvrWriter as TModflowMvrWriter;
  end
  else
  begin
    MoverWriter := nil;
  end;
  MvrReceiver.ReceiverKey.ReceiverPackage := rpcLak;
  MvrReceiver.ReceiverValues.StreamCells := nil;

  FirstTime := Model.ModflowFullStressPeriods.First.StartTime;
  LastTime := Model.ModflowFullStressPeriods.Last.EndTime;
  UsedOutlets := TList<Integer>.Create;
  AllTimes := TRealList.Create;
  StressPeriodStartTimes := TRealList.Create;
  try
    AllTimes.Sorted := True;

    for LakeIndex := 0 to FLakes.Count - 1 do
    begin
      ALake := FLakes[LakeIndex];
      ALake.FStartingTimeIndex := 0;
      for TimeIndex := 0 to ALake.FLakeSettings.Count - 1 do
      begin
        ALakeSetting := ALake.FLakeSettings[TimeIndex];
        ATime := ALakeSetting.StartTime;
        if (FirstTime <= ATime) and (ATime < LastTime) then
        begin
          AllTimes.AddUnique(ATime);
        end;
        ATime := ALakeSetting.EndTime;
        if (FirstTime <= ATime) and (ATime < LastTime) then
        begin
          AllTimes.AddUnique(ATime);
        end;
      end;

      for OutletIndex := 0 to ALake.FLakeOutlets.Count - 1 do
      begin
        AnOutlet := ALake.FLakeOutlets[OutletIndex];
        AnOutlet.FStartingTimeIndex := 0;
        for TimeIndex := 0 to AnOutlet.Count - 1 do
        begin
          OutletSetting := AnOutlet[TimeIndex];
          ATime := OutletSetting.StartTime;
          if (FirstTime <= ATime) and (ATime < LastTime) then
          begin
            AllTimes.AddUnique(ATime);
          end;
          ATime := OutletSetting.EndTime;
          if (FirstTime <= ATime) and (ATime < LastTime) then
          begin
            AllTimes.AddUnique(ATime);
          end;
//          AllTimes.AddUnique(OutletSetting.StartTime);
//          AllTimes.AddUnique(OutletSetting.EndTime);
        end;
      end;
    end;

    for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
    begin
      StressPeriodStartTimes.Add(Model.ModflowFullStressPeriods[TimeIndex].StartTime);
    end;
    StressPeriodStartTimes.Sorted := True;

    for TimeIndex := 0 to AllTimes.Count - 1 do
    begin
      ATime := AllTimes[TimeIndex];
      StressPeriodIndex := StressPeriodStartTimes.IndexOf(ATime);
      Assert(StressPeriodIndex >= 0);
      MvrReceiver.ReceiverKey.StressPeriod := StressPeriodIndex;
      MvrRegSourceKey.StressPeriod := StressPeriodIndex;

      WriteString('BEGIN PERIOD ');
      WriteInteger(StressPeriodIndex + 1);
      NewLine;

      OutletNumber := 0;
      for LakeIndex := 0 to FLakes.Count - 1 do
      begin
        ALake := FLakes[LakeIndex];
        Assert(ALake.FScreenObject <> nil);
        MvrReceiver.ReceiverKey.ScreenObject := ALake.FScreenObject;
        MvrRegSourceKey.SourceKey.ScreenObject := ALake.FScreenObject;
        LocalScreenObject := ALake.FScreenObject as TScreenObject;
        MvrUsed := (MoverWriter <> nil)
          and (LocalScreenObject.ModflowMvr <> nil)
          and LocalScreenObject.ModflowMvr.Used
          and (LocalScreenObject.ModflowMvr.SourcePackageChoice = spcLak);
        if MvrUsed then
        begin
          UsedOutlets.Clear;
          for OutletIndex := 0 to LocalScreenObject.ModflowMvr.Receivers.Count - 1 do
          begin
            ReceiverItem := LocalScreenObject.ModflowMvr.Receivers[OutletIndex];
            UsedOutlets.Add(ReceiverItem.LakeOutlet);
          end;
        end;

        if ALake.FStartingTimeIndex < ALake.FLakeSettings.Count then
        begin
          ALakeSetting := ALake.FLakeSettings[ALake.FStartingTimeIndex];
          while ALakeSetting.StartTime < ATime do
          begin
            Inc(ALake.FStartingTimeIndex);
            if ALake.FStartingTimeIndex < ALake.FLakeSettings.Count then
            begin
              ALakeSetting := ALake.FLakeSettings[ALake.FStartingTimeIndex];
            end
            else
            begin
              break;
            end;
          end;
          if ALakeSetting.StartTime = ATime then
          begin
            WriteString('  ');
            WriteInteger(LakeIndex+1);
            MvrReceiver.ReceiverValues.Index := LakeIndex+1;
            if MoverWriter <> nil then
            begin
              MoverWriter.AddMvrReceiver(MvrReceiver);
            end;
            case ALakeSetting.Status of
              lsActive:
                begin
                  WriteString('  STATUS ACTIVE');
                end;
              lsInactive:
                begin
                  WriteString(' STATUS INACTIVE');
                end;
              lsConstant:
                begin
                  WriteString(' STATUS CONSTANT');
                end;
              else
                Assert(False);
            end;
            NewLine;

            if ALakeSetting.Status = lsConstant then
            begin
              WriteString('  ');
              WriteInteger(LakeIndex+1);
              WriteString('  STAGE');
              WriteFloat(ALakeSetting.Stage);
              NewLine;
            end;

            WriteString('  ');
            WriteInteger(LakeIndex+1);
            WriteString('  RAINFALL');
            WriteFloat(ALakeSetting.Rainfall);
            NewLine;

            WriteString('  ');
            WriteInteger(LakeIndex+1);
            WriteString('  EVAPORATION');
            WriteFloat(ALakeSetting.Evaporation);
            NewLine;

            WriteString('  ');
            WriteInteger(LakeIndex+1);
            WriteString('  RUNOFF');
            WriteFloat(ALakeSetting.Runoff);
            NewLine;

            WriteString('  ');
            WriteInteger(LakeIndex+1);
            WriteString('  INFLOW');
            WriteFloat(ALakeSetting.Inflow);
            NewLine;

            WriteString('  ');
            WriteInteger(LakeIndex+1);
            WriteString('  WITHDRAWAL');
            WriteFloat(ALakeSetting.Withdrawal);
            NewLine;
          end;
        end;

        for OutletIndex := 0 to ALake.FLakeOutlets.Count - 1 do
        begin
          AnOutlet := ALake.FLakeOutlets[OutletIndex];
          Inc(OutletNumber);

          MvrRegSourceKey.Index := OutletNumber;
          MvrRegSourceKey.SourceKey.MvrIndex := OutletIndex;

          if AnOutlet.FStartingTimeIndex < AnOutlet.Count then
          begin
            OutletSetting := AnOutlet[AnOutlet.FStartingTimeIndex];
            while OutletSetting.StartTime < ATime do
            begin
              Inc(AnOutlet.FStartingTimeIndex);
              if AnOutlet.FStartingTimeIndex < AnOutlet.Count then
              begin
                OutletSetting := AnOutlet[AnOutlet.FStartingTimeIndex];
              end
              else
              begin
                break;
              end;
            end;
            if OutletSetting.StartTime = ATime then
            begin
              WriteString('  ');
              WriteInteger(OutletNumber);
              WriteString('  RATE');
              WriteFloat(OutletSetting.Rate);
              NewLine;

              WriteString('  ');
              WriteInteger(OutletNumber);
              WriteString('  INVERT');
              WriteFloat(OutletSetting.Invert);
              NewLine;

              WriteString('  ');
              WriteInteger(OutletNumber);
              WriteString('  WIDTH');
              WriteFloat(OutletSetting.Width);
              NewLine;

              WriteString('  ');
              WriteInteger(OutletNumber);
              WriteString('  SLOPE');
              WriteFloat(OutletSetting.Slope);
              NewLine;

              WriteString('  ');
              WriteInteger(OutletNumber);
              WriteString('  ROUGH');
              WriteFloat(OutletSetting.Rough);
              NewLine;

              if MvrUsed and (UsedOutlets.IndexOf(OutletIndex+1) >= 0 ) then
              begin
                TModflowMvrWriter(MvrWriter).AddMvrSource(MvrRegSourceKey);
              end;

            end;
          end;
        end;
      end;

      WriteString('END PERIOD ');
      NewLine;
      NewLine;
    end;

  finally
    AllTimes.Free;
    StressPeriodStartTimes.Free;
    UsedOutlets.Free;
  end;
end;

{ TLake }

{ TLake }

constructor TLake.Create;
begin
  FCellList := TCellAssignmentList.Create;
  FLakeCellList := TLakeCellList.Create;
  FLakeTable := TLakeTable.Create;
  FLakeOutlets := TLakeOutlets.Create;
  FLakeSettings := TLakeSettings.Create;
end;

destructor TLake.Destroy;
begin
  FLakeSettings.Free;
  FLakeOutlets.Free;
  FLakeTable.Free;
  FLakeCellList.Free;
  FCellList.Free;
  inherited;
end;

end.
