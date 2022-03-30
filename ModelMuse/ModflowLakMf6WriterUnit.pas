unit ModflowLakMf6WriterUnit;

interface

uses SysUtils, Classes, PhastModelUnit, CustomModflowWriterUnit,
  ModflowPackageSelectionUnit, ModflowTimeUnit, ModflowLakUnit,
  ScreenObjectUnit, System.Generics.Collections, ModflowCellUnit,
  ModflowLakMf6Unit, Modflow6ObsUnit, GoPhastTypes;

type
  TLakObservation = record
    FName: string;
    FBoundName: string;
    FObsTypes: TLakObs;
    FModflow6Obs: TModflow6Obs;
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

    BedLeakancePestName: string;
    BedThicknessPestName: string;
    BotElevPestName: string;
    TopElevPestName: string;
    ConnLengthPestName: string;
//    ConnWidthPestName: string;
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
    StagePestName: string;
    VolumePestName: string;
    SurfaceAreaPestName: string;
    ExchangeAreaPestName: string;
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
    RatePestParam: string;
    InvertPestParam: string;
    WidthPestParam: string;
    SlopePestParam: string;
    RoughPestParam: string;
    RateTimeSeriesName: string;
    InvertTimeSeriesName: string;
    RoughTimeSeriesName: string;
    WidthTimeSeriesName: string;
    SlopeTimeSeriesName: string;
  end;

  TLakeOutletMf6 = class(TObjectList<TOutletSetting>)
    OutletType: TLakeOutletType;
    // @name starts at zero.
    LakeOutNumber: integer;
    FStartingTimeIndex: Integer;
  end;

  TLakeOutlets = TObjectList<TLakeOutletMf6>;

  TLakeSetting = class(TObject)
  private
    function GetPestMethod(Index: Integer): TPestParamMethod;
    function GetPestName(Index: Integer): string;
    function GetPestSeries(Index: Integer): string;
    function GetValue(Index: Integer): double;
    procedure SetPestMethod(Index: Integer; const Value: TPestParamMethod);
    procedure SetPestName(Index: Integer; const Value: string);
    procedure SetPestSeries(Index: Integer; const Value: string);
    procedure SetValue(Index: Integer; const Value: double);
    function GetTimeSeriesName(Index: Integer): string;
    procedure SetTimeSeriesName(Index: Integer; const Value: string);
  public
    StartTime: double;
    EndTime: double;
    Status: TLakeStatus;
    Stage: double;
    Rainfall: double;
    Evaporation: double;
    Runoff: double;
    Inflow: double;
    Withdrawal: double;

    StagePest: string;
    RainfallPest: string;
    EvaporationPest: string;
    RunoffPest: string;
    InflowPest: string;
    WithdrawalPest: string;

    StagePestSeries: string;
    RainfallPestSeries: string;
    EvaporationPestSeries: string;
    RunoffPestSeries: string;
    InflowPestSeries: string;
    WithdrawalPestSeries: string;

    StagePestMethod: TPestParamMethod;
    RainfallPestMethod: TPestParamMethod;
    EvaporationPestMethod: TPestParamMethod;
    RunoffPestMethod: TPestParamMethod;
    InflowPestMethod: TPestParamMethod;
    WithdrawalPestMethod: TPestParamMethod;

    StageTimeSeriesName: string;
    RainfallTimeSeriesName: string;
    EvaporationTimeSeriesName: string;
    RunoffTimeSeriesName: string;
    InflowTimeSeriesName: string;
    WithdrawalTimeSeriesName: string;

    // GWT
    GwtStatus: TGwtBoundaryStatusArray;
    SpecifiedConcentrations: TGwtCellData;
    RainfallConcentrations: TGwtCellData;
    EvapConcentrations: TGwtCellData;
    RunoffConcentrations: TGwtCellData;
    InflowConcentrations: TGwtCellData;

    property Value[Index: Integer]: double read GetValue write SetValue;
    property PestName[Index: Integer]: string read GetPestName
      write SetPestName;
    property PestSeries[Index: Integer]: string read GetPestSeries
      write SetPestSeries;
    property PestMethod[Index: Integer]: TPestParamMethod read GetPestMethod
      write SetPestMethod;
    property TimeSeriesName[Index: Integer]: string read GetTimeSeriesName
      write SetTimeSeriesName;
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
    FStartingStagePestName: string;
    // GWT
    FStartingConcentrations: TGwtCellData;

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
    FSpeciesIndex: Integer;
    procedure Evaluate;
    procedure IdentifyLakesAndLakeCells;
    procedure SetLakeCellProperties;
    procedure SetLakeTables;
    procedure SetLakeOutlets;
    procedure SetLakeTimeProperties;
    function EvaluateFormula(Formula: string; const DataIdentifier,
      ScreenObjectName: string; out PestParamName: string): double;
    procedure WriteOptions;
    procedure WriteDimensions;
    procedure WritePackageData;
    procedure WriteConnectionData;
    procedure WriteLakeTables;
    procedure WriteLakeTableNames;
    procedure WriteOutlets;
    procedure WriteStressPeriods;
    function IsMf6Observation(AScreenObject: TScreenObject): Boolean; //override;
//    function IsMf6ToMvrObservation(AScreenObject: TScreenObject): Boolean;
    class function ObservationExtension: string; //override;
    procedure WriteLakeValueOrFormula(LakeSetting: TLakeSetting; Index: integer);
    procedure WriteFileInternal;
    procedure WriteAdditionalAuxVariables;
    // SFT
    procedure WriteGwtOptions;
    procedure WriteGwtPackageData;
    procedure WriteGwtStressPeriods;
    procedure WriteGwtFileInternal;
  protected
    function Package: TModflowPackageSelection; override;
  public
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
    procedure WriteLktFile(const AFileName: string; SpeciesIndex: Integer);
    procedure UpdateDisplay;
    property DirectObsLines: TStrings read FDirectObsLines write FDirectObsLines;
    property CalculatedObsLines: TStrings read FCalculatedObsLines write FCalculatedObsLines;
    property FileNameLines: TStrings read FFileNameLines write FFileNameLines;
    class function Extension: string; override;
  end;


implementation

uses
  ModflowOptionsUnit, frmProgressUnit, SparseDataSets,
  SparseArrayUnit, frmErrorsAndWarningsUnit, DataSetUnit,
  ModflowIrregularMeshUnit, AbstractGridUnit, RealListUnit, RbwParser,
  frmFormulaErrorsUnit, System.Math, Modflow6ObsWriterUnit,
  FastGEO, ModflowBoundaryDisplayUnit, ModflowMvrWriterUnit, ModflowMvrUnit,
  ModflowParameterUnit, ModelMuseUtilities, Modflow6TimeSeriesUnit,
  Mt3dmsChemSpeciesUnit, frmGoPhastUnit, Mt3dmsChemUnit;

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
//  StrLakeTopIsAboveTh = 'Lake top is above the top of the model in a horizontal' +
//  'ly connected lake cell';
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
  StrSAndThenRaised = '%s  and then raised to the cell bottom';
  StrSAndThenLowered = '%s  and then lowered to the cell top';
  StrLakeEtError = 'Lake evaporation rate less than zero.';
  StrTheLakeEvaporation = 'The lake evaporation rate at time = %0:g defined ' +
  'by the object %1:s is less than zero.';
  StrLakeBottomAndTop = 'Lake bottom and top elevations inconsistent';
  StrAtLayerRowColumn = 'At (Layer,Row,Column) = (%0:d,%1:d,%2:d), the lake ' +
  'bottom was greater than or equal to the lake top. The lake bottom and top' +
  ' were assigned via the following methods: "%3:s" and "%4:s"';
  StrNoOutletProperties = 'No outlet properties defined';
  StrInTheLakeDefinedProperties = 'In the lake defined by the object %0:s, n' +
  'o outlet properties are defined for outlet %1:d.';
  StrLakeChemSpeciesD = 'Lake Chem Species %d';

const
  StrLakeFlowPackageName = 'LAK-1';

{ TModflowLAKMf6Writer }

constructor TModflowLAKMf6Writer.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FLakeScreenObjects := TScreenObjectList.Create;
  FLakes := TLakeList.Create;
  FObservations := TLakObservationList.Create;
  DirectObsLines := Model.DirectObservationLines;
  CalculatedObsLines := Model.DerivedObservationLines;
  FileNameLines := Model.FileNameLines;
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
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrLakeEtError);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrLakeBottomAndTop);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoOutletProperties);
//  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrLakeTopIsAboveTh);

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

procedure TModflowLAKMf6Writer.WriteAdditionalAuxVariables;
var
  SpeciesIndex: Integer;
  ASpecies: TMobileChemSpeciesItem;
begin
  if Model.GwtUsed then
  begin
    for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
    begin
      ASpecies := Model.MobileComponents[SpeciesIndex];
      WriteString(' ' + ASpecies.Name);
    end;
  end;
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
        WriteFormulaOrValueBasedOnAPestName(ACell.BedLeakancePestName,
          ACell.BedLeakance, ACell.Cell.Layer, ACell.Cell.Row, ACell.Cell.Column);
      end;

      if (ACell.BotElev >= ACell.TopElev)
        and (ACell.LakeType in [ltHoriz, ltHorizEmbed]) then
      begin
        frmErrorsAndWarnings.AddError(Model, StrLakeBottomAndTop,
          Format(StrAtLayerRowColumn,
            [ACell.Cell.Layer+1, ACell.Cell.Row+1, ACell.Cell.Column+1,
            ACell.BotElevAnnotation, ACell.TopElevAnnotation]));
      end;

      WriteFormulaOrValueBasedOnAPestName(ACell.BotElevPestName,
        ACell.BotElev, ACell.Cell.Layer, ACell.Cell.Row, ACell.Cell.Column);
      WriteFormulaOrValueBasedOnAPestName(ACell.TopElevPestName,
        ACell.TopElev, ACell.Cell.Layer, ACell.Cell.Row, ACell.Cell.Column);
      WriteFormulaOrValueBasedOnAPestName(ACell.ConnLengthPestName,
        ACell.ConnLength, ACell.Cell.Layer, ACell.Cell.Row, ACell.Cell.Column);
      WriteFloat(ACell.ConnWidth);
      NewLine;
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
  FInputFileName := FFileName;
  WriteToNameFile('LAK6', 0, FFileName, foInput, Model, False, StrLakeFlowPackageName);
  FNameOfFile := FFileName;

  WriteLakeTables;
  WriteFileInternal;

  if FObservations.Count > 0 then
  begin
    ObsWriter := TLakObsWriter.Create(Model, etExport, FObservations);
    try
      ObsWriter.WriteFile(ChangeFileExt(FFileName, ObservationExtension));
    finally
      ObsWriter.Free;
    end;
  end;

  if  Model.PestUsed and FPestParamUsed then
  begin
    frmErrorsAndWarnings.BeginUpdate;
    try
      FNameOfFile := FNameOfFile + '.tpl';
      WritePestTemplateLine(FNameOfFile);
      WritingTemplate := True;
      WriteLakeTables;
      WriteFileInternal;
    finally
      frmErrorsAndWarnings.EndUpdate;
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
  LakeTableFFileName: string;
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
        LakeTableFFileName := ChangeFileExt(FFileName,Extension);
        ALake.FLakeTable.FFileName := LakeTableFFileName;

        Model.FilesToDelete.Add(LakeTableFFileName);
        if WritingTemplate then
        begin
          LakeTableFFileName := LakeTableFFileName + '.tpl';
          WritePestTemplateLine(LakeTableFFileName);
        end;

        OpenFile(LakeTableFFileName);
        try
          frmProgressMM.AddMessage('Writing LAK6 Table input.');

          WriteTemplateHeader;

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
            WriteFormulaOrValueBasedOnAPestName(TableRow.StagePestName,
              TableRow.Stage, -1, -1, -1);
            WriteFormulaOrValueBasedOnAPestName(TableRow.VolumePestName,
              TableRow.Volume, -1, -1, -1);
            WriteFormulaOrValueBasedOnAPestName(TableRow.SurfaceAreaPestName,
              TableRow.SurfaceArea, -1, -1, -1);
            if Embeded then
            begin
              WriteFormulaOrValueBasedOnAPestName(TableRow.ExchangeAreaPestName,
                TableRow.ExchangeArea, -1, -1, -1);
//              WriteFloat(TableRow.ExchangeArea);
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

procedure TModflowLAKMf6Writer.WriteLakeValueOrFormula(
  LakeSetting: TLakeSetting; Index: integer);
var
  Value: double;
  PestItem: string;
  PestSeries: string;
  PestMethod: TPestParamMethod;
  TimeSeriesName: string;
begin
  TimeSeriesName := LakeSetting.TimeSeriesName[Index];
  if TimeSeriesName <> '' then
  begin
    WriteString(' ' + TimeSeriesName);
    Exit;
  end;
  Value := LakeSetting.Value[Index];
  PestItem := LakeSetting.PestName[Index];
  PestSeries := LakeSetting.PestSeries[Index];
  if (PestItem <> '') or (PestSeries <> '') then
  begin
    FPestParamUsed := True;
  end;
  if Model.PestUsed and WritingTemplate and
    ((PestItem <> '') or (PestSeries <> '')) then
  begin
    PestMethod := LakeSetting.PestMethod[Index];
    WritePestTemplateFormula(Value, PestItem, PestSeries, PestMethod, nil);
  end
  else
  begin
    WriteFloat(Value);
  end;
end;

procedure TModflowLAKMf6Writer.WriteLktFile(const AFileName: string;
  SpeciesIndex: Integer);
var
  SpeciesName: string;
  Abbreviation: string;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.ModelSelection = msModflow2015 then
  begin
    Abbreviation := 'LKT6';
  end
  else
  begin
    Exit;
  end;
  if Model.PackageGeneratedExternally(Abbreviation) then
  begin
    Exit;
  end;
  FSpeciesIndex :=  SpeciesIndex;
  SpeciesName := Model.MobileComponents[FSpeciesIndex].Name;
  FNameOfFile := ChangeFileExt(AFileName, '') + '.' + SpeciesName + '.lkt';
  FInputFileName := FNameOfFile;

  WriteToGwtNameFile(Abbreviation, FNameOfFile, SpeciesIndex);

  frmErrorsAndWarnings.BeginUpdate;
  try
    WriteGwtFileInternal;
  finally
    frmErrorsAndWarnings.EndUpdate;
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
//  ACell: TLakeCell;
  CellTop: Real;
  ALakeCell: TLakeCell;
  LakeBottom: Double;
  CellBottom: Double;
  CellAnnotation: string;
  LakeTop: Double;
  Param: TModflowSteadyParameter;
  PestParamName: string;
  DataArray: TDataArray;
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
        Param := Model.GetPestParameterByName(BottomElevation);
        if Param <> nil then
        begin
          Param.IsUsedInTemplate := True;
          BottomElevation := FortranFloatToStr(Param.Value);
          PestParamName := Param.ParameterName;
        end
        else
        begin
          DataArray := Model.DataArrayManager.GetDataSetByName(BottomElevation);
          if (DataArray <> nil) and DataArray.PestParametersUsed then
          begin
            PestParamName := DataArray.Name;
          end
          else
          begin
            PestParamName := '';
          end;
        end;

        ScreenObject.AssignValuesWithCellList(BottomElevation, Model, CellList,
          Results, Annotation, StrLakeBottomElevatio);
        CellAnnotation := Format(StrSAndThenRaised, [Annotation]);
        Assert(ALake.FLakeCellList.Count = Results.Count);
        for CellIndex := 0 to ALake.FLakeCellList.Count - 1 do
        begin
          LakeBottom := Results[CellIndex];
          ALakeCell := ALake.FLakeCellList[CellIndex];
          CellBottom := Model.DiscretiztionElevation[ALakeCell.Cell.Column,
            ALakeCell.Cell.Row, ALakeCell.Cell.Layer +1];
          if (CellBottom > LakeBottom)
            and (ALake.FLakeCellList[CellIndex].LakeType = ltHoriz) then
          begin
            ALake.FLakeCellList[CellIndex].BotElev := CellBottom;
            ALake.FLakeCellList[CellIndex].BotElevAnnotation := CellAnnotation;
            ALake.FLakeCellList[CellIndex].BotElevPestName := '';
          end
          else
          begin
            ALake.FLakeCellList[CellIndex].BotElev := LakeBottom;
            ALake.FLakeCellList[CellIndex].BotElevAnnotation := Annotation;
            ALake.FLakeCellList[CellIndex].BotElevPestName := PestParamName;
          end;
        end;

        TopElevation := LakeBoundary.TopElevation;
        Param := Model.GetPestParameterByName(TopElevation);
        if Param <> nil then
        begin
          Param.IsUsedInTemplate := True;
          TopElevation := FortranFloatToStr(Param.Value);
          PestParamName := Param.ParameterName;
        end
        else
        begin
          DataArray := Model.DataArrayManager.GetDataSetByName(TopElevation);
          if (DataArray <> nil) and DataArray.PestParametersUsed then
          begin
            PestParamName := DataArray.Name;
          end
          else
          begin
            PestParamName := '';
          end;
        end;
        ScreenObject.AssignValuesWithCellList(TopElevation, Model, CellList,
          Results, Annotation, StrLakeTopElevation);
        CellAnnotation := Format(StrSAndThenLowered, [Annotation]);
        Assert(ALake.FLakeCellList.Count = Results.Count);
        for CellIndex := 0 to ALake.FLakeCellList.Count - 1 do
        begin
          LakeTop := Results[CellIndex];
          ALakeCell := ALake.FLakeCellList[CellIndex];
          CellTop := Model.DiscretiztionElevation[ALakeCell.Cell.Column,
            ALakeCell.Cell.Row, ALakeCell.Cell.Layer];

          if (CellTop < LakeTop)
            and (ALake.FLakeCellList[CellIndex].LakeType = ltHoriz) then
          begin
            ALake.FLakeCellList[CellIndex].TopElev := CellTop;
            ALake.FLakeCellList[CellIndex].TopElevAnnotation := CellAnnotation;
            ALake.FLakeCellList[CellIndex].TopElevPestName := '';
          end
          else
          begin
            ALake.FLakeCellList[CellIndex].TopElev := LakeTop;
            ALake.FLakeCellList[CellIndex].TopElevAnnotation := Annotation;
            ALake.FLakeCellList[CellIndex].TopElevPestName := PestParamName;
          end;
        end;

        BedK := LakeBoundary.BedK;
        Param := Model.GetPestParameterByName(BedK);
        if Param <> nil then
        begin
          Param.IsUsedInTemplate := True;
          BedK := FortranFloatToStr(Param.Value);
          PestParamName := Param.ParameterName;
        end
        else
        begin
          DataArray := Model.DataArrayManager.GetDataSetByName(BedK);
          if (DataArray <> nil) and DataArray.PestParametersUsed then
          begin
            PestParamName := DataArray.Name;
          end
          else
          begin
            PestParamName := '';
          end;
        end;

        ScreenObject.AssignValuesWithCellList(BedK, Model, CellList, Results,
          Annotation, StrLakebedHydraulicCo);
        BedKAnnotation := Annotation;
        Assert(ALake.FLakeCellList.Count = Results.Count);
        for CellIndex := 0 to ALake.FLakeCellList.Count - 1 do
        begin
          ALake.FLakeCellList[CellIndex].BedLeakance := Results[CellIndex];
          ALake.FLakeCellList[CellIndex].BedLeakancePestName := PestParamName;
        end;

        BedThickness := LakeBoundary.BedThickness;
        Param := Model.GetPestParameterByName(BedThickness);
        if Param <> nil then
        begin
          Param.IsUsedInTemplate := True;
          BedThickness := FortranFloatToStr(Param.Value);
          PestParamName := Param.ParameterName;
        end
        else
        begin
          DataArray := Model.DataArrayManager.GetDataSetByName(BedThickness);
          if (DataArray <> nil) and DataArray.PestParametersUsed then
          begin
            PestParamName := DataArray.Name;
          end
          else
          begin
            PestParamName := '';
          end;
        end;

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
          ALake.FLakeCellList[CellIndex].BedThicknessPestName := PestParamName;
        end;

        ConnectionLength := LakeBoundary.ConnectionLength;
        Param := Model.GetPestParameterByName(ConnectionLength);
        if Param <> nil then
        begin
          Param.IsUsedInTemplate := True;
          ConnectionLength := FortranFloatToStr(Param.Value);
          PestParamName := Param.ParameterName;
        end
        else
        begin
          DataArray := Model.DataArrayManager.GetDataSetByName(ConnectionLength);
          if (DataArray <> nil) and DataArray.PestParametersUsed then
          begin
            PestParamName := DataArray.Name;
          end
          else
          begin
            PestParamName := '';
          end;
        end;

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
            ALake.FLakeCellList[CellIndex].ConnLengthPestName := '';
          end
          else
          begin
            ALake.FLakeCellList[CellIndex].ConnLength := Results[CellIndex];
            ALake.FLakeCellList[CellIndex].ConnLengthAnnotation := Annotation;
            ALake.FLakeCellList[CellIndex].ConnLengthPestName := PestParamName;
          end;
        end;

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
  PestParamName: string;
  TimeSeries: TMf6TimeSeries;
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
          TimeSeries := Model.Mf6TimesSeries.GetTimeSeriesByName(OutletTimeItem.Rate);
          if TimeSeries = nil then
          begin
            OutletSetting.Rate := EvaluateFormula(OutletTimeItem.Rate,
              Format(StrLakeOutletRateIn,
              [OutletIndex +1, OutletSetting.StartTime]),
              ALake.FScreenObject.Name, PestParamName);
            OutletSetting.RatePestParam := PestParamName;
            OutletSetting.RateTimeSeriesName := '';
          end
          else
          begin
            OutletSetting.Rate := 0;
            OutletSetting.RatePestParam := '';
            OutletSetting.RateTimeSeriesName := String(TimeSeries.SeriesName);
            FTimeSeriesNames.Add(String(TimeSeries.SeriesName));
          end;

          TimeSeries := Model.Mf6TimesSeries.GetTimeSeriesByName(OutletTimeItem.Invert);
          if TimeSeries = nil then
          begin
            OutletSetting.Invert := EvaluateFormula(OutletTimeItem.Invert,
              Format(StrLakeOutletInvertI,
              [OutletIndex +1, OutletSetting.StartTime]),
              ALake.FScreenObject.Name, PestParamName);
            OutletSetting.InvertPestParam := PestParamName;
            OutletSetting.InvertTimeSeriesName := '';
          end
          else
          begin
            OutletSetting.Invert := 0;
            OutletSetting.InvertPestParam := '';
            OutletSetting.InvertTimeSeriesName := String(TimeSeries.SeriesName);
            FTimeSeriesNames.Add(String(TimeSeries.SeriesName));
          end;

          TimeSeries := Model.Mf6TimesSeries.GetTimeSeriesByName(OutletTimeItem.Width);
          if TimeSeries = nil then
          begin
            OutletSetting.Width := EvaluateFormula(OutletTimeItem.Width,
              Format(StrLakeOutletWidthIn,
              [OutletIndex +1, OutletSetting.StartTime]),
              ALake.FScreenObject.Name, PestParamName);
            OutletSetting.WidthPestParam := PestParamName;
            OutletSetting.WidthTimeSeriesName := '';
          end
          else
          begin;
            OutletSetting.Width := 0;
            OutletSetting.WidthPestParam := '';
            OutletSetting.WidthTimeSeriesName := String(TimeSeries.SeriesName);
            FTimeSeriesNames.Add(String(TimeSeries.SeriesName));
          end;

          TimeSeries := Model.Mf6TimesSeries.GetTimeSeriesByName(OutletTimeItem.Slope);
          if TimeSeries = nil then
          begin
            OutletSetting.Slope := EvaluateFormula(OutletTimeItem.Slope,
              Format(StrLakeOutletSlopeIn,
              [OutletIndex +1, OutletSetting.StartTime]),
              ALake.FScreenObject.Name, PestParamName);
            OutletSetting.SlopePestParam := PestParamName;
            OutletSetting.SlopeTimeSeriesName := '';
          end
          else
          begin;
            OutletSetting.Slope := 0;
            OutletSetting.SlopePestParam := '';
            OutletSetting.SlopeTimeSeriesName := String(TimeSeries.SeriesName);
            FTimeSeriesNames.Add(String(TimeSeries.SeriesName));
          end;

          TimeSeries := Model.Mf6TimesSeries.GetTimeSeriesByName(OutletTimeItem.Roughness);
          if TimeSeries = nil then
          begin
            OutletSetting.Rough := EvaluateFormula(OutletTimeItem.Roughness,
              Format(StrLakeOutletRoughnes,
              [OutletIndex +1, OutletSetting.StartTime]),
              ALake.FScreenObject.Name, PestParamName);
            OutletSetting.RoughPestParam := PestParamName;
            OutletSetting.RoughTimeSeriesName := '';
          end
          else
          begin;
            OutletSetting.Rough := 0;
            OutletSetting.RoughPestParam := '';
            OutletSetting.RoughTimeSeriesName := String(TimeSeries.SeriesName);
            FTimeSeriesNames.Add(String(TimeSeries.SeriesName));
          end;
        end;
      end;
    end;
  end;
end;

function TModflowLAKMf6Writer.EvaluateFormula(Formula: string;
  const DataIdentifier, ScreenObjectName: string;
  out PestParamName: string): double;
var
  Compiler: TRbwParser;
  ErrorFunction: string;
  Expression: TExpression;
  ErrorMessage: string;
  ResultTypeOK: Boolean;
  Param: TModflowSteadyParameter;
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
  Param := Model.GetPestParameterByName(Formula);
  if Param <> nil then
  begin
    Param.IsUsedInTemplate := True;
    Formula := FortranFloatToStr(Param.Value);
    PestParamName := Param.ParameterName;
  end
  else
  begin
    PestParamName := '';
  end;

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
  PestName: string;
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
        ALake.FScreenObject.Name, PestName);
      ALakeTableRow.StagePestName := PestName;

      Formula := TableItem.Volume;
      DataIdentifier := StrLakeVolume;
      ALakeTableRow.Volume := EvaluateFormula(Formula, DataIdentifier,
        ALake.FScreenObject.Name, PestName);
      ALakeTableRow.VolumePestName := PestName;

      Formula := TableItem.SurfaceArea;
      DataIdentifier := StrLakeSurfaceArea;
      ALakeTableRow.SurfaceArea := EvaluateFormula(Formula, DataIdentifier,
        ALake.FScreenObject.Name, PestName);
      ALakeTableRow.SurfaceAreaPestName := PestName;

      Formula := TableItem.ExchangeArea;
      if Formula = '' then
      begin
        ALakeTableRow.ExchangeArea := 0;
      ALakeTableRow.ExchangeAreaPestName := '';
      end
      else
      begin
        DataIdentifier := StrLakeExchangeArea;
        ALakeTableRow.ExchangeArea := EvaluateFormula(Formula, DataIdentifier,
          ALake.FScreenObject.Name, PestName);
        ALakeTableRow.ExchangeAreaPestName := PestName;
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
  SpeciesIndex: Integer;
  GwtFormulaIndex: Integer;
//  FormulaIndex: Integer;
  procedure AssignValue(DataSetIdentifier: Integer; const FormatString: string);
  var
    Formula: string;
    Param: TModflowSteadyParameter;
    Modifier: string;
    Method: TPestParamMethod;
    DummyVariable: string;
    TimeSeries: TMf6TimeSeries;
  begin
    Formula := LakeItem.BoundaryFormula[DataSetIdentifier];
    TimeSeries := Model.Mf6TimesSeries.GetTimeSeriesByName(Formula);
    if TimeSeries = nil then
    begin
      LakeSetting.TimeSeriesName[DataSetIdentifier] := '';
      Param := Model.GetPestParameterByName(Formula);
      if Param <> nil then
      begin
        Param.IsUsedInTemplate := True;
        LakeSetting.PestName[DataSetIdentifier] := Param.ParameterName;
        Formula := FortranFloatToStr(Param.Value);
      end
      else
      begin
        LakeSetting.PestName[DataSetIdentifier] := '';
      end;
      LakeSetting.Value[DataSetIdentifier] := EvaluateFormula(Formula,
        Format(FormatString,
        [LakeSetting.StartTime]),
        ALake.FScreenObject.Name, DummyVariable);
      Modifier := LakeBoundary.PestBoundaryFormula[DataSetIdentifier];
      Param := Model.GetPestParameterByName(Modifier);
      if Param <> nil then
      begin
        Param.IsUsedInTemplate := True;
        LakeSetting.PestSeries[DataSetIdentifier] := Param.ParameterName;
        Method := LakeBoundary.PestBoundaryMethod[DataSetIdentifier];
        LakeSetting.PestMethod[DataSetIdentifier] := Method;
        case Method of
          ppmMultiply:
            begin
              LakeSetting.Value[DataSetIdentifier] :=
                LakeSetting.Value[DataSetIdentifier] * Param.Value;
            end;
          ppmAdd:
            begin
              LakeSetting.Value[DataSetIdentifier] :=
                LakeSetting.Value[DataSetIdentifier] + Param.Value;
            end;
          else
            begin
              Assert(False);
            end;
        end;
      end
      else
      begin
        LakeSetting.PestSeries[DataSetIdentifier] := ''
      end;
    end
    else
    begin
      LakeSetting.PestName[DataSetIdentifier] := '';
      LakeSetting.PestSeries[DataSetIdentifier] := '';
      LakeSetting.Value[DataSetIdentifier] := 0;
      LakeSetting.TimeSeriesName[DataSetIdentifier] := String(TimeSeries.SeriesName);
      FTimeSeriesNames.Add(String(TimeSeries.SeriesName));
    end;
  end;
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
      if Model.GwtUsed then
      begin
        SetLength(LakeSetting.GwtStatus, Model.MobileComponents.Count);
        while LakeItem.GwtStatus.Count < Model.MobileComponents.Count do
        begin
          LakeItem.GwtStatus.Add;
        end;
        for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
        begin
          LakeSetting.GwtStatus[SpeciesIndex]
            := LakeItem.GwtStatus[SpeciesIndex].GwtBoundaryStatus;
        end;
      end;

      AssignValue(Lak6StagePosition, StrLakeStageAt0g);
      AssignValue(Lak6RainfallPosition, StrLakeRainfallAt0);
      AssignValue(Lak6EvaporationPosition, StrLakeEvaporationAt);
      if LakeSetting.Evaporation < 0 then
      begin
        frmErrorsAndWarnings.AddError(Model, StrLakeEtError,
          Format(StrTheLakeEvaporation,
          [LakeItem.StartTime, ALake.FScreenObject]), ALake.FScreenObject)
      end;

      AssignValue(Lak6RunoffPosition, StrLakeRunoffAt0g);
      AssignValue(Lak6InflowPosition, StrLakeInflowAt0g);
      AssignValue(Lak6WithdrawalPosition, StrLakeWithdrawalAt);

      if Model.GwtUsed then
      begin
        for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
        begin
          AssignValue(Lak6GwtPestStartPosition + SpeciesIndex,
            Format(StrLakeChemSpeciesD, [SpeciesIndex+1]) + ' at %0:g');
        end;
      end;
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
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoOutletProperties);

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

procedure TModflowLAKMf6Writer.WriteFileInternal;
begin
  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingLAK6Package);

    WriteTemplateHeader;

    frmProgressMM.AddMessage(StrWritingDataSet0);
    WriteDataSet0;

    WriteOptions;

    WriteDimensions;

    WritePackageData;

    WriteConnectionData;

    WriteLakeTableNames;

    WriteOutlets;

    WriteStressPeriods;
  finally
    CloseFile;
  end;
end;

procedure TModflowLAKMf6Writer.WriteGwtFileInternal;
begin
  OpenFile(FNameOfFile);
  try
    WriteTemplateHeader;

    WriteDataSet0;

    WriteGwtOptions;
    WriteGwtPackageData;
    WriteGwtStressPeriods;

  finally
    CloseFile;
  end;
end;

procedure TModflowLAKMf6Writer.WriteGwtOptions;
var
  ASpecies: TMobileChemSpeciesItem;
  budgetfile: string;
  BaseFileName: string;
  SfrMf6Package: TSfrModflow6PackageSelection;
  concentrationfile: string;
  budgetCsvFile: string;
begin
  WriteBeginOptions;
  try
    WriteString('    FLOW_PACKAGE_NAME ');
    WriteString(StrLakeFlowPackageName);
    NewLine;

    Assert(FSpeciesIndex >= 0);
    Assert(FSpeciesIndex < Model.MobileComponents.Count);
    WriteString('    FLOW_PACKAGE_AUXILIARY_NAME ');
    ASpecies := Model.MobileComponents[FSpeciesIndex];
    WriteString(' ' + ASpecies.Name);
    NewLine;

    WriteString('    BOUNDNAMES');
    NewLine;

    PrintListInputOption;
    PrintConcentrationOption;
    PrintFlowsOption;
    WriteSaveFlowsOption;

    SfrMf6Package := Model.ModflowPackages.SfrModflow6Package;
    BaseFileName := ChangeFileExt(FNameOfFile, '');
    BaseFileName := ChangeFileExt(BaseFileName, '') + '.' + ASpecies.Name;

    if SfrMf6Package.SaveGwtConcentration then
    begin
      WriteString('    CONCENTRATION FILEOUT ');
      concentrationfile := BaseFileName + '.lkt_conc';
      Model.AddModelOutputFile(concentrationfile);
      concentrationfile := ExtractFileName(concentrationfile);
      WriteString(concentrationfile);
      NewLine;
    end;

    if SfrMf6Package.SaveGwtBudget then
    begin
      WriteString('    BUDGET FILEOUT ');
      budgetfile := BaseFileName + '.lkt_budget';
      Model.AddModelOutputFile(budgetfile);
      budgetfile := ExtractFileName(budgetfile);
      WriteString(budgetfile);
      NewLine;
    end;

    if SfrMf6Package.SaveGwtBudgetCsv then
    begin
      WriteString('    BUDGETCSV FILEOUT ');
      budgetCsvFile := BaseFileName + '.lkt_budget.csv';
      Model.AddModelOutputFile(budgetCsvFile);
      budgetCsvFile := ExtractFileName(budgetCsvFile);
      WriteString(budgetCsvFile);
      NewLine;
    end;

  //  [TS6 FILEIN <ts6_filename>]
  //  [OBS6 FILEIN <obs6_filename>]
  finally
    WriteEndOptions
  end;

end;

procedure TModflowLAKMf6Writer.WriteGwtPackageData;
var
  LakeIndex: Integer;
  ALake: TLake;
  BoundName: string;
  SpeciesIndex: Integer;
begin
  WriteBeginPackageData;
  WriteString('# <lakeno> <strt> <boundname>');
  NewLine;
  for LakeIndex := 0 to FLakes.Count - 1 do
  begin
    ALake := FLakes[LakeIndex];
    WriteString('  ');
    WriteInteger(LakeIndex+1);

    WriteFormulaOrValueBasedOnAPestName(ALake.FStartingConcentrations.ConcentrationPestNames[FSpeciesIndex],
      ALake.FStartingConcentrations.Concentrations[FSpeciesIndex], -1, -1, -1);
//    WriteFloat(ALake.FStartingStage);

    if Model.GwtUsed then
    begin
      for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
      begin
        WriteFloat(0);
      end;
    end;

    // aux
    BoundName := Copy(ALake.FScreenObject.Name, 1, MaxBoundNameLength);
    BoundName := ' ''' + BoundName + ''' ';
    WriteString(BoundName);
    NewLine;
  end;
  WriteEndPackageData
end;

procedure TModflowLAKMf6Writer.WriteGwtStressPeriods;
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
  SpeciesIndex: Integer;
  ASpecies: TMobileChemSpeciesItem;
  GwtStatus: TGwtBoundaryStatus;
begin
//  if MvrWriter <> nil then
//  begin
//    MoverWriter := MvrWriter as TModflowMvrWriter;
//  end
//  else
//  begin
//    MoverWriter := nil;
//  end;
//  MvrReceiver.ReceiverKey.ReceiverPackage := rpcLak;
//  MvrReceiver.ReceiverValues.StreamCells := nil;

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
        end;
      end;
    end;

    for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
    begin
      StressPeriodStartTimes.Add(Model.ModflowFullStressPeriods[TimeIndex].StartTime);
    end;
    StressPeriodStartTimes.Sorted := True;

//    for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
//    begin
//      MvrReceiver.ReceiverKey.StressPeriod := TimeIndex;
//      for LakeIndex := 0 to FLakes.Count - 1 do
//      begin
//        ALake := FLakes[LakeIndex];
//        Assert(ALake.FScreenObject <> nil);
//        MvrReceiver.ReceiverValues.Index := LakeIndex+1;
//        MvrReceiver.ReceiverKey.ScreenObject := ALake.FScreenObject;
//          if (MoverWriter <> nil) and not WritingTemplate then
//          begin
//            MoverWriter.AddMvrReceiver(MvrReceiver);
//          end;
//      end;
//    end;

    for TimeIndex := 0 to AllTimes.Count - 1 do
    begin
      ATime := AllTimes[TimeIndex];
      StressPeriodIndex := StressPeriodStartTimes.IndexOf(ATime);
      Assert(StressPeriodIndex >= 0);
//      MvrRegSourceKey.StressPeriod := StressPeriodIndex;

      WriteString('BEGIN PERIOD ');
      WriteInteger(StressPeriodIndex + 1);
      NewLine;

      OutletNumber := 0;
      for LakeIndex := 0 to FLakes.Count - 1 do
      begin
        ALake := FLakes[LakeIndex];
        Assert(ALake.FScreenObject <> nil);
//        MvrRegSourceKey.SourceKey.ScreenObject := ALake.FScreenObject;
        LocalScreenObject := ALake.FScreenObject as TScreenObject;
//        MvrUsed := (MoverWriter <> nil)
//          and (LocalScreenObject.ModflowMvr <> nil)
//          and LocalScreenObject.ModflowMvr.Used
//          and (LocalScreenObject.ModflowMvr.SourcePackageChoice = spcLak);
//        if MvrUsed then
//        begin
//          UsedOutlets.Clear;
//          for OutletIndex := 0 to LocalScreenObject.ModflowMvr.Receivers.Count - 1 do
//          begin
//            ReceiverItem := LocalScreenObject.ModflowMvr.Receivers[OutletIndex];
//            UsedOutlets.Add(ReceiverItem.LakeOutlet);
//          end;
//        end;

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
            GwtStatus := gbsActive;
            case ALakeSetting.Status of
              lsInactive:
                begin
                  GwtStatus := gbsInactive;
                end;
              lsActive, lsConstant:
                begin
                  GwtStatus := ALakeSetting.GwtStatus[FSpeciesIndex];
                end;
              else
                Assert(False);
            end;

            WriteString(' STATUS');
            case GwtStatus of
              gbsInactive:
                begin
                  WriteString(' INACTIVE');
                end;
              gbsActive:
                begin
                  WriteString(' ACTIVE');
                end;
              gbsConstant:
                begin
                  WriteString(' CONSTANT');
                end;
              else
                Assert(False);
            end;
            NewLine;

            if GwtStatus = gbsConstant then
            begin
              WriteInteger(LakeIndex+1);
              WriteString(' CONCENTRATION');
              Assert(False);

//              FormulaIndex := SfrMf6DiversionStartPosition + 1 + DiversionCount
//                + GwtConcCount*GwtSpecifiedConcentrationPosition + FSpeciesIndex;
//              WriteValueOrFormula(ACell, FormulaIndex);
              NewLine;
            end;

            if GwtStatus = gbsActive then
            begin
              WriteInteger(LakeIndex+1);
              WriteString(' RAINFALL');
//              FormulaIndex := SfrMf6DiversionStartPosition + 1 + DiversionCount
//                + GwtConcCount*GwtRainfallConcentrationsPosition + FSpeciesIndex;
//              WriteValueOrFormula(ACell, FormulaIndex);
              NewLine;

              WriteInteger(LakeIndex+1);
              WriteString(' EVAPORATION');
//              FormulaIndex := SfrMf6DiversionStartPosition + 1 + DiversionCount
//                + GwtConcCount*GwtEvapConcentrationsPosition + FSpeciesIndex;
//              WriteValueOrFormula(ACell, FormulaIndex);
              NewLine;

              WriteInteger(LakeIndex+1);
              WriteString(' RUNOFF');
//              FormulaIndex := SfrMf6DiversionStartPosition + 1 + DiversionCount
//                + GwtConcCount*GwtRunoffConcentrationsPosition + FSpeciesIndex;
//              WriteValueOrFormula(ACell, FormulaIndex);
              NewLine;

              WriteInteger(LakeIndex+1);
              WriteString(' EXT-INFLOW');
//              FormulaIndex := SfrMf6DiversionStartPosition + 1 + DiversionCount
//                + GwtConcCount*GwtInflowConcentrationsPosition + FSpeciesIndex;
//              WriteValueOrFormula(ACell, FormulaIndex);
              NewLine;
            end;

          end;
        end;

//        for OutletIndex := 0 to ALake.FLakeOutlets.Count - 1 do
//        begin
//          AnOutlet := ALake.FLakeOutlets[OutletIndex];
//          Inc(OutletNumber);
//
//          MvrRegSourceKey.Index := OutletNumber;
//          MvrRegSourceKey.SourceKey.MvrIndex := OutletIndex;
//
//          if AnOutlet.FStartingTimeIndex < AnOutlet.Count then
//          begin
//            OutletSetting := AnOutlet[AnOutlet.FStartingTimeIndex];
//            while OutletSetting.StartTime < ATime do
//            begin
//              Inc(AnOutlet.FStartingTimeIndex);
//              if AnOutlet.FStartingTimeIndex < AnOutlet.Count then
//              begin
//                OutletSetting := AnOutlet[AnOutlet.FStartingTimeIndex];
//              end
//              else
//              begin
//                break;
//              end;
//            end;
//            if OutletSetting.StartTime = ATime then
//            begin
//              WriteString('  ');
//              WriteInteger(OutletNumber);
//              WriteString('  RATE');
//              if OutletSetting.RateTimeSeriesName = '' then
//              begin
//                WriteFormulaOrValueBasedOnAPestName(OutletSetting.RatePestParam,
//                  OutletSetting.Rate, -1, -1, -1);
//              end
//              else
//              begin
//                WriteString(' ' + OutletSetting.RateTimeSeriesName);
//              end;
//              NewLine;
//
//              WriteString('  ');
//              WriteInteger(OutletNumber);
//              WriteString('  INVERT');
//              if OutletSetting.InvertTimeSeriesName = '' then
//              begin
//                WriteFormulaOrValueBasedOnAPestName(OutletSetting.InvertPestParam,
//                  OutletSetting.Invert, -1, -1, -1);
//              end
//              else
//              begin
//                WriteString(' ' + OutletSetting.InvertTimeSeriesName);
//              end;
//              NewLine;
//
//              WriteString('  ');
//              WriteInteger(OutletNumber);
//              WriteString('  WIDTH');
//              if OutletSetting.WidthTimeSeriesName = '' then
//              begin
//                WriteFormulaOrValueBasedOnAPestName(OutletSetting.WidthPestParam,
//                  OutletSetting.Width, -1, -1, -1);
//              end
//              else
//              begin
//                WriteString(' ' + OutletSetting.WidthTimeSeriesName);
//              end;
//              NewLine;
//
//              WriteString('  ');
//              WriteInteger(OutletNumber);
//              WriteString('  SLOPE');
//              if OutletSetting.SlopeTimeSeriesName = '' then
//              begin
//                WriteFormulaOrValueBasedOnAPestName(OutletSetting.SlopePestParam,
//                  OutletSetting.Slope, -1, -1, -1);
//              end
//              else
//              begin
//                WriteString(' ' + OutletSetting.SlopeTimeSeriesName);
//              end;
////              WriteFloat(OutletSetting.Slope);
//              NewLine;
//
//              WriteString('  ');
//              WriteInteger(OutletNumber);
//              WriteString('  ROUGH');
//              if OutletSetting.RoughTimeSeriesName = '' then
//              begin
//                WriteFormulaOrValueBasedOnAPestName(OutletSetting.RoughPestParam,
//                  OutletSetting.Rough, -1, -1, -1);
//              end
//              else
//              begin
//                WriteString(' ' + OutletSetting.RoughTimeSeriesName);
//              end;
////              WriteFloat(OutletSetting.Rough);
//              NewLine;
//
//              if MvrUsed and (UsedOutlets.IndexOf(OutletIndex+1) >= 0 ) and not WritingTemplate then
//              begin
//                TModflowMvrWriter(MvrWriter).AddMvrSource(MvrRegSourceKey);
//              end;
//
//            end;
//          end;
//        end;
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
  PestParameterName: string;
  SpeciesIndex: Integer;
  Item: TStringConcValueItem;
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
          StrLakeStartingStage, ALake.FScreenObject.Name, PestParameterName);
        ALake.FStartingStagePestName := PestParameterName;

        if Model.GwtUsed then
        begin
          ALake.FStartingConcentrations.SpeciesCount :=
            Model.MobileComponents.Count;
          While LakeBoundary.StartingConcentrations.Count <
            Model.MobileComponents.Count do
          begin
            Item := LakeBoundary.StartingConcentrations.Add;
            Item.Value := '0';
            Item.Name := Model.MobileComponents[
              LakeBoundary.StartingConcentrations.Count-1].Name;
          end;
          While LakeBoundary.StartingConcentrationPestNames.Count <
            Model.MobileComponents.Count do
          begin
            LakeBoundary.StartingConcentrationPestNames.Add('');
          end;
        end;

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
//              if lctHorizontal in LakeBoundary.LakeConnections then
//              begin
                if IDomainArray.IntegerData[ACell.Layer + 1, ACell.Row, ACell.Column] > 0 then
                begin
                  ALakeCell := TLakeCell.Create;
                  ALake.FLakeCellList.Add(ALakeCell);
                  ALakeCell.Cell := ACell.Cell;
                  ALakeCell.Cell.Layer := ALakeCell.Cell.Layer + 1;
                  ALakeCell.LakeType := ltVert;
                  ALakeCell.LakeCell := ACell.Cell;
                end;
//              end
//              else
//              begin
//                if IDomainArray.IntegerData[ACell.Layer, ACell.Row, ACell.Column] > 0 then
//                begin
//                  ALakeCell := TLakeCell.Create;
//                  ALake.FLakeCellList.Add(ALakeCell);
//                  ALakeCell.Cell := ACell.Cell;
//                  ALakeCell.LakeType := ltVert;
//                  ALakeCell.LakeCell := ACell.Cell;
//                end;
//              end;
            end;
          end;
        end;

        if IsMf6Observation(ScreenObject) then
        begin
          MfObs := ScreenObject.Modflow6Obs;
          Obs.FName := MfObs.Name;
          Obs.FBoundName := ScreenObject.Name;
          Obs.FObsTypes := MfObs.LakObs;
          Obs.FModflow6Obs := MfObs;
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
  CsvFile: string;
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

  WriteTimeSeriesFiles(FInputFileName);

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

  if FLakMf6Package.WriteConvergenceData then
  begin
    WriteString('  PACKAGE_CONVERGENCE FILEOUT ');
    CsvFile := ChangeFileExt(FFileName, '.LakConvergence.CSV');
    Model.AddModelOutputFile(CsvFile);
    CsvFile := ExtractFileName(CsvFile);
    WriteString(CsvFile);
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

  WriteAdditionalAuxVariables;

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

        if AnOutlet.Count = 0 then
        begin
          frmErrorsAndWarnings.AddError(Model, StrNoOutletProperties,
            Format(StrInTheLakeDefinedProperties,
            [ALake.FScreenObject.Name, OutletIndex+1]), ALake.FScreenObject);
          NewLine;
          Continue;

        end;

        ASetting := AnOutlet[0];

        if ASetting.InvertTimeSeriesName = '' then
        begin
          WriteFormulaOrValueBasedOnAPestName(ASetting.InvertPestParam,
            ASetting.Invert, -1, -1, -1);
        end
        else
        begin
          WriteString(' ' + ASetting.InvertTimeSeriesName);
        end;

        if ASetting.WidthTimeSeriesName = '' then
        begin
          WriteFormulaOrValueBasedOnAPestName(ASetting.WidthPestParam,
            ASetting.Width, -1, -1, -1);
        end
        else
        begin
          WriteString(' ' + ASetting.WidthTimeSeriesName);
        end;

        if ASetting.RoughTimeSeriesName = '' then
        begin
          WriteFormulaOrValueBasedOnAPestName(ASetting.RoughPestParam,
            ASetting.Rough, -1, -1, -1);
        end
        else
        begin
          WriteString(' ' + ASetting.RoughTimeSeriesName);
        end;

        if ASetting.SlopeTimeSeriesName = '' then
        begin
          WriteFormulaOrValueBasedOnAPestName(ASetting.SlopePestParam,
            ASetting.Slope, -1, -1, -1);
        end
        else
        begin
          WriteString(' ' + ASetting.SlopeTimeSeriesName);
        end;

//        WriteFormulaOrValueBasedOnAPestName(ASetting.InvertPestParam,
//          ASetting.Invert, -1, -1, -1);
//        WriteFormulaOrValueBasedOnAPestName(ASetting.WidthPestParam,
//          ASetting.Width, -1, -1, -1);
//        WriteFormulaOrValueBasedOnAPestName(ASetting.RoughPestParam,
//          ASetting.Rough, -1, -1, -1);
//        WriteFormulaOrValueBasedOnAPestName(ASetting.SlopePestParam,
//          ASetting.Slope, -1, -1, -1);

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
  SpeciesIndex: Integer;
begin
  WriteBeginPackageData;
  WriteString('# <lakeno> <strt> <nlakeconn> [<aux(naux)>] [<boundname>]');
  NewLine;
  for LakeIndex := 0 to FLakes.Count - 1 do
  begin
    ALake := FLakes[LakeIndex];
    WriteString('  ');
    WriteInteger(LakeIndex+1);

    WriteFormulaOrValueBasedOnAPestName(ALake.FStartingStagePestName,
      ALake.FStartingStage, -1, -1, -1);
//    WriteFloat(ALake.FStartingStage);

    WriteInteger(ALake.FLakeCellList.Count);

    if Model.GwtUsed then
    begin
      for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
      begin
        WriteFloat(0);
      end;
    end;

    // aux
    BoundName := Copy(ALake.FScreenObject.Name, 1, MaxBoundNameLength);
    BoundName := ' ''' + BoundName + ''' ';
    WriteString(BoundName);
    NewLine;
  end;
  WriteEndPackageData
end;

procedure TModflowLAKMf6Writer.WriteStressPeriods;
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
  SpeciesIndex: Integer;
  ASpecies: TMobileChemSpeciesItem;
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
        end;
      end;
    end;

    for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
    begin
      StressPeriodStartTimes.Add(Model.ModflowFullStressPeriods[TimeIndex].StartTime);
    end;
    StressPeriodStartTimes.Sorted := True;

    for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
    begin
      MvrReceiver.ReceiverKey.StressPeriod := TimeIndex;
      for LakeIndex := 0 to FLakes.Count - 1 do
      begin
        ALake := FLakes[LakeIndex];
        Assert(ALake.FScreenObject <> nil);
        MvrReceiver.ReceiverValues.Index := LakeIndex+1;
        MvrReceiver.ReceiverKey.ScreenObject := ALake.FScreenObject;
          if (MoverWriter <> nil) and not WritingTemplate then
          begin
            MoverWriter.AddMvrReceiver(MvrReceiver);
          end;
      end;
    end;

    for TimeIndex := 0 to AllTimes.Count - 1 do
    begin
      ATime := AllTimes[TimeIndex];
      StressPeriodIndex := StressPeriodStartTimes.IndexOf(ATime);
      Assert(StressPeriodIndex >= 0);
      MvrRegSourceKey.StressPeriod := StressPeriodIndex;

      WriteString('BEGIN PERIOD ');
      WriteInteger(StressPeriodIndex + 1);
      NewLine;

      OutletNumber := 0;
      for LakeIndex := 0 to FLakes.Count - 1 do
      begin
        ALake := FLakes[LakeIndex];
        Assert(ALake.FScreenObject <> nil);
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
              WriteLakeValueOrFormula(ALakeSetting, Lak6StagePosition);
              NewLine;
            end;

            WriteString('  ');
            WriteInteger(LakeIndex+1);
            WriteString('  RAINFALL');
            WriteLakeValueOrFormula(ALakeSetting, Lak6RainfallPosition);
            NewLine;

            WriteString('  ');
            WriteInteger(LakeIndex+1);
            WriteString('  EVAPORATION');
            WriteLakeValueOrFormula(ALakeSetting, Lak6EvaporationPosition);
            NewLine;

            WriteString('  ');
            WriteInteger(LakeIndex+1);
            WriteString('  RUNOFF');
            WriteLakeValueOrFormula(ALakeSetting, Lak6RunoffPosition);
            NewLine;

            WriteString('  ');
            WriteInteger(LakeIndex+1);
            WriteString('  INFLOW');
            WriteLakeValueOrFormula(ALakeSetting, Lak6InflowPosition);
            NewLine;

            WriteString('  ');
            WriteInteger(LakeIndex+1);
            WriteString('  WITHDRAWAL');
            WriteLakeValueOrFormula(ALakeSetting, Lak6WithdrawalPosition);
            NewLine;

            if Model.GwtUsed then
            begin
              for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
              begin
                WriteString('  ');
                WriteInteger(LakeIndex+1);
                WriteString(' AUXILIARY ');
                ASpecies := Model.MobileComponents[SpeciesIndex];
                WriteString(' ' + ASpecies.Name);
                WriteFloat(0);
              end;
            end;

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
              if OutletSetting.RateTimeSeriesName = '' then
              begin
                WriteFormulaOrValueBasedOnAPestName(OutletSetting.RatePestParam,
                  OutletSetting.Rate, -1, -1, -1);
              end
              else
              begin
                WriteString(' ' + OutletSetting.RateTimeSeriesName);
              end;
              NewLine;

              WriteString('  ');
              WriteInteger(OutletNumber);
              WriteString('  INVERT');
              if OutletSetting.InvertTimeSeriesName = '' then
              begin
                WriteFormulaOrValueBasedOnAPestName(OutletSetting.InvertPestParam,
                  OutletSetting.Invert, -1, -1, -1);
              end
              else
              begin
                WriteString(' ' + OutletSetting.InvertTimeSeriesName);
              end;
              NewLine;

              WriteString('  ');
              WriteInteger(OutletNumber);
              WriteString('  WIDTH');
              if OutletSetting.WidthTimeSeriesName = '' then
              begin
                WriteFormulaOrValueBasedOnAPestName(OutletSetting.WidthPestParam,
                  OutletSetting.Width, -1, -1, -1);
              end
              else
              begin
                WriteString(' ' + OutletSetting.WidthTimeSeriesName);
              end;
              NewLine;

              WriteString('  ');
              WriteInteger(OutletNumber);
              WriteString('  SLOPE');
              if OutletSetting.SlopeTimeSeriesName = '' then
              begin
                WriteFormulaOrValueBasedOnAPestName(OutletSetting.SlopePestParam,
                  OutletSetting.Slope, -1, -1, -1);
              end
              else
              begin
                WriteString(' ' + OutletSetting.SlopeTimeSeriesName);
              end;
//              WriteFloat(OutletSetting.Slope);
              NewLine;

              WriteString('  ');
              WriteInteger(OutletNumber);
              WriteString('  ROUGH');
              if OutletSetting.RoughTimeSeriesName = '' then
              begin
                WriteFormulaOrValueBasedOnAPestName(OutletSetting.RoughPestParam,
                  OutletSetting.Rough, -1, -1, -1);
              end
              else
              begin
                WriteString(' ' + OutletSetting.RoughTimeSeriesName);
              end;
//              WriteFloat(OutletSetting.Rough);
              NewLine;

              if MvrUsed and (UsedOutlets.IndexOf(OutletIndex+1) >= 0 ) and not WritingTemplate then
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

{ TLakeSetting }

function TLakeSetting.GetPestMethod(Index: Integer): TPestParamMethod;
var
  SpeciesCount: Integer;
begin
  case Index of
    Lak6StagePosition:
      begin
        result := StagePestMethod;
      end;
    Lak6RainfallPosition:
      begin
        result := RainfallPestMethod;
      end;
    Lak6RunoffPosition:
      begin
        result := RunoffPestMethod;
      end;
    Lak6EvaporationPosition:
      begin
        result := EvaporationPestMethod;
      end;
    Lak6InflowPosition:
      begin
        result := InflowPestMethod;
      end;
    Lak6WithdrawalPosition:
      begin
        result :=  WithdrawalPestMethod;
      end;
    else
    begin
      if frmGoPhast.PhastModel.GwtUsed then
      begin
        SpeciesCount := frmGoPhast.PhastModel.MobileComponents.Count;
        Index := Index - Lak6GwtPestStartPosition;
        if Index < Length(SpecifiedConcentrations.ConcentrationPestSeriesMethods) then
        begin
          result := SpecifiedConcentrations.ConcentrationPestSeriesMethods[Index];
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        if Index < Length(RainfallConcentrations.ConcentrationPestSeriesMethods) then
        begin
          result := RainfallConcentrations.ConcentrationPestSeriesMethods[Index];
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        if Index < Length(EvapConcentrations.ConcentrationPestSeriesMethods) then
        begin
          result := EvapConcentrations.ConcentrationPestSeriesMethods[Index];
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        if Index < Length(RunoffConcentrations.ConcentrationPestSeriesMethods) then
        begin
          result := RunoffConcentrations.ConcentrationPestSeriesMethods[Index];
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        if Index < Length(InflowConcentrations.ConcentrationPestSeriesMethods) then
        begin
          result := InflowConcentrations.ConcentrationPestSeriesMethods[Index];
          Exit;
        end;
        result := ppmMultiply;
        Assert(False);
      end
      else
      begin
        result := ppmMultiply;
        Assert(False);
      end;
    end;
  end;
end;

function TLakeSetting.GetPestName(Index: Integer): string;
var
  SpeciesCount: Integer;
begin
  case Index of
    Lak6StagePosition:
      begin
        result := StagePest;
      end;
    Lak6RainfallPosition:
      begin
        result := RainfallPest;
      end;
    Lak6RunoffPosition:
      begin
        result := RunoffPest;
      end;
    Lak6EvaporationPosition:
      begin
        result := EvaporationPest;
      end;
    Lak6InflowPosition:
      begin
        result := InflowPest;
      end;
    Lak6WithdrawalPosition:
      begin
        result :=  WithdrawalPest;
      end;
    else
    begin
      if frmGoPhast.PhastModel.GwtUsed then
      begin
        SpeciesCount := frmGoPhast.PhastModel.MobileComponents.Count;
        Index := Index - Lak6GwtPestStartPosition;
        if Index < Length(SpecifiedConcentrations.ConcentrationPestNames) then
        begin
          result := SpecifiedConcentrations.ConcentrationPestNames[Index];
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        if Index < Length(RainfallConcentrations.ConcentrationPestNames) then
        begin
          result := RainfallConcentrations.ConcentrationPestNames[Index];
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        if Index < Length(EvapConcentrations.ConcentrationPestNames) then
        begin
          result := EvapConcentrations.ConcentrationPestNames[Index];
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        if Index < Length(RunoffConcentrations.ConcentrationPestNames) then
        begin
          result := RunoffConcentrations.ConcentrationPestNames[Index];
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        if Index < Length(InflowConcentrations.ConcentrationPestNames) then
        begin
          result := InflowConcentrations.ConcentrationPestNames[Index];
          Exit;
        end;
        result := '';
        Assert(False);
      end
      else
      begin
        result := '';
        Assert(False);
      end;
    end;
  end;
end;

function TLakeSetting.GetPestSeries(Index: Integer): string;
var
  SpeciesCount: Integer;
begin
  case Index of
    Lak6StagePosition:
      begin
        result := StagePestSeries;
      end;
    Lak6RainfallPosition:
      begin
        result := RainfallPestSeries;
      end;
    Lak6RunoffPosition:
      begin
        result := RunoffPestSeries;
      end;
    Lak6EvaporationPosition:
      begin
        result := EvaporationPestSeries;
      end;
    Lak6InflowPosition:
      begin
        result := InflowPestSeries;
      end;
    Lak6WithdrawalPosition:
      begin
        result :=  WithdrawalPestSeries;
      end;
    else
    begin
      if frmGoPhast.PhastModel.GwtUsed then
      begin
        SpeciesCount := frmGoPhast.PhastModel.MobileComponents.Count;
        Index := Index - Lak6GwtPestStartPosition;
        if Index < Length(SpecifiedConcentrations.ConcentrationPestSeriesNames) then
        begin
          result := SpecifiedConcentrations.ConcentrationPestSeriesNames[Index];
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        if Index < Length(RainfallConcentrations.ConcentrationPestSeriesNames) then
        begin
          result := RainfallConcentrations.ConcentrationPestSeriesNames[Index];
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        if Index < Length(EvapConcentrations.ConcentrationPestSeriesNames) then
        begin
          result := EvapConcentrations.ConcentrationPestSeriesNames[Index];
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        if Index < Length(RunoffConcentrations.ConcentrationPestSeriesNames) then
        begin
          result := RunoffConcentrations.ConcentrationPestSeriesNames[Index];
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        if Index < Length(InflowConcentrations.ConcentrationPestSeriesNames) then
        begin
          result := InflowConcentrations.ConcentrationPestSeriesNames[Index];
          Exit;
        end;
        result := '';
        Assert(False);
      end
      else
      begin
        result := '';
        Assert(False);
      end;
    end;
  end;
end;

function TLakeSetting.GetTimeSeriesName(Index: Integer): string;
var
  SpeciesCount: Integer;
begin
  case Index of
    Lak6StagePosition:
      begin
        result := StageTimeSeriesName;
      end;
    Lak6RainfallPosition:
      begin
        result := RainfallTimeSeriesName;
      end;
    Lak6RunoffPosition:
      begin
        result := RunoffTimeSeriesName;
      end;
    Lak6EvaporationPosition:
      begin
        result := EvaporationTimeSeriesName;
      end;
    Lak6InflowPosition:
      begin
        result := InflowTimeSeriesName;
      end;
    Lak6WithdrawalPosition:
      begin
        result :=  WithdrawalTimeSeriesName;
      end;
    else
    begin
      if frmGoPhast.PhastModel.GwtUsed then
      begin
        SpeciesCount := frmGoPhast.PhastModel.MobileComponents.Count;
        Index := Index - Lak6GwtPestStartPosition;
        if Index < Length(SpecifiedConcentrations.ConcentrationTimeSeriesNames) then
        begin
          result := SpecifiedConcentrations.ConcentrationTimeSeriesNames[Index];
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        if Index < Length(RainfallConcentrations.ConcentrationTimeSeriesNames) then
        begin
          result := RainfallConcentrations.ConcentrationTimeSeriesNames[Index];
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        if Index < Length(EvapConcentrations.ConcentrationTimeSeriesNames) then
        begin
          result := EvapConcentrations.ConcentrationTimeSeriesNames[Index];
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        if Index < Length(RunoffConcentrations.ConcentrationTimeSeriesNames) then
        begin
          result := RunoffConcentrations.ConcentrationTimeSeriesNames[Index];
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        if Index < Length(InflowConcentrations.ConcentrationTimeSeriesNames) then
        begin
          result := InflowConcentrations.ConcentrationTimeSeriesNames[Index];
          Exit;
        end;
        result := '';
        Assert(False);
      end
      else
      begin
        result := '';
        Assert(False);
      end;
    end;
  end;
end;

function TLakeSetting.GetValue(Index: Integer): double;
var
  SpeciesCount: Integer;
begin
  case Index of
    Lak6StagePosition:
      begin
        result := Stage;
      end;
    Lak6RainfallPosition:
      begin
        result := Rainfall;
      end;
    Lak6RunoffPosition:
      begin
        result := Runoff;
      end;
    Lak6EvaporationPosition:
      begin
        result := Evaporation;
      end;
    Lak6InflowPosition:
      begin
        result := Inflow;
      end;
    Lak6WithdrawalPosition:
      begin
        result :=  Withdrawal;
      end;
    else
    begin
      if frmGoPhast.PhastModel.GwtUsed then
      begin
        SpeciesCount := frmGoPhast.PhastModel.MobileComponents.Count;
        Index := Index - Lak6GwtPestStartPosition;
        if Index < Length(SpecifiedConcentrations.Concentrations) then
        begin
          result := SpecifiedConcentrations.Concentrations[Index];
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        if Index < Length(RainfallConcentrations.Concentrations) then
        begin
          result := RainfallConcentrations.Concentrations[Index];
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        if Index < Length(EvapConcentrations.Concentrations) then
        begin
          result := EvapConcentrations.Concentrations[Index];
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        if Index < Length(RunoffConcentrations.Concentrations) then
        begin
          result := RunoffConcentrations.Concentrations[Index];
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        if Index < Length(InflowConcentrations.Concentrations) then
        begin
          result := InflowConcentrations.Concentrations[Index];
          Exit;
        end;
        result := 0;
        Assert(False);
      end
      else
      begin
        result := 0;
        Assert(False);
      end;
    end;
  end;
end;

procedure TLakeSetting.SetPestMethod(Index: Integer;
  const Value: TPestParamMethod);
var
  SpeciesCount: Integer;
begin
  case Index of
    Lak6StagePosition:
      begin
        StagePestMethod := Value;
      end;
    Lak6RainfallPosition:
      begin
        RainfallPestMethod := Value;
      end;
    Lak6RunoffPosition:
      begin
        RunoffPestMethod := Value;
      end;
    Lak6EvaporationPosition:
      begin
        EvaporationPestMethod := Value;
      end;
    Lak6InflowPosition:
      begin
        InflowPestMethod := Value;
      end;
    Lak6WithdrawalPosition:
      begin
        WithdrawalPestMethod  := Value;
      end;
    else
    begin
      if frmGoPhast.PhastModel.GwtUsed then
      begin
        SpeciesCount := frmGoPhast.PhastModel.MobileComponents.Count;
        SpecifiedConcentrations.SpeciesCount := SpeciesCount;
        Index := Index - Lak6GwtPestStartPosition;
        if Index < Length(SpecifiedConcentrations.ConcentrationPestSeriesMethods) then
        begin
          SpecifiedConcentrations.ConcentrationPestSeriesMethods[Index] := Value;
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        RainfallConcentrations.SpeciesCount := SpeciesCount;
        if Index < Length(RainfallConcentrations.ConcentrationPestSeriesMethods) then
        begin
          RainfallConcentrations.ConcentrationPestSeriesMethods[Index] := Value;
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        EvapConcentrations.SpeciesCount := SpeciesCount;
        if Index < Length(EvapConcentrations.ConcentrationPestSeriesMethods) then
        begin
          EvapConcentrations.ConcentrationPestSeriesMethods[Index] := Value;
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        RunoffConcentrations.SpeciesCount := SpeciesCount;
        if Index < Length(RunoffConcentrations.ConcentrationPestSeriesMethods) then
        begin
          RunoffConcentrations.ConcentrationPestSeriesMethods[Index] := Value;
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        InflowConcentrations.SpeciesCount := SpeciesCount;
        if Index < Length(InflowConcentrations.ConcentrationPestSeriesMethods) then
        begin
          InflowConcentrations.ConcentrationPestSeriesMethods[Index] := Value;
          Exit;
        end;
        Assert(False);
      end
      else
      begin
        Assert(False);
      end;
    end;
  end;
end;

procedure TLakeSetting.SetPestName(Index: Integer; const Value: string);
var
  SpeciesCount: Integer;
begin
  case Index of
    Lak6StagePosition:
      begin
        StagePest := Value;
      end;
    Lak6RainfallPosition:
      begin
        RainfallPest := Value;
      end;
    Lak6RunoffPosition:
      begin
        RunoffPest := Value;
      end;
    Lak6EvaporationPosition:
      begin
        EvaporationPest := Value;
      end;
    Lak6InflowPosition:
      begin
        InflowPest := Value;
      end;
    Lak6WithdrawalPosition:
      begin
        WithdrawalPest  := Value;
      end;
    else
    begin
      if frmGoPhast.PhastModel.GwtUsed then
      begin
        SpeciesCount := frmGoPhast.PhastModel.MobileComponents.Count;
        SpecifiedConcentrations.SpeciesCount := SpeciesCount;
        Index := Index - Lak6GwtPestStartPosition;
        if Index < Length(SpecifiedConcentrations.ConcentrationPestNames) then
        begin
          SpecifiedConcentrations.ConcentrationPestNames[Index] := Value;
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        RainfallConcentrations.SpeciesCount := SpeciesCount;
        if Index < Length(RainfallConcentrations.ConcentrationPestNames) then
        begin
          RainfallConcentrations.ConcentrationPestNames[Index] := Value;
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        EvapConcentrations.SpeciesCount := SpeciesCount;
        if Index < Length(EvapConcentrations.ConcentrationPestNames) then
        begin
          EvapConcentrations.ConcentrationPestNames[Index] := Value;
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        RunoffConcentrations.SpeciesCount := SpeciesCount;
        if Index < Length(RunoffConcentrations.ConcentrationPestNames) then
        begin
          RunoffConcentrations.ConcentrationPestNames[Index] := Value;
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        InflowConcentrations.SpeciesCount := SpeciesCount;
        if Index < Length(InflowConcentrations.ConcentrationPestNames) then
        begin
          InflowConcentrations.ConcentrationPestNames[Index] := Value;
          Exit;
        end;
        Assert(False);
      end
      else
      begin
        Assert(False);
      end;
    end;
  end;
end;

procedure TLakeSetting.SetPestSeries(Index: Integer; const Value: string);
var
  SpeciesCount: Integer;
begin
  case Index of
    Lak6StagePosition:
      begin
        StagePestSeries := Value;
      end;
    Lak6RainfallPosition:
      begin
        RainfallPestSeries := Value;
      end;
    Lak6RunoffPosition:
      begin
        RunoffPestSeries := Value;
      end;
    Lak6EvaporationPosition:
      begin
        EvaporationPestSeries := Value;
      end;
    Lak6InflowPosition:
      begin
        InflowPestSeries := Value;
      end;
    Lak6WithdrawalPosition:
      begin
        WithdrawalPestSeries  := Value;
      end;
    else
    begin
      if frmGoPhast.PhastModel.GwtUsed then
      begin
        SpeciesCount := frmGoPhast.PhastModel.MobileComponents.Count;
        SpecifiedConcentrations.SpeciesCount := SpeciesCount;
        Index := Index - Lak6GwtPestStartPosition;
        if Index < Length(SpecifiedConcentrations.ConcentrationPestSeriesNames) then
        begin
          SpecifiedConcentrations.ConcentrationPestSeriesNames[Index] := Value;
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        RainfallConcentrations.SpeciesCount := SpeciesCount;
        if Index < Length(RainfallConcentrations.ConcentrationPestSeriesNames) then
        begin
          RainfallConcentrations.ConcentrationPestSeriesNames[Index] := Value;
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        EvapConcentrations.SpeciesCount := SpeciesCount;
        if Index < Length(EvapConcentrations.ConcentrationPestSeriesNames) then
        begin
          EvapConcentrations.ConcentrationPestSeriesNames[Index] := Value;
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        RunoffConcentrations.SpeciesCount := SpeciesCount;
        if Index < Length(RunoffConcentrations.ConcentrationPestSeriesNames) then
        begin
          RunoffConcentrations.ConcentrationPestSeriesNames[Index] := Value;
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        InflowConcentrations.SpeciesCount := SpeciesCount;
        if Index < Length(InflowConcentrations.ConcentrationPestSeriesNames) then
        begin
          InflowConcentrations.ConcentrationPestSeriesNames[Index] := Value;
          Exit;
        end;
        Assert(False);
      end
      else
      begin
        Assert(False);
      end;
    end;
  end;
end;

procedure TLakeSetting.SetTimeSeriesName(Index: Integer; const Value: string);
var
  SpeciesCount: Integer;
begin
  case Index of
    Lak6StagePosition:
      begin
        StageTimeSeriesName := Value;
      end;
    Lak6RainfallPosition:
      begin
        RainfallTimeSeriesName := Value;
      end;
    Lak6RunoffPosition:
      begin
        RunoffTimeSeriesName := Value;
      end;
    Lak6EvaporationPosition:
      begin
        EvaporationTimeSeriesName := Value;
      end;
    Lak6InflowPosition:
      begin
        InflowTimeSeriesName := Value;
      end;
    Lak6WithdrawalPosition:
      begin
        WithdrawalTimeSeriesName  := Value;
      end;
    else
    begin
      if frmGoPhast.PhastModel.GwtUsed then
      begin
        SpeciesCount := frmGoPhast.PhastModel.MobileComponents.Count;
        SpecifiedConcentrations.SpeciesCount := SpeciesCount;
        Index := Index - Lak6GwtPestStartPosition;
        if Index < Length(SpecifiedConcentrations.ConcentrationTimeSeriesNames) then
        begin
          SpecifiedConcentrations.ConcentrationTimeSeriesNames[Index] := Value;
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        RainfallConcentrations.SpeciesCount := SpeciesCount;
        if Index < Length(RainfallConcentrations.ConcentrationTimeSeriesNames) then
        begin
          RainfallConcentrations.ConcentrationTimeSeriesNames[Index] := Value;
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        EvapConcentrations.SpeciesCount := SpeciesCount;
        if Index < Length(EvapConcentrations.ConcentrationTimeSeriesNames) then
        begin
          EvapConcentrations.ConcentrationTimeSeriesNames[Index] := Value;
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        RunoffConcentrations.SpeciesCount := SpeciesCount;
        if Index < Length(RunoffConcentrations.ConcentrationTimeSeriesNames) then
        begin
          RunoffConcentrations.ConcentrationTimeSeriesNames[Index] := Value;
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        InflowConcentrations.SpeciesCount := SpeciesCount;
        if Index < Length(InflowConcentrations.ConcentrationTimeSeriesNames) then
        begin
          InflowConcentrations.ConcentrationTimeSeriesNames[Index] := Value;
          Exit;
        end;
        Assert(False);
      end
      else
      begin
        Assert(False);
      end;
    end;
  end;
end;

procedure TLakeSetting.SetValue(Index: Integer; const Value: double);
var
  SpeciesCount: Integer;
begin
  case Index of
    Lak6StagePosition:
      begin
        Stage := Value;
      end;
    Lak6RainfallPosition:
      begin
        Rainfall := Value;
      end;
    Lak6RunoffPosition:
      begin
        Runoff := Value;
      end;
    Lak6EvaporationPosition:
      begin
        Evaporation := Value;
      end;
    Lak6InflowPosition:
      begin
        Inflow := Value;
      end;
    Lak6WithdrawalPosition:
      begin
        Withdrawal  := Value;
      end;
    else
    begin
      if frmGoPhast.PhastModel.GwtUsed then
      begin
        SpeciesCount := frmGoPhast.PhastModel.MobileComponents.Count;
        SpecifiedConcentrations.SpeciesCount := SpeciesCount;
        Index := Index - Lak6GwtPestStartPosition;
        if Index < Length(SpecifiedConcentrations.Concentrations) then
        begin
          SpecifiedConcentrations.Concentrations[Index] := Value;
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        RainfallConcentrations.SpeciesCount := SpeciesCount;
        if Index < Length(RainfallConcentrations.Concentrations) then
        begin
          RainfallConcentrations.Concentrations[Index] := Value;
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        EvapConcentrations.SpeciesCount := SpeciesCount;
        if Index < Length(EvapConcentrations.Concentrations) then
        begin
          EvapConcentrations.Concentrations[Index] := Value;
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        RunoffConcentrations.SpeciesCount := SpeciesCount;
        if Index < Length(RunoffConcentrations.Concentrations) then
        begin
          RunoffConcentrations.Concentrations[Index] := Value;
          Exit;
        end;
        Index := Index - SpeciesCount;
        Assert(Index >= 0);
        InflowConcentrations.SpeciesCount := SpeciesCount;
        if Index < Length(InflowConcentrations.Concentrations) then
        begin
          InflowConcentrations.Concentrations[Index] := Value;
          Exit;
        end;
        Assert(False);
      end
      else
      begin
        Assert(False);
      end;
    end;
  end;
end;

end.
