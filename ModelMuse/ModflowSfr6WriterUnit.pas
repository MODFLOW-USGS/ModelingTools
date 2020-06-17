unit ModflowSfr6WriterUnit;

interface

uses Windows, Types, SysUtils, Classes, Contnrs, Forms, CustomModflowWriterUnit,
  ModflowPackageSelectionUnit, PhastModelUnit, System.Generics.Collections,
  ScreenObjectUnit, ModflowSfr6Unit, ModflowBoundaryDisplayUnit,
  Modflow6ObsUnit;

type
  TSfr6Observation = record
    FName: string;
    FBoundName: string;
    FReachStart: Integer;
    FCount: Integer;
    FObsTypes: TSfrObs;
    FSfrObsLocation: TSfrObsLocation;
    FModflow6Obs: TModflow6Obs;
  end;
  TSfr6ObservationList = TList<TSfr6Observation>;
  
  TSfr6Segment = class(TObject)
  private
    FModel: TCustomModel;
    FReaches: TList;
    FSteadyValues: TSfrMF6ConstArray;
    FSfr6Boundary: TSfrMf6Boundary;
    FScreenObject: TScreenObject;
    FReachCount: Integer;
    procedure SetReachCount(const Value: Integer);
    function GetFirst: TSfrMF6ConstantRecord;
    function GetLast: TSfrMF6ConstantRecord;
    procedure SetFirst(const Value: TSfrMF6ConstantRecord);
    procedure SetLast(const Value: TSfrMF6ConstantRecord);
    procedure EliminateInactiveReaches;
    procedure AssignReachNumbers(var StartingNumber: Integer);
  public
    constructor Create(Model: TCustomModel);
    destructor Destroy; override;
    property ReachCount: Integer read FReachCount write SetReachCount;
    property SteadyValues: TSfrMF6ConstArray read FSteadyValues
      write FSteadyValues;
    property First: TSfrMF6ConstantRecord read GetFirst write SetFirst;
    property Last: TSfrMF6ConstantRecord read GetLast write SetLast;
    property ScreenObject: TScreenObject read FScreenObject;
  end;

  TSfr6SegmentList = TObjectList<TSfr6Segment>;

  TModflowSFR_MF6_Writer = class(TCustomPackageWriter)
  private
    FNameOfFile: string;
    FValues: TList;
    FSegments: TSfr6SegmentList;
    FReachCount: integer;
    FDiversionCount: Integer;
    FObsList: TSfr6ObservationList;
    FDirectObsLines: TStrings;
    FFileNameLines: TStrings;
    FCalculatedObsLines: TStrings;
    procedure Evaluate;
    procedure EvaluateSteadyData;
    procedure AssignSteadyData(ASegment: TSfr6Segment);
    procedure AssignConnections;
    procedure WriteOptions;
    procedure WriteDimensions;
    procedure WritePackageData;
    procedure WriteConnections;
    procedure WriteDiversions;
    procedure WriteStressPeriods;
    procedure InternalUpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
//    // Check that stage decreases in the downstream direction.
//    procedure CheckStage;
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
    class function ObservationOutputExtension: string;
    function IsMf6Observation(AScreenObject: TScreenObject): Boolean;
    function ObservationsUsed: Boolean;
    class function ObservationExtension: string;
  public
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateSteadyData;
    property DirectObsLines: TStrings read FDirectObsLines write FDirectObsLines;
    property CalculatedObsLines: TStrings read FCalculatedObsLines write FCalculatedObsLines;
    property FileNameLines: TStrings read FFileNameLines write FFileNameLines;
  end;

implementation

uses
  frmErrorsAndWarningsUnit, GoPhastTypes, frmProgressUnit, ModflowGridUnit,
  RbwParser, GIS_Functions, DataSetUnit, frmFormulaErrorsUnit, ModflowCellUnit,
  AbstractGridUnit, Modflow6ObsWriterUnit,
  ModflowMvrWriterUnit, ModflowMvrUnit, ModflowIrregularMeshUnit, FastGEO;

resourcestring
  StrTheFollowingPairO = 'The following pair of objects have the same SFR se' +
  'gment numbers. Reach connections can not be assigned correctly unless ' +
  'each segment has a unique segment number';
  StrSegmentNumber0d = 'Segment number: %0:d; Objects: %1:s, %2:s';
  StrTheFollowingObject = 'The following objects define a downstream connect' +
  'ion to another segment that does not exist.';
  StrObject0sInvali = 'Object: %0:s; Invalid Downstream Segment %1:d';
  StrTheFollowingObjectDiv = 'The following objects define a diversion from ' +
  'another segment that does not exist.';
  StrAllReachesInASeg = 'All reaches in a segment except the first are autom' +
  'atically assigned an upstream fraction of 1.';
  StrNoReaches = 'The following objects do not define any reaches ' +
  'in the SFR package.';
  StrAboveTop = 'The stage is above the cell top and may be ignored by ' +
    'MODFLOW 6 in the following SFR reaches (Layer, Row, Column, Object)';
  StrBelowBottom = 'The stage is below the cell bottom and may be ignored by ' +
    'MODFLOW 6 in the following SFR reaches';
  StrWritingSFROPTION = '  Writing SFR OPTIONS';
  StrWritingSFRDimens = '  Writing SFR Dimensions';
  StrWritingSFRPackag = '  Writing SFR Package Data';
  StrWritingSFRConnec = '  Writing SFR Connections';
  StrWritingSFRDivers = '  Writing SFR Diversions';
  StrWritingSFRStress = '  Writing SFR Stress Periods';
  StrWritingSFRStre = '    Writing SFR Stress Period %d';
  StrStreamStatusIn = 'Stream Status in %s';
  StrReachNumberIn = 'Reach Number in %s';
  StrInvalidResultType = 'Invalid result type.';
  StrReachLength = 'Reach Length';
  StrReachWidth = 'Reach Width';
  StrGradient = 'Gradient';
  StrStreambedTop = 'Streambed Top';
  StrStreambedThickness = 'Streambed Thickness';
  StrHydraulicConductivi = 'Hydraulic Conductivity';
  StrRoughness = 'Roughness';
  StrUpstreamFraction = 'Upstream Fraction';
  StrStreambedTopElevat = 'Streambed top elevation increases in the downstre' +
  'am direction';
  StrIn0sAtLayer = 'In %0:s, at Layer: %1:d; Row: %2:d, Column: %3:d';
  StrIn0sAtLayerCell = 'In %0:s, at Layer: %1:d; Cell: %2:d';
  StrTheRoughnessIsLes = 'The roughness is less then or equal to zero in the' +
  ' following SFR reaches';
  StrDownstreamSFRSegme = 'Downstream SFR segment separated from upstream SF' +
  'R segment';
  StrTheDownstreamEndDisv = 'The downstream end of the segment defined by "%' +
  '0:s" in cell %1:d is separated from the upstream end of the segment defin' +
  'ed by %2:s in cell %3:d by %4:g.';
  StrTheDownstreamEndGrid = 'The downstream end of the segment defined by "%' +
  '0:s" in (Row,Col) (%1:d, %2:d) is separated from the upstream end of the ' +
  'segment defined by %3:s in (%4:d, %5:d) by %6:d cells.';
  StrSFRUpstreamValues = 'SFR Upstream Values do not add up to 1';
  StrTheSumOfTheUpstr = 'The sum of the upstream factions in stress period %' +
  '0:d in active downstream reaches does not add up to 1 in reach number %1:' +
  'd defined by %2:s. the downstream segments for this reach are defined by ' +
  'the following Objects. %3:s';
  StrErrorAssigningDive = 'Error assigning diversions';
  StrIn0sTheDiversio = 'In %0:s the diversion segment number in SFR is the s' +
  'ame of the segment itself. The diversion segment number should be the num' +
  'ber of a different segment from which flow is diverted.';
  StrTheStreambedMinusThe = 'The streambed top minus the streambed thickness is below th' +
  'e cell bottom in the following SFR reaches';
  StrLayerRowColumnNameStreambed = '[Layer, Row, Column, Object, Streambed top, S' +
  'treambed Thickness, Cell Bottom: %0:d, %1:d, %2:d, %3:s, %4:g, %5:g, %6:g';
  StrStreamTimeExtended = 'Stream time extended';
  StrIn0sTheLastDe = 'In %0:s, the last defined time was %1:g which was befo' +
  're the end of the model. It has been extended to %2:g';
  StrInactiveStreamPeri = 'Inactive stream period added';
  StrInSTheStarting = 'In %s, the starting time for the stream was after the' +
  ' start of the model. An inactive stream period has been added to cover th' +
  'at time.';
  StrIn0sThereWasA = 'In %0:s, there was a gap in time from %1:g to %2:g. An' +
  ' inactive period has been inserted to fill that time.';

{ TModflowSFR_MF6_Writer }

procedure TModflowSFR_MF6_Writer.AssignConnections;
var
  SegmentDictionary: TDictionary<Integer, TSfr6Segment>;
  SegIndex: Integer;
  ASegment: TSfr6Segment;
  OtherSegment: TSfr6Segment;
  ReachIndex: Integer;
  AReach: TSfrMF6ConstantRecord;
  PriorReach: TSfrMF6ConstantRecord;
  ConnectedIndex: Integer;
  DownstreamSegmentNumber: Integer;
  NewLength: Integer;
  OtherReach: TSfrMF6ConstantRecord;
  DiversionIndex: Integer;
  ADiversionSegIndex: Integer;
  ADiversion: TSDiversionItem;
  NeighborCells: Boolean;
  Cell2D1: TModflowIrregularCell2D;
  Cell2D2: TModflowIrregularCell2D;
  SeparationDistance: double;
  CellDistance: Integer;
  DivSeg: Integer;
  procedure CheckConnectionError;
  begin
    if AReach.Cell <> OtherReach.Cell then
    begin
      if Model.DisvUsed then
      begin
        Cell2D1 := Model.DisvGrid.TwoDGrid.Cells[AReach.Cell.Column];
        Cell2D2 := Model.DisvGrid.TwoDGrid.Cells[OtherReach.Cell.Column];
        NeighborCells :=  (Cell2D1 = Cell2D2) or Cell2D1.IsNeighbor(Cell2D2);
        if not NeighborCells then
        begin
          SeparationDistance := Distance(Cell2D1.Location, Cell2D2.Location);
          frmErrorsAndWarnings.AddWarning(Model,
            StrDownstreamSFRSegme, Format(StrTheDownstreamEndDisv,
            [ASegment.FScreenObject.Name, AReach.Cell.Column+1,
            OtherSegment.FScreenObject.Name, OtherReach.Cell.Column+1,
            SeparationDistance]),
            OtherSegment.FScreenObject)
        end;
      end
      else
      begin
        NeighborCells := (Abs(AReach.Cell.Column - OtherReach.Cell.Column) <= 1)
          and (Abs(AReach.Cell.Row - OtherReach.Cell.Row) <= 1);
        if not NeighborCells then
        begin
          CellDistance := Abs(AReach.Cell.Column - OtherReach.Cell.Column)
            + Abs(AReach.Cell.Row - OtherReach.Cell.Row);
          frmErrorsAndWarnings.AddWarning(Model,
            StrDownstreamSFRSegme, Format(StrTheDownstreamEndGrid,
            [ASegment.FScreenObject.Name, AReach.Cell.Row+1, AReach.Cell.Column+1,
            OtherSegment.FScreenObject.Name, OtherReach.Cell.Row+1, OtherReach.Cell.Column+1,
            CellDistance]),
            OtherSegment.FScreenObject)
        end;
      end;
    end
  end;
begin
  SegmentDictionary := TDictionary<Integer, TSfr6Segment>.Create;
  try
    FDiversionCount := 0;
    for SegIndex := 0 to FSegments.Count - 1 do
    begin
      ASegment := FSegments[SegIndex];
      if SegmentDictionary.TryGetValue(ASegment.FSfr6Boundary.SegmentNumber,
        OtherSegment) then
      begin
        frmErrorsAndWarnings.AddError(Model, StrTheFollowingPairO,
          Format(StrSegmentNumber0d,
          [ASegment.FSfr6Boundary.SegmentNumber, ASegment.FScreenObject.Name,
          OtherSegment.FScreenObject.Name]), ASegment.FScreenObject);
      end
      else
      begin
        SegmentDictionary.Add(ASegment.FSfr6Boundary.SegmentNumber, ASegment);
      end;

      for ReachIndex := 0 to Length(ASegment.SteadyValues) - 1 do
      begin
        AReach := ASegment.SteadyValues[ReachIndex];
        if Length(ASegment.SteadyValues) > 1 then
        begin
          // Assign connections for reaches that are part of the same segment.
          if ReachIndex = 0 then
          begin
            SetLength(ASegment.SteadyValues[ReachIndex].ConnectedReaches, 1);
            ASegment.SteadyValues[ReachIndex].ConnectedReaches[0] := -(AReach.ReachNumber+1);
          end
          else if ReachIndex = Length(ASegment.SteadyValues) - 1 then
          begin
            SetLength(ASegment.SteadyValues[ReachIndex].ConnectedReaches, 1);
            ASegment.SteadyValues[ReachIndex].ConnectedReaches[0] := AReach.ReachNumber-1;
          end
          else
          begin
            SetLength(ASegment.SteadyValues[ReachIndex].ConnectedReaches, 2);
            ASegment.SteadyValues[ReachIndex].ConnectedReaches[0] := AReach.ReachNumber-1;
            ASegment.SteadyValues[ReachIndex].ConnectedReaches[1] := -(AReach.ReachNumber+1);
          end;
        end
        else
        begin
          SetLength(ASegment.SteadyValues[ReachIndex].ConnectedReaches, 0);
        end;
        if ReachIndex > 0 then
        begin
          if PriorReach.StreambedTop < AReach.StreambedTop then
          begin
            if Model.DisvUsed then
            begin
              frmErrorsAndWarnings.AddWarning(Model, StrStreambedTopElevat,
                Format(StrIn0sAtLayerCell,
                [ASegment.FScreenObject.name, AReach.Cell.Layer+1,
                AReach.Cell.Column+1]), ASegment.FScreenObject);
            end
            else
            begin
              frmErrorsAndWarnings.AddWarning(Model, StrStreambedTopElevat,
                Format(StrIn0sAtLayer,
                [ASegment.FScreenObject.name, AReach.Cell.Layer+1, AReach.Cell.Row+1,
                AReach.Cell.Column+1]), ASegment.FScreenObject);
            end;
          end;
        end;
        PriorReach := AReach;
      end;
    end;
    for SegIndex := 0 to FSegments .Count - 1 do
    begin
      ASegment := FSegments[SegIndex];

      for DiversionIndex := 0 to ASegment.FSfr6Boundary.Diversions.Count - 1 do
      begin
        DivSeg := ASegment.FSfr6Boundary.Diversions[DiversionIndex].DownstreamSegment;
        if ASegment.FSfr6Boundary.DownstreamSegments.IndexOf(DivSeg) >= 0 then
        begin
          frmErrorsAndWarnings.AddError(Model, 'Diversion segment also listed as downstream segment',
            Format('In %0:s, $1:d is listed as both a downstream segment and diversion segment.',
            [ASegment.FScreenObject.Name, DivSeg]),  ASegment.FScreenObject);
        end;
      end;


      for ConnectedIndex := 0 to
        ASegment.FSfr6Boundary.DownstreamSegments.Count - 1 do
      begin
        DownstreamSegmentNumber := ASegment.FSfr6Boundary.
          DownstreamSegments[ConnectedIndex].Value;
        if SegmentDictionary.TryGetValue(DownstreamSegmentNumber,
          OtherSegment) then
        begin
          if (ASegment.ReachCount > 0) and (OtherSegment.ReachCount > 0) then
          begin
            AReach := ASegment.Last;
            OtherReach := OtherSegment.First;
            if OtherReach.StreambedTop < AReach.StreambedTop then
            begin
              if Model.DisvUsed then
              begin
                frmErrorsAndWarnings.AddWarning(Model, StrStreambedTopElevat,
                  Format(StrIn0sAtLayerCell,
                  [ASegment.FScreenObject.name, AReach.Cell.Layer+1,
                  AReach.Cell.Column+1]), ASegment.FScreenObject);
              end
              else
              begin
                frmErrorsAndWarnings.AddWarning(Model, StrStreambedTopElevat,
                  Format(StrIn0sAtLayer,
                  [ASegment.FScreenObject.name, AReach.Cell.Layer+1, AReach.Cell.Row+1,
                  AReach.Cell.Column+1]), ASegment.FScreenObject);
              end;
            end;

            CheckConnectionError;

            NewLength := Length(AReach.ConnectedReaches) + 1;
            SetLength(AReach.ConnectedReaches, NewLength);
            AReach.ConnectedReaches[NewLength-1] := -OtherReach.ReachNumber;
            ASegment.SteadyValues[Length(ASegment.SteadyValues)-1] := AReach;

            NewLength := Length(OtherReach.ConnectedReaches) + 1;
            SetLength(OtherReach.ConnectedReaches, NewLength);
            OtherReach.ConnectedReaches[NewLength-1] := AReach.ReachNumber;
            OtherSegment.SteadyValues[0] := OtherReach;
          end
          else
          begin
            if ASegment.ReachCount = 0 then
            begin
              frmErrorsAndWarnings.AddWarning(Model, StrNoReaches,
                ASegment.FScreenObject.Name, ASegment.FScreenObject);
            end;
            if OtherSegment.ReachCount = 0 then
            begin
              frmErrorsAndWarnings.AddWarning(Model, StrNoReaches,
                OtherSegment.FScreenObject.Name, OtherSegment.FScreenObject);
            end;
          end;
        end
        else
        begin
          frmErrorsAndWarnings.AddError(Model, StrTheFollowingObject,
            Format(StrObject0sInvali,
            [ASegment.FScreenObject.Name, DownstreamSegmentNumber]),
            ASegment.FScreenObject);
        end;
      end;
      for DiversionIndex := 0 to ASegment.FSfr6Boundary.Diversions.Count - 1 do
      begin
        ADiversion := ASegment.FSfr6Boundary.Diversions[DiversionIndex];
        Inc(FDiversionCount);
//        ADiversion.DiversionNumber := FDiversionCount;
        ADiversionSegIndex := ADiversion.DownstreamSegment;
        if SegmentDictionary.TryGetValue(ADiversionSegIndex, OtherSegment) then
        begin
          if OtherSegment = ASegment then
          begin
            frmErrorsAndWarnings.AddError(Model, StrErrorAssigningDive,
              Format(StrIn0sTheDiversio,
              [(ASegment.FScreenObject as TScreenObject).Name]),
              ASegment.FScreenObject);
//            Continue;
          end;
          if (ASegment.ReachCount > 0) and (OtherSegment.ReachCount > 0) then
          begin
            AReach := ASegment.Last;
            OtherReach := OtherSegment.First;
            if not AReach.IsConnected(-OtherReach.ReachNumber) then
            begin
              NewLength := Length(AReach.ConnectedReaches)+1;
              SetLength(AReach.ConnectedReaches, NewLength);
              AReach.ConnectedReaches[NewLength-1] := -OtherReach.ReachNumber;
              ASegment.Last := AReach;
            end;
            if not OtherReach.IsConnected(AReach.ReachNumber) then
            begin
              NewLength := Length(OtherReach.ConnectedReaches)+1;
              SetLength(OtherReach.ConnectedReaches, NewLength);
              OtherReach.ConnectedReaches[NewLength-1] := AReach.ReachNumber;
              OtherSegment.First := OtherReach;
            end;
            if OtherReach.StreambedTop > AReach.StreambedTop then
            begin
              if Model.DisvUsed then
              begin
                frmErrorsAndWarnings.AddWarning(Model, StrStreambedTopElevat,
                  Format(StrIn0sAtLayerCell,
                  [ASegment.FScreenObject.name, AReach.Cell.Layer+1,
                  AReach.Cell.Column+1]), ASegment.FScreenObject);
              end
              else
              begin
                frmErrorsAndWarnings.AddWarning(Model, StrStreambedTopElevat,
                  Format(StrIn0sAtLayer,
                  [ASegment.FScreenObject.name, AReach.Cell.Layer+1, AReach.Cell.Row+1,
                  AReach.Cell.Column+1]), ASegment.FScreenObject);
              end;
            end;

            CheckConnectionError;
          end
          else
          begin
            if ASegment.ReachCount = 0 then
            begin
              frmErrorsAndWarnings.AddWarning(Model, StrNoReaches,
                ASegment.FScreenObject.Name, ASegment.FScreenObject);
            end;
            if OtherSegment.ReachCount = 0 then
            begin
              frmErrorsAndWarnings.AddWarning(Model, StrNoReaches,
                OtherSegment.FScreenObject.Name, OtherSegment.FScreenObject);
            end;
          end;
        end
        else
        begin
          frmErrorsAndWarnings.AddError(Model, StrTheFollowingObjectDiv,
            Format(StrObject0sInvali, [ASegment.FScreenObject.Name]),
            ASegment.FScreenObject);
        end;
      end;
    end;
  finally
    SegmentDictionary.Free;
  end;
end;

procedure TModflowSFR_MF6_Writer.AssignSteadyData(ASegment: TSfr6Segment);
var
//  Grid: TModflowGrid;
  Compiler: TRbwParser;
  CellList: TCellAssignmentList;
  Formula: string;
  Expression: TExpression;
  ACell: TCellAssignment;
  CellIndex: Integer;
  UseList: TStringList;
  VarIndex: Integer;
  ADataSet: TDataArray;
  ResultTypeOK: Boolean;
  TempFormula: string;
  VariablePositions: array of Integer;
  DataSetIndexes: array of Integer;
  VarName: string;
  VarPosition: Integer;
  DataSetIndex: Integer;
  LayerToUse: Integer;
  RowToUse: Integer;
  ColToUse: Integer;
  Variable: TCustomValue;
  PropertyName: string;
  AnnotationString: string;
  PropertyNames: TStringList;
  PropertyFormulas: TStringList;
  FormulaIndex: Integer;
  CellValues: TSfrMF6ConstantRecord;
  CellBottom: Double;
begin
  CellList := TCellAssignmentList.Create;
  UseList := TStringList.Create;
  PropertyNames := TStringList.Create;
  PropertyFormulas := TStringList.Create;
  try
    PropertyNames.Add(StrReachLength);
    PropertyFormulas.Add(ASegment.FSfr6Boundary.ReachLength);
    PropertyNames.Add(StrReachWidth);
    PropertyFormulas.Add(ASegment.FSfr6Boundary.ReachWidth);
    PropertyNames.Add(StrGradient);
    PropertyFormulas.Add(ASegment.FSfr6Boundary.Gradient);
    PropertyNames.Add(StrStreambedTop);
    PropertyFormulas.Add(ASegment.FSfr6Boundary.StreambedTop);
    PropertyNames.Add(StrStreambedThickness);
    PropertyFormulas.Add(ASegment.FSfr6Boundary.StreambedThickness);
    PropertyNames.Add(StrHydraulicConductivi);
    PropertyFormulas.Add(ASegment.FSfr6Boundary.HydraulicConductivity);
//    PropertyNames.Add(StrRoughness);
//    PropertyFormulas.Add(ASegment.FSfr6Boundary.Roughness);
//    UpstreamFractionIndex := PropertyFormulas.Count;
//    PropertyNames.Add(StrUpstreamFraction);
//    PropertyFormulas.Add(ASegment.FSfr6Boundary.UpstreamFraction);

//    Grid := Model.ModflowGrid;
    Compiler := Model.rpThreeDFormulaCompiler;

    ASegment.FScreenObject.GetCellsToAssign('0', nil, nil, CellList,
      alAll, Model);
    ASegment.ReachCount := CellList.Count;

    for FormulaIndex := 0 to PropertyFormulas.Count - 1 do
    begin
      Formula := PropertyFormulas[FormulaIndex];
      TempFormula := Formula;
      PropertyName := PropertyNames[FormulaIndex];
      try
        Compiler.Compile(Formula);
      except  on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError('', PropertyName, TempFormula, E.Message);
          Formula := '0';
          Compiler.Compile(Formula);
        end;
      end;
      Expression := Compiler.CurrentExpression;
      ResultTypeOK := Expression.ResultType in [rdtInteger, rdtDouble];
      if not ResultTypeOK then
      begin
        frmFormulaErrors.AddFormulaError('', PropertyName, TempFormula, StrInvalidResultType);
        Formula := '0';
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
      end;
      UseList.Assign(Expression.VariablesUsed);
      for VarIndex := 0 to UseList.Count - 1 do
      begin
        VarName := UseList[VarIndex];
        ADataSet := Model.DataArrayManager.GetDataSetByName(VarName);
        if ADataSet <> nil then
        begin
          ADataSet.Initialize;
          Model.DataArrayManager.AddDataSetToCache(ADataSet);
        end;
      end;

      SetLength(VariablePositions, UseList.Count);
      SetLength(DataSetIndexes, UseList.Count);
      for VarIndex := 0 to UseList.Count - 1 do
      begin
        VarName := UseList[VarIndex];
        VarPosition := Compiler.IndexOfVariable(VarName);
        VariablePositions[VarIndex] := VarPosition;
        if VarPosition >= 0 then
        begin
          DataSetIndex := Model.DataArrayManager.IndexOfDataSet(VarName);
          DataSetIndexes[VarIndex] := DataSetIndex;
        end
        else
        begin
          DataSetIndexes[VarIndex] := -1;
        end;
      end;


  //    UpdateCurrentScreenObject(ASegment.FScreenObject);
      for CellIndex := 0 to CellList.Count - 1 do
      begin
        ACell := CellList[CellIndex];
        if FormulaIndex = 0 then
        begin
//          Inc(FReachCount);
//          ASegment.FSteadyValues[Index].ReachNumber := FReachCount;
          ASegment.FSteadyValues[CellIndex].Cell.Layer := ACell.Layer;
          ASegment.FSteadyValues[CellIndex].Cell.Row := ACell.Row;
          ASegment.FSteadyValues[CellIndex].Cell.Column := ACell.Column;
        end;
//        if (FormulaIndex = UpstreamFractionIndex)
//          and (Index <> 0) then
//        begin
//          //Always assign upstream fraction a value of 1 except for the
//          // first reach in a segment
//          ASegment.FSteadyValues[Index].BoundaryValue[FormulaIndex] := 1;
//          AnnotationString := StrAllReachesInASeg;
//          ASegment.FSteadyValues[Index].BoundaryAnnotation[FormulaIndex] := AnnotationString;
//          continue;
//        end;
        UpdateCurrentScreenObject(ASegment.FScreenObject);
        UpdateGlobalLocations(ACell.Column, ACell.Row, ACell.Layer,
          eaBlocks, Model);
        UpdateCurrentSegment(ACell.Segment);
        UpdateCurrentSection(ACell.Section);
        for VarIndex := 0 to UseList.Count - 1 do
        begin
          VarName := UseList[VarIndex];
          VarPosition := VariablePositions[VarIndex];
          if VarPosition >= 0 then
          begin
            Variable := Compiler.Variables[VarPosition];
            DataSetIndex := DataSetIndexes[VarIndex];
            if DataSetIndex >= 0 then
            begin
              ADataSet :=
                Model.DataArrayManager.DataSets[DataSetIndex];
  //            Assert(ADataSet <> self);
              Assert(Model = ADataSet.Model);
              Assert(ADataSet.DataType = Variable.ResultType);
              if ADataSet.Orientation = dsoTop then
              begin
                LayerToUse := 0;
              end
              else
              begin
                LayerToUse := ACell.Layer;
              end;
              if ADataSet.Orientation = dsoFront then
              begin
                RowToUse := 0;
              end
              else
              begin
                RowToUse := ACell.Row;
              end;
              if ADataSet.Orientation = dsoSide then
              begin
                ColToUse := 0;
              end
              else
              begin
                ColToUse := ACell.Column;
              end;

              case Variable.ResultType of
                rdtDouble:
                  begin
                    TRealVariable(Variable).Value :=
                      ADataSet.RealData[LayerToUse, RowToUse,
                      ColToUse];
                  end;
                rdtInteger:
                  begin
                    TIntegerVariable(Variable).Value :=
                      ADataSet.IntegerData[LayerToUse, RowToUse,
                      ColToUse];
                  end;
                rdtBoolean:
                  begin
                    TBooleanVariable(Variable).Value :=
                      ADataSet.BooleanData[LayerToUse, RowToUse,
                      ColToUse];
                  end;
                rdtString:
                  begin
                    TStringVariable(Variable).Value :=
                      ADataSet.StringData[LayerToUse, RowToUse,
                      ColToUse];
                  end;
              else
                Assert(False);
              end;
            end;
          end;
        end;
        Expression.Evaluate;
        ASegment.FSteadyValues[CellIndex].BoundaryValue[FormulaIndex] := Expression.DoubleResult;
        AnnotationString := ASegment.FScreenObject.IntersectAnnotation(Formula, nil);
        ASegment.FSteadyValues[CellIndex].BoundaryAnnotation[FormulaIndex] := AnnotationString;
        if FormulaIndex = 0 then
        begin
          ASegment.FSteadyValues[CellIndex].BoundName := ASegment.FScreenObject.Name;
        end;
  //      UpdateRequiredListData(DataSets, Variables, ACell, AModel);
      end;
    end;

    for CellIndex := 0 to ASegment.ReachCount - 1 do
    begin
      CellValues := ASegment.FSteadyValues[CellIndex];
      CellBottom := Model.DiscretiztionElevation[
        CellValues.Cell.Column, CellValues.Cell.Row, CellValues.Cell.Layer+1];
      if CellValues.StreambedTop - CellValues.StreambedThickness < CellBottom then
      begin
        frmErrorsAndWarnings.AddError(Model, StrTheStreambedMinusThe,
          Format(StrLayerRowColumnNameStreambed, [CellValues.Cell.Layer+1,
          CellValues.Cell.Row+1, CellValues.Cell.Column+1, ASegment.ScreenObject.Name,
          CellValues.StreambedTop, CellValues.StreambedThickness, CellBottom]),
          ASegment.ScreenObject);
      end;
    end;

  finally
    CellList.Free;
    UseList.Free;
    PropertyNames.Free;
    PropertyFormulas.Free;
  end;

end;

//procedure TModflowSFR_MF6_Writer.CheckStage;
//begin
//
//end;

constructor TModflowSFR_MF6_Writer.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FValues := TObjectList.Create;
  FSegments := TSfr6SegmentList.Create;
  FObsList := TSfr6ObservationList.Create;
  DirectObsLines := Model.DirectObservationLines;
  CalculatedObsLines := Model.DerivedObservationLines;
  FileNameLines := Model.FileNameLines;
end;

destructor TModflowSFR_MF6_Writer.Destroy;
begin
  FObsList.Free;
  FSegments.Free;
  FValues.Free;
  inherited;
end;

procedure TModflowSFR_MF6_Writer.Evaluate;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TSfrMf6Boundary;
  Dummy: TStringList;
  ASegment: TSfr6Segment;
  NextReachNumber: Integer;
  ACellList: TValueCellList;
  TimeIndex: Integer;
  CellIndex: Integer;
  ACell: TSfrMf6_Cell;
  CellBottom: Real;
  CellTop: Real;
  MfObs: TModflow6Obs;
  Obs: TSfr6Observation;
  ReachStart: Integer;
  EndTime: Double;
  StartTime: Double;
  SfrMf6Item: TSfrMf6Item;
  NewItem: TSfrMf6Item;
  ItemIndex: Integer;
  Item1: TSfrMf6Item;
  Item2: TSfrMf6Item;
//  StreambedThickness: Double;
begin
//  Grid := Model.Grid;
  FReachCount := 0;
  frmErrorsAndWarnings.RemoveWarningGroup(Model, NoSegmentsWarning);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNoReaches);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrAboveTop);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrBelowBottom);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrStreambedTopElevat);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrDownstreamSFRSegme);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrStreamTimeExtended);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrInactiveStreamPeri);

  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheFollowingPairO);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheFollowingObject);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheFollowingObjectDiv);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheRoughnessIsLes);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrErrorAssigningDive);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheStreambedMinusThe);

  StartTime := Model.ModflowFullStressPeriods.First.StartTime;
  EndTime := Model.ModflowFullStressPeriods.Last.Endtime;

  NextReachNumber := 1;
  ReachStart := 0;
  for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    if not ScreenObject.UsedModels.UsesModel(Model) then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowSfr6Boundary;
    if (Boundary = nil) or not Boundary.Used then
    begin
      Continue;
    end;
    frmProgressMM.AddMessage(Format(StrEvaluatingS, [ScreenObject.Name]));
    
    SfrMf6Item := Boundary.Values.First as TSfrMf6Item;
    if SfrMf6Item.StartTime > Starttime then
    begin
      NewItem := Boundary.Values.Add as TSfrMf6Item;
      NewItem.Assign(SfrMf6Item);
      NewItem.StreamStatus := ssInactive;
      NewItem.Starttime := Starttime;
      NewItem.Endtime := SfrMf6Item.Starttime;
      NewItem.Index := 0;
      frmErrorsAndWarnings.AddWarning(Model, StrInactiveStreamPeri,
        Format(StrInSTheStarting, [ScreenObject.Name]), ScreenObject);
    end;

    SfrMf6Item := Boundary.Values.Last as TSfrMf6Item;
    if SfrMf6Item.Endtime < Endtime then
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrStreamTimeExtended,
        Format(StrIn0sTheLastDe, [ScreenObject.Name, SfrMf6Item.Endtime,
        Endtime]), ScreenObject);
      SfrMf6Item.Endtime := Endtime;
    end;

    for ItemIndex := Boundary.Values.Count - 2 downto 0 do
    begin
      Item1 := Boundary.Values[ItemIndex] as TSfrMf6Item;
      Item2 := Boundary.Values[ItemIndex+1] as TSfrMf6Item;
      if Item1.EndTime < Item2.StartTime then
      begin
        NewItem := Boundary.Values.Add as TSfrMf6Item;
        NewItem.Assign(Item1);
        NewItem.StreamStatus := ssInactive;
        NewItem.Starttime := Item1.EndTime;
        NewItem.Endtime := Item2.Starttime;
        NewItem.Index := ItemIndex+1;
        frmErrorsAndWarnings.AddWarning(Model, StrInactiveStreamPeri,
          Format(StrIn0sThereWasA,
          [ScreenObject.Name, NewItem.Starttime, NewItem.Endtime]), ScreenObject);
      end;
    end;

    Dummy := TStringList.Create;
    try
      Boundary.GetCellValues(FValues, Dummy, Model);
      if FValues.Count > 0 then
      begin
        ASegment := TSfr6Segment.Create(Model);
        FSegments.Add(ASegment);
        ASegment.FReaches.Assign(FValues);
        try
          (FValues as TObjectList).OwnsObjects := False;
          FValues.Clear;
        finally
          (FValues as TObjectList).OwnsObjects := True;
        end;
        ASegment.FSfr6Boundary := Boundary;
        ASegment.FScreenObject := ScreenObject;
        AssignSteadyData(ASegment);
        ASegment.EliminateInactiveReaches;
        ASegment.AssignReachNumbers(NextReachNumber);

        for TimeIndex := 0 to ASegment.FReaches.Count - 1 do
        begin
          ACellList := ASegment.FReaches[TimeIndex];
          for CellIndex := ACellList.Count - 1 downto 0 do
          begin
            ACell := ACellList[CellIndex] as TSfrMF6_Cell;

            CellBottom := Model.DiscretiztionElevation[ACell.Column, ACell.Row, ACell.Layer+1];
            CellTop := Model.DiscretiztionElevation[ACell.Column, ACell.Row, ACell.Layer];
            if ACell.Values.Stage > CellTop then
            begin
              frmErrorsAndWarnings.AddWarning(Model, StrAboveTop,
                Format('%0:d, %1:d, %2:d, %3:s', [ACell.Layer+1,
                ACell.Row+1, ACell.Column+1, ScreenObject.Name]), ScreenObject);
            end;
//            StreambedThickness := ASegment.FSteadyValues[CellIndex].StreambedThickness;
            if ACell.Values.Stage < CellBottom then
            begin
              frmErrorsAndWarnings.AddWarning(Model, StrBelowBottom,
                Format('%0:d, %1:d, %2:d, %3:s', [ACell.Layer+1,
                ACell.Row+1, ACell.Column+1, ScreenObject.Name]), ScreenObject);
            end;

            if ACell.Values.Roughness <= 0 then
            begin
              frmErrorsAndWarnings.AddError(Model, StrTheRoughnessIsLes,
                Format('%0:d, %1:d, %2:d, %3:s', [ACell.Layer+1,
                ACell.Row+1, ACell.Column+1, ScreenObject.Name]), ScreenObject);
            end;

          end;
        end;

        if ObservationsUsed and IsMf6Observation(ScreenObject) then
        begin
          MfObs := ScreenObject.Modflow6Obs;
          Obs.FName := MfObs.Name;
          Obs.FBoundName := ScreenObject.Name;
          Obs.FObsTypes := MfObs.SfrObs;
          Obs.FSfrObsLocation := MfObs.SfrObsLocation;
          Obs.FReachStart := ReachStart;
          Obs.FCount := ASegment.ReachCount;
          Obs.FModflow6Obs := MfObs;
          FObsList.Add(Obs);
        end;
        ReachStart := ReachStart + ASegment.ReachCount;
      end
      else
      begin
        frmErrorsAndWarnings.AddWarning(Model,
          NoSegmentsWarning, ScreenObject.Name, ScreenObject);
      end;
    finally
      Dummy.Free;
    end;
  end;

  AssignConnections;
  FReachCount := NextReachNumber-1;

//  CheckStage;

end;

procedure TModflowSFR_MF6_Writer.EvaluateSteadyData;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TSfrMf6Boundary;
  ASegment: TSfr6Segment;
  Obs: TSfr6Observation;
  MfObs: TModflow6Obs;
  ReachStart: Integer;
begin
  ReachStart := 0;
  for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    if not ScreenObject.UsedModels.UsesModel(Model) then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowSfr6Boundary;
    if (Boundary = nil) or not Boundary.Used then
    begin
      Continue;
    end;
    frmProgressMM.AddMessage(Format(StrEvaluatingS, [ScreenObject.Name]));

    ASegment := TSfr6Segment.Create(Model);
    FSegments.Add(ASegment);
    ASegment.FSfr6Boundary := Boundary;
    ASegment.FScreenObject := ScreenObject;
    AssignSteadyData(ASegment);

    if ObservationsUsed and IsMf6Observation(ScreenObject) then
    begin
      MfObs := ScreenObject.Modflow6Obs;
      Obs.FName := MfObs.Name;
      Obs.FBoundName := ScreenObject.Name;
      Obs.FObsTypes := MfObs.SfrObs;
      Obs.FSfrObsLocation := MfObs.SfrObsLocation;
      Obs.FReachStart := ReachStart;
      Obs.FCount := ASegment.ReachCount;
      Obs.FModflow6Obs := MfObs;
      FObsList.Add(Obs);
    end;
    ReachStart := ReachStart + ASegment.ReachCount;
  end;

end;

class function TModflowSFR_MF6_Writer.Extension: string;
begin
  result := '.sfr';
end;

procedure TModflowSFR_MF6_Writer.InternalUpdateDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  Inflow: TModflowBoundaryDisplayTimeList;
  Rainfall: TModflowBoundaryDisplayTimeList;
  Evaporation: TModflowBoundaryDisplayTimeList;
  Runoff: TModflowBoundaryDisplayTimeList;
  UpstreamFraction: TModflowBoundaryDisplayTimeList;
  Stage: TModflowBoundaryDisplayTimeList;
  Roughness: TModflowBoundaryDisplayTimeList;
  StreamStatus: TModflowBoundaryDisplayTimeList;
  ReachNumber: TModflowBoundaryDisplayTimeList;
  Index: Integer;
  ADisplayList: TModflowBoundaryDisplayTimeList;
  SegmentIndex: Integer;
  Segment: TSfr6Segment;
  StreamStatusComment: string;
  ReachIndex: Integer;
  Reach: TSfrMf6_Cell;
  TimeIndex: Integer;
  DataArray: TModflowBoundaryDisplayDataArray;
  ReachNumberComment: string;
  ACellList: TValueCellList;
  CurrentReachNumber: Integer;
  procedure AssignReachValues;
  var
    TimeIndex: Integer;
    DataArray: TModflowBoundaryDisplayDataArray;
  begin
    Assert(Inflow.Count = 1);
    for TimeIndex := 0 to Inflow.Count - 1 do
    begin
      DataArray := Inflow[TimeIndex]
        as TModflowBoundaryDisplayDataArray;
      DataArray.AddDataValue(Reach.RealAnnotation[InflowPosition, Model],
        Reach.RealValue[InflowPosition, Model],
        Reach.Column, Reach.Row, Reach.Layer);
    end;
    for TimeIndex := 0 to Rainfall.Count - 1 do
    begin
      DataArray := Rainfall[TimeIndex]
        as TModflowBoundaryDisplayDataArray;
      DataArray.AddDataValue(Reach.RealAnnotation[RainfallPosition, Model],
        Reach.RealValue[RainfallPosition, Model],
        Reach.Column, Reach.Row, Reach.Layer);
    end;
    for TimeIndex := 0 to Evaporation.Count - 1 do
    begin
      DataArray := Evaporation[TimeIndex]
        as TModflowBoundaryDisplayDataArray;
      DataArray.AddDataValue(Reach.RealAnnotation[EvaporationPosition, Model],
        Reach.RealValue[EvaporationPosition, Model],
        Reach.Column, Reach.Row, Reach.Layer);
    end;
    for TimeIndex := 0 to Runoff.Count - 1 do
    begin
      DataArray := Runoff[TimeIndex]
        as TModflowBoundaryDisplayDataArray;
      DataArray.AddDataValue(Reach.RealAnnotation[RunoffPosition, Model],
        Reach.RealValue[RunoffPosition, Model],
        Reach.Column, Reach.Row, Reach.Layer);
    end;
    for TimeIndex := 0 to UpstreamFraction.Count - 1 do
    begin
      DataArray := UpstreamFraction[TimeIndex]
        as TModflowBoundaryDisplayDataArray;
      DataArray.AddDataValue(Reach.RealAnnotation[UpstreamFractionPosition, Model],
        Reach.RealValue[UpstreamFractionPosition, Model],
        Reach.Column, Reach.Row, Reach.Layer);
    end;
    for TimeIndex := 0 to Stage.Count - 1 do
    begin
      DataArray := Stage[TimeIndex]
        as TModflowBoundaryDisplayDataArray;
      DataArray.AddDataValue(Reach.RealAnnotation[StagePosition, Model],
        Reach.RealValue[StagePosition, Model],
        Reach.Column, Reach.Row, Reach.Layer);
    end;
    for TimeIndex := 0 to Roughness.Count - 1 do
    begin
      DataArray := Roughness[TimeIndex]
        as TModflowBoundaryDisplayDataArray;
      DataArray.AddDataValue(Reach.RealAnnotation[RoughnessPosition, Model],
        Reach.RealValue[RoughnessPosition, Model],
        Reach.Column, Reach.Row, Reach.Layer);
    end;
    for TimeIndex := 0 to StreamStatus.Count - 1 do
    begin
      DataArray := StreamStatus[TimeIndex]
        as TModflowBoundaryDisplayDataArray;
      DataArray.AddDataValue(StreamStatusComment,
        Ord(Reach.Values.Status),
        Reach.Column, Reach.Row, Reach.Layer);
    end;
    for TimeIndex := 0 to ReachNumber.Count - 1 do
    begin
      DataArray := ReachNumber[TimeIndex]
        as TModflowBoundaryDisplayDataArray;
      DataArray.AddDataValue(ReachNumberComment,
        CurrentReachNumber,
        Reach.Column, Reach.Row, Reach.Layer);
    end;
  end;
begin
  Inflow := TimeLists[InflowPosition];
  Rainfall := TimeLists[RainfallPosition];
  Evaporation := TimeLists[EvaporationPosition];
  Runoff := TimeLists[RunoffPosition];
  UpstreamFraction := TimeLists[UpstreamFractionPosition];
  Stage := TimeLists[StagePosition];
  Roughness := TimeLists[RoughnessPosition];
  StreamStatus := TimeLists[7];
  ReachNumber := TimeLists[8];

  // check that all the time lists contain the same number of times
  // as the first one.
  for Index := 1 to TimeLists.Count - 1 do
  begin
    ADisplayList := TimeLists[Index];
    Assert(Inflow.Count = ADisplayList.Count);
  end;

  CurrentReachNumber := 1;
  for SegmentIndex := 0 to FSegments.Count - 1 do
  begin
    Segment := FSegments[SegmentIndex];
    StreamStatusComment := Format(StrStreamStatusIn, [Segment.FScreenObject.Name]);
    ReachNumberComment := Format(StrReachNumberIn, [Segment.FScreenObject.Name]);

    Assert(Segment.FReaches.Count = 1);
//    for TimeIndex := 0 to Segment.FReaches.Count - 1 do
//    begin
      ACellList := Segment.FReaches[0];
      for ReachIndex := 0 to ACellList.Count -1 do
      begin
        Reach := ACellList[ReachIndex] as TSfrMf6_Cell;
        AssignReachValues;
        Inc(CurrentReachNumber);
      end;
//    end;
  end;

  // Mark all the data arrays and time lists as up to date.
  for Index := 0 to TimeLists.Count - 1 do
  begin
    ADisplayList := TimeLists[Index];
    for TimeIndex := 0 to ADisplayList.Count - 1 do
    begin
      DataArray := ADisplayList[TimeIndex]
        as TModflowBoundaryDisplayDataArray;
      DataArray.UpToDate := True;
    end;
    ADisplayList.SetUpToDate(True);
  end;

end;

function TModflowSFR_MF6_Writer.IsMf6Observation(
  AScreenObject: TScreenObject): Boolean;
var
  MfObs: TModflow6Obs;
begin
  MfObs := AScreenObject.Modflow6Obs;
  Result := (MfObs <> nil) and MfObs.Used and (MfObs.SfrObs <> []);
end;

class function TModflowSFR_MF6_Writer.ObservationExtension: string;
begin
  result := '.ob_sfr';
end;

class function TModflowSFR_MF6_Writer.ObservationOutputExtension: string;
begin
  result := '.ob_sfr_out';
end;

function TModflowSFR_MF6_Writer.ObservationsUsed: Boolean;
begin
  result := (Model.ModelSelection = msModflow2015)
    and Model.ModflowPackages.Mf6ObservationUtility.IsSelected;
end;

function TModflowSFR_MF6_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.SfrModflow6Package;
end;

procedure TModflowSFR_MF6_Writer.UpdateDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;

  Evaluate;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  InternalUpdateDisplay(TimeLists);

end;

procedure TModflowSFR_MF6_Writer.UpdateSteadyData;
var
  ReachLengthDataArray: TModflowBoundaryDisplayDataArray;
  SegmentIndex: Integer;
  ASegment: TSfr6Segment;
  ReachIndex: Integer;
  ReachProp: TSfrMF6ConstantRecord;
  ReachWidthDataArray: TModflowBoundaryDisplayDataArray;
  GradientDataArray: TModflowBoundaryDisplayDataArray;
  StreambedTopDataArray: TModflowBoundaryDisplayDataArray;
  StreambedThicknessDataArray: TModflowBoundaryDisplayDataArray;
  HydraulicConductivityDataArray: TModflowBoundaryDisplayDataArray;
//  RoughnessDataArray: TModflowBoundaryDisplayDataArray;
begin
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;
  EvaluateSteadyData;

  ReachLengthDataArray := Model.DataArrayManager.GetDataSetByName(KReachLengthSFR)
    as TModflowBoundaryDisplayDataArray;
  ReachWidthDataArray := Model.DataArrayManager.GetDataSetByName(KReachWidthSFR6)
    as TModflowBoundaryDisplayDataArray;
  GradientDataArray := Model.DataArrayManager.GetDataSetByName(KGradientSFR6)
    as TModflowBoundaryDisplayDataArray;
  StreambedTopDataArray := Model.DataArrayManager.GetDataSetByName(KStreambedTopSFR6)
    as TModflowBoundaryDisplayDataArray;
  StreambedThicknessDataArray := Model.DataArrayManager.GetDataSetByName(KStreambedThicknessSFR6)
    as TModflowBoundaryDisplayDataArray;
  HydraulicConductivityDataArray := Model.DataArrayManager.GetDataSetByName(KHydraulicConductivitySFR6)
    as TModflowBoundaryDisplayDataArray;
//  RoughnessDataArray := Model.DataArrayManager.GetDataSetByName(KRoughnessSFR6)
//    as TModflowBoundaryDisplayDataArray;

  if ReachLengthDataArray <> nil then
  begin
    ReachLengthDataArray.Clear;

    for SegmentIndex := 0 to FSegments.Count - 1 do
    begin
      ASegment := FSegments[SegmentIndex];
      for ReachIndex := 0 to Length(ASegment.SteadyValues) - 1 do
      begin
        ReachProp := ASegment.SteadyValues[ReachIndex];

        ReachLengthDataArray.AddDataValue(ReachProp.ReachLengthAnnotation,
          ReachProp.ReachLength, ReachProp.Cell.Column,
          ReachProp.Cell.Row, ReachProp.Cell.Layer);
      end;
    end;
    ReachLengthDataArray.ComputeAverage;
    ReachLengthDataArray.UpToDate := True;
  end;

  if ReachWidthDataArray <> nil then
  begin
    ReachWidthDataArray.Clear;

    for SegmentIndex := 0 to FSegments.Count - 1 do
    begin
      ASegment := FSegments[SegmentIndex];
      for ReachIndex := 0 to Length(ASegment.SteadyValues) - 1 do
      begin
        ReachProp := ASegment.SteadyValues[ReachIndex];

        ReachWidthDataArray.AddDataValue(ReachProp.ReachWidthAnnotation,
          ReachProp.ReachWidth, ReachProp.Cell.Column,
          ReachProp.Cell.Row, ReachProp.Cell.Layer);
      end;
    end;
    ReachWidthDataArray.ComputeAverage;
    ReachWidthDataArray.UpToDate := True;
  end;

  if GradientDataArray <> nil then
  begin
    GradientDataArray.Clear;

    for SegmentIndex := 0 to FSegments.Count - 1 do
    begin
      ASegment := FSegments[SegmentIndex];
      for ReachIndex := 0 to Length(ASegment.SteadyValues) - 1 do
      begin
        ReachProp := ASegment.SteadyValues[ReachIndex];

        GradientDataArray.AddDataValue(ReachProp.GradientAnnotation,
          ReachProp.Gradient, ReachProp.Cell.Column,
          ReachProp.Cell.Row, ReachProp.Cell.Layer);
      end;
    end;
    GradientDataArray.ComputeAverage;
    GradientDataArray.UpToDate := True;
  end;

  if StreambedTopDataArray <> nil then
  begin
    StreambedTopDataArray.Clear;

    for SegmentIndex := 0 to FSegments.Count - 1 do
    begin
      ASegment := FSegments[SegmentIndex];
      for ReachIndex := 0 to Length(ASegment.SteadyValues) - 1 do
      begin
        ReachProp := ASegment.SteadyValues[ReachIndex];

        StreambedTopDataArray.AddDataValue(ReachProp.StreambedTopAnnotation,
          ReachProp.StreambedTop, ReachProp.Cell.Column,
          ReachProp.Cell.Row, ReachProp.Cell.Layer);
      end;
    end;
    StreambedTopDataArray.ComputeAverage;
    StreambedTopDataArray.UpToDate := True;
  end;

  if StreambedThicknessDataArray <> nil then
  begin
    StreambedThicknessDataArray.Clear;

    for SegmentIndex := 0 to FSegments.Count - 1 do
    begin
      ASegment := FSegments[SegmentIndex];
      for ReachIndex := 0 to Length(ASegment.SteadyValues) - 1 do
      begin
        ReachProp := ASegment.SteadyValues[ReachIndex];

        StreambedThicknessDataArray.AddDataValue(ReachProp.StreambedThicknessAnnotation,
          ReachProp.StreambedThickness, ReachProp.Cell.Column,
          ReachProp.Cell.Row, ReachProp.Cell.Layer);
      end;
    end;
    StreambedThicknessDataArray.ComputeAverage;
    StreambedThicknessDataArray.UpToDate := True;
  end;

  if HydraulicConductivityDataArray <> nil then
  begin
    HydraulicConductivityDataArray.Clear;

    for SegmentIndex := 0 to FSegments.Count - 1 do
    begin
      ASegment := FSegments[SegmentIndex];
      for ReachIndex := 0 to Length(ASegment.SteadyValues) - 1 do
      begin
        ReachProp := ASegment.SteadyValues[ReachIndex];

        HydraulicConductivityDataArray.AddDataValue(ReachProp.HydraulicConductivityAnnotation,
          ReachProp.HydraulicConductivity, ReachProp.Cell.Column,
          ReachProp.Cell.Row, ReachProp.Cell.Layer);
      end;
    end;
    HydraulicConductivityDataArray.ComputeAverage;
    HydraulicConductivityDataArray.UpToDate := True;
  end;

//  if RoughnessDataArray <> nil then
//  begin
//    RoughnessDataArray.Clear;
//
//    for SegmentIndex := 0 to FSegments.Count - 1 do
//    begin
//      ASegment := FSegments[SegmentIndex];
//      for ReachIndex := 0 to Length(ASegment.SteadyValues) - 1 do
//      begin
//        ReachProp := ASegment.SteadyValues[ReachIndex];
//
//        RoughnessDataArray.AddDataValue(ReachProp.RoughnessAnnotation,
//          ReachProp.Roughness, ReachProp.Cell.Column,
//          ReachProp.Cell.Row, ReachProp.Cell.Layer);
//      end;
//    end;
//    RoughnessDataArray.ComputeAverage;
//    RoughnessDataArray.UpToDate := True;
//  end;

//  HydraulicConductivityDataArray := Model.DataArrayManager.GetDataSetByName(KHydraulicConductivitySFR6)
//  RoughnessDataArray := Model.DataArrayManager.GetDataSetByName(KRoughnessSFR6)


//      WriteFloat(ReachProp.HydraulicConductivity);
//      WriteFloat(ACell.Values.Roughness);


//    Cell: TCellLocation;
//    ReachLength: Double;
//    ReachWidth: Double;
//    Gradient: Double;
//    StreambedTop: Double;
//    StreambedThickness: Double;
//    HydraulicConductivity: Double;
////    Roughness: Double;
////    UpstreamFraction: Double;
//    ConnectedReaches: array of integer;
//    DownstreamDiversions: TDiversionArray;
//    ReachLengthAnnotation: string;
//    ReachWidthAnnotation: string;
//    GradientAnnotation: string;
//    StreambedTopAnnotation: string;
//    StreambedThicknessAnnotation: string;
//    HydraulicConductivityAnnotation: string;
////    RoughnessAnnotation: string;
////    UpstreamFractionAnnotation: string;
//    ConnectedReacheAnnotations: array of string;


end;

procedure TModflowSFR_MF6_Writer.WriteConnections;
var
  SegmentIndex: Integer;
  ASegment: TSfr6Segment;
  ReachIndex: Integer;
  AReach: TSfrMF6ConstantRecord;
  ConnectIndex: Integer;
begin
  WriteBeginConnectionData;
  for SegmentIndex := 0 to FSegments.Count - 1 do
  begin
    ASegment := FSegments[SegmentIndex];
    WriteString(Format('# defined by %s', [ASegment.FScreenObject.Name]));
    NewLine;
    for ReachIndex := 0 to Length(ASegment.SteadyValues) - 1 do
    begin
      AReach := ASegment.SteadyValues[ReachIndex];
      WriteInteger(AReach.ReachNumber);
      for ConnectIndex := 0 to Length(AReach.ConnectedReaches) - 1 do
      begin
        WriteInteger(AReach.ConnectedReaches[ConnectIndex]);
      end;
      NewLine;
    end;
  end;
  WriteEndConnectionData;
end;

procedure TModflowSFR_MF6_Writer.WriteDimensions;
begin
  WriteBeginDimensions;
  WriteString('    NREACHES ');
  WriteInteger(FReachCount);
  NewLine;
  WriteEndDimensions
end;

procedure TModflowSFR_MF6_Writer.WriteDiversions;
var
  SegIndex: Integer;
  ASegment: TSfr6Segment;
  DiverIndex: Integer;
  ADiversion: TSDiversionItem;
  SegmentDictionary: TDictionary<Integer, TSfr6Segment>;
  OtherSegment: TSfr6Segment;
  AReach: TSfrMF6ConstantRecord;
  OtherReach: TSfrMF6ConstantRecord;
begin

  if FDiversionCount > 0 then
  begin
    SegmentDictionary := TDictionary<Integer, TSfr6Segment>.Create;
    try
      for SegIndex := 0 to FSegments .Count - 1 do
      begin
        ASegment := FSegments[SegIndex];
        if not SegmentDictionary.TryGetValue(ASegment.FSfr6Boundary.SegmentNumber,
          OtherSegment) then
        begin
          SegmentDictionary.Add(ASegment.FSfr6Boundary.SegmentNumber, ASegment);
        end;
      end;

      WriteString('BEGIN DIVERSIONS');
      NewLine;
      for SegIndex := 0 to FSegments.Count - 1 do
      begin
        ASegment := FSegments[SegIndex];
        for DiverIndex := 0 to ASegment.FSfr6Boundary.Diversions.Count - 1 do
        begin
          ADiversion := ASegment.FSfr6Boundary.Diversions[DiverIndex];
          if SegmentDictionary.TryGetValue(ADiversion.DownstreamSegment,
            OtherSegment) then
          begin
            AReach := ASegment.Last;
            OtherReach := OtherSegment.First;
            WriteInteger(AReach.ReachNumber);
//            WriteInteger(ADiversion.DiversionNumber);
            WriteInteger(DiverIndex+1);
            WriteInteger(OtherReach.ReachNumber);
            case ADiversion.Priority of
              cpFraction: WriteString(' FRACTION');
              cpExcess: WriteString(' EXCESS');
              cpThreshold: WriteString(' THRESHOLD');
              cpUpTo: WriteString(' UPTO');
              else Assert(False);
            end;
            NewLine;
          end;
        end;
      end;
      WriteString('End DIVERSIONS');
      NewLine;
      NewLine;
    finally
      SegmentDictionary.Free;
    end;
  end;
end;

procedure TModflowSFR_MF6_Writer.WriteFile(const AFileName: string);
var
  Abbreviation: string;
  ObsWriter: TSfrObsWriter;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.ModelSelection = msModflow2015 then
  begin
    Abbreviation := 'SFR6';
  end
  else
  begin
    Exit;
  end;
  if Model.PackageGeneratedExternally(Abbreviation) then
  begin
    Exit;
  end;
  FNameOfFile := FileName(AFileName);
  WriteToNameFile(Abbreviation, -1, FNameOfFile, foInput, Model);
  frmErrorsAndWarnings.BeginUpdate;
  try

    Evaluate;

    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    OpenFile(FileName(FNameOfFile));
    try
      WriteDataSet0;

      frmProgressMM.AddMessage(StrWritingSFROPTION);
      WriteOptions;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingSFRDimens);
      WriteDimensions;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingSFRPackag);
      WritePackageData;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingSFRConnec);
      WriteConnections;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingSFRDivers);
      WriteDiversions;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingSFRStress);
      WriteStressPeriods;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
    finally
      CloseFile
    end;

    if FObsList.Count > 0 then
    begin
      ObsWriter := TSfrObsWriter.Create(Model, etExport, FObsList);
      try
        ObsWriter.WriteFile(ChangeFileExt(FNameOfFile, ObservationExtension));
      finally
        ObsWriter.Free;
      end;
    end;

  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowSFR_MF6_Writer.WriteOptions;
var
  SfrMf6Package: TSfrModflow6PackageSelection;
  stagefile: string;
  budgetfile: string;
  NameOfFile: string;
begin
  WriteBeginOptions;

  PrintListInputOption;

  SfrMf6Package := Model.ModflowPackages.SfrModflow6Package;
  if SfrMf6Package.PrintStage then
  begin
    WriteString('    PRINT_STAGE');
    NewLine;
  end;

  if SfrMf6Package.PrintFlows then
  begin
    WriteString('    PRINT_FLOWS');
    NewLine;
  end;

  WriteString('    BOUNDNAMES');
  NewLine;

  PrintFlowsOption;
  WriteSaveFlowsOption;

  if SfrMf6Package.SaveStageFile then
  begin
    WriteString('    STAGE FILEOUT ');
    stagefile := ChangeFileExt(FNameOfFile, '.stage');
    Model.AddModelOutputFile(stagefile);
    stagefile := ExtractFileName(stagefile);
    WriteString(stagefile);
    NewLine;
  end;

  if SfrMf6Package.SaveBudgetFile then
  begin
    WriteString('    BUDGET FILEOUT ');
    budgetfile := ChangeFileExt(FNameOfFile, '.sfr_budget');
    Model.AddModelOutputFile(budgetfile);
    budgetfile := ExtractFileName(budgetfile);
    WriteString(budgetfile);
    NewLine;
  end;

//  WriteNoNewtown;

  WriteString('    MAXIMUM_ITERATIONS');
  WriteInteger(SfrMf6Package.MaxIteration);
  NewLine;

  WriteString('    MAXIMUM_DEPTH_CHANGE');
  WriteFloat(SfrMf6Package.MaxDepthChange);
  NewLine;

  WriteString('    UNIT_CONVERSION');
  WriteFloat(Model.ModflowOptions.StreamConstant(Model));
  NewLine;

  if FObsList.Count > 0 then
  begin
    WriteString('    OBS6 FILEIN ');
    NameOfFile := ChangeFileExt(FNameOfFile, ObservationExtension);
    Model.AddModelInputFile(NameOfFile);
    NameOfFile := ExtractFileName(NameOfFile);
    WriteString(NameOfFile);
    NewLine;
  end;

  if (MvrWriter <> nil) then
  begin
    if spcSfr in TModflowMvrWriter(MvrWriter).UsedPackages then
    begin
      WriteString('  MOVER');
      NewLine
    end;
  end;


  WriteEndOptions
end;

procedure TModflowSFR_MF6_Writer.WritePackageData;
var
  SegmentIndex: Integer;
  ASegment: TSfr6Segment;
  ReachNumber: Integer;
  ReachIndex: Integer;
  ReachProp: TSfrMF6ConstantRecord;
  ncon: Integer;
  ndiv: Integer;
  ACellList: TValueCellList;
  ACell: TSfrMf6_Cell;
  boundname: string;
begin
  WriteBeginPackageData;
  WriteString('# <rno>   <cellid>  ');
  if not Model.DisvUsed then
  begin
    WriteString('      ');
  end;
  WriteString('<rlen>                <rwid>                <rgrd>                <rtp>                 <rbth>                <rhk>                 <man>                <ncon> <ustrf>                  <ndv> [<aux(naux)>] [<boundname>]');
  NewLine;

  ReachNumber := 0;
  for SegmentIndex := 0 to FSegments.Count - 1 do
  begin
    ASegment := FSegments[SegmentIndex];
    ACellList := ASegment.FReaches[0];
    Assert(ACellList.Count = Length(ASegment.SteadyValues));
    for ReachIndex := 0 to Length(ASegment.SteadyValues) - 1 do
    begin
      if ReachIndex = 0 then
      begin
        WriteString(Format('# defined by %s',
          [ASegment.FScreenObject.Name]));
        NewLine;
      end;
      Inc(ReachNumber);
      WriteInteger(ReachNumber);

      ReachProp := ASegment.SteadyValues[ReachIndex];
      ACell := ACellList[ReachIndex] as TSfrMF6_Cell;

      WriteInteger(ReachProp.Cell.Layer+1);
      if not Model.DisvUsed then
      begin
        WriteInteger(ReachProp.Cell.Row+1);
      end;
      WriteInteger(ReachProp.Cell.Column+1);
      WriteFloat(ReachProp.ReachLength);
      WriteFloat(ReachProp.ReachWidth);
      WriteFloat(ReachProp.Gradient);
      WriteFloat(ReachProp.StreambedTop);
      WriteFloat(ReachProp.StreambedThickness);
      WriteFloat(ReachProp.HydraulicConductivity);
      WriteFloat(ACell.Values.Roughness);
      ncon := Length(ReachProp.ConnectedReaches);
      WriteInteger(ncon);
      if ACell.Values.Status <> ssInactive then
      begin
        WriteFloat(ACell.Values.UpstreamFraction);
      end
      else
      begin
        WriteFloat(0);
      end;
      if ReachIndex = Length(ASegment.SteadyValues) - 1 then
      begin
        ndiv := ASegment.FSfr6Boundary.Diversions.Count;
      end
      else
      begin
        ndiv := 0;
      end;
      WriteInteger(ndiv);
      boundname := ' ' + Copy(ReachProp.BoundName, 1, 40);
      WriteString(boundname);

      NewLine;
    end;
  end;
  WriteEndPackageData;

  Assert(ReachNumber <= FReachCount);
end;

procedure TModflowSFR_MF6_Writer.WriteStressPeriods;
const
  // equivalent to DEM6 in MODFLOW 6.
  Epsilon = 1e-6;
var
  StressPeriodIndex: Integer;
  SegmentIndex: Integer;
  ASegment: TSfr6Segment;
  ACellList: TValueCellList;
  CellIndex: Integer;
  ACell: TSfrMF6_Cell;
  ReachNumber: Integer;
  DivIndex: Integer;
  idv: Integer;
  ReachCount: Integer;
  MoverWriter: TModflowMvrWriter;
  MvrReceiver: TMvrReceiver;
  MvrSource: TMvrRegisterKey;
  UpstreamFractions: array of double;
  SumUpstreamFractions: double;
  AssociatedScreenObjects: array of TScreenObject;
  ReachIndex: Integer;
  AReach: TSfrMF6ConstantRecord;
  ConnectIndex: Integer;
  HasDownstreamReaches: Boolean;
  ConnectedStreams: TStringList;
begin
  if MvrWriter <> nil then
  begin
    MoverWriter := MvrWriter as TModflowMvrWriter;
  end
  else
  begin
    MoverWriter := nil;
  end;
  MvrReceiver.ReceiverKey.ReceiverPackage := rpcSfr;
  SetLength(UpstreamFractions, FReachCount);
  SetLength(AssociatedScreenObjects, FReachCount);
  for StressPeriodIndex := 0 to Model.ModflowFullStressPeriods.Count -1 do
  begin
    frmProgressMM.AddMessage(Format(
      StrWritingSFRStre, [StressPeriodIndex+1]));
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    ReachCount := 0;
    for ReachIndex := 0 to FReachCount - 1 do
    begin
      UpstreamFractions[ReachIndex] := 0;
      AssociatedScreenObjects[ReachIndex] := nil;
    end;

    MvrSource.StressPeriod := StressPeriodIndex;
    MvrReceiver.ReceiverKey.StressPeriod := StressPeriodIndex;

    WriteBeginPeriod(StressPeriodIndex);
    ReachNumber := 0;
    for SegmentIndex := 0 to FSegments.Count - 1 do
    begin
      ASegment := FSegments[SegmentIndex];
      MvrReceiver.ReceiverKey.ScreenObject := ASegment.FScreenObject;

      Assert(ASegment.FReaches.Count = Model.ModflowFullStressPeriods.Count);
      WriteString(Format('# rno sfrsetting (defined by %s)',
        [ASegment.FScreenObject.Name]));
      NewLine;

      ACellList := ASegment.FReaches[StressPeriodIndex];
      SetLength(MvrReceiver.ReceiverValues.StreamCells, ACellList.Count);
      MvrReceiver.ReceiverValues.Index := ReachCount+1;
      for CellIndex := 0 to ACellList.Count - 1 do
      begin
        Inc(ReachCount);

        ACell := ACellList[CellIndex] as TSfrMF6_Cell;
        MvrReceiver.ReceiverValues.StreamCells[CellIndex] := ACell.Values.Cell;
        Inc(ReachNumber);

        AssociatedScreenObjects[ReachNumber-1] := ASegment.ScreenObject;
        if ACell.Values.Status <> ssInactive then
        begin
          UpstreamFractions[ReachNumber-1] := ACell.Values.UpstreamFraction;
        end;

        WriteInteger(ReachNumber);
        WriteString(' STATUS');
        case ACell.Values.Status of
          ssInactive: WriteString(' INACTIVE');
          ssActive: WriteString(' ACTIVE');
          ssSimple: WriteString(' SIMPLE');
        end;
        NewLine;

        WriteInteger(ReachNumber);
        WriteString(' UPSTREAM_FRACTION');
        WriteFloat(ACell.Values.UpstreamFraction);
        NewLine;

        if ACell.Values.Status = ssSimple then
        begin
          WriteInteger(ReachNumber);
          WriteString(' STAGE');
          WriteFloat(ACell.Values.Stage);
          NewLine;
        end;

        if ACell.Values.Status <> ssInactive then
        begin
          WriteInteger(ReachNumber);
          WriteString(' INFLOW');
          WriteFloat(ACell.Values.Inflow);
          NewLine;

          WriteInteger(ReachNumber);
          WriteString(' RAINFALL');
          WriteFloat(ACell.Values.Rainfall);
          NewLine;

          WriteInteger(ReachNumber);
          WriteString(' EVAPORATION');
          WriteFloat(ACell.Values.Evaporation);
          NewLine;

          WriteInteger(ReachNumber);
          WriteString(' RUNOFF');
          WriteFloat(ACell.Values.Runoff);
          NewLine;
        end;

        if ACell.Values.Status = ssActive then
        begin
          WriteInteger(ReachNumber);
          WriteString(' MANNING');
          WriteFloat(ACell.Values.Roughness);
          NewLine;
        end;

        if ACell.Values.Status <> ssInactive then
        begin
          if CellIndex = ACellList.Count - 1 then
          begin
            Assert(Length(ACell.Values.Diversions)
              = ASegment.FSfr6Boundary.Diversions.Count);
            for DivIndex := 0 to Length(ACell.Values.Diversions) - 1 do
            begin
              WriteInteger(ReachNumber);
              WriteString(' DIVERSION');
//              idv := ASegment.FSfr6Boundary.Diversions[DivIndex].DiversionNumber;
              idv := DivIndex+1;
              WriteInteger(idv);
              WriteFloat(ACell.Values.Diversions[DivIndex]);
              NewLine;
            end;
          end;
        end;
        NewLine;

        if ACell.MvrUsed and (MvrWriter <> nil) then
        begin
          MvrSource.Index := ReachNumber;
          MvrSource.SourceKey.MvrIndex := ACell.MvrIndex;
          MvrSource.SourceKey.ScreenObject := ASegment.FScreenObject;
          TModflowMvrWriter(MvrWriter).AddMvrSource(MvrSource);
        end;
      end;

      if MoverWriter <> nil then
      begin
        MoverWriter.AddMvrReceiver(MvrReceiver);
      end;

    end;
    WriteEndPeriod;
    Assert(ReachCount <= FReachCount);

    ConnectedStreams := TStringList.Create;
    try
      for SegmentIndex := 0 to FSegments.Count - 1 do
      begin
        ASegment := FSegments[SegmentIndex];
        ACellList := ASegment.FReaches[StressPeriodIndex];
        Assert(ACellList.Count = Length(ASegment.SteadyValues));
        for ReachIndex := 0 to Length(ASegment.SteadyValues) - 1 do
        begin
          AReach := ASegment.SteadyValues[ReachIndex];
          ACell := ACellList[ReachIndex] as TSfrMF6_Cell;
          if ACell.Values.Status = ssInactive then
          begin
            Continue;
          end;

          HasDownstreamReaches := False;
          ConnectedStreams.Clear;
          SumUpstreamFractions := 0;
          if Length(AReach.ConnectedReaches) > 0 then
          begin
            for ConnectIndex := 0 to Length(AReach.ConnectedReaches) - 1 do
            begin
              if AReach.ConnectedReaches[ConnectIndex] < 0 then
              begin
                HasDownstreamReaches := True;
                SumUpstreamFractions := SumUpstreamFractions
                  + UpstreamFractions[-AReach.ConnectedReaches[ConnectIndex]-1];
                ConnectedStreams.Add(AssociatedScreenObjects[
                  -AReach.ConnectedReaches[ConnectIndex]-1].Name);
              end;
            end;
          end;
          if HasDownstreamReaches and (Abs(SumUpstreamFractions -1) > Epsilon) then
          begin
            frmErrorsAndWarnings.AddError(Model, StrSFRUpstreamValues,
              format(StrTheSumOfTheUpstr,
              [StressPeriodIndex+1, AReach.ReachNumber,
              AssociatedScreenObjects[AReach.ReachNumber-1].Name,
              ConnectedStreams.Text]));
          end;
        end;
      end;
    finally
      ConnectedStreams.Free;
    end;


  end;
end;

{ TSfr6Segment }

procedure TSfr6Segment.AssignReachNumbers(var StartingNumber: Integer);
var
//  TimeIndex: Integer;
//  ACellList: TValueCellList;
//  ActiveDataArray: TDataArray;
  CellIndex: Integer;
//  CellCount: Integer;
//  CellLocation: TCellLocation;
//  ACell: TSfrMF6_Cell;
//  StartIndex: integer;
begin
//  for TimeIndex := 0 to FReaches.Count - 1 do
//  begin
//    StartIndex := StartingNumber;
//    ACellList := FReaches[TimeIndex];
//    for CellIndex := ACellList.Count - 1 downto 0 do
//    begin
//      ACell := ACellList[CellIndex] as TSfrMF6_Cell;
//      ACell.ReachNumber := StartIndex;
//      Inc(StartIndex);
//    end;
//  end;

  for CellIndex := 0 to Length(FSteadyValues) - 1 do
  begin
    FSteadyValues[CellIndex].ReachNumber := StartingNumber;
    Inc(StartingNumber);
  end;
end;

constructor TSfr6Segment.Create(Model: TCustomModel);
begin
  FModel := Model;
  FReaches := TObjectList.Create;
end;

destructor TSfr6Segment.Destroy;
begin
  FReaches.Free;
  inherited;
end;

procedure TSfr6Segment.EliminateInactiveReaches;
var
  TimeIndex: Integer;
  ACellList: TValueCellList;
  IDomainArray: TDataArray;
  CellIndex: Integer;
  CellCount: Integer;
  CellLocation: TCellLocation;
  ACell: TSfrMF6_Cell;
begin
  IDomainArray := FModel.DataArrayManager.GetDataSetByName(K_IDOMAIN);
//  ActiveDataArray := FModel.DataArrayManager.GetDataSetByName(rsActive);
  for TimeIndex := 0 to FReaches.Count - 1 do
  begin
    ACellList := FReaches[TimeIndex];
    for CellIndex := ACellList.Count - 1 downto 0 do
    begin
      ACell := ACellList[CellIndex] as TSfrMF6_Cell;
      if IDomainArray.IntegerData[ACell.Layer, ACell.Row, ACell.Column] <= 0 then
      begin
        ACellList.Delete(CellIndex);
      end;
    end;
  end;

  CellCount := 0;
  for CellIndex := 0 to Length(FSteadyValues) - 1 do
  begin
    if CellCount <> CellIndex then
    begin
      FSteadyValues[CellCount] := FSteadyValues[CellIndex];
    end;
    CellLocation := FSteadyValues[CellIndex].Cell;
    if IDomainArray.IntegerData[
      CellLocation.Layer, CellLocation.Row, CellLocation.Column] >0 then
    begin
      Inc(CellCount);
    end;
  end;
  ReachCount := CellCount;
end;

function TSfr6Segment.GetFirst: TSfrMF6ConstantRecord;
begin
  Result := SteadyValues[0]
end;

function TSfr6Segment.GetLast: TSfrMF6ConstantRecord;
begin
  Result := SteadyValues[Length(SteadyValues)-1]

end;

procedure TSfr6Segment.SetFirst(const Value: TSfrMF6ConstantRecord);
begin
  First;
  FSteadyValues[0] := Value
end;

procedure TSfr6Segment.SetLast(const Value: TSfrMF6ConstantRecord);
begin
  FSteadyValues[Length(SteadyValues)-1] := Value
end;

procedure TSfr6Segment.SetReachCount(const Value: Integer);
begin
  if FReachCount <> Value then
  begin
    FReachCount := Value;
    SetLength(FSteadyValues, Value);
  end;
end;

end.
