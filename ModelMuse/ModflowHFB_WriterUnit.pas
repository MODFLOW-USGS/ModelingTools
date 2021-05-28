unit ModflowHFB_WriterUnit;

interface

uses SysUtils, Classes, Contnrs, PhastModelUnit, CustomModflowWriterUnit,
  ScreenObjectUnit, ModflowPackageSelectionUnit, ModflowParameterUnit,
  EdgeDisplayUnit, GoPhastTypes, System.Generics.Collections, RbwParser;

type
  TModflowHfb_Writer = class;

  TBarrier = class(TCustomModflowGridEdgeFeature)
  protected
    function GetRealAnnotation(Index: integer): string; override;
    function GetRealValue(Index: integer): double; override;
  public
    Parameter: TModflowSteadyParameter;
    HydraulicConductivity: double;
    Thickness: double;
    HydraulicConductivityAnnotation: string;
    ThicknessAnnotation: string;
    FPestHydraulicConductivityName: string;
//    FPestHydraulicConductivitySeriesName: string;
//    FPestHydraulicConductivitySeriesMethod: TPestParamMethod;
    FPestThicknessName: string;
//    FPestThicknessSeriesName: string;
//    FPestThicknessSeriesMethod: TPestParamMethod;
    Function LocationSame(ABarrier: TBarrier): boolean;
    procedure WriteBarrier(Writer: TModflowHfb_Writer; const Comment: string);
  end;

  TDoubleList = TList<Double>;

  TTransientBarrier = class(TCustomModflowGridEdgeFeature)
  private
    FStartTimes: TDoubleList;
    FEndTimes: TDoubleList;
    FParameter: TModflowSteadyParameter;
    FHydraulicConductivities: TDoubleList;
    FThicknesses: TDoubleList;
    FHydraulicConductivityAnnotations: TStringList;
    FThicknessAnnotation: TStringList;
    FDuplicate: Boolean;
    FPriorIndex: Integer;
    FScreenObject: TScreenObject;
    FPestHydraulicConductivityNames: TStringList;
    FPestHydraulicConductivitySeriesNames: TStringList;
    FPestHydraulicConductivitySeriesMethods: TPestMethodList;
    FPestThicknessNames: TStringList;
    FPestThicknessSeriesNames: TStringList;
    FPestThicknessSeriesMethods: TPestMethodList;
    function IdenticalDoubleList(List1, List2: TDoubleList): Boolean;
//    function IdenticalStringList(List1, List2: TStringList): Boolean;
  protected
    function GetRealAnnotation(Index: integer): string; override;
    function GetRealValue(Index: integer): double; override;
    function GetHasData: Boolean; override;
  public
    constructor Create(AModel: TBaseModel; ScreenObject: TScreenObject);
    destructor Destroy; override;
    Function LocationSame(ABarrier: TTransientBarrier): boolean;
    procedure WriteBarrier(Writer: TModflowHfb_Writer; const time: double);
  end;

  TParamList = class(TObject)
  private
    FScreenObjectList: TList<TScreenObject>;
    FBarrierList: TObjectList<TBarrier>;
    FTransientBarrierList: TObjectList<TTransientBarrier>;
    FParam: TModflowSteadyParameter;
    function GetScreenObjectCount: integer;
    function GetScreenObject(Index: integer): TScreenObject;
    function GetBarrier(Index: integer): TBarrier;
    function GetBarrierCount: integer;
    function GetTransientBarrier(Index: integer): TTransientBarrier;
    function GetTransientBarrierCount: integer;
  public
    procedure AddScreenObject(ScreenObject: TScreenObject);
    Constructor Create(Param: TModflowSteadyParameter);
    Destructor Destroy; override;
    property ScreenObjectCount: integer read GetScreenObjectCount;
    property ScreenObjects[Index: integer]: TScreenObject
      read GetScreenObject; default;
    property Parameter: TModflowSteadyParameter read FParam;
    procedure AddBarrier(Barrier: TBarrier);
    property BarrierCount: integer read GetBarrierCount;
    property Barriers[Index: integer]: TBarrier read GetBarrier;
    procedure AddTransientBarrier(Barrier: TTransientBarrier);
    property TransientBarrierCount: integer read GetTransientBarrierCount;
    property TransientBarriers[Index: integer]: TTransientBarrier read GetTransientBarrier;
    procedure RemoveDuplicateTransientBarriers;
  end;

  TTransientExpressions = class(TObject)
    StartTime: double;
    EndTime: double;
    HydraulicConductivityFormula: string;
    HydraulicConductivityExpression: TExpression;
    HydraulicConductivityComment: string;
    HydraulicConductivityValue: double;
    ThicknessFormula: string;
    ThicknessExpression: TExpression;
    ThicknessComment: string;
    ThicknessValue: double;
    HydraulicConductivityPestName: string;
    HydraulicConductivityPestSeriesName: string;
    HydraulicConductivityPestSeriesMethod: TPestParamMethod;
    ThicknessPestName: string;
    ThicknessPestSeriesName: string;
    ThicknessPestSeriesMethod: TPestParamMethod;
  end;

  TTransientExpressionList = TObjectList<TTransientExpressions>;

  TModflowHfb_Writer = class(TCustomPackageWriter)
  private
    NPHFB: integer;
    FParameterScreenObjectList: TStringList;
//    FNameOfFile: string;
    procedure Evaluate;
    procedure EvaluateModflow6;
    {@name fills @link(FParameterScreenObjectList) with the names
    of the HFB parameters except for the first position which is set to ''.
    The Objects property of @link(FParameterScreenObjectList) is filled
    with a newly created @link(TParamList) that contains
    all the @link(TScreenObject)s that are associated with that parameter.
    @link(TScreenObject)s that are not associated with any parameter are placed
    in the @link(TParamList) in the first position.  }
    procedure FillParameterScreenObjectList;
    {@name frees all the @link(TParamList) in FParameterScreenObjectList
    and then clear it. }
    procedure ClearParameterScreenObjectList;
    procedure WriteDataSet1;
    procedure WriteDataSets2and3;
    procedure WriteDataSet4;
    procedure WriteDataSet5;
    procedure WriteDataSet6;
    procedure WriteOptionsMF6;
    procedure WriteDimensionsMF6;
    function CountBarriers: Integer;
    procedure WriteStressPeriodsMF6;
    procedure WriteAStressPeriod(StressPeriodIndex: Integer);
    procedure RemoveAllButOneStressPeriods;
    procedure WriteFileInternal;
    procedure WritePvalMf6;
  protected
    class function Extension: string; override;
    function Package: TModflowPackageSelection; override;
  public
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    Destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
    procedure UpdateDisplay;
  end;


implementation

uses Math, ModflowUnitNumbers, ModflowHfbUnit, OrderedCollectionUnit,
  frmErrorsAndWarningsUnit, ModflowGridUnit, GIS_Functions, 
  frmProgressUnit, frmFormulaErrorsUnit, Forms, ModflowIrregularMeshUnit,
  FastGEO, QuadtreeClass, ModflowTimeUnit, DataSetUnit, ModelMuseUtilities;

resourcestring
  StrInTheHFBPackage = 'In the HFB package, one or more objects do not defin' +
  'e any barriers.';
  StrSIsSupposedToDe = '%s is supposed to define a flow barrier but does not' +
  '.';
  StrInTheHFBPackage1 = 'In the HFB package, a parameter has been used withou' +
  't being defined';
  StrIn0sTheHFBPara = 'In %0:s the HFB parameter has been specified as %1:s ' +
  'but no such parameter has been defined.';
  StrNoDefinedBoundarie = 'No defined boundaries for an HFB parameter';
  StrForTheParameterS = 'For the parameter %s, there are no objects that def' +
  'ine the location of an HFB barrier';
  StrHydraulicConductiv = '(hydraulic conductivity for the HFB package)';
  StrThicknessForTheH = '(thickness for the HFB package)';
  StrEvaluatingHFBPacka = 'Evaluating HFB Package data.';
  StrWritingHFB6Package = 'Writing HFB6 Package input.';
  StrWritingDataSet0 = '  Writing Data Set 0.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
//  StrWritingDataSets2and3 = '  Writing Data Sets 2 and 3.';
//  StrWritingDataSet4 = '  Writing Data Set 4.';
//  StrWritingDataSet5 = '  Writing Data Set 5.';
//  StrWritingDataSet6 = '  Writing Data Set 6.';
  Str0sIn1s = '%0:s in %1:s.';
  StrHFBThicknessOrHyd = 'HFB thickness or hydraulic conductivity less than ' +
  'or equal to zero';
  StrLayer0dRow11 = 'Layer %0:d, Row1 %1:d, Col1 %2:d, Row2: %3:d, Col2 %4:d' +
  '.';
  StrHFBHydraulicConduc = 'HFB Hydraulic Conductivity';
  StrHFBThickness = 'HFB Thickness';
  StrLayer0dCell1 = 'Layer %0:d, Cell 1 %1:d, Cell 2 %2:d';
  StrNoValidHFBTimesH = 'No valid HFB times have been defined';
  StrInSNoValidHori = 'In %s, no valid Horizontal Flow Barrier times have be' +
  'en defined.';
  Str0sMultipliedByTParam = '%0:s multiplied by the parameter value "%1:g" a' +
  'ssigned to the parameter "%2:s."';

{ TModflowHfb_Writer }

function TModflowHfb_Writer.CountBarriers: Integer;
var
  Index: Integer;
  ParamList: TParamList;
begin
  result := 0;
  for Index := 0 to FParameterScreenObjectList.Count - 1 do
  begin
    ParamList := FParameterScreenObjectList.Objects[Index] as TParamList;
    Inc(Result, ParamList.TransientBarrierCount);
  end;
end;

constructor TModflowHfb_Writer.Create(Model: TCustomModel; EvaluationType: TEvaluationType);
begin
  inherited;
  FParameterScreenObjectList:= TStringList.Create;
end;

destructor TModflowHfb_Writer.Destroy;
begin
  ClearParameterScreenObjectList;
  FParameterScreenObjectList.Free;
  inherited;
end;

procedure TModflowHfb_Writer.Evaluate;
var
  ParmIndex: integer;
  ScreenObjectList: TParamList;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  SegmentIndex: integer;
  SegmentList: TList;
  PriorSection: integer;
  ASegment: TCellElementSegment;
  PriorCount: Integer;
  Compiler: TRbwParser;
  DataToCompile: TModflowDataObject;
  DataSetFunction: string;
  HydraulicConductivityExpression: TExpression;
  ThicknessExpression: TExpression;
  PriorSegments: TList;
  SubsequentSegments: TList;
  HydCondComment: string;
  ThicknessComment: string;
  ModflowHfbBoundary: THfbBoundary;
  HydraulicConductivityParam: TModflowSteadyParameter;
  HydraulicConductivityFormula: string;
  ThicknessParam: TModflowSteadyParameter;
  HydraulicConductivityPestName: string;
  ThicknessPestName: string;
  ThicknessFormula: string;
  procedure HandleSection;
  var
    Segment: TCellElementSegment;
    SegmentIndex: Integer;
    MinX, MinY, MaxX, MaxY: double;
    ColumnCenter: double;
    RowCenter: double;
    Grid: TModflowGrid;
    MidCellX, MidCellY: double;
    CrossRow, CrossColumn: integer;
    Barrier: TBarrier;
    Angle: double;
    Start: integer;
//    procedure ApplyHydraulicConductivitySeriesParam;
//    var
//      Param: TModflowSteadyParameter;
//      Method: TPestParamMethod;
//    begin
//      if Barrier.FPestHydraulicConductivitySeriesName <> '' then
//      begin
//        Param := Model.GetPestParameterByName(Barrier.FPestHydraulicConductivitySeriesName);
//        if Param <> nil then
//        begin
//          Method := Barrier.FPestHydraulicConductivitySeriesMethod;
//          case Method of
//            ppmMultiply:
//              begin
//                Barrier.HydraulicConductivity :=
//                  Barrier.HydraulicConductivity * Param.Value;
//              end;
//            ppmAdd:
//              begin
//                Barrier.HydraulicConductivity :=
//                  Barrier.HydraulicConductivity + Param.Value;
//              end;
//          end;
//        end;
//      end;
//    end;
//    procedure ApplyThicknessSeriesParam;
//    var
//      Param: TModflowSteadyParameter;
//      Method: TPestParamMethod;
//    begin
//      if Barrier.FPestThicknessSeriesName <> '' then
//      begin
//        Param := Model.GetPestParameterByName(Barrier.FPestThicknessSeriesName);
//        if Param <> nil then
//        begin
//          Method := Barrier.FPestThicknessSeriesMethod;
//          case Method of
//            ppmMultiply:
//              begin
//                Barrier.Thickness :=
//                  Barrier.Thickness * Param.Value;
//              end;
//            ppmAdd:
//              begin
//                Barrier.Thickness :=
//                  Barrier.Thickness + Param.Value;
//              end;
//          end;
//        end;
//      end;
//    end;
    procedure AssignValues;
    var
      Formula: string;
    begin
      UpdateCurrentScreenObject(ScreenObject);
      UpdateCurrentSegment(Segment);
      UpdateCurrentSection(Segment.SectionIndex);
      UpdateGlobalLocations(Segment.Col, Segment.Row, Segment.Layer, eaBlocks,
        Model);

      try
        HydraulicConductivityExpression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError(ScreenObject.Name,
            StrHydraulicConductiv,
            ScreenObject.ModflowHfbBoundary.HydraulicConductivityFormula,
            E.Message);

          ScreenObject.ModflowHfbBoundary.HydraulicConductivityFormula := '0.';
          Formula := ScreenObject.ModflowHfbBoundary.HydraulicConductivityFormula;
          Compiler.Compile(Formula);
          HydraulicConductivityExpression := Compiler.CurrentExpression;
          HydraulicConductivityExpression.Evaluate;
        end;
      end;
      Barrier.HydraulicConductivity := HydraulicConductivityExpression.DoubleResult;
      Barrier.HydraulicConductivityAnnotation := HydCondComment;
      Barrier.FPestHydraulicConductivityName  := HydraulicConductivityPestName;
//      Barrier.FPestHydraulicConductivitySeriesName  := ModflowHfbBoundary.PestHydraulicConductivityFormula;
//      Barrier.FPestHydraulicConductivitySeriesMethod  := ModflowHfbBoundary.PestHydraulicConductivityMethod;
      Barrier.FPestThicknessName  := ThicknessPestName;
//      Barrier.FPestThicknessSeriesName  := ModflowHfbBoundary.PestThicknessFormula;
//      Barrier.FPestThicknessSeriesMethod  := ModflowHfbBoundary.PestThicknessMethod;

      try
        ThicknessExpression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError(ScreenObject.Name,
            StrThicknessForTheH,
            ScreenObject.ModflowHfbBoundary.ThicknessFormula,
            E.Message);

          ScreenObject.ModflowHfbBoundary.ThicknessFormula := '0.';
          Formula := ScreenObject.ModflowHfbBoundary.ThicknessFormula;
          Compiler.Compile(Formula);
          ThicknessExpression := Compiler.CurrentExpression;
          ThicknessExpression.Evaluate;
        end;
      end;
      Barrier.Thickness := ThicknessExpression.DoubleResult;
      Barrier.ThicknessAnnotation := ThicknessComment;
    end;
    function PreviousBarrierExists: boolean;
    var
      Index: Integer;
      AnotherBarrier: TBarrier;
    begin
      result := False;
      for Index := ScreenObjectList.BarrierCount - 1 downto Start do
      begin
        AnotherBarrier :=ScreenObjectList.Barriers[Index];
        result := Barrier.LocationSame(AnotherBarrier);
        if result then Exit;
      end;
    end;
  begin
    Start := ScreenObjectList.BarrierCount;
    Grid := Model.ModflowGrid;
    for SegmentIndex := 0 to SegmentList.Count - 1 do
    begin
      Segment := SegmentList[SegmentIndex];
      if Model.IsLayerSimulated(Segment.Layer) then
      begin
        MinX := Min(Segment.X1, Segment.X2);
        MaxX := Max(Segment.X1, Segment.X2);
        MinY := Min(Segment.Y1, Segment.Y2);
        MaxY := Max(Segment.Y1, Segment.Y2);
        ColumnCenter := Grid.ColumnCenter(Segment.Col);
        RowCenter := Grid.RowCenter(Segment.Row);
        if (MinX <= ColumnCenter) and (MaxX > ColumnCenter) then
        begin
          MidCellY := (ColumnCenter-Segment.X1)/(Segment.X2-Segment.X1)
            *(Segment.Y2-Segment.Y1) + Segment.Y1;
          if MidCellY > RowCenter then
          begin
            CrossRow := Segment.Row -1;
          end
          else
          begin
            CrossRow := Segment.Row +1;
          end;
          if (CrossRow >=0) and (CrossRow < Grid.RowCount) then
          begin
            Barrier := TBarrier.Create(Model);
            Barrier.FCol1 := Segment.Col;
            Barrier.FCol2 := Segment.Col;
            Barrier.FRow1 := Segment.Row;
            Barrier.FRow2 := CrossRow;
            Barrier.FLayer := Segment.Layer;
            Barrier.Parameter := ScreenObjectList.Parameter;
            if PreviousBarrierExists then
            begin
              Barrier.Free;
            end
            else
            begin
              ScreenObjectList.AddBarrier(Barrier);
              AssignValues;
              if ScreenObject.ModflowHfbBoundary.AdjustmentMethod <> amNone then
              begin
                Angle := ArcTan2(Segment.Y1-Segment.Y2,Segment.X1-Segment.X2);
                case ScreenObject.ModflowHfbBoundary.AdjustmentMethod of
                  amNone:
                    begin
                      Assert(False);
                    end;
                  amAllEdges:
                    begin
                      Barrier.HydraulicConductivity :=
                        Barrier.HydraulicConductivity * Abs(Cos(Angle));
                    end;
                  amNearlyParallel:
                    begin
                      While Angle > Pi/2 do
                      begin
                        Angle := Angle - Pi;
                      end;
                      While Angle < -Pi/2 do
                      begin
                        Angle := Angle + Pi;
                      end;
                      if (Angle > Pi/4) or (Angle < -Pi/4) then
                      begin
                        Barrier.HydraulicConductivity := 0;
                      end
                      else
                      begin
                        Barrier.HydraulicConductivity :=
                          Barrier.HydraulicConductivity/Abs(Cos(Angle));
                      end;
                    end;
                  else Assert(False);
                end;
//                ApplyHydraulicConductivitySeriesParam;
//                ApplyThicknessSeriesParam;
              end;
            end;
          end;
        end;
        if (MinY <= RowCenter) and (MaxY > RowCenter) then
        begin
          MidCellX := (RowCenter-Segment.Y1)/(Segment.Y2-Segment.Y1)
            *(Segment.X2-Segment.X1) + Segment.X1;
          if MidCellX > ColumnCenter then
          begin
            CrossColumn := Segment.Col +1;
          end
          else
          begin
            CrossColumn := Segment.Col -1;
          end;
          if (CrossColumn >=0) and (CrossColumn < Grid.ColumnCount) then
          begin
            Barrier := TBarrier.Create(Model);
            Barrier.FCol1 := Segment.Col;
            Barrier.FCol2 := CrossColumn;
            Barrier.FRow1 := Segment.Row;
            Barrier.FRow2 := Segment.Row;
            Barrier.FLayer := Segment.Layer;
            Barrier.Parameter := ScreenObjectList.Parameter;
            if PreviousBarrierExists then
            begin
              Barrier.Free;
            end
            else
            begin
              ScreenObjectList.AddBarrier(Barrier);
              AssignValues;
              if ScreenObject.ModflowHfbBoundary.AdjustmentMethod <> amNone then
              begin
                Angle := ArcTan2(Segment.X1-Segment.X2,Segment.Y1-Segment.Y2);
                case ScreenObject.ModflowHfbBoundary.AdjustmentMethod of
                  amNone:
                    begin
                      Assert(False);
                    end;
                  amAllEdges:
                    begin
                      Barrier.HydraulicConductivity :=
                        Barrier.HydraulicConductivity * Abs(Cos(Angle));
                    end;
                  amNearlyParallel:
                    begin
                      While Angle > Pi/2 do
                      begin
                        Angle := Angle - Pi;
                      end;
                      While Angle < -Pi/2 do
                      begin
                        Angle := Angle + Pi;
                      end;
                      if (Angle > Pi/4) or (Angle < -Pi/4) then
                      begin
                        Barrier.HydraulicConductivity := 0;
                      end
                      else
                      begin
                        Barrier.HydraulicConductivity :=
                          Barrier.HydraulicConductivity/Abs(Cos(Angle));
                      end;
                    end;
                  else Assert(False);
                end;
//                ApplyHydraulicConductivitySeriesParam;
//                ApplyThicknessSeriesParam;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
begin
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrInTheHFBPackage);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrInTheHFBPackage1);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNoDefinedBoundarie);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrHFBThicknessOrHyd);


    frmProgressMM.AddMessage(StrEvaluatingHFBPacka);
    FillParameterScreenObjectList;
    PriorSegments := TList.Create;
    SubsequentSegments := TList.Create;
    SegmentList := TList.Create;
    try
      for ParmIndex := 0 to FParameterScreenObjectList.Count - 1 do
      begin
        ScreenObjectList := FParameterScreenObjectList.Objects[ParmIndex] as TParamList;
        for ScreenObjectIndex := 0 to ScreenObjectList.ScreenObjectCount - 1 do
        begin
          PriorCount := ScreenObjectList.BarrierCount;
          ScreenObject := ScreenObjectList[ScreenObjectIndex];
          if ScreenObject.Deleted then
          begin
            Continue;
          end;
          if not ScreenObject.UsedModels.UsesModel(Model) then
          begin
            Continue;
          end;

          ModflowHfbBoundary := ScreenObject.ModflowHfbBoundary;
          // Initialize HydraulicConductivityExpression and
          // ThicknessExpression.
          DataToCompile := TModflowDataObject.Create;
          try
            DataToCompile.Compiler := Model.GetCompiler(dsoTop, eaBlocks);
            HydraulicConductivityParam := Model.GetPestParameterByName(
              ModflowHfbBoundary.HydraulicConductivityFormula);
            if HydraulicConductivityParam <> nil then
            begin
              HydraulicConductivityPestName := HydraulicConductivityParam.ParameterName;
              HydraulicConductivityFormula := FortranFloatToStr(HydraulicConductivityParam.Value);
            end
            else
            begin
              HydraulicConductivityPestName := '';
              HydraulicConductivityFormula :=
                ModflowHfbBoundary.HydraulicConductivityFormula;
            end;
            DataToCompile.DataSetFunction := HydraulicConductivityFormula;
//              ModflowHfbBoundary.HydraulicConductivityFormula;
            DataToCompile.AlternateName := StrHFBHydraulicConduc;
            DataToCompile.AlternateDataType := rdtDouble;
            ScreenObject.Delegate.InitializeExpression(
              Compiler, DataSetFunction,
                HydraulicConductivityExpression, nil,
               DataToCompile, Model);
            HydCondComment := Format(Str0sIn1s,
              [ModflowHfbBoundary.HydraulicConductivityFormula,
              ScreenObject.Name]);

            ThicknessParam := Model.GetPestParameterByName(
              ModflowHfbBoundary.ThicknessFormula);
            if ThicknessParam <> nil then
            begin
              ThicknessPestName := ThicknessParam.ParameterName;
              ThicknessFormula := FortranFloatToStr(ThicknessParam.Value);
            end
            else
            begin
              ThicknessPestName := '';
              ThicknessFormula :=
                ModflowHfbBoundary.ThicknessFormula;
            end;
            DataToCompile.DataSetFunction := ThicknessFormula;
//              ModflowHfbBoundary.ThicknessFormula;
            DataToCompile.AlternateName := StrHFBThickness;
//            (ScreenObject.Delegate as TModflowDelegate).InitializeExpression(
            ScreenObject.Delegate.InitializeExpression(
              Compiler, DataSetFunction,
              ThicknessExpression, nil, DataToCompile, Model);
            ThicknessComment := Format(Str0sIn1s,
              [ModflowHfbBoundary.ThicknessFormula,
              ScreenObject.Name]);
          finally
            DataToCompile.Free;
          end;
          PriorSection := -1;
          SegmentList.Clear;
          for SegmentIndex := 0 to ScreenObject.Segments[Model].Count - 1 do
          begin
            ASegment := ScreenObject.Segments[Model][SegmentIndex];
            if ASegment.SectionIndex <> PriorSection then
            begin
              HandleSection;
              PriorSection := ASegment.SectionIndex;
              SegmentList.Clear;
            end;
            SegmentList.Add(ASegment);
          end;
          HandleSection;
          if PriorCount = ScreenObjectList.BarrierCount then
          begin
            frmErrorsAndWarnings.AddWarning(Model, StrInTheHFBPackage,
              Format(StrSIsSupposedToDe, [ScreenObject.Name]), ScreenObject);
          end;
        end;
      end;
    finally
      SegmentList.Free;
      PriorSegments.Free;
      SubsequentSegments.Free;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowHfb_Writer.EvaluateModflow6;
var
  ParmIndex: integer;
  ScreenObjectList: TParamList;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  SegmentIndex: integer;
  SegmentList: TList;
  PriorSection: integer;
  ASegment: TCellElementSegment;
  PriorCount: Integer;
  Compiler: TRbwParser;
  DataToCompile: TModflowDataObject;
  DataSetFunction: string;
  PriorSegments: TList;
  SubsequentSegments: TList;
  TransientData: TTransientExpressionList;
  TimeIndex: Integer;
  HfbItem: THfbItem;
  TransientExpressions: TTransientExpressions;
  CellList: TMFIrregularCell2D_List;
  Quadtree: TRbwQuadTree;
  ABarrier: TTransientBarrier;
  BarrierIndex: Integer;
  X: Integer;
  Y: Integer;
  Points: TQuadPointArray;
  InnerBarrierIndex: Integer;
  OtherBarrier: TTransientBarrier;
  StartTime: Double;
  EndTime: Double;
  IDomainDataSet: TDataArray;
  ModflowHfbBoundary: THfbBoundary;
  HydraulicConductivityParam: TModflowSteadyParameter;
  ThicknessParam: TModflowSteadyParameter;
  procedure GetBarrierIndicies(ABarrier: TTransientBarrier; out X,Y : Integer);
  begin
    X := ABarrier.Layer * Model.RowCount * Model.ColumnCount
      + ABarrier.Row1 * Model.ColumnCount + ABarrier.Col1;
    Y := ABarrier.Layer * Model.RowCount * Model.ColumnCount
      + ABarrier.Row2 * Model.ColumnCount + ABarrier.Col2;
  end;
  procedure AddBarrierToQuadTree(ABarrier: TTransientBarrier);
  var
    X: Integer;
    Y: Integer;
  begin
    GetBarrierIndicies(ABarrier, X,Y);
    Quadtree.AddPoint(X, Y, ABarrier);
    Quadtree.AddPoint(Y, X, ABarrier);
  end;
  procedure HandleSection;
  var
    Segment: TCellElementSegment;
    SegmentIndex: Integer;
    MinX, MinY, MaxX, MaxY: double;
    ColumnCenter: double;
    RowCenter: double;
    Grid: TModflowGrid;
    MidCellX, MidCellY: double;
    CrossRow, CrossColumn: integer;
    Barrier: TTransientBarrier;
    Angle: double;
    TimeIndex: Integer;
    DisvGrid: TModflowDisvGrid;
    TwoDCell: TModflowIrregularCell2D;
    OtherCellIndex: Integer;
    OtherCell: TModflowIrregularCell2D;
    CellSegment: TSegment2D;
    ConnectionAngle: Double;
    procedure AssignValues;
    var
      Formula: string;
      TimeIndex: Integer;
    begin
      UpdateCurrentScreenObject(ScreenObject);
      UpdateCurrentSegment(Segment);
      UpdateCurrentSection(Segment.SectionIndex);
      UpdateGlobalLocations(Segment.Col, Segment.Row, Segment.Layer, eaBlocks,
        Model);

      for TimeIndex := 0 to TransientData.Count -1 do
      begin
        TransientExpressions := TransientData[TimeIndex];
        try
          TransientExpressions.HydraulicConductivityExpression.Evaluate;
        except on E: ERbwParserError do
          begin
            frmFormulaErrors.AddFormulaError(ScreenObject.Name,
              StrHydraulicConductiv,
              TransientExpressions.HydraulicConductivityFormula,
              E.Message);

            TransientExpressions.HydraulicConductivityFormula := '0.';
            Formula := TransientExpressions.HydraulicConductivityFormula;
            Compiler.Compile(Formula);
            TransientExpressions.HydraulicConductivityExpression := Compiler.CurrentExpression;
            TransientExpressions.HydraulicConductivityExpression.Evaluate;
          end;
        end;
        TransientExpressions.HydraulicConductivityValue :=
          TransientExpressions.HydraulicConductivityExpression.DoubleResult;
        if ScreenObjectList.Parameter <> nil then
        begin
          TransientExpressions.HydraulicConductivityValue :=
            TransientExpressions.HydraulicConductivityValue *
            ScreenObjectList.Parameter.Value;
        end;

        try
          TransientExpressions.ThicknessExpression.Evaluate;
        except on E: ERbwParserError do
          begin
            frmFormulaErrors.AddFormulaError(ScreenObject.Name,
              StrThicknessForTheH,
              TransientExpressions.ThicknessFormula,
              E.Message);

            TransientExpressions.ThicknessFormula := '0.';
            Formula := TransientExpressions.ThicknessFormula;
            Compiler.Compile(Formula);
            TransientExpressions.ThicknessExpression := Compiler.CurrentExpression;
            TransientExpressions.ThicknessExpression.Evaluate;
          end;
        end;
        TransientExpressions.ThicknessValue :=
          TransientExpressions.ThicknessExpression.DoubleResult;

        Barrier.FStartTimes.Add(TransientExpressions.StartTime);
        Barrier.FEndTimes.Add(TransientExpressions.EndTime);
        Barrier.FHydraulicConductivities.Add(TransientExpressions.HydraulicConductivityValue);
        Barrier.FThicknesses.Add(TransientExpressions.ThicknessValue);
        Barrier.FHydraulicConductivityAnnotations.Add(TransientExpressions.HydraulicConductivityComment);
        Barrier.FThicknessAnnotation.Add(TransientExpressions.ThicknessComment);

        Barrier.FPestHydraulicConductivityNames.Add(
          TransientExpressions.HydraulicConductivityPestName);
        Barrier.FPestHydraulicConductivitySeriesNames.Add(
          TransientExpressions.HydraulicConductivityPestSeriesName);
        Barrier.FPestHydraulicConductivitySeriesMethods.Add(
          TransientExpressions.HydraulicConductivityPestSeriesMethod);
        Barrier.FPestThicknessNames.Add(
          TransientExpressions.ThicknessPestName);
        Barrier.FPestThicknessSeriesNames.Add(
          TransientExpressions.ThicknessPestSeriesName);
        Barrier.FPestThicknessSeriesMethods.Add(
          TransientExpressions.ThicknessPestSeriesMethod);
      end;
    end;
    procedure ApplyHydraulicConductivitySeriesParam;
    var
      HydraulicConductivitySeriesName: string;
      Param: TModflowSteadyParameter;
      Method: TPestParamMethod;
    begin
      HydraulicConductivitySeriesName :=
        Barrier.FPestHydraulicConductivitySeriesNames[TimeIndex];
      if HydraulicConductivitySeriesName <> '' then
      begin
        Param := Model.GetPestParameterByName(HydraulicConductivitySeriesName);
        if Param <> nil then
        begin
          Method := Barrier.FPestHydraulicConductivitySeriesMethods[TimeIndex];
          case Method of
            ppmMultiply:
              begin
                Barrier.FHydraulicConductivities[TimeIndex] :=
                  Barrier.FHydraulicConductivities[TimeIndex] * Param.Value;
              end;
            ppmAdd:
              begin
                Barrier.FHydraulicConductivities[TimeIndex] :=
                  Barrier.FHydraulicConductivities[TimeIndex] + Param.Value;
              end;
          end;
        end;
      end;
    end;
    procedure ApplyThicknessSeriesParam;
    var
      ThicknessSeriesName: string;
      Param: TModflowSteadyParameter;
      Method: TPestParamMethod;
    begin
      ThicknessSeriesName :=
        Barrier.FPestThicknessSeriesNames[TimeIndex];
      if ThicknessSeriesName <> '' then
      begin
        Param := Model.GetPestParameterByName(ThicknessSeriesName);
        if Param <> nil then
        begin
          Method := Barrier.FPestThicknessSeriesMethods[TimeIndex];
          case Method of
            ppmMultiply:
              begin
                Barrier.FThicknesses[TimeIndex] :=
                  Barrier.FThicknesses[TimeIndex] * Param.Value;
              end;
            ppmAdd:
              begin
                Barrier.FThicknesses[TimeIndex] :=
                  Barrier.FThicknesses[TimeIndex] + Param.Value;
              end;
          end;
        end;
      end;
    end;
  begin
    // Handle one section of a TScreenObject.
    // All the TCellElementSegments in the section will be in SegmentList.
    if not Model.DisvUsed then
    begin
      Grid := Model.ModflowGrid;
      for SegmentIndex := 0 to SegmentList.Count - 1 do
      begin
        Segment := SegmentList[SegmentIndex];

        MinX := Min(Segment.X1, Segment.X2);
        MaxX := Max(Segment.X1, Segment.X2);
        MinY := Min(Segment.Y1, Segment.Y2);
        MaxY := Max(Segment.Y1, Segment.Y2);
        ColumnCenter := Grid.ColumnCenter(Segment.Col);
        RowCenter := Grid.RowCenter(Segment.Row);
        if (MinX <= ColumnCenter) and (MaxX > ColumnCenter) then
        begin
          MidCellY := (ColumnCenter-Segment.X1)/(Segment.X2-Segment.X1)
            *(Segment.Y2-Segment.Y1) + Segment.Y1;
          if MidCellY > RowCenter then
          begin
            CrossRow := Segment.Row -1;
          end
          else
          begin
            CrossRow := Segment.Row +1;
          end;
          if (CrossRow >=0) and (CrossRow < Grid.RowCount)
            and (IDomainDataSet.IntegerData[
            Segment.Layer, CrossRow, Segment.Col] > 0) then
          begin
            Barrier := TTransientBarrier.Create(Model, ScreenObject);
            Barrier.FCol1 := Segment.Col;
            Barrier.FCol2 := Segment.Col;
            Barrier.FRow1 := Segment.Row;
            Barrier.FRow2 := CrossRow;
            Barrier.FLayer := Segment.Layer;
            Barrier.FParameter := ScreenObjectList.Parameter;
            AddBarrierToQuadTree(Barrier);

            ScreenObjectList.AddTransientBarrier(Barrier);
            AssignValues;
//            if ScreenObject.ModflowHfbBoundary.AdjustmentMethod <> amNone then
            begin
              Angle := ArcTan2(Segment.Y1-Segment.Y2,Segment.X1-Segment.X2);
              for TimeIndex := 0 to TransientData.Count -1 do
              begin
                case ScreenObject.ModflowHfbBoundary.AdjustmentMethod of
                  amNone:
                    begin
//                      Assert(False);
                    end;
                  amAllEdges:
                    begin
                      Barrier.FHydraulicConductivities[TimeIndex] :=
                        Barrier.FHydraulicConductivities[TimeIndex]
                        * Abs(Cos(Angle));
                    end;
                  amNearlyParallel:
                    begin
                      While Angle > Pi/2 do
                      begin
                        Angle := Angle - Pi;
                      end;
                      While Angle < -Pi/2 do
                      begin
                        Angle := Angle + Pi;
                      end;
                      if (Angle > Pi/4) or (Angle < -Pi/4) then
                      begin
                        Barrier.FHydraulicConductivities[TimeIndex] := 0;
                      end
                      else
                      begin
                        Barrier.FHydraulicConductivities[TimeIndex] :=
                          Barrier.FHydraulicConductivities[TimeIndex]/Abs(Cos(Angle));
                      end;
                    end;
                  else Assert(False);
                end;
                ApplyHydraulicConductivitySeriesParam;
                ApplyThicknessSeriesParam;
              end;
            end;
          end;
        end;
        if (MinY <= RowCenter) and (MaxY > RowCenter) then
        begin
          MidCellX := (RowCenter-Segment.Y1)/(Segment.Y2-Segment.Y1)
            *(Segment.X2-Segment.X1) + Segment.X1;
          if MidCellX > ColumnCenter then
          begin
            CrossColumn := Segment.Col +1;
          end
          else
          begin
            CrossColumn := Segment.Col -1;
          end;
          if (CrossColumn >=0) and (CrossColumn < Grid.ColumnCount)
            and (IDomainDataSet.IntegerData[
            Segment.Layer, Segment.Row, CrossColumn] > 0) then
          begin
            Barrier := TTransientBarrier.Create(Model, ScreenObject);
            Barrier.FCol1 := Segment.Col;
            Barrier.FCol2 := CrossColumn;
            Barrier.FRow1 := Segment.Row;
            Barrier.FRow2 := Segment.Row;
            Barrier.FLayer := Segment.Layer;
            Barrier.FParameter := ScreenObjectList.Parameter;
            AddBarrierToQuadTree(Barrier);

            ScreenObjectList.AddTransientBarrier(Barrier);
            AssignValues;
//            if ScreenObject.ModflowHfbBoundary.AdjustmentMethod <> amNone then
            begin
              Angle := ArcTan2(Segment.X1-Segment.X2,Segment.Y1-Segment.Y2);
              for TimeIndex := 0 to TransientData.Count -1 do
              begin

                case ScreenObject.ModflowHfbBoundary.AdjustmentMethod of
                  amNone:
                    begin
//                      Assert(False);
                    end;
                  amAllEdges:
                    begin
                      Barrier.FHydraulicConductivities[TimeIndex] :=
                        Barrier.FHydraulicConductivities[TimeIndex] * Abs(Cos(Angle));
                    end;
                  amNearlyParallel:
                    begin
                      While Angle > Pi/2 do
                      begin
                        Angle := Angle - Pi;
                      end;
                      While Angle < -Pi/2 do
                      begin
                        Angle := Angle + Pi;
                      end;
                      if (Angle > Pi/4) or (Angle < -Pi/4) then
                      begin
                        Barrier.FHydraulicConductivities[TimeIndex] := 0;
                      end
                      else
                      begin
                        Barrier.FHydraulicConductivities[TimeIndex] :=
                          Barrier.FHydraulicConductivities[TimeIndex]/Abs(Cos(Angle));
                      end;
                    end;
                  else Assert(False);
                end;
                ApplyHydraulicConductivitySeriesParam;
                ApplyThicknessSeriesParam;
              end;
            end;
          end;
        end;
      end;
    end
    else
    begin
      DisvGrid := Model.DisvGrid;
      CellList := TMFIrregularCell2D_List.Create;
      try
        // It may be necessary to check if cells are active.
        for SegmentIndex := 0 to SegmentList.Count - 1 do
        begin
          Segment := SegmentList[SegmentIndex];
          begin
            TwoDCell := DisvGrid.TwoDGrid.Cells[Segment.Col];
            TwoDCell.GetNeighbors(CellList);
            for OtherCellIndex := 0 to CellList.Count -1 do
            begin
              OtherCell := CellList[OtherCellIndex];
              CellSegment := EquateSegment(TwoDCell.Location, OtherCell.Location);
              if Intersect(CellSegment, Segment.Segment)
                and (IDomainDataSet.IntegerData[
                Segment.Layer, Segment.Row, OtherCell.ElementNumber] > 0) then
              begin
                Barrier := TTransientBarrier.Create(Model, ScreenObject);
                Barrier.FCol1 := Segment.Col;
                Barrier.FCol2 := OtherCell.ElementNumber;
                Barrier.FRow1 := Segment.Row;
                Barrier.FRow2 := Segment.Row;
                Barrier.FLayer := Segment.Layer;
                Barrier.FParameter := ScreenObjectList.Parameter;
                AddBarrierToQuadTree(Barrier);
                ScreenObjectList.AddTransientBarrier(Barrier);
                AssignValues;

//                if ScreenObject.ModflowHfbBoundary.AdjustmentMethod <> amNone then
                begin
                  // Calculate angle of interface between the cells
                  // in the barrier
                  ConnectionAngle := ArcTan2(
                    TwoDCell.Location.X-OtherCell.Location.X,
                    TwoDCell.Location.Y-OtherCell.Location.Y);
                  ConnectionAngle := ConnectionAngle - Pi/2;

                  // Calculate anble between segment and interface
                  // between cells in barrier.
                  Angle := ArcTan2(Segment.X1-Segment.X2,Segment.Y1-Segment.Y2);
                  Angle := Angle - ConnectionAngle;

                  for TimeIndex := 0 to TransientData.Count -1 do
                  begin

                    case ScreenObject.ModflowHfbBoundary.AdjustmentMethod of
                      amNone:
                        begin
//                          Assert(False);
                        end;
                      amAllEdges:
                        begin
                          Barrier.FHydraulicConductivities[TimeIndex] :=
                            Barrier.FHydraulicConductivities[TimeIndex] * Abs(Cos(Angle));
                        end;
                      amNearlyParallel:
                        begin
                          While Angle > Pi/2 do
                          begin
                            Angle := Angle - Pi;
                          end;
                          While Angle < -Pi/2 do
                          begin
                            Angle := Angle + Pi;
                          end;
                          if (Angle > Pi/4) or (Angle < -Pi/4) then
                          begin
                            Barrier.FHydraulicConductivities[TimeIndex] := 0;
                          end
                          else
                          begin
                            Barrier.FHydraulicConductivities[TimeIndex] :=
                              Barrier.FHydraulicConductivities[TimeIndex]/Abs(Cos(Angle));
                          end;
                        end;
                      else Assert(False);
                    end;
                    ApplyHydraulicConductivitySeriesParam;
                    ApplyThicknessSeriesParam;
                  end;
                end

              end;
            end;
          end;
        end;
      finally
        CellList.Free;
      end;

    end;
  end;
begin

  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrInTheHFBPackage);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrInTheHFBPackage1);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNoDefinedBoundarie);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrHFBThicknessOrHyd);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoValidHFBTimesH);

    IDomainDataSet := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);
    frmProgressMM.AddMessage(StrEvaluatingHFBPacka);
    FillParameterScreenObjectList;
    PriorSegments := TList.Create;
    SubsequentSegments := TList.Create;
    SegmentList := TList.Create;
    TransientData := TTransientExpressionList.Create;
    Quadtree := TRbwQuadTree.Create(nil);
    try
      Quadtree.XMax := Model.LayerCount * Model.RowCount * Model.ColumnCount;
      Quadtree.YMax := Quadtree.XMax;

      StartTime := Model.ModflowFullStressPeriods.First.StartTime;
      EndTime := Model.ModflowFullStressPeriods.Last.EndTime;

      for ParmIndex := 0 to FParameterScreenObjectList.Count - 1 do
      begin
        ScreenObjectList := FParameterScreenObjectList.Objects[ParmIndex] as TParamList;
        for ScreenObjectIndex := 0 to ScreenObjectList.ScreenObjectCount - 1 do
        begin
          PriorCount := ScreenObjectList.TransientBarrierCount;
          ScreenObject := ScreenObjectList[ScreenObjectIndex];
          if ScreenObject.Deleted then
          begin
            Continue;
          end;
          if not ScreenObject.UsedModels.UsesModel(Model) then
          begin
            Continue;
          end;
          TransientData.Clear;

          // Initialize HydraulicConductivityExpression and
          // ThicknessExpression.
          DataToCompile := TModflowDataObject.Create;
          try
            DataToCompile.Compiler := Model.GetCompiler(dsoTop, eaBlocks);

            ModflowHfbBoundary := ScreenObject.ModflowHfbBoundary;
            for TimeIndex := 0 to ModflowHfbBoundary.Values.Count - 1 do
            begin
              HfbItem := ModflowHfbBoundary.Values[TimeIndex];

              if HfbItem.EndTime <= StartTime then
              begin
                Continue;
              end;
              if HfbItem.StartTime >= EndTime then
              begin
                Continue;
              end;

              TransientExpressions := TTransientExpressions.Create;
              TransientData.Add(TransientExpressions);

              TransientExpressions.StartTime := HfbItem.StartTime;
              TransientExpressions.EndTime := HfbItem.EndTime;

              HydraulicConductivityParam := Model.GetPestParameterByName(HfbItem.HydraulicConductivity);
              if HydraulicConductivityParam <> nil then
              begin
                TransientExpressions.HydraulicConductivityFormula
                  := FortranFloatToStr(HydraulicConductivityParam.Value);
                TransientExpressions.HydraulicConductivityPestName :=
                  HydraulicConductivityParam.ParameterName;
              end
              else
              begin
                TransientExpressions.HydraulicConductivityFormula
                  := HfbItem.HydraulicConductivity;
                TransientExpressions.HydraulicConductivityPestName := '';
              end;

              ThicknessParam := Model.GetPestParameterByName(HfbItem.Thickness);
              if ThicknessParam <> nil then
              begin
                TransientExpressions.ThicknessFormula
                  := FortranFloatToStr(ThicknessParam.Value);
                TransientExpressions.ThicknessPestName := ThicknessParam.ParameterName;
              end
              else
              begin
                TransientExpressions.ThicknessFormula
                  := HfbItem.Thickness;
                TransientExpressions.ThicknessPestName := '';
              end;

              TransientExpressions.HydraulicConductivityPestSeriesName :=
                ModflowHfbBoundary.PestHydraulicConductivityFormula;
              TransientExpressions.HydraulicConductivityPestSeriesMethod :=
                ModflowHfbBoundary.PestHydraulicConductivityMethod;
              TransientExpressions.ThicknessPestSeriesName :=
                ModflowHfbBoundary.PestThicknessFormula;
              TransientExpressions.ThicknessPestSeriesMethod :=
                ModflowHfbBoundary.PestThicknessMethod;

              DataToCompile.DataSetFunction :=
                TransientExpressions.HydraulicConductivityFormula;
              DataToCompile.AlternateName := StrHFBHydraulicConduc;
              DataToCompile.AlternateDataType := rdtDouble;
              ScreenObject.Delegate.InitializeExpression(
                Compiler, DataSetFunction,
                TransientExpressions.HydraulicConductivityExpression, nil,
                DataToCompile, Model);
              TransientExpressions.HydraulicConductivityComment :=
                Format(Str0sIn1s,
                [HfbItem.HydraulicConductivity,
                ScreenObject.Name]);

              DataToCompile.DataSetFunction := TransientExpressions.ThicknessFormula;
              DataToCompile.AlternateName := StrHFBThickness;
              DataToCompile.AlternateDataType := rdtDouble;
              ScreenObject.Delegate.InitializeExpression(
                Compiler, DataSetFunction,
                TransientExpressions.ThicknessExpression, nil,
                DataToCompile, Model);
              TransientExpressions.ThicknessComment :=
                Format(Str0sIn1s,
                [HfbItem.Thickness,
                ScreenObject.Name]);
            end;


          finally
            DataToCompile.Free;
          end;
          PriorSection := -1;
          // SegmentList will have all the TCellElementSegments in the current
          // section of ScreenObject.
          SegmentList.Clear;
          for SegmentIndex := 0 to ScreenObject.Segments[Model].Count - 1 do
          begin
            ASegment := ScreenObject.Segments[Model][SegmentIndex];
            if IDomainDataSet.IntegerData[ASegment.Layer, ASegment.Row, ASegment.Col] <= 0 then
            begin
              Continue;
            end;
            if ASegment.SectionIndex <> PriorSection then
            begin
              HandleSection;
              PriorSection := ASegment.SectionIndex;
              SegmentList.Clear;
            end;
            SegmentList.Add(ASegment);
          end;
          HandleSection;
          if PriorCount = ScreenObjectList.TransientBarrierCount then
          begin
            frmErrorsAndWarnings.AddWarning(Model, StrInTheHFBPackage,
              Format(StrSIsSupposedToDe, [ScreenObject.Name]), ScreenObject);
          end;
        end;
      end;
      for ParmIndex := FParameterScreenObjectList.Count - 1 downto 0 do
      begin
        ScreenObjectList := FParameterScreenObjectList.Objects[ParmIndex] as TParamList;
        for BarrierIndex := ScreenObjectList.TransientBarrierCount - 1 downto 0 do
        begin
          ABarrier := ScreenObjectList.TransientBarriers[BarrierIndex];
          if ABarrier.FDuplicate then
          begin
            Continue;
          end;
          GetBarrierIndicies(ABarrier, X, Y);
          Quadtree.FindNearestPoints(X, Y, 1, Points);
          Assert(Length(Points) = 1);
          for InnerBarrierIndex := 0 to Length(Points[0].Data) - 1 do
          begin
            OtherBarrier := Points[0].Data[InnerBarrierIndex];
            if (not OtherBarrier.FDuplicate) and (ABarrier <> OtherBarrier) then
            begin
              if ABarrier.LocationSame(OtherBarrier) then
              begin
                OtherBarrier.FDuplicate := True;
              end;
            end;
          end;
        end;
      end;
      for ParmIndex := FParameterScreenObjectList.Count - 1 downto 0 do
      begin
        ScreenObjectList := FParameterScreenObjectList.Objects[ParmIndex] as TParamList;
        ScreenObjectList.RemoveDuplicateTransientBarriers;
      end;
    finally
      SegmentList.Free;
      PriorSegments.Free;
      SubsequentSegments.Free;
      TransientData.Free;
      Quadtree.Free;
    end;


  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

class function TModflowHfb_Writer.Extension: string;
begin
  result := '.hfb';
end;

function TModflowHfb_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.HfbPackage;
end;

procedure TModflowHfb_Writer.UpdateDisplay;
var
  ParmIndex: Integer;
  ScreenObjectList: TParamList;
  BarrierIndex: Integer;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
begin
  if Model.ModelSelection = msModflow2015 then
  begin
    Model.UpdateModflowFullStressPeriods;
    RemoveAllButOneStressPeriods;

    EvaluateModflow6;
  end
  else
  begin
    Evaluate;
  end;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;
  Model.HfbDisplayer.Clear;
  for ParmIndex := 0 to FParameterScreenObjectList.Count - 1 do
  begin
    ScreenObjectList := FParameterScreenObjectList.Objects[ParmIndex]
      as TParamList;
    if Model.ModelSelection = msModflow2015 then
    begin
      for BarrierIndex := 0 to ScreenObjectList.TransientBarrierCount - 1 do
      begin
        Model.HfbDisplayer.Add(ScreenObjectList.TransientBarriers[BarrierIndex]);
      end;
    end
    else
    begin
      for BarrierIndex := 0 to ScreenObjectList.BarrierCount - 1 do
      begin
        Model.HfbDisplayer.Add(ScreenObjectList.Barriers[BarrierIndex]);
      end;
    end;
    for ScreenObjectIndex := 0 to ScreenObjectList.ScreenObjectCount - 1 do
    begin
      ScreenObject := ScreenObjectList.ScreenObjects[ScreenObjectIndex];
      ScreenObject.ModflowHfbBoundary.BoundaryObserver.UpToDate := True;
    end;
  end;
  Model.HfbDisplayer.UpToDate := True;
end;

procedure TModflowHfb_Writer.RemoveAllButOneStressPeriods;
var
  DisplayTime: Double;
  StressPeriodIndex: Integer;
  StressPeriod: TModflowStressPeriod;
begin
  DisplayTime := Model.HfbDisplayer.DisplayTime;
  DisplayTime := Max(DisplayTime, Model.ModflowFullStressPeriods.First.StartTime);
  DisplayTime := Min(DisplayTime, Model.ModflowFullStressPeriods.Last.EndTime);
  for StressPeriodIndex := Model.ModflowFullStressPeriods.Count - 1 downto 0 do
  begin
    StressPeriod := Model.ModflowFullStressPeriods[StressPeriodIndex];
    if (DisplayTime > StressPeriod.EndTime) or (DisplayTime < StressPeriod.StartTime) then
    begin
      Model.ModflowFullStressPeriods.Delete(StressPeriodIndex);
    end
    else if (DisplayTime = StressPeriod.EndTime) and (StressPeriodIndex <> Model.ModflowFullStressPeriods.Count - 1) then
    begin
      Model.ModflowFullStressPeriods.Delete(StressPeriodIndex);
    end;
  end;
  Assert(Model.ModflowFullStressPeriods.Count = 1);
end;

procedure TModflowHfb_Writer.WriteAStressPeriod(StressPeriodIndex: Integer);
var
  ParmIndex: Integer;
  ScreenObjectList: TParamList;
  BarrierIndex: Integer;
  Time: double;
  ABarrier: TTransientBarrier;
begin
  Time := Model.ModflowFullStressPeriods[StressPeriodIndex].StartTime;
  for ParmIndex := 0 to FParameterScreenObjectList.Count - 1 do
  begin
    ScreenObjectList := FParameterScreenObjectList.Objects[ParmIndex] as TParamList;
    for BarrierIndex := 0 to ScreenObjectList.TransientBarrierCount - 1 do
    begin
      ABarrier := ScreenObjectList.TransientBarriers[BarrierIndex];
      ABarrier.WriteBarrier(self, Time);
    end;
  end;
end;

procedure TModflowHfb_Writer.WriteDataSet1;
var
  MXFB: integer;
  NHFBNP: integer;
  Index: Integer;
  ParamList: TParamList;
begin
  NPHFB := 0;
  MXFB := 0;
  // Start at 1 rather than 0 because the list at 0 is for
  // barriers that are not associated with parameters.
  for Index := 1 to FParameterScreenObjectList.Count - 1 do
  begin
    ParamList := FParameterScreenObjectList.Objects[Index] as TParamList;
    if ParamList.BarrierCount > 0 then
    begin
      Inc(NPHFB);
      MXFB := MXFB + ParamList.BarrierCount;
    end;
  end;
  Assert(FParameterScreenObjectList.Count > 0);
  ParamList := FParameterScreenObjectList.Objects[0] as TParamList;
  NHFBNP := ParamList.BarrierCount;
  WriteInteger(NPHFB);
  WriteInteger(MXFB);
  WriteInteger(NHFBNP);
  WriteString(' # Data set 1: NPHFB MXFB NHFBNP');
  NewLine; 
end;

procedure TModflowHfb_Writer.WriteDataSet4;
const
  DataSet4Comment = ' # Data set 4: Layer IROW1 ICOL1 IROW2  ICOL2 Hydchr';
var
  ParamList: TParamList;
  BarrierIndex: Integer;
  Barrier: TBarrier;
begin
  ParamList := FParameterScreenObjectList.Objects[0] as TParamList;
  if ParamList.BarrierCount > 0 then
  begin
    // data set 4
    for BarrierIndex := 0 to ParamList.BarrierCount - 1 do
    begin
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      Barrier := ParamList.Barriers[BarrierIndex];
      Barrier.WriteBarrier(self, DataSet4Comment);
    end;
  end;
end;

procedure TModflowHfb_Writer.WriteDataSet5;
var
  NACTHFB: integer;
begin
  NACTHFB := NPHFB;
  WriteInteger(NACTHFB);
  WriteString(' # Data Set 5: NACTHFB');
  NewLine;
end;

procedure TModflowHfb_Writer.WriteDataSet6;
var
  Index: Integer;
  ParamList: TParamList;
begin
  // Start at 1 rather than 0 because the list at 0 is for
  // barriers that are not associated with parameters.
  for Index := 1 to FParameterScreenObjectList.Count - 1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    ParamList := FParameterScreenObjectList.Objects[Index] as TParamList;
    if ParamList.BarrierCount > 0 then
    begin
      WriteString(ParamList.Parameter.ParameterName);
      WriteString(' # Data set 6: Pname');
      NewLine;
    end;
  end;
end;

procedure TModflowHfb_Writer.WriteDataSets2and3;
const
  DataSet3Comment = ' # Data set 3: Layer IROW1 ICOL1 IROW2  ICOL2 Factor';
var
  Index: Integer;
  ParamList: TParamList;
  BarrierIndex: Integer;
  Barrier: TBarrier;
begin
  // Start at 1 rather than 0 because the list at 0 is for
  // barriers that are not associated with parameters.
  for Index := 1 to FParameterScreenObjectList.Count - 1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    ParamList := FParameterScreenObjectList.Objects[Index] as TParamList;
    if ParamList.BarrierCount > 0 then
    begin
      // data set 2
      WriteString(ParamList.Parameter.ParameterName + ' ');
      WriteString('HFB ');
      WriteFloat(ParamList.Parameter.Value);
      WriteInteger(ParamList.BarrierCount);
      WriteString(' # Data set 2: PARNAM PARTYP Parval NLST');
      NewLine;

      Model.WritePValAndTemplate(ParamList.Parameter.ParameterName,
        ParamList.Parameter.Value, ParamList.Parameter);

      // data set 3
      for BarrierIndex := 0 to ParamList.BarrierCount - 1 do
      begin
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        Barrier := ParamList.Barriers[BarrierIndex];
        Barrier.WriteBarrier(self, DataSet3Comment);
      end;
    end;
  end;
end;

procedure TModflowHfb_Writer.WriteDimensionsMF6;
var
  maxhfb: Integer;
begin
  maxhfb := CountBarriers;
  WriteBeginDimensions;
  WriteString('  MAXHFB');
  WriteInteger(maxhfb);
  NewLine;
  WriteEndDimensions;
end;

procedure TModflowHfb_Writer.WriteFileInternal;
begin
  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingHFB6Package);
    frmProgressMM.AddMessage(StrWritingDataSet0);

    WriteTemplateHeader;

    WriteDataSet0;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    if Model.ModelSelection = msModflow2015 then
    begin
      WriteOptionsMF6;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      WriteDimensionsMF6;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      if not WritingTemplate then
      begin
        WritePvalMf6;
      end;

        WriteStressPeriodsMF6;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
    end
    else
    begin
      frmProgressMM.AddMessage(StrWritingDataSet1);
      WriteDataSet1;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSets2and3);
      WriteDataSets2and3;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet4);
      WriteDataSet4;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet5);
      WriteDataSet5;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet6);
      WriteDataSet6;
    end;
  finally
    CloseFile;
  end;
end;

procedure TModflowHfb_Writer.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrHFB) then
  begin
    Exit;
  end;
  NameOfFile := FileName(AFileName);
  FInputFileName := NameOfFile;
  WriteToNameFile(StrHFB, Model.UnitNumbers.UnitNumber(StrHFB), NameOfFile, foInput, Model);
  if Model.ModelSelection = msModflow2015 then
  begin
    EvaluateModflow6;
  end
  else
  begin
    Evaluate;
  end;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  FNameOfFile := FileName(AFileName);
  WriteFileInternal;
  FInputFileName := FNameOfFile;

  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  // The first item in  FParameterScreenObjectList is for non-parameters
//  if (Model.ModelSelection = msModflow2015) and Model.PestUsed
//    and (FParameterScreenObjectList.Count > 1) then
  if  Model.PestUsed and (FPestParamUsed
    or ((Model.ModelSelection = msModflow2015) and (FParameterScreenObjectList.Count > 0))) then
  begin
    frmErrorsAndWarnings.BeginUpdate;
    try
      FNameOfFile := FNameOfFile + '.tpl';
      WritePestTemplateLine(FNameOfFile);
      WritingTemplate := True;
      WriteFileInternal;

    finally
      frmErrorsAndWarnings.EndUpdate;
    end;
  end;
end;

procedure TModflowHfb_Writer.WriteOptionsMF6;
begin
  WriteBeginOptions;
  try
    PrintListInputOption;
  finally
    WriteEndOptions;
  end;
end;

procedure TModflowHfb_Writer.WritePvalMf6;
var
  ParmIndex: Integer;
  ScreenObjectList: TParamList;
begin
  for ParmIndex := 1 to FParameterScreenObjectList.Count - 1 do
  begin
    ScreenObjectList := FParameterScreenObjectList.Objects[ParmIndex] as TParamList;
    Model.WritePValAndTemplate(ScreenObjectList.Parameter.ParameterName,
      ScreenObjectList.Parameter.Value, ScreenObjectList.Parameter);
  end;

end;

procedure TModflowHfb_Writer.WriteStressPeriodsMF6;
var
  StressPeriodIndex: Integer;
begin
  for StressPeriodIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
  begin
    WriteBeginPeriod(StressPeriodIndex);
    WriteAStressPeriod(StressPeriodIndex);
    WriteEndPeriod
  end;

end;

procedure TModflowHfb_Writer.ClearParameterScreenObjectList;
var
  Index: Integer;
begin
  for Index := 0 to FParameterScreenObjectList.Count - 1 do
  begin
    FParameterScreenObjectList.Objects[Index].Free;
  end;
  FParameterScreenObjectList.Clear;
end;

procedure TModflowHfb_Writer.FillParameterScreenObjectList;
var
  Index: Integer;
  ScreenObject: TScreenObject;
  Boundary: THfbBoundary;
  ParamIndex: Integer;
  Param: TModflowSteadyParameter;
  List: TParamList;
begin
  ClearParameterScreenObjectList;
  FParameterScreenObjectList.AddObject('', TParamList.Create(nil));
  for ParamIndex := 0 to Model.ModflowSteadyParameters.Count-1 do
  begin
    Param := Model.ModflowSteadyParameters[ParamIndex];
    if Param.ParameterType = ptHFB then
    begin
      FParameterScreenObjectList.AddObject(Param.ParameterName,
        TParamList.Create(Param));
    end;
  end;
  for Index := 0 to Model.ScreenObjectCount - 1 do
  begin
    ScreenObject := Model.ScreenObjects[Index];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowHfbBoundary;
    if (Boundary <> nil) then
    begin
      if (Model.ModelSelection = msModflow2015) then
      begin
        if Boundary.UsedMf6 then
        begin
          ParamIndex := FParameterScreenObjectList.IndexOf(Boundary.ParameterName);
          if ParamIndex < 0 then
          begin
            frmErrorsAndWarnings.AddWarning(Model, StrInTheHFBPackage1,
              Format(StrIn0sTheHFBPara, [ScreenObject.Name, Boundary.ParameterName]),
              ScreenObject);
            ParamIndex := 0;
          end;
          List := FParameterScreenObjectList.Objects[ParamIndex] as TParamList;
          List.AddScreenObject(ScreenObject);
        end;
      end
      else
      begin
        if Boundary.Used then
        begin
          ParamIndex := FParameterScreenObjectList.IndexOf(Boundary.ParameterName);
          if ParamIndex < 0 then
          begin
            frmErrorsAndWarnings.AddWarning(Model, StrInTheHFBPackage1,
              Format(StrIn0sTheHFBPara, [ScreenObject.Name, Boundary.ParameterName]),
              ScreenObject);
            ParamIndex := 0;
          end;
          List := FParameterScreenObjectList.Objects[ParamIndex] as TParamList;
          List.AddScreenObject(ScreenObject);
        end;
      end;
    end;
  end;
  for Index := 1 to FParameterScreenObjectList.Count - 1 do
  begin
    List := FParameterScreenObjectList.Objects[Index] as TParamList;
    if List.ScreenObjectCount = 0 then
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrNoDefinedBoundarie,
        Format(StrForTheParameterS, [FParameterScreenObjectList[Index]]) );
    end;
  end;
end;

{ TParamList }

procedure TParamList.AddBarrier(Barrier: TBarrier);
begin
  FBarrierList.Add(Barrier);
end;

procedure TParamList.AddScreenObject(ScreenObject: TScreenObject);
begin
  FScreenObjectList.Add(ScreenObject);
end;

procedure TParamList.AddTransientBarrier(Barrier: TTransientBarrier);
begin
  FTransientBarrierList.Add(Barrier);
end;

constructor TParamList.Create(Param: TModflowSteadyParameter);
begin
  FScreenObjectList:= TList<TScreenObject>.Create;
  FBarrierList := TObjectList<TBarrier>.Create;
  FTransientBarrierList := TObjectList<TTransientBarrier>.Create;
  FParam := Param;
end;

destructor TParamList.Destroy;
begin
  FTransientBarrierList.Free;
  FBarrierList.Free;
  FScreenObjectList.Free;
  inherited;
end;

function TParamList.GetBarrier(Index: integer): TBarrier;
begin
  result := FBarrierList[Index];
end;

function TParamList.GetBarrierCount: integer;
begin
  result := FBarrierList.Count;
end;

function TParamList.GetScreenObject(Index: integer): TScreenObject;
begin
  result := FScreenObjectList[Index];
end;

function TParamList.GetScreenObjectCount: integer;
begin
  result := FScreenObjectList.Count;
end;

function TParamList.GetTransientBarrier(Index: integer): TTransientBarrier;
begin
  result := FTransientBarrierList[Index];
end;

function TParamList.GetTransientBarrierCount: integer;
begin
  result := FTransientBarrierList.Count;
end;

procedure TParamList.RemoveDuplicateTransientBarriers;
var
  BarrierIndex: Integer;
  ABarrier: TTransientBarrier;
begin
  for BarrierIndex := TransientBarrierCount - 1 downto 0 do
  begin
    ABarrier := TransientBarriers[BarrierIndex];
    if ABarrier.FDuplicate then
    begin
      FTransientBarrierList.Delete(BarrierIndex);
    end;
  end;

end;

{ TBarrier }

function TBarrier.GetRealAnnotation(Index: integer): string;
begin
  result := '';
  case Index of
    0:
      begin
        result := HydraulicConductivityAnnotation;
        if Parameter <> nil then
        begin
          result := Format(Str0sMultipliedByTParam,
            [result, Parameter.Value, Parameter.ParameterName]);
//          result := result + ' multiplied by the parameter value "'
//            + FloatToStr(Parameter.Value) + '" assigned to the parameter "'
//            + Parameter.ParameterName + '."';
        end;
      end;
    1:
      begin
        result := ThicknessAnnotation;
      end;
    2:
      begin
        result := 'Hydraulic Conductivity/Thickness';
      end;
    else
      Assert(False);
  end;
end;

function TBarrier.GetRealValue(Index: integer): double;
begin
  result := 0;
  case Index of
    0:
      begin
        result := HydraulicConductivity;
        if Parameter <> nil then
        begin
          result := result * Parameter.Value;
        end;
      end;
    1:
      begin
        result := Thickness;
      end;
    2:
      begin
        if Thickness = 0 then
        begin
          result := 0;
        end
        else
        begin
          result := GetRealValue(0)/Thickness;
        end;
      end;
    else
      Assert(False);
  end;
end;

function TBarrier.LocationSame(ABarrier: TBarrier): boolean;
begin
  result := (Layer = ABarrier.Layer) and (Parameter = ABarrier.Parameter);
  if not Result then
  begin
    Exit;
  end;
  result := ((Col2 = ABarrier.Col1) and (Col1 = ABarrier.Col2))
    or ((Col1 = ABarrier.Col1) and (Col2 = ABarrier.Col2));
  if not Result then
  begin
    Exit;
  end;
  result := ((Row2 = ABarrier.Row1) and (Row1 = ABarrier.Row2))
    or ((Row1 = ABarrier.Row1) and (Row2 = ABarrier.Row2));
end;

procedure TBarrier.WriteBarrier(Writer: TModflowHfb_Writer;
  const Comment: string);
var
  ModelLayer: integer;
  HydraulicConductivityFormula: string;
  ThicknessFormula: string;
  ExtendedTemplateCharacter: string;
//  Method: TPestParamMethod;
begin
  ModelLayer := Writer.Model.
    DataSetLayerToModflowLayer(Layer);
  Writer.WriteInteger(ModelLayer);
  Writer.WriteInteger(Row1+1);
  Writer.WriteInteger(Col1+1);
  Writer.WriteInteger(Row2+1);
  Writer.WriteInteger(Col2+1);
  if Thickness = 0 then
  begin
    Writer.WriteFloat(0);
  end
  else
  begin
    if (not Writer.WritingTemplate) or (Parameter <> nil) then
    begin
      Writer.WriteFloat(HydraulicConductivity/Thickness);
      if Parameter <> nil then
      begin
        Writer.FPestParamUsed := True;
      end
      else if (FPestHydraulicConductivityName <> '')
//        or (FPestHydraulicConductivitySeriesName <> '')
        or (FPestThicknessName <> '')
//        or (FPestThicknessSeriesName <> '')
        then
      begin
        Writer.FPestParamUsed := True;
      end;
    end
    else if Writer.WritingTemplate then
    begin
      if (FPestHydraulicConductivityName <> '')
//        or (FPestHydraulicConductivitySeriesName <> '')
        or (FPestThicknessName <> '')
//        or (FPestThicknessSeriesName <> '')
        then
      begin
        ExtendedTemplateCharacter := Writer.Model.PestProperties.ExtendedTemplateCharacter;
        if (FPestHydraulicConductivityName <> '')
          {or (FPestHydraulicConductivitySeriesName <> '')} then
        begin
//          Method := ppmMultiply;
          HydraulicConductivityFormula := Writer.GetPestTemplateFormula(
            HydraulicConductivity, FPestHydraulicConductivityName,
            '', ppmMultiply, nil, nil);
        end
        else
        begin
          HydraulicConductivityFormula := FortranFloatToStr(HydraulicConductivity)
        end;

        if (FPestThicknessName <> '')
          {or (FPestThicknessSeriesName <> '')} then
        begin
//          Method := FPestThicknessSeriesMethod;
          ThicknessFormula := Writer.GetPestTemplateFormula(
            Thickness, FPestThicknessName,
            '', ppmMultiply, nil, nil);
        end
        else
        begin
          ThicknessFormula := FortranFloatToStr(Thickness)
        end;
        Writer.WriteString(Format(
          ' %0:s                    if((%2:s = 0), 0, (%1:s)/(%2:s))%0:s',
          [ExtendedTemplateCharacter, HydraulicConductivityFormula,
          ThicknessFormula]));
      end
      else
      begin
        Writer.WriteFloat(HydraulicConductivity/Thickness);
      end;
    end
    else
    begin
      Writer.WriteFloat(HydraulicConductivity/Thickness);
      if (FPestHydraulicConductivityName <> '')
//        or (FPestHydraulicConductivitySeriesName <> '')
        or (FPestThicknessName <> '')
//        or (FPestThicknessSeriesName <> '')
        then
      begin
        Writer.FPestParamUsed := True;
      end;
    end;
//    if Writer.WritingTemplate and (Parameter <> nil) then
//    begin
//      Writer.WriteTemplateFormula(Parameter.ParameterName, HydraulicConductivity/Thickness);
//    end
//    else
//    begin
//    end;
  end;
  if (Thickness <= 0) or (HydraulicConductivity <= 0) then
  begin
    frmErrorsAndWarnings.AddWarning(Writer.Model, StrHFBThicknessOrHyd,
      Format(StrLayer0dRow11, [Layer+1, Row1+1, Col1+1, Row2+1, Col2+1]));
  end;
  Writer.WriteString(Comment);
  Writer.NewLine;
end;

{ TTransientBarrier }

constructor TTransientBarrier.Create(AModel: TBaseModel; ScreenObject: TScreenObject);
begin
  inherited Create(AModel);
  FStartTimes := TDoubleList.Create;
  FEndTimes := TDoubleList.Create;
  FHydraulicConductivities := TDoubleList.Create;
  FThicknesses := TDoubleList.Create;
  FHydraulicConductivityAnnotations := TStringList.Create;
  FThicknessAnnotation := TStringList.Create;
  FDuplicate := False;
  FPriorIndex := 0;
  FScreenObject := ScreenObject;
  Assert(ScreenObject <> nil);

  FPestHydraulicConductivityNames := TStringList.Create;
  FPestHydraulicConductivitySeriesNames := TStringList.Create;
  FPestHydraulicConductivitySeriesMethods := TPestMethodList.Create;
  FPestThicknessNames := TStringList.Create;
  FPestThicknessSeriesNames := TStringList.Create;
  FPestThicknessSeriesMethods := TPestMethodList.Create;
end;

destructor TTransientBarrier.Destroy;
begin
  FPestThicknessSeriesMethods.Free;
  FPestThicknessSeriesNames.Free;
  FPestThicknessNames.Free;
  FPestHydraulicConductivitySeriesMethods.Free;
  FPestHydraulicConductivitySeriesNames.Free;
  FPestHydraulicConductivityNames.Free;

  FStartTimes.Free;
  FEndTimes.Free;
  FHydraulicConductivities.Free;
  FThicknesses.Free;
  FHydraulicConductivityAnnotations.Free;
  FThicknessAnnotation.Free;

  inherited;
end;

function TTransientBarrier.GetHasData: Boolean;
begin
  result := FHydraulicConductivities.count > 0;
end;

function TTransientBarrier.GetRealAnnotation(Index: integer): string;
begin
  result := '';
  case Index of
    0:
      begin
        result := FHydraulicConductivityAnnotations[0];
        if FParameter <> nil then
        begin
          result := Format(Str0sMultipliedByTParam,
            [result, FParameter.Value, FParameter.ParameterName]);
//          result := result + ' multiplied by the parameter value "'
//            + FloatToStr(FParameter.Value) + '" assigned to the parameter "'
//            + FParameter.ParameterName + '."';
        end;
      end;
    1:
      begin
        result := FThicknessAnnotation[0];
      end;
    2:
      begin
        result := 'Hydraulic Conductivity/Thickness';
      end;
    else
      Assert(False);
  end;
end;

function TTransientBarrier.GetRealValue(Index: integer): double;
begin
  result := 0;
  case Index of
    0:
      begin
        result := FHydraulicConductivities[0];
//        if Parameter <> nil then
//        begin
//          result := result * Parameter.Value;
//        end;
      end;
    1:
      begin
        result := FThicknesses[0];
      end;
    2:
      begin
        if FThicknesses[0] = 0 then
        begin
          result := 0;
        end
        else
        begin
          result := FHydraulicConductivities[0]/FThicknesses[0];
        end;
      end;
    else
      Assert(False);
  end;
end;

function TTransientBarrier.IdenticalDoubleList(List1,
  List2: TDoubleList): Boolean;
var
  ItemIndex: Integer;
begin
  result := (List1.Count = List2.Count);
  if Result then
  begin
    for ItemIndex := 0 to List1.Count - 1 do
    begin
      result := List1[ItemIndex] = List2[ItemIndex];
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

//function TTransientBarrier.IdenticalStringList(List1,
//  List2: TStringList): Boolean;
//var
//  ItemIndex: Integer;
//begin
//  result := (List1.Count = List2.Count);
//  if Result then
//  begin
//    for ItemIndex := 0 to List1.Count - 1 do
//    begin
//      result := List1[ItemIndex] = List2[ItemIndex];
//      if not result then
//      begin
//        Exit;
//      end;
//    end;
//  end;
//end;

function TTransientBarrier.LocationSame(ABarrier: TTransientBarrier): boolean;
begin
  result := (Layer = ABarrier.Layer) and (FParameter = ABarrier.FParameter);
  if not Result then
  begin
    Exit;
  end;
  result := ((Col2 = ABarrier.Col1) and (Col1 = ABarrier.Col2))
    or ((Col1 = ABarrier.Col1) and (Col2 = ABarrier.Col2));
  if not Result then
  begin
    Exit;
  end;
  result := ((Row2 = ABarrier.Row1) and (Row1 = ABarrier.Row2))
    or ((Row1 = ABarrier.Row1) and (Row2 = ABarrier.Row2));
  if not Result then
  begin
    Exit;
  end;
  Result := IdenticalDoubleList(FStartTimes, ABarrier.FStartTimes)
    and  IdenticalDoubleList(FEndTimes, ABarrier.FEndTimes)
//    and  IdenticalDoubleList(HydraulicConductivities, ABarrier.HydraulicConductivities)
//    and  IdenticalDoubleList(Thicknesses, ABarrier.Thicknesses)
end;

procedure TTransientBarrier.WriteBarrier(Writer: TModflowHfb_Writer;
  const time: double);
var
  ModelLayer: Integer;
  Thickness: Double;
  HydraulicConductivity: Double;
  TimeIndex: Integer;
  PestHydraulicConductivityName: string;
  PestHydraulicConductivitySeriesName: string;
  PestThicknessName: string;
  PestThicknessSeriesName: string;
  HydraulicConductivityFormula: string;
  ThicknessFormula: string;
  ExtendedTemplateCharacter: string;
  Method: TPestParamMethod;
begin
    { TODO -cPEST : Add PEST support for PEST here }
    // handle pest parameter
    // handle multiply or add
  for TimeIndex := FPriorIndex to FStartTimes.Count - 1 do
  begin
    if FStartTimes[TimeIndex] <= time then
    begin
      FPriorIndex := TimeIndex;
    end;
    if FEndTimes[TimeIndex] > time then
    begin
      Break;
    end;
  end;
  if FStartTimes.Count = 0 then
  begin
    frmErrorsAndWarnings.AddError(Writer.Model, StrNoValidHFBTimesH,
      Format(StrInSNoValidHori, [FScreenObject.Name]), FScreenObject);
    Exit;
  end;
  if (FStartTimes[FPriorIndex] <= time) and (FEndTimes[FPriorIndex] > time) then
  begin
    ModelLayer := Writer.Model.
      DataSetLayerToModflowLayer(Layer);
    if Writer.Model.DisvUsed then
    begin
      Writer.WriteInteger(ModelLayer);
      Writer.WriteInteger(Col1+1);
      Writer.WriteInteger(ModelLayer);
      Writer.WriteInteger(Col2+1);
    end
    else
    begin
      Writer.WriteInteger(ModelLayer);
      Writer.WriteInteger(Row1+1);
      Writer.WriteInteger(Col1+1);
      Writer.WriteInteger(ModelLayer);
      Writer.WriteInteger(Row2+1);
      Writer.WriteInteger(Col2+1);
    end;

    Thickness := FThicknesses[FPriorIndex];
    HydraulicConductivity := FHydraulicConductivities[FPriorIndex];
    if Thickness = 0 then
    begin
      Writer.WriteFloat(0);
    end
    else
    begin
      if Writer.WritingTemplate then
      begin
        if (FParameter <> nil) then
        begin
          if FParameter.Value = 0 then
          begin
            Writer.WriteFloat(0);
          end
          else
          begin
            Writer.WriteTemplateFormula(FParameter.ParameterName,
              HydraulicConductivity/Thickness/FParameter.Value, ppmMultiply);
          end;
        end
        else
        begin
          PestHydraulicConductivityName := FPestHydraulicConductivityNames[FPriorIndex];
          PestHydraulicConductivitySeriesName := FPestHydraulicConductivitySeriesNames[FPriorIndex];
          PestThicknessName := FPestThicknessNames[FPriorIndex];
          PestThicknessSeriesName := FPestThicknessSeriesNames[FPriorIndex];

          if (PestHydraulicConductivityName <> '')
            or (PestHydraulicConductivitySeriesName <> '')
            or (PestThicknessName <> '')
            or (PestThicknessSeriesName <> '')
            then
          begin
            ExtendedTemplateCharacter := Writer.Model.PestProperties.ExtendedTemplateCharacter;
            if (PestHydraulicConductivityName <> '')
              or (PestHydraulicConductivitySeriesName <> '') then
            begin
              Method := FPestHydraulicConductivitySeriesMethods[FPriorIndex];
              HydraulicConductivityFormula := Writer.GetPestTemplateFormula(
                HydraulicConductivity, PestHydraulicConductivityName,
                PestHydraulicConductivitySeriesName, Method, nil, nil);
            end
            else
            begin
              HydraulicConductivityFormula := FortranFloatToStr(HydraulicConductivity)
            end;

            if (PestThicknessName <> '')
              or (PestThicknessSeriesName <> '') then
            begin
              Method := FPestThicknessSeriesMethods[FPriorIndex];
              ThicknessFormula := Writer.GetPestTemplateFormula(
                Thickness, PestThicknessName,
                PestThicknessSeriesName, Method, nil, nil);
            end
            else
            begin
              ThicknessFormula := FortranFloatToStr(Thickness)
            end;
            Writer.WriteString(Format(
              ' %0:s                    if((%2:s = 0), 0, (%1:s)/(%2:s))%0:s',
              [ExtendedTemplateCharacter, HydraulicConductivityFormula,
              ThicknessFormula]));
          end
          else
          begin
            Writer.WriteFloat(HydraulicConductivity/Thickness);
          end;
        end
      end
      else
      begin
        Writer.WriteFloat(HydraulicConductivity/Thickness);
        if (FParameter <> nil) then
        begin
          Writer.FPestParamUsed := True;
        end
        else
        begin
          PestHydraulicConductivityName := FPestHydraulicConductivityNames[FPriorIndex];
          PestHydraulicConductivitySeriesName := FPestHydraulicConductivitySeriesNames[FPriorIndex];
          PestThicknessName := FPestThicknessNames[FPriorIndex];
          PestThicknessSeriesName := FPestThicknessSeriesNames[FPriorIndex];

          if (PestHydraulicConductivityName <> '')
            or (PestHydraulicConductivitySeriesName <> '')
            or (PestThicknessName <> '')
            or (PestThicknessSeriesName <> '')
            then
          begin
            Writer.FPestParamUsed := True;
          end;

        end;
      end;

    end;
    if (Thickness <= 0) or (HydraulicConductivity <= 0) then
    begin
      if Writer.Model.DisvUsed then
      begin
        frmErrorsAndWarnings.AddWarning(Writer.Model, StrHFBThicknessOrHyd,
          Format(StrLayer0dCell1, [Layer+1, Col1+1, Col2+1]));
      end
      else
      begin
        frmErrorsAndWarnings.AddWarning(Writer.Model, StrHFBThicknessOrHyd,
          Format(StrLayer0dRow11, [Layer+1, Row1+1, Col1+1, Row2+1, Col2+1]));
      end;
    end;
    Writer.WriteString(Format(' # (%0:s)/(%1:s)', [
      FHydraulicConductivityAnnotations[FPriorIndex],
      FThicknessAnnotation[FPriorIndex]]));
    Writer.NewLine;

  end;
end;

end.
