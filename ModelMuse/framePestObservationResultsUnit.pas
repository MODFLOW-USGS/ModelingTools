unit framePestObservationResultsUnit;

interface

uses
  GoPhastTypes, OrderedCollectionUnit,
  PhastModelUnit,
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Grids, RbwDataGrid4, JvSpin, JvExControls,
  JvColorBox, JvColorButton, frameDisplayLimitUnit, Vcl.Mask, JvExMask,
  JvToolEdit, QuadTreeClass, ObsInterfaceUnit, PestObservationResults,
  frmCustomGoPhastUnit, System.Generics.Collections, UndoItems,
  ScreenObjectUnit, System.Generics.Defaults;

type
  TUndoType = (utChange, utImport);
  TPestObsColumns = (pocName, pocGroup, pocObject, pocTime, pocMeasured,
    pocModeled, pocResidual, pocWeight, pocWtMeas, pocWtMod, pocWtRes,
    pocMeasSD, pocNaturalWeight, pocOriginalOrder);

  TCustomUndoChangePestObsResults = class(TCustomUndo)
  private
    FNewObservations: TPestObsCollection;
    FOldObservations: TPestObsCollection;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure ApplyChange(Observations: TPestObsCollection);
    procedure UpdateGUI;
  public
    procedure DoCommand; override;
    procedure Undo; override;
  public
    constructor Create(var Observations: TPestObsCollection);
    Destructor Destroy; override;
  end;

  TUndoChangePestObsResults = class(TCustomUndoChangePestObsResults)
  protected
    function Description: string; override;
  end;

  TUndoImportChangePestObsResults = class(TCustomUndoChangePestObsResults)
  protected
    function Description: string; override;
  end;


  TframePestObservationResults = class(TFrame)
    pnlBottom: TPanel;
    lblRMS: TLabel;
    comboModels: TComboBox;
    pgcObservations: TPageControl;
    tabControls: TTabSheet;
    lblNegativeColor: TLabel;
    lblColorPositive: TLabel;
    lblMaxSymbolSize: TLabel;
    lblHeadObsResults: TLabel;
    flnmedHeadObsResults: TJvFilenameEdit;
    grpbxFilter: TGroupBox;
    lblMaximumTime: TLabel;
    lblMaxResidual: TLabel;
    lblMinimumTime: TLabel;
    lblMinResidual: TLabel;
    lblMinWeightedResidual: TLabel;
    lblMaxWeightedResidual: TLabel;
    framelmtMinimumTime: TframeDisplayLimit;
    framelmtMaxResidual: TframeDisplayLimit;
    framelmtMaximumTime: TframeDisplayLimit;
    framelmtMinResidual: TframeDisplayLimit;
    framelmtMinWeightedResidual: TframeDisplayLimit;
    framelmtMaxWeightedResidual: TframeDisplayLimit;
    clrbtnNegative: TJvColorButton;
    clrbtnPositive: TJvColorButton;
    spinSymbolSize: TJvSpinEdit;
    cbShow: TCheckBox;
    tabValues: TTabSheet;
    rdgPestObs: TRbwDataGrid4;
    pnlValueControls: TPanel;
    btnCopy: TButton;
    btnHightlightObjects: TButton;
    btnRestore: TButton;
    tabLegend: TTabSheet;
    shpMax: TShape;
    shpHalfMax: TShape;
    lblMax: TLabel;
    lblHalfMax: TLabel;
    tabGraph: TTabSheet;
    pbObservations: TPaintBox;
    pnlGraphControls: TPanel;
    lblGraphInstructions: TLabel;
    rgGraphType: TRadioGroup;
    qtreeObservations: TRbwQuadTree;
    rgDrawChoice: TRadioGroup;
    procedure flnmedHeadObsResultsChange(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnHightlightObjectsClick(Sender: TObject);
    procedure spinSymbolSizeChange(Sender: TObject);
    procedure rgGraphTypeClick(Sender: TObject);
    procedure pgcObservationsChange(Sender: TObject);
    procedure pbObservationsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbObservationsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbObservationsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbObservationsPaint(Sender: TObject);
    procedure rdgPestObsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnRestoreClick(Sender: TObject);
  public
    procedure UpdateChildModels;
  protected
    procedure UpdateObsLinkList;
  private
    FObservations: TPestObsCollection;
    FGettingData: Boolean;
    FImportResult: Boolean;
    FUndoType: TUndoType;
    FSelectedObsItem: TPestObsResult;
    procedure FillTable;
    procedure InitializeTableHeaders;
    function GetSelectedObjectFromGrid: TScreenObject;
    function GetSelectedObjectFromGraph: TScreenObject;
    procedure PlotValues;
    procedure TestForNewFile;
    { Private declarations }
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure GetData;
    procedure SetData;
    { Public declarations }
  end;

implementation

uses
  PestObsUnit, frmGoPhastUnit, UndoItemsScreenObjects, frmGoToUnit, xygraph,
  System.Math, Contnrs;

type
  TGridCrack = class(TRbwDataGrid4);
  TCompareMethod = class(TObject)
    Method: TPestObsColumns;
  end;
var
  Data: Tdatatype;
  SortOrder: TList = nil;

resourcestring
  StrTheFileSHasADi = 'The file %s has a different date than the imported re' +
  'sults. Do you want to import it?';
  StrRootMeanSquareRes = 'Root Mean Square Weighted Residual = %g';
  StrImportPESTResults = 'import PEST results';
  StrChangePESTResultO = 'change PEST result options';

{$R *.dfm}

function CompareTextItems(Item1, Item2: Pointer; Column: TPestObsColumns): Integer;
var
  P1, P2: TPestObsResult;
  Splitter1: TStringList;
  Splitter2: TStringList;
  index: Integer;
  Part1: string;
  Part2: string;
  Value1: Integer;
  Value2: Integer;
  Text1: string;
  Text2: string;
begin
  P1 := Item1;
  P2 := Item2;
  case Column of
    pocName:
      begin
        Text1 := P1.Name;
        Text2 := P2.Name;
      end;
    pocGroup:
      begin
        Text1 := P1.GroupName;
        Text2 := P2.GroupName;
      end;
    pocObject:
      begin
        Text1 := P1.ObjectName;
        Text2 := P2.ObjectName;
      end;
    else
      begin
        Text1 := '';
        Text2 := '';
        Assert(False);
      end;
  end;
  Splitter1 := TStringList.Create;
  Splitter2 := TStringList.Create;
  try
    Splitter1.Delimiter := '_';
    Splitter2.Delimiter := '_';
    Splitter1.DelimitedText := Text1;
    Splitter2.DelimitedText := Text1;
    for index := 0 to Min(Splitter1.Count, Splitter2.Count)  - 1 do
    begin
      Part1 := Splitter1[index];
      Part2 := Splitter2[index];
      if TryStrToInt(Part1, Value1) and TryStrToInt(Part2, Value2) then
      begin
        result := Value1-Value2
      end
      else
      begin
        result := AnsiCompareText(Part1, Part2);
      end;
      if result <> 0 then
      begin
        Exit;
      end;
    end;
    result := AnsiCompareText(Text1, Text2);
  finally
    Splitter1.Free;
    Splitter2.Free;
  end;
end;

function CompareRealValue(Item1, Item2: Pointer; Column: TPestObsColumns): Integer;
var
  P1, P2: TPestObsResult;
  Value1: Double;
  Value2: Double;
begin
  P1 := Item1;
  P2 := Item2;
  case Column of
    pocTime:
      begin
        Value1 := P1.Time;
        Value2 := P2.Time;
      end;
    pocMeasured:
      begin
        Value1 := P1.Measured;
        Value2 := P2.Measured;
      end;
    pocModeled:
      begin
        Value1 := P1.Modeled;
        Value2 := P2.Modeled;
      end;
    pocResidual:
      begin
        Value1 := P1.Residual;
        Value2 := P2.Residual;
      end;
    pocWeight:
      begin
        Value1 := P1.Weight;
        Value2 := P2.Weight;
      end;
    pocWtMeas:
      begin
        Value1 := P1.WeightedMeasured;
        Value2 := P2.WeightedMeasured;
      end;
    pocWtMod:
      begin
        Value1 := P1.WeightedModeled;
        Value2 := P2.WeightedModeled;
      end;
    pocWtRes:
      begin
        Value1 := P1.WeightedResidual;
        Value2 := P2.WeightedResidual;
      end;
    pocMeasSD:
      begin
        Value1 := P1.MeasurementStdDeviation;
        Value2 := P2.MeasurementStdDeviation;
      end;
    pocNaturalWeight:
      begin
        Value1 := P1.NaturalWeight;
        Value2 := P2.NaturalWeight;
      end;
    else
      begin
        Value1 := 0;
        Value2 := 0;
        Assert(False);
      end;
  end;
  result := Sign(Value1 - Value2);
end;

function CompareOriginalOrder(Item1, Item2: Pointer): Integer;
var
  P1, P2: TPestObsResult;
begin
  P1 := Item1;
  P2 := Item2;
  result := Sign(P1.OriginalOrder - P2.OriginalOrder);
end;

function CompareObservations(Item1, Item2: Pointer): Integer;
var
  Index: Integer;
  CM: TCompareMethod;
begin
  result := 0;
  for Index := 0 to SortOrder.Count - 1 do
  begin
    CM := SortOrder[Index];
    case CM.Method of
      pocName, pocGroup, pocObject:
        begin
          result := CompareTextItems(Item1, Item2, CM.Method)
        end;
      pocTime, pocMeasured, pocModeled, pocResidual, pocWeight,
        pocWtMeas, pocWtMod, pocWtRes, pocMeasSD, pocNaturalWeight:
        begin
          result := CompareRealValue(Item1, Item2, CM.Method);
        end;
      pocOriginalOrder: result := CompareOriginalOrder(Item1, Item2);
    end;
    if result <> 0 then
    begin
      Exit;
    end;
  end;
end;

procedure TframePestObservationResults.btnCopyClick(Sender: TObject);
begin
  rdgPestObs.CopyAllCellsToClipboard;
end;

procedure TframePestObservationResults.btnHightlightObjectsClick(
  Sender: TObject);
var
  Undo: TUndoChangeSelection;
  AScreenObject: TScreenObject;
  XCoordinate: real;
  YCoordinate: real;
begin
  Undo := TUndoChangeSelection.Create;
  frmGoPhast.ResetSelectedScreenObjects;

  if Sender = btnHightlightObjects then
  begin
    AScreenObject := GetSelectedObjectFromGrid;
  end
  else
  begin
    AScreenObject := GetSelectedObjectFromGraph;
  end;

  Undo.SetPostSelection;

  if Undo.SelectionChanged then
  begin
    frmGoPhast.UndoStack.Submit(Undo);
  end
  else
  begin
    Undo.Free;
  end;

//    NameIndex := ScreenObjects.IndexOf(ScreenObjectName);
  if AScreenObject <> nil then
  begin
//      AScreenObject := ScreenObjects.Objects[NameIndex] as TScreenObject;
    AScreenObject.Selected := True;
    XCoordinate := AScreenObject.Points[0].X;
    YCoordinate := AScreenObject.Points[0].Y;
    case AScreenObject.ViewDirection of
      vdTop:
        begin
          SetTopPosition(XCoordinate, YCoordinate);
        end;
      vdFront:
        begin
          SetFrontPosition(XCoordinate, YCoordinate);
        end;
      vdSide:
        begin
          SetSidePosition(YCoordinate, XCoordinate);
        end;
    else
      Assert(False);
    end;
  end;
end;

procedure TframePestObservationResults.btnRestoreClick(Sender: TObject);
var
  ObsCol: TPestObsColumns;
  Index: Integer;
  CM: TCompareMethod;
begin
  ObsCol := pocOriginalOrder;
  for Index := 0 to SortOrder.Count-1 do
  begin
    CM := SortOrder[Index];
    if CM.Method = ObsCol then
    begin
      SortOrder.Extract(CM);
      SortOrder.Insert(0, CM);
      FillTable;
      break;
    end;
  end;
end;


constructor TframePestObservationResults.Create(Owner: TComponent);
begin
  inherited;
//  FUsedObservations := TDictionary<string, IObservationItem>.Create;
  FObservations := TPestObsCollection.Create(nil);
end;

destructor TframePestObservationResults.Destroy;
begin
  FObservations.Free;
//  FUsedObservations.Free;
  inherited;
end;

procedure TframePestObservationResults.InitializeTableHeaders;
begin
  rdgPestObs.Cells[Ord(pocName), 0] := 'Observation Name';
  rdgPestObs.Cells[Ord(pocGroup), 0] := 'Group Name';
  rdgPestObs.Cells[Ord(pocObject), 0] := 'Object Name';
  rdgPestObs.Cells[Ord(pocTime), 0] := 'Time';
  rdgPestObs.Cells[Ord(pocMeasured), 0] := 'Measured';
  rdgPestObs.Cells[Ord(pocModeled), 0] := 'Modeled';
  rdgPestObs.Cells[Ord(pocResidual), 0] := 'Residual';
  rdgPestObs.Cells[Ord(pocWeight), 0] := 'Weight';
  rdgPestObs.Cells[Ord(pocWtMeas), 0] := 'Weight * Measured';
  rdgPestObs.Cells[Ord(pocWtMod), 0] := 'Weight * Modeled';
  rdgPestObs.Cells[Ord(pocWtRes), 0] := 'Weight * Residual';
  rdgPestObs.Cells[Ord(pocMeasSD), 0] := 'Measured SD';
  rdgPestObs.Cells[Ord(pocNaturalWeight), 0] := 'Natural Weight';
end;

procedure TframePestObservationResults.pbObservationsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  XYMouseDown(Button, Shift, X, Y);
end;

procedure TframePestObservationResults.pbObservationsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  XYMouseMove(Shift, X, Y);
end;

procedure TframePestObservationResults.pbObservationsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  XYMouseup(Button, Shift, X, Y);
  FSelectedObsItem := nil;
  if qtreeObservations.Count > 0 then
  begin
    FSelectedObsItem := qtreeObservations.NearestPointsFirstData
      (xyexportb.xw, xyexportb.yw[1]);
    btnHightlightObjectsClick(nil);
  end;
end;

procedure TframePestObservationResults.pbObservationsPaint(Sender: TObject);
begin
  PlotValues;
end;

procedure TframePestObservationResults.pgcObservationsChange(Sender: TObject);
begin
  if pgcObservations.ActivePage = tabGraph then
  begin
    PlotValues
  end;
end;

procedure TframePestObservationResults.PlotValues;
var
  index: Integer;
  ObservedMin: Double;
  SimulatedMin: Double;
  ObservedMax: Double;
  SimulatedMax: Double;
  SimValue: Double;
//  HobDry: Double;
//  HDry: Real;
//  HNoFlow: Real;
  Initialized: Boolean;
  Min1To1: double;
  Max1To1: double;
  Count: Integer;
  SimList: TList<TPestObsResult>;
  ObsItem: TPestObsResult;
  function NearlyTheSame(const X, Y: extended): boolean;
  const
    Epsilon = 1e-6;
  begin
    result := (X = Y) or (Abs(X - Y) < Epsilon) or
      (Abs(X - Y) / (Abs(X) + Abs(Y) + Epsilon) < Epsilon);
  end;
  function OkSimValue(ASimulatedValue: Double): Boolean;
  begin
//    if NearlyTheSame(HobDry, ASimulatedValue)
//      or NearlyTheSame(HDry, ASimulatedValue)
//      or NearlyTheSame(HNoFlow, ASimulatedValue)
//       then
//    begin
//      Result := False;
//    end
//    else
//    begin
      result := True;
//    end;
  end;
  function GetPlotValue(ObsItem: TPestObsResult): double;
  begin
    result := 0;
    case rgGraphType.ItemIndex of
      0: result := ObsItem.Modeled;
      1: result := ObsItem.Residual;
      2: result := ObsItem.WeightedResidual;
      else
        Assert(False);
    end;
  end;
begin
  qtreeObservations.Clear;
  if FObservations.Count > 0 then
  begin
    SimList := TList<TPestObsResult>.Create;
    try
      SimList.Capacity := FObservations.Count;
      ObservedMin := FObservations[0].Measured;
      ObservedMax := ObservedMin;
      Initialized := False;
      SimulatedMin := 0;
      SimulatedMax := 0;
      for index := 0 to FObservations.Count - 1 do
      begin
        ObsItem := FObservations[index];
        if ObsItem.Measured < ObservedMin then
        begin
          ObservedMin := ObsItem.Measured
        end
        else if ObsItem.Measured > ObservedMax then
        begin
          ObservedMax := ObsItem.Measured
        end;

        SimValue := GetPlotValue(ObsItem);
        if OkSimValue(SimValue) then
        begin
          SimList.Add(ObsItem);
//          if rgGraphType.ItemIndex = 1 then
//          begin
//            SimValue := ObsItem.Residual;
//          end;
//          Inc(Count);
          if Initialized then
          begin
            if SimValue < SimulatedMin then
            begin
              SimulatedMin := SimValue
            end
            else if SimValue > SimulatedMax then
            begin
              SimulatedMax := SimValue
            end;
          end
          else
          begin
            Initialized := True;
            SimulatedMin := SimValue;
            SimulatedMax := SimulatedMin;
          end;
        end;
      end;

      SimList.Sort(TComparer<TPestObsResult>.Construct(
        function (const L, R: TPestObsResult): Integer
          begin
            result := Sign(L.Measured - R.Measured);
          end
        ));

      if ObservedMax = ObservedMin then
      begin
        ObservedMax := ObservedMax + 1;
        ObservedMin := ObservedMin - 1;
      end;
      if SimulatedMax = SimulatedMin then
      begin
        SimulatedMax := SimulatedMax + 1;
        SimulatedMin := SimulatedMin - 1;
      end;
      qtreeObservations.XMax := ObservedMax;
      qtreeObservations.XMin := ObservedMin;
      qtreeObservations.YMax := SimulatedMax;
      qtreeObservations.YMin := SimulatedMin;

  //    xysetdataarray(Data, 2, Count);
      xysetdataarray(Data, SimList.Count, 1);
  //    SetLength(Data, 2, Count);

      try
        xycleargraph(pbObservations,clWhite,clBlack,1);

        xystartgraph(0, 100, 0, 100, 50, 50, 50, 50, clipoff);

        xyxaxis(clBlack,ObservedMin,ObservedMax,
          (ObservedMax-ObservedMin)/10,0,'Observed',1,False,False,True, 2);

        case rgGraphType.ItemIndex  of
          0:
            begin
              xyyaxis(clBlack,SimulatedMin,SimulatedMax,
                (SimulatedMax-SimulatedMin)/10,0,'Simulated',5,False,False,True, 2);
            end;
          1:
            begin
              xyyaxis(clBlack,Min(0, SimulatedMin), Max(0, SimulatedMax),
                (SimulatedMax-SimulatedMin)/10,0,'Residual',5,False,False,True, 2);
            end;
          2:
            begin
              xyyaxis(clBlack,Min(0, SimulatedMin), Max(0, SimulatedMax),
                (SimulatedMax-SimulatedMin)/10,0,'Weighted Residual',5,False,False,True, 2);
            end;
          else
            Assert(False);
        end;

        Count := 1;
        for index := 0 to SimList.Count - 1 do
        begin
          SimValue := GetPlotValue(SimList[index]);
//          if OkSimValue(SimValue) then
//          begin
            qtreeObservations.AddPoint(SimList[index].Measured,
              SimValue, SimList[index]);

            Data[Count, 0] := SimList[index].Measured;
            Data[Count, 1] := SimValue;
            Inc(Count);
//          end;
        end;

        xysymbol(1,0,0);
        xyplotarray(data,2,2);

        if rgGraphType.ItemIndex = 0 then
        begin
          Min1To1 := Max(SimulatedMin, ObservedMin);
          Max1To1 := Min(SimulatedMax, ObservedMax);
          xymove(Min1To1, Min1To1);
          xyDraw(Max1To1, Max1To1);
        end
        else
        begin
          xymove(ObservedMin, 0);
          xyDraw(ObservedMax, 0);
        end;

        xyfinish;
      except on E: exception do
        begin
          ShowMessage(e.message);
          pgcObservations.ActivePageIndex := 0;
          Exit;
        end;
      end;
    finally
      SimList.Free;
    end;
  end
end;

procedure TframePestObservationResults.rdgPestObsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol: Integer;
  ARow: Integer;
  ObsCol: TPestObsColumns;
  Index: integer;
  CM: TCompareMethod;
begin
    rdgPestObs.MouseToCell(X, Y, ACol, ARow);
    if (ARow = 0) and (ACol >= 0) and (ACol < rdgPestObs.ColCount) then
    begin
      TGridCrack(rdgPestObs).HideEditor;
      ObsCol := TPestObsColumns(ACol);
      for Index := 0 to SortOrder.Count-1 do
      begin
        CM := SortOrder[Index];
        if CM.Method = ObsCol then
        begin
          SortOrder.Extract(CM);
          SortOrder.Insert(0, CM);
          FillTable;
          break;
        end;
      end;
    end;
end;

procedure TframePestObservationResults.rgGraphTypeClick(Sender: TObject);
begin
  pbObservations.Invalidate;
end;

procedure TframePestObservationResults.FillTable;
var
  ItemIndex: Integer;
  AnItem: TPestObsResult;
  AList: TList;
begin
  if FObservations.Count > 0 then
  begin
    rdgPestObs.BeginUpdate;
    try
      rdgPestObs.RowCount := FObservations.Count;
      AList := TList.Create;
      try
        AList.Capacity := FObservations.Count;
        for ItemIndex := 0 to FObservations.Count - 1 do
        begin
          AList.Add(FObservations[ItemIndex]);
        end;
        AList.Sort(CompareObservations);

        for ItemIndex := 0 to AList.Count - 1 do
        begin
          AnItem := AList[ItemIndex];
          rdgPestObs.Cells[Ord(pocName), ItemIndex+1] := AnItem.Name;
          rdgPestObs.Cells[Ord(pocGroup), ItemIndex+1] := AnItem.GroupName;
          rdgPestObs.Cells[Ord(pocObject), ItemIndex+1] := AnItem.ObjectName;
          rdgPestObs.Objects[Ord(pocObject), ItemIndex+1] :=  AnItem.ScreenObject;
          rdgPestObs.RealValue[Ord(pocTime), ItemIndex+1] := AnItem.Time;
          rdgPestObs.RealValue[Ord(pocMeasured), ItemIndex+1] := AnItem.Measured;
          rdgPestObs.RealValue[Ord(pocModeled), ItemIndex+1] := AnItem.Modeled;
          rdgPestObs.RealValue[Ord(pocResidual), ItemIndex+1] := AnItem.Residual;
          rdgPestObs.RealValue[Ord(pocWeight), ItemIndex+1] := AnItem.Weight;
          rdgPestObs.RealValue[Ord(pocWtMeas), ItemIndex+1] := AnItem.WeightedMeasured;
          rdgPestObs.RealValue[Ord(pocWtMod), ItemIndex+1] := AnItem.WeightedModeled;
          rdgPestObs.RealValue[Ord(pocWtRes), ItemIndex+1] := AnItem.WeightedResidual;
          rdgPestObs.RealValue[Ord(pocMeasSD), ItemIndex+1] := AnItem.MeasurementStdDeviation;
          rdgPestObs.RealValue[Ord(pocNaturalWeight), ItemIndex+1] := AnItem.NaturalWeight;
        end;
      finally
        AList.Free;
      end;
    finally
      rdgPestObs.EndUpdate;
    end;
    lblRMS.Caption := Format(StrRootMeanSquareRes, [FObservations.RootMeanSquareWeightedResidual]);
  end
  else
  begin
    lblRMS.Caption := 'Root Mean Square Weighted Residual = ?';
    ClearGrid(rdgPestObs);
  end;
end;

procedure TframePestObservationResults.flnmedHeadObsResultsChange(
  Sender: TObject);
begin
  if FGettingData then
  begin
    Exit;
  end;
  if FileExists(flnmedHeadObsResults.FileName) then
  begin
    FObservations.FileName := flnmedHeadObsResults.FileName;
    FImportResult := FObservations.ReadFromFile;
    FUndoType := utImport;
  end
  else
  begin
    FObservations.FileName := '';
    FObservations.FileDate := 0;
    FObservations.Clear;
  end;
  FillTable;
end;

procedure TframePestObservationResults.GetData;
begin
  FGettingData := True;
  try
    lblRMS.Caption := 'Root Mean Square Weighted Residual = ?';
    InitializeTableHeaders;
    if FObservations = nil then
    begin
      FObservations := TPestObsCollection.Create(nil);
    end;
    FObservations.Assign(frmGoPhast.PhastModel.PestObsCollection);
    flnmedHeadObsResults.FileName := FObservations.FileName;
    FillTable;

    framelmtMinResidual.Limit := FObservations.MinResidualLimit;
    framelmtMaxResidual.Limit := FObservations.MaxResidualLimit;
    framelmtMinimumTime.Limit := FObservations.MinTimeLimit;
    framelmtMaximumTime.Limit := FObservations.MaxTimeLimit;
    framelmtMinWeightedResidual.Limit := FObservations.MinWeightedResidualLimit;
    framelmtMaxWeightedResidual.Limit := FObservations.MaxWeightedResidualLimit;

    rgDrawChoice.ItemIndex := Ord(FObservations.DrawChoice);

    FUndoType := utChange;
  finally
    FGettingData := False;
  end;
end;

function TframePestObservationResults.GetSelectedObjectFromGraph: TScreenObject;
var
  RowIndex: Integer;
  Selection: TGridRect;
begin
  result := nil;
  if FSelectedObsItem <> nil then
  begin
    result := FSelectedObsItem.ScreenObject;
    if result <> nil then
    begin
      result.Selected := True;
    end;

    RowIndex := rdgPestObs.Cols[Ord(pocName)].IndexOf(FSelectedObsItem.Name);
    if RowIndex >= 1 then
    begin
      if rdgPestObs.VisibleRowCount < rdgPestObs.RowCount - rdgPestObs.FixedRows then
      begin
        rdgPestObs.TopRow := RowIndex;
      end;
      Selection := rdgPestObs.Selection;
      Selection.Top := RowIndex;
      Selection.Bottom := RowIndex;
      rdgPestObs.Selection := Selection;
    end;
  end;
end;

function TframePestObservationResults.GetSelectedObjectFromGrid: TScreenObject;
begin
  result := nil;
  if rdgPestObs.SelectedRow <= 0 then
  begin
    Exit;
  end;
  result := rdgPestObs.Objects[Ord(pocObject), rdgPestObs.SelectedRow] as TScreenObject;
end;

procedure TframePestObservationResults.SetData;
var
  Undo: TCustomUndoChangePestObsResults;
begin
  FObservations.FileName := flnmedHeadObsResults.FileName;
  FObservations.MinResidualLimit := framelmtMinResidual.Limit;
  FObservations.MaxResidualLimit := framelmtMaxResidual.Limit;
  FObservations.MinTimeLimit := framelmtMinimumTime.Limit;
  FObservations.MaxTimeLimit := framelmtMaximumTime.Limit;
  FObservations.MinWeightedResidualLimit := framelmtMinWeightedResidual.Limit;
  FObservations.MaxWeightedResidualLimit := framelmtMaxWeightedResidual.Limit;
  FObservations.DrawChoice := TDrawChoice(rgDrawChoice.ItemIndex);
  TestForNewFile;
  Undo := nil;
  case FUndoType of
    utChange:
      begin
        Undo := TUndoChangePestObsResults.Create(FObservations);
      end;
    utImport:
      begin
        Undo := TUndoImportChangePestObsResults.Create(FObservations);
      end;
    else Assert(False);
  end;
  frmGoPhast.UndoStack.Submit(Undo);
  FUndoType := utChange;

end;

procedure TframePestObservationResults.spinSymbolSizeChange(Sender: TObject);
begin
  shpMax.Height := spinSymbolSize.AsInteger;
  shpMax.Width := spinSymbolSize.AsInteger;
  shpHalfMax.Width := shpMax.Width;
  shpHalfMax.Height := Round(Sqrt(Sqr(spinSymbolSize.AsInteger/2)/2)*2);
  shpHalfMax.Top := shpMax.Top + shpMax.Height + 8;
  lblMax.Left := shpMax.Left + shpMax.Width + 8;
  lblMax.Top := shpMax.Top + (shpMax.Height-lblMax.Height) div 2;
  lblHalfMax.Left := lblMax.Left;
  lblHalfMax.Top := shpHalfMax.Top + (shpHalfMax.Height-lblHalfMax.Height) div 2;
end;

procedure TframePestObservationResults.TestForNewFile;
var
  ReadFile: Boolean;
  FileDate: TDateTime;
begin
  if FileExists(FObservations.FileName) then
  begin
    FileAge(FObservations.FileName, FileDate);
    if FileDate <> FObservations.FileDate then
    begin
      ReadFile := MessageDlg(Format(StrTheFileSHasADi,
        [FObservations.FileName]),
        mtWarning, [mbYes, mbNo], 0) = mrYes;
      if ReadFile then
      begin
        FObservations.ReadFromFile;
      end;
    end;
  end;
end;

procedure TframePestObservationResults.UpdateChildModels;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
  LocalModel: TPhastModel;
  Index: Integer;
  ModelList: TList;
  APointer: TObject;
begin
  ModelList := TList.Create;
  try
    ModelList.Add(frmGoPhast.PhastModel);
    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ModelList.Add(frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel);
    end;
    for Index := comboModels.Items.Count - 1 downto 0 do
    begin
      APointer := comboModels.Items.Objects[Index];
      if ModelList.IndexOf(APointer) < 0 then
      begin
        comboModels.Items.Delete(Index);
      end;
    end;
  finally
    ModelList.Free;
  end;

  UpdateObsLinkList;

  LocalModel := frmGoPhast.PhastModel;
  // comboModels.Clear;
  if comboModels.Items.IndexOfObject(LocalModel) < 0 then
  begin
    comboModels.Items.InsertObject(0, LocalModel.DisplayName, LocalModel);
  end;
  if LocalModel.LgrUsed then
  begin
    comboModels.Visible := True;
    for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
    begin
      ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
      if comboModels.Items.IndexOfObject(ChildModel) < 0 then
      begin
        comboModels.Items.InsertObject(ChildIndex + 1, ChildModel.DisplayName,
          ChildModel);
      end;
    end;
  end
  else
  begin
    comboModels.Visible := False;
  end;
  comboModels.ItemIndex := 0;
end;

procedure TframePestObservationResults.UpdateObsLinkList;
begin

end;

{ TCustomUndoChangePestObsResults }

procedure TCustomUndoChangePestObsResults.ApplyChange(
  Observations: TPestObsCollection);
begin
  frmGoPhast.PhastModel.PestObsCollection := Observations;
end;

constructor TCustomUndoChangePestObsResults.Create(
  var Observations: TPestObsCollection);
begin
  FNewObservations := Observations;
  Observations := nil;
  FOldObservations := TPestObsCollection.Create(nil);
  FOldObservations.Assign(frmGoPhast.PhastModel.PestObsCollection);
end;

destructor TCustomUndoChangePestObsResults.Destroy;
begin
  FNewObservations.Free;
  FOldObservations.Free;
  inherited;
end;

procedure TCustomUndoChangePestObsResults.DoCommand;
begin
  ApplyChange(FNewObservations);
  UpdateGUI;
  inherited;
end;

procedure TCustomUndoChangePestObsResults.Undo;
begin
  ApplyChange(FOldObservations);
  UpdateGUI;
  inherited;
end;

procedure TCustomUndoChangePestObsResults.UpdateGUI;
begin

end;

procedure InitializeSortOrder;
var
  Index: TPestObsColumns;
  CM: TCompareMethod;
begin
  SortOrder.Free;
  SortOrder := TObjectList.Create;
  for Index := Low(TPestObsColumns) to High(TPestObsColumns) do
  begin
    CM := TCompareMethod.Create;
    CM.Method := Index;
    if CM.Method = pocOriginalOrder then
    begin
      SortOrder.Insert(0, CM)
    end
    else
    begin
      SortOrder.Add(CM)
    end;
  end;
end;

{ TUndoImportChangePestObsResults }

function TUndoImportChangePestObsResults.Description: string;
begin
  result := StrImportPESTResults
end;

{ TUndoChangePestObsResults }

function TUndoChangePestObsResults.Description: string;
begin
  result := StrChangePESTResultO
end;

initialization
  InitializeSortOrder;

finalization
  SortOrder.Free;

end.
