unit frameHeadObservationResultsUnit;

interface

{ TODO : give the option to plot residuals as a number and to filter out large residuals. }

uses System.Types, System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Grids, RbwDataGrid4, JvSpin, JvExControls,
  JvColorBox, JvColorButton, frameDisplayLimitUnit, Mask, JvExMask, JvToolEdit,
  ComCtrls, GoPhastTypes, ModflowHeadObsResults, Generics.Collections,
  UndoItems, QuadTreeClass, GrayTabs, frameCustomObservationResultsUnit;

type
  TUndoType = (utChange, utImport);
  TObsCol = (ocName, ocResidual, ocObserved, ocSimulated,
    ocX, ocY, ocTime, ocObjectName, ocOriginalOrder);

  TObsHeadLink = class(TObject)
  private
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    FModel: TBaseModel;
    FOldHeadObsCollection: THeadObsCollection;
    FNewHeadObsCollection: THeadObsCollection;
  public
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    constructor Create(AModel: TBaseModel);
    destructor Destroy; override;
  end;

  TObsHeadLinkList = class (TObjectList<TObsHeadLink>)
  public
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function IndexOfModel(AModel: TBaseModel): Integer;
  end;

  TCustomUndoChangeHeadObsResults = class(TCustomUndo)
  private
    FObsLinkList: TObsHeadLinkList;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure ApplyChange(Model: TBaseModel;
      HeadObsCollection: THeadObsCollection);
    procedure UpdateGUI;
  public
    procedure DoCommand; override;
    procedure Undo; override;
  public
    constructor Create(var ObsLinkList: TObsHeadLinkList);
    Destructor Destroy; override;
  end;

  TUndoChangeHeadObsResults = class(TCustomUndoChangeHeadObsResults)
  protected
    function Description: string; override;
  end;

  TUndoImportChangeHeadObsResults = class(TCustomUndoChangeHeadObsResults)
  protected
    function Description: string; override;
  end;

  TframeHeadObservationResults = class(TframeCustomObservationResults)
//    pgcObservations: TPageControl;
//    tabControls: TTabSheet;
//    lblNegativeColor: TLabel;
//    lblColorPositive: TLabel;
//    lblMaxSymbolSize: TLabel;
//    lblHeadObsResults: TLabel;
//    flnmedHeadObsResults: TJvFilenameEdit;
//    grpbxFilter: TGroupBox;
//    lblMaximumTime: TLabel;
//    lblMaxResidual: TLabel;
//    lblMinimumTime: TLabel;
//    lblMinResidual: TLabel;
//    framelmtMinimumTime: TframeDisplayLimit;
//    framelmtMaxResidual: TframeDisplayLimit;
//    framelmtMaximumTime: TframeDisplayLimit;
//    framelmtMinResidual: TframeDisplayLimit;
//    clrbtnNegative: TJvColorButton;
//    clrbtnPositive: TJvColorButton;
//    spinSymbolSize: TJvSpinEdit;
//    cbShow: TCheckBox;
//    tabValues: TTabSheet;
//    rdgHeadObs: TRbwDataGrid4;
//    pnlBottom: TPanel;
//    comboModels: TComboBox;
//    btnHightlightObjects: TButton;
//    btnRestore: TButton;
//    btnCopy: TButton;
//    framelmtMinLayer: TframeDisplayLimit;
//    lblMinLayer: TLabel;
//    lblMaxLayer: TLabel;
//    framelmtMaxLayer: TframeDisplayLimit;
//    tabLegend: TTabSheet;
//    shpMax: TShape;
//    shpHalfMax: TShape;
//    lblMax: TLabel;
//    lblHalfMax: TLabel;
//    lblRMS: TLabel;
//    tabGraph: TTabSheet;
//    pbHeadObs: TPaintBox;
//    qtreeObservations: TRbwQuadTree;
//    pnlValueControls: TPanel;
//    pnlGraphControls: TPanel;
//    rgGraphType: TRadioGroup;
//    lblGraphInstructions: TLabel;
    procedure flnmedHeadObsResultsChange(Sender: TObject);
    procedure comboModelsChange(Sender: TObject);
    procedure btnHightlightObjectsClick(Sender: TObject);
    procedure rdgHeadObsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnRestoreClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure spinSymbolSizeChange(Sender: TObject);
    procedure pbHeadObsPaint(Sender: TObject);
    procedure pbHeadObsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbHeadObsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbHeadObsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rgGraphTypeClick(Sender: TObject);
  private
    FUndoType: TUndoType;
    FCurrentModelLink: TObsHeadLink;
    FGettingDate: Boolean;
    FImportResult: Boolean;
    ObsLinkList : TObsHeadLinkList;
    FSelectedObsItem: THeadObsItem;
    procedure SetCurrentModelLink(Value: TObsHeadLink);
    procedure Initialize(AHeadObsColl: THeadObsCollection);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure DisplayValues(AModel: TBaseModel; AHeadObsColl: THeadObsCollection);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure GetDataForAModel(AModel: TBaseModel; AHeadObsColl: THeadObsCollection);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function ReadFileForAModel(AModel: TBaseModel; AHeadObsColl: THeadObsCollection;
      const FileName: string; ShowDialog: boolean): Boolean;
    procedure SetDataForAModel(HeadObs: THeadObsCollection);
    procedure PlotValues;
    property CurrentModelLink: TObsHeadLink read FCurrentModelLink
      write SetCurrentModelLink;
    procedure TestForNewFiles;
    function GetSelectedObjectsFromGrid(ScreenObjects: TStringList): string;
    function GetSelectedObjectsFromGraph(ScreenObjects: TStringList): string;

    { Private declarations }
  protected
    procedure Loaded; override;
    procedure UpdateObsLinkList; override;
  public
    procedure GetData;
    procedure SetData;
    function ReadFile(const FileName: string): Boolean;
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateSelectedModel;
    { Public declarations }
  end;

implementation

uses
  PhastModelUnit, frmGoPhastUnit, ModflowHobUnit, frmDisplayDataUnit, Math,
  UndoItemsScreenObjects, ScreenObjectUnit, frmGoToUnit, Contnrs, xygraph,
  Generics.Defaults;

{$R *.dfm}

resourcestring
  StrTheFileSHasADi = 'The file %s has a different date than the imported re' +
  'sults. Do you want to import it?';
  StrTheFileSHasADiMultiple = 'The following files %s have a different date '
  + 'than the imported results. Do you want to import them?';
  StrChangeHeadResultP = 'change head result parameters';
  StrImportHeadResults = 'import head results';
  StrObservationName = 'Observation Name';
  StrX = 'X';
  StrY = 'Y';
  StrTime = 'Time';
  StrObservedValue = 'Observed Value';
  StrSimulatedValue = 'Simulated Value';
  StrResidual = 'Residual';
  StrObjectName = 'Object Name';
  StrRootMeanSquareRes = 'Root Mean Square Residual = %g';


type
  TGridCrack = class(TRbwDataGrid4);
  TCompareMethod = class(TObject)
    Method: TObsCol;
  end;

var
  SortOrder: TList = nil;


function CompareScreenObjectName(Item1, Item2: Pointer): Integer;
var
  P1, P2: THeadObsItem;
begin
  P1 := Item1;
  P2 := Item2;
  result := AnsiCompareText(P1.ScreenObjectName, P2.ScreenObjectName);
end;

function CompareName(Item1, Item2: Pointer): Integer;
var
  P1, P2: THeadObsItem;
  Splitter1: TStringList;
  Splitter2: TStringList;
  index: Integer;
  Part1: string;
  Part2: string;
  Value1: Integer;
  Value2: Integer;
begin
  P1 := Item1;
  P2 := Item2;
  Splitter1 := TStringList.Create;
  Splitter2 := TStringList.Create;
  try
    Splitter1.Delimiter := '_';
    Splitter2.Delimiter := '_';
    Splitter1.DelimitedText := P1.Name;
    Splitter2.DelimitedText := P2.Name;
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
    result := AnsiCompareText(P1.Name, P2.Name);
  finally
    Splitter1.Free;
    Splitter2.Free;
  end;

end;

function CompareObservedValue(Item1, Item2: Pointer): Integer;
var
  P1, P2: THeadObsItem;
begin
  P1 := Item1;
  P2 := Item2;
  result := Sign(P1.ObservedValue - P2.ObservedValue);
end;

function CompareSimulatedValue(Item1, Item2: Pointer): Integer;
var
  P1, P2: THeadObsItem;
begin
  P1 := Item1;
  P2 := Item2;
  result := Sign(P1.SimulatedValue - P2.SimulatedValue);
end;

function CompareResidual(Item1, Item2: Pointer): Integer;
var
  P1, P2: THeadObsItem;
begin
  P1 := Item1;
  P2 := Item2;
  result := Sign(P1.Residual - P2.Residual);
end;

function CompareOriginalOrder(Item1, Item2: Pointer): Integer;
var
  P1, P2: THeadObsItem;
begin
  P1 := Item1;
  P2 := Item2;
  result := P1.OriginalOrder - P2.OriginalOrder;
end;

function CompareTime(Item1, Item2: Pointer): Integer;
var
  P1, P2: THeadObsItem;
begin
  P1 := Item1;
  P2 := Item2;
  result := Sign(P1.Time - P2.Time);
end;

function CompareX(Item1, Item2: Pointer): Integer;
var
  P1, P2: THeadObsItem;
begin
  P1 := Item1;
  P2 := Item2;
  result := Sign(P1.X - P2.X);
end;

function CompareY(Item1, Item2: Pointer): Integer;
var
  P1, P2: THeadObsItem;
begin
  P1 := Item1;
  P2 := Item2;
  result := Sign(P1.Y - P2.Y);
end;

function CompareVisible(Item1, Item2: Pointer): Integer;
var
  P1, P2: THeadObsItem;
begin
  P1 := Item1;
  P2 := Item2;
  result := Sign(Ord(P1.Visible) - Ord(P2.Visible));
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
      ocName: result := CompareName(Item1, Item2);
      ocResidual: result := CompareResidual(Item1, Item2);
      ocObserved: result := CompareObservedValue(Item1, Item2);
      ocSimulated: result := CompareSimulatedValue(Item1, Item2);
      ocX: result := CompareX(Item1, Item2);
      ocY: result := CompareY(Item1, Item2);
      ocTime: result := CompareTime(Item1, Item2);
      ocObjectName: result := CompareScreenObjectName(Item1, Item2);
      ocOriginalOrder: result := CompareOriginalOrder(Item1, Item2);
    end;
    if result <> 0 then
    begin
      Exit;
    end;
  end;
end;


{ TObsHeadLink }

constructor TObsHeadLink.Create(AModel: TBaseModel);
var
  LocalModel: TCustomModel;
begin
  FModel := AModel;
  FOldHeadObsCollection := THeadObsCollection.Create(nil);
  FNewHeadObsCollection := THeadObsCollection.Create(nil);

  LocalModel := FModel as TCustomModel;
  FOldHeadObsCollection.Assign(LocalModel.HeadObsResults);
  FNewHeadObsCollection.Assign(LocalModel.HeadObsResults);
end;

destructor TObsHeadLink.Destroy;
begin
  FOldHeadObsCollection.Free;
  FNewHeadObsCollection.Free;
  inherited;
end;

{ TObsHeadLinkList }

function TObsHeadLinkList.IndexOfModel(AModel: TBaseModel): Integer;
var
  Index: Integer;
  AnItem: TObsHeadLink;
begin
  for Index := 0 to Count - 1 do
  begin
    AnItem := Items[Index];
    if AnItem.FModel = AModel then
    begin
      Result := Index;
      Exit;
    end;
  end;
  result := -1;
end;

{ TCustomUndoChangeHeadObsResults }

procedure TCustomUndoChangeHeadObsResults.ApplyChange(Model: TBaseModel;
  HeadObsCollection: THeadObsCollection);
begin
  (Model as TCustomModel).HeadObsResults.Assign(HeadObsCollection);
end;

constructor TCustomUndoChangeHeadObsResults.Create(var ObsLinkList: TObsHeadLinkList);
begin
  FObsLinkList := ObsLinkList;
  ObsLinkList := nil;
end;

destructor TCustomUndoChangeHeadObsResults.Destroy;
begin
  FObsLinkList.Free;
  inherited;
end;

procedure TCustomUndoChangeHeadObsResults.UpdateGUI;
begin
  frmGoPhast.TopDiscretizationChanged := True;
  frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
  frmGoPhast.EnableExportHeadObs(nil);

  if frmDisplayData <> nil then
  begin
    frmDisplayData.frameHeadObservationResults.UpdateSelectedModel;
  end;
end;

procedure TCustomUndoChangeHeadObsResults.DoCommand;
var
  Index: Integer;
  ObsItem: TObsHeadLink;
begin
  inherited;
  for Index := 0 to FObsLinkList.Count - 1 do
  begin
    ObsItem := FObsLinkList[Index];
    ApplyChange(ObsItem.FModel, ObsItem.FNewHeadObsCollection);
  end;
  UpdateGUI;
end;

procedure TCustomUndoChangeHeadObsResults.Undo;
var
  Index: Integer;
  ObsItem: TObsHeadLink;
begin
  inherited;
  for Index := 0 to FObsLinkList.Count - 1 do
  begin
    ObsItem := FObsLinkList[Index];
    ApplyChange(ObsItem.FModel, ObsItem.FOldHeadObsCollection);
  end;
  UpdateGUI;
end;

{ TUndoChangeHeadObsResults }

function TUndoChangeHeadObsResults.Description: string;
begin
  result := StrChangeHeadResultP;
end;

{ TUndoImportChangeHeadObsResults }

function TUndoImportChangeHeadObsResults.Description: string;
begin
  result := StrImportHeadResults;
end;


{ TframeHeadObservationResults }

procedure TframeHeadObservationResults.btnCopyClick(Sender: TObject);
begin
  rdgHeadObs.CopyAllCellsToClipboard;
end;

procedure TframeHeadObservationResults.btnHightlightObjectsClick(
  Sender: TObject);
var
  Undo: TUndoChangeSelection;
  ScreenObjects: TStringList;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  ScreenObjectName: string;
  NameIndex: Integer;
  XCoordinate: real;
  YCoordinate: real;
begin
  ScreenObjects := TStringList.Create;
  try
    for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
      if not AScreenObject.Deleted
        and (AScreenObject.ModflowBoundaries.ModflowHeadObservations <> nil)
        and AScreenObject.ModflowBoundaries.ModflowHeadObservations.Used then
      begin
        ScreenObjects.AddObject(AScreenObject.Name, AScreenObject)
      end;
    end;
    ScreenObjects.Sorted := True;

    Undo := TUndoChangeSelection.Create;
    frmGoPhast.ResetSelectedScreenObjects;

    if Sender = btnHightlightObjects then
    begin
      ScreenObjectName := GetSelectedObjectsFromGrid(ScreenObjects);
    end
    else
    begin
      ScreenObjectName := GetSelectedObjectsFromGraph(ScreenObjects);
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

    NameIndex := ScreenObjects.IndexOf(ScreenObjectName);
    if NameIndex >= 0 then
    begin
      AScreenObject := ScreenObjects.Objects[NameIndex] as TScreenObject;
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

  finally
    ScreenObjects.Free;
  end;

//  PlotValues;
end;

procedure TframeHeadObservationResults.btnRestoreClick(Sender: TObject);
var
  ObsCol: TObsCol;
  Index: Integer;
  CM: TCompareMethod;
begin
  ObsCol := ocOriginalOrder;
  for Index := 0 to SortOrder.Count-1 do
  begin
    CM := SortOrder[Index];
    if CM.Method = ObsCol then
    begin
      SortOrder.Extract(CM);
      SortOrder.Insert(0, CM);
      GetDataForAModel(FCurrentModelLink.FModel, FCurrentModelLink.FNewHeadObsCollection);
      break;
    end;
  end;

end;

procedure TframeHeadObservationResults.comboModelsChange(Sender: TObject);
var
  AModel: TBaseModel;
  ModelIndex: Integer;
begin
  inherited;
  if comboModels.ItemIndex >= 0 then
  begin
    AModel := comboModels.Items.Objects[comboModels.ItemIndex] as TBaseModel;
    ModelIndex := ObsLinkList.IndexOfModel(AModel);
    if ModelIndex < 0 then
    begin
      CurrentModelLink := TObsHeadLink.Create(AModel);
      ObsLinkList.Add(FCurrentModelLink);
    end
    else
    begin
      CurrentModelLink := ObsLinkList[ModelIndex];
    end;
  end
  else
  begin
    CurrentModelLink := nil;
  end;
end;

constructor TframeHeadObservationResults.Create(Owner: TComponent);
begin
  inherited;
  ObsLinkList := TObsHeadLinkList.Create;
end;

destructor TframeHeadObservationResults.Destroy;
begin
  ObsLinkList.Free;
  inherited;
end;

procedure TframeHeadObservationResults.DisplayValues(AModel: TBaseModel;
  AHeadObsColl: THeadObsCollection);
var
  Item: THeadObsItem;
  Index: Integer;
  AList: TList;
  ColIndex: Integer;
begin
  rdgHeadObs.BeginUpdate;
  try
    rdgHeadObs.RowCount := Max(AHeadObsColl.Count + 1,2);
    AList := TList.Create;
    try
      AList.Capacity := AHeadObsColl.Count;
      for Index := 0 to AHeadObsColl.Count - 1 do
      begin
        AList.Add(AHeadObsColl[Index]);
      end;
      AList.Sort(CompareObservations);
      for Index := 0 to AList.Count - 1 do
      begin
        Item := AList[Index];
        rdgHeadObs.Cells[Ord(ocName), Index + 1] := Item.Name;
        rdgHeadObs.Cells[Ord(ocX), Index + 1] := FloatToStr(Item.X);
        rdgHeadObs.Cells[Ord(ocY), Index + 1] := FloatToStr(Item.Y);
        rdgHeadObs.Cells[Ord(ocTime), Index + 1] := FloatToStr(Item.Time);
        rdgHeadObs.Cells[Ord(ocObserved), Index + 1] := FloatToStr(Item.ObservedValue);
        rdgHeadObs.Cells[Ord(ocSimulated), Index + 1] := FloatToStr(Item.SimulatedValue);
        rdgHeadObs.Cells[Ord(ocResidual), Index + 1] := FloatToStr(Item.Residual);
        rdgHeadObs.Cells[Ord(ocObjectName), Index + 1] := Item.ScreenObjectName;
      end;
      tabLegend.TabVisible := AHeadObsColl.Count > 0;
      if AHeadObsColl.Count > 0 then
      begin
        AHeadObsColl.CalculateMaxResidual(AModel);
        lblMax.Caption := FloatToStr(AHeadObsColl.MaxResidual);
        lblHalfMax.Caption := FloatToStr(AHeadObsColl.MaxResidual/2);
        spinSymbolSizeChange(nil);
      end
      else
      begin
        for ColIndex := Ord(Low(TObsCol)) to Ord(High(TObsCol)) do
        begin
          rdgHeadObs.Cells[ColIndex, 1] := '';
        end;
      end;
    finally
      AList.Free;
    end;
  finally
    rdgHeadObs.EndUpdate;
  end;
  if AHeadObsColl.Count > 0 then
  begin
    lblRMS.Caption := Format(StrRootMeanSquareRes, [AHeadObsColl.RootMeanSquare]);
  end
  else
  begin
    lblRMS.Caption := 'Root Mean Square Residual = ?';
  end;
end;

var
  Data: Tdatatype;

type
  THeadObsComparer = TComparer<THeadObsItem>;


procedure TframeHeadObservationResults.PlotValues;
var
  Observations: THeadObsCollection;
  index: Integer;
  ObservedMin: Double;
  SimulatedMin: Double;
  ObservedMax: Double;
  SimulatedMax: Double;
  SimValue: Double;
  HobDry: Double;
  HDry: Real;
  HNoFlow: Real;
  Initialized: Boolean;
  Min1To1: double;
  Max1To1: double;
  Count: Integer;
  SimList: TList<THeadObsItem>;
  ObsItem: THeadObsItem;
  function NearlyTheSame(const X, Y: extended): boolean;
  const
    Epsilon = 1e-6;
  begin
    result := (X = Y) or (Abs(X - Y) < Epsilon) or
      (Abs(X - Y) / (Abs(X) + Abs(Y) + Epsilon) < Epsilon);
  end;
  function OkSimValue(ASimulatedValue: Double): Boolean;
  begin
    if NearlyTheSame(HobDry, ASimulatedValue)
      or NearlyTheSame(HDry, ASimulatedValue)
      or NearlyTheSame(HNoFlow, ASimulatedValue)
       then
    begin
      Result := False;
    end
    else
    begin
      result := True;
    end;
  end;
begin
  Observations := CurrentModelLink.FNewHeadObsCollection;

  HobDry := frmGoPhast.PhastModel.ModflowPackages.HobPackage.DryHead;
  HDry := frmGoPhast.PhastModel.ModflowOptions.HDry;
  HNoFlow := frmGoPhast.PhastModel.ModflowOptions.HNoFlow;

  Count := 0;

  qtreeObservations.Clear;
  if Observations.Count > 0 then
  begin
    SimList := TList<THeadObsItem>.Create;
    try
      SimList.Capacity := Observations.Count;
      ObservedMin := Observations[0].ObservedValue;
      ObservedMax := ObservedMin;
      Initialized := False;
      SimulatedMin := 0;
      SimulatedMax := 0;
      for index := 0 to Observations.Count - 1 do
      begin
        ObsItem := Observations[index];
        if ObsItem.ObservedValue < ObservedMin then
        begin
          ObservedMin := ObsItem.ObservedValue
        end
        else if ObsItem.ObservedValue > ObservedMax then
        begin
          ObservedMax := ObsItem.ObservedValue
        end;

        SimValue := ObsItem.SimulatedValue;
        if OkSimValue(SimValue) then
        begin
          SimList.Add(ObsItem);
          if rgGraphType.ItemIndex = 1 then
          begin
            SimValue := ObsItem.Residual;
          end;
          Inc(Count);
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

      SimList.Sort(TComparer<THeadObsItem>.Construct(
        function (const L, R: THeadObsItem): Integer
          begin
            result := Sign(L.ObservedValue - R.ObservedValue);
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
      xysetdataarray(Data, Count, 1);
  //    SetLength(Data, 2, Count);

      try
        xycleargraph(pbHeadObs,clWhite,clBlack,1);

        xystartgraph(0, 100, 0, 100, 50, 50, 50, 50, clipoff);

        xyxaxis(clBlack,ObservedMin,ObservedMax,
          (ObservedMax-ObservedMin)/10,0,'Observed',1,False,False,True, 2);

        if rgGraphType.ItemIndex = 0 then
        begin
          xyyaxis(clBlack,SimulatedMin,SimulatedMax,
            (SimulatedMax-SimulatedMin)/10,0,'Simulated',5,False,False,True, 2);
        end
        else
        begin
          xyyaxis(clBlack,Min(0, SimulatedMin), Max(0, SimulatedMax),
            (SimulatedMax-SimulatedMin)/10,0,'Residual',5,False,False,True, 2);
        end;

        Count := 1;
        for index := 0 to SimList.Count - 1 do
        begin
          if rgGraphType.ItemIndex = 0 then
          begin
            SimValue := SimList[index].SimulatedValue;
          end
          else
          begin
            SimValue := SimList[index].Residual;
          end;
//          if OkSimValue(SimValue) then
//          begin
            qtreeObservations.AddPoint(SimList[index].ObservedValue,
              SimValue, SimList[index]);

            Data[Count, 0] := SimList[index].ObservedValue;
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
  end;
end;

procedure TframeHeadObservationResults.flnmedHeadObsResultsChange(
  Sender: TObject);
begin
  if FGettingDate then
  begin
    Exit;
  end;
  if FileExists(flnmedHeadObsResults.FileName) then
  begin
    CurrentModelLink.FNewHeadObsCollection.FileName := flnmedHeadObsResults.FileName;
    FImportResult := CurrentModelLink.FNewHeadObsCollection.ReadFromFile(CurrentModelLink.FModel);
    FUndoType := utImport;
  end
  else
  begin
    CurrentModelLink.FNewHeadObsCollection.FileName := '';
    CurrentModelLink.FNewHeadObsCollection.FileDate := 0;
    CurrentModelLink.FNewHeadObsCollection.Clear;
  end;
end;

procedure TframeHeadObservationResults.GetData;
begin
  Handle;
  lblRMS.Caption := 'Root Mean Square Residual = ?';
  FCurrentModelLink := nil;
  UpdateChildModels;
  CurrentModelLink := ObsLinkList[0];
  PlotValues;
end;

procedure TframeHeadObservationResults.GetDataForAModel(AModel: TBaseModel;
  AHeadObsColl: THeadObsCollection);
begin
  FGettingDate := True;
  try
    Initialize(AHeadObsColl);
    try
      flnmedHeadObsResults.FileName := AHeadObsColl.FileName;
    except on EComboEditError do
      begin
        // do nothing.
      end;
    end;
    DisplayValues(AModel, AHeadObsColl);
  finally
    FGettingDate := False;
  end;
end;

procedure TframeHeadObservationResults.Initialize(
  AHeadObsColl: THeadObsCollection);
begin
//  FHeadObsCollection.Assign(AHeadObsColl);
  framelmtMinResidual.Limit := AHeadObsColl.MinResidualLimit;
  framelmtMaxResidual.Limit := AHeadObsColl.MaxResidualLimit;
  framelmtMinimumTime.Limit := AHeadObsColl.MinTimeLimit;
  framelmtMaximumTime.Limit := AHeadObsColl.MaxTimeLimit;
  framelmtMinLayer.Limit := AHeadObsColl.MinLayerLimit;
  framelmtMaxLayer.Limit := AHeadObsColl.MaxLayerLimit;

  clrbtnNegative.Color := AHeadObsColl.NegativeColor;
  clrbtnPositive.Color := AHeadObsColl.PositiveColor;
  spinSymbolSize.AsInteger := AHeadObsColl.MaxSymbolSize;
  cbShow.Checked := AHeadObsColl.Visible;
end;

procedure TframeHeadObservationResults.Loaded;
begin
  inherited;
  pgcObservations.ActivePageIndex := 0;
  framelmtMinResidual.Enabled := True;
  framelmtMaxResidual.Enabled := True;
  framelmtMinimumTime.Enabled := True;
  framelmtMaximumTime.Enabled := True;
  framelmtMinLayer.Enabled := True;
  framelmtMaxLayer.Enabled := True;

  rdgHeadObs.BeginUpdate;
  try
    rdgHeadObs.Cells[Ord(ocName),0] := StrObservationName;
    rdgHeadObs.Cells[Ord(ocX),0] := StrX;
    rdgHeadObs.Cells[Ord(ocY),0] := StrY;
    rdgHeadObs.Cells[Ord(ocTime),0] := StrTime;
    rdgHeadObs.Cells[Ord(ocObserved),0] := StrObservedValue;
    rdgHeadObs.Cells[Ord(ocSimulated),0] := StrSimulatedValue;
    rdgHeadObs.Cells[Ord(ocResidual),0] := StrResidual;
    rdgHeadObs.Cells[Ord(ocObjectName),0] := StrObjectName;
  finally
    rdgHeadObs.EndUpdate;
  end;

  UpdateChildModels;
end;

procedure TframeHeadObservationResults.pbHeadObsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  XYMouseDown(Button, Shift, X, Y);
end;

procedure TframeHeadObservationResults.pbHeadObsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  XYMouseMove(Shift, X, Y);
end;

procedure TframeHeadObservationResults.pbHeadObsMouseUp(Sender: TObject;
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

procedure TframeHeadObservationResults.pbHeadObsPaint(Sender: TObject);
begin
  PlotValues;
end;

procedure TframeHeadObservationResults.rdgHeadObsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol: Integer;
  ARow: Integer;
  ObsCol: TObsCol;
  Index: integer;
  CM: TCompareMethod;
begin
  if FCurrentModelLink <> nil then
  begin
    rdgHeadObs.MouseToCell(X, Y, ACol, ARow);
    if (ARow = 0) and (ACol >= 0) and (ACol < rdgHeadObs.ColCount) then
    begin
      TGridCrack(rdgHeadObs).HideEditor;
      ObsCol := TObsCol(ACol);
      for Index := 0 to SortOrder.Count-1 do
      begin
        CM := SortOrder[Index];
        if CM.Method = ObsCol then
        begin
          SortOrder.Extract(CM);
          SortOrder.Insert(0, CM);
          GetDataForAModel(FCurrentModelLink.FModel, FCurrentModelLink.FNewHeadObsCollection);
          break;
        end;
      end;
    end;
  end;
end;

function TframeHeadObservationResults.ReadFile(const FileName: string): Boolean;
var
  Index: Integer;
  ObsItem: TObsHeadLink;
  AFileName: string;
begin
  UpdateChildModels;

  result := True;
  for Index := 0 to ObsLinkList.Count - 1 do
  begin
    ObsItem := ObsLinkList[Index];
    if ObsItem.FModel is TChildModel then
    begin
      AFileName := TChildModel(ObsItem.FModel).Child_NameFile_Name(FileName);
      AFileName := ChangeFileExt(AFileName, StrHobout);
    end
    else
    begin
      AFileName := FileName;
    end;
    FCurrentModelLink := ObsItem;
    Result := ReadFileForAModel(ObsItem.FModel, ObsItem.FNewHeadObsCollection, AFileName, Index=0);
    if not result then
    begin
      Exit;
    end;
  end;
  FCurrentModelLink := nil;
  CurrentModelLink := ObsLinkList[0];
end;

function TframeHeadObservationResults.ReadFileForAModel(AModel: TBaseModel;
  AHeadObsColl: THeadObsCollection; const FileName: string;
  ShowDialog: boolean): Boolean;
begin
  Initialize(AHeadObsColl);
  flnmedHeadObsResults.Dialog.FileName := FileName;
  if ShowDialog then
  begin
    result := flnmedHeadObsResults.Dialog.Execute;
  end
  else
  begin
    result := True;
  end;
  if result then
  begin
    FImportResult := False;
    flnmedHeadObsResults.FileName := flnmedHeadObsResults.Dialog.FileName;
    result := FImportResult;
  end;
  DisplayValues(AModel, AHeadObsColl);
end;

procedure TframeHeadObservationResults.rgGraphTypeClick(Sender: TObject);
begin
  pbHeadObs.Invalidate;
end;

procedure TframeHeadObservationResults.SetCurrentModelLink(Value: TObsHeadLink);
var
  HeadObs: THeadObsCollection;
  FOldModelLink: TObsHeadLink;
begin
  if FCurrentModelLink <> nil then
  begin
    HeadObs := FCurrentModelLink.FNewHeadObsCollection;
    SetDataForAModel(HeadObs);
  end;
  FOldModelLink := FCurrentModelLink;
  FCurrentModelLink := Value;
  if FCurrentModelLink <> nil then
  begin
    if (FOldModelLink <> FCurrentModelLink) and (FOldModelLink <> nil) then
    begin
      FCurrentModelLink.FNewHeadObsCollection.MaxSymbolSize :=
        FOldModelLink.FNewHeadObsCollection.MaxSymbolSize
    end;
    GetDataForAModel(FCurrentModelLink.FModel, FCurrentModelLink.FNewHeadObsCollection)
  end;
end;

procedure TframeHeadObservationResults.SetData;
var
  Undo: TCustomUndoChangeHeadObsResults;
begin
  CurrentModelLink := nil;
  TestForNewFiles;
  Undo := nil;
  case FUndoType of
    utChange:
      begin
        Undo := TUndoChangeHeadObsResults.Create(ObsLinkList);
      end;
    utImport:
      begin
        Undo := TUndoImportChangeHeadObsResults.Create(ObsLinkList);
      end;
    else Assert(False);
  end;
  frmGoPhast.UndoStack.Submit(Undo);
  FUndoType := utChange;
end;

procedure TframeHeadObservationResults.SetDataForAModel(
  HeadObs: THeadObsCollection);
begin
  HeadObs.MinLayerLimit := framelmtMinLayer.Limit;
  HeadObs.MaxLayerLimit := framelmtMaxLayer.Limit;
  HeadObs.MinResidualLimit := framelmtMinResidual.Limit;
  HeadObs.MaxResidualLimit := framelmtMaxResidual.Limit;
  HeadObs.MinTimeLimit := framelmtMinimumTime.Limit;
  HeadObs.MaxTimeLimit := framelmtMaximumTime.Limit;
  HeadObs.NegativeColor := clrbtnNegative.Color;
  HeadObs.PositiveColor := clrbtnPositive.Color;
  HeadObs.MaxSymbolSize := spinSymbolSize.AsInteger;
  HeadObs.Visible := cbShow.Checked;
end;

procedure TframeHeadObservationResults.spinSymbolSizeChange(Sender: TObject);
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

procedure TframeHeadObservationResults.TestForNewFiles;
var
  ObsItem: TObsHeadLink;
  ReadFiles: Boolean;
  Index: Integer;
  FileDate: TDateTime;
  NewerFiles: TStringList;
begin
  NewerFiles := TStringList.Create;
  try
    for Index := 0 to ObsLinkList.Count - 1 do
    begin
      ObsItem := ObsLinkList[Index];
      if FileExists(ObsItem.FNewHeadObsCollection.FileName) then
      begin
        FileAge(ObsItem.FNewHeadObsCollection.FileName, FileDate);
        if FileDate <> ObsItem.FNewHeadObsCollection.FileDate then
        begin
          NewerFiles.Add(ObsItem.FNewHeadObsCollection.FileName);
        end;
      end;
    end;
    ReadFiles := False;
    if NewerFiles.Count > 0 then
    begin
      Beep;
      if NewerFiles.Count = 1 then
      begin
        ReadFiles := MessageDlg(Format(StrTheFileSHasADi, [NewerFiles[0]]),
          mtWarning, [mbYes, mbNo], 0) = mrYes;
      end
      else
      begin
        NewerFiles.LineBreak := ', ';
        ReadFiles := MessageDlg(Format(StrTheFileSHasADiMultiple, [NewerFiles.Text]),
          mtWarning, [mbYes, mbNo], 0) = mrYes;
      end;
    end;
    for Index := 0 to ObsLinkList.Count - 1 do
    begin
      ObsItem := ObsLinkList[Index];
      if ReadFiles and FileExists(ObsItem.FNewHeadObsCollection.FileName) then
      begin
        FileAge(ObsItem.FNewHeadObsCollection.FileName, FileDate);
        if FileDate <> ObsItem.FNewHeadObsCollection.FileDate then
        begin
          ObsItem.FNewHeadObsCollection.ReadFromFile(ObsItem.FModel);
        end;
      end;
      if ObsItem.FNewHeadObsCollection.FileName = '' then
      begin
        ObsItem.FNewHeadObsCollection.Clear;
      end;
    end;
  finally
    NewerFiles.Free;
  end;
end;

procedure TframeHeadObservationResults.UpdateSelectedModel;
var
  ModelIndex: Integer;
begin
  ModelIndex := comboModels.ItemIndex;
  GetData;
  if ModelIndex < comboModels.Items.Count then
  begin
    comboModels.ItemIndex := ModelIndex;
    comboModelsChange(nil);
  end;
end;

procedure TframeHeadObservationResults.UpdateObsLinkList;
var
  Index: Integer;
  AModel: TBaseModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  LocalModel: TPhastModel;
begin
  if ObsLinkList = nil then
  begin
    ObsLinkList := TObsHeadLinkList.Create;
  end;
  ObsLinkList.Clear;
  LocalModel := frmGoPhast.PhastModel;
  ObsLinkList.Add(TObsHeadLink.Create(LocalModel));
  if LocalModel.LgrUsed then
  begin
    for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
    begin
      ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
      ObsLinkList.Add(TObsHeadLink.Create(ChildModel));
    end;
  end;
  for Index := comboModels.Items.Count - 1 downto 0 do
  begin
    AModel := comboModels.Items.Objects[Index] as TBaseModel;
    if ObsLinkList.IndexOfModel(AModel) < 0 then
    begin
      comboModels.Items.Delete(Index);
    end;
  end;
end;

function TframeHeadObservationResults.GetSelectedObjectsFromGraph(
  ScreenObjects: TStringList): string;
var
  ItemIndex: Integer;
  AScreenObject: TScreenObject;
  RowIndex: Integer;
  Selection: TGridRect;
begin
  if FSelectedObsItem <> nil then
  begin
    result := FSelectedObsItem.ScreenObjectName;
    ItemIndex := ScreenObjects.IndexOf(result);
    if ItemIndex >= 0 then
    begin
      AScreenObject := ScreenObjects.Objects[ItemIndex] as TScreenObject;
      AScreenObject.Selected := True;
    end;

    RowIndex := rdgHeadObs.Cols[Ord(ocName)].IndexOf(FSelectedObsItem.Name);
    if RowIndex >= 1 then
    begin
      if rdgHeadObs.VisibleRowCount < rdgHeadObs.RowCount - rdgHeadObs.FixedRows then
      begin
        rdgHeadObs.TopRow := RowIndex;
      end;
      Selection := rdgHeadObs.Selection;
      Selection.Top := RowIndex;
      Selection.Bottom := RowIndex;
      rdgHeadObs.Selection := Selection;
    end;
  end;
end;

function TframeHeadObservationResults.GetSelectedObjectsFromGrid(
  ScreenObjects: TStringList): string;
var
  RowIndex: Integer;
  ScreenObjectName: string;
  ColIndex: Integer;
  NameIndex: Integer;
  AScreenObject: TScreenObject;
begin
  result := '';
  if rdgHeadObs.SelectedRow <= 0 then
  begin
    Exit;
  end;
  for RowIndex := 1 to rdgHeadObs.RowCount - 1 do
  begin
    for ColIndex := 0 to rdgHeadObs.ColCount - 1 do
    begin
      if rdgHeadObs.IsSelectedCell(ColIndex, RowIndex) then
      begin
        ScreenObjectName := rdgHeadObs.Cells[Ord(ocObjectName), RowIndex];
        NameIndex := ScreenObjects.IndexOf(ScreenObjectName);
        if NameIndex >= 0 then
        begin
          AScreenObject := ScreenObjects.Objects[NameIndex] as TScreenObject;
          AScreenObject.Selected := True;
        end;
        break;
      end;
    end;
  end;
  result := rdgHeadObs.Cells[
    Ord(ocObjectName),rdgHeadObs.SelectedRow];
end;

procedure InitializeSortOrder;
var
  Index: TObsCol;
  CM: TCompareMethod;
begin
  SortOrder.Free;
  SortOrder := TObjectList.Create;
  for Index := Low(TObsCol) to High(TObsCol) do
  begin
    CM := TCompareMethod.Create;
    CM.Method := Index;
    if CM.Method = ocOriginalOrder then
    begin
      SortOrder.Insert(0, CM)
    end
    else
    begin
      SortOrder.Add(CM)
    end;
  end;
end;

initialization
  InitializeSortOrder;

finalization
  SortOrder.Free;

end.
