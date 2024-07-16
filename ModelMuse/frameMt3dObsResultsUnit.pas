unit frameMt3dObsResultsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, QuadTreeClass,
  Vcl.StdCtrls, Vcl.CheckLst, Vcl.ExtCtrls, Vcl.Grids, RbwDataGrid4, JvSpin,
  JvExControls, JvColorBox, JvColorButton, frameDisplayLimitUnit, Vcl.Mask,
  JvExMask, JvToolEdit, Vcl.ComCtrls, Mt3dObservationResultsUnit, UndoItems,
  frmCustomGoPhastUnit, ScreenObjectUnit, System.Generics.Defaults, System.Math;

type
  TMt3dUndoType = (utMChange, utMImport);
  TMt3dObsColumns = (pocName, pocObject, pocTime, pocMeasured,
    pocModeled, pocResidual, pocWeight, pocWtMeas, pocWtMod, pocWtRes,
    pocOriginalOrder);

  TWhatToPlot = (wtpObservations, wtpPriorInformation);

  TCustomUndoChangeMt3dObsResults = class(TCustomUndo)
  private
    FNewObservations: TMt3dObsCollection;
    FOldObservations: TMt3dObsCollection;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure ApplyChange(Observations: TMt3dObsCollection);
    procedure UpdateGUI;
  public
    procedure DoCommand; override;
    procedure Undo; override;
  public
    constructor Create(var Observations: TMt3dObsCollection);
    Destructor Destroy; override;
  end;

  TUndoChangeMt3dObsResults = class(TCustomUndoChangeMt3dObsResults)
  protected
    function Description: string; override;
  end;

  TUndoImportChangeMt3dObsResults = class(TCustomUndoChangeMt3dObsResults)
  protected
    function Description: string; override;
  end;

  TframeMt3dObsResults = class(TFrame)
    pgcObservations: TPageControl;
    tabControls: TTabSheet;
    lblNegativeColor: TLabel;
    lblColorPositive: TLabel;
    lblMaxSymbolSize: TLabel;
    lblHeadObsResults: TLabel;
    fedHeadObsResults: TJvFilenameEdit;
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
    seSymbolSize: TJvSpinEdit;
    cbShow: TCheckBox;
    rgDrawChoice: TRadioGroup;
    tabValues: TTabSheet;
    rdgMt3dObs: TRbwDataGrid4;
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
    pnlBottom: TPanel;
    lblRMS: TLabel;
    comboModels: TComboBox;
    qtreeObservations: TRbwQuadTree;
    procedure btnCopyClick(Sender: TObject);
    procedure btnHightlightObjectsClick(Sender: TObject);
    procedure fedHeadObsResultsChange(Sender: TObject);
    procedure seSymbolSizeChange(Sender: TObject);
    procedure rgGraphTypeClick(Sender: TObject);
    procedure pgcObservationsChange(Sender: TObject);
    procedure pbObservationsMouseDown(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
    procedure pbObservationsMouseMove(Sender: TObject; Shift: TShiftState; X, Y:
        Integer);
    procedure pbObservationsMouseUp(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
    procedure pbObservationsPaint(Sender: TObject);
    procedure rdgMt3dObsMouseUp(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
    procedure btnRestoreClick(Sender: TObject);
    procedure rgDrawChoiceClick(Sender: TObject);
  private
    FObservations: TMt3dObsCollection;
    FGettingData: Boolean;
    FUndoType: TMt3dUndoType;
    FImportResult: Boolean;
    FSelectedObsItem: TMt3dObsResult;
    procedure InitializeTableHeaders;
    procedure FillTable;
    function GetSelectedObjectFromGrid: TScreenObject;
    function GetSelectedObjectFromGraph: TScreenObject;
    procedure PlotValues;
    procedure TestForNewFile;
    procedure DisplayRMS;
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
  frmGoPhastUnit, System.IOUtils, System.Generics.Collections, Mt3dmsTobUnit,
  UndoItemsScreenObjects, GoPhastTypes, frmGoToUnit, xycommon, xygraph;

type
  TGridCrack = class(TRbwDataGrid4);
  TCompareMethod = class(TObject)
    Method: TMt3dObsColumns;
  end;

  var
  Data: Tdatatype;
  SortOrder: TList = nil;

resourcestring
  StrTheFileSHasADi = 'The file %s has a different date than the imported re' +
  'sults. Do you want to import it?';
  StrImportMT3DObservat = 'import MT3D observation results';
  StrChangeMT3DObservat = 'change MT3D observation result options';
  StrRootMeanSquareWeightRes = 'Root Mean Square Weighted Residual = %g';
  StrRootMeanSquareRes = 'Root Mean Square Residual = %g';
  StrRootMeanSquareResQ = 'Root Mean Square Residual = ?';
  StrRootMeanSquareWeiQ = 'Root Mean Square Weighted Residual = ?';
  StrShowResiduals = 'Show residuals';
  StrShowWeightedResidu = 'Show weighted residuals';

{$R *.dfm}


function CompareObservations(Item1, Item2: Pointer): Integer;
begin
  result := 0;
end;

{ TCustomUndoChangeMt3dObsResults }

procedure TCustomUndoChangeMt3dObsResults.ApplyChange(
  Observations: TMt3dObsCollection);
begin
  frmGoPhast.PhastModel.Mt3dObsCollection := Observations;
end;

constructor TCustomUndoChangeMt3dObsResults.Create(
  var Observations: TMt3dObsCollection);
begin
  FNewObservations := Observations;
  Observations := nil;
  FOldObservations := TMt3dObsCollection.Create(frmGoPhast.PhastModel);
  FOldObservations.Assign(frmGoPhast.PhastModel.Mt3dObsCollection);
end;

destructor TCustomUndoChangeMt3dObsResults.Destroy;
begin
  FNewObservations.Free;
  FOldObservations.Free;
  inherited;
end;

procedure TCustomUndoChangeMt3dObsResults.DoCommand;
begin
  ApplyChange(FNewObservations);
  UpdateGUI;
  inherited;
end;

procedure TCustomUndoChangeMt3dObsResults.Undo;
begin
  ApplyChange(FOldObservations);
  UpdateGUI;
  inherited;
end;

procedure TCustomUndoChangeMt3dObsResults.UpdateGUI;
begin
  frmGoPhast.TopDiscretizationChanged := True;
  frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
end;

{ TUndoChangeMt3dObsResults }

function TUndoChangeMt3dObsResults.Description: string;
begin
  result :=  StrImportMT3DObservat
end;

{ TUndoImportChangeMt3dObsResults }

function TUndoImportChangeMt3dObsResults.Description: string;
begin
  result := StrChangeMT3DObservat;
end;

{ TframeMt3dObsResults }

constructor TframeMt3dObsResults.Create(Owner: TComponent);
begin
  inherited;
  FObservations := TMt3dObsCollection.Create(frmGoPhast.PhastModel);
end;

destructor TframeMt3dObsResults.Destroy;
begin
  FObservations.Free;
  inherited;
end;

procedure TframeMt3dObsResults.DisplayRMS;
begin
  if FObservations.Count > 0 then
  begin
    case rgDrawChoice.ItemIndex of
      0:
        begin
          lblRMS.Caption := Format(StrRootMeanSquareRes, [FObservations.RootMeanSquareResidual]);
          lblMax.Caption := FloatToStr(FObservations.MaxObjectResidual);
          lblHalfMax.Caption := FloatToStr(FObservations.MaxObjectResidual/2);
        end;
      1:
        begin
          lblRMS.Caption := Format(StrRootMeanSquareWeightRes, [FObservations.RootMeanSquareWeightedResidual]);
          lblMax.Caption := FloatToStr(FObservations.MaxObjectWeightedResidual);
          lblHalfMax.Caption := FloatToStr(FObservations.MaxObjectWeightedResidual/2);
        end;
    end;
  end
  else
  begin
    lblMax.Caption := '';
    lblHalfMax.Caption := '';
    case rgDrawChoice.ItemIndex of
      0:
        begin
          lblRMS.Caption := StrRootMeanSquareResQ;
        end;
      1:
        begin
          lblRMS.Caption := StrRootMeanSquareWeiQ;
        end;
    end;
  end;
end;

procedure TframeMt3dObsResults.btnCopyClick(Sender: TObject);
begin
  rdgMt3dObs.CopyAllCellsToClipboard;
end;

procedure TframeMt3dObsResults.btnHightlightObjectsClick(Sender: TObject);
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

procedure TframeMt3dObsResults.btnRestoreClick(Sender: TObject);
var
  ObsCol: TMt3dObsColumns;
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

procedure TframeMt3dObsResults.fedHeadObsResultsChange(Sender: TObject);
var
  ObsNameDictionary: TObsNameDictionary;
  AScreenObject: TSCreenObject;
  TransObservations: TMt3dmsTransObservations;
  ObsName: string;
begin
  if FGettingData then
  begin
    Exit;
  end;
  if TFile.Exists(fedHeadObsResults.FileName) then
  begin
    if TFile.GetSize(fedHeadObsResults.FileName) = 0 then
    begin
      Beep;
      MessageDlg(Format('%s is empty.',
        [fedHeadObsResults.FileName]), mtWarning, [mbOK], 0);
      Exit;
    end;
    ObsNameDictionary := TObsNameDictionary.Create;
    try
      for var ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
      begin
        AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
        if AScreenObject.Deleted then
        begin
          Continue;
        end;
        TransObservations := AScreenObject.Mt3dmsTransObservations;
        if TransObservations = nil then
        begin
          Continue;
        end;
        if TransObservations.Values.Count > 1 then
        begin
          for var ObsIndex := 0 to TransObservations.Values.Count - 1 do
          begin
            ObsName := TransObservations.ObservationName + '_' + IntToStr(ObsIndex+1);
            ObsNameDictionary.Add(ObsName, AScreenObject.Name)
          end;
        end
        else
        begin
          ObsNameDictionary.Add(TransObservations.ObservationName,
            AScreenObject.Name)
        end;
      end;
      FImportResult := FObservations.ReadFromFile(fedHeadObsResults.FileName,
        ObsNameDictionary);
    finally
      ObsNameDictionary.Free;
    end;
    FUndoType := utMImport;
  end
  else
  begin
    FObservations.FileName := '';
    FObservations.FileDate := 0;
    FObservations.Clear;
  end;
  FillTable;
end;

procedure TframeMt3dObsResults.FillTable;
var
  ItemIndex: Integer;
  AnItem: TMt3dObsResult;
  AList: TList;
begin
  FObservations.CalculateMaxValues;
  if FObservations.Count > 0 then
  begin
    rdgMt3dObs.BeginUpdate;
    try
      rdgMt3dObs.RowCount := FObservations.Count+1;
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
          rdgMt3dObs.Cells[Ord(pocName), ItemIndex+1] := AnItem.Name;
//          rdgMt3dObs.Cells[Ord(pocGroup), ItemIndex+1] := AnItem.GroupName;
          rdgMt3dObs.Cells[Ord(pocObject), ItemIndex+1] := AnItem.ScreenObjectName;
          rdgMt3dObs.Objects[Ord(pocObject), ItemIndex+1] :=  AnItem.ScreenObject;
          rdgMt3dObs.RealValue[Ord(pocTime), ItemIndex+1] := AnItem.Time;
          rdgMt3dObs.RealValue[Ord(pocMeasured), ItemIndex+1] := AnItem.Measured;
          rdgMt3dObs.RealValue[Ord(pocModeled), ItemIndex+1] := AnItem.SimulatedValue;
          if AnItem.ResidualText = '' then
          begin
            rdgMt3dObs.RealValue[Ord(pocResidual), ItemIndex+1] := AnItem.Residual;
          end
          else
          begin
            rdgMt3dObs.Cells[Ord(pocResidual), ItemIndex+1] := AnItem.ResidualText;
          end;

          if AnItem.WeightText = '' then
          begin
            rdgMt3dObs.RealValue[Ord(pocWeight), ItemIndex+1] :=
              AnItem.Weight;
          end
          else
          begin
            rdgMt3dObs.Cells[Ord(pocWeight), ItemIndex+1] :=
              AnItem.WeightText;
          end;

          if AnItem.WeightedMeasuredText = '' then
          begin
            rdgMt3dObs.RealValue[Ord(pocWtMeas), ItemIndex+1] :=
              AnItem.WeightedMeasured;
          end
          else
          begin
            rdgMt3dObs.Cells[Ord(pocWtMeas), ItemIndex+1] :=
              AnItem.WeightedMeasuredText;
          end;

          if AnItem.WeightedSimulatedText = '' then
          begin
            rdgMt3dObs.RealValue[Ord(pocWtMod), ItemIndex+1] :=
              AnItem.WeightedSimulated;
          end
          else
          begin
            rdgMt3dObs.Cells[Ord(pocWtMod), ItemIndex+1] :=
              AnItem.WeightedSimulatedText;
          end;

          if AnItem.WeightedResidualText = '' then
          begin
            rdgMt3dObs.RealValue[Ord(pocWtRes), ItemIndex+1] :=
              AnItem.WeightedResidual;
          end
          else
          begin
            rdgMt3dObs.Cells[Ord(pocWtRes), ItemIndex+1] :=
              AnItem.WeightedResidualText;
          end;
        end;
      finally
        AList.Free;
      end;
    finally
      rdgMt3dObs.EndUpdate;
    end;
  end
  else
  begin
    ClearGrid(rdgMt3dObs);
  end;
  DisplayRMS;
end;

procedure TframeMt3dObsResults.GetData;
begin
  FGettingData := True;
  try
    lblRMS.Caption := 'Root Mean Square Weighted Residual = ?';
    InitializeTableHeaders;
    if FObservations = nil then
    begin
      FObservations := TMt3dObsCollection.Create(frmGoPhast.PhastModel);
    end;
    FObservations.Assign(frmGoPhast.PhastModel.Mt3dObsCollection);
    FObservations.CalculateMaxValues;
    fedHeadObsResults.FileName := FObservations.FileName;
    FillTable;

    framelmtMinResidual.Limit := FObservations.MinResidualLimit;
    framelmtMaxResidual.Limit := FObservations.MaxResidualLimit;
    framelmtMinimumTime.Limit := FObservations.MinTimeLimit;
    framelmtMaximumTime.Limit := FObservations.MaxTimeLimit;
    framelmtMinWeightedResidual.Limit := FObservations.MinWeightedResidualLimit;
    framelmtMaxWeightedResidual.Limit := FObservations.MaxWeightedResidualLimit;

    clrbtnNegative.Color := FObservations.NegativeColor;
    clrbtnPositive.Color := FObservations.PositiveColor;

    rgDrawChoice.ItemIndex := Ord(FObservations.DrawChoice);
    cbShow.Checked := FObservations.Visible;
    seSymbolSize.AsInteger := FObservations.MaxSymbolSize;
    seSymbolSizeChange(nil);

    FUndoType := utMChange;
  finally
    FGettingData := False;
  end;
end;

function TframeMt3dObsResults.GetSelectedObjectFromGraph: TScreenObject;
var
  RowIndex: Integer;
  Selection: TGridRect;
begin
  result := nil;
  if FSelectedObsItem <> nil then
  begin
    result := frmGoPhast.PhastModel.GetScreenObjectByName(FSelectedObsItem.ScreenObjectName);
    if result <> nil then
    begin
      result.Selected := True;
    end;

    RowIndex := rdgMt3dObs.Cols[Ord(pocName)].IndexOf(FSelectedObsItem.Name);
    if RowIndex >= 1 then
    begin
      if rdgMt3dObs.VisibleRowCount < rdgMt3dObs.RowCount - rdgMt3dObs.FixedRows then
      begin
        rdgMt3dObs.TopRow := RowIndex;
      end;
      Selection := rdgMt3dObs.Selection;
      Selection.Top := RowIndex;
      Selection.Bottom := RowIndex;
      rdgMt3dObs.Selection := Selection;
    end;
  end;
end;

function TframeMt3dObsResults.GetSelectedObjectFromGrid: TScreenObject;
var
  ScreenObjectName: string;
begin
  result := nil;
  if rdgMt3dObs.SelectedRow <= 0 then
  begin
    Exit;
  end;
  ScreenObjectName := rdgMt3dObs.Cells[Ord(pocObject), rdgMt3dObs.SelectedRow];
  if ScreenObjectName <> '' then
  begin
    result := frmGoPhast.PhastModel.GetScreenObjectByName(ScreenObjectName)
  end;
end;

procedure TframeMt3dObsResults.InitializeTableHeaders;
begin
  rdgMt3dObs.Cells[Ord(pocName), 0] := 'Observation Name';
  rdgMt3dObs.Cells[Ord(pocObject), 0] := 'Object Name';
  rdgMt3dObs.Cells[Ord(pocTime), 0] := 'Time';
  rdgMt3dObs.Cells[Ord(pocMeasured), 0] := 'Measured';
  rdgMt3dObs.Cells[Ord(pocModeled), 0] := 'Modeled';
  rdgMt3dObs.Cells[Ord(pocResidual), 0] := 'Residual';
  rdgMt3dObs.Cells[Ord(pocWeight), 0] := 'Weight';
  rdgMt3dObs.Cells[Ord(pocWtMeas), 0] := 'Weight * Measured';
  rdgMt3dObs.Cells[Ord(pocWtMod), 0] := 'Weight * Modeled';
  rdgMt3dObs.Cells[Ord(pocWtRes), 0] := 'Weight * Residual';
end;

procedure TframeMt3dObsResults.pbObservationsMouseDown(Sender: TObject; Button:
    TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  XYMouseDown(Button, Shift, X, Y);
end;

procedure TframeMt3dObsResults.pbObservationsMouseMove(Sender: TObject; Shift:
    TShiftState; X, Y: Integer);
begin
  XYMouseMove(Shift, X, Y);
end;

procedure TframeMt3dObsResults.pbObservationsMouseUp(Sender: TObject; Button:
    TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  XYMouseup(Button, Shift, X, Y);
  FSelectedObsItem := nil;
  if qtreeObservations.Count > 0 then
  begin
    FSelectedObsItem := qtreeObservations.NearestPointsFirstData
      (xyexportb.xw, xyexportb.yw[1]);
    btnHightlightObjectsClick(nil);
    FSelectedObsItem.PlotLabel := not FSelectedObsItem.PlotLabel;
    PlotValues;
  end;
end;

procedure TframeMt3dObsResults.pbObservationsPaint(Sender: TObject);
begin
  PlotValues;
end;

procedure TframeMt3dObsResults.pgcObservationsChange(Sender: TObject);
begin
  if pgcObservations.ActivePage = tabGraph then
  begin
    PlotValues
  end;
end;

procedure TframeMt3dObsResults.PlotValues;
var
  index: Integer;
  ObservedMin: Double;
  SimulatedMin: Double;
  ObservedMax: Double;
  SimulatedMax: Double;
  SimValue: Double;
  Initialized: Boolean;
  Min1To1: double;
  Max1To1: double;
  Count: Integer;
  SimList: TList<TMt3dObsResult>;
  ObsItem: TMt3dObsResult;
  CoordX: Integer;
  CoordY: Integer;
//  SimItem: TMt3dObsResult;
  function NearlyTheSame(const X, Y: extended): boolean;
  const
    Epsilon = 1e-6;
  begin
    result := (X = Y) or (Abs(X - Y) < Epsilon) or
      (Abs(X - Y) / (Abs(X) + Abs(Y) + Epsilon) < Epsilon);
  end;
  function OkSimValue(ASimulatedValue: Double): Boolean;
  begin
    result := True;
  end;
  function OkResidual(Residual: Double): Boolean;
  begin
    result := True;
    if framelmtMinResidual.cbCheck.Checked then
    begin
      result := Residual >= framelmtMinResidual.rdeLimit.RealValue;
    end;
    if framelmtMaxResidual.cbCheck.Checked then
    begin
      result := result and (Residual <= framelmtMaxResidual.rdeLimit.RealValue);
    end;
  end;
  function OkTime(Time: Double): Boolean;
  begin
    result := True;
    if framelmtMinimumTime.cbCheck.Checked then
    begin
      result := Time >= framelmtMinimumTime.rdeLimit.RealValue;
    end;
    if framelmtMaximumTime.cbCheck.Checked then
    begin
      result := result and (Time <= framelmtMaximumTime.rdeLimit.RealValue);
    end;
  end;
  function OkWeigthedResidual(WeightedResidual: Double): Boolean;
  begin
    result := True;
    if framelmtMinWeightedResidual.cbCheck.Checked then
    begin
      result := WeightedResidual >= framelmtMinWeightedResidual.rdeLimit.RealValue;
    end;
    if framelmtMaxWeightedResidual.cbCheck.Checked then
    begin
      result := result and (WeightedResidual <= framelmtMaxWeightedResidual.rdeLimit.RealValue);
    end;
  end;
  function OkItem(ObsItem: TMt3dObsResult): Boolean;
  begin
    result := True;
    case rgGraphType.ItemIndex of
      0: result := True;
      1: result := ObsItem.ResidualText = '';
      2: result := ObsItem.WeightedResidualText = '';
      else
        Assert(False);
    end;

  end;
  function GetPlotValue(ObsItem: TMt3dObsResult): double;
  begin
    result := 0;
    case rgGraphType.ItemIndex of
      0: result := ObsItem.SimulatedValue;
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
    SimList := TList<TMt3dObsResult>.Create;
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
        if not OkItem(ObsItem) then
        begin
          Continue;
        end;

        if ObsItem.Measured < ObservedMin then
        begin
          ObservedMin := ObsItem.Measured
        end
        else if ObsItem.Measured > ObservedMax then
        begin
          ObservedMax := ObsItem.Measured
        end;

        SimValue := GetPlotValue(ObsItem);
        if OkSimValue(SimValue) and OkResidual(ObsItem.Residual)
          and OkTime(ObsItem.Time) and OkWeigthedResidual(ObsItem.WeightedResidual) then
        begin
          SimList.Add(ObsItem);
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

      if SimList.Count = 0 then
      begin
        xycleargraph(pbObservations,clWhite,clBlack,1);
        Exit;
      end;

      SimList.Sort(TComparer<TMt3dObsResult>.Construct(
        function (const L, R: TMt3dObsResult): Integer
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

        for index := 0 to SimList.Count - 1 do
        begin
          ObsItem := SimList[index];
          if ObsItem.PlotLabel then
          begin
            SimValue := GetPlotValue(ObsItem);

            xyusertoabs(ObsItem.Measured, SimValue, CoordX, CoordY);
            simpletext(clBlack, ObsItem.Name, CoordX, CoordY-10, 0,0);
          end;
        end;
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

procedure TframeMt3dObsResults.rdgMt3dObsMouseUp(Sender: TObject; Button:
    TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol: Integer;
  ARow: Integer;
  ObsCol: TMt3dObsColumns;
  Index: integer;
  CM: TCompareMethod;
begin
  rdgMt3dObs.MouseToCell(X, Y, ACol, ARow);
  if (ARow = 0) and (ACol >= 0) and (ACol < rdgMt3dObs.ColCount) then
  begin
    TGridCrack(rdgMt3dObs).HideEditor;
    ObsCol := TMt3dObsColumns(ACol);
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

procedure TframeMt3dObsResults.rgDrawChoiceClick(Sender: TObject);
begin
  DisplayRMS;
  case rgDrawChoice.ItemIndex of
    0:
      begin
        cbShow.Caption := StrShowResiduals;
      end;
    1:
      begin
        cbShow.Caption := StrShowWeightedResidu;
      end;
  end;
end;

procedure TframeMt3dObsResults.rgGraphTypeClick(Sender: TObject);
begin
  pbObservations.Invalidate;
end;

procedure TframeMt3dObsResults.seSymbolSizeChange(Sender: TObject);
begin
  shpMax.Height := seSymbolSize.AsInteger;
  shpMax.Width := seSymbolSize.AsInteger;
  shpHalfMax.Width := shpMax.Width;
  shpHalfMax.Height := Round(Sqrt(Sqr(seSymbolSize.AsInteger/2)/2)*2);
  shpHalfMax.Top := shpMax.Top + shpMax.Height + 8;
  lblMax.Left := shpMax.Left + shpMax.Width + 8;
  lblMax.Top := shpMax.Top + (shpMax.Height-lblMax.Height) div 2;
  lblHalfMax.Left := lblMax.Left;
  lblHalfMax.Top := shpHalfMax.Top + (shpHalfMax.Height-lblHalfMax.Height) div 2;
end;

procedure TframeMt3dObsResults.SetData;
var
  Undo: TCustomUndoChangeMt3dObsResults;
begin
  FObservations.FileName := fedHeadObsResults.FileName;
  FObservations.MinResidualLimit := framelmtMinResidual.Limit;
  FObservations.MaxResidualLimit := framelmtMaxResidual.Limit;
  FObservations.MinTimeLimit := framelmtMinimumTime.Limit;
  FObservations.MaxTimeLimit := framelmtMaximumTime.Limit;
  FObservations.MinWeightedResidualLimit := framelmtMinWeightedResidual.Limit;
  FObservations.MaxWeightedResidualLimit := framelmtMaxWeightedResidual.Limit;
  FObservations.DrawChoice := TMt3dDrawChoice(rgDrawChoice.ItemIndex);
  FObservations.Visible := cbShow.Checked;
  FObservations.NegativeColor := clrbtnNegative.Color;
  FObservations.PositiveColor := clrbtnPositive.Color;
  FObservations.MaxSymbolSize := seSymbolSize.AsInteger;
  TestForNewFile;
  Undo := nil;
  case FUndoType of
    utmChange:
      begin
        Undo := TUndoChangeMt3dObsResults.Create(FObservations);
      end;
    utmImport:
      begin
        Undo := TUndoImportChangeMt3dObsResults.Create(FObservations);
      end;
    else Assert(False);
  end;
  frmGoPhast.UndoStack.Submit(Undo);
  GetData;
end;

procedure TframeMt3dObsResults.TestForNewFile;
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
        fedHeadObsResultsChange(nil);
      end;
    end;
  end;
end;

end.
