unit frameMt3dObsResultsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, QuadTreeClass,
  Vcl.StdCtrls, Vcl.CheckLst, Vcl.ExtCtrls, Vcl.Grids, RbwDataGrid4, JvSpin,
  JvExControls, JvColorBox, JvColorButton, frameDisplayLimitUnit, Vcl.Mask,
  JvExMask, JvToolEdit, Vcl.ComCtrls, Mt3dObservationResultsUnit, UndoItems,
  frmCustomGoPhastUnit, ScreenObjectUnit;

type
  TMt3dUndoType = (utMChange, utMImport);
  TMt3dObsColumns = (pocName, pocGroup, pocObject, pocTime, pocMeasured,
    pocModeled, pocResidual, pocWeight, pocWtMeas, pocWtMod, pocWtRes,
    pocMeasSD, pocNaturalWeight, pocOriginalOrder);

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
    btnNegative: TJvColorButton;
    btnPositive: TJvColorButton;
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
    lblWhatToPlot: TLabel;
    rgGraphType: TRadioGroup;
    clbWhatToPlot: TCheckListBox;
    pnlBottom: TPanel;
    lblRMS: TLabel;
    comboModels: TComboBox;
    qtreeeObservations: TRbwQuadTree;
    procedure fedHeadObsResultsChange(Sender: TObject);
  private
    FObservations: TMt3dObsCollection;
    FGettingData: Boolean;
    FUndoType: TMt3dUndoType;
    FImportResult: Boolean;
    procedure InitializeTableHeaders;
    procedure FillTable;
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
  frmGoPhastUnit, System.IOUtils, System.Generics.Collections, Mt3dmsTobUnit;

resourcestring
  StrImportMT3DObservat = 'import MT3D observation results';
  StrChangeMT3DObservat = 'change MT3D observation result options';

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
  FOldObservations := TMt3dObsCollection.Create(nil);
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
  FObservations := TMt3dObsCollection.Create(nil);
end;

destructor TframeMt3dObsResults.Destroy;
begin
  FObservations.Free;
  inherited;
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
//          rdgMt3dObs.Objects[Ord(pocObject), ItemIndex+1] :=  AnItem.ScreenObject;
          rdgMt3dObs.RealValue[Ord(pocTime), ItemIndex+1] := AnItem.Time;
//          rdgMt3dObs.RealValue[Ord(pocMeasured), ItemIndex+1] := AnItem.Measured;
          rdgMt3dObs.RealValue[Ord(pocModeled), ItemIndex+1] := AnItem.SimulatedValue;
//          if AnItem.ResidualText = '' then
//          begin
//            rdgMt3dObs.RealValue[Ord(pocResidual), ItemIndex+1] := AnItem.Residual;
//          end
//          else
//          begin
//            rdgMt3dObs.Cells[Ord(pocResidual), ItemIndex+1] := AnItem.ResidualText;
//          end;

//          if AnItem.WeightText = '' then
//          begin
//            rdgMt3dObs.RealValue[Ord(pocWeight), ItemIndex+1] :=
//              AnItem.Weight;
//          end
//          else
//          begin
//            rdgMt3dObs.Cells[Ord(pocWeight), ItemIndex+1] :=
//              AnItem.WeightText;
//          end;
//
//          if AnItem.WeightedMeasuredText = '' then
//          begin
//            rdgMt3dObs.RealValue[Ord(pocWtMeas), ItemIndex+1] :=
//              AnItem.WeightedMeasured;
//          end
//          else
//          begin
//            rdgMt3dObs.Cells[Ord(pocWtMeas), ItemIndex+1] :=
//              AnItem.WeightedMeasuredText;
//          end;

//          if AnItem.WeightedModeledText = '' then
//          begin
//            rdgMt3dObs.RealValue[Ord(pocWtMod), ItemIndex+1] :=
//              AnItem.WeightedModeled;
//          end
//          else
//          begin
//            rdgMt3dObs.Cells[Ord(pocWtMod), ItemIndex+1] :=
//              AnItem.WeightedModeledText;
//          end;
//
//          if AnItem.WeightedResidualText = '' then
//          begin
//            rdgMt3dObs.RealValue[Ord(pocWtRes), ItemIndex+1] :=
//              AnItem.WeightedResidual;
//          end
//          else
//          begin
//            rdgMt3dObs.Cells[Ord(pocWtRes), ItemIndex+1] :=
//              AnItem.WeightedResidualText;
//          end;

//          if AnItem.MeasurementStdDeviationText = '' then
//          begin
//            rdgMt3dObs.RealValue[Ord(pocMeasSD), ItemIndex+1] :=
//              AnItem.MeasurementStdDeviation;
//          end
//          else
//          begin
//            rdgMt3dObs.Cells[Ord(pocMeasSD), ItemIndex+1] :=
//              AnItem.MeasurementStdDeviationText;
//          end;
//          if AnItem.NaturalWeightText = '' then
//          begin
//            rdgMt3dObs.RealValue[Ord(pocNaturalWeight), ItemIndex+1] :=
//              AnItem.NaturalWeight;
//          end
//          else
//          begin
//            rdgMt3dObs.Cells[Ord(pocNaturalWeight), ItemIndex+1] :=
//              AnItem.NaturalWeightText;
//          end;
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
//  DisplayRMS;
end;

procedure TframeMt3dObsResults.GetData;
begin
  FGettingData := True;
  try
    clbWhatToPlot.CheckAll(cbChecked);
    lblRMS.Caption := 'Root Mean Square Weighted Residual = ?';
    InitializeTableHeaders;
    if FObservations = nil then
    begin
      FObservations := TMt3dObsCollection.Create(nil);
    end;
    FObservations.Assign(frmGoPhast.PhastModel.Mt3dObsCollection);
    fedHeadObsResults.FileName := FObservations.FileName;
    FillTable;

//    framelmtMinResidual.Limit := FObservations.MinResidualLimit;
//    framelmtMaxResidual.Limit := FObservations.MaxResidualLimit;
//    framelmtMinimumTime.Limit := FObservations.MinTimeLimit;
//    framelmtMaximumTime.Limit := FObservations.MaxTimeLimit;
//    framelmtMinWeightedResidual.Limit := FObservations.MinWeightedResidualLimit;
//    framelmtMaxWeightedResidual.Limit := FObservations.MaxWeightedResidualLimit;
//
//    clrbtnNegative.Color := FObservations.NegativeColor;
//    clrbtnPositive.Color := FObservations.PositiveColor;
//
//    rgDrawChoice.ItemIndex := Ord(FObservations.DrawChoice);
//    cbShow.Checked := FObservations.Visible;
//    spinSymbolSize.AsInteger := FObservations.MaxSymbolSize;
//    spinSymbolSizeChange(nil);

    FUndoType := utMChange;
  finally
    FGettingData := False;
  end;
end;

procedure TframeMt3dObsResults.InitializeTableHeaders;
begin
  rdgMt3dObs.Cells[Ord(pocName), 0] := 'Observation Name';
  rdgMt3dObs.Cells[Ord(pocGroup), 0] := 'Group Name';
  rdgMt3dObs.Cells[Ord(pocObject), 0] := 'Object Name';
  rdgMt3dObs.Cells[Ord(pocTime), 0] := 'Time';
  rdgMt3dObs.Cells[Ord(pocMeasured), 0] := 'Measured';
  rdgMt3dObs.Cells[Ord(pocModeled), 0] := 'Modeled';
  rdgMt3dObs.Cells[Ord(pocResidual), 0] := 'Residual';
  rdgMt3dObs.Cells[Ord(pocWeight), 0] := 'Weight';
  rdgMt3dObs.Cells[Ord(pocWtMeas), 0] := 'Weight * Measured';
  rdgMt3dObs.Cells[Ord(pocWtMod), 0] := 'Weight * Modeled';
  rdgMt3dObs.Cells[Ord(pocWtRes), 0] := 'Weight * Residual';
  rdgMt3dObs.Cells[Ord(pocMeasSD), 0] := 'Measured SD';
  rdgMt3dObs.Cells[Ord(pocNaturalWeight), 0] := 'Natural Weight';
end;

procedure TframeMt3dObsResults.SetData;
var
  Undo: TCustomUndoChangeMt3dObsResults;
begin
  FObservations.FileName := fedHeadObsResults.FileName;
//  FObservations.MinResidualLimit := framelmtMinResidual.Limit;
//  FObservations.MaxResidualLimit := framelmtMaxResidual.Limit;
//  FObservations.MinTimeLimit := framelmtMinimumTime.Limit;
//  FObservations.MaxTimeLimit := framelmtMaximumTime.Limit;
//  FObservations.MinWeightedResidualLimit := framelmtMinWeightedResidual.Limit;
//  FObservations.MaxWeightedResidualLimit := framelmtMaxWeightedResidual.Limit;
//  FObservations.DrawChoice := TDrawChoice(rgDrawChoice.ItemIndex);
//  FObservations.Visible := cbShow.Checked;
//  FObservations.NegativeColor := clrbtnNegative.Color;
//  FObservations.PositiveColor := clrbtnPositive.Color;
//  FObservations.MaxSymbolSize := spinSymbolSize.AsInteger;
//  TestForNewFile;
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

end.
