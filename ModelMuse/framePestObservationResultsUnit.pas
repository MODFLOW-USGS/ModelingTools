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
  frmCustomGoPhastUnit, System.Generics.Collections, frmUndoUnit;

type
  TUndoType = (utChange, utImport);
  TPestObsColumns = (pocName, pocGroup, pocObject, pocMeasured, pocModeled,
    pocResidual, pocWeight, pocWtMeas, pocWtMod, pocWtRes, pocMeasSD,
    pocNaturalWeight);

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
    lblMinLayer: TLabel;
    lblMaxLayer: TLabel;
    framelmtMinimumTime: TframeDisplayLimit;
    framelmtMaxResidual: TframeDisplayLimit;
    framelmtMaximumTime: TframeDisplayLimit;
    framelmtMinResidual: TframeDisplayLimit;
    framelmtMinLayer: TframeDisplayLimit;
    framelmtMaxLayer: TframeDisplayLimit;
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
    pbHeadObs: TPaintBox;
    pnlGraphControls: TPanel;
    lblGraphInstructions: TLabel;
    rgGraphType: TRadioGroup;
    qtreeObservations: TRbwQuadTree;
    procedure flnmedHeadObsResultsChange(Sender: TObject);
  public
    procedure UpdateChildModels;
  protected
    procedure UpdateObsLinkList;
  private
    FUsedObservations: TDictionary<string, IObservationItem>;
    FObservations: TPestObsCollection;
    FGettingData: Boolean;
    FImportResult: Boolean;
    FUndoType: TUndoType;
    procedure GetExistingObservations;
    procedure GetData;
    procedure SetData;
    procedure FillTable;
    procedure InitializeTableHeaders;
    { Private declarations }
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
  end;

implementation

uses
  PestObsUnit, frmGoPhastUnit, ScreenObjectUnit;

{$R *.dfm}

constructor TframePestObservationResults.Create(Owner: TComponent);
begin
  inherited;
  FUsedObservations := TDictionary<string, IObservationItem>.Create;
  FObservations := TPestObsCollection.Create(nil);
end;

destructor TframePestObservationResults.Destroy;
begin
  FObservations.Free;
  FUsedObservations.Free;
  inherited;
end;

procedure TframePestObservationResults.InitializeTableHeaders;
begin
  rdgPestObs.Cells[Ord(pocName), 0] := 'Observation Name';
  rdgPestObs.Cells[Ord(pocGroup), 0] := 'Group Name';
  rdgPestObs.Cells[Ord(pocObject), 0] := 'Object Name';
  rdgPestObs.Cells[Ord(pocMeasured), 0] := 'Measured';
  rdgPestObs.Cells[Ord(pocModeled), 0] := 'Modeled';
  rdgPestObs.Cells[Ord(pocResidual), 0] := 'Residual';
  rdgPestObs.Cells[Ord(pocWeight), 0] := 'Weight';
  rdgPestObs.Cells[Ord(pocWtMeas), 0] := 'Weight * Measured';
  rdgPestObs.Cells[Ord(pocWtMod), 0] := 'Weight * Modeled';
  rdgPestObs.Cells[Ord(pocWtRes), 0] := 'Weight * Residual';
  rdgPestObs.Cells[Ord(pocMeasSD), 0] := 'Measured SD';
  rdgPestObs.Cells[Ord(pocNaturalWeight), 0] := 'Natural Weight';
//  TPestObsColumns = (pocName, pocGroup, pocObject, pocMeasured, pocModeled,
//    pocResidual, pocWeight, pocWtMeas, pocWtMod, pocWtRes, pocMeasSD,
//    pocNaturalWeight);
end;

procedure TframePestObservationResults.FillTable;
var
  ItemIndex: Integer;
  AnItem: TPestObsResult;
  Obs: IObservationItem;
  ScreenObject: TScreenObject;
begin
  if FObservations.Count > 0 then
  begin
    rdgPestObs.RowCount := FObservations.Count;
    for ItemIndex := 0 to FObservations.Count - 1 do
    begin
      AnItem := FObservations[ItemIndex];
      rdgPestObs.Cells[Ord(pocName), ItemIndex+1] := AnItem.Name;
      rdgPestObs.Cells[Ord(pocGroup), ItemIndex+1] := AnItem.GroupName;
      if FUsedObservations.TryGetValue(AnItem.Name, Obs) then
      begin
        ScreenObject := Obs.ScreenObject as TScreenObject;
        rdgPestObs.Cells[Ord(pocObject), ItemIndex+1] := ScreenObject.Name;
        rdgPestObs.Objects[Ord(pocObject), ItemIndex+1] := ScreenObject;
      end
      else
      begin
        rdgPestObs.Cells[Ord(pocObject), ItemIndex+1] := '';
        rdgPestObs.Objects[Ord(pocObject), ItemIndex+1] := nil;
      end;
      rdgPestObs.RealValue[Ord(pocMeasured), ItemIndex+1] := AnItem.Measured;
      rdgPestObs.RealValue[Ord(pocModeled), ItemIndex+1] := AnItem.Modelled;
      rdgPestObs.RealValue[Ord(pocResidual), ItemIndex+1] := AnItem.Residual;
      rdgPestObs.RealValue[Ord(pocWeight), ItemIndex+1] := AnItem.Weight;
      rdgPestObs.RealValue[Ord(pocWtMeas), ItemIndex+1] := AnItem.WeightedMeasured;
      rdgPestObs.RealValue[Ord(pocWtMod), ItemIndex+1] := AnItem.WeightedModelled;
      rdgPestObs.RealValue[Ord(pocWtRes), ItemIndex+1] := AnItem.WeightedResidual;
      rdgPestObs.RealValue[Ord(pocMeasSD), ItemIndex+1] := AnItem.MeasurementStdDeviation;
      rdgPestObs.RealValue[Ord(pocNaturalWeight), ItemIndex+1] := AnItem.NaturalWeight;
    end;
  end
  else
  begin
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
    FImportResult := FObservations.ReadFromFile(frmGoPhast.PhastModel);
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
    InitializeTableHeaders;
    GetExistingObservations;
    FObservations.Assign(frmGoPhast.PhastModel.PestObsCollection);
    flnmedHeadObsResults.FileName := FObservations.FileName;
    FillTable;
  finally
    FGettingData := False;
  end;
end;

procedure TframePestObservationResults.GetExistingObservations;
var
  TempList: TObservationInterfaceList;
  ObsIndex: Integer;
  IObs: IObservationItem;
  AnObs: TCustomObservationItem;
begin
  FUsedObservations.Clear;
  TempList := TObservationInterfaceList.Create;
  try
    frmGoPhast.PhastModel.FillObsInterfaceItemList(TempList, True);
    FUsedObservations.Capacity := TempList.Count;
    for ObsIndex := 0 to TempList.Count - 1 do
    begin
      IObs := TempList[ObsIndex];
      if IObs is TCustomObservationItem then
      begin
        AnObs := TCustomObservationItem(IObs);
        if AnObs.Print then
        begin
          FUsedObservations.Add(IObs.Name, IObs);
        end;
      end
      else
      begin
        FUsedObservations.Add(IObs.Name, IObs);
      end;
    end;
  finally
    TempList.Free;
  end;
end;

procedure TframePestObservationResults.SetData;
begin

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

end.
