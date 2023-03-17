unit framePestObsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameGridUnit,
  Vcl.StdCtrls, Vcl.ExtCtrls, System.Generics.Collections, Vcl.Grids,
  PestObsUnit, GoPhastTypes;

type
  TPestObsColumns = (pocName, pocType, pocGroup, pocTime, pocValue, pocWeight, pocComment);
  TPestObsCompColumns = (poccName, poccGroup, poccObs1, poccObs2, poccValue,
    poccWeight, poccComment);

  TframePestObs = class(TFrame)
    splObservations: TSplitter;
    grpDirectObs: TGroupBox;
    frameObservations: TframeGrid;
    grpObsComparisons: TGroupBox;
    frameObsComparisons: TframeGrid;
    procedure frameObservationsGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure frameObservationsGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameObsComparisonsGridEnter(Sender: TObject);
    procedure frameObservationssbAddClick(Sender: TObject);
    procedure frameObservationssbInsertClick(Sender: TObject);
    procedure frameObservationssbDeleteClick(Sender: TObject);
    procedure frameObsComparisonsGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameObservationsseNumberChange(Sender: TObject);
    procedure frameObsComparisonsseNumberChange(Sender: TObject);
  private
    FObservationsName: string;
    FMatchedCells1: TGenericIntegerList;
    FMatchedCells2: TGenericIntegerList;
    FOnControlsChange: TNotifyEvent;
    FGettingData: Boolean;
    procedure UpdatedSelectedCell;
    { Private declarations }
  protected
    procedure SetObsColumnCaptions; virtual;
    procedure GetDirectObs(Observations: TCustomComparisonCollection); virtual;
    procedure SetDirectObs(Observations: TCustomComparisonCollection;
      const LocationName: string); virtual;
    procedure GetObservationGroups; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitializeControls;
    procedure SpecifyObservationTypes(ObsTypes: TStrings);
    procedure GetData(Observations: TCustomComparisonCollection);
    procedure SetData(Observations: TCustomComparisonCollection;
      const LocationName: string);
    property OnControlsChange: TNotifyEvent read FOnControlsChange
      write FOnControlsChange;
    { Public declarations }
  end;

resourcestring
  StrObservationName = 'Observation Name (OBSNME)';
  StrObservationType = 'Observation Type';
  StrObservationTime = 'Observation Time';
  StrObservationValue = 'Observation Value (OBSVAL)';
  StrObservationValueDif = 'Observation Value (OBSVAL)' + sLineBreak +  '(OBSVAL1 - OBSVAL2)';
  StrObservationWeight = 'Observation Weight (WEIGHT)';
  StrComment = 'Comment';
  StrFirstObservation = 'First Observation (OBSNME1)';
  StrSecondObservation = 'Second Observation (OBSNME2)';
  StrObservationGroup = 'Observation Group (OBGNME)';

implementation

uses
  frmCustomGoPhastUnit, frmGoPhastUnit, PestObsGroupUnit;

{$R *.dfm}

{ TframePestObs }

constructor TframePestObs.Create(AOwner: TComponent);
begin
  inherited;
  FMatchedCells1 := TGenericIntegerList.Create;
  FMatchedCells2 := TGenericIntegerList.Create;
end;

destructor TframePestObs.Destroy;
begin
  FMatchedCells1.Free;
  FMatchedCells2.Free;
  inherited;
end;

procedure TframePestObs.frameObsComparisonsGridEnter(Sender: TObject);
var
  ObsNames: TStringList;
begin
  inherited;
  ObsNames := TStringList.Create;
  try
    ObsNames.Assign(frameObservations.Grid.Cols[0]);
    ObsNames.Delete(0);

    frameObsComparisons.Grid.Columns[Ord(poccObs1)].Picklist := ObsNames;
    frameObsComparisons.Grid.Columns[Ord(poccObs2)].Picklist := ObsNames;
  finally
    ObsNames.Free;
  end;
end;

procedure TframePestObs.frameObsComparisonsGridSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  if (not FGettingData) and Assigned(OnControlsChange) then
  begin
    OnControlsChange(Self);
  end;
end;

procedure TframePestObs.frameObsComparisonsseNumberChange(Sender: TObject);
begin
  frameObsComparisons.seNumberChange(Sender);
  if Assigned(OnControlsChange) then
  begin
    OnControlsChange(Self);
  end;
end;

procedure TframePestObs.frameObservationsGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  RowIndex: Integer;
begin
  inherited;
  if not frameObservations.Grid.Drawing
    and (Ord(pocName) = ACol) and (ARow >= 1) then
  begin
    FObservationsName := frameObservations.Grid.Cells[ACol, ARow];
    FMatchedCells1.Clear;
    FMatchedCells2.Clear;
    for RowIndex := 1 to frameObsComparisons.Grid.RowCount - 1 do
    begin
      if frameObsComparisons.Grid.Cells[Ord(poccObs1), RowIndex] = FObservationsName then
      begin
        FMatchedCells1.Add(RowIndex);
      end;
      if frameObsComparisons.Grid.Cells[Ord(poccObs2), RowIndex] = FObservationsName then
      begin
        FMatchedCells2.Add(RowIndex);
      end;
    end;
  end;
end;

procedure TframePestObs.frameObservationsGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  Index: Integer;
  RowIndex: Integer;
begin
  inherited;
  if (Ord(pocName) = ACol) and (ARow >= 1) then
  begin
    if (FObservationsName <> Value) and (FObservationsName <> '') then
    begin
      for Index := 0 to FMatchedCells1.Count - 1 do
      begin
        RowIndex := FMatchedCells1[Index];
        frameObsComparisons.Grid.Cells[Ord(poccObs1), RowIndex] := Value;
      end;
      for Index := 0 to FMatchedCells2.Count - 1 do
      begin
        RowIndex := FMatchedCells2[Index];
        frameObsComparisons.Grid.Cells[Ord(poccObs2), RowIndex] := Value;
      end;
    end;
  end;
  if Assigned(OnControlsChange) then
  begin
    OnControlsChange(Self);
  end;
end;

procedure TframePestObs.frameObservationssbAddClick(Sender: TObject);
begin
  frameObservations.sbAddClick(Sender);
  UpdatedSelectedCell;
end;

procedure TframePestObs.frameObservationssbDeleteClick(Sender: TObject);
begin
  frameObservations.sbDeleteClick(Sender);
  UpdatedSelectedCell;
end;

procedure TframePestObs.frameObservationssbInsertClick(Sender: TObject);
begin
  frameObservations.sbInsertClick(Sender);
  UpdatedSelectedCell;
end;

procedure TframePestObs.frameObservationsseNumberChange(Sender: TObject);
begin
  frameObservations.seNumberChange(Sender);
  if Assigned(OnControlsChange) then
  begin
    OnControlsChange(Self);
  end;
end;

procedure TframePestObs.GetData(Observations: TCustomComparisonCollection);
var
  ItemIndex: Integer;
  Obs: TCustomTimeObservationItem;
  Comparisons: TObsComparisons;
  ObsComp: TObsCompareItem;
begin
  FGettingData := True;
  try
    if Observations = nil then
    begin
      frameObservations.seNumber.AsInteger := 0;
      frameObsComparisons.seNumber.AsInteger := 0;
      Exit;
    end;

    frameObservations.seNumber.AsInteger := Observations.Count;
    GetDirectObs(Observations);

    Comparisons := Observations.Comparisons;
    frameObsComparisons.seNumber.AsInteger := Comparisons.Count;
    for ItemIndex := 0 to Comparisons.Count-1 do
    begin
      ObsComp := Comparisons[ItemIndex];
      frameObsComparisons.Grid.Cells[Ord(poccName), ItemIndex+1] := ObsComp.Name;

      Obs := Observations[ObsComp.Index1];
      frameObsComparisons.Grid.Cells[Ord(poccObs1), ItemIndex+1] := Obs.Name;

      Obs := Observations[ObsComp.Index2];
      frameObsComparisons.Grid.Cells[Ord(poccObs2), ItemIndex+1] := Obs.Name;

      frameObsComparisons.Grid.RealValue[Ord(poccValue), ItemIndex+1] := ObsComp.ObservedValue;
      frameObsComparisons.Grid.RealValue[Ord(poccWeight), ItemIndex+1] := ObsComp.Weight;
      frameObsComparisons.Grid.Cells[Ord(poccGroup), ItemIndex+1] := ObsComp.ObservationGroup;
      frameObsComparisons.Grid.Cells[Ord(poccComment), ItemIndex+1] := ObsComp.Comment;
    end;
  finally
    FGettingData := False;
  end;
end;

procedure TframePestObs.InitializeControls;
var
  ColIndex: Integer;
  GridSel: TGridRect;
begin
  ClearGrid(frameObservations.Grid);
  for ColIndex := 0 to frameObservations.Grid.ColCount - 1 do
  begin
    frameObservations.Grid.Columns[ColIndex].AutoAdjustColWidths := True;
  end;
  SetObsColumnCaptions;
  GetObservationGroups;

  GridSel := frameObservations.Grid.Selection;
  GridSel.Left := 1;
  GridSel.Right := 1;
  frameObservations.Grid.Selection := GridSel;

  ClearGrid(frameObsComparisons.Grid);
  frameObsComparisons.Grid.BeginUpdate;
  for ColIndex := 0 to frameObsComparisons.Grid.ColCount - 1 do
  begin
    frameObsComparisons.Grid.Columns[ColIndex].AutoAdjustColWidths := True;
  end;
  try
    frameObsComparisons.Grid.Cells[Ord(poccName), 0] := StrObservationName;
    frameObsComparisons.Grid.Cells[Ord(poccObs1), 0] := StrFirstObservation;
    frameObsComparisons.Grid.Cells[Ord(poccObs2), 0] := StrSecondObservation;
    frameObsComparisons.Grid.Cells[Ord(poccValue), 0] := StrObservationValueDif;
    frameObsComparisons.Grid.Cells[Ord(poccWeight), 0] := StrObservationWeight;
    frameObsComparisons.Grid.Cells[Ord(poccGroup), 0] := StrObservationGroup;
    frameObsComparisons.Grid.Cells[Ord(poccComment), 0] := StrComment;
  finally
    frameObsComparisons.Grid.EndUpdate;
  end;
  for ColIndex := 0 to frameObsComparisons.Grid.ColCount - 1 do
  begin
    frameObsComparisons.Grid.Columns[ColIndex].AutoAdjustColWidths := False;
  end;
end;

procedure TframePestObs.SetData(Observations: TCustomComparisonCollection;
  const LocationName: string);
var
  RowOK: Boolean;
  ColIndex: Integer;
  Obs: TCustomTimeObservationItem;
  ObNames: TStringList;
  ObIndex: Integer;
  CompCount: Integer;
  Comparisons: TObsComparisons;
  ItemIndex: Integer;
  Index1: Integer;
  Index2: Integer;
  ObsComp: TObsCompareItem;
begin
  SetDirectObs(Observations, LocationName);

  ObNames := TStringList.Create;
  try
    for ObIndex := 0 to Observations.Count - 1 do
    begin
      Obs := Observations[ObIndex];
      ObNames.Add(Obs.Name);
    end;

    CompCount := 0;
    Comparisons := Observations.Comparisons;
    Comparisons.Count := frameObsComparisons.seNumber.AsInteger;
    for ItemIndex := 0 to frameObsComparisons.seNumber.AsInteger-1 do
    begin
      RowOk := True;
      for ColIndex := 0 to Ord(poccWeight) do
      begin
        if frameObsComparisons.Grid.Cells[ColIndex, ItemIndex+1] = '' then
        begin
          RowOk := False;
          Break;
        end;
      end;
      Index1 := -1;
      Index2 := -1;
      if RowOk then
      begin
        Index1 := ObNames.IndexOf(frameObsComparisons.Grid.Cells[
            Ord(poccObs1), ItemIndex+1]);
        Index2 := ObNames.IndexOf(frameObsComparisons.Grid.Cells[
            Ord(poccObs2), ItemIndex+1]);
        RowOk := (Index1 >= 0) and (Index2 >= 0);
      end;
      if RowOk then
      begin
        ObsComp := Comparisons[CompCount];
        Inc(CompCount);
        ObsComp.Name := frameObsComparisons.Grid.Cells[Ord(poccName), ItemIndex+1];
        ObsComp.Index1 := Index1;
        ObsComp.Index2 := Index2;
        ObsComp.ObservedValue := frameObsComparisons.Grid.RealValue[Ord(poccValue), ItemIndex+1];
        ObsComp.Weight := frameObsComparisons.Grid.RealValue[Ord(poccWeight), ItemIndex+1];
        ObsComp.ObservationGroup := frameObsComparisons.Grid.Cells[Ord(poccGroup), ItemIndex+1];
        ObsComp.Comment := frameObsComparisons.Grid.Cells[Ord(poccComment), ItemIndex+1];
      end;
    end;
  finally
    ObNames.Free
  end;
  Comparisons.Count := CompCount;
end;

procedure TframePestObs.SetDirectObs(Observations: TCustomComparisonCollection;
  const LocationName: string);
var
  ObsCount: Integer;
  RowIndex: Integer;
  RowOK: Boolean;
  ColIndex: Integer;
  Obs: TCustomTimeObservationItem;
  OtherObs: TCustomTimeObservationItem;
  MyGuid: TGUID;
  ObsNames: TStringList;
begin
  ObsCount := 0;
  ObsNames := TStringList.Create;
  try
    ObsNames.Assign(frameObservations.Grid.Cols[Ord(pocName)]);
    ObsNames[0] := '';
    for RowIndex := 1 to frameObservations.seNumber.AsInteger do
    begin
      RowOK := True;
      for ColIndex := 0 to Ord(pocWeight) do
      begin
        if ColIndex in [Ord(pocGroup), Ord(pocName)] then
        begin
          Continue;
        end;
        if frameObservations.Grid.Cells[ColIndex,RowIndex] = '' then
        begin
          RowOK := False;
          Break;
        end;
      end;
      if ObsNames.IndexOf(frameObservations.Grid.Cells[Ord(pocName),RowIndex])
        < RowIndex then
      begin
        frameObservations.Grid.Cells[Ord(pocName),RowIndex] := LocationName
          + '_' + IntToStr(RowIndex);
      end;
      if RowOK then
      begin
        if ObsCount < Observations.Count then
        begin
          Obs := Observations[ObsCount]
        end
        else
        begin
          Obs := Observations.Add;
        end;
        Inc(ObsCount);
        Obs.Name := frameObservations.Grid.Cells[Ord(pocName), RowIndex];
        if frameObservations.Grid.Objects[Ord(pocName), RowIndex] <> nil then
        begin
          OtherObs := frameObservations.Grid.Objects[Ord(pocName), RowIndex] as TCustomTimeObservationItem;
          Obs.GUID  := OtherObs.GUID;
        end
        else
        begin
          if CreateGUID(MyGuid) = 0 then
          begin
            Obs.GUID := GUIDToString(MyGuid);
          end;
        end;
        Obs.ObsTypeString := frameObservations.Grid.Cells[Ord(pocType), RowIndex];
        Obs.ObservationGroup := frameObservations.Grid.Cells[Ord(pocGroup), RowIndex];
        Obs.Time := frameObservations.Grid.RealValue[Ord(pocTime), RowIndex];
        Obs.ObservedValue := frameObservations.Grid.RealValue[Ord(pocValue), RowIndex];
        Obs.Weight := frameObservations.Grid.RealValue[Ord(pocWeight), RowIndex];
        Obs.Comment := frameObservations.Grid.Cells[Ord(pocComment), RowIndex];
      end;
    end;
    Observations.Count := ObsCount;
  finally
    ObsNames.Free;
  end;
end;

procedure TframePestObs.GetDirectObs(Observations: TCustomComparisonCollection);
var
  ItemIndex: Integer;
  Obs: TCustomTimeObservationItem;
begin
  for ItemIndex := 0 to Observations.Count - 1 do
  begin
    Obs := Observations[ItemIndex];
    frameObservations.Grid.Cells[Ord(pocName), ItemIndex + 1] := Obs.Name;
    frameObservations.Grid.Objects[Ord(pocName), ItemIndex + 1] := Obs;
    frameObservations.Grid.Cells[Ord(pocType), ItemIndex + 1] := Obs.ObsTypeString;
    frameObservations.Grid.Cells[Ord(pocGroup), ItemIndex + 1] := Obs.ObservationGroup;
    frameObservations.Grid.RealValue[Ord(pocTime), ItemIndex + 1] := Obs.Time;
    frameObservations.Grid.RealValue[Ord(pocValue), ItemIndex + 1] := Obs.ObservedValue;
    frameObservations.Grid.RealValue[Ord(pocWeight), ItemIndex + 1] := Obs.Weight;
    frameObservations.Grid.Cells[Ord(pocComment), ItemIndex + 1] := Obs.Comment;
  end;
end;

procedure TframePestObs.GetObservationGroups;
var
  ObsGroups: TPestObservationGroups;
  PickList: TStrings;
  GroupIndex: Integer;
begin
  ObsGroups := frmGoPhast.PhastModel. PestProperties.ObservationGroups;
  PickList := frameObservations.Grid.Columns[Ord(pocGroup)].PickList;
  PickList.Clear;
  PickList.Capacity := ObsGroups.Count;
  for GroupIndex := 0 to ObsGroups.Count - 1 do
  begin
    PickList.Add(ObsGroups[GroupIndex].ObsGroupName);
  end;

  PickList := frameObsComparisons.Grid.Columns[Ord(poccGroup)].PickList;
  PickList.Clear;
  PickList.Capacity := ObsGroups.Count;
  for GroupIndex := 0 to ObsGroups.Count - 1 do
  begin
    PickList.Add(ObsGroups[GroupIndex].ObsGroupName);
  end;
end;

procedure TframePestObs.SetObsColumnCaptions;
begin
  frameObservations.Grid.BeginUpdate;
  try
    frameObservations.Grid.Cells[Ord(pocName), 0] := StrObservationName;
    frameObservations.Grid.Cells[Ord(pocType), 0] := StrObservationType;
    frameObservations.Grid.Cells[Ord(pocGroup), 0] := StrObservationGroup;
    frameObservations.Grid.Cells[Ord(pocTime), 0] := StrObservationTime;
    frameObservations.Grid.Cells[Ord(pocValue), 0] := StrObservationValue;
    frameObservations.Grid.Cells[Ord(pocWeight), 0] := StrObservationWeight;
    frameObservations.Grid.Cells[Ord(pocComment), 0] := StrComment;
  finally
    frameObservations.Grid.EndUpdate;
  end;
end;

procedure TframePestObs.SpecifyObservationTypes(ObsTypes: TStrings);
begin
  frameObservations.Grid.Columns[Ord(pocType)].PickList := ObsTypes;
end;

procedure TframePestObs.UpdatedSelectedCell;
var
  CanSelect: Boolean;
  Selection: TGridRect;
begin
  CanSelect := True;
  Selection := frameObservations.Grid.Selection;
  frameObservationsGridSelectCell(frameObservations.Grid, Selection.Left,
    Selection.Top, CanSelect);
end;

end.
