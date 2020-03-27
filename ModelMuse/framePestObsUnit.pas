unit framePestObsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameGridUnit,
  Vcl.StdCtrls, Vcl.ExtCtrls, System.Generics.Collections, Vcl.Grids,
  PestObsUnit;

type
  TMnwiObsColumns = (mocName, mocType, mocTime, mocValue, mocWeight, mocComment);
  TMnwiObsCompColumns = (moccName, moccObs1, moccObs2, moccValue, moccWeight, moccComment);

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
  private
    FObservationsName: string;
    FMatchedCells1: TList<Integer>;
    FMatchedCells2: TList<Integer>;
    procedure UpdatedSelectedCell;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitializeControls;
    procedure GetData(Observations: TCustomComparisonCollection);
    procedure SetData(Observations: TCustomComparisonCollection);
    { Public declarations }
  end;

implementation

uses
  frmCustomGoPhastUnit;

{$R *.dfm}

resourcestring
  StrObservationName = 'Observation Name';
  StrObservationType = 'Observation Type';
  StrObservationTime = 'Observation Time';
  StrObservationValue = 'Observation Value';
  StrObservationWeight = 'Observation Weight';
  StrComment = 'Comment';
  StrFirstObservation = 'First Observation';
  StrSecondObservation = 'Second Observation';


{ TframePestObs }

constructor TframePestObs.Create(AOwner: TComponent);
begin
  inherited;
  FMatchedCells1 := TList<Integer>.Create;
  FMatchedCells2 := TList<Integer>.Create;
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

    frameObsComparisons.Grid.Columns[Ord(moccObs1)].Picklist := ObsNames;
    frameObsComparisons.Grid.Columns[Ord(moccObs2)].Picklist := ObsNames;
  finally
    ObsNames.Free;
  end;
end;

procedure TframePestObs.frameObservationsGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  RowIndex: Integer;
begin
  inherited;
  if not frameObservations.Grid.Drawing
    and (Ord(mocName) = ACol) and (ARow >= 1) then
  begin
    FObservationsName := frameObservations.Grid.Cells[ACol, ARow];
    FMatchedCells1.Clear;
    FMatchedCells2.Clear;
    for RowIndex := 1 to frameObsComparisons.Grid.RowCount - 1 do
    begin
      if frameObsComparisons.Grid.Cells[Ord(moccObs1), RowIndex] = FObservationsName then
      begin
        FMatchedCells1.Add(RowIndex);
      end;
      if frameObsComparisons.Grid.Cells[Ord(moccObs2), RowIndex] = FObservationsName then
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
  if (Ord(mocName) = ACol) and (ARow >= 1) then
  begin
    if FObservationsName <> Value then
    begin
      for Index := 0 to FMatchedCells1.Count - 1 do
      begin
        RowIndex := FMatchedCells1[Index];
        frameObsComparisons.Grid.Cells[Ord(moccObs1), RowIndex] := Value;
      end;
      for Index := 0 to FMatchedCells2.Count - 1 do
      begin
        RowIndex := FMatchedCells2[Index];
        frameObsComparisons.Grid.Cells[Ord(moccObs2), RowIndex] := Value;
      end;
    end;
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

procedure TframePestObs.GetData(Observations: TCustomComparisonCollection);
var
  ItemIndex: Integer;
  Mnw2Ob: TCustomTimeObservationItem;
  Comparisons: TObsComparisons;
  Mnw2ObComp: TObsCompareItem;
begin
  frameObservations.seNumber.AsInteger := Observations.Count;
  for ItemIndex := 0 to Observations.Count-1 do
  begin
    Mnw2Ob := Observations[ItemIndex];
    frameObservations.Grid.Cells[Ord(mocName), ItemIndex+1] := Mnw2Ob.Name;
    frameObservations.Grid.Objects[Ord(mocName), ItemIndex+1] := Mnw2Ob;
    frameObservations.Grid.ItemIndex[Ord(mocType), ItemIndex+1] := Mnw2Ob.ObsTypeIndex;
    frameObservations.Grid.RealValue[Ord(mocTime), ItemIndex+1] := Mnw2Ob.Time;
    frameObservations.Grid.RealValue[Ord(mocValue), ItemIndex+1] := Mnw2Ob.ObservedValue;
    frameObservations.Grid.RealValue[Ord(mocWeight), ItemIndex+1] := Mnw2Ob.Weight;
    frameObservations.Grid.Cells[Ord(mocComment), ItemIndex+1] := Mnw2Ob.Comment;
  end;

  Comparisons := Observations.Comparisons;
  frameObsComparisons.seNumber.AsInteger := Comparisons.Count;
  for ItemIndex := 0 to Comparisons.Count-1 do
  begin
    Mnw2ObComp := Comparisons[ItemIndex];
    frameObsComparisons.Grid.Cells[Ord(moccName), ItemIndex+1] := Mnw2ObComp.Name;

    Mnw2Ob := Observations[Mnw2ObComp.Index1];
    frameObsComparisons.Grid.Cells[Ord(moccObs1), ItemIndex+1] := Mnw2Ob.Name;

    Mnw2Ob := Observations[Mnw2ObComp.Index2];
    frameObsComparisons.Grid.Cells[Ord(moccObs2), ItemIndex+1] := Mnw2Ob.Name;

    frameObsComparisons.Grid.RealValue[Ord(moccValue), ItemIndex+1] := Mnw2ObComp.ObservedValue;
    frameObsComparisons.Grid.RealValue[Ord(moccWeight), ItemIndex+1] := Mnw2ObComp.Weight;
    frameObsComparisons.Grid.Cells[Ord(moccComment), ItemIndex+1] := Mnw2ObComp.Comment;
  end;
end;

procedure TframePestObs.InitializeControls;
var
  ColIndex: Integer;
  GridSel: TGridRect;
begin
  ClearGrid(frameObservations.Grid);
  frameObservations.Grid.BeginUpdate;
  try
    frameObservations.Grid.Cells[Ord(mocName), 0] := StrObservationName;
    frameObservations.Grid.Cells[Ord(mocType), 0] := StrObservationType;
    frameObservations.Grid.Cells[Ord(mocTime), 0] := StrObservationTime;
    frameObservations.Grid.Cells[Ord(mocValue), 0] := StrObservationValue;
    frameObservations.Grid.Cells[Ord(mocWeight), 0] := StrObservationWeight;
    frameObservations.Grid.Cells[Ord(mocComment), 0] := StrComment;
  finally
    frameObservations.Grid.EndUpdate;
  end;
  for ColIndex := 0 to frameObservations.Grid.ColCount - 1 do
  begin
    frameObservations.Grid.Columns[ColIndex].AutoAdjustColWidths := False;
  end;
  GridSel := frameObservations.Grid.Selection;
  GridSel.Left := 1;
  GridSel.Right := 1;
  frameObservations.Grid.Selection := GridSel;

  ClearGrid(frameObsComparisons.Grid);
  frameObsComparisons.Grid.BeginUpdate;
  try
    frameObsComparisons.Grid.Cells[Ord(moccName), 0] := StrObservationName;
    frameObsComparisons.Grid.Cells[Ord(moccObs1), 0] := StrFirstObservation;
    frameObsComparisons.Grid.Cells[Ord(moccObs2), 0] := StrSecondObservation;
    frameObsComparisons.Grid.Cells[Ord(moccValue), 0] := StrObservationValue;
    frameObsComparisons.Grid.Cells[Ord(moccWeight), 0] := StrObservationWeight;
    frameObsComparisons.Grid.Cells[Ord(moccComment), 0] := StrComment;
  finally
    frameObsComparisons.Grid.EndUpdate;
  end;
  for ColIndex := 0 to frameObsComparisons.Grid.ColCount - 1 do
  begin
    frameObsComparisons.Grid.Columns[ColIndex].AutoAdjustColWidths := False;
  end;
end;

procedure TframePestObs.SetData(Observations: TCustomComparisonCollection);
var
  RowIndex: Integer;
  RowOK: Boolean;
  ColIndex: Integer;
  ObsCount: Integer;
  Mnw2Ob: TCustomTimeObservationItem;
  OtherMnw2Ob: TCustomTimeObservationItem;
  MyGuid: TGUID;
  ObNames: TStringList;
  ObIndex: Integer;
  CompCount: Integer;
  Comparisons: TObsComparisons;
  ItemIndex: Integer;
  Index1: Integer;
  Index2: Integer;
  Mnw2ObComp: TObsCompareItem;
begin
  for RowIndex := 1 to frameObservations.seNumber.AsInteger do
  begin
    RowOK := True;
    for ColIndex := 0 to Ord(mocWeight) do
    begin
      if frameObservations.Grid.Cells[ColIndex,RowIndex] = '' then
      begin
        RowOK := False;
        Break;
      end;
    end;
    if RowOK then
    begin
      if ObsCount < Observations.Count then
      begin
        Mnw2Ob := Observations[ObsCount]
      end
      else
      begin
        Mnw2Ob := Observations.Add;
      end;
      Inc(ObsCount);
      Mnw2Ob.Name := frameObservations.Grid.Cells[Ord(mocName), RowIndex];
      if frameObservations.Grid.Objects[Ord(mocName), RowIndex] <> nil then
      begin
        OtherMnw2Ob := frameObservations.Grid.Objects[Ord(mocName), RowIndex] as TCustomTimeObservationItem;
        Mnw2Ob.GUID  := OtherMnw2Ob.GUID;
      end
      else
      begin
        if CreateGUID(MyGuid) = 0 then
        begin
          Mnw2Ob.GUID := GUIDToString(MyGuid);
        end;
      end;
      Mnw2Ob.ObsTypeIndex := frameObservations.Grid.ItemIndex[Ord(mocType), RowIndex];
      Mnw2Ob.Time := frameObservations.Grid.RealValue[Ord(mocTime), RowIndex];
      Mnw2Ob.ObservedValue := frameObservations.Grid.RealValue[Ord(mocValue), RowIndex];
      Mnw2Ob.Weight := frameObservations.Grid.RealValue[Ord(mocWeight), RowIndex];
      Mnw2Ob.Comment := frameObservations.Grid.Cells[Ord(mocComment), RowIndex];
    end;
  end;
  Observations.Count := ObsCount;

  ObNames := TStringList.Create;
  try
    for ObIndex := 0 to Observations.Count - 1 do
    begin
      Mnw2Ob := Observations[ObIndex];
      ObNames.Add(Mnw2Ob.Name);
    end;

    CompCount := 0;
    Comparisons := Observations.Comparisons;
    Comparisons.Count := frameObsComparisons.seNumber.AsInteger;
    for ItemIndex := 0 to frameObsComparisons.seNumber.AsInteger-1 do
    begin
      RowOk := True;
      for ColIndex := 0 to Ord(moccWeight) do
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
            Ord(moccObs1), ItemIndex+1]);
        Index2 := ObNames.IndexOf(frameObsComparisons.Grid.Cells[
            Ord(moccObs2), ItemIndex+1]);
        RowOk := (Index1 >= 0) and (Index2 >= 0);
      end;
      if RowOk then
      begin
        Mnw2ObComp := Comparisons[CompCount];
        Inc(CompCount);
        Mnw2ObComp.Name := frameObsComparisons.Grid.Cells[Ord(moccName), ItemIndex+1];
        Mnw2ObComp.Index1 := Index1;
        Mnw2ObComp.Index2 := Index2;
        Mnw2ObComp.ObservedValue := frameObsComparisons.Grid.RealValue[Ord(moccValue), ItemIndex+1];
        Mnw2ObComp.Weight := frameObsComparisons.Grid.RealValue[Ord(moccWeight), ItemIndex+1];
        Mnw2ObComp.Comment := frameObsComparisons.Grid.Cells[Ord(moccComment), ItemIndex+1];
      end;
    end;
  finally
    ObNames.Free
  end;
  Comparisons.Count := CompCount;
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
