unit frameScreenObjectLktUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectNoParamUnit, Vcl.Grids,
  RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, Vcl.ComCtrls, UndoItemsScreenObjects, JvToolEdit;

type
  TGridColumns = (gcStartTime, gcEndTime, gcComponentStart);

  TframeScreenObjectLkt = class(TframeScreenObjectNoParam)
    pcLkt: TPageControl;
    tabInitialConc: TTabSheet;
    tabPrecipConc: TTabSheet;
    tabRunoffConc: TTabSheet;
    pnlButtonsRunoff: TPanel;
    lbl1: TLabel;
    seRunoffTimes: TJvSpinEdit;
    btnDeleteRunoff: TBitBtn;
    btnInsertRunoff: TBitBtn;
    pnlGridRunoff: TPanel;
    pnl3: TPanel;
    lblRunoffFormula: TLabel;
    rdeRunoffFormula: TRbwDataEntry;
    rdgRunoffConc: TRbwDataGrid4;
    lblInitialConcentration: TLabel;
    rdgInitialConcentration: TRbwDataGrid4;
    procedure seRunoffTimesChange(Sender: TObject);
    procedure btnInsertRunoffClick(Sender: TObject);
    procedure btnDeleteRunoffClick(Sender: TObject);
    procedure rdgRunoffConcBeforeDrawCell(Sender: TObject; ACol, ARow: Integer);
    procedure rdgRunoffConcColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure rdgRunoffConcHorizontalScroll(Sender: TObject);
    procedure rdgRunoffConcMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdgRunoffConcSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgRunoffConcSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
  protected
    procedure Loaded; override;
  private
    FDeleting: Boolean;
    FDeletedRunoffCells: array of array of boolean;
    FSelectedRunoffText: string;
    procedure LayoutRunoffMultiRowEditControls;
    function ShouldEnableRunoffMultisetControls: Boolean;
    function GetDeletedRunoffCells(ACol, ARow: integer): boolean;
    procedure SetDeletedRunoffCells(ACol, ARow: integer; const Value: boolean);
    { Private declarations }
  public
    procedure ClearDeletedRunoffCells;
    property DeletedRunoffCells[ACol, ARow: integer]: boolean
      read GetDeletedRunoffCells write SetDeletedRunoffCells;
    procedure GetData(const List: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    { Public declarations }
  end;

var
  frameScreenObjectLkt: TframeScreenObjectLkt;

implementation

uses
  ScreenObjectUnit, Mt3dLktUnit, PhastModelUnit, frmGoPhastUnit, GoPhastTypes,
  frmCustomGoPhastUnit, System.Math;

resourcestring
  StrChemicalSpecies = 'Chemical species';
  StrInitialConcentratio = 'Initial Concentration';



{$R *.dfm}

{ TframeScreenObjectLkt }

procedure TframeScreenObjectLkt.LayoutRunoffMultiRowEditControls;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  LayoutControls(rdgRunoffConc, rdeRunoffFormula, lblRunoffFormula,
    Max(FLastTimeColumn+1,rdgRunoffConc.LeftCol));
end;

procedure TframeScreenObjectLkt.Loaded;
begin
  inherited;
  MoveGridToTabSheet(tabPrecipConc);
end;

procedure TframeScreenObjectLkt.rdgRunoffConcBeforeDrawCell(Sender: TObject;
  ACol, ARow: Integer);
var
  EndTime: double;
  NextStartTime: double;
begin
  if (ACol = 1) and (ARow >= rdgRunoffConc.FixedRows)
    and (ARow < rdgRunoffConc.RowCount -1) then
  begin
    if TryStrToFloat(rdgRunoffConc.Cells[ACol, ARow], EndTime)
      and TryStrToFloat(rdgRunoffConc.Cells[0, ARow+1], NextStartTime) then
    begin
      if NextStartTime < EndTime then
      begin
        rdgRunoffConc.Canvas.Brush.Color := clRed;
      end;
    end;
  end;
end;

procedure TframeScreenObjectLkt.rdgRunoffConcColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  LayoutRunoffMultiRowEditControls;
end;

procedure TframeScreenObjectLkt.rdgRunoffConcHorizontalScroll(Sender: TObject);
begin
  inherited;
  LayoutRunoffMultiRowEditControls;
end;

procedure TframeScreenObjectLkt.rdgRunoffConcMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  rdeRunoffFormula.Enabled := ShouldEnableRunoffMultisetControls;
end;

procedure TframeScreenObjectLkt.rdgRunoffConcSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if (ARow = rdgRunoffConc.FixedRows)
    and (seRunoffTimes.AsInteger = 0) then
  begin
    FSelectedRunoffText := rdgRunoffConc.Cells[ACol, ARow];
    CanSelect := False;
    Exit;
  end;
end;

procedure TframeScreenObjectLkt.rdgRunoffConcSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
  if FDeleting  then
  begin
    Exit;
  end;
  if seRunoffTimes.AsInteger < rdgRunoffConc.RowCount -1  then
  begin
    seRunoffTimes.AsInteger := rdgRunoffConc.RowCount -1;
    seRunoffTimes.OnChange(seRunoffTimes);
  end;
  if FSelectedRunoffText <> Value then
  begin
    DeletedRunoffCells[ACol, ARow] := Value = '';
  end;

  UpdateNextTimeCell(rdgRunoffConc, ACol, ARow);
end;

procedure TframeScreenObjectLkt.seRunoffTimesChange(Sender: TObject);
begin
  inherited;
  FDeleting := True;
  try
    if seRunoffTimes.AsInteger = 0 then
    begin
      rdgRunoffConc.RowCount := 2;
    end
    else
    begin
      rdgRunoffConc.RowCount := seRunoffTimes.AsInteger + 1;
    end;
    btnDeleteRunoff.Enabled := seRunoffTimes.AsInteger >= 1;
    rdgRunoffConc.Invalidate;
  finally
    FDeleting := False;
  end;
end;

procedure TframeScreenObjectLkt.btnDeleteRunoffClick(Sender: TObject);
begin
  if (rdgRunoffConc.RowCount > 2)
    and (rdgRunoffConc.Row> 0) then
  begin
    rdgRunoffConc.DeleteRow(rdgRunoffConc.Row);
  end;
  seRunoffTimes.AsInteger := seRunoffTimes.AsInteger -1;
end;

procedure TframeScreenObjectLkt.btnInsertRunoffClick(Sender: TObject);
begin
  if (rdgRunoffConc.SelectedRow <= 0)
    or (rdgRunoffConc.SelectedRow >= rdgRunoffConc.RowCount) then
  begin
    Beep;
    MessageDlg(StrYouNeedToSelectA, mtInformation, [mbOK], 0);
    Exit;
  end;
  if (seRunoffTimes.AsInteger > 0) then
  begin
    rdgRunoffConc.InsertRow(rdgRunoffConc.SelectedRow);
  end;
  seRunoffTimes.AsInteger := seRunoffTimes.AsInteger +1;
end;

procedure TframeScreenObjectLkt.ClearDeletedRunoffCells;
begin
  SetLength(FDeletedRunoffCells, 0, 0);
end;

procedure TframeScreenObjectLkt.GetData(
  const List: TScreenObjectEditCollection);
var
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  Mt3dLktConcBoundary: TMt3dLktConcBoundary;
  FoundFirst: Boolean;
  PrecipConc: TMt3dLktConcCollection;
  NCOMP: Integer;
  Model: TPhastModel;
  CIndex: Integer;
  SpeciesName: string;
  ComponentIndex: Integer;
  TimeIndex: Integer;
  ConcItem: TMt3dLktConcItem;
  RunoffConcentration: TMt3dLktConcCollection;
  FirstPrecipConc: TMt3dLktConcCollection;
  FirstRunoffConcentration: TMt3dLktConcCollection;
  AColumn: TRbwColumn4;
  InitCIndex: Integer;
begin
  pcLkt.ActivePageIndex := 0;
  FoundFirst := False;
  Model := frmGoPhast.PhastModel;
  NCOMP := Model.MobileComponents.Count + Model.ImmobileComponents.Count;
  ClearGrid(rdgModflowBoundary);
  ClearGrid(rdgRunoffConc);
  ClearGrid(rdgInitialConcentration);
  rdgModflowBoundary.ColCount := NCOMP+2;
  rdgRunoffConc.ColCount := NCOMP+2;
  rdgInitialConcentration.RowCount := NCOMP + 1;
  rdgInitialConcentration.FixedCols := 1;
  rdgInitialConcentration.Cells[0,0] := StrChemicalSpecies;
  rdgInitialConcentration.Cells[1,0] := StrInitialConcentratio;

  Model.ModflowStressPeriods.FillPickListWithStartTimes(rdgRunoffConc, Ord(gcStartTime));
  Model.ModflowStressPeriods.FillPickListWithEndTimes(rdgRunoffConc, Ord(gcEndTime));
//  Model.ModflowStressPeriods.FillPickListWithStartTimes(rdeRunoffFormula, Ord(gcStartTime));
//  Model.ModflowStressPeriods.FillPickListWithEndTimes(rdeRunoffFormula, Ord(gcEndTime));

  rdgModflowBoundary.Cells[Ord(gcStartTime), 0] := StrStartingTime;
  rdgModflowBoundary.Cells[Ord(gcEndTime), 0] := StrEndingTime;
  rdgRunoffConc.Cells[Ord(gcStartTime), 0] := StrStartingTime;
  rdgRunoffConc.Cells[Ord(gcEndTime), 0] := StrEndingTime;
  CIndex := Ord(gcComponentStart);
  InitCIndex := 1;
  for ComponentIndex := 0 to Model.MobileComponents.Count - 1 do
  begin
    SpeciesName := Model.MobileComponents[ComponentIndex].Name;
    rdgModflowBoundary.Cells[CIndex, 0] := SpeciesName;
    rdgRunoffConc.Cells[CIndex, 0] := SpeciesName;
    rdgInitialConcentration.Cells[0, InitCIndex] := SpeciesName;

    AColumn := rdgModflowBoundary.Columns[CIndex];
    AColumn.ButtonUsed := true;
    AColumn.ButtonWidth := 35;
    AColumn.ButtonCaption := 'F()';

    AColumn := rdgRunoffConc.Columns[CIndex];
    AColumn.ButtonUsed := true;
    AColumn.ButtonWidth := 35;
    AColumn.ButtonCaption := 'F()';

    Inc(CIndex);
    Inc(InitCIndex);
  end;
  for ComponentIndex := 0 to Model.ImmobileComponents.Count - 1 do
  begin
    SpeciesName := Model.ImmobileComponents[ComponentIndex].Name;
    rdgModflowBoundary.Cells[CIndex, 0] := SpeciesName;
    rdgRunoffConc.Cells[CIndex, 0] := SpeciesName;
    rdgInitialConcentration.Cells[0, InitCIndex] := SpeciesName;

    AColumn := rdgModflowBoundary.Columns[CIndex];
    AColumn.ButtonUsed := true;
    AColumn.ButtonWidth := 35;
    AColumn.ButtonCaption := 'F()';

    AColumn := rdgRunoffConc.Columns[CIndex];
    AColumn.ButtonUsed := true;
    AColumn.ButtonWidth := 35;
    AColumn.ButtonCaption := 'F()';

    Inc(CIndex);
    Inc(InitCIndex);
  end;

  FirstPrecipConc := nil;
  FirstRunoffConcentration := nil;
  for ScreenObjectIndex := 0 to List.Count - 1 do
  begin
    AScreenObject := List[ScreenObjectIndex].ScreenObject;
    Mt3dLktConcBoundary := AScreenObject.Mt3dLktConcBoundary;
    if (Mt3dLktConcBoundary <> nil) and Mt3dLktConcBoundary.Used then
    begin
      if not FoundFirst then
      begin
        FoundFirst := True;

        for ComponentIndex := 0 to NCOMP - 1 do
        begin
          rdgInitialConcentration.Cells[1, ComponentIndex+1] :=
            Mt3dLktConcBoundary.InitialConcentrations[ComponentIndex].InitConc;
        end;

        PrecipConc :=  Mt3dLktConcBoundary.Values as TMt3dLktConcCollection;
        FirstPrecipConc := PrecipConc;
        seNumberOfTimes.AsInteger := PrecipConc.Count;
        for TimeIndex := 0 to PrecipConc.Count - 1 do
        begin
          ConcItem := PrecipConc[TimeIndex] as TMt3dLktConcItem;
          rdgModflowBoundary.RealValue[Ord(gcStartTime), TimeIndex+1] := ConcItem.StartTime;
          rdgModflowBoundary.RealValue[Ord(gcEndTime), TimeIndex+1] := ConcItem.EndTime;
          for ComponentIndex := 0 to NCOMP - 1 do
          begin
            rdgModflowBoundary.Cells[ComponentIndex + Ord(gcComponentStart),
              TimeIndex+1] := ConcItem.Mt3dmsConcRate[ComponentIndex];
          end;
        end;

        RunoffConcentration := Mt3dLktConcBoundary.RunoffConcentration;
        FirstRunoffConcentration := RunoffConcentration;
        seRunoffTimes.AsInteger := RunoffConcentration.Count;
        for TimeIndex := 0 to RunoffConcentration.Count - 1 do
        begin
          ConcItem := RunoffConcentration[TimeIndex] as TMt3dLktConcItem;
          rdgRunoffConc.RealValue[Ord(gcStartTime), TimeIndex+1] := ConcItem.StartTime;
          rdgRunoffConc.RealValue[Ord(gcEndTime), TimeIndex+1] := ConcItem.EndTime;
          for ComponentIndex := 0 to NCOMP - 1 do
          begin
            rdgRunoffConc.Cells[ComponentIndex + Ord(gcComponentStart),
              TimeIndex+1] := ConcItem.Mt3dmsConcRate[ComponentIndex];
          end;
        end;
      end
      else
      begin
        for ComponentIndex := 0 to NCOMP - 1 do
        begin
          if (rdgModflowBoundary.Cells[0, ComponentIndex+1] <>
            Mt3dLktConcBoundary.InitialConcentrations[ComponentIndex].InitConc) then
          begin
            rdgModflowBoundary.Cells[0, ComponentIndex+1] := '';
          end;
        end;

        PrecipConc :=  Mt3dLktConcBoundary.Values as TMt3dLktConcCollection;
        if not PrecipConc.IsSame(FirstPrecipConc) then
        begin
          ClearGrid(rdgModflowBoundary);
        end;
        RunoffConcentration := Mt3dLktConcBoundary.RunoffConcentration;
        if not RunoffConcentration.IsSame(FirstRunoffConcentration) then
        begin
          ClearGrid(rdgRunoffConc);
        end;
      end;
    end;
  end;
end;

function TframeScreenObjectLkt.GetDeletedRunoffCells(ACol,
  ARow: integer): boolean;
begin
  if (ACol < 0) or (ARow < 0) then
  begin
    result := False;
    Exit;
  end;
  if (Length(FDeletedRunoffCells) = 0) or (Length(FDeletedRunoffCells[0]) = 0) then
  begin
    result := False;
    Exit;
  end;
  if (ACol < Length(FDeletedRunoffCells))
    and (ARow < Length(FDeletedRunoffCells[0])) then
  begin
    result := FDeletedRunoffCells[ACol,ARow];
  end
  else
  begin
    result := False;
  end;
end;

procedure TframeScreenObjectLkt.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
var
  Index: Integer;
  AScreenObject: TScreenObject;
  Mt3dLktConcBoundary: TMt3dLktConcBoundary;
  PrecipConc: TMt3dLktConcCollection;
  RunoffConcentration: TMt3dLktConcCollection;
  TimeIndex: Integer;
  ConcItem: TMt3dLktConcItem;
  ComponentIndex: Integer;
  Model: TPhastModel;
  NCOMP: Integer;
  BoundaryUsed: Boolean;
  function LakeUsed(AScreenObject: TScreenObject): Boolean;
  begin
    result := ((AScreenObject.ModflowLakBoundary <> nil)
            and AScreenObject.ModflowLakBoundary.Used)
          or ((AScreenObject.ModflowLak6 <> nil)
            and AScreenObject.ModflowLak6.Used);
  end;
  function RowOk(Grid: TRbwDataGrid4; Row: Integer): Boolean;
  var
    ColIndex: Integer;
  begin
    result := True;
    for ColIndex := 0 to Grid.ColCount - 1 do
    begin
      if Trim(Grid.Cells[ColIndex, Row]) = '' then
      begin
        result := False;
        Exit;
      end;
    end;
  end;
begin
  Model := frmGoPhast.PhastModel;
  NCOMP := Model.MobileComponents.Count + Model.ImmobileComponents.Count;

  PrecipConc := TMt3dLktConcCollection.Create(nil, nil, nil);
  RunoffConcentration := TMt3dLktConcCollection.Create(nil, nil, nil);
  try
    if not ClearAll then
    begin
      for TimeIndex := 0 to seNumberOfTimes.AsInteger - 1 do
      begin
        if RowOk(rdgModflowBoundary, TimeIndex+1) then
        begin
          ConcItem := PrecipConc.Add as TMt3dLktConcItem;
          ConcItem.StartTime := rdgModflowBoundary.RealValue[Ord(gcStartTime), TimeIndex+1];
          ConcItem.EndTime := rdgModflowBoundary.RealValue[Ord(gcEndTime), TimeIndex+1];
          for ComponentIndex := 0 to NCOMP - 1 do
          begin
            ConcItem.Mt3dmsConcRate[ComponentIndex] :=
              rdgModflowBoundary.Cells[ComponentIndex + Ord(gcComponentStart),
              TimeIndex+1];
          end;
        end;
      end;

      for TimeIndex := 0 to seRunoffTimes.AsInteger - 1 do
      begin
        if RowOk(rdgRunoffConc, TimeIndex+1) then
        begin
          ConcItem := RunoffConcentration.Add as TMt3dLktConcItem;
          ConcItem.StartTime := rdgRunoffConc.RealValue[Ord(gcStartTime), TimeIndex+1];
          ConcItem.EndTime := rdgRunoffConc.RealValue[Ord(gcEndTime), TimeIndex+1];
          for ComponentIndex := 0 to NCOMP - 1 do
          begin
            ConcItem.Mt3dmsConcRate[ComponentIndex] :=
              rdgRunoffConc.Cells[ComponentIndex + Ord(gcComponentStart),
              TimeIndex+1];
          end;
        end;
      end;
    end;

    for Index := 0 to List.Count - 1 do
    begin
      BoundaryUsed := False;
      AScreenObject := List.Items[Index].ScreenObject;
      Mt3dLktConcBoundary := AScreenObject.Mt3dLktConcBoundary;
      if (Mt3dLktConcBoundary <> nil) and Mt3dLktConcBoundary.Used then
      begin
        BoundaryUsed := True;
      end;
      if ClearAll then
      begin
        if BoundaryUsed then
        begin
          Mt3dLktConcBoundary.Clear;
        end;
      end
      else if SetAll or BoundaryUsed then
      begin
        if (Mt3dLktConcBoundary = nil) and LakeUsed(AScreenObject)
          then
        begin
          AScreenObject.CreateMt3dLktConcBoundary;
          Mt3dLktConcBoundary := AScreenObject.Mt3dLktConcBoundary;
        end;

        if Mt3dLktConcBoundary <> nil then
        begin
          if not LakeUsed(AScreenObject) then
          begin
            Mt3dLktConcBoundary.Clear;
          end
          else
          begin
            while Mt3dLktConcBoundary.InitialConcentrations.Count < NCOMP do
            begin
              Mt3dLktConcBoundary.InitialConcentrations.Add;
            end;
            while Mt3dLktConcBoundary.InitialConcentrations.Count > NCOMP do
            begin
              Mt3dLktConcBoundary.InitialConcentrations.Last.Free;
            end;
            for ComponentIndex := 1 to rdgInitialConcentration.RowCount - 1 do
            begin
              if rdgInitialConcentration.Cells[1, ComponentIndex] <> '' then
              begin

                Mt3dLktConcBoundary.InitialConcentrations[ComponentIndex-1].InitConc
                  := rdgInitialConcentration.Cells[1, ComponentIndex];
              end;
            end;

            if (PrecipConc.Count > 0) or (List.Count = 1) then
            begin
              Mt3dLktConcBoundary.Values := PrecipConc;
            end;

            if (RunoffConcentration.Count > 0) or (List.Count = 1) then
            begin
              Mt3dLktConcBoundary.RunoffConcentration := RunoffConcentration;
            end;
          end;
        end;
      end;
    end;
  finally
    PrecipConc.Free;
    RunoffConcentration.Free;
  end;
end;


procedure TframeScreenObjectLkt.SetDeletedRunoffCells(ACol, ARow: integer;
  const Value: boolean);
var
  OldColCount: integer;
  OldRowCount: integer;
  ColIndex: Integer;
  RowIndex: Integer;
begin
  if (ACol < 0) or (ARow < 0) or (ACol >= rdgRunoffConc.ColCount)
    or (ARow >= rdgRunoffConc.RowCount) then
  begin
    Exit;
  end;
  Assert(ACol >= 0);
  Assert(ARow >= 0);
  Assert(ACol < rdgRunoffConc.ColCount);
  Assert(ARow < rdgRunoffConc.RowCount);
  OldColCount := Length(FDeletedRunoffCells);
  if OldColCount = 0 then
  begin
    OldRowCount := 0;
  end
  else
  begin
    OldRowCount := Length(FDeletedRunoffCells[0])
  end;
  if (ACol >= OldColCount) or (ARow >= OldRowCount) then
  begin
    SetLength(FDeletedRunoffCells, rdgRunoffConc.ColCount,
      rdgRunoffConc.RowCount);
    for ColIndex := OldColCount to rdgRunoffConc.ColCount - 1 do
    begin
      for RowIndex := 0 to rdgRunoffConc.RowCount - 1 do
      begin
        FDeletedRunoffCells[ColIndex,RowIndex] := False;
      end;
    end;
    for ColIndex := 0 to OldColCount - 1 do
    begin
      for RowIndex := OldRowCount to rdgRunoffConc.RowCount - 1 do
      begin
        FDeletedRunoffCells[ColIndex,RowIndex] := False;
      end;
    end;
  end;
  FDeletedRunoffCells[ACol, ARow] := Value;
end;

function TframeScreenObjectLkt.ShouldEnableRunoffMultisetControls: Boolean;
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  result := False;
  for RowIndex := rdgRunoffConc.FixedRows to rdgRunoffConc.RowCount - 1 do
  begin
    for ColIndex := FLastTimeColumn + 1 to rdgRunoffConc.ColCount - 1 do
    begin
      result := rdgRunoffConc.IsSelectedCell(ColIndex, RowIndex);
      if result then
      begin
        Exit;
      end;
    end;
  end;
end;

end.
