unit frameSwiObsInterpolatedUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  JvExStdCtrls, JvCombobox, JvListComb, frameGridUnit, Vcl.ExtCtrls,
  GoPhastTypes, UndoItemsScreenObjects, RbwDataGrid4, Vcl.Mask, JvExMask, JvSpin,
  ArgusDataEntry;

type
  TSwiObsCol = (socName, socTime, socZeta, socStatistic, socStatisticType, socComment);

  TframeSwiObsInterpolated = class(TFrame)
    pnlCaption: TPanel;
    frameSwiObs: TframeGrid;
    pnlMultiEdit: TPanel;
    comboMultiStatFlag: TJvImageComboBox;
    lblZetaSurfaceNumber: TLabel;
    rdeZetaSurfaceNumber: TRbwDataEntry;
    lblTreatment: TLabel;
    comboTreatment: TComboBox;
    procedure frameSwiObsGridColSize(Sender: TObject; ACol,
      PriorWidth: Integer);
    procedure frameSwiObsGridHorizontalScroll(Sender: TObject);
    procedure comboMultiStatFlagChange(Sender: TObject);
    procedure frameSwiObsGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure frameSwiObsseNumberChange(Sender: TObject);
    procedure frameSwiObsGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
  private
    FChanged: Boolean;
    FTimesCountChanged: Boolean;
    procedure LayoutMultiCellEditControls;
    procedure AssignValuesToSelectedGridCells(const NewText: string;
      Grid: TRbwDataGrid4; const StartCol, EndCol: integer);
    { Private declarations }
  public
    procedure InitializeControls;
    procedure GetData(List: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    { Public declarations }
  end;

implementation

uses
  frmCustomGoPhastUnit, ModflowSwiObsUnit, frmGoPhastUnit;

{$R *.dfm}

{ TframeSwiObsInterpolated }

procedure TframeSwiObsInterpolated.AssignValuesToSelectedGridCells(
  const NewText: string; Grid: TRbwDataGrid4; const StartCol, EndCol: integer);
var
  ColIndex: Integer;
  RowIndex: Integer;
  TempText: string;
begin
  if Grid = nil then
  begin
    Exit;
  end;
  FChanged := True;
  for ColIndex := StartCol to EndCol do
  begin
    if Grid.Columns[ColIndex].Format = rcf4Integer then
    begin
      TempText := IntToStr(Round(StrToFloat(NewText)));
    end
    else
    begin
      TempText := NewText;
    end;
    for RowIndex := Grid.FixedRows to Grid.RowCount - 1 do
    begin
      if Grid.IsSelectedCell(ColIndex, RowIndex) then
      begin
        Grid.Cells[ColIndex, RowIndex] := TempText;
        if Assigned(Grid.OnSetEditText) then
        begin
          Grid.OnSetEditText(Grid, ColIndex, RowIndex, TempText);
        end;
      end;
    end;
  end;
  if Assigned(Grid.OnExit) then
  begin
    Grid.OnExit(Grid);
  end;
end;

procedure TframeSwiObsInterpolated.comboMultiStatFlagChange(Sender: TObject);
begin
  AssignValuesToSelectedGridCells(comboMultiStatFlag.Text, frameSwiObs.Grid,
    Ord(socStatisticType), Ord(socStatisticType));
end;

procedure TframeSwiObsInterpolated.frameSwiObsGridColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  LayoutMultiCellEditControls;
end;

procedure TframeSwiObsInterpolated.frameSwiObsGridHorizontalScroll(
  Sender: TObject);
begin
  LayoutMultiCellEditControls
end;

procedure TframeSwiObsInterpolated.frameSwiObsGridSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  EnableMultiEditControl(frameSwiObs.Grid, comboMultiStatFlag,
    Ord(socStatisticType));
end;

procedure TframeSwiObsInterpolated.frameSwiObsGridSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  FChanged := True;
end;

procedure TframeSwiObsInterpolated.frameSwiObsseNumberChange(Sender: TObject);
begin
  FTimesCountChanged := True;
  FChanged := True;
  frameSwiObs.seNumberChange(Sender);
end;

procedure TframeSwiObsInterpolated.GetData(List: TScreenObjectEditCollection);
var
  Index: Integer;
  Item: TScreenObjectEditItem;
  Observations: TSwiObsBoundary;
  FoundFirst: Boolean;
  ItemIndex: Integer;
  SwiItem: TSwiObsItem;
  ObsCount: Integer;
  AValue: integer;
begin
  InitializeControls;
  frameSwiObs.seNumber.AsInteger := 0;
  frameSwiObs.Grid.BeginUpdate;
  ObsCount := 0;
  try
    frameSwiObs.ClearGrid;
    FoundFirst := False;
    for Index := 0 to List.Count - 1 do
    begin
      Item := List.Items[Index];
      Observations := Item.ScreenObject.ModflowSwiObservations;
      if (Observations <> nil) and Observations.Used then
      begin
        if not FoundFirst then
        begin
          rdeZetaSurfaceNumber.IntegerValue := Observations.ZetaSurfaceNumber;
          comboTreatment.ItemIndex := Ord(Observations.Purpose);

          ObsCount := Observations.Values.Count;
          frameSwiObs.seNumber.AsInteger := ObsCount;
          frameSwiObs.seNumberChange(nil);

          for ItemIndex := 0 to Observations.Values.Count - 1 do
          begin
            SwiItem := Observations.Values.SwiItems[ItemIndex];
            frameSwiObs.Grid.Cells[Ord(socName), ItemIndex+ 1] := SwiItem.Name;
            frameSwiObs.Grid.RealValue[Ord(socTime), ItemIndex+ 1] := SwiItem.Time;
            frameSwiObs.Grid.RealValue[Ord(socZeta), ItemIndex+ 1] := SwiItem.ObservedValue;
            frameSwiObs.Grid.RealValue[Ord(socStatistic), ItemIndex+ 1] := SwiItem.Statistic;
            frameSwiObs.Grid.ItemIndex[Ord(socStatisticType), ItemIndex+ 1] := Ord(SwiItem.StatFlag);
            frameSwiObs.Grid.Cells[Ord(socComment), ItemIndex+ 1] := SwiItem.Comment;
          end;
          FoundFirst := true;
        end
        else
        begin
          if TryStrToInt(rdeZetaSurfaceNumber.Text, AValue) then
          begin
            rdeZetaSurfaceNumber.Text := '';
          end;
          if comboTreatment.ItemIndex <> Ord(Observations.Purpose) then
          begin
            comboTreatment.ItemIndex := -1;
          end;

          if frameSwiObs.seNumber.AsInteger <> Observations.Values.Count then
          begin
            frameSwiObs.ClearGrid;
          end
          else
          begin
            for ItemIndex := 0 to Observations.Values.Count - 1 do
            begin
              SwiItem := Observations.Values.SwiItems[ItemIndex];
              if frameSwiObs.Grid.Cells[Ord(socName), ItemIndex+ 1] <> SwiItem.Name then
              begin
                frameSwiObs.Grid.Cells[Ord(socName), ItemIndex+ 1] := '';
              end;
              if frameSwiObs.Grid.Cells[Ord(socTime), ItemIndex+ 1] <>
                FloatToStr(SwiItem.Time) then
              begin
                frameSwiObs.Grid.Cells[Ord(socTime), ItemIndex+ 1] := '';
              end;
              if frameSwiObs.Grid.Cells[Ord(socZeta), ItemIndex+ 1] <>
                FloatToStr(SwiItem.ObservedValue) then
              begin
                frameSwiObs.Grid.Cells[Ord(socZeta), ItemIndex+ 1] := '';
              end;
              if frameSwiObs.Grid.Cells[Ord(socStatistic), ItemIndex+ 1] <>
                FloatToStr(SwiItem.Statistic) then
              begin
                frameSwiObs.Grid.Cells[Ord(socStatistic), ItemIndex+ 1] := '';
              end;
              if frameSwiObs.Grid.ItemIndex[Ord(socStatisticType), ItemIndex+ 1] <> Ord(SwiItem.StatFlag) then
              begin
                frameSwiObs.Grid.Cells[Ord(socStatisticType), ItemIndex+ 1] := '';
              end;
              if frameSwiObs.Grid.Cells[Ord(socComment), ItemIndex+ 1] <> SwiItem.Comment then
              begin
                frameSwiObs.Grid.Cells[Ord(socComment), ItemIndex+ 1] := '';
              end;
            end;
          end;
        end;
      end;
    end;

  finally
    frameSwiObs.Grid.EndUpdate;
    frameSwiObs.seNumber.AsInteger := ObsCount;
    frameSwiObs.seNumberChange(nil);
  end;
  FChanged := False;
  FTimesCountChanged := False;
end;

procedure TframeSwiObsInterpolated.InitializeControls;
begin
  rdeZetaSurfaceNumber.Max := frmGoPhast.PhastModel.ModflowPackages.SwiPackage.NumberOfSurfaces;
  rdeZetaSurfaceNumber.IntegerValue := 1;
  comboTreatment.ItemIndex := 0;
  {$IF CompilerVersion > 28}
  comboMultiStatFlag.Items.ClearAndResetID;
  {$ENDIF}
  comboMultiStatFlag.Items.Assign(ObservationStatFlagLabels);
  frameSwiObs.Grid.BeginUpdate;
  try
    frameSwiObs.Grid.Columns[Ord(socStatisticType)].PickList :=
      ObservationStatFlagLabels;
    frameSwiObs.Grid.Cells[Ord(socName),0] := 'Observation Name';
    frameSwiObs.Grid.Cells[Ord(socTime),0] := 'Time';
    frameSwiObs.Grid.Cells[Ord(socZeta),0] := 'Zeta';
    frameSwiObs.Grid.Cells[Ord(socStatistic),0] := 'Statistic';
    frameSwiObs.Grid.Cells[Ord(socStatisticType),0] := 'StatFlag';
    frameSwiObs.Grid.Cells[Ord(socComment),0] := 'Comment';

  finally
    frameSwiObs.Grid.EndUpdate;
  end;

  LayoutMultiCellEditControls;
end;

procedure TframeSwiObsInterpolated.LayoutMultiCellEditControls;
begin
  LayoutControls(frameSwiObs.Grid, comboMultiStatFlag, nil, Ord(socStatisticType));
end;

procedure TframeSwiObsInterpolated.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
var
  Index: Integer;
  Item: TScreenObjectEditItem;
  Observations: TSwiObsBoundary;
  ObservationUsed: boolean;
  ValueCount: integer;
  RowIndex: Integer;
  Zeta, Time, Statistic: double;
  SwiItem: TSwiObsItem;
  NewComment: string;
  AValue: Integer;
begin
  if not FChanged then
  begin
    Exit;
  end;
  for Index := 0 to List.Count - 1 do
  begin
    Item := List.Items[Index];
    Observations := Item.ScreenObject.ModflowSwiObservations;
    ObservationUsed := (Observations <> nil) and Observations.Used;

    if ClearAll then
    begin
      if ObservationUsed then
      begin
        Observations.Clear;
      end;
    end
    else if SetAll or ObservationUsed then
    begin
      if Observations = nil then
      begin
        Item.ScreenObject.CreateHeadObservations;
        Observations := Item.ScreenObject.ModflowSwiObservations;
      end;

      if TryStrToInt(rdeZetaSurfaceNumber.Text, AValue) then
      begin
        Observations.ZetaSurfaceNumber := AValue;
      end;

      if comboTreatment.ItemIndex >= 0 then
      begin
        Observations.Purpose := TObservationPurpose(comboTreatment.ItemIndex);
      end;

      ValueCount := 0;
      for RowIndex := 1 to frameSwiObs.seNumber.AsInteger do
      begin
        if (TryStrToFloat(frameSwiObs.Grid.Cells[Ord(socTime), RowIndex], Time)
          or (frameSwiObs.Grid.Cells[Ord(socTime), RowIndex] = ''))
          and (TryStrToFloat(frameSwiObs.Grid.Cells[Ord(socZeta), RowIndex], Zeta)
          or (frameSwiObs.Grid.Cells[Ord(socZeta), RowIndex] = '')) then
        begin
          Inc(ValueCount);
        end;
      end;
      if FTimesCountChanged then
      begin
        while Observations.Values.Count < ValueCount do
        begin
          Observations.Values.Add;
        end;
        while Observations.Values.Count > ValueCount do
        begin
          Observations.Values.Delete(Observations.Values.Count-1);
        end;
      end;
      ValueCount := 0;
      for RowIndex := 1 to frameSwiObs.seNumber.AsInteger do
      begin
        if (TryStrToFloat(frameSwiObs.Grid.Cells[Ord(socTime), RowIndex], Time)
          or (frameSwiObs.Grid.Cells[Ord(socTime), RowIndex] = ''))
          and (TryStrToFloat(frameSwiObs.Grid.Cells[Ord(socZeta), RowIndex], Zeta)
          or (frameSwiObs.Grid.Cells[Ord(socZeta), RowIndex] = '')) then
        begin
          if ValueCount < Observations.Values.Count then
          begin
            SwiItem := Observations.Values.SwiItems[ValueCount];
            if (frameSwiObs.Grid.Cells[Ord(socName), RowIndex] <> '') then
            begin
              SwiItem.Name := frameSwiObs.Grid.Cells[Ord(socName), RowIndex];
            end;
            if (frameSwiObs.Grid.Cells[Ord(socTime), RowIndex] <> '') then
            begin
              SwiItem.Time := Time;
            end;
            if (frameSwiObs.Grid.Cells[Ord(socZeta), RowIndex] <> '') then
            begin
              SwiItem.ObservedValue := Zeta;
            end;
            if TryStrToFloat(frameSwiObs.Grid.Cells[Ord(socStatistic), RowIndex], Statistic) then
            begin
              SwiItem.Statistic := Statistic;
            end;
            if (frameSwiObs.Grid.Cells[Ord(socStatisticType), RowIndex] <> '') then
            begin
              SwiItem.StatFlag := TStatFlag(frameSwiObs.Grid.ItemIndex[Ord(socStatisticType), RowIndex]);
            end;
            NewComment := frameSwiObs.Grid.Cells[Ord(socComment), RowIndex];
            if (List.Count = 1) or (NewComment <> '') then
            begin
              SwiItem.Comment := NewComment;
            end;
          end;
          Inc(ValueCount);
        end;
      end;
    end;
  end;
end;

end.
