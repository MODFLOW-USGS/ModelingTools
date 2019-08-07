unit frameFarmDiversionUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frameFormulaGridUnit, ExtCtrls,
  Grids, RbwDataGrid4, StdCtrls, Mask, JvExMask, JvSpin, Buttons,
  UndoItemsScreenObjects, ArgusDataEntry, ModflowFmpFarmUnit;

type
  TDiversionType = (dtDiversion, dtReturnFlow);

  TframeFarmDiversion = class(TframeFormulaGrid)
    comboMethod: TComboBox;
    lblLocationMethod: TLabel;
    comboSfrObjects: TComboBox;
    lblSfrObjects: TLabel;
    comboPositionChoice: TComboBox;
    lblPositionChoice: TLabel;
    rdeVertexNumber: TRbwDataEntry;
    lblVertexNumber: TLabel;
    lblX: TLabel;
    rdeX: TRbwDataEntry;
    rdeY: TRbwDataEntry;
    lblY: TLabel;
    rdeRow: TRbwDataEntry;
    lblRow: TLabel;
    rdeCol: TRbwDataEntry;
    lblCol: TLabel;
    procedure comboMethodChange(Sender: TObject);
    procedure GridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure edFormulaChange(Sender: TObject);
    procedure seNumberChange(Sender: TObject);
    procedure sbAddClick(Sender: TObject);
    procedure sbInsertClick(Sender: TObject);
    procedure sbDeleteClick(Sender: TObject);
    procedure comboSfrObjectsChange(Sender: TObject);
    procedure comboPositionChoiceChange(Sender: TObject);
    procedure rdeVertexNumberChange(Sender: TObject);
    procedure rdeXChange(Sender: TObject);
    procedure rdeYChange(Sender: TObject);
    procedure rdeRowChange(Sender: TObject);
    procedure rdeColChange(Sender: TObject);
    procedure GridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  private
    FChanged: boolean;
    FOnChange: TNotifyEvent;
    FChanging: Boolean;
    property Changing: Boolean read FChanging write FChanging;
    procedure DoChange;
//    procedure ClearGrid(Grid: TRbwDataGrid4);
    { Private declarations }
  public
    property DataChanged: Boolean read FChanged;
    procedure InitializeControls;
    // ScreenObjectList contains only objects that define farms.
    procedure GetData(FarmList: TFarmList;
      DiversionType: TDiversionType);
    procedure SetData(FarmList: TFarmList; DiversionType: TDiversionType);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    procedure LayoutMultiRowEditControls; override;
    { Public declarations }
  end;

var
  frameFarmDiversion: TframeFarmDiversion;

implementation

uses
  ModflowDrtUnit, frmGoPhastUnit, ModflowGridUnit,
  GoPhastTypes, ModflowTimeUnit, PhastModelUnit, ScreenObjectUnit,
  ModflowSfrUnit, frmCustomGoPhastUnit, ModflowSwrReachUnit;

resourcestring
  StrObjectName = 'Option or object name';
  StrPositionChoice = 'Position choice';
  StrVertexNumber = 'Vertex number';
//  StrFirstVertex = 'First vertex';
//  StrAMiddleVertex = 'A middle vertex';
//  StrLastVertex = 'Last vertex';

type
  TDiversionTimeColumns = (dtcStart, dtcEnd);
  TDiversionObjectColumns = (docObject, docChoice, docVertex);
  TDiversionLocationColumns = (dlcX, dlcY);
  TDiversionCellColumns = (dccRow, dccColumn);

{$R *.dfm}

//procedure TframeFarmDiversion.ClearGrid(Grid: TRbwDataGrid4);
//var
//  RowIndex: Integer;
//  ColIndex: Integer;
//begin
//  for RowIndex := Grid.FixedRows to Grid.RowCount - 1 do
//  begin
//    for ColIndex := Grid.FixedCols to Grid.ColCount - 1 do
//    begin
//      Grid.Cells[ColIndex,RowIndex] := ''
//    end;
//  end;
//end;

procedure TframeFarmDiversion.comboMethodChange(Sender: TObject);
const
  // There is an option to prorate over segments.
  // This is the width required to make that option visible without
  // the user making the column wider.
  RequiredWidthForObjectOption = 230;
var
  DiversionChoice: TReturnChoice;
  ModflowGrid: TModflowGrid;
  ColIndex: Integer;
  RowIndex: Integer;
begin
  inherited;
//  FChanged := True;
  Grid.BeginUpdate;
  try
    if Ord(docVertex)+2 < Grid.ColCount then
    begin
      Grid.Columns[Ord(docVertex)+2].CheckMin := False;
    end;
    if comboMethod.ItemIndex >= 0 then
    begin
      DiversionChoice := TReturnChoice(comboMethod.ItemIndex+1);
      case DiversionChoice of
        rtObject:
          begin
            Grid.ColWidths[Ord(docObject)+2] := RequiredWidthForObjectOption;

            Grid.ColCount := Ord(docVertex)+3;
            Grid.Columns[Ord(docVertex)+2].WordWrapCaptions := True;

            Grid.Cells[Ord(docObject)+2, 0] := StrObjectName;
            Grid.Cells[Ord(docChoice)+2, 0] := StrPositionChoice;
            Grid.Cells[Ord(docVertex)+2, 0] := StrVertexNumber;
            Grid.Columns[Ord(docObject)+2].Format := rcf4String;
            Grid.Columns[Ord(docChoice)+2].Format := rcf4String;
            Grid.Columns[Ord(docVertex)+2].Format := rcf4Integer;
            Grid.Columns[Ord(docObject)+2].ComboUsed := True;
            Grid.Columns[Ord(docChoice)+2].ComboUsed := True;
            Grid.Columns[Ord(docObject)+2].CheckMax := False;
            Grid.Columns[Ord(docChoice)+2].CheckMax := False;
            Grid.Columns[Ord(docObject)+2].CheckMin := False;
            Grid.Columns[Ord(docChoice)+2].CheckMin := False;
            Grid.Columns[Ord(docChoice)+2].PickList := comboPositionChoice.Items;
            Grid.Columns[Ord(docObject)+2].PickList := comboSfrObjects.Items;
            Grid.Columns[Ord(docObject)+2].LimitToList := True;
            Grid.Columns[Ord(docChoice)+2].LimitToList := True;

            Grid.Columns[Ord(docVertex)+2].CheckMin := True;
            Grid.Columns[Ord(docVertex)+2].Min := 1;
          end;
        rtLocation:
          begin
            Grid.ColCount := Ord(dlcY)+3;
            Grid.Cells[Ord(dlcX)+2, 0] := StrX;
            Grid.Cells[Ord(dlcY)+2, 0] := StrY;
            Grid.Columns[Ord(dlcX)+2].Format := rcf4Real;
            Grid.Columns[Ord(dlcY)+2].Format := rcf4Real;
            Grid.Columns[Ord(dlcX)+2].CheckMax := False;
            Grid.Columns[Ord(dlcY)+2].CheckMax := False;
            Grid.Columns[Ord(dlcX)+2].CheckMin := False;
            Grid.Columns[Ord(dlcY)+2].CheckMin := False;
            Grid.Columns[Ord(docObject)+2].LimitToList := False;
            Grid.Columns[Ord(docChoice)+2].LimitToList := False;
            Grid.Columns[Ord(docObject)+2].ComboUsed := False;
            Grid.Columns[Ord(docChoice)+2].ComboUsed := False;
          end;
        rtCell:
          begin
            Grid.ColCount := Ord(dccColumn)+3;
            Grid.Cells[Ord(dccRow)+2, 0] := StrRow;
            Grid.Cells[Ord(dccColumn)+2, 0] := StrColumn;
            Grid.Columns[Ord(dccRow)+2].Format := rcf4Integer;
            Grid.Columns[Ord(dccColumn)+2].Format := rcf4Integer;
            Grid.Columns[Ord(dccRow)+2].CheckMax := True;
            Grid.Columns[Ord(dccColumn)+2].CheckMax := True;
            Grid.Columns[Ord(dccRow)+2].CheckMin := True;
            Grid.Columns[Ord(dccColumn)+2].CheckMin := True;
            ModflowGrid := frmGoPhast.PhastModel.ModflowGrid;
            Grid.Columns[Ord(dccRow)+2].Max := ModflowGrid.RowCount;
            Grid.Columns[Ord(dccColumn)+2].Max := ModflowGrid.ColumnCount;
            Grid.Columns[Ord(dccRow)+2].Min := 1;
            Grid.Columns[Ord(dccColumn)+2].Min := 1;
            Grid.Columns[Ord(docObject)+2].LimitToList := False;
            Grid.Columns[Ord(docChoice)+2].LimitToList := False;
            Grid.Columns[Ord(docObject)+2].ComboUsed := False;
            Grid.Columns[Ord(docChoice)+2].ComboUsed := False;
          end;
        else
          Assert(False);
      end;
    end;
    for RowIndex := 1 to Grid.RowCount-1 do
    begin
      for ColIndex := 2 to Grid.ColCount - 1 do
      begin
        Grid.Cells[ColIndex,RowIndex] := '';
      end;
    end;
  finally
    Grid.EndUpdate
  end;
  DoChange;
  LayoutMultiRowEditControls;
end;

procedure TframeFarmDiversion.comboPositionChoiceChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(Grid, Ord(docChoice)+2, comboPositionChoice.Text);
end;

procedure TframeFarmDiversion.comboSfrObjectsChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(Grid, Ord(docObject)+2, comboSfrObjects.Text);
end;

procedure TframeFarmDiversion.DoChange;
begin
  if Changing then
  begin
    Exit;
  end;
  if Assigned(OnChange) then
  begin
    OnChange(Self);
  end;
  FChanged := True;
end;

procedure TframeFarmDiversion.edFormulaChange(Sender: TObject);
begin
  inherited;
  DoChange;
end;

procedure TframeFarmDiversion.GetData(
  FarmList: TFarmList; DiversionType: TDiversionType);
var
  FirstFarm: TFarm;
  DelivReturns: TSemiRoutedDeliveriesAndReturnFlowCollection;
  AnItem: TSemiRoutedDeliveriesAndRunoffItem;
  ItemIndex: Integer;
  LinkedStream: TSfrDiversion;
  DiversionObject: TSfrDiversionObject;
  DiversionLocation: TReturnLocation;
  DiversionCell: TReturnCell;
  ObjectIndex: Integer;
  AFarm: TFarm;
  FirstDelivReturns: TSemiRoutedDeliveriesAndReturnFlowCollection;
begin
  Assert(FarmList.Count > 0);
  Changing := True;
  Grid.BeginUpdate;
  try
    ClearGrid;
    FirstFarm := FarmList[0];
    DelivReturns := nil;
    case DiversionType of
      dtDiversion:
        begin
          DelivReturns := FirstFarm.SemiRoutedDeliveries
        end;
      dtReturnFlow:
        begin
          DelivReturns := FirstFarm.SemiRoutedReturnFlow
        end;
      else Assert(False);
    end;
    FirstDelivReturns := DelivReturns;
    if DelivReturns.Count > 0 then
    begin
      seNumber.AsInteger := DelivReturns.Count;
      seNumber.OnChange(seNumber);
      AnItem := DelivReturns[0];
      comboMethod.ItemIndex := Ord(AnItem.LinkedStream.DiversionChoice)-1;
      for ItemIndex := 0 to DelivReturns.Count - 1 do
      begin
        AnItem := DelivReturns[ItemIndex];
        LinkedStream := AnItem.LinkedStream;
        Grid.Cells[Ord(dtcStart), ItemIndex+1] := FloatToStr(AnItem.StartTime);
        Grid.Cells[Ord(dtcEnd), ItemIndex+1] := FloatToStr(AnItem.EndTime);
        case LinkedStream.DiversionChoice of
          rtObject:
            begin
              DiversionObject := LinkedStream.DiversionObject;
              if DiversionObject.ScreenObject = nil then
              begin
                Grid.ItemIndex[Ord(docObject)+2, ItemIndex+1] := 0
              end
              else
              begin
                Grid.Cells[Ord(docObject)+2, ItemIndex+1] :=
                  DiversionObject.ObjectName;
              end;
              Grid.ItemIndex[Ord(docChoice)+2, ItemIndex+1] :=
                Ord(DiversionObject.DiversionPosition);
              if DiversionObject.DiversionPosition = dpMiddle then
              begin
                Grid.Cells[Ord(docVertex)+2, ItemIndex+1] :=
                  IntToStr(DiversionObject.DiversionVertex);
              end
              else
              begin
                Grid.Cells[Ord(docVertex)+2, ItemIndex+1] := '';
              end;
            end;
          rtLocation:
            begin
              DiversionLocation := LinkedStream.DiversionLocation;
              Grid.Cells[Ord(dlcX)+2, ItemIndex+1] :=
                FloatToStr(DiversionLocation.X);
              Grid.Cells[Ord(dlcY)+2, ItemIndex+1] :=
                FloatToStr(DiversionLocation.Y);

            end;
          rtCell:
            begin
              DiversionCell := LinkedStream.DiversionCell;
              Grid.Cells[Ord(dccRow)+2, ItemIndex+1] :=
                IntToStr(DiversionCell.Row);
              Grid.Cells[Ord(dccColumn)+2, ItemIndex+1] :=
                IntToStr(DiversionCell.Col);
            end;
          else
            Assert(False);
        end;
      end;

      for ObjectIndex := 1 to FarmList.Count - 1 do
      begin
        AFarm := FarmList[ObjectIndex];
        case DiversionType of
          dtDiversion:
            begin
              DelivReturns := AFarm.SemiRoutedDeliveries
            end;
          dtReturnFlow:
            begin
              DelivReturns := AFarm.SemiRoutedReturnFlow
            end;
          else
            Assert(False);
        end;
        if not FirstDelivReturns.IsSame(DelivReturns) then
        begin
          ClearGrid;
          seNumber.AsInteger := 0;
          seNumber.OnChange(seNumber);
          comboMethod.ItemIndex := 0;
          break;
        end;
      end;
    end
    else
    begin
      ClearGrid;
      seNumber.AsInteger := 0;
      seNumber.OnChange(seNumber);
      comboMethod.ItemIndex := 0;
    end;
  finally
    Grid.EndUpdate;
    FChanged := False;
    Changing := False;
  end;
end;

procedure TframeFarmDiversion.GridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ShouldEnable: Boolean;
  ColIndex: Integer;
  RowIndex: Integer;
begin
  inherited;
  ShouldEnable := False;
  ColIndex := Ord(docObject)+2;
  begin
    for RowIndex := Grid.FixedRows to Grid.RowCount -1 do
    begin
      ShouldEnable := Grid.IsSelectedCell(ColIndex,RowIndex);
      if ShouldEnable then
      begin
        Break;
      end;
    end;
  end;
  comboSfrObjects.Enabled := ShouldEnable;
  lblSfrObjects.Enabled := ShouldEnable;
  rdeRow.Enabled := ShouldEnable;
  lblRow.Enabled := ShouldEnable;
  rdeX.Enabled := ShouldEnable;
  lblX.Enabled := ShouldEnable;

  ShouldEnable := False;
  ColIndex := Ord(docChoice)+2;
  begin
    for RowIndex := Grid.FixedRows to Grid.RowCount -1 do
    begin
      ShouldEnable := Grid.IsSelectedCell(ColIndex,RowIndex);
      if ShouldEnable then
      begin
        Break;
      end;
    end;
  end;
  comboPositionChoice.Enabled := ShouldEnable;
  lblPositionChoice.Enabled := ShouldEnable;
  rdeCol.Enabled := ShouldEnable;
  lblCol.Enabled := ShouldEnable;
  rdeY.Enabled := ShouldEnable;
  lblY.Enabled := ShouldEnable;

  ShouldEnable := False;
  ColIndex := Ord(docVertex)+2;
  begin
    for RowIndex := Grid.FixedRows to Grid.RowCount -1 do
    begin
      ShouldEnable := Grid.IsSelectedCell(ColIndex,RowIndex);
      if ShouldEnable then
      begin
        Break;
      end;
    end;
  end;
  rdeVertexNumber.Enabled := ShouldEnable;
  LblVertexNumber.Enabled := ShouldEnable;

end;

procedure TframeFarmDiversion.GridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  DiversionChoice: TReturnChoice;
begin
  inherited;
  DiversionChoice := TReturnChoice(comboMethod.ItemIndex+1);
  if DiversionChoice = rtObject then
  begin
    if (ARow >= 1) then
    begin
      if (ACol = Ord(docVertex)+2) then
      begin
        CanSelect := (Grid.ItemIndex[Ord(docObject)+2,ARow] > 0)
          and (Grid.ItemIndex[Ord(docChoice)+2,ARow] = Ord(dpMiddle));
      end
      else if (ACol = Ord(docChoice)+2) then
      begin
        CanSelect := (Grid.ItemIndex[Ord(docObject)+2,ARow] > 0);
      end;
    end;
  end;
end;

procedure TframeFarmDiversion.GridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
  if (ARow >= 1) and (comboMethod.ItemIndex = Ord(rtObject)-1)
    and (TDiversionObjectColumns(ACol-2) in [docObject, docChoice]) then
  begin
    Grid.Invalidate;
  end;
  DoChange;
end;

procedure TframeFarmDiversion.LayoutMultiRowEditControls;
var
  ReturnChoice: TReturnChoice;
begin
  inherited;
  if comboSfrObjects = nil then
  begin
    Exit;
  end;

  edFormula.Visible := False;
  ReturnChoice := TReturnChoice(comboMethod.ItemIndex+1);

  comboSfrObjects.Visible := ReturnChoice = rtObject;
  lblSfrObjects.Visible := ReturnChoice = rtObject;
  comboPositionChoice.Visible := ReturnChoice = rtObject;
  lblPositionChoice.Visible := ReturnChoice = rtObject;
  rdeVertexNumber.Visible := ReturnChoice = rtObject;
  lblVertexNumber.Visible := ReturnChoice = rtObject;

  rdeX.Visible := ReturnChoice = rtLocation;
  lblX.Visible := ReturnChoice = rtLocation;
  rdeY.Visible := ReturnChoice = rtLocation;
  lblY.Visible := ReturnChoice = rtLocation;

  rdeRow.Visible := ReturnChoice = rtCell;
  lblRow.Visible := ReturnChoice = rtCell;
  rdeCol.Visible := ReturnChoice = rtCell;
  lblCol.Visible := ReturnChoice = rtCell;

  case ReturnChoice of
    rtObject:
      begin
        LayoutControls(Grid, comboSfrObjects, lblSfrObjects,
          Ord(docObject) + 2);
        LayoutControls(Grid, comboPositionChoice, lblPositionChoice,
          Ord(docChoice) + 2);
        LayoutControls(Grid, rdeVertexNumber, lblVertexNumber,
          Ord(docVertex) + 2);
      end;
    rtLocation:
      begin
        LayoutControls(Grid, rdeX, lblX,
          Ord(dlcX) + 2);
        LayoutControls(Grid, rdeY, lblY,
          Ord(dlcY) + 2);
      end;
    rtCell:
      begin
        LayoutControls(Grid, rdeRow, lblRow,
          Ord(dccRow) + 2);
        LayoutControls(Grid, rdeCol, lblCol,
          Ord(dccColumn) + 2);
      end;
    else Assert(False);
  end;
end;

procedure TframeFarmDiversion.rdeColChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(Grid, Ord(dccColumn)+2, rdeCol.Text);
end;

procedure TframeFarmDiversion.rdeRowChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(Grid, Ord(dccRow)+2, rdeRow.Text);
end;

procedure TframeFarmDiversion.rdeVertexNumberChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(Grid, Ord(docVertex)+2, rdeVertexNumber.Text);
end;

procedure TframeFarmDiversion.rdeXChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(Grid, Ord(dlcX)+2, rdeX.Text);
end;

procedure TframeFarmDiversion.rdeYChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(Grid, Ord(dlcY)+2, rdeY.Text);
end;

procedure TframeFarmDiversion.sbAddClick(Sender: TObject);
begin
  inherited;
//  FChanged := True;
  DoChange;
end;

procedure TframeFarmDiversion.sbDeleteClick(Sender: TObject);
begin
  inherited;
//  FChanged := True;
  DoChange;
end;

procedure TframeFarmDiversion.sbInsertClick(Sender: TObject);
begin
  inherited;
  DoChange;
end;

procedure TframeFarmDiversion.seNumberChange(Sender: TObject);
begin
  inherited;
  DoChange;
end;

procedure TframeFarmDiversion.SetData(FarmList: TFarmList;
  DiversionType: TDiversionType);
var
  index: Integer;
  Farm: TFarm;
  DelivReturn: TSemiRoutedDeliveriesAndReturnFlowCollection;
  Count: Integer;
  TimeIndex: Integer;
  StartTime: double;
  EndTime: double;
  DelivRetItem: TSemiRoutedDeliveriesAndRunoffItem;
  LinkedStream: TSfrDiversion;
  DiversionObject: TSfrDiversionObject;
  DiversionLocation: TReturnLocation;
  DiversionCell: TReturnCell;
  GridItemIndex: Integer;
begin
  for index := 0 to FarmList.Count - 1 do
  begin
    Farm := FarmList[index];
    if Farm <> nil then
    begin
      DelivReturn := nil;
      case DiversionType of
        dtDiversion:
          begin
            DelivReturn := Farm.SemiRoutedDeliveries;
          end;
        dtReturnFlow:
          begin
            DelivReturn := Farm.SemiRoutedReturnFlow;
          end;
        else Assert(False);
      end;
      Count := 0;
      for TimeIndex := 1 to seNumber.AsInteger do
      begin
        if TryStrToFloat(Grid.Cells[Ord(dtcStart), TimeIndex], StartTime)
          and TryStrToFloat(Grid.Cells[Ord(dtcEnd), TimeIndex], EndTime) then
        begin
          if Count < DelivReturn.Count then
          begin
            DelivRetItem := DelivReturn[Count];
          end
          else
          begin
            DelivRetItem := DelivReturn.Add;
          end;
          Inc(Count);
          DelivRetItem.StartTime := StartTime;
          DelivRetItem.EndTime := EndTime;
          LinkedStream := DelivRetItem.LinkedStream;
          Assert(comboMethod.ItemIndex >= 0);
          LinkedStream.DiversionChoice := TReturnChoice(comboMethod.ItemIndex+1);
          case LinkedStream.DiversionChoice of
            rtObject:
              begin
                DiversionObject := LinkedStream.DiversionObject;
                GridItemIndex := Grid.ItemIndex[Ord(docObject)+2,TimeIndex];
                if GridItemIndex >= 0 then
                begin
                  DiversionObject.ScreenObject := Grid.Columns[Ord(docObject)+2].PickList.Objects[GridItemIndex];
                end
                else
                begin
                  DiversionObject.ScreenObject := nil;
                end;
                DiversionObject.DiversionPosition :=
                  TDiversionPosition(Grid.ItemIndex[Ord(docChoice)+2,TimeIndex]);
                if DiversionObject.DiversionPosition = dpMiddle then
                begin
                  DiversionObject.DiversionVertex :=
                    StrToIntDef(Grid.Cells[Ord(docVertex)+2,TimeIndex], 1);
                end;
              end;
            rtLocation:
              begin
                DiversionLocation := LinkedStream.DiversionLocation;
                DiversionLocation.X :=
                  StrToFloatDef(Grid.Cells[Ord(dlcX)+2,TimeIndex], 0);
                DiversionLocation.Y :=
                  StrToFloatDef(Grid.Cells[Ord(dlcY)+2,TimeIndex], 0);
                DiversionLocation.Z := 0;
              end;
            rtCell:
              begin
                DiversionCell := LinkedStream.DiversionCell;
                DiversionCell.Row :=
                  StrToIntDef(Grid.Cells[Ord(dccRow)+2,TimeIndex], 1);
                DiversionCell.Col :=
                  StrToIntDef(Grid.Cells[Ord(dccColumn)+2,TimeIndex], 1);
                DiversionCell.Lay := 0;
              end;
            else Assert(False);
          end;
        end;
      end;
    end;
  end;
end;

procedure TframeFarmDiversion.InitializeControls;
var
  ColIndex: Integer;
  StressPeriods: TModflowStressPeriods;
  LocalModel: TPhastModel;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  SfrBoundary: TSfrBoundary;
  SwrBoundary: TSwrReachBoundary;
begin
  comboSfrObjects.Items.BeginUpdate;
  try
    comboSfrObjects.Items.Clear;
    if frmGoPhast.PhastModel.SfrIsSelected or frmGoPhast.PhastModel.SwrIsSelected then
    begin
      LocalModel := frmGoPhast.PhastModel;
      comboSfrObjects.Items.Add('prorate over segments option');
      for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
      begin
        AScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
        if AScreenObject.Deleted then
        begin
          Continue;
        end;
        SfrBoundary := AScreenObject.ModflowSfrBoundary;
        SwrBoundary := AScreenObject.ModflowSwrReaches;
        if (SfrBoundary <> nil) and SfrBoundary.Used then
        begin
          comboSfrObjects.Items.AddObject(AScreenObject.Name, AScreenObject);
        end
        else if (SwrBoundary <> nil) and SwrBoundary.Used then
        begin
          comboSfrObjects.Items.AddObject(AScreenObject.Name, AScreenObject);
        end;
      end;
    end;

  finally
    comboSfrObjects.Items.EndUpdate
  end;
  Grid.BeginUpdate;
  try
    ClearGrid;
    FirstFormulaColumn := Succ(Ord(dtcEnd));
    Grid.ColCount := 5;
    for ColIndex := 0 to Grid.ColCount - 1 do
    begin
      Grid.Columns[ColIndex].AutoAdjustColWidths := True;
      Grid.Columns[ColIndex].AutoAdjustRowHeights := True;
      Grid.Columns[ColIndex].WordWrapCaptions := True;
    end;
    Grid.Columns[Ord(docObject)+2].WordWrapCaptions := False;

    for ColIndex := Succ(Ord(High(TDiversionTimeColumns))) to Grid.ColCount - 1 do
    begin
      Grid.Columns[ColIndex].ButtonUsed := True;
      Grid.Columns[ColIndex].ButtonCaption := StrFormulaButtonCaption;
      Grid.Columns[ColIndex].ButtonWidth := 35;
    end;
    Grid.Cells[Ord(dtcStart), 0] := StrStartingTime;
    Grid.Cells[Ord(dtcEnd), 0] := StrEndingTime;
    StressPeriods := frmGoPhast.PhastModel.ModflowStressPeriods;
    StressPeriods.FillPickListWithStartTimes(Grid, Ord(dtcStart));
    StressPeriods.FillPickListWithEndTimes(Grid, Ord(dtcEnd));

    comboMethodChange(nil);
  finally
    Grid.EndUpdate;
  end;

  for ColIndex := 0 to Grid.ColCount - 1 do
  begin
    Grid.Columns[ColIndex].AutoAdjustColWidths := False;
  end;

  LayoutMultiRowEditControls;
end;

end.
