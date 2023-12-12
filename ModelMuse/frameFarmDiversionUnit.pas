unit frameFarmDiversionUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frameFormulaGridUnit, ExtCtrls,
  Grids, RbwDataGrid4, StdCtrls, Mask, JvExMask, JvSpin, Buttons,
  ArgusDataEntry, ModflowFmpFarmUnit, RbwController;

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
    FLowerLimitCol: Integer;
    FUpperLimitCol: Integer;
    FLowerLimitUsed: Boolean;
    FUpperLimitUsed: Boolean;
    property Changing: Boolean read FChanging write FChanging;
    procedure DoChange;
    { Private declarations }
  public
    property DataChanged: Boolean read FChanged;
    procedure InitializeControls;
    // ScreenObjectList contains only objects that define farms.
    procedure GetData(FarmList: TFarmList;
      DiversionType: TDiversionType);
    procedure GetDataFromListOfSemiRoutedLists(SrList: TSrCollList;
      DiversionType: TDiversionType);
    procedure SetData(FarmList: TFarmList; DiversionType: TDiversionType);
    procedure SetDataForListOfSemiRoutedLists(SrList: TSrCollList;
      DiversionType: TDiversionType);
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
  ModflowSfrUnit, frmCustomGoPhastUnit, ModflowSwrReachUnit,
  ModflowPackagesUnit, ModflowPackageSelectionUnit;

resourcestring
  StrObjectName = 'Object name';
  StrObjectOrOptionName = 'Option or object name';
  StrPositionChoice = 'Position choice';
  StrVertexNumber = 'Vertex number';
  StrFraction = 'Fraction';
  StrLowerLimit = 'Lower limit';
  StrUpperLimit = 'Upper limit';

type
  TDiversionTimeColumns = (dtcStart, dtcEnd);
  TDiversionObjectColumns = (docObject, docChoice, docVertex, docFraction, docLowerLimit, docUpperLimit);
  TDiversionLocationColumns = (dlcX, dlcY, dlFraction, dlLowerLimit, dlUpperLimit);
  TDiversionCellColumns = (dccRow, dccColumn, dccFraction, dccLowerLimit, dccUpperLimit);

{$R *.dfm}

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
  ModelSelection: TModelSelection;
begin
  inherited;
  ModelSelection := frmGoPhast.ModelSelection;
  Grid.BeginUpdate;
  try
//    if Ord(docVertex)+2 < Grid.ColCount then
//    begin
//      Grid.Columns[Ord(docVertex)+2].CheckMin := False;
//    end;
    if comboMethod.ItemIndex >= 0 then
    begin
      DiversionChoice := TReturnChoice(comboMethod.ItemIndex+1);
      case DiversionChoice of
        rtObject:
          begin
            if ModelSelection = msModflowFMP then
            begin
              Grid.ColWidths[Ord(docObject)+2] := RequiredWidthForObjectOption;
            end;

            if ModelSelection = msModflowFmp then
            begin
              Grid.ColCount := Ord(docVertex)+3;
            end
            else
            begin
              Grid.ColCount := Ord(docUpperLimit)+3;
            end;
            Grid.Columns[Ord(docVertex)+2].WordWrapCaptions := True;

            if ModelSelection = msModflowFmp then
            begin
              Grid.Cells[Ord(docObject)+2, 0] := StrObjectOrOptionName;
            end
            else
            begin
              Grid.Cells[Ord(docObject)+2, 0] := StrObjectName;
            end;
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

            if frmGoPhast.ModelSelection = msModflowOwhm2 then
            begin
              for ColIndex := Ord(docFraction)+2 to Ord(docUpperLimit)+2 do
              begin
                if ColIndex < Grid.ColCount then
                begin
                  Grid.Columns[ColIndex].Format := rcf4String;
                end;
              end;
              Grid.Cells[Ord(docFraction)+2, 0] := StrFraction;
              Grid.Cells[Ord(docLowerLimit)+2, 0] := StrLowerLimit;
              Grid.Cells[Ord(docUpperLimit)+2, 0] := StrUpperLimit;
            end;
            FLowerLimitCol := Ord(docLowerLimit)+2;
            FUpperLimitCol := Ord(docUpperLimit)+2;
          end;
        rtLocation:
          begin
            if ModelSelection = msModflowFmp then
            begin
              Grid.ColCount := Ord(dlcY)+3;
            end
            else
            begin
              Grid.ColCount := Ord(dlUpperLimit)+3;
            end;
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
            if frmGoPhast.ModelSelection = msModflowOwhm2 then
            begin
              for ColIndex := Ord(dlFraction)+2 to Ord(dlUpperLimit)+2 do
              begin
                if ColIndex < Grid.ColCount then
                begin
                  Grid.Columns[ColIndex].Format := rcf4String;
                end;
              end;
              Grid.Cells[Ord(dlFraction)+2, 0] := StrFraction;
              Grid.Cells[Ord(dlLowerLimit)+2, 0] := StrLowerLimit;
              Grid.Cells[Ord(dlUpperLimit)+2, 0] := StrUpperLimit;
            end;
            FLowerLimitCol := Ord(dlLowerLimit)+2;
            FUpperLimitCol := Ord(dlUpperLimit)+2;
          end;
        rtCell:
          begin
            if ModelSelection = msModflowFmp then
            begin
              Grid.ColCount := Ord(dccColumn)+3;
            end
            else
            begin
              Grid.ColCount := Ord(dccUpperLimit)+3;
            end;
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
            if frmGoPhast.ModelSelection = msModflowOwhm2 then
            begin
              for ColIndex := Ord(dccFraction)+2 to Ord(dccUpperLimit)+2 do
              begin
                if ColIndex < Grid.ColCount then
                begin
                  Grid.Columns[ColIndex].Format := rcf4String;
                end;
              end;
              Grid.Cells[Ord(dccFraction)+2, 0] := StrFraction;
              Grid.Cells[Ord(dccLowerLimit)+2, 0] := StrLowerLimit;
              Grid.Cells[Ord(dccUpperLimit)+2, 0] := StrUpperLimit;
            end;
            FLowerLimitCol := Ord(dccLowerLimit)+2;
            FUpperLimitCol := Ord(dccUpperLimit)+2;
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
  DelivReturns: TSemiRoutedDeliveriesAndReturnFlowCollection;
  AFarm: TFarm;
  SrList: TSrCollList;
  Index: Integer;
begin
  SrList := TSrCollList.Create;
  try
    for Index := 0 to FarmList.Count - 1 do
    begin
      AFarm := FarmList[Index];
      DelivReturns := nil;
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
      SrList.Add(DelivReturns);
    end;
    GetDataFromListOfSemiRoutedLists(SrList, DiversionType);
  finally
    SrList.Free;
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
        CanSelect := (Grid.Objects[Ord(docObject)+2,ARow] <> nil)
          and (Grid.ItemIndex[Ord(docChoice)+2,ARow] = Ord(dpMiddle));
      end
      else if (ACol = Ord(docChoice)+2) then
      begin
        CanSelect := (Grid.Objects[Ord(docObject)+2,ARow] <> nil);
      end;
    end;
  end;
  if (ARow >= 1) then
  begin
    if ACol = FLowerLimitCol then
    begin
      CanSelect := FLowerLimitUsed;
    end;
    if ACol = FUpperLimitCol then
    begin
      CanSelect := FUpperLimitUsed;
    end;
  end;
end;

procedure TframeFarmDiversion.GridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  ItemIndex: Integer;
begin
  inherited;
  if (ARow >= 1) and (comboMethod.ItemIndex = Ord(rtObject)-1)
    and (ACol - 2 = Ord(docObject)) then
  begin
    ItemIndex  := Grid.ItemIndex[ACol, ARow];
    if ItemIndex >= 0 then
    begin
      Grid.Objects[ACol, ARow] := comboSfrObjects.Items.Objects[ItemIndex];
    end
    else
    begin
      Grid.Objects[ACol, ARow] := nil;
    end;
  end;
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

procedure TframeFarmDiversion.SetDataForListOfSemiRoutedLists(SrList: TSrCollList;
  DiversionType: TDiversionType);
const
  PropertyColOffset = 2;
var
  ModelSelection: TModelSelection;
  index: Integer;
  Count: Integer;
  TimeIndex: Integer;
  StartTime: Double;
  EndTime: Double;
  DelivRetItem: TSemiRoutedDeliveriesAndRunoffItem;
  LinkedStream: TSfrDiversion;
  DiversionObject: TSfrDiversionObject;
  GridItemIndex: Integer;
  DiversionLocation: TReturnLocation;
  DiversionCell: TReturnCell;
  DelivReturn: TSemiRoutedDeliveriesAndReturnFlowCollection;
  Fraction: string;
  ItemIndex: Integer;
  ARow: Integer;
begin
  ModelSelection := frmGoPhast.ModelSelection;
  for index := 0 to SrList.Count - 1 do
  begin
    DelivReturn := SrList[index];
    Count := 0;
    for TimeIndex := 1 to seNumber.AsInteger do
    begin
      ARow := TimeIndex+PestRowOffset;
      if TryStrToFloat(Grid.Cells[Ord(dtcStart), ARow], StartTime)
        and TryStrToFloat(Grid.Cells[Ord(dtcEnd), ARow], EndTime) then
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
        LinkedStream.DiversionChoice := TReturnChoice(comboMethod.ItemIndex + 1);
        case LinkedStream.DiversionChoice of
          rtObject:
            begin
              DiversionObject := LinkedStream.DiversionObject;
              GridItemIndex := Grid.ItemIndex[Ord(docObject) + PropertyColOffset, ARow];
              if GridItemIndex >= 0 then
              begin
                DiversionObject.ScreenObject :=
                  Grid.Columns[Ord(docObject) + PropertyColOffset].PickList.Objects[GridItemIndex];
              end
              else
              begin
                DiversionObject.ScreenObject := nil;
              end;
              ItemIndex := Grid.ItemIndex[Ord(docChoice) + PropertyColOffset, ARow];
              if ItemIndex >= 0 then
              begin
                DiversionObject.DiversionPosition :=
                  TDiversionPosition(ItemIndex);
              end;
              if DiversionObject.DiversionPosition = dpMiddle then
              begin
                DiversionObject.DiversionVertex :=
                  StrToIntDef(Grid.Cells[Ord(docVertex) + PropertyColOffset, ARow], 1);
              end;
              if ModelSelection = msModflowOwhm2 then
              begin
                DelivRetItem.Frac := Grid.Cells[Ord(docFraction) + PropertyColOffset, ARow];
                if DiversionType = dtDiversion then
                begin
                  DelivRetItem.LowerLimit := Grid.Cells[Ord(docLowerLimit) + PropertyColOffset, ARow];
                  DelivRetItem.UpperLimit := Grid.Cells[Ord(docUpperLimit) + PropertyColOffset, ARow];
                end;
              end;
            end;
          rtLocation:
            begin
              DiversionLocation := LinkedStream.DiversionLocation;
              DiversionLocation.X := StrToFloatDef(Grid.Cells[Ord(dlcX) + PropertyColOffset, ARow], 0);
              DiversionLocation.Y := StrToFloatDef(Grid.Cells[Ord(dlcY) + PropertyColOffset, ARow], 0);
              DiversionLocation.Z := 0;
              if ModelSelection = msModflowOwhm2 then
              begin
                DelivRetItem.Frac := Grid.Cells[Ord(dlFraction) + 2, ARow];
                if DelivRetItem.Frac = '' then
                begin
                  DelivRetItem.Frac := '1';
                end;
                if DiversionType = dtDiversion then
                begin
                  DelivRetItem.LowerLimit := Grid.Cells[Ord(dlLowerLimit) + PropertyColOffset, ARow];
                  DelivRetItem.UpperLimit := Grid.Cells[Ord(dlUpperLimit) + PropertyColOffset, ARow];
                end;
              end;
            end;
          rtCell:
            begin
              DiversionCell := LinkedStream.DiversionCell;
              DiversionCell.Row := StrToIntDef(Grid.Cells[Ord(dccRow) + PropertyColOffset, ARow], 1);
              DiversionCell.Col := StrToIntDef(Grid.Cells[Ord(dccColumn) + PropertyColOffset, ARow], 1);
              DiversionCell.Lay := 0;
              if ModelSelection = msModflowOwhm2 then
              begin
                Fraction := Grid.Cells[Ord(dccFraction) + PropertyColOffset, ARow];
                if Fraction = '' then
                begin
                  Fraction := '1';
                end;
                DelivRetItem.Frac := Fraction;
                if DiversionType = dtDiversion then
                begin
                  DelivRetItem.LowerLimit := Grid.Cells[Ord(dccLowerLimit) + PropertyColOffset, ARow];
                  DelivRetItem.UpperLimit := Grid.Cells[Ord(dccUpperLimit) + PropertyColOffset, ARow];
                end;
              end;
            end;
        else
          Assert(False);
        end;
      end;
    end;
    DelivReturn := SrList[index];
    if DelivReturn.Count > 0 then
    begin


      DelivRetItem := DelivReturn[0];
      LinkedStream := DelivRetItem.LinkedStream;
      case LinkedStream.DiversionChoice of
        rtNone: ;
        rtObject:
          begin
            if PestModifierAssigned[Ord(docFraction)+PropertyColOffset] then
            begin
              DelivReturn.PestSeriesParameter := PestModifier[Ord(docFraction)+PropertyColOffset];
            end;
            if PestMethodAssigned[Ord(docFraction)+PropertyColOffset]  then
            begin
              DelivReturn.PestParamMethod := PestMethod[Ord(docFraction)+PropertyColOffset];
            end;
            if Ord(docLowerLimit)+PropertyColOffset < Grid.ColCount then
            begin
              if PestModifierAssigned[Ord(docLowerLimit)+PropertyColOffset] then
              begin
                DelivReturn.LowerLimitPestSeriesParameter := PestModifier[Ord(docLowerLimit)+PropertyColOffset];
              end;
              if PestMethodAssigned[Ord(docLowerLimit)+PropertyColOffset] then
              begin
                DelivReturn.LowerLimitPestParamMethod := PestMethod[Ord(docLowerLimit)+PropertyColOffset];
              end;
            end;
            if Ord(docUpperLimit)+PropertyColOffset < Grid.ColCount then
            begin
              if PestModifierAssigned[Ord(docUpperLimit)+PropertyColOffset] then
              begin
                DelivReturn.UpperLimitPestSeriesParameter := PestModifier[Ord(docUpperLimit)+PropertyColOffset];
              end;
              if PestMethodAssigned[Ord(docUpperLimit)+PropertyColOffset] then
              begin
                DelivReturn.UpperLimitPestParamMethod := PestMethod[Ord(docUpperLimit)+PropertyColOffset];
              end;
            end;
          end;
        rtLocation:
          begin
            if PestModifierAssigned[Ord(dlFraction)+PropertyColOffset] then
            begin
              DelivReturn.PestSeriesParameter := PestModifier[Ord(dlFraction)+PropertyColOffset];
            end;
            if PestMethodAssigned[Ord(dlFraction)+PropertyColOffset] then
            begin
              DelivReturn.PestParamMethod := PestMethod[Ord(dlFraction)+PropertyColOffset];
            end;
            if Ord(dlLowerLimit)+PropertyColOffset < Grid.ColCount then
            begin
              if PestModifierAssigned[Ord(dlLowerLimit)+PropertyColOffset] then
              begin
                DelivReturn.LowerLimitPestSeriesParameter := PestModifier[Ord(dlLowerLimit)+PropertyColOffset];
              end;
              if PestMethodAssigned[Ord(dlLowerLimit)+PropertyColOffset] then
              begin
                DelivReturn.LowerLimitPestParamMethod := PestMethod[Ord(dlLowerLimit)+PropertyColOffset];
              end;
            end;
            if Ord(dlUpperLimit)+PropertyColOffset < Grid.ColCount then
            begin
              if PestModifierAssigned[Ord(dlUpperLimit)+PropertyColOffset] then
              begin
                DelivReturn.UpperLimitPestSeriesParameter := PestModifier[Ord(dlUpperLimit)+PropertyColOffset];
              end;
              if PestMethodAssigned[Ord(dlUpperLimit)+PropertyColOffset] then
              begin
                DelivReturn.UpperLimitPestParamMethod := PestMethod[Ord(dlUpperLimit)+PropertyColOffset];
              end;
            end;
          end;
        rtCell:
          begin
            if PestModifierAssigned[Ord(dccFraction)+PropertyColOffset] then
            begin
              DelivReturn.PestSeriesParameter := PestModifier[Ord(dccFraction)+PropertyColOffset];
            end;
            if PestMethodAssigned[Ord(dccFraction)+PropertyColOffset] then
            begin
              DelivReturn.PestParamMethod := PestMethod[Ord(dccFraction)+PropertyColOffset];
            end;
            if Ord(dccLowerLimit)+PropertyColOffset < Grid.ColCount then
            begin
              if PestModifierAssigned[Ord(dccLowerLimit)+PropertyColOffset] then
              begin
                DelivReturn.LowerLimitPestSeriesParameter := PestModifier[Ord(dccLowerLimit)+PropertyColOffset];
              end;
              if PestMethodAssigned[Ord(dccLowerLimit)+PropertyColOffset] then
              begin
                DelivReturn.LowerLimitPestParamMethod := PestMethod[Ord(dccLowerLimit)+PropertyColOffset];
              end;
            end;
            if Ord(dccUpperLimit)+PropertyColOffset < Grid.ColCount then
            begin
              if PestModifierAssigned[Ord(dccUpperLimit)+PropertyColOffset] then
              begin
                DelivReturn.UpperLimitPestSeriesParameter := PestModifier[Ord(dccUpperLimit)+PropertyColOffset];
              end;
              if PestMethodAssigned[Ord(dccUpperLimit)+PropertyColOffset] then
              begin
                DelivReturn.UpperLimitPestParamMethod := PestMethod[Ord(dccUpperLimit)+PropertyColOffset];
              end;
            end;
          end;
      else
        Assert(False);
      end;

    end;
  end;
end;

procedure TframeFarmDiversion.GetDataFromListOfSemiRoutedLists(
  SrList: TSrCollList; DiversionType: TDiversionType);
const
  PropertyColOffset = 2;
var
  ModelSelection: TModelSelection;
  Packages: TModflowPackages;
  FirstDelivReturns: TSemiRoutedDeliveriesAndReturnFlowCollection;
  AnItem: TSemiRoutedDeliveriesAndRunoffItem;
  ItemIndex: Integer;
  LinkedStream: TSfrDiversion;
  DiversionObject: TSfrDiversionObject;
  DiversionLocation: TReturnLocation;
  DiversionCell: TReturnCell;
  ObjectIndex: Integer;
  DelivReturns: TSemiRoutedDeliveriesAndReturnFlowCollection;
begin
  ModelSelection := frmGoPhast.ModelSelection;
  Packages := frmGoPhast.PhastModel.ModflowPackages;
  FLowerLimitUsed := Packages.FarmSurfaceWater4.SemiRoutedDeliveryLowerLimit.FarmOption <> foNotUsed;
  FUpperLimitUsed := Packages.FarmSurfaceWater4.SemiRoutedDeliveryUpperLimit.FarmOption <> foNotUsed;
  Assert(SrList.Count > 0);
  Changing := True;
  Grid.BeginUpdate;
  try
    ClearGrid;
    DelivReturns := SrList[0];
    FirstDelivReturns := DelivReturns;
    if DelivReturns.Count > 0 then
    begin
      seNumber.AsInteger := DelivReturns.Count;
      seNumber.OnChange(seNumber);
      AnItem := DelivReturns[0];
      LinkedStream := AnItem.LinkedStream;
      case LinkedStream.DiversionChoice of
        rtNone: ;
        rtObject:
          begin
            PestModifier[Ord(docFraction)+PropertyColOffset] := DelivReturns.PestSeriesParameter;
            PestMethod[Ord(docFraction)+PropertyColOffset] := DelivReturns.PestParamMethod;
            if Ord(docLowerLimit)+PropertyColOffset < Grid.ColCount then
            begin
              PestModifier[Ord(docLowerLimit)+PropertyColOffset] := DelivReturns.LowerLimitPestSeriesParameter;
              PestMethod[Ord(docLowerLimit)+PropertyColOffset] := DelivReturns.LowerLimitPestParamMethod;
            end;
            if Ord(docUpperLimit)+PropertyColOffset < Grid.ColCount then
            begin
              PestModifier[Ord(docUpperLimit)+PropertyColOffset] := DelivReturns.UpperLimitPestSeriesParameter;
              PestMethod[Ord(docUpperLimit)+PropertyColOffset] := DelivReturns.UpperLimitPestParamMethod;
            end;
          end;
        rtLocation:
          begin
            PestModifier[Ord(dlFraction)+PropertyColOffset] := DelivReturns.PestSeriesParameter;
            PestMethod[Ord(dlFraction)+PropertyColOffset] := DelivReturns.PestParamMethod;
            if Ord(dlLowerLimit)+PropertyColOffset < Grid.ColCount then
            begin
              PestModifier[Ord(dlLowerLimit)+PropertyColOffset] := DelivReturns.LowerLimitPestSeriesParameter;
              PestMethod[Ord(dlLowerLimit)+PropertyColOffset] := DelivReturns.LowerLimitPestParamMethod;
            end;
            if Ord(dlUpperLimit)+PropertyColOffset < Grid.ColCount then
            begin
              PestModifier[Ord(dlUpperLimit)+PropertyColOffset] := DelivReturns.UpperLimitPestSeriesParameter;
              PestMethod[Ord(dlUpperLimit)+PropertyColOffset] := DelivReturns.UpperLimitPestParamMethod;
            end;
          end;
        rtCell:
          begin
            PestModifier[Ord(dccFraction)+PropertyColOffset] := DelivReturns.PestSeriesParameter;
            PestMethod[Ord(dccFraction)+PropertyColOffset] := DelivReturns.PestParamMethod;
            if Ord(dccLowerLimit)+PropertyColOffset < Grid.ColCount then
            begin
              PestModifier[Ord(dccLowerLimit)+PropertyColOffset] := DelivReturns.LowerLimitPestSeriesParameter;
              PestMethod[Ord(dccLowerLimit)+PropertyColOffset] := DelivReturns.LowerLimitPestParamMethod;
            end;
            if Ord(dccUpperLimit)+PropertyColOffset < Grid.ColCount then
            begin
              PestModifier[Ord(dccUpperLimit)+PropertyColOffset] := DelivReturns.UpperLimitPestSeriesParameter;
              PestMethod[Ord(dccUpperLimit)+PropertyColOffset] := DelivReturns.UpperLimitPestParamMethod;
            end;
          end;
      else
        Assert(False);
      end;

      comboMethod.ItemIndex := Ord(AnItem.LinkedStream.DiversionChoice) - 1;
      for ItemIndex := 0 to DelivReturns.Count - 1 do
      begin
        AnItem := DelivReturns[ItemIndex];
        LinkedStream := AnItem.LinkedStream;
        Grid.Cells[Ord(dtcStart), ItemIndex + 1+PestRowOffset] := FloatToStr(AnItem.StartTime);
        Grid.Cells[Ord(dtcEnd), ItemIndex + 1+PestRowOffset] := FloatToStr(AnItem.EndTime);
        case LinkedStream.DiversionChoice of
          rtObject:
            begin
              DiversionObject := LinkedStream.DiversionObject;
              if DiversionObject.ScreenObject = nil then
              begin
                Grid.ItemIndex[Ord(docObject) + PropertyColOffset, ItemIndex + 1+PestRowOffset] := 0;
              end
              else
              begin
                Grid.Cells[Ord(docObject) + PropertyColOffset, ItemIndex + 1+PestRowOffset] := DiversionObject.ObjectName;
              end;
              Grid.Objects[Ord(docObject) + PropertyColOffset, ItemIndex + 1+PestRowOffset] := DiversionObject.ScreenObject;
              Grid.ItemIndex[Ord(docChoice) + PropertyColOffset, ItemIndex + 1+PestRowOffset] := Ord(DiversionObject.DiversionPosition);
              if DiversionObject.DiversionPosition = dpMiddle then
              begin
                Grid.Cells[Ord(docVertex) + PropertyColOffset, ItemIndex + 1+PestRowOffset] := IntToStr(DiversionObject.DiversionVertex);
              end
              else
              begin
                Grid.Cells[Ord(docVertex) + PropertyColOffset, ItemIndex + 1+PestRowOffset] := '';
              end;
              if ModelSelection = msModflowOwhm2 then
              begin
                Grid.Cells[Ord(docFraction) + PropertyColOffset, ItemIndex + 1+PestRowOffset] := AnItem.Frac;
                PestUsedOnCol[Ord(docFraction) + PropertyColOffset] := True;
                if DiversionType = dtDiversion then
                begin
                  Grid.Cells[Ord(docLowerLimit) + PropertyColOffset, ItemIndex + 1+PestRowOffset] := AnItem.LowerLimit;
                  Grid.Cells[Ord(docUpperLimit) + PropertyColOffset, ItemIndex + 1+PestRowOffset] := AnItem.UpperLimit;
                  PestUsedOnCol[Ord(docLowerLimit) + PropertyColOffset] := True;
                  PestUsedOnCol[Ord(docUpperLimit) + PropertyColOffset] := True;
                end;
              end;
            end;
          rtLocation:
            begin
              DiversionLocation := LinkedStream.DiversionLocation;
              Grid.Cells[Ord(dlcX) + PropertyColOffset, ItemIndex + 1+PestRowOffset] := FloatToStr(DiversionLocation.X);
              Grid.Cells[Ord(dlcY) + PropertyColOffset, ItemIndex + 1+PestRowOffset] := FloatToStr(DiversionLocation.Y);
              if ModelSelection = msModflowOwhm2 then
              begin
                Grid.Cells[Ord(dlFraction) + PropertyColOffset, ItemIndex + 1+PestRowOffset] := AnItem.Frac;
                PestUsedOnCol[Ord(dlFraction) + PropertyColOffset] := True;
                if DiversionType = dtDiversion then
                begin
                  Grid.Cells[Ord(dlLowerLimit) + PropertyColOffset, ItemIndex + 1+PestRowOffset] := AnItem.LowerLimit;
                  Grid.Cells[Ord(dlUpperLimit) + PropertyColOffset, ItemIndex + 1+PestRowOffset] := AnItem.UpperLimit;
                  PestUsedOnCol[Ord(dlLowerLimit) + PropertyColOffset] := True;
                  PestUsedOnCol[Ord(dlUpperLimit) + PropertyColOffset] := True;
                end;
              end;
            end;
          rtCell:
            begin
              DiversionCell := LinkedStream.DiversionCell;
              Grid.Cells[Ord(dccRow) + PropertyColOffset, ItemIndex + 1+PestRowOffset] := IntToStr(DiversionCell.Row);
              Grid.Cells[Ord(dccColumn) + PropertyColOffset, ItemIndex + 1+PestRowOffset] := IntToStr(DiversionCell.Col);
              if ModelSelection = msModflowOwhm2 then
              begin
                Grid.Cells[Ord(dccFraction) + PropertyColOffset, ItemIndex + 1+PestRowOffset] := AnItem.Frac;
                PestUsedOnCol[Ord(dccFraction) + PropertyColOffset] := True;
                if DiversionType = dtDiversion then
                begin
                  Grid.Cells[Ord(dccLowerLimit) + PropertyColOffset, ItemIndex + 1+PestRowOffset] := AnItem.LowerLimit;
                  Grid.Cells[Ord(dccUpperLimit) + PropertyColOffset, ItemIndex + 1+PestRowOffset] := AnItem.UpperLimit;
                  PestUsedOnCol[Ord(dccLowerLimit) + PropertyColOffset] := True;
                  PestUsedOnCol[Ord(dccUpperLimit) + PropertyColOffset] := True;
                end;
              end;
            end;
        else
          Assert(False);
        end;
      end;
      for ObjectIndex := 1 to SrList.Count - 1 do
      begin
        DelivReturns := SrList[ObjectIndex];
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
  Farm: TFarm;
  DelivReturn: TSemiRoutedDeliveriesAndReturnFlowCollection;
  SrList: TSrCollList;
  FarmIndex: Integer;
begin
  SrList := TSrCollList.Create;
  try
    for FarmIndex := 0 to FarmList.Count - 1 do
    begin
      Farm := FarmList[FarmIndex];
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
        SrList.Add(DelivReturn);
      end;
    end;
    SetDataForListOfSemiRoutedLists(SrList, DiversionType);
  finally
    SrList.Free;
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
  LocalModel := frmGoPhast.PhastModel;

  IncludePestAdjustment := True;
  InitializePestParameters;
  seNumberChange(nil);

  comboSfrObjects.Items.BeginUpdate;
  try
    comboSfrObjects.Items.Clear;
    if LocalModel.SfrIsSelected or LocalModel.SwrIsSelected then
    begin
      if LocalModel.ModelSelection = msModflowFmp then
      begin
        comboSfrObjects.Items.Add('prorate over segments option');
      end;
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
    if frmGoPhast.ModelSelection = msModflowFmp then
    begin
      Grid.ColCount := 5;
    end
    else
    begin
      Grid.ColCount := 8;
    end;
    for ColIndex := 0 to Grid.ColCount - 1 do
    begin
      Grid.Columns[ColIndex].AutoAdjustColWidths := True;
      Grid.Columns[ColIndex].AutoAdjustRowHeights := True;
      Grid.Columns[ColIndex].WordWrapCaptions := True;
      PestUsedOnCol[ColIndex] := False;
    end;
    if LocalModel.ModelSelection = msModflowFmp then
    begin
      Grid.Columns[Ord(docObject)+2].WordWrapCaptions := False;
    end;

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

initialization

RegisterClass(TframeFarmDiversion);

end.
