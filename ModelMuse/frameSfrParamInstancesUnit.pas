unit frameSfrParamInstancesUnit;

interface

uses System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, Buttons, Mask, JvExMask, JvSpin, Grids, RbwDataGrid4,
  ExtCtrls;

type
  TSfrInstanceColumn = (sicStartTime, sicEndTime, sicInstanceName);

  TframeSfrParamInstances = class(TFrame)
    pnlSfrInstancesBottom: TPanel;
    rdgSfrParamInstances: TRbwDataGrid4;
    pnlLabel: TPanel;
    seInstanceCount: TJvSpinEdit;
    lblInstanceCount: TLabel;
    btnInsertFlowTableRow: TBitBtn;
    btnDeleteFlowTableRow: TBitBtn;
    procedure seInstanceCountChange(Sender: TObject);
    procedure btnInsertFlowTableRowClick(Sender: TObject);
    procedure btnDeleteFlowTableRowClick(Sender: TObject);
    procedure rdgSfrParamInstancesExit(Sender: TObject);
    procedure rdgSfrParamInstancesSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure rdgSfrParamInstancesEndUpdate(Sender: TObject);
    procedure rdgSfrParamInstancesSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
  private
    ErrorFound: boolean;
    FFrameLoaded: boolean;
    procedure UpdateNextTimeCell(DataGrid: TRbwDataGrid4; ACol, ARow: Integer);
    procedure SetFrameLoaded(const Value: boolean);
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    property FrameLoaded: boolean read FFrameLoaded write SetFrameLoaded;
    { Public declarations }
  end;

implementation

uses
  frmCustomGoPhastUnit;

resourcestring
  StrStartTime = 'Start time';
  StrEndTime = 'End time';
  StrInstanceName = 'Instance Name';
  StrTheNamesOfParamet = 'The names of parameter instances must be unique. Y' +
  'ou should correct the name "';

{$R *.dfm}

procedure TframeSfrParamInstances.btnDeleteFlowTableRowClick(Sender: TObject);
begin
  if rdgSfrParamInstances.SelectedRow > 0 then
  begin
    if rdgSfrParamInstances.RowCount > 2 then
    begin
      rdgSfrParamInstances.DeleteRow(rdgSfrParamInstances.SelectedRow);
    end;
    seInstanceCount.AsInteger := seInstanceCount.AsInteger -1;
    seInstanceCount.OnChange(nil);
  end;
end;

procedure TframeSfrParamInstances.btnInsertFlowTableRowClick(Sender: TObject);
begin
  if rdgSfrParamInstances.SelectedRow > 0 then
  begin
    rdgSfrParamInstances.InsertRow(rdgSfrParamInstances.SelectedRow);
    seInstanceCount.AsInteger := seInstanceCount.AsInteger +1;
    seInstanceCount.OnChange(nil);
  end;
end;

constructor TframeSfrParamInstances.Create(AOwner: TComponent);
begin
  inherited;
  rdgSfrParamInstances.Cells[Ord(sicStartTime), 0] := StrStartTime;
  rdgSfrParamInstances.Cells[Ord(sicEndTime), 0] := StrEndTime;
  rdgSfrParamInstances.Cells[Ord(sicInstanceName), 0] := StrInstanceName;
end;

procedure TframeSfrParamInstances.rdgSfrParamInstancesEndUpdate(
  Sender: TObject);
begin
  if seInstanceCount <> nil then
  begin
    seInstanceCount.AsInteger := rdgSfrParamInstances.RowCount -1;
  end;
end;

procedure TframeSfrParamInstances.rdgSfrParamInstancesExit(Sender: TObject);
const
  ValidFirstChar = ['a'..'z', 'A'..'Z', '_'];
  ValidChar = ['a'..'z', 'A'..'Z', '_', '0'..'9'];
var
  Index: Integer;
  AName: string;
  CharIndex: Integer;
  InstanceNames: TStringList;
  NewSelection: TGridRect;
begin
  if ErrorFound then
  begin
    // prevent infinite recursion.
    ErrorFound := False;
    Exit;
  end;
  // Ensure validity of instance names.
  for Index := 1 to rdgSfrParamInstances.RowCount - 1 do
  begin
    AName := rdgSfrParamInstances.Cells[Ord(sicInstanceName), Index];
    AName := Trim(AName);
    if Length(AName) > 0 then
    begin
      if not CharInSet(AName[1], ValidFirstChar) then
      begin
        AName[1] := '_'
      end;
      for CharIndex := 1 to Length(AName) do
      begin
        if not CharInSet(AName[CharIndex], ValidChar) then
        begin
          AName[CharIndex] := '_'
        end;
      end;
    end
    else
    begin
      AName := 'SP' + IntToStr(Index);
    end;
    rdgSfrParamInstances.Cells[Ord(sicInstanceName), Index] := AName;
  end;
  InstanceNames := TStringList.Create;
  try
    InstanceNames.Assign(rdgSfrParamInstances.Cols[Ord(sicInstanceName)]);
    InstanceNames.Delete(0);
    for Index := 0 to InstanceNames.Count - 1 do
    begin
      InstanceNames[Index] := UpperCase(InstanceNames[Index]);
    end;
    for Index := 0 to InstanceNames.Count - 1 do
    begin
      AName := InstanceNames[Index];
      if (AName <> '') and (InstanceNames.IndexOf(AName) <> Index) then
      begin
        NewSelection.Left := Ord(sicInstanceName);
        NewSelection.Right := NewSelection.Left;
        NewSelection.Top := Index+1;
        NewSelection.Bottom := NewSelection.Top;
        rdgSfrParamInstances.Selection := NewSelection;
        AName := rdgSfrParamInstances.Cells[Ord(sicInstanceName), Index+1];
        Beep;
        MessageDlg(StrTheNamesOfParamet + AName + '."', mtError, [mbOK], 0);
        ErrorFound := True;
        rdgSfrParamInstances.SetFocus;
        Exit;
      end;
    end;
  finally
    InstanceNames.Free;
  end;
end;

procedure TframeSfrParamInstances.rdgSfrParamInstancesSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  CanSelect := seInstanceCount.AsInteger > 0;
end;

procedure TframeSfrParamInstances.rdgSfrParamInstancesSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
var
  NewValue: AnsiString;
begin
  if (ARow > 0) and (ACol = Ord(sicInstanceName)) then
  begin
    NewValue := AnsiString(Value);
    if string(NewValue) <> Value then
    begin
      rdgSfrParamInstances.Cells[ACol, ARow] := string(NewValue);
    end;
  end;
  UpdateNextTimeCell(rdgSfrParamInstances, ACol, ARow);
end;

procedure TframeSfrParamInstances.seInstanceCountChange(Sender: TObject);
var
  ColIndex: Integer;
begin
  if seInstanceCount.AsInteger = 0 then
  begin
    rdgSfrParamInstances.RowCount := 2;
    for ColIndex := 0 to rdgSfrParamInstances.ColCount - 1 do
    begin
      rdgSfrParamInstances.Cells[ColIndex,1] := '';
    end;
  end
  else
  begin
    rdgSfrParamInstances.RowCount := seInstanceCount.AsInteger + 1;
  end;
  btnDeleteFlowTableRow.Enabled := seInstanceCount.AsInteger > 0;
end;

procedure TframeSfrParamInstances.SetFrameLoaded(const Value: boolean);
begin
  FFrameLoaded := Value;
end;

procedure TframeSfrParamInstances.UpdateNextTimeCell(DataGrid: TRbwDataGrid4;
  ACol, ARow: Integer);
var
  SelectIndex: Integer;
begin
  if FrameLoaded then
  begin
    frmCustomGoPhastUnit.UpdateNextTimeCell(DataGrid, ACol, ARow);
  end;
//  if FrameLoaded and (ARow >= DataGrid.FixedRows) and (ACol in [0, 1])
//    {and (FLastTimeColumn = 1)} then
//  begin
//    SelectIndex := DataGrid.ItemIndex[ACol, ARow];
//    if SelectIndex >= 0 then
//    begin
//      if (ACol = 0) then
//      begin
//        if DataGrid.Cells[1, ARow] = '' then
//        begin
//          DataGrid.ItemIndex[1, ARow] := SelectIndex;
//        end;
//      end
//      else if (ACol = 1) then
//      begin
//        if (ARow + 1 < DataGrid.RowCount) and
//          (DataGrid.Cells[0, ARow + 1] = '') then
//        begin
//          if SelectIndex + 1 < DataGrid.Columns[0].PickList.Count then
//          begin
//            DataGrid.ItemIndex[0, ARow + 1] := SelectIndex + 1;
//          end;
//        end;
//      end;
//    end;
//  end;
end;

end.
