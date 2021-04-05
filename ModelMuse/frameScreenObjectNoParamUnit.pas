{@name defines @link(TframeScreenObjectParam).}
unit frameScreenObjectNoParamUnit;

interface

uses
  System.UITypes, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, Dialogs, Grids, Math, RbwDataGrid4, JvExControls, JvComponent,
  JvxCheckListBox, ExtCtrls, Buttons, Mask, JvExMask, JvSpin, ArgusDataEntry,
  ModflowBoundaryUnit, frameScreenObjectUnit, Vcl.ComCtrls, GoPhastTypes;

type
  // See @link(TframeScreenObjectParam.UnselectableColumnsIfParametersUsed).
  TColumn = 0..255;
  // See @link(TframeScreenObjectParam.UnselectableColumnsIfParametersUsed).
  TColumns = set of TColumn;


  {
    @abstract(@name is used to define time-varying MODFLOW
    boundary condition data associated with @link(TScreenObject)s
    when parameter instances can NOT be defined.)
    Typically, the first two columns of @link(rdgModflowBoundary) will have
    the start and ending times for the boundary condtion.  Following that
    will be columns representing the values to be assigned the boundary.

    @member(rdgModflowBoundary @name is used to indicate the time-varying
      boundary condition values.)
    @member(pnlBottom @name holds buttons and controls that affect
      @link(rdgModflowBoundary).)
    @member(seNumberOfTimes @name specifies the number of times at which
      the parameter is specified and thus the number of rows in
      @link(rdgModflowBoundary).)
    @member(lblNumTimes @name labels @link(seNumberOfTimes).)
    @member(btnDelete @name deletes one of the times at which
      the parameter is specified.)
    @member(btnInsert @name inserts a new time at which
      the boundary condition is specified.)
    @member(rdgModflowBoundarySelectCell @name prevents the user from selecting
      certain cells.)
    @member(seNumberOfTimesChange @name changes the number of rows
      in @link(rdgModflowBoundary) and takes other related actions. @name is
      the OnChange event handler for @link(seNumberOfTimes).)
    @member(btnDeleteClick @name deletes the selected row in
      @link(rdgModflowBoundary).  @name is the OnClick eventhandler for
      @link(btnDelete).)
    @member(btnInsertClick @name inserts a new row beneath the selected row in
      @link(rdgModflowBoundary).  @name is the OnClick eventhandler for
      @link(btnInsert).)
  }
  TframeScreenObjectNoParam = class(TframeScreenObject)
    rdgModflowBoundary: TRbwDataGrid4;
    pnlBottom: TPanel;
    seNumberOfTimes: TJvSpinEdit;
    lblNumTimes: TLabel;
    btnDelete: TBitBtn;
    btnInsert: TBitBtn;
    pnlTop: TPanel;
    pnlCaption: TPanel;
    pnlGrid: TPanel;
    pnlEditGrid: TPanel;
    lblFormula: TLabel;
    rdeFormula: TRbwDataEntry;
    procedure rdgModflowBoundarySelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure seNumberOfTimesChange(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure rdgModflowBoundaryMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdeFormulaChange(Sender: TObject);
    procedure rdgModflowBoundaryHorizontalScroll(Sender: TObject);
    procedure rdgModflowBoundaryColSize(Sender: TObject; ACol,
      PriorWidth: Integer);
    procedure FrameResize(Sender: TObject);
    procedure rdgModflowBoundarySetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdgModflowBoundaryBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
  private
    FSelectedText: string;
    FDeleting: Boolean;
    FDeletedCells: array of array of boolean;
    FConductanceColumn: Integer;
    FOnCheckPestCell: TSelectCellEvent;
    // See @link(DeletedCells)
    function GetDeletedCells(ACol, ARow: integer): boolean;
    // See @link(DeletedCells)
    procedure SetDeletedCells(ACol, ARow: integer; const Value: boolean);
    function GetPestMethod(ACol: Integer): TPestParamMethod;
    procedure SetPestMethod(ACol: Integer; const Value: TPestParamMethod);
    function GetPestModifier(ACol: Integer): string;
    procedure SetPestModifier(ACol: Integer; const Value: string);
    function GetPestMethodAssigned(ACol: Integer): Boolean;
    function GetPestModifierAssigned(ACol: Integer): Boolean;
    procedure SetPestMethodAssigned(ACol: Integer; const Value: Boolean);
    procedure SetPestModifierAssigned(ACol: Integer; const Value: Boolean);
    { Private declarations }
  protected
    procedure LayoutMultiRowEditControls; virtual;
    function ShouldEnableMultisetControls: Boolean;
    procedure MoveGridToTabSheet(tab: TTabSheet);
  public
    property ConductanceColumn: Integer read FConductanceColumn
      write FConductanceColumn;
    procedure ClearDeletedCells;
    property DeletedCells[ACol, ARow: integer]: boolean read GetDeletedCells
      write SetDeletedCells;
    function ConductanceCaption(DirectCaption: string): string; virtual;
    procedure InitializeNoParamFrame(
      Boundary: TModflowBoundary);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // @name loads the starting times of all the MODFLOW stress periods into
    // the @link(TCustomRowOrColumn.PickList) of the
    // @link(TCustomRowOrColumn column) of @link(rdgModflowBoundary)
    // specified by Col.
    procedure GetStartTimes(Col: integer);
    // @name loads the ending times of all the MODFLOW stress periods into
    // the @link(TCustomRowOrColumn.PickList) of the
    // @link(TCustomRowOrColumn column) of @link(rdgModflowBoundary)
    // specified by Col.
    procedure GetEndTimes(Col: Integer);
    procedure SetButtonCaptions;
    property OnCheckPestCell: TSelectCellEvent read FOnCheckPestCell
      write FOnCheckPestCell;
    Property PestMethod[ACol: Integer]: TPestParamMethod
      read GetPestMethod write SetPestMethod;
    Property PestModifier[ACol: Integer]: string
      read GetPestModifier write SetPestModifier;
    Property PestMethodAssigned[ACol: Integer]: Boolean
      read GetPestMethodAssigned write SetPestMethodAssigned;
    Property PestModifierAssigned[ACol: Integer]: Boolean
      read GetPestModifierAssigned write SetPestModifierAssigned;
    { Public declarations }
  end;

implementation

uses OrderedCollectionUnit, frmGoPhastUnit, ModflowTimeUnit,
  frmCustomGoPhastUnit;

resourcestring
  StrF = 'F()';

var
  FPestMethods: TStringList;

{$R *.dfm}

{ TframeScreenObjectParam }

procedure TframeScreenObjectNoParam.ClearDeletedCells;
begin
  SetLength(FDeletedCells, 0, 0);
end;

function TframeScreenObjectNoParam.ConductanceCaption(DirectCaption: string): string;
begin
  result := DirectCaption;
end;

procedure TframeScreenObjectNoParam.InitializeNoParamFrame(
  Boundary: TModflowBoundary);
var
  Index: integer;
  TimeList: TModflowTimeList;
  GridRect: TGridRect;
  ColIndex: Integer;
  RowIndex: Integer;
begin
  seNumberOfTimes.AsInteger := 0;
  if Assigned(seNumberOfTimes.OnChange) then
  begin
    seNumberOfTimes.OnChange(seNumberOfTimes);
  end;
  for ColIndex := 0 to rdgModflowBoundary.ColCount - 1 do
  begin
    rdgModflowBoundary.Objects[ColIndex,0] := nil;
    rdgModflowBoundary.Columns[ColIndex].WordWrapCaptions := True;
    for RowIndex := 1 to rdgModflowBoundary.RowCount - 1 do
    begin
      rdgModflowBoundary.Cells[ColIndex,RowIndex] := '';
    end;
  end;
  rdgModflowBoundary.Columns[0].Format := rcf4Real;
  rdgModflowBoundary.Columns[1].Format := rcf4Real;
  rdgModflowBoundary.Columns[0].ComboUsed := true;
  rdgModflowBoundary.Columns[1].ComboUsed := true;
  for Index := FLastTimeColumn+1 to rdgModflowBoundary.ColCount - 1 do
  begin
    rdgModflowBoundary.Columns[Index].ButtonUsed := true;
  end;
  rdgModflowBoundary.Cells[0, 0] := StrStartingTime;
  rdgModflowBoundary.Cells[1, 0] := StrEndingTime;
  {$IFDEF PEST}
  rdgModflowBoundary.Cells[0, PestModifierRow] := StrPestModifier;
  rdgModflowBoundary.Cells[0, PestMethodRow] := StrModificationMethod;
  {$ENDIF}
  if Boundary <> nil then
  begin
    for Index := 0 to Boundary.Values.TimeListCount(frmGoPhast.PhastModel) - 1 do
    begin
      ColIndex := FLastTimeColumn+1+Index;
      TimeList := Boundary.Values.TimeLists[Index, frmGoPhast.PhastModel];
      if Index = ConductanceColumn then
      begin
        rdgModflowBoundary.Cells[ColIndex, 0] :=
          ConductanceCaption(TimeList.NonParamDescription);
      end
      else
      begin
        rdgModflowBoundary.Cells[ColIndex, 0] := TimeList.NonParamDescription;
      end;
      rdgModflowBoundary.Columns[ColIndex].AutoAdjustColWidths := False;
      rdgModflowBoundary.ColWidths[ColIndex] :=
        rdgModflowBoundary.WidthNeededToFitText(ColIndex,0);
    end;
  end;
  rdgModflowBoundary.HideEditor;
  GridRect.Left := 2;
  GridRect.Right := 2;
  GridRect.Top := 1 + PestRowOffset;
  GridRect.Bottom := 1 + PestRowOffset;
  rdgModflowBoundary.Selection := GridRect;
  rdgModflowBoundary.HideEditor;
  SetButtonCaptions;
end;

procedure TframeScreenObjectNoParam.btnInsertClick(Sender: TObject);
begin
  if (rdgModflowBoundary.SelectedRow <= 0 + PestRowOffset)
    or (rdgModflowBoundary.SelectedRow >= rdgModflowBoundary.RowCount) then
  begin
    Beep;
    MessageDlg(StrYouNeedToSelectA, mtInformation, [mbOK], 0);
    Exit;
  end;
  if (seNumberOfTimes.AsInteger > 0) then
  begin
    rdgModflowBoundary.InsertRow(rdgModflowBoundary.SelectedRow);
  end;
  seNumberOfTimes.AsInteger := seNumberOfTimes.AsInteger +1;
end;

procedure TframeScreenObjectNoParam.btnDeleteClick(Sender: TObject);
begin
  if (rdgModflowBoundary.RowCount > 2 + PestRowOffset)
    and (rdgModflowBoundary.Row > 0 + PestRowOffset) then
  begin
    rdgModflowBoundary.DeleteRow(rdgModflowBoundary.Row);
  end;
  seNumberOfTimes.AsInteger := seNumberOfTimes.AsInteger -1;
end;

constructor TframeScreenObjectNoParam.Create(AOwner: TComponent);
begin
  inherited;
  ConductanceColumn := -1;
end;

destructor TframeScreenObjectNoParam.Destroy;
begin
  inherited;
end;

procedure TframeScreenObjectNoParam.rdgModflowBoundaryBeforeDrawCell(
  Sender: TObject; ACol, ARow: Integer);
var
  EndTime: double;
  NextStartTime: double;
begin
  if (ACol = 1) and (ARow >= rdgModflowBoundary.FixedRows + PestRowOffset)
    and (ARow < rdgModflowBoundary.RowCount -1) then
  begin
    if TryStrToFloat(rdgModflowBoundary.Cells[ACol, ARow], EndTime)
      and TryStrToFloat(rdgModflowBoundary.Cells[0, ARow+1], NextStartTime) then
    begin
      if NextStartTime < EndTime then
      begin
        rdgModflowBoundary.Canvas.Brush.Color := clRed;
      end;
    end;
  end;
end;

procedure TframeScreenObjectNoParam.rdgModflowBoundaryColSize(Sender: TObject;
  ACol, PriorWidth: Integer);
begin
  LayoutMultiRowEditControls;
end;

procedure TframeScreenObjectNoParam.rdgModflowBoundaryHorizontalScroll(
  Sender: TObject);
begin
  LayoutMultiRowEditControls;
end;

procedure TframeScreenObjectNoParam.rdgModflowBoundaryMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  rdeFormula.Enabled := ShouldEnableMultisetControls;
end;

procedure TframeScreenObjectNoParam.rdgModflowBoundarySelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  if (ARow = rdgModflowBoundary.FixedRows + PestRowOffset)
    and (seNumberOfTimes.AsInteger = 0) then
  begin
    FSelectedText := rdgModflowBoundary.Cells[ACol, ARow];
    CanSelect := False;
    Exit;
  end;

  if Assigned(OnCheckPestCell) then
  begin
    OnCheckPestCell(Sender, ACol, ARow, CanSelect);
  end
  else
  begin
    if ARow <= PestRowOffset then
    begin
      CanSelect := False;
    end;
  end;
end;

procedure TframeScreenObjectNoParam.rdgModflowBoundarySetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  if FDeleting  then
  begin
    Exit;
  end;
  if seNumberOfTimes.AsInteger < rdgModflowBoundary.RowCount -1 - PestRowOffset then
  begin
    seNumberOfTimes.AsInteger := rdgModflowBoundary.RowCount -1 - PestRowOffset;
    seNumberOfTimes.OnChange(seNumberOfTimes);
  end;
  if FSelectedText <> Value then
  begin
    DeletedCells[ACol, ARow] := Value = '';
  end;

  UpdateNextTimeCell(rdgModflowBoundary, ACol, ARow);
end;

procedure TframeScreenObjectNoParam.FrameResize(Sender: TObject);
begin
  LayoutMultiRowEditControls;
end;

function TframeScreenObjectNoParam.GetDeletedCells(ACol,
  ARow: integer): boolean;
begin
  if (ACol < 0) or (ARow < 0) then
  begin
    result := False;
    Exit;
  end;
  if (Length(FDeletedCells) = 0) or (Length(FDeletedCells[0]) = 0) then
  begin
    result := False;
    Exit;
  end;
  if (ACol < Length(FDeletedCells))
    and (ARow < Length(FDeletedCells[0])) then
  begin
    result := FDeletedCells[ACol,ARow];
  end
  else
  begin
    result := False;
  end;
end;

procedure TframeScreenObjectNoParam.GetEndTimes(Col: integer);
begin
  frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithEndTimes(
    rdgModflowBoundary, Col);
end;

function TframeScreenObjectNoParam.GetPestMethod(
  ACol: Integer): TPestParamMethod;
var
  ItemIndex: Integer;
begin
  if PestRowOffset = 0 then
  begin
    result := ppmMultiply;
    Assert(False);
    Exit;
  end;
  ItemIndex := FPestMethods.IndexOf(
    rdgModflowBoundary.Cells[ACol,PestMethodRow]);
  if ItemIndex >= 0 then
  begin
    result := TPestParamMethod(ItemIndex);
  end
  else
  begin
    result := ppmMultiply;
  end;
end;

function TframeScreenObjectNoParam.GetPestMethodAssigned(
  ACol: Integer): Boolean;
begin
  result := inherited PestMethodAssigned[rdgModflowBoundary, ACol];
end;

function TframeScreenObjectNoParam.GetPestModifier(ACol: Integer): string;
begin
  if PestRowOffset = 0 then
  begin
    result := '';
    Assert(False);
    Exit;
  end;
  result := rdgModflowBoundary.Cells[ACol, PestModifierRow];
  if result = strNone then
  begin
    result := '';
  end;
end;

function TframeScreenObjectNoParam.GetPestModifierAssigned(
  ACol: Integer): Boolean;
begin
  result := inherited PestModifierAssigned[rdgModflowBoundary, ACol];
end;

//function TframeScreenObjectNoParam.GetPestMethod(ACol: Integer): TPestParamMethod;
//var
//  ItemIndex: Integer;
//begin
//  ItemIndex := FPestMethods.IndexOf(rdgModflowBoundary.Cells[ACol,PestMethodRow]);
//  if ItemIndex >= 0 then
//  begin
//    result := TPestParamMethod(ItemIndex);
//  end
//  else
//  begin
//    result := ppmMultiply;
//  end;
//end;

procedure TframeScreenObjectNoParam.LayoutMultiRowEditControls;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  LayoutControls(rdgModflowBoundary, rdeFormula, lblFormula,
    Max(FLastTimeColumn+1,rdgModflowBoundary.LeftCol));
end;

procedure TframeScreenObjectNoParam.MoveGridToTabSheet(tab: TTabSheet);
begin
  pnlBottom.Parent := tab;
  pnlGrid.Parent := tab;
  pnlGrid.Align := alClient;
end;

procedure TframeScreenObjectNoParam.GetStartTimes(Col: integer);
begin
  frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithStartTimes(
    rdgModflowBoundary, Col);
end;

procedure TframeScreenObjectNoParam.rdeFormulaChange(Sender: TObject);
var
  ColIndex: Integer;
  RowIndex: Integer;
  TempOptions: Vcl.Grids.TGridOptions;
begin
  rdgModflowBoundary.BeginUpdate;
  try
    for RowIndex := rdgModflowBoundary.FixedRows + PestRowOffset to
      rdgModflowBoundary.RowCount - 1 do
    begin
      for ColIndex := FLastTimeColumn+1 to rdgModflowBoundary.ColCount - 1 do
      begin
        if rdgModflowBoundary.IsSelectedCell(ColIndex, RowIndex) then
        begin
          rdgModflowBoundary.Cells[ColIndex, RowIndex] := rdeFormula.Text;
          if Assigned(rdgModflowBoundary.OnSetEditText) then
          begin
            rdgModflowBoundary.OnSetEditText(
              rdgModflowBoundary,ColIndex,RowIndex, rdeFormula.Text);
          end;
        end;
      end;
    end;
  finally
    rdgModflowBoundary.EndUpdate;
  end;
  TempOptions := rdgModflowBoundary.Options;
  try
    rdgModflowBoundary.Options := [goEditing, goAlwaysShowEditor];
    rdgModflowBoundary.UpdateEditor;
  finally
    rdgModflowBoundary.Options := TempOptions;
  end;
end;

procedure TframeScreenObjectNoParam.seNumberOfTimesChange(Sender: TObject);
begin
  FDeleting := True;
  try
    if not (csLoading in ComponentState) then
    begin
      if seNumberOfTimes.AsInteger = 0 then
      begin
        rdgModflowBoundary.RowCount := 2 + PestRowOffset;
      end
      else
      begin
        rdgModflowBoundary.RowCount := seNumberOfTimes.AsInteger + 1 + PestRowOffset;
      end;
    end;
    btnDelete.Enabled := seNumberOfTimes.AsInteger >= 1;
    rdgModflowBoundary.Invalidate;
  finally
    FDeleting := False;
  end;
end;

procedure TframeScreenObjectNoParam.SetDeletedCells(ACol, ARow: integer;
  const Value: boolean);
var
  OldColCount: integer;
  OldRowCount: integer;
  ColIndex: Integer;
  RowIndex: Integer;
begin
  if (ACol < 0) or (ARow < 0) or (ACol >= rdgModflowBoundary.ColCount)
    or (ARow >= rdgModflowBoundary.RowCount) then
  begin
    Exit;
  end;
  Assert(ACol >= 0);
  Assert(ARow >= 0);
  Assert(ACol < rdgModflowBoundary.ColCount);
  Assert(ARow < rdgModflowBoundary.RowCount);
  OldColCount := Length(FDeletedCells);
  if OldColCount = 0 then
  begin
    OldRowCount := 0;
  end
  else
  begin
    OldRowCount := Length(FDeletedCells[0])
  end;
  if (ACol >= OldColCount) or (ARow >= OldRowCount) then
  begin
    SetLength(FDeletedCells, rdgModflowBoundary.ColCount,
      rdgModflowBoundary.RowCount);
    for ColIndex := OldColCount to rdgModflowBoundary.ColCount - 1 do
    begin
      for RowIndex := 0 to rdgModflowBoundary.RowCount - 1 do
      begin
        FDeletedCells[ColIndex,RowIndex] := False;
      end;
    end;
    for ColIndex := 0 to OldColCount - 1 do
    begin
      for RowIndex := OldRowCount to rdgModflowBoundary.RowCount - 1 do
      begin
        FDeletedCells[ColIndex,RowIndex] := False;
      end;
    end;
  end;
  FDeletedCells[ACol, ARow] := Value;
end;

procedure TframeScreenObjectNoParam.SetPestMethod(ACol: Integer;
  const Value: TPestParamMethod);
begin
  if PestMethodRow = 0 then
  begin
    Exit;
  end;
  rdgModflowBoundary.Cells[ACol,PestMethodRow] := FPestMethods[Ord(Value)];
end;

procedure TframeScreenObjectNoParam.SetPestMethodAssigned(ACol: Integer;
  const Value: Boolean);
begin
  inherited PestMethodAssigned[rdgModflowBoundary, ACol] := Value;
end;

procedure TframeScreenObjectNoParam.SetPestModifier(ACol: Integer;
  const Value: string);
begin
  if PestRowOffset = 0 then
  begin
    Assert(False);
    Exit;
  end;
  if Value = '' then
  begin
    rdgModflowBoundary.Cells[ACol, PestModifierRow] := strNone;
  end
  else
  begin
    rdgModflowBoundary.Cells[ACol, PestModifierRow] := Value;
  end;
end;

procedure TframeScreenObjectNoParam.SetPestModifierAssigned(ACol: Integer;
  const Value: Boolean);
begin
  inherited PestModifierAssigned[rdgModflowBoundary, ACol] := Value;
end;

procedure TframeScreenObjectNoParam.SetButtonCaptions;
var
  Index: Integer;
begin
  for Index := 0 to rdgModflowBoundary.ColCount - 1 do
  begin
    if rdgModflowBoundary.Columns[Index].ButtonCaption = '...' then
    begin
      rdgModflowBoundary.Columns[Index].ButtonCaption := StrF;
      rdgModflowBoundary.Columns[Index].ButtonWidth := 35;
    end;
  end;
end;

function TframeScreenObjectNoParam.ShouldEnableMultisetControls: Boolean;
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  result := False;
  for RowIndex := rdgModflowBoundary.FixedRows + PestRowOffset to
    rdgModflowBoundary.RowCount - 1 do
  begin
    for ColIndex := FLastTimeColumn + 1 to rdgModflowBoundary.ColCount - 1 do
    begin
      result := rdgModflowBoundary.IsSelectedCell(ColIndex, RowIndex);
      if result then
      begin
        Exit;
      end;
    end;
  end;
end;

initialization
  FPestMethods := TStringList.Create;
  FPestMethods.Add(StrMultiply);
  FPestMethods.Add(StrAdd);

finalization
 FPestMethods.Free;

end.
