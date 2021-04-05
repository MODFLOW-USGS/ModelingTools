unit frameScreenObjectTabbedUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectUnit, Vcl.Grids,
  RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, Vcl.ComCtrls, GoPhastTypes;

type
  TframeScreenObjectTabbed = class(TframeScreenObject)
    pcMain: TPageControl;
    tabTransient: TTabSheet;
    pnlBottom: TPanel;
    lblNumTimes: TLabel;
    seNumberOfTimes: TJvSpinEdit;
    btnDelete: TBitBtn;
    btnInsert: TBitBtn;
    pnlGrid: TPanel;
    pnlEditGrid: TPanel;
    lblFormula: TLabel;
    rdeFormula: TRbwDataEntry;
    rdgModflowBoundary: TRbwDataGrid4;
    pnlCaption: TPanel;
    procedure seNumberOfTimesChange(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure rdeFormulaChange(Sender: TObject);
    procedure rdgModflowBoundaryBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure rdgModflowBoundaryColSize(Sender: TObject; ACol,
      PriorWidth: Integer);
    procedure rdgModflowBoundarySetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure rdgModflowBoundaryMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdgModflowBoundarySelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  private
    FOnEdited: TNotifyEvent;
    FOnCheckPestCell: TSelectCellEvent;
    FSelectedText: string;
    procedure ClearSelectedRow;
    procedure UpdateNumTimes;
    procedure UpdateTransientEditor;
    function GetPestMethod(ACol: Integer): TPestParamMethod;
    function GetPestMethodAssigned(ACol: Integer): Boolean;
    function GetPestModifier(ACol: Integer): string;
    function GetPestModifierAssigned(ACol: Integer): Boolean;
    procedure SetPestMethod(ACol: Integer; const Value: TPestParamMethod);
    procedure SetPestMethodAssigned(ACol: Integer; const Value: Boolean);
    procedure SetPestModifier(ACol: Integer; const Value: string);
    procedure SetPestModifierAssigned(ACol: Integer; const Value: Boolean);
    { Private declarations }
  protected
    FGettingData: Boolean;
    function CanEdit(Sender: TObject; ACol, ARow: Integer): Boolean; virtual;
    procedure CanSelectTimeCell(ARow: Integer; ACol: Integer; var CanSelect: Boolean); virtual;
    procedure LayoutMultiRowEditControls; virtual;
    procedure FillPicklistsWithStartTimes;
    procedure Edited;
  public
    constructor Create(AOwner: TComponent); override;
    property OnEdited: TNotifyEvent read FOnEdited write FOnEdited;
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

var
  frameScreenObjectTabbed: TframeScreenObjectTabbed;

implementation

uses
  System.Math, frmCustomGoPhastUnit, frmGoPhastUnit;

{$R *.dfm}

procedure TframeScreenObjectTabbed.btnDeleteClick(Sender: TObject);
begin
  inherited;
  if rdgModflowBoundary.SelectedRow >=
    rdgModflowBoundary.FixedRows+PestRowOffset  then
  begin
    if rdgModflowBoundary.RowCount >
      rdgModflowBoundary.FixedRows + 1 + PestRowOffset then
    begin
      ClearSelectedRow;
      rdgModflowBoundary.DeleteRow(rdgModflowBoundary.SelectedRow);
      UpdateNumTimes;

    end
    else
    begin
      ClearSelectedRow;
      seNumberOfTimes.AsInteger := seNumberOfTimes.AsInteger -1;
    end;
    Edited;
  end;
end;

procedure TframeScreenObjectTabbed.btnInsertClick(Sender: TObject);
begin
  if rdgModflowBoundary.SelectedRow >= rdgModflowBoundary.FixedRows+PestRowOffset  then
  begin
    rdgModflowBoundary.InsertRow(rdgModflowBoundary.SelectedRow);
    ClearSelectedRow;
    UpdateNumTimes;
    Edited;
  end;
end;

function TframeScreenObjectTabbed.CanEdit(Sender: TObject; ACol,
  ARow: Integer): Boolean;
begin
  result := ACol >= 2;
end;

procedure TframeScreenObjectTabbed.CanSelectTimeCell(ARow, ACol: Integer;
  var CanSelect: Boolean);
begin
  CanSelect := True;
end;

procedure TframeScreenObjectTabbed.ClearSelectedRow;
var
  ColIndex: Integer;
begin
  for ColIndex := 0 to rdgModflowBoundary.ColCount - 1 do
  begin
    rdgModflowBoundary.Cells[ColIndex, rdgModflowBoundary.SelectedRow] := '';
    rdgModflowBoundary.Checked[ColIndex, rdgModflowBoundary.SelectedRow] := False;
    rdgModflowBoundary.Objects[ColIndex, rdgModflowBoundary.SelectedRow] := nil;
  end;
end;

constructor TframeScreenObjectTabbed.Create(AOwner: TComponent);
begin
  inherited;
  FLastTimeColumn := 1;
end;

procedure TframeScreenObjectTabbed.Edited;
begin
  if Assigned(FOnEdited) and not FGettingData then
  begin
    FOnEdited(self);
  end;
end;

procedure TframeScreenObjectTabbed.FillPicklistsWithStartTimes;
begin
    frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithStartTimes
      (rdgModflowBoundary, 0);
    frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithEndTimes
      (rdgModflowBoundary, 1);

end;

function TframeScreenObjectTabbed.GetPestMethod(
  ACol: Integer): TPestParamMethod;
//var
//  ItemIndex: Integer;
begin
  result := inherited PestMethod[rdgModflowBoundary, ACol];
//  if PestRowOffset = 0 then
//  begin
//    result := ppmMultiply;
//    Assert(False);
//    Exit;
//  end;
//  ItemIndex := FPestMethods.IndexOf(
//    rdgModflowBoundary.Cells[ACol,PestMethodRow]);
//  if ItemIndex >= 0 then
//  begin
//    result := TPestParamMethod(ItemIndex);
//  end
//  else
//  begin
//    result := ppmMultiply;
//  end;
end;

function TframeScreenObjectTabbed.GetPestMethodAssigned(ACol: Integer): Boolean;
begin
  result := inherited PestMethodAssigned[rdgModflowBoundary, ACol];
end;

function TframeScreenObjectTabbed.GetPestModifier(ACol: Integer): string;
begin
  result := inherited PestModifier[rdgModflowBoundary, ACol];
end;

function TframeScreenObjectTabbed.GetPestModifierAssigned(
  ACol: Integer): Boolean;
begin
  result := inherited PestModifierAssigned[rdgModflowBoundary, ACol];
end;

procedure TframeScreenObjectTabbed.LayoutMultiRowEditControls;
var
  FormulaColumn: Integer;
begin
  inherited;
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;

  FormulaColumn := Max(FLastTimeColumn+1,rdgModflowBoundary.LeftCol);
  LayoutControls(rdgModflowBoundary, rdeFormula, lblFormula, FormulaColumn);
end;

procedure TframeScreenObjectTabbed.rdeFormulaChange(Sender: TObject);
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  inherited;
  rdgModflowBoundary.BeginUpdate;
  try
    for RowIndex := rdgModflowBoundary.FixedRows+PestRowOffset to
      rdgModflowBoundary.RowCount - 1 do
    begin
      for ColIndex := 2 to rdgModflowBoundary.ColCount -1 do
      begin
        if rdgModflowBoundary.IsSelectedCell(ColIndex, RowIndex)
          and CanEdit(rdgModflowBoundary, ColIndex, RowIndex)  then
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
  UpdateTransientEditor;
end;

procedure TframeScreenObjectTabbed.rdgModflowBoundaryBeforeDrawCell(
  Sender: TObject; ACol, ARow: Integer);
var
  CanSelect: Boolean;
begin
  inherited;
  CanSelect := True;
  CanSelectTimeCell(ARow, ACol, CanSelect);
  if not CanSelect then
  begin
    rdgModflowBoundary.Canvas.Brush.Color := clBtnFace;
  end;
end;

procedure TframeScreenObjectTabbed.rdgModflowBoundaryColSize(Sender: TObject;
  ACol, PriorWidth: Integer);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TframeScreenObjectTabbed.rdgModflowBoundaryMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ShouldEnable: Boolean;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  ShouldEnable := False;
  for RowIndex := rdgModflowBoundary.FixedRows + PestRowOffset
    to rdgModflowBoundary.RowCount -1 do
  begin
    for ColIndex := 2 to rdgModflowBoundary.ColCount do
    begin
      ShouldEnable := rdgModflowBoundary.IsSelectedCell(ColIndex,RowIndex);
      if ShouldEnable then
      begin
        break;
      end;
    end;
    if ShouldEnable then
    begin
      break;
    end;
  end;
  rdeFormula.Enabled := ShouldEnable;

end;

procedure TframeScreenObjectTabbed.rdgModflowBoundarySelectCell(Sender: TObject;
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

procedure TframeScreenObjectTabbed.rdgModflowBoundarySetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  UpdateNextTimeCell(rdgModflowBoundary, ACol, ARow);
  seNumberOfTimes.AsInteger := rdgModflowBoundary.RowCount -1-PestRowOffset;
  Edited;
end;

procedure TframeScreenObjectTabbed.seNumberOfTimesChange(Sender: TObject);
begin
  inherited;
  rdgModflowBoundary.RowCount := Max(2, seNumberOfTimes.AsInteger + 1)+PestRowOffset;
  if seNumberOfTimes.AsInteger = 0 then
  begin
    ClearGrid(rdgModflowBoundary);
  end;
  Edited;
end;

procedure TframeScreenObjectTabbed.SetPestMethod(ACol: Integer;
  const Value: TPestParamMethod);
begin
  inherited PestMethod[rdgModflowBoundary, ACol] := Value
end;

procedure TframeScreenObjectTabbed.SetPestMethodAssigned(ACol: Integer;
  const Value: Boolean);
begin
  inherited PestMethodAssigned[rdgModflowBoundary, ACol] := Value
end;

procedure TframeScreenObjectTabbed.SetPestModifier(ACol: Integer;
  const Value: string);
begin
  inherited PestModifier[rdgModflowBoundary, ACol] := Value
end;

procedure TframeScreenObjectTabbed.SetPestModifierAssigned(ACol: Integer;
  const Value: Boolean);
begin
  inherited PestModifierAssigned[rdgModflowBoundary, ACol] := Value
end;

procedure TframeScreenObjectTabbed.UpdateNumTimes;
begin
  if seNumberOfTimes <> nil then
  begin
    seNumberOfTimes.AsInteger := rdgModflowBoundary.RowCount - 1-PestRowOffset;
  end;
end;

procedure TframeScreenObjectTabbed.UpdateTransientEditor;
var
  TempOptions: TGridOptions;
begin
  TempOptions := rdgModflowBoundary.Options;
  try
    rdgModflowBoundary.Options := [goEditing, goAlwaysShowEditor];
    rdgModflowBoundary.UpdateEditor;
  finally
    rdgModflowBoundary.Options := TempOptions;
  end;
end;

end.
