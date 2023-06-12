unit frameFormulaGridUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frameGridUnit, Grids, RbwDataGrid4,
  StdCtrls, Mask, JvExMask, JvSpin, Buttons, ExtCtrls,
  GoPhastTypes,
  PhastModelInterfaceUnit
  ;

type
  TValidCellEvent = procedure (Sender: TObject; ACol, ARow: Integer; var ValidCell: Boolean) of object;

  TframeFormulaGrid = class(TframeGrid)
    pnlTop: TPanel;
    edFormula: TLabeledEdit;
    cbMultiCheck: TCheckBox;
    comboChoice: TComboBox;
    procedure edFormulaChange(Sender: TObject);
    procedure GridColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure GridHorizontalScroll(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure GridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cbMultiCheckClick(Sender: TObject);
    procedure comboChoiceChange(Sender: TObject);
    procedure GridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect:
        Boolean);
  private
    FFirstFormulaColumn: Integer;
    FOnValidCell: TValidCellEvent;
    FFirstCheckColumn: Integer;
    FOnValidCheckCell: TValidCellEvent;
    FFirstChoiceColumn: Integer;
    FOnValidChoiceCell: TValidCellEvent;
    FPestParameters: TStringList;
    FPestUsedOnCol: array of Boolean;

    procedure AssignNewTextToMultipleCells(NewText: string);
    function GetPestMethod(ACol: Integer): TPestParamMethod;
    function GetPestMethodAssigned(ACol: Integer): Boolean;
    function GetPestModifier(ACol: Integer): string;
    function GetPestModifierAssigned(ACol: Integer): Boolean;
    procedure SetPestMethod(ACol: Integer; const Value: TPestParamMethod);
    procedure SetPestMethodAssigned(ACol: Integer; const Value: Boolean);
    procedure SetPestModifier(ACol: Integer; const Value: string);
    procedure SetPestModifierAssigned(ACol: Integer; const Value: Boolean);
    procedure SetPestUsedOnCol(ACol: Integer; const Value: Boolean);
    function GetPestUsedOnCol(ACol: Integer): Boolean;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LayoutMultiRowEditControls; virtual;
    // When @link(edFormula) is used to assign the same formula to multiple
    // cells, @name is the first column in the grid that will be potential
    // targets for the change.
    property FirstFormulaColumn: Integer read FFirstFormulaColumn
      write FFirstFormulaColumn;
    // When @link(cbMultiCheck) is used to assign the same formula to multiple
    // cells, @name is the first column in the grid that will be potential
    // targets for the change.
    property FirstCheckColumn: Integer read FFirstCheckColumn
      write FFirstCheckColumn;
    // When @link(comboChoice) is used to assign the same formula to multiple
    // cells, @name is the first column in the grid that will be potential
    // targets for the change.
    property FirstChoiceColumn: Integer read FFirstChoiceColumn
      write FFirstChoiceColumn;
    // When @link(edFormula) is used to assign the same formula to multiple
    // cells, @name can be used to detect whether or not a particular cell
    // is a valid target for the change.
    property OnValidCell: TValidCellEvent read FOnValidCell write FOnValidCell;
    // When @link(cbMultiCheck) is used to assign the same formula to multiple
    // cells, @name can be used to detect whether or not a particular cell
    // is a valid target for the change.
    property OnValidCheckCell: TValidCellEvent read FOnValidCheckCell
      write FOnValidCheckCell;
    // When @link(comboChoice) is used to assign the same formula to multiple
    // cells, @name can be used to detect whether or not a particular cell
    // is a valid target for the change.
    property OnValidChoiceCell: TValidCellEvent read FOnValidChoiceCell
      write FOnValidChoiceCell;
    Property PestMethod[ACol: Integer]: TPestParamMethod
      read GetPestMethod write SetPestMethod;
    Property PestModifier[ACol: Integer]: string
      read GetPestModifier write SetPestModifier;
    Property PestMethodAssigned[ACol: Integer]: Boolean
      read GetPestMethodAssigned write SetPestMethodAssigned;
    Property PestModifierAssigned[ACol: Integer]: Boolean
      read GetPestModifierAssigned write SetPestModifierAssigned;
    procedure InitializePestParameters;
    property PestUsedOnCol[ACol: Integer]: Boolean read GetPestUsedOnCol
       write SetPestUsedOnCol;
    { Public declarations }
  end;

var
  frameFormulaGrid: TframeFormulaGrid;

implementation

uses
  frmCustomGoPhastUnit, Math;

{$R *.dfm}

var
  FPestMethods: TStringList;

procedure TframeFormulaGrid.cbMultiCheckClick(Sender: TObject);
var
  ColIndex: integer;
  RowIndex: Integer;
//  TempOptions: TGridOptions;
  ValidCell: Boolean;
begin
  inherited;
  Grid.BeginUpdate;
  try
    for RowIndex := Grid.FixedRows to
      Grid.RowCount - 1 do
    begin
      for ColIndex := FirstFormulaColumn to Grid.ColCount - 1 do
      begin
        if Grid.IsSelectedCell(ColIndex, RowIndex) then
        begin
          ValidCell := True;
          if Assigned (OnValidCell) then
          begin
            OnValidCell(Grid, ColIndex, RowIndex, ValidCell);
          end;
          if ValidCell then
          begin
            Grid.Checked[ColIndex, RowIndex] := cbMultiCheck.Checked;
            if Assigned(Grid.OnStateChange) then
            begin
              Grid.OnStateChange(
                Grid,ColIndex,RowIndex, cbMultiCheck.State);
            end;
          end;
        end;
      end;
    end;
  finally
    Grid.EndUpdate
  end
end;

procedure TframeFormulaGrid.comboChoiceChange(Sender: TObject);
begin
  AssignNewTextToMultipleCells(comboChoice.Text);
end;

constructor TframeFormulaGrid.Create(AOwner: TComponent);
begin
  inherited;
  FPestParameters := TStringList.Create;
end;

destructor TframeFormulaGrid.Destroy;
begin
  FPestParameters.Free;
  inherited;
end;

procedure TframeFormulaGrid.edFormulaChange(Sender: TObject);
begin
  AssignNewTextToMultipleCells(edFormula.Text);
end;

procedure TframeFormulaGrid.FrameResize(Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

function TframeFormulaGrid.GetPestMethod(ACol: Integer): TPestParamMethod;
var
  ItemIndex: Integer;
begin
  ItemIndex := FPestMethods.IndexOf(
    Grid.Cells[ACol,PestMethodRow]);
  if ItemIndex >= 0 then
  begin
    result := TPestParamMethod(ItemIndex);
  end
  else
  begin
    result := ppmMultiply;
  end;
end;

function TframeFormulaGrid.GetPestMethodAssigned(ACol: Integer): Boolean;
begin
  result := FPestMethods.IndexOf(Grid.Cells[ACol,PestMethodRow]) >= 0;
end;

function TframeFormulaGrid.GetPestModifier(ACol: Integer): string;
begin
  result := Grid.Cells[ACol, PestModifierRow];
  if result = strNone then
  begin
    result := '';
  end;
end;

function TframeFormulaGrid.GetPestModifierAssigned(ACol: Integer): Boolean;
begin
  result := Grid.Cells[ACol, PestModifierRow] <> '';
end;

function TframeFormulaGrid.GetPestUsedOnCol(ACol: Integer): Boolean;
begin
  if (ACol < Length(FPestUsedOnCol)) and (ACol >= 0) then
  begin
    result := FPestUsedOnCol[ACol];
  end
  else
  begin
    result := False;
  end;

end;

procedure TframeFormulaGrid.GridColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TframeFormulaGrid.GridHorizontalScroll(Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TframeFormulaGrid.GridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ShouldEnable: boolean;
  ColIndex, RowIndex: Integer;
begin
  inherited;
  if edFormula.Visible then
  begin
    ShouldEnable := False;
    for RowIndex := Grid.FixedRows to Grid.RowCount -1 do
    begin
      for ColIndex := FirstFormulaColumn to Grid.ColCount - 1 do
      begin
        ShouldEnable := Grid.IsSelectedCell(ColIndex,RowIndex);
        if ShouldEnable then
        begin
          if Assigned(OnValidCell) then
          begin
            OnValidCell(self, ColIndex, RowIndex, ShouldEnable);
            if ShouldEnable then
            begin
              Break;
            end;
          end
          else
          begin
            break;
          end;
        end;
      end;
      if ShouldEnable then
      begin
        break;
      end;
    end;
    edFormula.Enabled := ShouldEnable;
  end;

  if cbMultiCheck.Visible then
  begin
    ShouldEnable := False;
    for RowIndex := Grid.FixedRows to Grid.RowCount -1 do
    begin
      for ColIndex := FirstCheckColumn to Grid.ColCount - 1 do
      begin
        ShouldEnable := Grid.IsSelectedCell(ColIndex,RowIndex);
        if ShouldEnable then
        begin
          if Assigned(OnValidCheckCell) then
          begin
            OnValidCheckCell(self, ColIndex, RowIndex, ShouldEnable);
            if ShouldEnable then
            begin
              Break;
            end;
          end
          else
          begin
            break;
          end;
        end;
      end;
      if ShouldEnable then
      begin
        break;
      end;
    end;
    cbMultiCheck.Enabled := ShouldEnable;
  end;

  if comboChoice.Visible then
  begin
    ShouldEnable := False;
    for RowIndex := Grid.FixedRows to Grid.RowCount -1 do
    begin
      for ColIndex := FirstChoiceColumn to Grid.ColCount - 1 do
      begin
        ShouldEnable := Grid.IsSelectedCell(ColIndex,RowIndex);
        if ShouldEnable then
        begin
          if Assigned(OnValidChoiceCell) then
          begin
            OnValidChoiceCell(self, ColIndex, RowIndex, ShouldEnable);
            if ShouldEnable then
            begin
              Break;
            end;
          end
          else
          begin
            break;
          end;
        end;
      end;
      if ShouldEnable then
      begin
        break;
      end;
    end;
    comboChoice.Enabled := ShouldEnable;
  end;
end;

procedure TframeFormulaGrid.InitializePestParameters;
begin
  if IGlobalModelForOrderedCollection <> nil then
  begin
    IGlobalModelForOrderedCollection.GetPestParameterNames(FPestParameters);
  end
  else
  begin
    FPestParameters.Clear;
  end;
end;

procedure TframeFormulaGrid.LayoutMultiRowEditControls;
var
  Column: integer;
  Row: Integer;
  ColIndex: Integer;
  ValidCell: Boolean;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  if edFormula.Visible then
  begin
    Column := Max(FirstFormulaColumn,Grid.LeftCol);
    if Assigned(OnValidCell) then
    begin
      Row := 1;
      for ColIndex := Column to Grid.ColCount - 1 do
      begin
        ValidCell := True;
        OnValidCell(self, ColIndex,Row,ValidCell);
        if ValidCell then
        begin
          Column := ColIndex;
          break;
        end;
      end;
    end;
    LayoutControls(Grid, edFormula, nil,
      Column);
  end;
  if cbMultiCheck.Visible then
  begin
    Column := Max(FirstCheckColumn,Grid.LeftCol);
    if Assigned(OnValidCheckCell) then
    begin
      Row := 1;
      for ColIndex := Column to Grid.ColCount - 1 do
      begin
        ValidCell := True;
        OnValidCheckCell(self, ColIndex,Row,ValidCell);
        if ValidCell then
        begin
          Column := ColIndex;
          break;
        end;
      end;
    end;
    LayoutControls(Grid, cbMultiCheck, nil,
      Column);
  end;
  if comboChoice.Visible then
  begin
    Column := Max(FirstChoiceColumn,Grid.LeftCol);
    if Assigned(OnValidChoiceCell) then
    begin
      Row := 1;
      for ColIndex := Column to Grid.ColCount - 1 do
      begin
        ValidCell := True;
        OnValidChoiceCell(self, ColIndex,Row,ValidCell);
        if ValidCell then
        begin
          Column := ColIndex;
          break;
        end;
      end;
    end;
    LayoutControls(Grid, comboChoice, nil,
      Column);
  end;
end;

procedure TframeFormulaGrid.SetPestMethod(ACol: Integer;
  const Value: TPestParamMethod);
begin
  Grid.Cells[ACol,PestMethodRow] := FPestMethods[Ord(Value)];
end;

procedure TframeFormulaGrid.SetPestMethodAssigned(ACol: Integer;
  const Value: Boolean);
begin
  if not Value then
  begin
    Grid.Cells[ACol,PestMethodRow] := '';
  end;
end;

procedure TframeFormulaGrid.SetPestModifier(ACol: Integer; const Value: string);
begin
  if Value = '' then
  begin
    Grid.Cells[ACol, PestModifierRow] := strNone;
  end
  else
  begin
    Grid.Cells[ACol, PestModifierRow] := Value;
  end;
end;

procedure TframeFormulaGrid.SetPestModifierAssigned(ACol: Integer;
  const Value: Boolean);
begin
  if not Value then
  begin
    Grid.Cells[ACol, PestModifierRow] := '';
  end;
end;

procedure TframeFormulaGrid.SetPestUsedOnCol(ACol: Integer;
  const Value: Boolean);
begin
  if Length(FPestUsedOnCol) <> Grid.ColCount then
  begin
    SetLength(FPestUsedOnCol, Grid.ColCount)
  end;
  if Value then
  begin
    Grid.UseSpecialFormat[ACol,PestModifierRow] := True;
    Grid.UseSpecialFormat[ACol,PestMethodRow] := True;
    FPestUsedOnCol[ACol] := True;
  end
  else
  begin
    Grid.UseSpecialFormat[ACol,PestModifierRow] := False;
    Grid.UseSpecialFormat[ACol,PestMethodRow] := False;
    FPestUsedOnCol[ACol] := False;
  end;
end;

procedure TframeFormulaGrid.AssignNewTextToMultipleCells(NewText: string);
var
  RowIndex: Integer;
  ColIndex: Integer;
  ValidCell: Boolean;
  TempOptions: TGridOptions;
begin
  Grid.BeginUpdate;
  try
    for RowIndex := Grid.FixedRows to Grid.RowCount - 1 do
    begin
      for ColIndex := FirstFormulaColumn to Grid.ColCount - 1 do
      begin
        if Grid.IsSelectedCell(ColIndex, RowIndex) then
        begin
          ValidCell := True;
          if Assigned(OnValidCell) then
          begin
            OnValidCell(Grid, ColIndex, RowIndex, ValidCell);
          end;
          if ValidCell then
          begin
            Grid.Cells[ColIndex, RowIndex] := NewText;
            if Assigned(Grid.OnSetEditText) then
            begin
              Grid.OnSetEditText(Grid, ColIndex, RowIndex, NewText);
            end;
          end;
        end;
      end;
    end;
  finally
    Grid.EndUpdate;
  end;
  TempOptions := Grid.Options;
  try
    Grid.Options := [goEditing, goAlwaysShowEditor];
    Grid.UpdateEditor;
  finally
    Grid.Options := TempOptions;
  end;
end;

procedure TframeFormulaGrid.GridSelectCell(Sender: TObject; ACol, ARow:
    Integer; var CanSelect: Boolean);
var
  Column: TRbwColumn4;
begin
  inherited;
  if ComponentState * [csLoading,csReading] <> [] then
  begin
    Exit
  end;
  if PestUsedOnCol[ACol] and (ARow >= 1) and not Grid.Drawing then
  begin
    Column := Grid.Columns[ACol];
    if (ARow <= PestRowOffset)  then
    begin
      Column.ComboUsed := True;
      Column.LimitToList := True;
      if ARow = PestMethodRow then
      begin
        Column.PickList := FPestMethods
      end
      else
      begin
        Column.PickList := FPestParameters;
      end;
    end
    else
    begin
      Column.ButtonUsed := True;
      Column.LimitToList := False;
    end;
  end;
  if IncludePestAdjustment
    and (ARow in [PestModifierRow, PestMethodRow])
    and not PestUsedOnCol[ACol] then
  begin
    CanSelect := False;
  end;
end;

initialization
  FPestMethods := TStringList.Create;
  FPestMethods.Add(StrMultiply);
  FPestMethods.Add(StrAdd);

finalization
 FPestMethods.Free;

end.
