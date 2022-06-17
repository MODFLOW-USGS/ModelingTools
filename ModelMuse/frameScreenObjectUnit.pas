unit frameScreenObjectUnit;

interface

uses
  Grids, RbwDataGrid4, Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms,
  Dialogs, GoPhastTypes;

type
  TframeScreenObject = class(TFrame)
  private { Private declarations }
    // See @link(FrameLoaded).
    FFrameLoaded: boolean;
    // See @link(FrameLoaded).
    procedure SetFrameLoaded(const Value: boolean);
    function GetPestMethod(Grid: TRbwDataGrid4;
      ACol: Integer): TPestParamMethod;
    function GetPestMethodAssigned(Grid: TRbwDataGrid4; ACol: Integer): Boolean;
    function GetPestModifier(Grid: TRbwDataGrid4; ACol: Integer): string;
    function GetPestModifierAssigned(Grid: TRbwDataGrid4;
      ACol: Integer): Boolean;
    procedure SetPestMethod(Grid: TRbwDataGrid4; ACol: Integer;
      const Value: TPestParamMethod);
    procedure SetPestMethodAssigned(Grid: TRbwDataGrid4; ACol: Integer;
      const Value: Boolean);
    procedure SetPestModifier(Grid: TRbwDataGrid4; ACol: Integer;
      const Value: string);
    procedure SetPestModifierAssigned(Grid: TRbwDataGrid4; ACol: Integer;
      const Value: Boolean);
  protected
    FLastTimeColumn: integer;
    // When the user sets the starting time for a boundary that matches the
    // starting time of a stress period, automatically
    // set the ending time to be the ending time to be the ending time of the
    // same stress period.
    procedure UpdateNextTimeCell(DataGrid: TRbwDataGrid4; ACol, ARow: Integer);
    procedure ClearGrid(Grid: TRbwDataGrid4); virtual;

  public
    // @name is used in @link(TframeScreenObjectParam.clbParametersStateChange
    // TframeScreenObjectParam.clbParametersStateChange) to prevent grayed
    // check boxes from being converted to normal ones incorrectly.
    // It is also used in @link(UpdateNextTimeCell) to prevent cells from
    // being set improperly when @link(TfrmScreenObjectProperties) is
    // reading data.
    property FrameLoaded: boolean read FFrameLoaded write SetFrameLoaded;
    constructor Create(AOwner: TComponent); override;
    Property PestMethod[Grid: TRbwDataGrid4; ACol: Integer]: TPestParamMethod
      read GetPestMethod write SetPestMethod;
    Property PestModifier[Grid: TRbwDataGrid4; ACol: Integer]: string
      read GetPestModifier write SetPestModifier;
    Property PestMethodAssigned[Grid: TRbwDataGrid4; ACol: Integer]: Boolean
      read GetPestMethodAssigned write SetPestMethodAssigned;
    Property PestModifierAssigned[Grid: TRbwDataGrid4; ACol: Integer]: Boolean
      read GetPestModifierAssigned write SetPestModifierAssigned;
  end;

implementation

uses
  frmCustomGoPhastUnit;

{$R *.dfm}

var
  FPestMethods: TStringList;

procedure TframeScreenObject.ClearGrid(Grid: TRbwDataGrid4);
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  Grid.BeginUpdate;
  try
    for RowIndex := Grid.FixedRows to Grid.RowCount - 1 do
    begin
      for ColIndex := Grid.FixedCols to Grid.ColCount - 1 do
      begin
        Grid.Cells[ColIndex,RowIndex] := '';
        Grid.Checked[ColIndex,RowIndex] := False;
        Grid.Objects[ColIndex,RowIndex] := nil;
      end;
    end;
  finally
    Grid.EndUpdate;
  end;
end;

constructor TframeScreenObject.Create(AOwner: TComponent);
begin
  inherited;
  FLastTimeColumn := 1;
end;

function TframeScreenObject.GetPestMethod(Grid: TRbwDataGrid4;
  ACol: Integer): TPestParamMethod;
var
  ItemIndex: Integer;
begin
//  if PestRowOffset = 0 then
//  begin
//    result := ppmMultiply;
//    Assert(False);
//    Exit;
//  end;
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

function TframeScreenObject.GetPestMethodAssigned(Grid: TRbwDataGrid4;
  ACol: Integer): Boolean;
begin
//  if PestRowOffset = 0 then
//  begin
//    result := False;
//    Assert(False);
//    Exit;
//  end;
  result := FPestMethods.IndexOf(Grid.Cells[ACol,PestMethodRow]) >= 0;
end;

function TframeScreenObject.GetPestModifier(Grid: TRbwDataGrid4;
  ACol: Integer): string;
begin
//  if PestRowOffset = 0 then
//  begin
//    result := '';
//    Assert(False);
//    Exit;
//  end;
  result := Grid.Cells[ACol, PestModifierRow];
  if result = strNone then
  begin
    result := '';
  end;
end;

function TframeScreenObject.GetPestModifierAssigned(Grid: TRbwDataGrid4;
  ACol: Integer): Boolean;
begin
//  if PestRowOffset = 0 then
//  begin
//    result := False;
//    Assert(False);
//    Exit;
//  end;
  result := Grid.Cells[ACol, PestModifierRow] <> '';
end;

procedure TframeScreenObject.SetFrameLoaded(const Value: boolean);
begin
  FFrameLoaded := Value;
end;

procedure TframeScreenObject.SetPestMethod(Grid: TRbwDataGrid4; ACol: Integer;
  const Value: TPestParamMethod);
begin
  if PestMethodRow = 0 then
  begin
    Exit;
  end;
  Grid.Cells[ACol,PestMethodRow] := FPestMethods[Ord(Value)];
end;

procedure TframeScreenObject.SetPestMethodAssigned(Grid: TRbwDataGrid4;
  ACol: Integer; const Value: Boolean);
begin
  if PestMethodRow = 0 then
  begin
    Exit;
  end;
  if not Value then
  begin
    Grid.Cells[ACol,PestMethodRow] := '';
  end;
end;

procedure TframeScreenObject.SetPestModifier(Grid: TRbwDataGrid4; ACol: Integer;
  const Value: string);
begin
//  if PestRowOffset = 0 then
//  begin
//    Assert(False);
//    Exit;
//  end;
  if Value = '' then
  begin
    Grid.Cells[ACol, PestModifierRow] := strNone;
  end
  else
  begin
    Grid.Cells[ACol, PestModifierRow] := Value;
  end;
end;

procedure TframeScreenObject.SetPestModifierAssigned(Grid: TRbwDataGrid4;
  ACol: Integer; const Value: Boolean);
begin
//  if PestRowOffset = 0 then
//  begin
//    Assert(False);
//    Exit;
//  end;
  if not Value then
  begin
    Grid.Cells[ACol, PestModifierRow] := '';
  end;
end;

procedure TframeScreenObject.UpdateNextTimeCell(DataGrid: TRbwDataGrid4;
  ACol, ARow: Integer);
//var
//  SelectIndex: Integer;
begin
  if FrameLoaded and (FLastTimeColumn = 1) then
  begin
    frmCustomGoPhastUnit.UpdateNextTimeCell(DataGrid, ACol, ARow);
  end;
end;

initialization
  FPestMethods := TStringList.Create;
  FPestMethods.Add(StrMultiply);
  FPestMethods.Add(StrAdd);

finalization
 FPestMethods.Free;

end.
