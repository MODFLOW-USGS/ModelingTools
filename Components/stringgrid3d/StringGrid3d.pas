unit StringGrid3d;

interface

{
by Richard B. Winston
rbwinst@usgs.gov
rbwinston@mindspring.com
This component is in the public domain.
}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Grids, DataGrid;

type
   ERBWGridException = class(Exception);

  TRBWCustom3DGrid = class(TPageControl)
  private
    FFixedCol : integer;
    FFixedRow : integer;
{    FRowCount : integer;
    FColCount : integer; }
    FGrids : TList;
    FOnColumnMoved : TMovedEvent;
    FOnDrawCell: TDrawCellEvent;
    FOnGetEditText: TGetEditEvent;
    FOnGetEditMask: TGetEditEvent;
    FOnRowMoved: TMovedEvent;
    FOnTopLeftChanged: TNotifyEvent;
    FOnSelectCell: TSelectCellEvent;
    FOnSetEditText: TSetEditEvent;
    FColCount: integer;
    FRowCount: integer;
    FOptions: TGridOptions;
    FDefaultColWidth: integer;
    FDefaultRowHeight: integer;
    Function GetGrid(Index : integer) : TStringGrid;
    procedure SetFixedCol(Value : integer);
    procedure SetFixedRow(Value : integer);
    procedure SetOnColumnMoved(AValue : TMovedEvent);
    function GetGridCount : integer;
    procedure SetGridCount(const Value : integer);
    procedure SetOnDrawCell(const Value: TDrawCellEvent);
    procedure SetOnGetEditMask(const Value: TGetEditEvent);
    procedure SetOnGetEditText(const Value: TGetEditEvent);
    procedure SetOnRowMoved(const Value: TMovedEvent);
    procedure SetOnSelectCell(const Value: TSelectCellEvent);
    procedure SetOnSetEditText(const Value: TSetEditEvent);
    procedure SetOnTopLeftChanged(const Value: TNotifyEvent);
    procedure SetColCount(const Value: integer);
    procedure SetRowCount(const Value: integer);
    procedure SetOptions(const Value: TGridOptions);
    procedure SetDefaultColWidth(const Value: integer);
    procedure SetDefaultRowHeight(const Value: integer);
  protected
    procedure SetGridProperties(AStringGrid : TStringGrid);
  public
    function AddGrid : integer; virtual; abstract;
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    property GridCount : integer read GetGridCount write SetGridCount;
    property Grids [Index : integer] : TStringGrid read GetGrid;
    Procedure InsertGrid(const Index : integer);
    Procedure MovePage(const OldIndex, NewIndex : integer);
    Procedure RemoveGrid(Index : integer); virtual; abstract;
  published
    property DefaultColWidth : integer read FDefaultColWidth write SetDefaultColWidth;
    property DefaultRowHeight : integer read FDefaultRowHeight write SetDefaultRowHeight;
    property FixedCol : integer read FFixedCol write SetFixedCol;
    property FixedRow : integer read FFixedRow write SetFixedRow;
    property GridColCount : integer read FColCount write SetColCount;
    property GridRowCount : integer read FRowCount write SetRowCount;
    property Options: TGridOptions read FOptions write SetOptions;
    property OnColumnMoved: TMovedEvent read FOnColumnMoved write SetOnColumnMoved;
    property OnDrawCell: TDrawCellEvent read FOnDrawCell write SetOnDrawCell;
    property OnGetEditMask: TGetEditEvent read FOnGetEditMask write SetOnGetEditMask;
    property OnGetEditText: TGetEditEvent read FOnGetEditText write SetOnGetEditText;
    property OnRowMoved: TMovedEvent read FOnRowMoved write SetOnRowMoved;
    property OnSelectCell: TSelectCellEvent read FOnSelectCell write SetOnSelectCell;
    property OnSetEditText: TSetEditEvent read FOnSetEditText write SetOnSetEditText;
    property OnTopLeftChanged: TNotifyEvent read FOnTopLeftChanged write SetOnTopLeftChanged;
  end;

  TRBWStringGrid3d = class(TRBWCustom3DGrid)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    function AddGrid : integer; override;
    Procedure RemoveGrid(Index : integer); override;
    { Public declarations }
  published
    { Published declarations }
  end;

  TRBWDataGrid3d = class(TRBWCustom3DGrid)
  private
    Function GetDataGrid(Index : integer) : TDataGrid;
    { Private declarations }
  protected
    { Protected declarations }
  public
    function AddGrid : integer; override;
    Procedure RemoveGrid(Index : integer); override;
    property Grids [Index : integer] : TDataGrid read GetDataGrid;
    { Public declarations }
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Argus', [TRBWStringGrid3d, TRBWDataGrid3d]);
end;

{ TStringGrid3d }

function TRBWStringGrid3d.AddGrid: integer;
var
  ATabSheet : TTabSheet;
  AStringGrid : TStringGrid;
begin
  ATabSheet := TTabSheet.Create(self);
  ATabSheet.PageControl := self;
  ATabSheet.HelpContext := HelpContext;
  ATabSheet := Pages[PageCount-1];
  AStringGrid := TStringGrid.Create(Self);;
  AStringGrid.Parent := ATabSheet;

  SetGridProperties(AStringGrid);

  FGrids.Add(AStringGrid);
  result := PageCount - 1;
end;

procedure TRBWStringGrid3d.RemoveGrid(Index: integer);
var
  AStringGrid : TStringGrid;
  ATabSheet : TTabSheet;
begin
  AStringGrid := Grids[Index];
  ATabSheet := AStringGrid.Parent as TTabSheet;
  FGrids.Remove(AStringGrid);
  AStringGrid.Free;
  ATabSheet.Free;
end;

{ TRBWCustom3DGrid }

constructor TRBWCustom3DGrid.Create(AOwner: TComponent);
var
  AStringGrid : TStringGrid;
begin
  inherited;
  FGrids := TList.Create;
  FFixedCol := 1;
  FFixedRow := 1;
  FColCount := 5;
  FRowCount := 5;
  FDefaultColWidth := 64;
  FDefaultRowHeight := 24;
  AStringGrid := TStringGrid.Create(self);
  try
    Options := AStringGrid.Options;
  finally
    AStringGrid.Free;
  end;


end;

destructor TRBWCustom3DGrid.Destroy;
begin
  FGrids.Free;
  inherited;

end;

function TRBWCustom3DGrid.GetGrid(Index : integer): TStringGrid;
begin
  result := FGrids[Index];
end;

function TRBWCustom3DGrid.GetGridCount: integer;
begin
  result := FGrids.Count;
end;

procedure TRBWCustom3DGrid.InsertGrid(const Index: integer);
var
  EndPosition : integer;
begin
  EndPosition := AddGrid;
  MovePage(EndPosition, Index);
end;

procedure TRBWCustom3DGrid.MovePage(const OldIndex, NewIndex: integer);
var
  Grid : Pointer;
begin
  if (OldIndex < 0) or (OldIndex >= PageCount) then
  begin
    raise ERBWGridException.Create('Invalid old page index');
  end
  else if (NewIndex < 0) or (NewIndex >= PageCount) then
  begin
    raise ERBWGridException.Create('Invalid new page index');
  end
  else
  begin
    Grid := FGrids[OldIndex];
    FGrids.Delete(OldIndex);
    FGrids.Insert(NewIndex, Grid);

    Pages[OldIndex].PageIndex := NewIndex;
  end;   

end;

procedure TRBWCustom3DGrid.SetColCount(const Value: integer);
var
  Index : integer;
  AStringGrid : TStringGrid;
begin
  if FColCount <> Value then
  begin
    FColCount := Value;
    for Index := 0 to FGrids.Count -1 do
    begin
      AStringGrid := FGrids[Index];
      AStringGrid.ColCount := FColCount;
    end;
    if FColCount < 2 then
    begin
      FixedCol := 1;
    end;
  end;
end;

procedure TRBWCustom3DGrid.SetDefaultColWidth(const Value: integer);
var
  Index : integer;
  AStringGrid : TStringGrid;
begin
  if FDefaultColWidth <> Value then
  begin
    FDefaultColWidth := Value;
    for Index := 0 to FGrids.Count -1 do
    begin
      AStringGrid := FGrids[Index];
      AStringGrid.DefaultColWidth := FDefaultColWidth;
    end;
  end;
end;

procedure TRBWCustom3DGrid.SetDefaultRowHeight(const Value: integer);
var
  Index : integer;
  AStringGrid : TStringGrid;
begin
  if FDefaultRowHeight <> Value then
  begin
    FDefaultRowHeight := Value;
    for Index := 0 to FGrids.Count -1 do
    begin
      AStringGrid := FGrids[Index];
      AStringGrid.DefaultRowHeight := FDefaultRowHeight;
    end;
  end;
end;

procedure TRBWCustom3DGrid.SetFixedCol(Value: integer);
var
  Index : integer;
  AStringGrid : TStringGrid;
begin
  if FFixedCol <> Value then
  begin
    FFixedCol := Value;
    for Index := 0 to FGrids.Count -1 do
    begin
      AStringGrid := FGrids[Index];
      AStringGrid.FixedCols := FFixedCol;
    end;

  end;
end;

procedure TRBWCustom3DGrid.SetFixedRow(Value: integer);
var
  Index : integer;
  AStringGrid : TStringGrid;
begin
  if FFixedRow <> Value then
  begin
    FFixedRow := Value;
    for Index := 0 to FGrids.Count -1 do
    begin
      AStringGrid := FGrids[Index];
      AStringGrid.FixedRows := FFixedRow;
    end;

  end;
end;

procedure TRBWCustom3DGrid.SetGridCount(const Value: integer);
var
  Index : integer;
begin
  if Value <> GridCount then
  begin
    for Index := GridCount-1 downto Value do
    begin
      RemoveGrid(Index);
    end;
    for Index := GridCount+1 to Value do
    begin
      AddGrid;
    end;
  end;
end;

procedure TRBWCustom3DGrid.SetGridProperties(AStringGrid: TStringGrid);
begin
  AStringGrid.Align := alClient;
  AStringGrid.FixedCols := FFixedCol;
  AStringGrid.FixedRows := FFixedRow;
  AStringGrid.ColCount := FColCount;
  AStringGrid.RowCount := FRowCount;
  AStringGrid.HelpContext := HelpContext;
  AStringGrid.Options := FOptions;

  AStringGrid.OnColumnMoved := OnColumnMoved;
  AStringGrid.OnDrawCell := OnDrawCell;
  AStringGrid.OnGetEditMask := OnGetEditMask;
  AStringGrid.OnGetEditText := OnGetEditText;
  AStringGrid.OnRowMoved := OnRowMoved;
  AStringGrid.OnSelectCell := OnSelectCell;
  AStringGrid.OnSetEditText := OnSetEditText;
  AStringGrid.OnTopLeftChanged := OnTopLeftChanged;

  AStringGrid.DefaultColWidth := FDefaultColWidth;
  AStringGrid.DefaultRowHeight := FDefaultRowHeight;

end;

procedure TRBWCustom3DGrid.SetOnColumnMoved(AValue: TMovedEvent);
var
  Index : integer;
  AStringGrid : TStringGrid;
begin
  FOnColumnMoved := AValue;
  for Index := 0 to GridCount -1 do
  begin
    AStringGrid := FGrids[Index];
    AStringGrid.OnColumnMoved := FOnColumnMoved;
  end;
end;

procedure TRBWCustom3DGrid.SetOnDrawCell(const Value: TDrawCellEvent);
var
  Index : integer;
  AStringGrid : TStringGrid;
begin
  FOnDrawCell := Value;
  for Index := 0 to GridCount -1 do
  begin
    AStringGrid := FGrids[Index];
    AStringGrid.OnDrawCell := Value;
  end;
end;

procedure TRBWCustom3DGrid.SetOnGetEditMask(const Value: TGetEditEvent);
var
  Index : integer;
  AStringGrid : TStringGrid;
begin
  FOnGetEditMask := Value;
  for Index := 0 to GridCount -1 do
  begin
    AStringGrid := FGrids[Index];
    AStringGrid.OnGetEditMask := Value;
  end;
end;

procedure TRBWCustom3DGrid.SetOnGetEditText(const Value: TGetEditEvent);
var
  Index : integer;
  AStringGrid : TStringGrid;
begin
  FOnGetEditText := Value;
  for Index := 0 to GridCount -1 do
  begin
    AStringGrid := FGrids[Index];
    AStringGrid.OnGetEditText := Value;
  end;
end;

procedure TRBWCustom3DGrid.SetOnRowMoved(const Value: TMovedEvent);
var
  Index : integer;
  AStringGrid : TStringGrid;
begin
  FOnRowMoved := Value;
  for Index := 0 to GridCount -1 do
  begin
    AStringGrid := FGrids[Index];
    AStringGrid.OnRowMoved := Value;
  end;
end;

procedure TRBWCustom3DGrid.SetOnSelectCell(const Value: TSelectCellEvent);
var
  Index : integer;
  AStringGrid : TStringGrid;
begin
  FOnSelectCell := Value;
  for Index := 0 to GridCount -1 do
  begin
    AStringGrid := FGrids[Index];
    AStringGrid.OnSelectCell := Value;
  end;
end;

procedure TRBWCustom3DGrid.SetOnSetEditText(const Value: TSetEditEvent);
var
  Index : integer;
  AStringGrid : TStringGrid;
begin
  FOnSetEditText := Value;
  for Index := 0 to GridCount -1 do
  begin
    AStringGrid := FGrids[Index];
    AStringGrid.OnSetEditText := Value;
  end;
end;

procedure TRBWCustom3DGrid.SetOnTopLeftChanged(const Value: TNotifyEvent);
var
  Index : integer;
  AStringGrid : TStringGrid;
begin
  FOnTopLeftChanged := Value;
  for Index := 0 to GridCount -1 do
  begin
    AStringGrid := FGrids[Index];
    AStringGrid.OnTopLeftChanged := Value;
  end;
end;

procedure TRBWCustom3DGrid.SetOptions(const Value: TGridOptions);
var
  Index : integer;
  AStringGrid : TStringGrid;
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    for Index := 0 to FGrids.Count -1 do
    begin
      AStringGrid := FGrids[Index];
      AStringGrid.Options := FOptions;
    end;
  end;
end;

procedure TRBWCustom3DGrid.SetRowCount(const Value: integer);
var
  Index : integer;
  AStringGrid : TStringGrid;
begin
  if FRowcount <> Value then
  begin
    FRowcount := Value;
    for Index := 0 to FGrids.Count -1 do
    begin
      AStringGrid := FGrids[Index];
      AStringGrid.RowCount := FRowcount;
    end;
    if FRowcount < 2 then
    begin
      FixedRow := 1;
    end;
  end;
end;

{ TRBWDataGrid3d }

function TRBWDataGrid3d.AddGrid: integer;
var
  ATabSheet : TTabSheet;
  ADataGrid : TDataGrid;
begin
  ATabSheet := TTabSheet.Create(self);
  ATabSheet.PageControl := self;
  ATabSheet.HelpContext := HelpContext;
  ADataGrid := TDataGrid.Create(Self);;
  ADataGrid.Parent := ATabSheet;
{  ADataGrid.Align := alClient;
  ADataGrid.FixedCols := FFixedCol;
  ADataGrid.FixedRows := FFixedRow;
  ADataGrid.HelpContext := HelpContext; }

  SetGridProperties(ADataGrid);

  FGrids.Add(ADataGrid);
  result := PageCount - 1;
end;

function TRBWDataGrid3d.GetDataGrid(Index: integer): TDataGrid;
begin
  result := FGrids[Index];
end;

procedure TRBWDataGrid3d.RemoveGrid(Index: integer);
var
  ADataGrid : TDataGrid;
  ATabSheet : TTabSheet;
begin
  ADataGrid := Grids[Index];
  ATabSheet := ADataGrid.Parent as TTabSheet;
  FGrids.Remove(ADataGrid);
  ADataGrid.Free;
  ATabSheet.Free;
end;

end.
