unit RbwDataGrid2;

interface

uses
  Windows, Messages, {Types,} SysUtils, Classes, Controls, StdCtrls, Grids,
    Graphics, Forms, siComboBox;

type
  TRbwColumnFormat2 = (rcf2String, rcf2Integer, rcf2Real, rcf2Boolean, rcf2Combo);

  TChangeCheckEvent = procedure (Sender: TObject; ACol, ARow: Longint;
    const Value: TCheckBoxState) of object;

  TRbwDataGrid2 = class;

  TRbwColumn2 = class(TCollectionItem)
  private
    FLimitToList: Boolean;
    FMaxLength: Integer;
    FFormat: TRbwColumnFormat2;
    FPickList: TStrings;
    FCheckMin: boolean;
    FCheckMax: boolean;
    FMax: extended;
    FMin: extended;
    FUseButton: boolean;
    FButtonCaption: string;
    FButtonWidth: integer;
    FCaptionAlignment: TAlignment;
    FAutoAdjustColWidths: boolean;
    FWordWrapCells: boolean;
    FWordWrapCaptions: boolean;
    FAutoAdjustRowHeights: boolean;
    procedure SetFormat(const Value: TRbwColumnFormat2);
    procedure SetPickList(const Value: TStrings);
    procedure SetCheckMax(const Value: boolean);
    procedure SetCheckMin(const Value: boolean);
    procedure SetMax(const Value: extended);
    procedure SetMin(const Value: extended);
    procedure CheckRange;
    Procedure CheckCell(const ACol, ARow : integer);
    procedure SetButtonCaption(const Value: string);
    procedure SetUseButton(const Value: boolean);
    procedure SetButtonWidth(const Value: integer);
    procedure SetCaptionAlignment(const Value: TAlignment);
    procedure SetAutoAdjustColWidths(const Value: boolean);
    procedure SetWordWrapCaptions(const Value: boolean);
    procedure SetWordWrapCells(const Value: boolean);
    procedure SetAutoAdjustRowHeights(const Value: boolean);
    function Column: integer;
  protected
    function GetGrid: TRbwDataGrid2;
    procedure PickListChanged(Sender: TObject);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property  Grid: TRbwDataGrid2 read GetGrid;
  published
    property CaptionAlignment: TAlignment read FCaptionAlignment write SetCaptionAlignment default taCenter;
    property CheckMax : boolean read FCheckMax write SetCheckMax;
    property CheckMin : boolean read FCheckMin write SetCheckMin;
    property Format: TRbwColumnFormat2 read FFormat write SetFormat;
    property LimitToList: Boolean read FLimitToList write FLimitToList;
    property MaxLength: Integer read FMaxLength write FMaxLength;
    Property Max : extended read FMax write SetMax;
    Property Min : extended read Fmin write Setmin;
    property PickList: TStrings read FPickList write SetPickList;
    Property UseButton: boolean read FUseButton write SetUseButton;
    Property ButtonCaption: string read FButtonCaption write SetButtonCaption;
    Property ButtonWidth: integer read FButtonWidth write SetButtonWidth;
    property AutoAdjustColWidths: boolean read FAutoAdjustColWidths write SetAutoAdjustColWidths;
    property WordWrapCaptions: boolean read FWordWrapCaptions write SetWordWrapCaptions;
    property WordWrapCells: boolean read FWordWrapCells write SetWordWrapCells;
    property AutoAdjustRowHeights: boolean read FAutoAdjustRowHeights write SetAutoAdjustRowHeights;
  end;

  TRbwColumnClass2 = class of TRbwColumn2;

  TRbwDataGridColumns2 = class(TCollection)
  private
    FGrid: TRbwDataGrid2;
    Destroying : boolean;
    function GetItems(Index: Integer): TRbwColumn2;
    procedure SetItems(Index: Integer; const Value: TRbwColumn2);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(Grid: TRbwDataGrid2; ColumnClass: TRbwColumnClass2);
    destructor Destroy; override;
    function  Add: TRbwColumn2;
    property Grid: TRbwDataGrid2 read FGrid;
    property Items[Index: Integer]: TRbwColumn2 read GetItems write SetItems; default;
  end;

  TRbwComboBox2 = class(TsiComboBox)
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TRbwDataGrid2 = class(TStringGrid)
  private
    FDrawing : boolean;
    MouseIsDown : boolean;
    bmpChecked : TBitMap;
    bmpUnchecked : TBitMap;
    FColumns: TRbwDataGridColumns2;
    Combo : TRbwComboBox2;
    FdgColumn: integer;
    FdgRow: integer;
    FWordWrapColTitles: boolean;
    Updating : boolean;
    FUnselectableColor: TColor;
    FFixedCols : integer;
    FColorSelectedRow: boolean;
    FSelectedRowColor: TColor;
    FSelectedRow: integer;
    Button: TButton;
    FChecked: array of array of TCheckBoxState;
    FOnButtonClick: TGetEditEvent;
    FOnStateChange: TChangeCheckEvent;
    FAutoDistributeText: boolean;
    function GetCaptionFlags(const Col: integer) : UINT;
    function GetLeft: integer;
    function GetTop: integer;
    function LocalizeString(ANumberString : string) : string;
    function LocalStrToFloat(S: string): Extended;
    procedure SetColorSelectedRow(const Value: boolean);
    procedure SetColumns(const Value: TRbwDataGridColumns2);
    procedure SetControlPosition;
    procedure SetdgColumn(const Value: integer);
    procedure SetdgRow(const Value: integer);
    procedure SetLeft(const Value: integer);
    procedure SetSelectedRowColor(const Value: TColor);
    procedure SetTop(const Value: integer);
    procedure SetUnselectableColor(const Value: TColor);
    procedure SetWordWrapColTitles(const Value: boolean);
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure ComboExit(Sender: TObject);
    function GetChecked(const ACol, ARow: integer): boolean;
    procedure SetChecked(const ACol, ARow: integer; const Value: boolean);
    function GetColCount: Longint;
    function GetRowCount: Longint;
    procedure SetColCount(const Value: Longint);
    procedure SetRowCount(const Value: Longint);
    function GetCheckState(const ACol, ARow: integer): TCheckBoxState;
    procedure SetCheckState(const ACol, ARow: integer;
      const Value: TCheckBoxState);
    procedure AdjustColWidths(const ACol: integer);
    function RequiredCellWidth(const ACol, ARow: integer): integer;
    procedure FillCaptionList(CellCaption: string;
      const CaptionList: TStringList; Width: integer);
    function GetCells(ACol, ARow: Integer): string;
    procedure SetCells(ACol, ARow: Integer; const Value: string);
    function RequiredCellHeight(const ACol, ARow: integer): integer;
    procedure AdjustRowHeights(const ARow: integer);
    function GetSelection: TGridRect;
    procedure SetSelection(const Value: TGridRect);
    function GetCellFlags(const Col: integer): UINT;
    procedure DrawOrdinaryCell(ACol, ARow: Integer; ARect: TRect;
      AState: TGridDrawState);
    { Private declarations }
  protected
    procedure ButtonClick(Sender: TObject);
    function CanEditShow: Boolean; override;
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    procedure ColWidthsChanged; override;
    procedure ComboChange(Sender: TObject);
    function  CreateColumns: TRbwDataGridColumns2; dynamic;
    function CreateEditor: TInplaceEdit; override;
    property dgColumn : integer read FdgColumn write SetdgColumn;
    property dgRow : integer read FdgRow write SetdgRow;
    procedure DoExit; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;
    procedure DrawCheckBoxCell(ACol, ARow: Integer; ARect: TRect;
      AState: TGridDrawState);
    procedure DrawColumnTitleCell(ACol, ARow: Integer; ARect: TRect;
      AState: TGridDrawState); virtual;
    function GetFixedCols : integer;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure RowHeightsChanged; override;
    procedure RowMoved(FromIndex, ToIndex: Longint); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    procedure SetFixedCols(const Value : integer);
    procedure SetParent(AParent: TWinControl); override;
    procedure SizeChanged(OldColCount, OldRowCount: Longint); override;
    procedure SetEnabled(Value: boolean); override;
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Drawing: boolean read FDrawing;
    Property SelectedRow : integer read FSelectedRow;
    procedure DeleteRow(ARow: Longint); override;
    property Checked[const ACol, ARow: integer]: boolean read GetChecked write SetChecked;
    property CheckState[const ACol, ARow: integer]: TCheckBoxState read GetCheckState write SetCheckState;
    property Cells[ACol, ARow: Integer]: string read GetCells write SetCells;
    property Row: integer read FdgRow;
    { Public declarations }
    property Column: integer read FdgColumn;
    function DistributeText(const ACol, ARow: integer; CellContents: string): boolean;
    property Selection: TGridRect read GetSelection write SetSelection;
  published
    property AutoDistributeText: boolean read FAutoDistributeText write FAutoDistributeText;
    property ColCount: Longint read GetColCount write SetColCount default 5;
    Property ColorSelectedRow : boolean read FColorSelectedRow write SetColorSelectedRow;
    property Columns : TRbwDataGridColumns2 read FColumns write SetColumns;
    property FixedCols : integer read GetFixedCols write SetFixedCols;
    property Left : integer read GetLeft write SetLeft;
    property RowCount: Longint read GetRowCount write SetRowCount default 5;
    property SelectedRowColor : TColor read FSelectedRowColor write SetSelectedRowColor;
    property Top : integer read GetTop write SetTop;
    property WordWrapColTitles : boolean read FWordWrapColTitles write SetWordWrapColTitles Stored False;
    property UnselectableColor : TColor read FUnselectableColor write SetUnselectableColor;
    Property OnButtonClick: TGetEditEvent read FOnButtonClick write FOnButtonClick;
    property OnStateChange: TChangeCheckEvent read FOnStateChange write FOnStateChange;
    { Published declarations }
  end;

  TRbwInplaceEdit2 = class(TInplaceEdit)
    procedure BoundsChanged; override;
  end;
  // TRbwInplaceEdit2 is used to gain access to the protected UpdateContents
  // procedure.

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RBW', [TRbwDataGrid2]);
end;

{ TRbwColumn2 }

procedure TRbwColumn2.Assign(Source: TPersistent);
begin
  if Source is TRbwColumn2 then
  begin
    if Assigned(Collection) then Collection.BeginUpdate;
    try
      LimitToList := TRbwColumn2(Source).LimitToList;
      MaxLength := TRbwColumn2(Source).MaxLength;
      Format := TRbwColumn2(Source).Format;
      PickList := TRbwColumn2(Source).PickList;
      Min := TRbwColumn2(Source).Min;
      Max := TRbwColumn2(Source).Max;
      CheckMin := TRbwColumn2(Source).CheckMin;
      CheckMax := TRbwColumn2(Source).CheckMax;
      ButtonCaption := TRbwColumn2(Source).ButtonCaption;
      ButtonWidth := TRbwColumn2(Source).ButtonWidth;
      UseButton := TRbwColumn2(Source).UseButton;
      CaptionAlignment := TRbwColumn2(Source).CaptionAlignment;
    finally
      if Assigned(Collection) then Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TRbwColumn2.CheckCell(const ACol, ARow: integer);
var
  IntValue : integer;
  RealValue : extended;
  AGrid : TRbwDataGrid2;
  CellValue : string;
begin
  if not CheckMax and not CheckMin then Exit;
  if not (Format in [rcf2Integer, rcf2Real]) then Exit;
  AGrid := Grid;
  if (ACol < AGrid.FixedCols) or (ARow < AGrid.FixedRows) then Exit;
  CellValue := AGrid.Cells[ACol, ARow];
  if (CellValue = '') then Exit;
  if Format = rcf2Integer then
  begin
    IntValue := 0;
    try
      IntValue := StrToInt(CellValue);
    except on EConvertError do
      begin
        AGrid.Cells[ACol, ARow] := '0';
        CheckCell(ACol, ARow);
      end;
    end;

    if CheckMax and (IntValue > Max) then
    begin
      IntValue := Trunc(Max);
      if (Max < 0) and (IntValue <> Max) then
      begin
        Dec(IntValue);
      end;
      Beep;
      AGrid.Cells[ACol, ARow] := IntToStr(IntValue);
    end;
    if CheckMin and (IntValue < Min) then
    begin
      IntValue := Trunc(Min);
      if (Min > 0) and (IntValue <> Min) then
      begin
        Inc(IntValue);
      end;
      Beep;
      AGrid.Cells[ACol, ARow] := IntToStr(IntValue);
    end;
  end
  else if Format = rcf2Real then
  begin
    RealValue := 0;
    try
      RealValue := AGrid.LocalStrToFloat(CellValue);
    except on EConvertError do
      begin
        AGrid.Cells[ACol, ARow] := '0';
        CheckCell(ACol, ARow);
      end;
    end;
    if CheckMax and (RealValue > Max) then
    begin
      Beep;
      AGrid.Cells[ACol, ARow] := FloatToStr(Max);
    end;
    if CheckMin and (RealValue < Min) then
    begin
      Beep;
      AGrid.Cells[ACol, ARow] := FloatToStr(Min);
    end;
  end;

end;

function TRbwColumn2.Column: integer;
var
  AGrid : TRbwDataGrid2;
  Index : Integer;
begin
  AGrid := Grid;
  result := -1;
  for Index := 0 to AGrid.ColCount -1 do
  begin
    if AGrid.Columns[Index] = self then
    begin
      result := Index;
      break;
    end;
  end;
end;

procedure TRbwColumn2.CheckRange;
var
  ACol, ARow: integer;
  Index : Integer;
  AGrid : TRbwDataGrid2;
begin
  if not CheckMax and not CheckMin then Exit;
  AGrid := Grid;
  if (Format in [rcf2Integer, rcf2Real]) and (AGrid <> nil) then
  begin
    ACol := -1;
    for Index := 0 to AGrid.ColCount -1 do
    begin
      if AGrid.Columns[Index] = self then
      begin
        ACol := Index;
        break;
      end;
    end;
    if ACol > AGrid.FixedCols then
    begin
      for ARow := AGrid.FixedRows to AGrid.RowCount -1 do
      begin
        CheckCell(ACol, ARow);
      end;
    end;
  end;
end;

constructor TRbwColumn2.Create(Collection: TCollection);
var
  TempPickList: TStringList;
begin
  inherited;
  FButtonCaption := '...';
  FButtonWidth := 20;
  FUseButton := False;
  TempPickList := TStringList.Create;
  TempPickList.OnChange := PickListChanged;
  FPickList := TempPickList;
  FCaptionAlignment := taCenter;
  if not Grid.Updating and not (csLoading in Grid.ComponentState) then
  begin
    Grid.Updating := True;
    Grid.ColCount := Collection.Count;
    Grid.Updating := False;
  end;
end;

destructor TRbwColumn2.Destroy;
begin
  FPickList.Free;
  if not Grid.Updating and (([csLoading, csDestroying] * Grid.ComponentState) = []) then
  begin
    Grid.Updating := True;
    Grid.ColCount := Collection.Count-1;
    Grid.Updating := False;
  end;
  inherited;
end;

function TRbwColumn2.GetGrid: TRbwDataGrid2;
begin
  if Assigned(Collection) and (Collection is TRbwDataGridColumns2) then
    Result := TRbwDataGridColumns2(Collection).Grid
  else
    Result := nil;
end;


procedure TRbwColumn2.SetAutoAdjustColWidths(const Value: boolean);
var
  ACol: integer;
begin
  if FAutoAdjustColWidths <> Value then
  begin
    FAutoAdjustColWidths := Value;
    if Value then
    begin
      ACol := Index;
      if ACol >= 0 then
      begin
        (Grid as TRbwDataGrid2).AdjustColWidths(ACol);
      end;
    end;
  end;
end;

procedure TRbwColumn2.SetAutoAdjustRowHeights(const Value: boolean);
var
  ARow: integer;
begin
  if FAutoAdjustRowHeights <> Value then
  begin
    FAutoAdjustRowHeights := Value;
    if Value then
    begin
      ARow := (Grid as TRbwDataGrid2).Row;
      if ARow >= 0 then
      begin
        (Grid as TRbwDataGrid2).AdjustRowHeights(ARow);
      end;
    end;
  end;
end;

procedure TRbwColumn2.SetButtonCaption(const Value: string);
begin
  FButtonCaption := Value;
  Changed(False);
end;

procedure TRbwColumn2.SetButtonWidth(const Value: integer);
begin
  if Value = FButtonWidth then Exit;
  FButtonWidth := Value;
  Changed(False);
end;

procedure TRbwColumn2.SetCaptionAlignment(const Value: TAlignment);
begin
  FCaptionAlignment := Value;
end;

procedure TRbwColumn2.SetCheckMax(const Value: boolean);
begin
  if (FCheckMax <> Value) then FCheckMax := Value;
  if FCheckMax then CheckRange;
end;

procedure TRbwColumn2.SetCheckMin(const Value: boolean);
begin
  if (FCheckMin <> Value) then FCheckMin := Value;
  if FCheckMin then CheckRange;
end;

procedure TRbwColumn2.SetFormat(const Value: TRbwColumnFormat2);
begin
  if Value = FFormat then Exit;
  FFormat := Value;
  if FFormat = rcf2Combo then
  begin
    FUseButton := False;
  end;
  Changed(False);
end;


procedure TRbwColumn2.SetMax(const Value: extended);
begin
  if FMax <> Value then
  begin
    if Value >= FMin then
    begin
      FMax := Value;
      If CheckMax then
      begin
        CheckRange;
      end;
     end
    else
    begin
      FMin := Value;
      FMax := Value;
      If CheckMax or CheckMin then
      begin
        CheckRange;
      end;
    end;
  end;
end;

procedure TRbwColumn2.SetMin(const Value: extended);
begin
  if FMin <> Value then
  begin
    if Value <= FMax then
    begin
      FMin := Value;
      If CheckMin then
      begin
        CheckRange;
      end;
     end
    else
    begin
      FMin := Value;
      FMax := Value;
      If CheckMax or CheckMin then
      begin
        CheckRange;
      end;
    end;
  end;
end;

procedure TRbwColumn2.SetPickList(const Value: TStrings);
begin
  FPickList.Assign(Value);
  if Column = Grid.FdgColumn then
  begin
    Grid.SetControlPosition;
  end; 

end;


procedure TRbwColumn2.SetUseButton(const Value: boolean);
begin
  FUseButton := Value;
  if FUseButton and (FFormat = rcf2Combo) then
  begin
    FFormat := rcf2String;
  end;
  Changed(False);
end;

procedure TRbwColumn2.SetWordWrapCaptions(const Value: boolean);
begin
  FWordWrapCaptions := Value;
  Grid.Invalidate;
end;

procedure TRbwColumn2.SetWordWrapCells(const Value: boolean);
begin
  FWordWrapCells := Value;
  Grid.Invalidate;
end;

procedure TRbwColumn2.PickListChanged(Sender: TObject);
begin
  if (Format = rcf2Combo) and (Column = Grid.Column)
    and (Grid.Combo <> nil) then
  begin
    Grid.Combo.Items := PickList;
  end;
end;

{ TRbwDataGridColumns2 }

function TRbwDataGridColumns2.Add: TRbwColumn2;
begin
  Result := TRbwColumn2(inherited Add);
  if not Grid.Updating and not (csLoading in FGrid.ComponentState) then
  begin
    FGrid.Updating := True;
    FGrid.ColCount := Count;
    FGrid.Updating := False;
  end;
end;


constructor TRbwDataGridColumns2.Create(Grid: TRbwDataGrid2;
  ColumnClass: TRbwColumnClass2);
begin
  inherited Create(ColumnClass);
  Destroying := False;
  FGrid := Grid;
end;

destructor TRbwDataGridColumns2.Destroy;
begin
  Destroying := True;
  inherited;
end;

function TRbwDataGridColumns2.GetItems(Index: Integer): TRbwColumn2;
begin
  Result := inherited Items[Index] as TRbwColumn2;
end;

function TRbwDataGridColumns2.GetOwner: TPersistent;
begin
  Result := FGrid;
end;


procedure TRbwDataGridColumns2.SetItems(Index: Integer;
  const Value: TRbwColumn2);
begin
  Items[Index].Assign(Value);
end;

procedure TRbwDataGridColumns2.Update(Item: TCollectionItem);
begin
  inherited;
  if (FGrid = nil) or (csLoading in FGrid.ComponentState) then Exit;

  if (csDesigning in FGrid.ComponentState) then FGrid.invalidate
  else FGrid.invalidatecol(FGrid.dgColumn);
end;

{ TRbwDataGrid2 }

procedure TRbwDataGrid2.AdjustColWidths(const ACol: integer);
var
  Index: integer;
  RequiredWidth, TestWidth: integer;
begin
  if Columns[ACol].AutoAdjustColWidths then
  begin
    RequiredWidth := 0;
    Canvas.Font.Assign(Font);
    for Index := 0 to RowCount -1 do
    begin
      TestWidth := RequiredCellWidth(ACol, Index);
      if TestWidth > RequiredWidth then
      begin
        RequiredWidth := TestWidth
      end;
    end;
    if Columns[ACol].Format = rcf2Combo then
    begin
      for Index := 0 to Columns[ACol].PickList.Count -1 do
      begin
        TestWidth := Canvas.TextWidth(Columns[ACol].PickList[Index]) + 24;
        if TestWidth > RequiredWidth then
        begin
          RequiredWidth := TestWidth
        end;
      end;
    end;

    if ColWidths[ACol] < RequiredWidth then
    begin
      ColWidths[ACol] := RequiredWidth
    end;
  end;
end;

function TRbwDataGrid2.CanEditShow: Boolean;
var
  Column : TRbwColumn2;
begin
  result := inherited CanEditShow and (Col >= FixedCols) and (Row >= FixedRows);
  if result then
  begin
    if (dgColumn >=0) and (dgColumn < ColCount) then
    begin
      Column := Columns[dgColumn];
      result := not (Column.Format in [rcf2Boolean, rcf2Combo]);
    end;
  end;
end;

procedure TRbwDataGrid2.ColumnMoved(FromIndex, ToIndex: Integer);
var
  ColIndex: integer;
  RowIndex: integer;
  Temp: TCheckBoxState;
begin
  inherited;
  Columns[FromIndex].Index := ToIndex;
  if FromIndex < ToIndex then
  begin
    for RowIndex := 0 to RowCount -1 do
    begin
      Temp := FChecked[FromIndex, RowIndex];
      for ColIndex := FromIndex to ToIndex -1 do
      begin
        FChecked[ColIndex, RowIndex] := FChecked[ColIndex+1, RowIndex];
      end;
      FChecked[ToIndex, RowIndex] := Temp;
    end;
  end
  else
  begin
    for RowIndex := 0 to RowCount -1 do
    begin
      Temp := FChecked[FromIndex, RowIndex];
      for ColIndex := FromIndex downto ToIndex +1 do
      begin
        FChecked[ColIndex, RowIndex] := FChecked[ColIndex-1, RowIndex];
      end;
      FChecked[ToIndex, RowIndex] := Temp;
    end;
  end;
end;

procedure TRbwDataGrid2.ColWidthsChanged;
var
  Index: integer;
begin
  inherited;
  SetControlPosition;
  for Index := 0 to FixedRows-1 do
  begin
    InvalidateRow(Index);
  end;
end;

procedure TRbwDataGrid2.ComboChange(Sender: TObject);
begin
  Cells[dgColumn, dgRow] := Combo.Text;
  if Assigned(OnSetEditText) then
  begin
    OnSetEditText(self, dgColumn, dgRow, Combo.Text);
  end;
end;

procedure TRbwDataGrid2.ComboExit(Sender: TObject);
begin
  Combo.Visible := False;
end;

constructor TRbwDataGrid2.Create(AOwner: TComponent);
var
  ARect : TRect;
  Index : integer;
begin
  inherited;

  SetLength(FChecked, 5, 5);

  bmpUnchecked := TBitMap.Create;
  with bmpUnchecked do
  begin
    Width := 13;
    Height := 13;
    PixelFormat := pf8bit;

    Canvas.Brush.Color := clWhite;
    ARect.Top := 1;
    ARect.Left := 1;
    ARect.Right := 14;
    ARect.Bottom := 14;
    Canvas.FillRect(ARect);

    Canvas.Pen.Color := $848284;
    Canvas.MoveTo(1,12);
    Canvas.LineTo(1,1);
    Canvas.LineTo(12,1);
    Canvas.MoveTo(2,11);
    Canvas.LineTo(2,2);
    Canvas.LineTo(11,2);

    Canvas.Pen.Color := $C6C3C6;
    Canvas.MoveTo(2,12);
    Canvas.LineTo(12,12);
    Canvas.LineTo(12,2);
  end;

  bmpChecked := TBitMap.Create;
  with bmpChecked do
  begin
    Assign(bmpUnchecked);

    Canvas.Pen.Color := clBlack;
    Canvas.MoveTo(4,6);
    Canvas.LineTo(6,8);
    Canvas.LineTo(10,4);
    Canvas.MoveTo(4,7);
    Canvas.LineTo(6,9);
    Canvas.LineTo(10,5);
  end;

  FColumns := CreateColumns;
  for Index := 0 to ColCount-1 do
  begin
    FColumns.Add;
  end;
  FixedCols := 1;
  Options := Options + [goEditing] -[goDrawFocusSelected];
  UnselectableColor := FixedColor;
  SelectedRowColor := clAqua;
  ColorSelectedRow := True;

  FdgColumn := 1;
  FdgRow := 1;
  FSelectedRow := FdgRow;
  if not (csDesigning in ComponentState) then
  begin
    combo := TRbwComboBox2.Create(self);
    Button := TButton.Create(self);
  end;
end;

function TRbwDataGrid2.CreateColumns: TRbwDataGridColumns2;
begin
  Result := TRbwDataGridColumns2.Create(Self, TRbwColumn2);
end;


function TRbwDataGrid2.CreateEditor: TInplaceEdit;
begin
  Result := TRbwInplaceEdit2.Create(Self);

end;

destructor TRbwDataGrid2.Destroy;
begin
  bmpUnchecked.Free;
  bmpChecked.Free;
  FColumns.Free;
  inherited;
end;


procedure TRbwDataGrid2.DoExit;
begin
  inherited;
  if (dgColumn >= 0) and (dgColumn < ColCount)
    and (dgRow >= 0) and (dgRow < RowCount) then
  begin
    Columns[dgColumn].CheckCell(dgColumn, dgRow);
  end;
end;

procedure TRbwDataGrid2.DrawColumnTitleCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
var
  FontColor : TColor;
begin
  Canvas.Brush.Color := FixedColor;

  ARect.Left := ARect.Left - GridLineWidth div 2 + 1;
  ARect.Top := ARect.Top - GridLineWidth div 2 + 1;
  ARect.Right := ARect.Right + GridLineWidth div 2 - 1;
  ARect.Bottom := ARect.Bottom + GridLineWidth div 2 - 1;

  Canvas.Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);


  ARect.Left := ARect.Left + GridLineWidth div 2 + 2;
  ARect.Top := ARect.Top + GridLineWidth div 2 + 2;
  ARect.Right := ARect.Right - GridLineWidth div 2 - 2;
  ARect.Bottom := ARect.Bottom - GridLineWidth div 2 - 2;

  FontColor := Canvas.Font.Color;
  try
    Canvas.Font.Color := Font.Color;
    DrawText(Canvas.Handle,PChar(Cells[ACol, ARow]),
      Length(Cells[ACol, ARow]),ARect, GetCaptionFlags(ACol));
  finally
    Canvas.Font.Color := FontColor;
  end;
end;

procedure TRbwDataGrid2.DrawOrdinaryCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
var
  FontColor : TColor;
  
begin
  InflateRect(ARect, 1, 1);

  Canvas.Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);

  InflateRect(ARect, -2, -2);

{  ARect.Left := ARect.Left - GridLineWidth div 2 + 1;
  ARect.Top := ARect.Top - GridLineWidth div 2 + 1;
  ARect.Right := ARect.Right + GridLineWidth div 2 - 1;
  ARect.Bottom := ARect.Bottom + GridLineWidth div 2 - 1;



  ARect.Left := ARect.Left + GridLineWidth div 2 + 2;
  ARect.Top := ARect.Top + GridLineWidth div 2 + 2;
  ARect.Right := ARect.Right - GridLineWidth div 2 - 2;
  ARect.Bottom := ARect.Bottom - GridLineWidth div 2 - 2; }

  FontColor := Canvas.Font.Color;
  try
    Canvas.Font.Color := Font.Color;
    DrawText(Canvas.Handle,PChar(Cells[ACol, ARow]),
      Length(Cells[ACol, ARow]),ARect, GetCellFlags(ACol));
  finally
    Canvas.Font.Color := FontColor;
  end;
end;

procedure TRbwDataGrid2.DrawCheckBoxCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
var
  OldStyle : TBrushStyle;
  Dest: TRect;
begin
  // draw checkbox;
  if not (gdFixed in AState) then
  begin
    if not (gdFixed in AState) then
    begin
      OldStyle := Canvas.Brush.Style;
      try
        Canvas.Brush.Style := bsSolid;
        Canvas.FillRect(ARect);
        Dest.Left := ARect.Left + 2;
        Dest.Top := ARect.Top + (ARect.Bottom - ARect.Top - 13) div 2;
        Dest.Right := Dest.Left + 13;
        Dest.Bottom := Dest.Top + 13;
        if Checked[ACol, ARow] then
        begin
          canvas.copyrect(Dest, bmpChecked.canvas, Rect(0,0,14,14));
        end
        else
        begin
          canvas.copyrect(Dest, bmpUnChecked.canvas, Rect(0,0,13,13));
        end;
          Canvas.Font := Font;
          Canvas.Brush.Style := bsClear;
          Canvas.TextRect(ARect, ARect.Left + 17, ARect.Top + 2, Cells[ACol, ARow]);
      finally
        Canvas.Brush.Style := OldStyle;
      end;
    end;
  end;
end;

procedure TRbwDataGrid2.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
var
  Column : TRbwColumn2;
  BrushColor : TColor;
  FontColor : TColor;
  CanSelect : boolean;
begin
  inherited;
  FDrawing := True;
  BrushColor := Canvas.Brush.Color;
  FontColor := Canvas.Font.Color;
  try
    if (ARow < FixedRows) and Columns[ACol].WordWrapCaptions then
    begin
      DrawColumnTitleCell(ACol, ARow, ARect, AState);
    end
    else if (ACol >= FixedCols) and (ARow >= FixedRows) then
    begin
      CanSelect := True;
      Canvas.Brush.Color := Color;
      Canvas.Font.Color := Font.Color;
      if (goEditing in Options) and Assigned(OnSelectCell) then
      begin
        OnSelectCell(self, ACol, ARow, CanSelect);
      end;
      if not CanSelect then
      begin
        Canvas.Brush.Color := UnselectableColor;
      end
      else if ColorSelectedRow and (ARow = dgRow) then
      begin
        Canvas.Brush.Color := SelectedRowColor;
      end;

      Column := Columns[ACol];
      if Column.Format = rcf2Boolean then
      begin
        // draw checkbox;
        DrawCheckBoxCell(ACol, ARow, ARect, AState);
      end
      else if (ARow >= FixedRows) and (ACol >= FixedCols) then
      begin
        inherited;
        DrawOrdinaryCell(ACol, ARow, ARect, AState);
      end;
    end;

    if Assigned(OnDrawCell) then
    begin
      OnDrawCell(Self, ACol, ARow, ARect, AState);
    end;
  finally
    Canvas.Brush.Color := BrushColor;
    Canvas.Font.Color := FontColor;
    FDrawing := False;
  end;

end;

function TRbwDataGrid2.GetFixedCols: integer;
begin
  result := inherited FixedCols;
end;

function TRbwDataGrid2.GetCaptionFlags(const Col: integer): UINT;
begin
  result := DT_NOPREFIX;
  case Columns[Col].CaptionAlignment of
    taLeftJustify:
      begin
        result := result or DT_Left;
      end;
    taRightJustify:
      begin
        result := result or DT_Right;
      end;
    taCenter:
      begin
        result := result or DT_CENTER;
      end;
  else Assert(False);
  end;

  if Columns[Col].WordWrapCaptions then
  begin
    result := result or DT_WORDBREAK;
  end;
end;

function TRbwDataGrid2.GetCellFlags(const Col: integer): UINT;
begin
  result := DT_NOPREFIX;
  case Columns[Col].CaptionAlignment of
    taLeftJustify:
      begin
        result := result or DT_Left;
      end;
    taRightJustify:
      begin
        result := result or DT_Right;
      end;
    taCenter:
      begin
        result := result or DT_CENTER;
      end;
  else Assert(False);
  end;

  if Columns[Col].WordWrapCells then
  begin
    result := result or DT_WORDBREAK;
  end;
end;



function TRbwDataGrid2.GetLeft: integer;
begin
  result := inherited Left;
end;

function TRbwDataGrid2.GetTop: integer;
begin
  result := inherited Top;
end;

procedure TRbwDataGrid2.Loaded;
//var
//  iColCount : integer;
begin
  inherited;
  FixedCols := FFixedCols;
//  Col := 0;
//  Row := 0;
//  Combo.Visible := False;
    Updating := True;
//    iColCount := ColCount;
{    for iColCount := Columns.count downto ColCount do
    begin
      Columns[iColCount-1].Free;
    end;
//    while Columns.count > iColCount do Columns[ColCount-1].Free;
//    iColCount := ColCount;
    for iColCount := Columns.count to ColCount do
    begin
      Columns.Add;
    end; }

//    while Columns.count < iColCount do Columns.Add;
    Updating := False;
    SetControlPosition;
end;

function TRbwDataGrid2.LocalizeString(ANumberString : string) : string;
var
  DecimalPosition : integer;
begin
  if (DecimalSeparator = '.') then
  begin
    DecimalPosition := Pos(',', ANumberString);
    if DecimalPosition > 0 then
    begin
      ANumberString[DecimalPosition] := DecimalSeparator;
    end;
  end
  else
  begin
    DecimalPosition := Pos('.', ANumberString);
    if DecimalPosition > 0 then
    begin
      ANumberString[DecimalPosition] := DecimalSeparator;
    end;
  end;
  result := ANumberString;
end;

function TRbwDataGrid2.LocalStrToFloat(S: string): Extended;
begin
  if (S = '') then
  begin
    result := 0;
    Exit;
  end;
  result := StrToFloat(LocalizeString(S));
end;


procedure TRbwDataGrid2.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  ACol, ARow : integer;
  ARect : TRect;
  Column : TRbwColumn2;
  Offset : integer;
  CanSelect : boolean;
  NewCoord : TGridCoord;
  NewSelection : TGridRect;
begin
  inherited;
  MouseIsDown := True;
  MouseToCell(X, Y, ACol, ARow);
  CanSelect := inherited SelectCell(ACol, ARow);
  dgRow := ARow;
  dgColumn := ACol;
  if not CanSelect then
  begin
    EditorMode := False;
  end;
  if CanSelect then
  begin
    dgRow := ARow;
    dgColumn := ACol;
  end;
  SetControlPosition;
  if (ACol >= 0) and (ACol < ColCount) then
  begin
    if (Combo <> nil) and Combo.Visible then
    begin
      Combo.SetFocus;
    end;
    Column := Columns[ACol];
    if (ARow >= 0) and (ARow < RowCount)
      and (CanSelect or (Column.Format = rcf2Boolean)) then
    begin
      NewCoord.X := ACol;
      NewCoord.Y := ARow;
      NewSelection.TopLeft := NewCoord;
      NewSelection.BottomRight := NewCoord;
      inherited Selection := NewSelection;
    end;
    if (Column.Format = rcf2Boolean) and (ACol >= FixedCols) and (ARow >= FixedRows) then
    begin
      EditorMode := False;
      HideEditor;
      ARect :=CellRect(ACol, ARow);
      Offset := (ARect.Bottom - ARect.Top - 13) div 2;
      if (goEditing in Options) and
        (Y >= ARect.Top + Offset) and
        (Y <= ARect.Bottom - Offset) and
        (X >= ARect.Left + Offset) and
        (X <= ARect.Left + Offset + 13) then
      begin
        Checked[ACol, ARow] := not Checked[ACol, ARow];
      end;
      DrawCell(ACol, ARow, ARect, [gdSelected]);
    end;
  end;
  Invalidate;
  MouseIsDown := False;
end;

procedure TRbwDataGrid2.RowHeightsChanged;
begin
  inherited;
  SetControlPosition;
end;

function TRbwDataGrid2.SelectCell(ACol, ARow: Integer): Boolean;
var
  Column : TRbwColumn2;
begin
  result := inherited SelectCell(ACol, ARow);

  if result and not MouseIsDown then
  begin
    if FSelectedRow <> ARow then
    begin
      FSelectedRow := ARow;
      Invalidate;
    end;

    Column := Columns[ACol];
    if (Column.Format = rcf2Boolean) or (Column.Format = rcf2Combo) then
    begin
      result := False;
    end
  end;
  dgColumn := ACol;
  dgRow := ARow;
end;

procedure TRbwDataGrid2.SetColorSelectedRow(const Value: boolean);
begin
  if FColorSelectedRow <> Value then
  begin
    FColorSelectedRow := Value;
    Invalidate;
  end;
end;

procedure TRbwDataGrid2.SetColumns(const Value: TRbwDataGridColumns2);
begin
  FColumns.Assign(Value);
end;

procedure TRbwDataGrid2.SetControlPosition;
var
  ARect : TRect;
  FixedHeight, FixedWidth : integer;
  Index : integer;
  ControlTop: Integer;
  ControlLeft : integer;
  ControlWidth : integer;
  NewWidth, AWidth : integer;
  ParentControl : TWinControl;
  Form : TForm;
  AFont : TFont;
  NewCoord : TGridCoord;
  NewSelection : TGridRect;
begin
  if (Parent <> nil)
    and not (csDesigning in ComponentState)
    and not (csLoading	 in ComponentState)
    and (dgColumn >=0) and (dgColumn < ColCount)
    and (dgRow >=0) and (dgRow < RowCount)
    and ((Columns[dgColumn].Format = rcf2Combo) or Columns[dgColumn].UseButton) then
  begin
    ARect := CellRect(dgColumn, dgRow);
    ControlLeft := ARect.Left + Left + 2;
    ControlTop := ARect.Top + Top + 2;
    ControlWidth := ARect.Right - ARect.Left;
    if Columns[dgColumn].UseButton and (ControlWidth > Columns[dgColumn].ButtonWidth) then
    begin
      ControlLeft := ControlLeft + ControlWidth - Columns[dgColumn].ButtonWidth;
      ControlWidth := Columns[dgColumn].ButtonWidth;
    end;

    FixedHeight := 0;
    for Index := 0 to FixedRows -1 do
    begin
      FixedHeight := FixedHeight + RowHeights[Index]
    end;
    FixedWidth := 0;
    for Index := 0 to FixedCols -1 do
    begin
      FixedWidth := FixedWidth + ColWidths[Index]
    end;

    if (goEditing in Options) and
      (ControlTop >= Top + FixedHeight) and
      (ControlLeft >= Left + FixedWidth) and
      (ControlLeft + ControlWidth <= Left + Width) then
    begin
      if Columns[dgColumn].UseButton then
      begin
        if Button = nil then
        begin
          Button := TButton.Create(self);
          Button.Parent := Parent;
        end;
        Button.Left := ControlLeft;
        Button.Top := ControlTop;
        Button.Width := ControlWidth;
        Button.OnClick := ButtonClick;
        Button.Font := Font;
        Button.Caption := Columns[dgColumn].ButtonCaption;

        if (Button.Top  >= Top)
            and (Button.Top + Button.Height <= Top + Height) and Enabled then
        begin
          Button.Visible := True;
        end
        else
        begin
          NewCoord.X := dgColumn;
          NewCoord.Y := dgRow;
          NewSelection.TopLeft := NewCoord;
          NewSelection.BottomRight := NewCoord;
          inherited Selection := NewSelection;
          ARect := CellRect(dgColumn, dgRow);
          ControlTop := ARect.Top + Top + 2;
          if Button <> nil then
          begin
            Button.Visible := False;
            Button.Top := ControlTop;
          end;
        end;
      end
      else
      begin
        if (Combo = nil) then
        begin
          combo := TRbwComboBox2.Create(self);
          combo.Parent := Parent;
        end;
        Combo.Left := ControlLeft;
        Combo.Top := ControlTop;
        Combo.Width := ControlWidth;
        Combo.Items.Assign(Columns[dgColumn].PickList);
        Combo.MaxLength := Columns[dgColumn].MaxLength;
        if Columns[dgColumn].LimitToList then
        begin
          Combo.Style := csDropDownList;
          Combo.ItemIndex := Combo.Items.IndexOf(Cells[dgColumn, dgRow]);
        end
        else
        begin
          Combo.Style := csDropDown;
          Combo.Text := Cells[dgColumn, dgRow];
        end;
        Combo.OnChange := ComboChange;
        Combo.OnExit := ComboExit;
        Combo.Font := Font;

        if RowHeights[dgRow] < Combo.Height then
        begin
          RowHeights[dgRow] := Combo.Height
        end;

        if (Combo.Top  >= Top)
            and (Combo.Top + Combo.Height <= Top + Height) and Enabled then
        begin
          Combo.Visible := True;

          ParentControl := Parent;
          Form := nil;
          While ParentControl <> nil do
          begin
            if ParentControl is TForm then
            begin
              Form := ParentControl as TForm;
              Break;
            end;
            ParentControl := ParentControl.Parent;
          end;
          if Form <> nil then
          begin
            AFont := Form.Canvas.Font;
            try
              Form.Canvas.Font := Font;
              NewWidth := Combo.Width;
              for Index := 0 to Combo.Items.Count -1 do
              begin
                AWidth := Form.Canvas.TextWidth(Combo.Items[Index]) + 25;
                if AWidth > NewWidth then
                begin
                  NewWidth := AWidth;
                end;
              end;

              if NewWidth > Combo.Width then
              begin
                Combo.CWX := NewWidth - Combo.Width;
              end
              else
              begin
                Combo.CWX := 0;
              end;
            finally
              Form.Canvas.Font := AFont;
            end;
          end;

        end
        else
        begin
          NewCoord.X := dgColumn;
          NewCoord.Y := dgRow;
          NewSelection.TopLeft := NewCoord;
          NewSelection.BottomRight := NewCoord;
          inherited Selection := NewSelection;
          ARect := CellRect(dgColumn, dgRow);
          ControlTop := ARect.Top + Top + 2;
          if Combo <> nil then
          begin
            Combo.Visible := False;
            Combo.Top := ControlTop;
          end;
        end;
      end;
    end
    else
    begin
      if Combo <> nil then
      begin
        Combo.Visible := False;
      end;
      If Button <> nil then
      begin
        Button.Visible := False;
      end
    end;
  end
  else
  begin
    if Combo <> nil then
    begin
      Combo.Visible := False;
    end;
    If Button <> nil then
    begin
      Button.Visible := False;
    end
  end;
end;

procedure TRbwDataGrid2.SetdgColumn(const Value: integer);
begin
  if (FdgColumn >= 0) and (FdgColumn < ColCount) then
  begin
    Columns[FdgColumn].CheckCell(FdgColumn, FdgRow);
  end;
  FdgColumn := Value;
end;

procedure TRbwDataGrid2.SetdgRow(const Value: integer);
begin
  if (FdgColumn >= 0) and (FdgColumn < ColCount) then
  begin
    Columns[FdgColumn].CheckCell(FdgColumn, FdgRow);
  end;
  FdgRow := Value;
  FSelectedRow := Value;
end;

procedure TRbwDataGrid2.SetEditText(ACol, ARow: Integer;
  const Value: string);
var
  Column : TRbwColumn2;
  IntValue : integer;
  NewValue : string;
  ValString : PChar;
  ConversionOK : boolean;
  AFloat : double;
  E : integer;
begin
  if AutoDistributeText then
  begin
    if DistributeText(ACol, ARow, Value) then
    begin
      Exit;
    end;
  end;

  Column := Columns[ACol];
  case Column.Format of
    rcf2String:
      begin
        if (Column.MaxLength > 0) and (Length(Value) > Column.MaxLength) then
        begin
          inherited SetEditText(ACol, ARow, Copy(Value, 1, Column.MaxLength));
          (InplaceEditor as TRbwInplaceEdit2).UpdateContents;
          (InplaceEditor as TRbwInplaceEdit2).SelStart := Column.MaxLength;
        end
        else
        begin
          inherited;
        end;
      end;
    rcf2Integer:
      begin
        NewValue := Value;
        if Value <> '' then
        begin
          Val(Value, IntValue, E);
          if E <> 0 then
          begin
            NewValue := Copy(Value,1,E-1);
          end;
        end;
        inherited SetEditText(ACol, ARow, NewValue);
        if Value <> NewValue then
        begin
          (InplaceEditor as TRbwInplaceEdit2).UpdateContents;
          (InplaceEditor as TRbwInplaceEdit2).SelStart := E;
        end;
      end;
    rcf2Real:
      begin
        NewValue := Value;
        GetMem(ValString, Length(NewValue) + 1);
        try
          if (Value <> '') and (Value <> '-') and (Value <> '+') then
          begin
            ConversionOK := false;
            While not ConversionOK do
            begin
              StrPCopy(ValString, NewValue);
              ConversionOK := TextToFloat(ValString, AFloat, fvExtended);

              if Not ConversionOK and (Length(NewValue) > 0) then
              begin
                NewValue := Copy(NewValue,1,Length(NewValue)-1);
              end;

              {if the string is '', convert it to '0'.}
              if Length(NewValue) = 0 then
              begin
                NewValue := '0'
              end;
            end;
          end;
        finally
          FreeMem(ValString);
        end;

        inherited SetEditText(ACol, ARow, NewValue);
        if Value <> NewValue then
        begin
          (InplaceEditor as TRbwInplaceEdit2).UpdateContents;
          (InplaceEditor as TRbwInplaceEdit2).SelStart := Length(NewValue);
        end;
      end;
    rcf2Boolean:
      begin
        inherited;
      end;
    rcf2Combo:
      begin
        if not Column.LimitToList and (Column.MaxLength > 0)
          and (Length(Value) > Column.MaxLength) then
        begin
          inherited SetEditText(ACol, ARow, Copy(Value, 1, Column.MaxLength));
          (InplaceEditor as TRbwInplaceEdit2).UpdateContents;
          (InplaceEditor as TRbwInplaceEdit2).SelStart := Column.MaxLength;
        end
        else
        begin
          inherited;
        end;
      end;
  else Assert(False);
  end;
end;

procedure TRbwDataGrid2.SetFixedCols(const Value: integer);
begin
  FFixedCols := Value;
  inherited FixedCols := Value;
end;


procedure TRbwDataGrid2.SetLeft(const Value: integer);
begin
  inherited Left := Value;
  SetControlPosition;
end;

procedure TRbwDataGrid2.SetParent(AParent: TWinControl);
begin
  Inherited;
  if (AParent <> nil) then
  begin
    if (Combo <> nil) then
      Combo.Parent := AParent;
    if (Button <> nil) then
      Button.Parent := AParent;
    SetControlPosition;
  end;
end;

procedure TRbwDataGrid2.SetSelectedRowColor(const Value: TColor);
begin
  if FSelectedRowColor <> Value then
  begin
    FSelectedRowColor := Value;
    Invalidate;
  end;
end;

procedure TRbwDataGrid2.SetTop(const Value: integer);
begin
  inherited Top := Value;
  SetControlPosition;
end;

procedure TRbwDataGrid2.SetUnselectableColor(const Value: TColor);
begin
  if FUnselectableColor <> Value then
  begin
    FUnselectableColor := Value;
    Invalidate;
  end;
end;

procedure TRbwDataGrid2.SetWordWrapColTitles(const Value: boolean);
var
  Index: integer;
begin
  FWordWrapColTitles := Value;
  for Index := 0 to Columns.Count -1 do
  begin
    Columns[Index].WordWrapCaptions := Value;
  end;
  Invalidate;

{  FWordWrapColTitles := Value;
  if FWordWrapColTitles <> Value then
  begin
    FWordWrapColTitles := Value;
//    if not Drawing then
    begin
      Invalidate;
    end;
  end;   }
end;

procedure TRbwDataGrid2.SizeChanged(OldColCount, OldRowCount: Integer);
//var
//  iColCount : integer;
begin
  inherited;
  if not (csLoading in ComponentState)
    and not (csReading in ComponentState)
    and not (csReadingState in ControlState)
    and not (csCreating in ControlState)
    and not Updating{and (FLayoutFlag = 0)} then
  begin
    Updating := True;
//    iColCount := ColCount;
{    for iColCount := Columns.count downto ColCount do
    begin
      Columns[iColCount-1].Free;
    end; }
    while Columns.count > ColCount do Columns[ColCount-1].Free;
//    iColCount := ColCount;
{    for iColCount := Columns.count to ColCount do
    begin
      Columns.Add;
    end;}

    while Columns.count < ColCount do Columns.Add;
    Updating := False;
  end;
end;

procedure TRbwDataGrid2.WMHScroll(var Msg: TWMHScroll);
begin
  inherited;
  SetControlPosition;
end;

procedure TRbwDataGrid2.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  SetControlPosition;
end;

procedure TRbwDataGrid2.DeleteRow(ARow: Integer);
begin
  inherited DeleteRow(ARow);
end;

function TRbwDataGrid2.GetChecked(const ACol, ARow: integer): boolean;
begin
  result := CheckState[ACol, ARow] = cbChecked;
end;

procedure TRbwDataGrid2.SetChecked(const ACol, ARow: integer;
  const Value: boolean);
begin
  if Value then
  begin
    CheckState[ACol, ARow] := cbChecked;
  end
  else
  begin
    CheckState[ACol, ARow] := cbUnchecked;
  end;
end;

function TRbwDataGrid2.GetColCount: Longint;
begin
  result := inherited ColCount;
end;

function TRbwDataGrid2.GetRowCount: Longint;
begin
  result := inherited RowCount;
end;

procedure TRbwDataGrid2.SetColCount(const Value: Longint);
begin
  inherited ColCount := Value;
  SetLength(FChecked, ColCount, RowCount);
end;

procedure TRbwDataGrid2.SetRowCount(const Value: Longint);
begin
  inherited RowCount := Value;
  SetLength(FChecked, ColCount, RowCount);
end;

procedure TRbwDataGrid2.RowMoved(FromIndex, ToIndex: Integer);
var
  ColIndex: integer;
  RowIndex: integer;
  Temp: TCheckBoxState;
begin
  inherited;
  if FromIndex < ToIndex then
  begin
    for ColIndex := 0 to ColCount -1 do
    begin
      Temp := FChecked[ColIndex, FromIndex];
      for RowIndex := FromIndex to ToIndex -1 do
      begin
        FChecked[ColIndex, RowIndex] := FChecked[ColIndex, RowIndex+1];
      end;
      FChecked[ColIndex, ToIndex] := Temp;
    end;
  end
  else
  begin
    for ColIndex := 0 to ColCount -1 do
    begin
      Temp := FChecked[ColIndex, FromIndex];
      for RowIndex := FromIndex downto ToIndex +1 do
      begin
        FChecked[ColIndex, RowIndex] := FChecked[ColIndex, RowIndex-1];
      end;
      FChecked[ColIndex, ToIndex] := Temp;
    end;
  end;
end;

procedure TRbwDataGrid2.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);
  if dgColumn >= 0 then
  begin
    Col := dgColumn;
  end
  else
  begin
    dgColumn := Col
  end;
  if dgRow >= 0 then
  begin
    inherited Row := dgRow;
  end
  else
  begin
    dgRow := Row;
  end;
  SetControlPosition;
end;

procedure TRbwDataGrid2.ButtonClick(Sender: TObject);
var
  NewText: string;
begin
  if Assigned(FOnButtonClick) then
  begin
    NewText := Cells[dgColumn, dgRow];
    FOnButtonClick(self, dgColumn, dgRow, NewText);
    Cells[dgColumn, dgRow] := NewText;
    if Assigned(OnSetEditText) then
    begin
      OnSetEditText(self, dgColumn, dgRow, NewText);
    end;
  end;
end;

function TRbwDataGrid2.GetCheckState(const ACol,
  ARow: integer): TCheckBoxState;
begin
  if Columns[ACol].Format = rcf2Boolean then
  begin
    result := FChecked[ACol,ARow];
  end
  else
  begin
    result := cbUnchecked;
  end;
end;

procedure TRbwDataGrid2.SetCheckState(const ACol, ARow: integer;
  const Value: TCheckBoxState);
var
  Changed: boolean;
begin
  if Columns[ACol].Format = rcf2Boolean then
  begin
    Changed := FChecked[ACol,ARow] <> Value;
    if Changed then
    begin
      FChecked[ACol,ARow] := Value;
      if Assigned(FOnStateChange) then
      begin
        FOnStateChange(self, ACol, ARow, Value);
      end;
      Invalidate;
    end;
  end;
end;

function TRbwDataGrid2.RequiredCellWidth(const ACol,
  ARow: integer): integer;
var
  CellList: TStringList;
  CellCaption: String;
  CaptionIndex: integer;
  Temp: integer;
begin
  result := 0;
  if Columns[ACol].AutoAdjustColWidths then
  begin
    Canvas.Font.Assign(Font);
    if ((ARow < FixedRows) and Columns[ACol].WordWrapCaptions)
      or ((ARow >= FixedRows) and Columns[ACol].WordWrapCells) then
    begin
      CellList := TStringList.Create;
      try
        CellCaption := Cells[ACol, ARow];
        FillCaptionList(CellCaption, CellList, ColWidths[ACol]);
        for CaptionIndex := 0 to CellList.Count -1 do
        begin
          Temp := Canvas.TextWidth(CellList[CaptionIndex]) + 4;
          if Temp > result then
          begin
            result := Temp;
          end;
        end;
      finally
        CellList.Free;
      end;
    end
    else
    begin
      result := Canvas.TextWidth(Cells[ACol, ARow]) + 4;
    end;

    if (ARow >= FixedRows)
      and (Columns[ACol].Format in [rcf2Boolean, rcf2Combo]) then
    begin
      Inc(result, 20);
    end;
  end;
end;

procedure TRbwDataGrid2.FillCaptionList(CellCaption: string;
  const CaptionList: TStringList; Width: integer);
var
  SpacePosition: integer;
  MaxWidth: integer;
  Index: integer;
  JoinedLine: Boolean;
  NewLine: string;
begin
  CellCaption := Trim(CellCaption);
  SpacePosition := Pos(' ', CellCaption);
  while SpacePosition > 0 do
  begin
    CaptionList.Add(Copy(CellCaption, 1, SpacePosition-1));
    CellCaption := Trim(Copy(CellCaption,SpacePosition + 1, MAXINT));
    SpacePosition := Pos(' ', CellCaption);
  end;
  CaptionList.Add(CellCaption);
  MaxWidth := Width;
  Canvas.Font := Font;
  for Index := 0 to CaptionList.Count -1 do
  begin
    if Canvas.TextWidth(CaptionList[Index]) > MaxWidth then
    begin
      MaxWidth := Canvas.TextWidth(CaptionList[Index]);
    end;
  end;
  repeat
    JoinedLine := False;
    for Index := 0 to CaptionList.Count -2 do
    begin
      NewLine := CaptionList[Index] + ' ' + CaptionList[Index+1];
      if Canvas.TextWidth(NewLine) < MaxWidth then
      begin
        CaptionList[Index] := NewLine;
        CaptionList.Delete(Index + 1);
        JoinedLine := True;
        Break;
      end;
    end;
  until not JoinedLine;
end;

function TRbwDataGrid2.GetCells(ACol, ARow: Integer): string;
begin
  result := inherited Cells[ACol, ARow];
end;

procedure TRbwDataGrid2.SetCells(ACol, ARow: Integer; const Value: string);
var
  RequiredWidth, RequiredHeight: integer;
  SizeChanged: boolean;
begin
  SizeChanged:= False;
  if (ACol >= 0) and (ARow >= 0) then
  begin
    inherited Cells[ACol, ARow] := Value;
    RequiredWidth := RequiredCellWidth(ACol, ARow);
    if RequiredWidth > ColWidths[ACol] then
    begin
      ColWidths[ACol] := RequiredWidth;
      SizeChanged:= True;
    end;
    RequiredHeight := RequiredCellHeight(ACol, ARow);
    if RequiredHeight > RowHeights[ARow] then
    begin
      RowHeights[ARow] := RequiredHeight;
      SizeChanged:= True;
    end;
    if SizeChanged then
    begin
      SetControlPosition;
    end;
  end;
end;

function TRbwDataGrid2.RequiredCellHeight(const ACol,
  ARow: integer): integer;
var
  CellList: TStringList;
  CellCaption: string;
begin
  result := 2;
  Canvas.Font.Assign(Font);
  if Columns[ACol].AutoAdjustRowHeights then
  begin
    if ((ARow < FixedRows) and Columns[ACol].WordWrapCaptions)
      or ((ARow >= FixedRows) and Columns[ACol].WordWrapCells) then
    begin
      CellList := TStringList.Create;
      try
        CellCaption := Cells[ACol, ARow];
        FillCaptionList(CellCaption, CellList, ColWidths[ACol]);
        result := CellList.Count * (Canvas.TextHeight('0') + 2);
      finally
        CellList.Free;
      end;
    end
    else
    begin
      result := Canvas.TextHeight(Cells[ACol, ARow]) + 4;
    end;
  end;
  if Canvas.TextHeight('0') + 4 > result then
  begin
    result := Canvas.TextHeight('0') + 4;
  end;
end;

procedure TRbwDataGrid2.AdjustRowHeights(const ARow: integer);
var
  RequiredHeight, TestHeight: integer;
  ColIndex: integer;
begin
  for ColIndex := 0 to ColCount -1 do
  begin
    if (ColIndex >= Columns.Count) then Exit;
    if Columns[ColIndex].AutoAdjustRowHeights then
    begin
      RequiredHeight := 0;
      Canvas.Font.Assign(Font);
      TestHeight := RequiredCellHeight(ColIndex, ARow);
      if TestHeight > RequiredHeight then
      begin
        RequiredHeight := TestHeight
      end;

      if RowHeights[ARow] < RequiredHeight then
      begin
        RowHeights[ARow] := RequiredHeight
      end;
    end;
  end;
end;

function TRbwDataGrid2.DistributeText(const ACol, ARow: integer;
  CellContents: string): boolean;
var
  AStringList: TStringList;
  LineIndex: integer;
  AString: string;
  Row: integer;
  NewString: String;
  WordIndex: integer;
  function ExtractWord(var AString: string): string;
  var
    TabPos: integer;
  begin
    TabPos := Pos(#9, AString);
    if TabPos > 0 then
    begin
      result := Copy(AString, 1, TabPos -1);
      AString := Copy(AString, TabPos +1, MAXINT);
    end
    else
    begin
      result := AString;
      AString := '';
    end;
  end;
begin
  AStringList := TStringList.Create;
  try
    AStringList.Text := CellContents;
    result := (AStringList.Count > 1) or (Pos(#9, CellContents) > 0);
    if result then
    begin
      for LineIndex := 0 to AStringList.Count -1 do
      begin
        AString := AStringList[LineIndex];
        Row := ARow + LineIndex;
        WordIndex := 0;
        if AString = '' then
        begin
          Cells[ACol, Row] := '';
          SetEditText(ACol, Row, '');
        end
        else
        begin
          while Length(AString) > 0 do
          begin
            NewString := ExtractWord(AString);
            Cells[ACol + WordIndex, Row] := NewString;
            SetEditText(ACol + WordIndex, Row, NewString);
            Inc(WordIndex);
          end;
        end;
      end;
    end;

  finally
    AStringList.Free;
  end;
end;

function TRbwDataGrid2.GetSelection: TGridRect;
begin
  result := inherited Selection;
end;

procedure TRbwDataGrid2.SetSelection(const Value: TGridRect);
begin
  inherited Selection := Value;
  dgColumn := Value.Left;
  dgRow := Value.Top;
  SetControlPosition;
end; 

{ TRbwComboBox2 }

constructor TRbwComboBox2.Create(AOwner: TComponent);
begin
  inherited;
  Visible := False;
end;

{ TRbwInplaceEdit2 }

procedure TRbwInplaceEdit2.BoundsChanged;
var
  ParentGrid: TRbwDataGrid2;
  R: TRect;
begin
  ParentGrid := (Grid as TRbwDataGrid2);
  if (ParentGrid = nil) or (ParentGrid.Button = nil)
    or not ParentGrid.Button.Visible then
  begin
    inherited;
  end
  else
  begin
    R := Rect(2, 2, Width - 2 - ParentGrid.Columns[ParentGrid.Col].ButtonWidth, Height);
    SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@R));
    SendMessage(Handle, EM_SCROLLCARET, 0, 0);
  end;
end;

end.
