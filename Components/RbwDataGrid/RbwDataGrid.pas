unit RbwDataGrid;

interface

uses
  Windows, Messages, {Types,} SysUtils, Classes, Controls, StdCtrls, Grids,
    Graphics, Forms, siComboBox;

type
  TRbwColumnFormat = (rcfString, rcfInteger, rcfReal, rcfBoolean, rcfCombo);

  TRbwDataGrid = class;

  TRbwColumn = class(TCollectionItem)
  private
    FLimitToList: Boolean;
    FMaxLength: Integer;
    FFormat: TRbwColumnFormat;
    FPickList: TStrings;
    FUnCheckedString: string;
    FCheckedString: string;
    FCheckMin: boolean;
    FCheckMax: boolean;
    FMax: extended;
    FMin: extended;
    FDropDownCount: integer;
    FButtonCaption: string;
    FButtonFont: TFont;
    FButtonUsed: boolean;
    FButtonWidth: integer;
    FAutoAdjustColWidths: boolean;
    FAutoAdjustRowHeights: boolean;
    FWordWrapCaptions: boolean;
    FWordWrapCells: boolean;
    procedure SetFormat(const Value: TRbwColumnFormat);
    procedure SetPickList(const Value: TStrings);
    procedure SetCheckedString(const Value: string);
    procedure SetUnCheckedString(const Value: string);
    procedure SetCheckMax(const Value: boolean);
    procedure SetCheckMin(const Value: boolean);
    procedure SetMax(const Value: extended);
    procedure SetMin(const Value: extended);
    procedure CheckRange;
    Procedure CheckCell(const ACol, ARow : integer);
    procedure SetDropDownCount(const Value: integer);
    procedure SetButtonCaption(const Value: string);
    procedure SetButtonFont(const Value: TFont);
    procedure SetButtonUsed(const Value: boolean);
    procedure SetButtonWidth(const Value: integer);
    procedure SetAutoAdjustColWidths(const Value: boolean);
    procedure SetAutoAdjustRowHeights(const Value: boolean);
    procedure SetWordWrapCaptions(const Value: boolean);
    procedure SetWordWrapCells(const Value: boolean);
  protected
    function GetGrid: TRbwDataGrid;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property  Grid: TRbwDataGrid read GetGrid;
  published
    { TODO : Implement ButtonCaption }
    property ButtonCaption: string read FButtonCaption write SetButtonCaption;
    { TODO : Implement ButtonFont }
    property ButtonFont: TFont read FButtonFont write SetButtonFont;
    { TODO : Implement ButtonUsed }
    property ButtonUsed: boolean read FButtonUsed write SetButtonUsed;
    { TODO : Implement ButtonWidth }
    property ButtonWidth: integer read FButtonWidth write SetButtonWidth;
    { TODO : Implement AutoAdjustColWidths }
    property AutoAdjustColWidths: boolean read FAutoAdjustColWidths write SetAutoAdjustColWidths;
    { TODO : Implement AutoAdjustRowHeights }
    property AutoAdjustRowHeights: boolean read FAutoAdjustRowHeights write SetAutoAdjustRowHeights;
    { TODO : Implement WordWrapCaptions }
    property WordWrapCaptions: boolean read FWordWrapCaptions write SetWordWrapCaptions;
    { TODO : Implement WordWrapCells }
    property WordWrapCells: boolean read FWordWrapCells write SetWordWrapCells;

    property CheckMax : boolean read FCheckMax write SetCheckMax;
    property CheckMin : boolean read FCheckMin write SetCheckMin;
    property CheckedString : string read FCheckedString write SetCheckedString;
    property DropDownCount: integer read FDropDownCount write SetDropDownCount default 8;
    property Format: TRbwColumnFormat read FFormat write SetFormat;
    property LimitToList: Boolean read FLimitToList write FLimitToList;
    property MaxLength: Integer read FMaxLength write FMaxLength;
    Property Max : extended read FMax write SetMax;
    Property Min : extended read Fmin write Setmin;
    property PickList: TStrings read FPickList write SetPickList;
    property UnCheckedString : string read FUnCheckedString write SetUnCheckedString;
  end;

  TRbwColumnClass = class of TRbwColumn;

  TRbwDataGridColumns = class(TCollection)
  private
    FGrid: TRbwDataGrid;
    Destroying : boolean;
    function GetItems(Index: Integer): TRbwColumn;
    procedure SetItems(Index: Integer; const Value: TRbwColumn);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(Grid: TRbwDataGrid; ColumnClass: TRbwColumnClass);
    destructor Destroy; override;
    function  Add: TRbwColumn;
{    procedure LoadFromFile(const Filename: string);
    procedure LoadFromStream(S: TStream);
    procedure SaveToFile(const Filename: string);
    procedure SaveToStream(S: TStream); }
    property Grid: TRbwDataGrid read FGrid;
    property Items[Index: Integer]: TRbwColumn read GetItems write SetItems; default;
  end;

  TRbwComboBox = class(TsiComboBox)
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TRbwDataGrid = class(TStringGrid)
  private
    Drawing : boolean;
    MouseIsDown : boolean;
    bmpChecked : TBitMap;
    bmpUnchecked : TBitMap;
    FColumns: TRbwDataGridColumns;
    Combo : TRbwComboBox;
    FdgColumn: integer;
    FdgRow: integer;
    FWordWrapColTitles: boolean;
    Updating : boolean;
    FUnselectableColor: TColor;
    FFixedCols : integer;
    FColorSelectedRow: boolean;
    FSelectedRowColor: TColor;
    FSelectedRow: integer;
    FButtonColor: TColor;
    FButtonClicked: TSetEditEvent;
    function GetFlags : UINT;
    function GetLeft: integer;
    function GetTop: integer;
    function LocalizeString(ANumberString : string) : string;
    function LocalStrToFloat(S: string): Extended;
    procedure SetColorSelectedRow(const Value: boolean);
    procedure SetColumns(const Value: TRbwDataGridColumns);
    procedure SetComboPosition;
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
    { Private declarations }
  protected
    function CanEditShow: Boolean; override;
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    procedure ColWidthsChanged; override;
    procedure ComboChange(Sender: TObject);
    function  CreateColumns: TRbwDataGridColumns; dynamic;
    function CreateEditor: TInplaceEdit; override;
    property dgColumn : integer read FdgColumn write SetdgColumn;
    property dgRow : integer read FdgRow write SetdgRow;
//    procedure DoEnter; override;
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
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    procedure SetFixedCols(const Value : integer);
    procedure SetParent(AParent: TWinControl); override;
    procedure SizeChanged(OldColCount, OldRowCount: Longint); override;
    procedure SetEnabled(Value: Boolean); override;
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    Property SelectedRow : integer read FSelectedRow;
    procedure DeleteRow(ARow: Longint); override;
    { Public declarations }
  published
      { TODO : Implement ButtonColor }
    property ButtonColor: TColor read FButtonColor write FButtonColor;
      { TODO : Implement ButtonColor }
    property OnButtonClicked: TSetEditEvent read FButtonClicked
      write FButtonClicked;


    Property ColorSelectedRow : boolean read FColorSelectedRow write SetColorSelectedRow;
    property Columns : TRbwDataGridColumns read FColumns write SetColumns;
    property FixedCols : integer read GetFixedCols write SetFixedCols;
    property Left : integer read GetLeft write SetLeft;
    property SelectedRowColor : TColor read FSelectedRowColor write SetSelectedRowColor;
    property Top : integer read GetTop write SetTop;
    property WordWrapColTitles : boolean read FWordWrapColTitles write SetWordWrapColTitles;
    property UnselectableColor : TColor read FUnselectableColor write SetUnselectableColor;
    { Published declarations }
  end;

  TRbwInplaceEdit = class(TInplaceEdit);
  // TRbwInplaceEdit is used to gain access to the protected UpdateContents
  // procedure.

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RBW', [TRbwDataGrid]);
end;

{ TRbwColumn }

procedure TRbwColumn.Assign(Source: TPersistent);
begin
  if Source is TRbwColumn then
  begin
    if Assigned(Collection) then Collection.BeginUpdate;
    try
      LimitToList := TRbwColumn(Source).LimitToList;
      MaxLength := TRbwColumn(Source).MaxLength;
      Format := TRbwColumn(Source).Format;
      PickList := TRbwColumn(Source).PickList;
      UnCheckedString := TRbwColumn(Source).UnCheckedString;
      CheckedString := TRbwColumn(Source).CheckedString;
      Min := TRbwColumn(Source).Min;
      Max := TRbwColumn(Source).Max;
      CheckMin := TRbwColumn(Source).CheckMin;
      CheckMax := TRbwColumn(Source).CheckMax;
      DropDownCount := TRbwColumn(Source).DropDownCount;
    finally
      if Assigned(Collection) then Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TRbwColumn.CheckCell(const ACol, ARow: integer);
var
  IntValue : integer;
  RealValue : extended;
  AGrid : TRbwDataGrid;
  CellValue : string;
begin
  if not CheckMax and not CheckMin then Exit;
  if not (Format in [rcfInteger, rcfReal]) then Exit;
  AGrid := Grid;
  if (ACol < AGrid.FixedCols) or (ARow < AGrid.FixedRows) then Exit;
  CellValue := AGrid.Cells[ACol, ARow];
  if (CellValue = '') then Exit;
  if Format = rcfInteger then
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
  else if Format = rcfReal then
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

procedure TRbwColumn.CheckRange;
var
  ACol, ARow: integer;
  Index : Integer;
  AGrid : TRbwDataGrid;
begin
  if not CheckMax and not CheckMin then Exit;
  AGrid := Grid;
  if (Format in [rcfInteger, rcfReal]) and (AGrid <> nil) then
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

constructor TRbwColumn.Create(Collection: TCollection);
begin
  inherited;
  FButtonFont:= TFont.Create;
  FDropDownCount := 8;
  FPickList := TStringList.Create;
  UnCheckedString := 'No';
  CheckedString := 'Yes';
  if not Grid.Updating and not (csLoading in Grid.ComponentState) then
  begin
 
    Grid.Updating := True;
    Grid.ColCount := Collection.Count;
    Grid.Updating := False;
  end;
end;

destructor TRbwColumn.Destroy;
begin
  FPickList.Free;
  if not Grid.Updating and (([csLoading, csDestroying] * Grid.ComponentState) = []) then
  begin
    Grid.Updating := True;
    Grid.ColCount := Collection.Count-1;
    Grid.Updating := False;
  end;
  FButtonFont.Free;
  inherited;
end;

function TRbwColumn.GetGrid: TRbwDataGrid;
begin
  if Assigned(Collection) and (Collection is TRbwDataGridColumns) then
    Result := TRbwDataGridColumns(Collection).Grid
  else
    Result := nil;
end;

procedure TRbwColumn.SetAutoAdjustColWidths(const Value: boolean);
begin
  FAutoAdjustColWidths := Value;
end;

procedure TRbwColumn.SetAutoAdjustRowHeights(const Value: boolean);
begin
  FAutoAdjustRowHeights := Value;
end;

procedure TRbwColumn.SetButtonCaption(const Value: string);
begin
  FButtonCaption := Value;
end;

procedure TRbwColumn.SetButtonFont(const Value: TFont);
begin
  FButtonFont.Assign(Value);
end;

procedure TRbwColumn.SetButtonUsed(const Value: boolean);
begin
  FButtonUsed := Value;
end;

procedure TRbwColumn.SetButtonWidth(const Value: integer);
begin
  FButtonWidth := Value;
end;

procedure TRbwColumn.SetCheckedString(const Value: string);
begin
  FCheckedString := Value;
end;

procedure TRbwColumn.SetCheckMax(const Value: boolean);
begin
  if (FCheckMax <> Value) then FCheckMax := Value;
  if FCheckMax then CheckRange;
end;

procedure TRbwColumn.SetCheckMin(const Value: boolean);
begin
  if (FCheckMin <> Value) then FCheckMin := Value;
  if FCheckMin then CheckRange;
end;

procedure TRbwColumn.SetDropDownCount(const Value: integer);
begin
  FDropDownCount := Value;
end;

procedure TRbwColumn.SetFormat(const Value: TRbwColumnFormat);
begin
  if Value = FFormat then Exit;
  FFormat := Value;
  Changed(False);
end;


procedure TRbwColumn.SetMax(const Value: extended);
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

procedure TRbwColumn.SetMin(const Value: extended);
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

procedure TRbwColumn.SetPickList(const Value: TStrings);
begin
  FPickList.Assign(Value);
end;

procedure TRbwColumn.SetUnCheckedString(const Value: string);
begin
  FUnCheckedString := Value;
end;

procedure TRbwColumn.SetWordWrapCaptions(const Value: boolean);
begin
  FWordWrapCaptions := Value;
end;

procedure TRbwColumn.SetWordWrapCells(const Value: boolean);
begin
  FWordWrapCells := Value;
end;

{ TRbwDataGridColumns }

function TRbwDataGridColumns.Add: TRbwColumn;
begin
  Result := TRbwColumn(inherited Add);
  if not Grid.Updating and not (csLoading in FGrid.ComponentState) then
  begin
    FGrid.Updating := True;
    FGrid.ColCount := Count;
    FGrid.Updating := False;
  end;
//  FGrid.ColCount := Count;
end;


constructor TRbwDataGridColumns.Create(Grid: TRbwDataGrid;
  ColumnClass: TRbwColumnClass);
begin
  inherited Create(ColumnClass);
  Destroying := False;
  FGrid := Grid;
end;

destructor TRbwDataGridColumns.Destroy;
begin
  Destroying := True;
  inherited;
end;

function TRbwDataGridColumns.GetItems(Index: Integer): TRbwColumn;
begin
  Result := inherited Items[Index] as TRbwColumn;
end;

function TRbwDataGridColumns.GetOwner: TPersistent;
begin
  Result := FGrid;
end;

{procedure TRbwDataGridColumns.LoadFromFile(const Filename: string);
var
  S: TFileStream;
begin
  S := TFileStream.Create(Filename, fmOpenRead);
  try
    LoadFromStream(S);
  finally
    S.Free;
  end;
end; }

//type
{  TRbwColumnsWrapper = class(TComponent)
  private
    FColumns: TRbwDataGridColumns;
  published
    property Columns: TRbwDataGridColumns read FColumns write FColumns;
  end;  }

{procedure TRbwDataGridColumns.LoadFromStream(S: TStream);
var
  Wrapper: TRbwColumnsWrapper;
begin
  Wrapper := TRbwColumnsWrapper.Create(nil);
  try
    Wrapper.Columns := FGrid.CreateColumns;
    S.ReadComponent(Wrapper);
    Assign(Wrapper.Columns);
  finally
    Wrapper.Columns.Free;
    Wrapper.Free;
  end;
end;

procedure TRbwDataGridColumns.SaveToFile(const Filename: string);
var
  S: TStream;
begin
  S := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(S);
  finally
    S.Free;
  end;
end;

procedure TRbwDataGridColumns.SaveToStream(S: TStream);
var
  Wrapper: TRbwColumnsWrapper;
begin
  Wrapper := TRbwColumnsWrapper.Create(nil);
  try
    Wrapper.Columns := Self;
    S.WriteComponent(Wrapper);
  finally
    Wrapper.Free;
  end;
end;  }

procedure TRbwDataGridColumns.SetItems(Index: Integer;
  const Value: TRbwColumn);
begin
  Items[Index].Assign(Value);
end;

procedure TRbwDataGridColumns.Update(Item: TCollectionItem);
begin
  inherited;
     if (FGrid = nil) or (csLoading in FGrid.ComponentState) then Exit;

     if (csDesigning in FGrid.ComponentState) then FGrid.invalidate
     else FGrid.invalidatecol(FGrid.dgColumn);

end;


{ TRbwDataGrid }

function TRbwDataGrid.CanEditShow: Boolean;
var
  Column : TRbwColumn;
begin
  result := inherited CanEditShow;
  if result then
  begin
    if (dgColumn >=0) and (dgColumn < ColCount) then
    begin
      Column := Columns[dgColumn];
      result := not (Column.Format in [rcfBoolean, rcfCombo]);
    end;
  end;
end;

procedure TRbwDataGrid.ColumnMoved(FromIndex, ToIndex: Integer);
begin
  inherited;
  Columns[FromIndex].Index := ToIndex;

end;

procedure TRbwDataGrid.ColWidthsChanged;
begin
  inherited;
  SetComboPosition;
end;

procedure TRbwDataGrid.ComboChange(Sender: TObject);
begin
  Cells[dgColumn, dgRow] := Combo.Text;
  if Assigned(OnSetEditText) then
  begin
    OnSetEditText(self, dgColumn, dgRow, Combo.Text);
  end;
end;

procedure TRbwDataGrid.ComboExit(Sender: TObject);
begin
  Combo.Visible := False;
end;

constructor TRbwDataGrid.Create(AOwner: TComponent);
var
  ARect : TRect;
  Index : integer;
begin
  inherited;

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
  if not (csDesigning in ComponentState) then
  begin
    combo := TRbwComboBox.Create(self);
  end;
//  Col := 1;
//  Row := 1;
end;

function TRbwDataGrid.CreateColumns: TRbwDataGridColumns;
begin
  Result := TRbwDataGridColumns.Create(Self, TRbwColumn);
end;


function TRbwDataGrid.CreateEditor: TInplaceEdit;
begin
  Result := TRbwInplaceEdit.Create(Self);

end;

destructor TRbwDataGrid.Destroy;
begin
  bmpUnchecked.Free;
  bmpChecked.Free;
  FColumns.Free;
  inherited;
end;

{procedure TRbwDataGrid.DoEnter;
var
  AComponentState : TComponentState;
begin
  inherited;
{  AComponentState := [csLoading, csReading, csDestroying, csDesigning]
    * ComponentState;
  if AComponentState = [] then
  begin
    combo := TRbwComboBox.Create(self);
    combo.Parent := Parent;
  end;  }

//end;

procedure TRbwDataGrid.DoExit;
begin
  inherited;
  if dgColumn >= 0 then
  begin
    Columns[dgColumn].CheckCell(dgColumn, dgRow);
  end;
{  if not ((Owner as TForm).ActiveControl = Combo) then
  begin
    Combo.Visible := False;
  end; }
//  FreeAndNil(Combo);
end;

procedure TRbwDataGrid.DrawColumnTitleCell(ACol, ARow: Integer; ARect: TRect;
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
      Length(Cells[ACol, ARow]),ARect, GetFlags);
  finally
    Canvas.Font.Color := FontColor;
  end;
end;

procedure TRbwDataGrid.DrawCheckBoxCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
var
  Column : TRbwColumn;
  OldStyle : TBrushStyle;
  Dest: TRect;
begin
  // draw checkbox;
  if not (gdFixed in AState) then
  begin
    Column := Columns[ACol];
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
        if Cells[ACol, ARow] = Column.CheckedString then
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

procedure TRbwDataGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
var
  Column : TRbwColumn;
  BrushColor : TColor;
  FontColor : TColor;
  CanSelect : boolean;
begin
  inherited;
  Drawing := True;
  BrushColor := Canvas.Brush.Color;
  FontColor := Canvas.Font.Color;
  try
    if (ARow < FixedRows) and WordWrapColTitles then
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
      if Column.Format = rcfBoolean then
      begin
        // draw checkbox;
        DrawCheckBoxCell(ACol, ARow, ARect, AState);
      end
      else if (ARow >= FixedRows) and (ACol >= FixedCols) then
      begin
        inherited;
      end;
    end;

    if Assigned(OnDrawCell) then
    begin
      OnDrawCell(Self, ACol, ARow, ARect, AState);
    end;
  finally
    Canvas.Brush.Color := BrushColor;
    Canvas.Font.Color := FontColor;
    Drawing := False;
  end;

end;

function TRbwDataGrid.GetFixedCols: integer;
begin
  result := inherited FixedCols;
end;

function TRbwDataGrid.GetFlags: UINT;
begin
  result := DT_CENTER or DT_NOPREFIX;
  if FWordWrapColTitles then
  begin
    result := result or DT_WORDBREAK;
  end;
end;

function TRbwDataGrid.GetLeft: integer;
begin
  result := inherited Left;
end;

function TRbwDataGrid.GetTop: integer;
begin
  result := inherited Top;
end;

procedure TRbwDataGrid.Loaded;
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

end;

function TRbwDataGrid.LocalizeString(ANumberString : string) : string;
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

function TRbwDataGrid.LocalStrToFloat(S: string): Extended;
begin
  if (S = '') then
  begin
    result := 0;
    Exit;
  end;
  result := StrToFloat(LocalizeString(S));
end;


procedure TRbwDataGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  ACol, ARow : integer;
  ARect : TRect;
  Column : TRbwColumn;
  Offset : integer;
  CanSelect : boolean;
  NewCoord : TGridCoord;
  NewSelection : TGridRect;
//  GridDrawState : TGridDrawState;
begin
  inherited;
  MouseIsDown := True;
  MouseToCell(X, Y, ACol, ARow);
//  CanSelect := True;
  CanSelect := (ACol >= FixedCols) and (ARow >= FixedRows)
    and inherited SelectCell(ACol, ARow);
{  if Assigned(OnSelectCell) then
  begin
    OnSelectCell(self, ACol, ARow, CanSelect);
  end; }
  dgRow := ARow;
  dgColumn := ACol;
  if not CanSelect then
  begin
    EditorMode := False;
  end;
  if CanSelect then
  begin
{    if (ACol < ColCount) and (ACol >=0) then
    begin
      Col := ACol;
    end
    else
    begin
      Col := ColCount -1;
    end;
    if (ARow < RowCount) and (ARow >= 0) then
    begin
      Row := ARow;
    end
    else
    begin
      Row := RowCount -1;
    end;
    GridDrawState := [];
//    GridDrawState := [gdFocused];
    if (ACol < FixedCols) or (ARow < FixedRows) then
    begin
//      GridDrawState := [gdFixed]
      Include(GridDrawState, gdFixed);
    end;    }
    dgRow := ARow;
    dgColumn := ACol;
//    DrawCell(ACol, ARow, CellRect(ACol, ARow),GridDrawState);
  end;
  if (ACol >= 0) and (ACol < ColCount) then
  begin
    SetComboPosition;
    if (Combo <> nil) and Combo.Visible then
    begin
      Combo.SetFocus;
    end;
    Column := Columns[ACol];
    if (ARow >= 0) and (ARow < RowCount)
      and (CanSelect or (Column.Format = rcfBoolean)) then
    begin
      NewCoord.X := ACol;
      NewCoord.Y := ARow;
      NewSelection.TopLeft := NewCoord;
      NewSelection.BottomRight := NewCoord;
      Selection := NewSelection;
    end;
    if (Column.Format = rcfBoolean) and (ACol >= FixedCols) and (ARow >= FixedRows) then
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
        if Cells[ACol, ARow] = Column.CheckedString then
        begin
          Cells[ACol, ARow] := Column.UnCheckedString;
        end
        else
        begin
          Cells[ACol, ARow] := Column.CheckedString;
        end;
      end;
      DrawCell(ACol, ARow, ARect, [gdSelected]);
    end;
  end;
  Invalidate;
  MouseIsDown := False;
end;

procedure TRbwDataGrid.RowHeightsChanged;
begin
  inherited;
  SetComboPosition;
end;

function TRbwDataGrid.SelectCell(ACol, ARow: Integer): Boolean;
var
  Column : TRbwColumn;
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
    if (Column.Format = rcfBoolean) or (Column.Format = rcfCombo) then
    begin
      result := False;
    end
  end;
  dgColumn := ACol;
  dgRow := ARow;
end;

procedure TRbwDataGrid.SetColorSelectedRow(const Value: boolean);
begin
  if FColorSelectedRow <> Value then
  begin
    FColorSelectedRow := Value;
    Invalidate;
  end;
end;

procedure TRbwDataGrid.SetColumns(const Value: TRbwDataGridColumns);
begin
  FColumns.Assign(Value);
end;

procedure TRbwDataGrid.SetComboPosition;
var
  ARect : TRect;
  FixedHeight, FixedWidth : integer;
  Index : integer;
  ComboTop: Integer;
  ComboLeft : integer;
  ComboWidth : integer;
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
    and (Columns[dgColumn].Format = rcfCombo) then
  begin
    ARect := CellRect(dgColumn, dgRow);
    ComboLeft := ARect.Left + Left + 2;
    ComboTop := ARect.Top + Top + 2;
    ComboWidth := ARect.Right - ARect.Left;

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
      (ComboTop >= Top + FixedHeight) and
      (ComboLeft >= Left + FixedWidth) and
      (ComboLeft + ComboWidth <= Left + Width) then
    begin
      if (Combo = nil) then
      begin
        combo := TRbwComboBox.Create(self);
        combo.Parent := Parent;
      end;
      Combo.Left := ComboLeft;
      Combo.Top := ComboTop;
      Combo.Width := ComboWidth;
      Combo.Items.Assign(Columns[dgColumn].PickList);
      Combo.MaxLength := Columns[dgColumn].MaxLength;
      Combo.DropDownCount := Columns[dgColumn].DropDownCount;
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
          and (Combo.Top + Combo.Height <= Top + Height) then
      begin
        Combo.Visible := True;

{        if (Combo.Top  < Top)
          or (Combo.Top + Combo.Height >= Top + Height) then
        begin
          NewCoord.X := dgColumn;
          NewCoord.Y := dgRow;
          NewSelection.TopLeft := NewCoord;
          NewSelection.BottomRight := NewCoord;
          Selection := NewSelection;
          ARect := CellRect(dgColumn, dgRow);
          ComboTop := ARect.Top + Top + 2;
          Combo.Top := ComboTop;
//          SetComboPosition;
        end;  }

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
          Selection := NewSelection;
          ARect := CellRect(dgColumn, dgRow);
          ComboTop := ARect.Top + Top + 2;
        if Combo <> nil then
        begin
          Combo.Visible := False;
          Combo.Top := ComboTop;
        end;
//          SetComboPosition;
      end;
    end
    else
    begin
      if Combo <> nil then
      begin
        Combo.Visible := False;
      end;
    end;
  end
  else
  begin
    if Combo <> nil then
    begin
      Combo.Visible := False;
    end;
  end;
end;

procedure TRbwDataGrid.SetdgColumn(const Value: integer);
begin
  if (FdgColumn >= 0) and (FdgColumn < ColCount) then
  begin
    Columns[FdgColumn].CheckCell(FdgColumn, FdgRow);
  end;
  FdgColumn := Value;
end;

procedure TRbwDataGrid.SetdgRow(const Value: integer);
begin
  if (FdgColumn >= 0) and (FdgColumn < ColCount) then
  begin
    Columns[FdgColumn].CheckCell(FdgColumn, FdgRow);
  end;
  FdgRow := Value;
end;

procedure TRbwDataGrid.SetEditText(ACol, ARow: Integer;
  const Value: string);
var
  Column : TRbwColumn;
  IntValue : integer;
  NewValue : string;
  ValString : PChar;
  ConversionOK : boolean;
  AFloat : double;
  E : integer;
begin
  Column := Columns[ACol];
  case Column.Format of
    rcfString:
      begin
        if (Column.MaxLength > 0) and (Length(Value) > Column.MaxLength) then
        begin
          inherited SetEditText(ACol, ARow, Copy(Value, 1, Column.MaxLength));
          (InplaceEditor as TRbwInplaceEdit).UpdateContents;
          (InplaceEditor as TRbwInplaceEdit).SelStart := Column.MaxLength;
        end
        else
        begin
          inherited;
        end;
      end;
    rcfInteger:
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
          (InplaceEditor as TRbwInplaceEdit).UpdateContents;
          (InplaceEditor as TRbwInplaceEdit).SelStart := E;
        end;
      end;
    rcfReal:
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
          (InplaceEditor as TRbwInplaceEdit).UpdateContents;
          (InplaceEditor as TRbwInplaceEdit).SelStart := Length(NewValue);
        end;
      end;
    rcfBoolean:
      begin
        inherited;
      end;
    rcfCombo:
      begin
        if not Column.LimitToList and (Column.MaxLength > 0)
          and (Length(Value) > Column.MaxLength) then
        begin
          inherited SetEditText(ACol, ARow, Copy(Value, 1, Column.MaxLength));
          (InplaceEditor as TRbwInplaceEdit).UpdateContents;
          (InplaceEditor as TRbwInplaceEdit).SelStart := Column.MaxLength;
        end
        else
        begin
          inherited;
        end;
      end;
  else Assert(False);
  end;
end;

procedure TRbwDataGrid.SetFixedCols(const Value: integer);
begin
  FFixedCols := Value;
  inherited FixedCols := Value;
end;


procedure TRbwDataGrid.SetLeft(const Value: integer);
begin
  inherited Left := Value;
  SetComboPosition;
end;

procedure TRbwDataGrid.SetParent(AParent: TWinControl);
begin
  Inherited;
  if (AParent <> nil) and (Combo <> nil) then
  begin
    Combo.Parent := AParent;
    SetComboPosition;
  end;
end;

procedure TRbwDataGrid.SetSelectedRowColor(const Value: TColor);
begin
  if FSelectedRowColor <> Value then
  begin
    FSelectedRowColor := Value;
    Invalidate;
  end;
end;

procedure TRbwDataGrid.SetTop(const Value: integer);
begin
  inherited Top := Value;
  SetComboPosition;
end;

procedure TRbwDataGrid.SetUnselectableColor(const Value: TColor);
begin
  if FUnselectableColor <> Value then
  begin
    FUnselectableColor := Value;
    Invalidate;
  end;

end;

procedure TRbwDataGrid.SetWordWrapColTitles(const Value: boolean);
begin
  FWordWrapColTitles := Value;
  if FWordWrapColTitles <> Value then
  begin
    FWordWrapColTitles := Value;
//    if not Drawing then
    begin
      Invalidate;
    end;
  end;
end;

procedure TRbwDataGrid.SizeChanged(OldColCount, OldRowCount: Integer);
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

procedure TRbwDataGrid.WMHScroll(var Msg: TWMHScroll);
begin
  inherited;
  SetComboPosition;
end;

procedure TRbwDataGrid.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  SetComboPosition;
end;

procedure TRbwDataGrid.DeleteRow(ARow: Integer);
begin
  inherited DeleteRow(ARow);
end;

procedure TRbwDataGrid.SetEnabled(Value: Boolean);
begin
  inherited;
  if not value and (Combo <> nil) then Combo.Visible := False;
end;

{ TRbwComboBox }

constructor TRbwComboBox.Create(AOwner: TComponent);
begin
  inherited;
  Visible := False;
end;

end.
