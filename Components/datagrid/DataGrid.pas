{*******************************************************}
{                                                       }
{       TDataGrid 2.0                                   }
{                                                       }
{       © EC Software 1997-1999                         }
{       http://www.easycash.co.at (outdated)            }
{       http://www.ec-software.com                      }
{                                                       }
{*******************************************************}


unit DataGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, Menus, DBCtrls, DB
// RBW begin change
{$ifdef CONDITIONALEXPRESSIONS}
  {$if CompilerVersion>=20}
    {$DEFINE Delphi_2009_UP}
  {$ifend}
  {$if CompilerVersion>15.0}
    {$DEFINE Delphi_7_UP}
    {$define DELPHI}
    {$define DELPHI_7}
  {$IFEND}
{$endif}
{$ifdef VER150}
    {$DEFINE Delphi_7_UP}
    {$define DELPHI}
    {$define DELPHI_7}
{$endif}
{$IFDEF Delphi_7_UP}, Variants {$ENDIF}
// RBW end change
  ;

type
// RBW begin change
  Natural = 0..MaxInt;
// RBW end change
  TColumnValue = (cvColor, cvWidth, cvFont, cvAlignment, cvReadOnly, cvTitleColor, cvTitleCaption, cvTitleAlignment, cvTitleFont);
  TColumnValues = set of TColumnValue;

const
  ColumnTitleValues = [cvTitleColor..cvTitleFont];
  cm_DeferLayout = WM_USER + 100;
  cVersion = '2.0';

type
  TColumnButtonStyle = (cbsAuto, cbsEllipsis, cbsNone);
  TColumnFormat = (cfString, cfNumber, cfDate);

  TDataGridOption = (dgoAppendRow, dgoInsertRow, dgoDeleteRow);
  TDataGridOptions = set of TDataGridOption;

  TColumn = class;
  TDataGrid = class;

  TColumnTitle = class(TPersistent)
  private
    FColumn: TColumn;
    FCaption: string;
    FFont: TFont;
    FAlignment: TAlignment;
    FWordWrap: boolean;
    procedure FontChanged(Sender: TObject);
    function GetAlignment: TAlignment;
    function GetCaption: string;
    function GetFont: TFont;
    function IsAlignmentStored: Boolean;
    function IsFontStored: Boolean;
    function IsCaptionStored: Boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetFont(Value: TFont);
    procedure SetCaption(const Value: string); virtual;
    procedure SetWordWrap(const Value: boolean);
  protected
    procedure RefreshDefaultFont;
  public
    constructor Create(Column: TColumn);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function DefaultAlignment: TAlignment;
    function DefaultColor: TColor;
    function DefaultFont: TFont;
    function DefaultCaption: string;
    procedure RestoreDefaults; virtual;
  published
    property Alignment: TAlignment read GetAlignment write SetAlignment stored IsAlignmentStored;
    property Caption: string read GetCaption write SetCaption stored IsCaptionStored;
    property Font: TFont read GetFont write SetFont stored IsFontStored;
    property WordWrap: boolean read FWordWrap write SetWordWrap;
  end;

  TColumn = class(TCollectionItem)
  private
    FColor: TColor;
    FLimitToList: Boolean;
    FEditMask: String;
    FMaxLength: Integer;
    FFormat: TColumnFormat;
    FDisplayMask: String;
    FTitle: TColumnTitle;
    FFont: TFont;
    FPickList: TStrings;
    FPopupMenu: TPopupMenu;
// RBW begin change
//    FDropDownRows: Cardinal;
    FDropDownRows: Natural;
// RBW end change
    FButtonStyle: TColumnButtonStyle;
    FAlignment: TAlignment;
    FReadonly: Boolean;
    FAssignedValues: TColumnValues;
    FInternalCol: Longint;
    procedure FontChanged(Sender: TObject);
    function  GetAlignment: TAlignment;
    function  GetColor: TColor;
    function  GetFont: TFont;
    function  GetPickList: TStrings;
    function  GetReadOnly: Boolean;
    function  GetDisplayMask: String;
    function  GetMaxLength: Integer;
    function  IsAlignmentStored: Boolean;
    function  IsColorStored: Boolean;
    function  IsFontStored: Boolean;
    function  IsReadOnlyStored: Boolean;
    function  IsDisplayMaskStored: Boolean;
    procedure SetAlignment(Value: TAlignment); virtual;
    procedure SetButtonStyle(Value: TColumnButtonStyle);
    procedure SetFormat(Value: TColumnFormat);
    procedure SetColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetPickList(Value: TStrings);
    procedure SetPopupMenu(Value: TPopupMenu);
    procedure SetReadOnly(Value: Boolean); virtual;
    procedure SetTitle(Value: TColumnTitle);
    procedure SetDisplayMask(Value: String); virtual;
    procedure SetMaxLength(Value: Integer);
  protected
    function  CreateTitle: TColumnTitle; virtual;
    function  GetGrid: TDataGrid;
    procedure RefreshDefaultFont;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function  DefaultAlignment: TAlignment;
    function  DefaultColor: TColor;
    function  DefaultFont: TFont;
    function  DefaultReadOnly: Boolean;
    function  DefaultWidth: Integer;
    procedure RestoreDefaults; virtual;
    property  Grid: TDataGrid read GetGrid;
    property  AssignedValues: TColumnValues read FAssignedValues;
  published
    property  EditMask: String read FEditMask write FEditMask;
    property  MaxLength: Integer read GetMaxLength write SetMaxLength default 0;
    property  LimitToList: Boolean read FLimitToList write FLimitToList default false;
    property  Alignment: TAlignment read GetAlignment write SetAlignment stored IsAlignmentStored;
    property  ButtonStyle: TColumnButtonStyle read FButtonStyle write SetButtonStyle default cbsAuto;
    property  Format: TColumnFormat read FFormat write SetFormat default cfString;
    property  Color: TColor read GetColor write SetColor stored IsColorStored;
// RBW begin change
//    property  DropDownRows: Cardinal read FDropDownRows write FDropDownRows default 7;
    property  DropDownRows: Natural read FDropDownRows write FDropDownRows default 7;
// RBW end change
    property  Font: TFont read GetFont write SetFont stored IsFontStored;
    property  PickList: TStrings read GetPickList write SetPickList;
    property  PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property  ReadOnly: Boolean read GetReadOnly write SetReadOnly stored IsReadOnlyStored;
    property  Title: TColumnTitle read FTitle write SetTitle;
    property  DisplayMask: String read GetDisplayMask write SetDisplayMask stored IsDisplayMaskStored;
    property  InternalCol: LongInt read FInternalCol;
  end;

  TColumnClass = class of TColumn;

  TDataGridColumns = class(TCollection)
  private
    FGrid: TDataGrid;
    function GetColumn(Index: Integer): TColumn;
    procedure SetColumn(Index: Integer; Value: TColumn);
  protected
    function GetOwner: TPersistent; override; {D3}
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(Grid: TDataGrid; ColumnClass: TColumnClass);
    function  Add: TColumn;
    procedure LoadFromFile(const Filename: string);
    procedure LoadFromStream(S: TStream);
    procedure RestoreDefaults;
    procedure SaveToFile(const Filename: string);
    procedure SaveToStream(S: TStream);
    property Grid: TDataGrid read FGrid;
    property Items[Index: Integer]: TColumn read GetColumn write SetColumn; default;
  end;

{ TDataGrid }
  TDataGrid = class(TStringGrid)
  private
    FOnEditButtonClick: TNotifyEvent;
    FOnGetEditMask: TGetEditEvent;
    FColumns: TDataGridColumns;
    FBeforeInsert: TNotifyEvent;
    FAfterInsert: TNotifyEvent; // erb added 2/25/08
    FBeforeDelete: TNotifyEvent;
    FAfterDelete: TNotifyEvent; // erb added 2/25/08
    FOnUserChanged: TNotifyEvent;
    FRowCountMin: LongInt;
    FDataGridOptions: TDataGridOptions;
    FLayoutFlag: Integer;
    FBGImage: TBitmap;
    procedure SetVersion(Value: string);
    function  GetVersion: string;
    function  GetIntValue(ACol, ARow: Integer): Integer;
    procedure SetIntValue(ACol, ARow: Integer; const Value: Integer);
    function  GetNumValue(ACol, ARow: Integer): Real;
    procedure SetNumValue(ACol, ARow: Integer; const Value: Real);
    procedure SetColumns(Value: TDataGridColumns);
    function  GetSelectedIndex: Integer;
    procedure SetSelectedIndex(Value: Integer);
    procedure SetColumnCount(NewCount: LongInt);
    procedure SetRowCountMin(Value: LongInt);
    procedure SetBGImage(newImg: TBitmap);
    function  CheckDataGridKey(var Key: Word; Shift: TShiftState): Boolean;
    procedure DrawBackground(rect: TRect; AState: TGridDrawState);
  protected
    procedure SizeChanged(OldColCount, OldRowCount: Longint); override;
    procedure RowCountMinChanged; dynamic;
    function  CreateEditor: TInplaceEdit; override;
    function  CreateColumns: TDataGridColumns; dynamic;
    function  GetEditMask(ACol, ARow: Longint): string; override;
    procedure EditButtonClick; dynamic;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure TopLeftChanged; override;
    procedure Paint; override;
  public
    procedure DefaultHandler(var Msg); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AppendRow;
    procedure InsertRow(ARow: LongInt);
    procedure DeleteRow(ARow: LongInt);  override;  // erb addded override 2/25/08
    property  IntValue[ACol, ARow: Integer]: Integer read GetIntValue write SetIntValue;
    property  NumValue[ACol, ARow: Integer]: Real    read GetNumValue write SetNumValue;
  published
    property BeforeDelete: TNotifyEvent read FBeforeDelete write FBeforeDelete;
    property AfterDelete: TNotifyEvent read FAfterDelete write FAfterDelete; // erb added 2/25/08
    property BeforeInsert: TNotifyEvent read FBeforeInsert write FBeforeInsert;
    property AfterInsert: TNotifyEvent read FAfterInsert write FAfterInsert; // erb added 2/25/08
    property BGImage: TBitmap read fBGImage write SetBGImage;
    property Columns: TDataGridColumns read FColumns write SetColumns;
    property DataGridOptions: TDataGridOptions read FDataGridOptions write FDataGridOptions default [];
    property RowCountMin: LongInt read FRowCountMin write SetRowCountMin;
    property OnEditButtonClick: TNotifyEvent read FOnEditButtonClick write FOnEditButtonClick;
    property OnGetEditMask: TGetEditEvent read FOnGetEditMask write FOnGetEditMask;
    property OnUserChanged: TNotifyEvent read FOnUserChanged write FOnUserChanged;
    property SelectedIndex: Integer read GetSelectedIndex write SetSelectedIndex;
    property Version: String read GetVersion write SetVersion;
  end;

  // TDataGrid can't be installed in Delphi-2009 because it conflicts with
  // MidItems.TDataGrid.
  TEcDataGrid = class(TDataGrid);

{ TDBDataGrid }

  TDBDataGrid = class(TECDataGrid)
  private
    FDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure UpdateData(Sender: TObject);
    function  SetEditing: Boolean;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure SizeChanged(OldColCount, OldRowCount: Longint); override;
    procedure RowCountMinChanged; override;
    function  CanEditModify: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property  Field: TField read GetField;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;


{ TDataGridInplaceEdit }

type
  TEditStyle = (esSimple, esEllipsis, esPickList, esDataList);
  TPopupListbox = class;

  TDataGridInplaceEdit = class(TInplaceEdit)
  private
    FButtonWidth: Integer;
    FPickList: TPopupListbox;
    FActiveList: TWinControl;
    FEditStyle: TEditStyle;
    FListVisible: Boolean;
    FTracking: Boolean;
    FPressed: Boolean;
    FLastText: String;
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SetEditStyle(Value: TEditStyle);
    procedure StopTracking;
    procedure TrackButton(X,Y: Integer);
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CancelMode;
    procedure WMCancelMode(var Message: TMessage); message WM_CancelMode;
    procedure WMKillFocus(var Message: TMessage); message WM_KillFocus;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message wm_LButtonDblClk;
    procedure WMPaint(var Message: TWMPaint); message wm_Paint;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SetCursor;
  protected
    procedure ValidateContent; dynamic;
    procedure BoundsChanged; override;
    procedure CloseUp(Accept: Boolean);
    procedure DoDropDownKeys(var Key: Word; Shift: TShiftState);
    procedure DropDown;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PaintWindow(DC: HDC); override;
    procedure UpdateContents; override;
    procedure WndProc(var Message: TMessage); override;
    property  EditStyle: TEditStyle read FEditStyle write SetEditStyle;
    property  ActiveList: TWinControl read FActiveList write FActiveList;
    property  PickList: TPopupListbox read FPickList;
  public
    constructor Create(Owner: TComponent); override;
  end;

{ TPopupListbox }

  TPopupListbox = class(TCustomListbox)
  private
    FSearchText: String;
    FSearchTickCount: Longint;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;


type
  TWinControlCracker = class(TWinControl) end;


function  ReplaceChar(C1: Char; S: String; C2: Char): String;
procedure KillMessage(Wnd: HWnd; Msg: Integer);

procedure Register;

implementation

procedure WriteText(ACanvas: TCanvas; ARect: TRect; DX, DY: Integer;
  const Text: string; Alignment: TAlignment; transparent: boolean;
  AState: TGridDrawState; const WordWrap: boolean);
const
  AlignFlags : array [TAlignment] of Integer =
    ( DT_LEFT {or DT_WORDBREAK} or DT_EXPANDTABS or DT_NOPREFIX,
      DT_RIGHT {or DT_WORDBREAK} or DT_EXPANDTABS or DT_NOPREFIX,
      DT_CENTER {or DT_WORDBREAK} or DT_EXPANDTABS or DT_NOPREFIX );
var
{RBW begin change}
//  I, Left: Integer;
  I: TColorRef;
  Left: Integer;
  AnAlignment: integer;
{RBW end change}
begin
  I := ColorToRGB(ACanvas.Brush.Color);
  if GetNearestColor(ACanvas.Handle, I) = I then
  begin
    case Alignment of
      taLeftJustify:
        Left := ARect.Left + DX;
      taRightJustify:
        Left := ARect.Right - ACanvas.TextWidth(Text) - DX;
    else { taCenter }
      Left := ARect.Left + (ARect.Right - ARect.Left) shr 1
        - (ACanvas.TextWidth(Text) shr 1);
    end;

    AnAlignment := AlignFlags[Alignment];
    if WordWrap then
    begin
      AnAlignment := AnAlignment or DT_WORDBREAK;
    end;
    ARect.Left := Left;
    ARect.Top := ARect.Top + DY;
    if transparent then ExtTextOut(ACanvas.Handle, Left, ARect.Top, ETO_CLIPPED, @ARect, PChar(Text), Length(Text), nil)
    else  DrawText(ACanvas.Handle, PChar(Text), Length(Text), ARect, AnAlignment);

    {ExtTextOut(ACanvas.Handle, Left, ARect.Top + DY,
      ETO_CLIPPED or ETO_OPAQUE, @ARect, PChar(Text), Length(Text), nil);}
  end;
{  else begin                          This routine is implemented from DBGrid
    DrawBitmap.Canvas.Lock;            It enables image support in cells
    try
      with DrawBitmap, ARect do
      begin
        Width := Max(Width, Right - Left);
        Height := Max(Height, Bottom - Top);
        R := Rect(DX, DY, Right - Left - 1, Bottom - Top - 1);
        B := Rect(0, 0, Right - Left, Bottom - Top);
      end;
      with DrawBitmap.Canvas do
      begin
        Font := ACanvas.Font;
        Font.Color := ACanvas.Font.Color;
        Brush := ACanvas.Brush;
        Brush.Style := bsSolid;
        FillRect(B);
        SetBkMode(Handle, TRANSPARENT);
        DrawText(Handle, PChar(Text), Length(Text), R, AlignFlags[Alignment]);
      end;
      ACanvas.CopyRect(ARect, DrawBitmap.Canvas, B);
    finally
      DrawBitmap.Canvas.Unlock;
    end;
  end;}
end;


constructor TDataGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited DefaultRowHeight := 20;
  FBGImage := TBitmap.create;
  FLayoutFlag := 2;
  options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing, goColSizing];
  FColumns := CreateColumns;
  HideEditor;
  FLayoutFlag := 0;
  sizechanged(ColCount, RowCount);
end;

destructor TDataGrid.Destroy;
begin
  FLayoutFlag := 2;
  FColumns.Free;
  FColumns := nil;
  fBGImage.free;
  inherited Destroy;
end;

procedure TDataGrid.SetVersion(Value: string);
begin
//dummy, do nothing
end;

function TDataGrid.GetVersion: string;
begin
     result := cversion;
end;

procedure TDataGrid.SizeChanged(OldColCount, OldRowCount: Longint);
begin
     if not (csLoading in ComponentState) and (FLayoutFlag = 0) then
     begin
          inc(FLayoutFlag);
          while Columns.count > ColCount do Columns[ColCount].destroy;
          while Columns.count < ColCount do Columns.add;
          dec(FLayoutFlag);
     end;
end;

procedure TDataGrid.SetColumnCount(NewCount: LongInt);
begin
     if (FLayoutFlag > 0) or (csLoading in ComponentState) then exit;
     inc(FLayoutFlag);
     ColCount := NewCount;
     dec(FLayoutFlag);
end;

procedure TDataGrid.SetRowCountMin(Value: LongInt);
begin
     if value > -1 then FRowCountMin := value else FRowCountMin := 0;
     if (FRowCountMin > 0) and (RowCount < FRowCountMin) then RowCount := FRowCountMin;
     RowCountMinChanged;
end;

procedure TDataGrid.RowCountMinChanged;
begin
// TDBDataGrid overrides this procedure
end;

procedure TDataGrid.DoEnter;
begin
     if (goediting in options) and (goAlwaysShowEditor in options) then ShowEditor;
     inherited DoEnter;
end;

procedure TDataGrid.DoExit;
begin
     if (goediting in options) and (goAlwaysShowEditor in options) then HideEditor;
     inherited DoExit;
end;

procedure TDataGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
  Alignment: TAlignment;
  Value: string;
  TmpColor: TColor;
  HasBG: boolean;
  ShouldWordWrap: boolean;
begin
  ShouldWordWrap := False;
  Value := cells[ACol, ARow];
  Alignment := taLeftJustify;
  HasBG := (assigned(fBGImage) and (fBGImage.width > 0) and (fBGImage.height > 0));

  if ACol < Columns.count then
  begin
       Alignment   := Columns[ACol].Alignment;
       ShouldWordWrap   := Columns[ACol].Title.WordWrap;
       if (gdFocused in AState) or ((goRowSelect in Options) and (gdSelected in AState)) then
       begin
            TmpColor := Canvas.font.color;
            Canvas.font := Columns[ACol].font;
            Canvas.font.color := TmpColor;
       end else if (gdSelected in AState) and
         (not (gdFocused in AState) or
         ([goDrawFocusSelected, goRowSelect] * Options <> [])) then
       begin
//         Brush.Color := clHighlight;
//         Font.Color := clHighlightText;
       end else
       begin
            Canvas.font := Columns[ACol].font;
            Canvas.brush.color := Columns[ACol].color;
       end;
       if Columns[ACol].Displaymask <> '' then
       try
          case Columns[Acol].Format of
            cfNumber: Value := formatfloat(Columns[ACol].Displaymask, strtofloat(value));
            cfDate:   Value := formatdateTime(Columns[ACol].Displaymask, strtodatetime(value));
          end;
       except
       end;
  end;
  if (ARow < FixedRows) and (ACol < Columns.count) then  //Title
  begin
       if Columns[ACol].Title.caption <> '' then Value := Columns[ACol].Title.caption;
       Alignment := Columns[ACol].Title.Alignment;
       Canvas.font := Columns[ACol].Title.font;
  end
  else
  begin
    ShouldWordWrap := False;
  end;
  if ACol < FixedCols then Canvas.brush.color := FixedColor;
  if ARow < FixedRows then Canvas.brush.color := FixedColor;

  if DefaultDrawing then
  begin
       if HasBG and (ACol >= FixedCols) and (ARow >= FixedRows) then
       begin
            DrawBackground(ARect, AState);
            Canvas.brush.style := bsClear;
            if (gdFocused in AState) or (gdSelected in AState) then Canvas.font.color := clHighlightText;
       end;
       WriteText(Canvas, ARect, 2, 2, Value, Alignment, HasBG, AState, ShouldWordWrap);
       if Assigned(OnDrawCell) then OnDrawCell(self,ACol, ARow, ARect, AState);
  end
  else inherited DrawCell(ACol, ARow, ARect, AState);

end;

procedure TDataGrid.SetIntValue(ACol, ARow: Integer; const Value: Integer);
begin
     cells[ACol,ARow] := inttostr(Value);
end;

function TDataGrid.GetIntValue(ACol, ARow: Integer): Integer;
begin
  try
     result := strtoint(cells[ACol,ARow]);
  except;
     result := 0;
  end;
end;

procedure TDataGrid.SetNumValue(ACol, ARow: Integer; const Value: Real);
begin
     cells[ACol,ARow] := Floattostr(Value);
end;

function TDataGrid.GetNumValue(ACol, ARow: Integer): Real;
begin
  try
     result := strtoFloat(cells[ACol,ARow]);
  except;
     result := 0;
  end;
end;

procedure TDataGrid.SetBGImage(newImg: TBitmap);
begin
  fBGImage.assign(newImg);
  invalidate;
end;

function TDataGrid.GetEditMask(ACol, ARow: Longint): string;
begin
  Result := '';
  if columns.count > ACol then Result := columns[ACol].EditMask;
  if Assigned(FOnGetEditMask) then FOnGetEditMask(Self, ACol, ARow, Result);
end;

function TDataGrid.GetSelectedIndex: Integer;
begin
  Result := Col;
end;

procedure TDataGrid.SetSelectedIndex(Value: Integer);
begin
  Col := Value;
end;

procedure TDataGrid.EditButtonClick;
begin
  if Assigned(FOnEditButtonClick) then FOnEditButtonClick(Self);
end;

function TDataGrid.CreateEditor: TInplaceEdit;
begin
  Result := TDataGridInplaceEdit.Create(Self);
end;

function TDataGrid.CreateColumns: TDataGridColumns;
begin
  Result := TDataGridColumns.Create(Self, TColumn);
end;

procedure TDataGrid.SetColumns(Value: TDataGridColumns);
begin
  Columns.Assign(Value);
end;

procedure TDataGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if CheckDataGridKey(Key, shift) then
  case Key of
    VK_TAB:    AppendRow;
    VK_DOWN:   AppendRow;
    VK_INSERT: InsertRow(row);
    VK_DELETE: DeleteRow(row);
  end;
  inherited keydown(Key, Shift);
end;

function TDataGrid.CheckDataGridKey(var Key: Word; Shift: TShiftState): Boolean;
begin
     result := false;
     case key of
          VK_TAB:    if (not (ssShift in Shift) and (Row = Rowcount-1) and (Col = Colcount-1)) and (dgoAppendRow in DataGridOptions) then result := true;
          VK_DOWN:   if (Row = Rowcount-1) and (dgoAppendRow in DataGridOptions) then result := true;
          VK_INSERT: if (ssCtrl in Shift) and (dgoInsertRow in DataGridOptions) then result := true;
          VK_DELETE: if (ssCtrl in Shift) and (dgoDeleteRow in DataGridOptions) then result := true;
     end;
end;

procedure TDataGrid.Paint;
var
   i, endx, endy: integer;
begin
  inherited paint;
  if assigned(fBGImage) and (fbgImage.width > 0) and (fbgImage.height > 0) then
  begin
     endx := 0;
     for I := 0 to FixedCols-1 do inc(endx, (colwidths[i]+1));
     for I := LeftCol to colcount-1 do inc(endx, (colwidths[i]+1));
     endy := 0;
     for I := 0 to FixedRows-1 do inc(endy, (Rowheights[i]+1));
     for I := TopRow to Rowcount-1 do inc(endy, (Rowheights[i]+1));

     if endX < clientwidth  then DrawBackground(rect(endx+1, 0, clientwidth, endy), []);
     if endy < clientheight then DrawBackground(rect(0, endy, clientwidth, clientheight), []);
  end;
end;

procedure TDataGrid.TopLeftChanged;
begin
  inherited;
  if assigned(fBGImage) and (fbgImage.width > 0) and (fbgImage.height > 0)
    then invalidatergn(handle, 0, false);
end;

procedure TDataGrid.DrawBackground(rect: TRect; AState: TGridDrawState);
var
  rMode: TCopyMode;
  srect, drect, irect: trect;
  XCnt, YCnt, X, Y: Integer;
begin
     if (fbgImage.width > 0) and (fbgImage.height > 0) then
     begin
       rmode := Canvas.copymode;
       if (gdSelected in AState) then canvas.copymode := cmNotSrcCopy;

       XCnt := (Clientwidth) div fBGImage.width;
       YCnt := (Clientheight) div fBGImage.height;

       for x := 0 to XCnt do
       begin
            for y := 0 to YCnt do
            begin
                 drect.left   := x * fbgImage.width;
                 drect.top    := y * fbgImage.height;
                 drect.right  := drect.left + fbgImage.width;
                 drect.bottom := drect.top  + fbgImage.height;

                 if Intersectrect(irect, rect, drect) then
                 begin
                    srect := irect;
                    while srect.left >= fbgimage.width  do offsetrect(srect, -fbgimage.width, 0);
                    while srect.top  >= fbgimage.height do offsetrect(srect, 0, -fbgimage.height);
                    canvas.copyrect(irect, fbgimage.canvas, srect);
                 end;
            end;
       end;
       Canvas.copymode := rmode;
     end;
end;

procedure TDataGrid.DefaultHandler(var Msg);
var
  P: TPopupMenu;
  Cell: TGridCoord;
begin
  inherited DefaultHandler(Msg);
  if TMessage(Msg).Msg = wm_RButtonUp then
    with TWMRButtonUp(Msg) do
    begin
      Cell := MouseCoord(XPos, YPos);
      if (Cell.X < 0) or (Cell.Y < 0) then Exit;
      P := Columns[Cell.X].PopupMenu;
      if (P <> nil) and P.AutoPopup then
      begin
        SendCancelMode(nil);
        P.PopupComponent := Self;
        with ClientToScreen(SmallPointToPoint(Pos)) do
          P.Popup(X, Y);
        Result := 1;
      end;
    end;
end;

procedure TDataGrid.ColumnMoved(FromIndex, ToIndex: Longint);
begin
  Columns[FromIndex].Index := ToIndex;
  Inherited;
end;

procedure TDataGrid.AppendRow;
begin
  if assigned(FBeforeInsert) then FBeforeInsert(self);
  RowCount := RowCount + 1;
  invalidateRow(Rowcount-1);
  SelectCell(Col,Rowcount-1);
end;

procedure TDataGrid.InsertRow(ARow: LongInt);
var
   I, L: LongInt;
begin
  if assigned(FBeforeInsert) then FBeforeInsert(self);
  RowCount := RowCount + 1;
  for I := RowCount-1 downto ARow do
  begin
    for L := 0 to ColCount-1 do
    begin
      cells[L,I] := cells[L,I-1];
// rbw begin change
      Objects[L,I] := Objects[L,I-1];
// rbw end change
    end;
  //  for L := 0 to Colcount -1 do cells[L, Row] := '';
  end;
  for L := 0 to Colcount -1 do cells[L, ARow] := '';
  if assigned(FAfterInsert) then FAfterInsert(self); // erb added 2/25/08
end;

procedure TDataGrid.DeleteRow(ARow: LongInt);
var
   I, L: LongInt;
begin
     if assigned(FBeforeDelete) then FBeforeDelete(self);
     if (RowCount > FixedRows+1) and ((RowCountMin = 0) or (RowCount > RowCountMin)) then
     begin
        for I := ARow to RowCount-1 do for L := 0 to ColCount-1 do

        begin
          cells[L,I] := cells[L,I+1];
          {RBW begin change}
          Objects[L,I] := Objects[L,I+1];
          {RBW end change}
        end;
        for L := 0 to Colcount -1 do cells[L, RowCount-1] := '';
        RowCount := RowCount - 1;
     end else for L := FixedCols to ColCount-1 do cells[L, ARow] := '';
     InvalidateEditor;
     if assigned(FAfterDelete) then FAfterDelete(self); // erb added 2/25/08
end;

{ TColumnTitle }
constructor TColumnTitle.Create(Column: TColumn);
begin
  inherited Create;
  FColumn := Column;
  FFont := TFont.Create;
  FFont.Assign(DefaultFont);
  FFont.OnChange := FontChanged;
end;

destructor TColumnTitle.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TColumnTitle.Assign(Source: TPersistent);
begin
  if Source is TColumnTitle then
  begin
    if cvTitleAlignment in TColumnTitle(Source).FColumn.FAssignedValues then
      Alignment := TColumnTitle(Source).Alignment;
{    if cvTitleColor in TColumnTitle(Source).FColumn.FAssignedValues then
      Color := TColumnTitle(Source).Color;}
    if cvTitleCaption in TColumnTitle(Source).FColumn.FAssignedValues then
      Caption := TColumnTitle(Source).Caption;
    if cvTitleFont in TColumnTitle(Source).FColumn.FAssignedValues then
      Font := TColumnTitle(Source).Font;
    WordWrap := TColumnTitle(Source).WordWrap;
  end
  else
    inherited Assign(Source);
end;

function TColumnTitle.DefaultAlignment: TAlignment;
begin
  Result := taLeftJustify;
end;

function TColumnTitle.DefaultColor: TColor;
var
  Grid: TDataGrid;
begin
  Grid := FColumn.GetGrid;
  if Assigned(Grid) then
    Result := Grid.FixedColor
  else
    Result := clBtnFace;
end;

function TColumnTitle.DefaultFont: TFont;
var
  Grid: TDataGrid;
begin
  Grid := FColumn.GetGrid;
  if Assigned(Grid) then
    Result := Grid.font //Grid.TitleFont
  else
    Result := FColumn.Font;
end;

function TColumnTitle.DefaultCaption: string;
begin
  Result := '';
end;

procedure TColumnTitle.FontChanged(Sender: TObject);
begin
  Include(FColumn.FAssignedValues, cvTitleFont);
  FColumn.Changed(True);
end;

function TColumnTitle.GetAlignment: TAlignment;
begin
  if cvTitleAlignment in FColumn.FAssignedValues then
    Result := FAlignment
  else
    Result := DefaultAlignment;
end;

function TColumnTitle.GetCaption: string;
begin
  if cvTitleCaption in FColumn.FAssignedValues then
    Result := FCaption
  else
    Result := DefaultCaption;
end;

function TColumnTitle.GetFont: TFont;
var
  Save: TNotifyEvent;
  Def: TFont;
begin
  if not (cvTitleFont in FColumn.FAssignedValues) then
  begin
    Def := DefaultFont;
    if (FFont.Handle <> Def.Handle) or (FFont.Color <> Def.Color) then
    begin
      Save := FFont.OnChange;
      FFont.OnChange := nil;
      FFont.Assign(DefaultFont);
      FFont.OnChange := Save;
    end;
  end;
  Result := FFont;
end;

function TColumnTitle.IsAlignmentStored: Boolean;
begin
  Result := (cvTitleAlignment in FColumn.FAssignedValues) and
    (FAlignment <> DefaultAlignment);
end;

function TColumnTitle.IsFontStored: Boolean;
begin
  Result := (cvTitleFont in FColumn.FAssignedValues);
end;

function TColumnTitle.IsCaptionStored: Boolean;
begin
  Result := (cvTitleCaption in FColumn.FAssignedValues) and
    (FCaption <> DefaultCaption);
end;

procedure TColumnTitle.RefreshDefaultFont;
var
  Save: TNotifyEvent;
begin
  if (cvTitleFont in FColumn.FAssignedValues) then Exit;
  Save := FFont.OnChange;
  FFont.OnChange := nil;
  try
    FFont.Assign(DefaultFont);
  finally
    FFont.OnChange := Save;
  end;
end;

procedure TColumnTitle.RestoreDefaults;
var
  FontAssigned: Boolean;
begin
  FontAssigned := cvTitleFont in FColumn.FAssignedValues;
  FColumn.FAssignedValues := FColumn.FAssignedValues - ColumnTitleValues;
  FCaption := '';
  RefreshDefaultFont;
  FColumn.Changed(FontAssigned);
end;

procedure TColumnTitle.SetAlignment(Value: TAlignment);
begin
  if (cvTitleAlignment in FColumn.FAssignedValues) and (Value = FAlignment) then Exit;
  FAlignment := Value;
  Include(FColumn.FAssignedValues, cvTitleAlignment);
  FColumn.Changed(False);
end;

procedure TColumnTitle.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TColumnTitle.SetCaption(const Value: string);
begin
  if (cvTitleCaption in FColumn.FAssignedValues) and (Value = FCaption) then Exit;
  FCaption := Value;
  Include(FColumn.FAssignedValues, cvTitleCaption);
  FColumn.Changed(False);
end;


procedure TColumnTitle.SetWordWrap(const Value: boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    FColumn.Changed(False);
  end;
end;

{ TColumn }

constructor TColumn.Create(Collection: TCollection);
var
  Grid: TDataGrid;
begin
  Grid := nil;
  if Assigned(Collection) and (Collection is TDataGridColumns) then Grid := TDataGridColumns(Collection).Grid;
  try
    inherited Create(Collection);
    FDropDownRows := 7;
    FButtonStyle := cbsAuto;
    FFont := TFont.Create;
    FFont.Assign(DefaultFont);
    FFont.OnChange := FontChanged;
    FReadOnly := false;
    FTitle := CreateTitle;
  finally
    if (Grid <> nil) then
    begin
         grid.setcolumncount(Grid.columns.count);
         if not (csDesigning in Grid.ComponentState) then FInternalCol := Grid.columns.count-1;
    end;
  end;
end;

destructor TColumn.Destroy;
begin
  FTitle.Free;
  FFont.Free;
  FPickList.Free;
  with TDataGridColumns(Collection).Grid do if FLayoutFlag = 0 then setcolumncount(Grid.colcount-1);
  inherited Destroy;
end;

procedure TColumn.Assign(Source: TPersistent);
begin
  if Source is TColumn then
  begin
    if Assigned(Collection) then Collection.BeginUpdate;
    try
      RestoreDefaults;
      if cvColor in TColumn(Source).AssignedValues then
        Color := TColumn(Source).Color;
      if cvFont in TColumn(Source).AssignedValues then
        Font := TColumn(Source).Font;
      if cvAlignment in TColumn(Source).AssignedValues then
        Alignment := TColumn(Source).Alignment;
      if cvReadOnly in TColumn(Source).AssignedValues then
        ReadOnly := TColumn(Source).ReadOnly;
      Title := TColumn(Source).Title;
      DropDownRows := TColumn(Source).DropDownRows;
      ButtonStyle := TColumn(Source).ButtonStyle;
      PickList := TColumn(Source).PickList;
      PopupMenu := TColumn(Source).PopupMenu;
    finally
      if Assigned(Collection) then Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TColumn.CreateTitle: TColumnTitle;
begin
  Result := TColumnTitle.Create(Self);
end;

function TColumn.DefaultAlignment: TAlignment;
begin
    Result := taLeftJustify;
end;

function TColumn.DefaultColor: TColor;
var
  Grid: TDataGrid;
begin
  Grid := GetGrid;
  if Assigned(Grid) then
    Result := Grid.Color
  else
    Result := clWindow;
end;

function TColumn.DefaultFont: TFont;
var
  Grid: TDataGrid;
begin
  Grid := GetGrid;
  if Assigned(Grid) then
    Result := Grid.Font
  else
    Result := FFont;
end;

function TColumn.DefaultReadOnly: Boolean;
var
  Grid: TDataGrid;
begin
  Grid := GetGrid;
  if Assigned(Grid) then Result := not (goEditing in Grid.options) else Result := true;
end;

function TColumn.DefaultWidth: Integer;
begin
  if GetGrid = nil then
  begin
    Result := 64;
    Exit;
  end;
  with GetGrid do
  begin
      Result := DefaultColWidth;
  end;
end;

procedure TColumn.FontChanged;
begin
  Include(FAssignedValues, cvFont);
  Title.RefreshDefaultFont;
  Changed(False);
end;

function TColumn.GetAlignment: TAlignment;
begin
  if cvAlignment in FAssignedValues then
    Result := FAlignment
  else
    Result := DefaultAlignment;
end;

function TColumn.GetColor: TColor;
begin
  if cvColor in FAssignedValues then
    Result := FColor
  else
    Result := DefaultColor;
end;

function TColumn.GetFont: TFont;
var
  Save: TNotifyEvent;
begin
  if not (cvFont in FAssignedValues) and (FFont.Handle <> DefaultFont.Handle) then
  begin
    Save := FFont.OnChange;
    FFont.OnChange := nil;
    FFont.Assign(DefaultFont);
    FFont.OnChange := Save;
  end;
  Result := FFont;
end;

function TColumn.GetGrid: TDataGrid;
begin
  if Assigned(Collection) and (Collection is TDataGridColumns) then
    Result := TDataGridColumns(Collection).Grid
  else
    Result := nil;
end;

function TColumn.GetPickList: TStrings;
begin
  if FPickList = nil then
    FPickList := TStringList.Create;
  Result := FPickList;
end;

function TColumn.GetReadOnly: Boolean;
begin
  if cvReadOnly in FAssignedValues then
    Result := FReadOnly
  else
    Result := DefaultReadOnly;
end;

function TColumn.GetDisplayMask: String;
begin
    Result := FDisplayMask;
end;

function TColumn.IsAlignmentStored: Boolean;
begin
  Result := (cvAlignment in FAssignedValues) and (FAlignment <> DefaultAlignment);
end;

function TColumn.IsColorStored: Boolean;
begin
  Result := (cvColor in FAssignedValues) and (FColor <> DefaultColor);
end;

function TColumn.IsFontStored: Boolean;
begin
  Result := (cvFont in FAssignedValues);
end;

function TColumn.IsReadOnlyStored: Boolean;
begin
  Result := (cvReadOnly in FAssignedValues) and (FReadOnly <> DefaultReadOnly);
end;

function TColumn.IsDisplayMaskStored: Boolean;
begin
  Result := true;
end;

procedure TColumn.RefreshDefaultFont;
var
  Save: TNotifyEvent;
begin
  if cvFont in FAssignedValues then Exit;
  Save := FFont.OnChange;
  FFont.OnChange := nil;
  try
    FFont.Assign(DefaultFont);
  finally
    FFont.OnChange := Save;
  end;
end;

procedure TColumn.RestoreDefaults;
var
  FontAssigned: Boolean;
begin
  FontAssigned := cvFont in FAssignedValues;
  FTitle.RestoreDefaults;
  FAssignedValues := [];
  RefreshDefaultFont;
  FPickList.Free;
  FPickList := nil;
  ButtonStyle := cbsAuto;
  Changed(FontAssigned);
end;

procedure TColumn.SetAlignment(Value: TAlignment);
begin
  if (cvAlignment in FAssignedValues) and (Value = FAlignment) then Exit;
  FAlignment := Value;
  Include(FAssignedValues, cvAlignment);
  Changed(False);
end;

procedure TColumn.SetButtonStyle(Value: TColumnButtonStyle);
begin
  if Value = FButtonStyle then Exit;
  FButtonStyle := Value;
  Changed(False);
end;

procedure TColumn.SetFormat(Value: TColumnFormat);
begin
  if Value = FFormat then Exit;
  FFormat := Value;
  FDisplayMask := '';
  Changed(False);
end;

procedure TColumn.SetColor(Value: TColor);
begin
  if (cvColor in FAssignedValues) and (Value = FColor) then Exit;
  FColor := Value;
  Include(FAssignedValues, cvColor);
  Changed(False);
end;


procedure TColumn.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  Include(FAssignedValues, cvFont);
  Changed(False);
end;

procedure TColumn.SetPickList(Value: TStrings);
begin
  if Value = nil then
  begin
    FPickList.Free;
    FPickList := nil;
    Exit;
  end;
  PickList.Assign(Value);
end;

procedure TColumn.SetPopupMenu(Value: TPopupMenu);
begin
  FPopupMenu := Value;
  if Value <> nil then Value.FreeNotification(GetGrid);
end;

procedure TColumn.SetReadOnly(Value: Boolean);
begin
  if (cvReadOnly in FAssignedValues) and (Value = FReadOnly) then Exit;
  FReadOnly := Value;
  Include(FAssignedValues, cvReadOnly);
  Changed(False);
end;

procedure TColumn.SetTitle(Value: TColumnTitle);
begin
  FTitle.Assign(Value);
end;

function TColumn.GetMaxLength: Integer;
begin
  Result := FMaxLength;
end;

procedure TColumn.SetMaxLength(Value: Integer);
begin
  FMaxLength := Value;
end;

procedure TColumn.SetDisplayMask(Value: String);
begin
  FDisplayMask := value;
  Changed(False);
end;

{ TDataGridColumns }

constructor TDataGridColumns.Create(Grid: TDataGrid; ColumnClass: TColumnClass);
begin
  inherited Create(ColumnClass);
  FGrid := Grid;
end;

function TDataGridColumns.Add: TColumn;
begin
  Result := TColumn(inherited Add);
end;

function TDataGridColumns.GetColumn(Index: Integer): TColumn;
begin
  Result := TColumn(inherited Items[Index]);
end;

function TDataGridColumns.GetOwner: TPersistent;
begin
  Result := FGrid;
end;

procedure TDataGridColumns.LoadFromFile(const Filename: string);
var
  S: TFileStream;
begin
  S := TFileStream.Create(Filename, fmOpenRead);
  try
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

type
  TColumnsWrapper = class(TComponent)
  private
    FColumns: TDataGridColumns;
  published
    property Columns: TDataGridColumns read FColumns write FColumns;
  end;

procedure TDataGridColumns.LoadFromStream(S: TStream);
var
  Wrapper: TColumnsWrapper;
begin
  Wrapper := TColumnsWrapper.Create(nil);
  try
    Wrapper.Columns := FGrid.CreateColumns;
    S.ReadComponent(Wrapper);
    Assign(Wrapper.Columns);
  finally
    Wrapper.Columns.Free;
    Wrapper.Free;
  end;
end;

procedure TDataGridColumns.RestoreDefaults;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Count-1 do
      Items[I].RestoreDefaults;
  finally
    EndUpdate;
  end;
end;

procedure TDataGridColumns.SaveToFile(const Filename: string);
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

procedure TDataGridColumns.SaveToStream(S: TStream);
var
  Wrapper: TColumnsWrapper;
begin
  Wrapper := TColumnsWrapper.Create(nil);
  try
    Wrapper.Columns := Self;
    S.WriteComponent(Wrapper);
  finally
    Wrapper.Free;
  end;
end;

procedure TDataGridColumns.SetColumn(Index: Integer; Value: TColumn);
begin
  Items[Index].Assign(Value);
end;

procedure TDataGridColumns.Update(Item: TCollectionItem);
begin
     if (FGrid = nil) or (csLoading in FGrid.ComponentState) then Exit;

     if (csDesigning in FGrid.ComponentState) then FGrid.invalidate
     else FGrid.invalidatecol(FGrid.SelectedIndex);
end;


{ DataGridInplaceEdit }

type
  TSelection = record
    StartPos, EndPos: Integer;
  end;

procedure TDataGridInplaceEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (EditStyle = esEllipsis) and (Key = VK_RETURN) and (Shift = [ssCtrl]) then
  begin
    TDataGrid(Grid).EditButtonClick;
    KillMessage(Handle, WM_CHAR);
  end else
  begin
    case key of
      VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_PRIOR, VK_NEXT, VK_TAB, VK_HOME, VK_END: validateContent;
    end;

    with TDataGrid(Grid) do if CheckDataGridkey(key, shift) then
    begin
         keydown(key, Shift);
         key := 0;
         Shift := [];
    end;
    inherited KeyDown(Key, Shift);
  end;
end;

procedure TDataGridInplaceEdit.KeyPress(var Key: Char);
var
  Selection: TSelection;
  I: Integer;
begin
  TDatagrid(Grid).KeyPress(Key);
  {$IFDEF Delphi_2009_UP}
  if CharInSet(Key, [#32..#255]) and not TDatagrid(Grid).CanEditAcceptKey(Key) then
  {$ELSE}
  if (Key in [#32..#255]) and not TDatagrid(Grid).CanEditAcceptKey(Key) then
  {$ENDIF}
  begin
    Key := #0;
    MessageBeep(0);
  end;
  case Key of
    #9, #27: Key := #0;
    #13:
      begin
        SendMessage(Handle, EM_GETSEL, Longint(@Selection.StartPos), Longint(@Selection.EndPos));
        if (Selection.StartPos = 0) and (Selection.EndPos = GetTextLen) then
          Deselect else
          SelectAll;
        Key := #0;
      end;
    ^H, ^V, ^X, #32..#255:
      if not TDatagrid(Grid).CanEditModify then Key := #0;
  end;
  if (Key <> #0) then
  begin
       if (EditStyle = esPickList) and readonly then
       begin
       //check if picklist was visible...(items are assigned in dropdown proc)
            if not FListVisible and Assigned(FActiveList) then
            with TDataGrid(Grid) do FPickList.items := Columns[SelectedIndex].Picklist;

            for I := 0 to FPicklist.items.count-1 do if uppercase(copy(FPickList.items[i],1,1)) = uppercase(Key) then
            begin
                 Text := FPickList.items[i];
                 with TDatagrid(Grid) do SetEditText(col, row, Text);
                 modified := true;
                 Key := #0;
                 break;
            end;
       end;
       inherited KeyPress(Key);
  end;
end;

procedure TDataGridInplaceEdit.ListMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    CloseUp(PtInRect(FActiveList.ClientRect, Point(X, Y)));
end;

procedure TDataGridInplaceEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if ((Button = mbLeft) and (FEditStyle <> esSimple) and
    PtInRect(Rect(Width - FButtonWidth, 0, Width, Height), Point(X,Y))) or
    ((Button = mbLeft) and (ssDouble in Shift) and (FEditStyle <> esSimple)) and ReadOnly then
  begin
    if FEditStyle = esEllipsis then TDataGrid(Grid).EditButtonClick
    else begin
         if FListVisible then CloseUp(False)
         else begin
              MouseCapture := True;
              FTracking := True;
              TrackButton(X, Y);
              if Assigned(FActiveList) then DropDown;
         end;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TDataGridInplaceEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ListPos: TPoint;
  MousePos: TSmallPoint;
begin
  if FTracking then
  begin
    TrackButton(X, Y);
    if FListVisible then
    begin
      ListPos := FActiveList.ScreenToClient(ClientToScreen(Point(X, Y)));
      if PtInRect(FActiveList.ClientRect, ListPos) then
      begin
        StopTracking;
        MousePos := PointToSmallPoint(ListPos);
        SendMessage(FActiveList.Handle, WM_LBUTTONDOWN, 0, Integer(MousePos));
        Exit;
      end;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TDataGridInplaceEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  WasPressed: Boolean;
begin
  WasPressed := FPressed;
  StopTracking;
  if (Button = mbLeft) and (FEditStyle = esEllipsis) and WasPressed then
    TDataGrid(Grid).EditButtonClick;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TDataGridInplaceEdit.PaintWindow(DC: HDC);
var
  R: TRect;
  Flags: Integer;
  W: Integer;
begin
  if FEditStyle <> esSimple then
  begin
    SetRect(R, Width - FButtonWidth, 0, Width, Height);
    Flags := 0;
    if FEditStyle in [esDataList, esPickList] then
    begin
      if FActiveList = nil then
        Flags := DFCS_INACTIVE
      else if FPressed then
        Flags := DFCS_FLAT or DFCS_PUSHED;
      DrawFrameControl(DC, R, DFC_SCROLL, Flags or DFCS_SCROLLCOMBOBOX);
    end
    else   { esEllipsis }
    begin
      if FPressed then Flags := BF_FLAT;
      DrawFrameControl(DC, R, DFC_BUTTON, Flags or DFCS_BUTTONPUSH);
      Flags := ((R.Right - R.Left) shr 1) - 1 + Ord(FPressed);

      W := Height shr 3;
      if W = 0 then W := 1;
      PatBlt(DC, R.Left + Flags, R.Top + Flags + 3, W, W, BLACKNESS);
      PatBlt(DC, R.Left + Flags - (W * 2), R.Top + Flags + 3, W, W, BLACKNESS);
      PatBlt(DC, R.Left + Flags + (W * 2), R.Top + Flags + 3, W, W, BLACKNESS);
    end;
    ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
  end;
  inherited PaintWindow(DC);
end;

procedure TDataGridInplaceEdit.SetEditStyle(Value: TEditStyle);
begin
  FEditStyle := Value;
  case Value of
    esPickList:
      begin
        if FPickList = nil then
        begin
          FPickList := TPopupListbox.Create(Self);
          FPickList.Visible := False;
          FPickList.Parent := Self;
          FPickList.OnMouseUp := ListMouseUp;
          FPickList.IntegralHeight := True;
          FPickList.ItemHeight := 11;
        end;
        FActiveList := FPickList;
      end;
  else  { cbsNone, cbsEllipsis, or read only field }
    FActiveList := nil;
  end;
  with TDataGrid(Grid) do if Columns.count > SelectedIndex then
  begin
     Self.ReadOnly := Columns[SelectedIndex].ReadOnly or ((FEditStyle = esPickList) and Columns[SelectedIndex].Limittolist);
  end else Self.ReadOnly := false;
  Repaint;
end;

procedure TDataGridInplaceEdit.StopTracking;
begin
  if FTracking then
  begin
    TrackButton(-1, -1);
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TDataGridInplaceEdit.TrackButton(X,Y: Integer);
var
  NewState: Boolean;
  R: TRect;
begin
  SetRect(R, ClientWidth - FButtonWidth, 0, ClientWidth, ClientHeight);
  NewState := PtInRect(R, Point(X, Y));
  if FPressed <> NewState then
  begin
    FPressed := NewState;
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TDataGridInplaceEdit.ValidateContent;
var
   value: string;
   q, cr, lf: string;
   ICR: integer;
begin
  with TDataGrid(Grid) do
  begin
    value := cells[col, row];
  end;
  q := #39;  // #39 is ascii code for single quote.
  cr := #13; // #13 is ascii code for carriage return.
  lf := #10; // #10 is ascii code for line feed.
  if not modified then exit;
  with TDataGrid(Grid) do if columns.count > SelectedIndex then
  try
       if cells[col, row] <> '' then
       begin
            Case Columns[SelectedIndex].Format of
              cfString: value := cells[col, row];
              cfNumber: begin
                // Edited by ERB 2/12/2009 to enable
                // handling of text pasted from clipboard.
                          value := cells[col, row];
                          // Deal with text pasted from clipboard.
                          ICR := Pos(cr,value);
                          if ICR > 0 then
                            // Extract part preceding the carriage return.
                            begin
//                              Delete(value,1,1);
//                              I := Pos(q,value);
//                              if I > 0 then
                              SetLength(value,ICR-1);
                            end;
                          value := floattostr(strtofloat(value));
                        end;
              cfDate:   begin
                             value := cells[col, row];
                             if pos(uppercase(copy(value,1,1)), 'NTHJD') > 0 then value := datetostr(now)
                             else value := datetostr(strtodate(value));
                        end;
            end;
            cells[col, row] := value;
       end;
  except
       beep; 
       SetEditText(col, row, FLastText);
       cells[col, row] := FLastText;
       Modified := false;
  end;
  with TDataGrid(Grid) do if Assigned(FOnUserChanged) then FOnUserChanged(self);
end;

procedure TDataGridInplaceEdit.UpdateContents;
var
  NewStyle: TEditStyle;
  Column: TColumn;
begin
  NewStyle := esSimple;
  with TDataGrid(Grid) do if columns.count > SelectedIndex then
  begin
       Column := Columns[SelectedIndex];
       case Column.ButtonStyle of
            cbsEllipsis: NewStyle := esEllipsis;
            cbsAuto:
                 if Assigned(Column.Picklist) and (Column.PickList.Count > 0) and
                    not Column.Readonly then
                        NewStyle := esPickList;
       end;
  end;
  EditStyle := NewStyle;
  inherited UpdateContents;
  with TDataGrid(Grid) do if columns.count > SelectedIndex then MaxLength := columns[SelectedIndex].MaxLength;
  FLastText := EditText;
end;

procedure TDataGridInplaceEdit.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and (Message.Sender <> FActiveList) then
    CloseUp(False);
end;

procedure TDataGridInplaceEdit.WMCancelMode(var Message: TMessage);
begin
  StopTracking;
  inherited;
end;

procedure TDataGridInplaceEdit.WMKillFocus(var Message: TMessage);
begin
  inherited;
  CloseUp(False);
  ValidateContent;
end;

procedure TDataGridInplaceEdit.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  with Message do
  if (FEditStyle <> esSimple) and
    PtInRect(Rect(Width - FButtonWidth, 0, Width, Height), Point(XPos, YPos)) then
    Exit;
  inherited;
end;

procedure TDataGridInplaceEdit.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

procedure TDataGridInplaceEdit.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
begin
  GetCursorPos(P);
  if (FEditStyle <> esSimple) and
    PtInRect(Rect(Width - FButtonWidth, 0, Width, Height), ScreenToClient(P)) then
    Windows.SetCursor(LoadCursor(0, idc_Arrow))
  else
    inherited;
end;

procedure TDataGridInplaceEdit.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    wm_KeyDown, wm_SysKeyDown, wm_Char:
      if EditStyle in [esPickList, esDataList] then
      with TWMKey(Message) do
      begin
        DoDropDownKeys(CharCode, KeyDataToShiftState(KeyData));
        if (CharCode <> 0) and FListVisible then
        begin
          with TMessage(Message) do
            SendMessage(FActiveList.Handle, Msg, WParam, LParam);
          Exit;
        end;
      end
  end;
  inherited;
end;

procedure TPopupListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

procedure TPopupListbox.CreateWnd;
begin
  inherited CreateWnd;
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, wm_SetFocus, 0, 0);
end;

procedure TPopupListbox.Keypress(var Key: Char);
var
  TickCount: Integer;
begin
  case Key of
    #8, #27: FSearchText := '';
    #32..#255:
      begin
        TickCount := GetTickCount;
        if TickCount - FSearchTickCount > 2000 then FSearchText := '';
        FSearchTickCount := TickCount;
        if Length(FSearchText) < 32 then FSearchText := FSearchText + Key;
        SendMessage(Handle, LB_SelectString, WORD(-1), Longint(PChar(FSearchText)));
        Key := #0;
      end;
  end;
  inherited Keypress(Key);
end;

procedure TPopupListbox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  TDataGridInPlaceEdit(Owner).CloseUp((X >= 0) and (Y >= 0) and
      (X < Width) and (Y < Height));
end;

constructor TDataGridInplaceEdit.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
  FEditStyle := esSimple;
end;

procedure TDataGridInplaceEdit.BoundsChanged;
var
  R: TRect;
begin
  SetRect(R, 2, 2, Width - 2, Height);
  if FEditStyle <> esSimple then Dec(R.Right, FButtonWidth);
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@R));
  SendMessage(Handle, EM_SCROLLCARET, 0, 0);
end;

procedure TDataGridInplaceEdit.CloseUp(Accept: Boolean);
var
  ListValue: Variant;
begin
  if FListVisible then
  begin
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);

    if FPickList.ItemIndex <> -1 then ListValue := FPickList.Items[FPicklist.ItemIndex];

    SetWindowPos(FActiveList.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    FListVisible := False;
    Invalidate;
    if Accept then if (not VarIsNull(ListValue)) then
    begin
         if (EditCanModify or (not EditCanModify and not Self.Readonly)) then
         begin
//              Text := ListValue;
              with TDatagrid(Grid) do cells[col, row] := ListValue;
              // RBW begin change
              if Assigned(TDatagrid(Grid).FOnUserChanged) then TDatagrid(Grid).FOnUserChanged(Grid);
              with TDatagrid(Grid) do
              begin
              if Assigned(OnSetEditText) then
                 OnSetEditText(Grid, Col, Row, ListValue);
              end;
              // RBW end change
              modified := true;
         end;
    end;
  end;
end;

procedure TDataGridInplaceEdit.DoDropDownKeys(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP, VK_DOWN:
      if ssAlt in Shift then
      begin
        if FListVisible then CloseUp(True) else DropDown;
        Key := 0;
      end;
    VK_RETURN, VK_ESCAPE:
      if FListVisible and not (ssAlt in Shift) then
      begin
        CloseUp(Key = VK_RETURN);
        Key := 0;
      end;
  end;
end;

procedure TDataGridInplaceEdit.DropDown;
var
  P: TPoint;
  I,J,Y: Integer;
  Column: TColumn;
  AFont : TFont;
begin
  if not FListVisible and Assigned(FActiveList) then
  begin
    FActiveList.Width := Width;
    with TDataGrid(Grid) do Column := Columns[SelectedIndex];

      FPickList.Color := Color;
      FPickList.Font := Font;
      FPickList.Items := Column.Picklist;
      if FPickList.Items.Count >= Column.DropDownRows then
        FPickList.Height := Column.DropDownRows * FPickList.ItemHeight + 4
      else
        FPickList.Height := FPickList.Items.Count * FPickList.ItemHeight + 4;
//setpicklist...
      FPickList.itemindex := FPickList.items.indexof(Text);
      if FPickList.itemindex = -1 then FPickList.itemindex := 0;

      J := FPickList.ClientWidth;
      for I := 0 to FPickList.Items.Count - 1 do
      begin
        {new}
        AFont := TFont.Create;
        AFont.Assign(FPickList.Canvas.Font);
        try
          FPickList.Canvas.Font.Assign(Font);
          {end new}
          Y := FPickList.Canvas.TextWidth(FPickList.Items[I]) {new} + 4 {end new} ;
        {new}
        finally
          FPickList.Canvas.Font.Assign(AFont);
          AFont.Free;
        end;
        {end new}
        if Y > J then J := Y;
      end;
      FPickList.ClientWidth := J;

    P := Parent.ClientToScreen(Point(Left, Top));
    Y := P.Y + Height;
    if Y + FActiveList.Height > Screen.Height then Y := P.Y - FActiveList.Height;
    SetWindowPos(FActiveList.Handle, HWND_TOP, P.X, Y, 0, 0,
      SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
    FListVisible := True;

    Invalidate;
    Windows.SetFocus(Handle);
  end;
end;

constructor TDBDataGrid.Create(AOwner: TComponent);
begin
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  inherited Create(AOwner);
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TDBDataGrid.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

function TDBDataGrid.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TDBDataGrid.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBDataGrid.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TDBDataGrid.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TDBDataGrid.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TDBDataGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if CheckDataGridKey(key, shift) then if not SetEditing then exit;
  inherited keydown(key, shift);
end;

function TDBDataGrid.SetEditing: Boolean;
begin
  Result := FDatalink.Editing;
  if not Result then
  begin
       Result := FDatalink.canmodify;
       if Result then begin
          FDatalink.Edit;
          Result := FDatalink.Editing;
          if Result then FDatalink.Modified;
       end;
  end;
end;

procedure TDBDataGrid.SizeChanged(OldColCount, OldRowCount: Longint);
begin
     inherited;
     if not (csLoading in ComponentState) and (FLayoutFlag = 0) then DataChange(self);
end;

procedure TDBDataGrid.RowCountMinChanged;
begin
     if not (csLoading in ComponentState) and (FLayoutFlag = 0) then DataChange(self);
end;

procedure TDBDataGrid.DataChange(Sender: TObject);
var
   Lines, C: TStringList;
   I,L: Integer;
   InternalCol: LongInt;
begin
    if FLayoutFlag <> 0 then exit;

    Lines := TStringList.create;
    C     := TStringList.create;
    if FDataLink.field <> nil then Lines.assign( FDataLink.Field );

    for I := FixedRows to Rowcount-1 do Rows[i].clear;

    if Lines.count > 0 then I := FixedRows+Lines.count else I := FixedRows+1;
    if I < RowCountMin then RowCount := RowCountMin else RowCount := I;
    for I := 0 to Lines.count-1 do
    begin
         C.text := replaceChar(#9,Lines[i],#13);
         for L := FixedCols to (ColCount-1) do
         begin
              if (Columns.count > L+FixedCols) and not (csDesigning in ComponentState) then InternalCol := Columns[L].InternalCol
              else InternalCol := L;
              if (InternalCol-FixedCols) < C.count then Cells[L,I+FixedRows] := C[InternalCol-FixedCols]
              else Cells[L,I+FixedRows] := '';
         end;
    end;
    Lines.free;
    C.free;
end;

procedure TDBDataGrid.EditingChange(Sender: TObject);
begin
  if (FDatalink.editing and (FDatalink.datasource.state <> dsInsert)) then FlayoutFlag := 2
  else FLayoutflag := 0;
end;

procedure TDBDataGrid.UpdateData(Sender: TObject);
var
   Lines: TStringList;
   I, L, K: Integer;
   InternalCol, LastFilledRow: LongInt;
begin
    hideeditor;    //forces  TDataGridInplaceEdit.validatecontent
    Lines := TStringlist.create;
    LastFilledRow := 0;
    for I := FixedRows to Rowcount-1 do
    begin
         K := Lines.add('');
         for L := FixedCols to ColCount -1 do
         begin
              if Columns.count > L then InternalCol := Columns[L].InternalCol
              else InternalCol := L;
              Lines[k] := Lines[k] + Cells[InternalCol,I];
              if L < ColCount-1 then Lines[k] := Lines[k] + #9;
              if Cells[InternalCol,I] <> '' then LastFilledRow := I-FixedRows;
         end;
    end;
    while Lines.count-1 > LastFilledRow do Lines.delete(LastFilledRow+1);
    FDataLink.Field.assign(Lines);
end;

function TDBDataGrid.CanEditModify: Boolean;
begin
  Result := (goEditing in Options) and FDatalink.canmodify;
  if Result and (Columns.count > SelectedIndex) then with Columns[SelectedIndex] do result := not ReadOnly;
  if result then SetEditing;
end;

function ReplaceChar(C1: Char; S: String; C2: Char): String;
var
   I: Integer;
begin
     result := '';
     repeat
           I := pos(C1,S);
           if I > 0 then
           begin
                result := result + copy(S,1,I-1) + C2;
                S := copy(S,I+1,length(S)-I);
           end else result := result + S;
     until I = 0;
end;

procedure KillMessage(Wnd: HWnd; Msg: Integer);
var
  M: TMsg;
begin
  M.Message := 0;
  if PeekMessage(M, Wnd, Msg, Msg, pm_Remove) and (M.Message = WM_QUIT) then PostQuitMessage(M.wparam);
end;

procedure Register;
begin
  RegisterComponents('EC', [{TDataGrid,}TDBDataGrid,TEcDataGrid]);
end;

end.

