{
Richard B. Winston
Nov. 1, 2006
rbwinst@usgs.gov
rbwinston@mindspring.com
}
{--------------------------------------------------------------------------------
@author(Richard B. Winston; rbwinst@usgs.gov, rbwinston@mindspring.com)

@abstract(
The purpose of @name is to define @link(TRbwDataGrid4)
and @link(TRbwRowDataGrid).  These two components allow individual columns
or rows to be specialized for specific data types and to have other
related properties.)

The software and related documentation were developed by
the U.S. Geological Survey (USGS) for use by the USGS in fulfilling its mission.
The software can be used, copied, modified, and distributed without any fee
or cost. Use of appropriate credit is requested. The software is provided as
a minimum in source code form as used on USGS computers. In many cases, the
executable runfiles also are provided for these computers.

The USGS provides no warranty, expressed or implied, as to the correctness
of the furnished software or the suitability for any purpose. The software
has been tested, but as with any complex software, there could be undetected
errors. Users who find errors are requested to report them to the USGS. The
USGS has limited resources to assist non-USGS users; however, we make an
attempt to fix reported problems and help whenever possible.
--------------------------------------------------------------------------------
}

unit RbwDataGrid4;

{$ifdef CONDITIONALEXPRESSIONS}
  {$if CompilerVersion>=20}
    {$DEFINE Delphi_2009_UP}
  {$ifend}
{$endif}

interface

uses 
{$ifdef CONDITIONALEXPRESSIONS}
  {$if CompilerVersion>=31}
     System.UITypes,
  {$ifend}
{$endif}
  Windows, StdCtrls, Graphics, Classes, Grids, SysUtils, Messages, Types,
  Controls, Forms, Clipbrd, Buttons, AppEvnts;

const
  // @name represents the size of a check box.
  CheckBoxSize = 13;
  // @name represents the width of a combobox occupied by the down arrow.
  // @name is used in @link(TCustomRBWDataGrid.AdjustColWidths) to ensure
  // that the text in a cell is fully visible when that text is being
  // edited in a combobox.
  ComboAdustSize = 22;

type
  // @name specifies the type of data stored in a column of a
  // @link(TRbwDataGrid4) or a row of a @link(TRbwRowDataGrid).
  TRbwColumnFormat4 = (rcf4String, rcf4Integer, rcf4Real, rcf4Boolean);

  EColWidthError = class(Exception);
  EInvalidColumn = class(Exception);

  // @name is used in @link(TCustomRBWDataGrid.SpecialFormat) to assign
  // a @link(TRbwColumnFormat4) to a particular cell that is different from
  // the default format for that cell.
  ISpecialFormatter = interface(IUnknown)
    // @name sets the @link(TRbwColumnFormat4) of a cell.
    // See @link(Format).
    procedure SetFormat(const Value: TRbwColumnFormat4);
    // @name gets the @link(TRbwColumnFormat4) of a cell.
    // See @link(Format).
    function GetFormat: TRbwColumnFormat4;
    // @name gets whether a special format is used for a
    // particular cell.
    function GetUsed: boolean;
    // @name sets whether a special format is used for a
    // particular cell.
    procedure SetUsed(const Value: boolean);
    // @name is the @link(TRbwColumnFormat4) of a cell.
    property Format: TRbwColumnFormat4 read GetFormat write SetFormat;
    // @name is used to specify whether a special format is used for a
    // particular cell.
    property Used: boolean read GetUsed write SetUsed;
  end;

  {$TYPEINFO ON}
  // @name implements @link(ISpecialFormatter).
  TSpecialFormatter = Class(TInterfacedObject, ISpecialFormatter)
  private
    // @name is the field whose value is get and set in @link(Format).
    FFormat: TRbwColumnFormat4;
    // @name is the field whose value is get and set in @link(Used).
    FUsed: boolean;
    // See @link(Format).
    // @seealso(ISpecialFormatter.SetFormat ISpecialFormatter.SetFormat).
    procedure SetFormat(const Value: TRbwColumnFormat4);
    // See @link(Format).
    // @seealso(ISpecialFormatter.GetFormat ISpecialFormatter.GetFormat).
    function GetFormat: TRbwColumnFormat4;
    // See @link(Used).
    // @seealso(ISpecialFormatter.GetUsed ISpecialFormatter.GetUsed).
    function GetUsed: boolean;
    // See @link(Used).
    // @seealso(ISpecialFormatter.SetUsed ISpecialFormatter.SetUsed).
    procedure SetUsed(const Value: boolean);
  published
    // @name is used to specify whether a special format is used for a
    // particular cells
    // @seealso(ISpecialFormatter.Format ISpecialFormatter.Format).
    property Format: TRbwColumnFormat4 read GetFormat write SetFormat;
    // @name is used to specify whether a special format is used for a
    // particular cells
    // @seealso(ISpecialFormatter.Used ISpecialFormatter.Used).
    property Used: boolean read GetUsed write SetUsed;
  End;
  {$TYPEINFO OFF}

  {@name is the type of @link(TCustomRBWDataGrid.OnStateChange
  TCustomRBWDataGrid.OnStateChange).
  It occurs the TCheckBoxState of check box changes.
    @param(Sender = the @link(TCustomRBWDataGrid).)
    @param(ACol = the column that is being drawn.)
    @param(ARow = the row that is being drawn.)
    @param(Value = the new TCheckBoxState of the checkbox.)
  }
  TChangeCheckEvent = procedure (Sender: TObject; ACol, ARow: Longint;
    const Value: TCheckBoxState) of object;

  TOnIsCaptionEvent = procedure (Sender: TObject; ACol, ARow: Longint;
    var Value: Boolean) of object;


  {@name is the type of @link(TCustomRBWDataGrid.OnBeforeDrawCell
  TCustomRBWDataGrid.OnBeforeDrawCell).
  A @name is called just before a cell specified by ACol and ARow is drawn.
    @param(Sender = the @link(TCustomRBWDataGrid).)
    @param(ACol = the column that is being drawn.)
    @param(ARow = the row that is being drawn.)
  }
  TBeforeDrawCellEvent = procedure (Sender: TObject;
    ACol, ARow: Longint) of object;

  TDistributeTextProgressEvent = procedure (Sender: TObject;
    Position, Max: integer) of object;

  TRbwDataGrid4 = class;

  {
    @abstract(@name is the type of
    @link(TCustomRBWDataGrid.OnColSize
    TCustomRBWDataGrid.OnColSize))
    @param(Sender = the @link(TCustomRBWDataGrid).)
    @param(ACol = the column whose width has changed.)
    @param(PriorWidth = the old width of ACol.)
  }
  TColSizeEvent = procedure(Sender: TObject; ACol, PriorWidth: Longint)
    of object;

  {
    @abstract(@name is the type of
    @link(TCustomRBWDataGrid.OnRowMoving
    TCustomRBWDataGrid.OnRowMoving)
    and @link(TCustomRBWDataGrid.OnColMoving
    TCustomRBWDataGrid.OnColMoving))

    @param(Sender = the @link(TCustomRBWDataGrid).)
    @param(Origin = the column or row being moved.)
    @param(Destination = the new position of the column or row being moved.)
    @param(CanMove = Set CanMove to false to prevent the move.)
  }
  TCheckMoveEvent = procedure(Sender: TObject; const Origin,
    Destination: Longint; var CanMove: boolean) of object;

  { @abstract(@name is the type of @link(TCustomRBWDataGrid.OnButtonClick
    TCustomRBWDataGrid.OnButtonClick). It occurs when a button
    is clicked.)
    @param(Sender = the @link(TCustomRBWDataGrid).)
    @param(ACol = the column that is being drawn.)
    @param(ARow = the row that is being drawn.)
  }
  TGridButtonEvent = procedure (Sender: TObject; ACol, ARow: Longint) of object;

  {
    @name has a series of TGridRect's that indicate which cells are
    selected.
  }
  TRangeSelections = class(TObject)
  {$IFDEF Delphi_2009_UP}
  strict private
  {$ELSE}
  private
  {$ENDIF}
    // @name is an array of TGridRect's that indicate which cells are
    // selected.
    // See @link(Ranges).
    FRanges: array of TGridRect;
    // See @link(Count).
    FCount: integer;
  private
    // See @link(Ranges).
    function GetRange(const Index: integer): TGridRect;
    // @name increases the size of @link(FRanges).
    procedure Grow;
  public
    // @name is the number of TGridRect's in @link(Ranges).
    property Count: integer read FCount;
    // @name is used to read the TGridRect's in @classname.
    property Ranges[const Index: integer]: TGridRect read GetRange; default;
    // @name adds a new TGridRect to @link(Ranges).
    procedure Add(const Range: TGridRect);
    // @name removes all the TGridRect's in @link(Ranges).
    procedure Clear;
  end;

  TCustomRBWDataGrid = class;

  TCheckStyle = (csCheck, csRadio);

  // @name is the abstract ancestor of @link(TRbwColumn4) and
  // @link(TRbwRow).
  TCustomRowOrColumn = class (TCollectionItem)
  private
    // See @link(AutoAdjustRowHeights).
    FAutoAdjustRowHeights: boolean;
    // See @link(ButtonCaption).
    FButtonCaption: string;
    // See @link(ButtonFont).
    FButtonFont: TFont;
    // See @link(ButtonUsed).
    FButtonUsed: boolean;
    // See @link(ButtonWidth).
    FButtonWidth: integer;
    // See @link(CaptionAlignment).
    FCaptionAlignment: TAlignment;
    // See @link(CellAlignment).
    FCellAlignment: TAlignment;
    // See @link(CheckMax).
    FCheckMax: boolean;
    // See @link(CheckMin).
    FCheckMin: boolean;
    // See @link(Format).
    FFormat: TRbwColumnFormat4;
    // See @link(LimitToList).
    FLimitToList: Boolean;
    // See @link(Max).
    FMax: extended;
    // See @link(MaxLength).
    FMaxLength: Integer;
    // See @link(Min).
    FMin: extended;
    // See @link(ParentButtonFont).
    FParentButtonFont: boolean;
    // See @link(PickList).
    FPickList: TStrings;
    // See @link(WordWrapCaptions).
    FWordWrapCaptions: boolean;
    // See @link(WordWrapCells).
    FWordWrapCells: boolean;
    // See @link(ComboUsed).
    FComboUsed: boolean;
    FCheckStyle: TCheckStyle;
    FAutoAdjustCaptionRowHeights: boolean;
    // @name checks that the contents of a cell specified by
    // ACol and ARow match the constraints on the cell
    // as specified in @link(Format), @link(CheckMax),
    // @link(CheckMin), @link(Max), @link(Min), and @link(MaxLength).
    Procedure CheckCell(const ACol, ARow : integer);
    // @name returns either @link(Grid).Font or @link(FButtonFont) as
    // appropriate.
    // See @link(ButtonFont).
    function GetButtonFont: TFont;
    // See @link(AutoAdjustRowHeights).
    procedure SetAutoAdjustRowHeights(const Value: boolean);
    // See @link(ButtonCaption).
    procedure SetButtonCaption(const Value: string);
    // See @link(ButtonFont).
    procedure SetButtonFont(const Value: TFont);
    // See @link(ButtonUsed).
    procedure SetButtonUsed(const Value: boolean);
    // See @link(ButtonWidth).
    procedure SetButtonWidth(const Value: integer);
    // See @link(CaptionAlignment).
    procedure SetCaptionAlignment(const Value: TAlignment);
    // See @link(CellAlignment).
    procedure SetCellAlignment(const Value: TAlignment);
    // See @link(CheckMax).
    procedure SetCheckMax(const Value: boolean);
    // See @link(CheckMin).
    procedure SetCheckMin(const Value: boolean);
    // See @link(ComboUsed).
    procedure SetComboUsed(const Value: boolean);
    // See @link(Format).
    procedure SetFormat(const Value: TRbwColumnFormat4);
    // See @link(Max).
    procedure SetMax(const Value: extended);
    // See @link(Min).
    procedure SetMin(const Value: extended);
    // See @link(ParentButtonFont).
    procedure SetParentButtonFont(const Value: boolean);
    // See @link(PickList).
    procedure SetPickList(const Value: TStrings);
    // See @link(WordWrapCaptions).
    procedure SetWordWrapCaptions(const Value: boolean);
    // See @link(WordWrapCells).
    procedure SetWordWrapCells(const Value: boolean);
    function GetCaseSensitivePicklist: boolean;
    procedure SetCaseSensitivePicklist(const Value: boolean);
    procedure SetCheckStyle(const Value: TCheckStyle);
    procedure InvalidateCachedWidth(Sender: TObject);
    procedure SetAutoAdjustCaptionRowHeights(const Value: boolean);
  protected
    // @name checks that all the cells in the @classname have values
    // that are between @link(Max) and @link(Min)
    procedure CheckRange;virtual;abstract;
    // @name is the position of @name in its TCollection.
    function CollectionItemPosition: integer;virtual;abstract;
    // See @link(Grid).
    function GetGrid: TCustomRbwDataGrid;virtual;abstract;
    // @name returns the selected row or column of @link(Grid).
    function SelectedRowOrColumn: integer;virtual;abstract;
    // @name sets the format of the @classname).
    procedure SetGridRowOrColumnCount(const Value: integer); virtual; abstract;
  public
    procedure CheckACell(const ACol, ARow: integer; LocalCheckMax,
      LocalCheckMin: Boolean; LocalMax, LocalMin: extended);
    // @name creates an instance of @classname.
    constructor Create(Collection: TCollection); override;
    // @name destroys @classname. Do not call @name. Call Free instead.
    destructor Destroy; override;
    // if Source is a @classname, @name copies it to the current instance
    // of @classname.
    procedure Assign(Source: TPersistent); override;
    // @name is the @link(TCustomRbwDataGrid) that owns @classname.
    property  Grid: TCustomRbwDataGrid read GetGrid;
  published
    // @name specifies whether the height of the row should be adjusted for
    // every cell.
    property AutoAdjustRowHeights: boolean read FAutoAdjustRowHeights write
       SetAutoAdjustRowHeights;
    // @name specifies whether the height of the row should be adjusted for
    // caption cells.
    property AutoAdjustCaptionRowHeights: boolean read FAutoAdjustCaptionRowHeights write
       SetAutoAdjustCaptionRowHeights;
    // @name is the caption displayed on the button.
    Property ButtonCaption: string read FButtonCaption write SetButtonCaption;
    // @name is the font on the button.
    property ButtonFont: TFont read GetButtonFont write SetButtonFont;
    // @name specifies whether or not a button is used in the current
    // column or row.
    Property ButtonUsed: boolean read FButtonUsed write SetButtonUsed;
    // @name ButtonWidth is the width of the button.
    Property ButtonWidth: integer read FButtonWidth write SetButtonWidth;
    // In the cells that are captions of a column or row, @name specifies
    // the alignment of the text in those cells.
    property CaptionAlignment: TAlignment read FCaptionAlignment
      write SetCaptionAlignment default taCenter;
    // In the cells that are NOT captions of a column or row, @name specifies
    // the alignment of the text in those cells.
    property CellAlignment: TAlignment read FCellAlignment
      write SetCellAlignment default taLeftJustify;
    // @name specifies whether the maximum value that can be entered in a cell
    // is @link(Max).
    // @name is only used if @link(Format) is rcf4Integer or rcf4Real.
    property CheckMax : boolean read FCheckMax write SetCheckMax;
    // @name specifies whether the minimum value that can be entered in a cell
    // is @link(Min).
    // @name is only used if @link(Format) is rcf4Integer or rcf4Real.
    property CheckMin : boolean read FCheckMin write SetCheckMin;
    // @name specifies whether or not a combobox is used in the current
    // column or row.
    property ComboUsed: boolean read FComboUsed write SetComboUsed;
    // @name is the format of a column or row.
    property Format: TRbwColumnFormat4 read FFormat write SetFormat;
    // @name specifies whether or not a user can type a value in a
    // combobox that is not in @link(PickList).
    // @name is only used if @link(ComboUsed) is @true.
    property LimitToList: Boolean read FLimitToList write FLimitToList;
    // @name is the maximum value that can be entered in a cell.
    // @name is only used if @link(Format) is rcf4Integer or rcf4Real
    // and @link(CheckMax) is @true.
    Property Max : extended read FMax write SetMax;
    // @name is the maximum length of a string that can be entered in
    // a cell;
    // @name is only used if @link(Format) is rcf4String.
    property MaxLength: Integer read FMaxLength write FMaxLength;
    // @name is the minimum value that can be entered in a cell.
    // @name is only used if @link(Format) is rcf4Integer or rcf4Real
    // and @link(CheckMin) is @true.
    Property Min : extended read Fmin write SetMin;
    // if @name is @true, the button will always have the same font
    // as Grid.  If @name is @false the button can have a font that is
    // different from the grid.
    property ParentButtonFont: boolean read FParentButtonFont
      write SetParentButtonFont default True;
    // @name contains the list of items in the combobox.
    property PickList: TStrings read FPickList write SetPickList;
    // @exclude UseButton is for backwards compatibility.
    property UseButton: boolean read FButtonUsed write SetButtonUsed
      stored False;
    // @name determines whether or not word-wrapping will be applied to
    // the text of cells that are captions.
    property WordWrapCaptions: boolean read FWordWrapCaptions
      write SetWordWrapCaptions;
    // @name determines whether or not word-wrapping will be applied to
    // the text of cells that are NOT captions.
    property WordWrapCells: boolean read FWordWrapCells write SetWordWrapCells;
    property CaseSensitivePicklist: boolean read GetCaseSensitivePicklist
      write SetCaseSensitivePicklist;
    property CheckStyle: TCheckStyle read FCheckStyle write SetCheckStyle;
  end;

  TRbwColumn4 = class(TCustomRowOrColumn)
  private
    FAutoAdjustColWidths: boolean;
    procedure SetAutoAdjustColWidths(const Value: boolean);
  protected
    function CollectionItemPosition: integer;override;
    function GetGrid: TCustomRbwDataGrid;override;
    function SelectedRowOrColumn: integer;override;
    procedure CheckRange;override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure SetGridRowOrColumnCount(const Value: integer); override;
  published
    property AutoAdjustColWidths: boolean read FAutoAdjustColWidths
      write SetAutoAdjustColWidths;
  end;

  TRbwColumnClass4 = class of TRbwColumn4;

  TRbwDataGridColumns4 = class(TCollection)
  private
    FGrid: TRbwDataGrid4;
    function GetItems(Index: Integer): TRbwColumn4;
    procedure SetItems(Index: Integer; const Value: TRbwColumn4);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(Grid: TRbwDataGrid4; ColumnClass: TRbwColumnClass4);
    function  Add: TRbwColumn4;
    property Grid: TRbwDataGrid4 read FGrid;
    property Items[Index: Integer]: TRbwColumn4 read GetItems
      write SetItems; default;
  end;

  {
  @abstract(@name defines how the behavior of a column in a
  @link(TRBWRowDataGrid).)
  }
  TRbwRow = class(TCustomRowOrColumn)
  protected
    // See TCustomRowOrColumn.@link(TCustomRowOrColumn.CheckRange).
    procedure CheckRange; override;
    // See TCustomRowOrColumn.@link(TCustomRowOrColumn.GetGrid).
    function GetGrid: TCustomRBWDataGrid; override;
    // See TCustomRowOrColumn.@link(TCustomRowOrColumn.SetGridRowOrColumnCount).
    procedure SetGridRowOrColumnCount(const Value: integer); override;
    // See TCustomRowOrColumn.@link(TCustomRowOrColumn.SelectedRowOrColumn).
    function SelectedRowOrColumn: integer; override;
    function CollectionItemPosition: integer;override;
  end;

  // TRbwRowClass = class of TRbwRow;
  TRbwRowClass = class of TRbwRow;

  TRbwRowDataGrid = class;

  // @abstract(@name is a collection of all the rows in
  // a @link(TRBWRowDataGrid).)
  TRbwDataGridRows = class(TCollection)
  private
    // @name: @link(TCustomRBWDataGrid);
    //
    // See @link(Grid)
    FGrid: TCustomRBWDataGrid;
    // See @link(Items)
    function GetItems(Index: Integer): TRbwRow;
    // See @link(Items)
    procedure SetItems(Index: Integer; const Value: TRbwRow);
  protected
    // GetOwner returns the @link(TRBWRowDataGrid) that owns the
    // TRbwDataGridRows.
    function GetOwner: TPersistent; override;
    // Update invalidates all or part of the grid.
    procedure Update(Item: TCollectionItem); override;
  public
    // @name create a new row.
    function Add: TRbwRow;
    // Call inherited Create and assigns the @link(TRbwRowDataGrid).
    constructor Create(Grid: TRbwRowDataGrid; RowClass: TRbwRowClass);
    // @name is the @link(TCustomRBWDataGrid) that owns the collection.
    property Grid: TCustomRBWDataGrid read FGrid;
    // @name is used to access individual rows.
    property Items[Index: Integer]: TRbwRow read GetItems
      write SetItems; default;
  end;

  TCustomRBWDataGrid = class(TStringGrid)
  private
    FDeleting: Boolean;
    FDeletingRow: boolean;
    FAnchor: TGridCoord;
    FOtherPoint: TGridCoord;
    FAutoDistributeText: boolean;
    FChecked: array of array of TCheckBoxState;
    FColWidths: TIntegerDynArray;
    FComboColumn: integer;
    FComboRow: integer;
    FDrawing : boolean;
    fMouseIsDown : boolean;
    FOnButtonClick: TGridButtonEvent;
    FOnColMoving: TCheckMoveEvent;
    FOnColSize: TColSizeEvent;
    FOnRowMoving: TCheckMoveEvent;
    FOnStateChange: TChangeCheckEvent;
    FOptions: TGridOptions;
    FPriorOnFontChange: TNotifyEvent;
    FRangeSelections: TRangeSelections;
    FSelectedRowOrColumnColor: TColor;
    FUnselectableColor: TColor;
    FUpdating : boolean;
    FRangeSelectionColor: TColor;
    FColorRangeSelection: boolean;
    FRangeSelectionFontColor: TColor;
    FOnBeforeDrawCell: TBeforeDrawCellEvent;
    FAutoIncreaseRowCount: boolean;
    FOnVScroll: TNotifyEvent;
    FOnHScroll: TNotifyEvent;
    // @name is used to define a format for a particular cell that differs
    // from the one that would ordinarily be used for that cell.
    FSpecialFormat: array of array of ISpecialFormatter;
    FOnDistributeTextProgress: TDistributeTextProgressEvent;
    FAutoIncreaseColCount: boolean;
    FDistributingText: Boolean;
    FAutoMultiEdit: boolean;
    FExtendedAutoDistributeText: boolean;
    FDecimalSeparator: Char;
    FAppEvents: TApplicationEvents;
    FUpdateCount: integer;
    FOnIsCaptionEvent: TOnIsCaptionEvent;
    function CollectionItem(const ACol, ARow: Longint):
      TCustomRowOrColumn; virtual; abstract;
    function GetCellVisible(ACol, ARow: Integer): boolean;
    function GetChecked(const ACol, ARow: integer): boolean;
    function GetCheckState(const ACol, ARow: integer): TCheckBoxState;
    function GetColCount: Longint;
    function GetRowCount: Longint;
    function LocalizeString(ANumberString : string) : string;
    function LocalStrToFloat(S: string): Extended;
    function RequiredCellHeight(const ACol, ARow: integer): integer;
    function TextRect(const ACol, ARow: integer): TRect;
    procedure AdjustRowHeights(const ARow: integer);virtual; abstract;
    procedure ButtonClick(Sender: TObject);
    procedure FillCaptionList(CellCaption: string;
      const CaptionList: TStringList; Width: integer);
    procedure MoveCheckStateWithColumn(FromColIndex, ToColIndex: Integer);
    procedure MoveCheckStateWithRow(FromRowIndex, ToRowIndex: Integer);
    procedure SetChecked(const ACol, ARow: integer; const Value: boolean);
    procedure SetColCount(const Value: Longint);
    procedure SetdgColumn(const Value: integer);
    procedure SetdgRow(const Value: integer);
    procedure SetOptions(const Value: TGridOptions);
    procedure SetRowCount(const Value: Longint);
    procedure SetSelectedRowOrColumnColor(const Value: TColor);
    procedure SetUnselectableColor(const Value: TColor);
    function GetSelection: TGridRect;
    procedure SetSelection(const Value: TGridRect);
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure SetColumn(const Value: integer);
    procedure SetRow(const Value: integer);
    function GetRangeSelection: TGridRect;
    procedure SetColorRangeSelection(const Value: boolean);
    procedure SetRangeSelectionColor(const Value: TColor);
    procedure SetRangeSelectionFontColor(const Value: TColor);
    function GetSpecialFormat(ACol, ARow: Integer): TRbwColumnFormat4;
    function GetUseSpecialFormat(ACol, ARow: Integer): boolean;
    procedure ResizeSpecialFormat;
    procedure AssignSpecialFormat(ACol, ARow: Integer);
    procedure SetSpecialFormat(ACol, ARow: Integer;
      const Value: TRbwColumnFormat4);
    procedure SetUseSpecialFormat(ACol, ARow: Integer; const Value: boolean);
    function GetCellFormat(const ACol, ARow: Integer): TRbwColumnFormat4;
    procedure GetPickListItems(ACol, ARow: Integer; Items: TStrings);
    procedure GetButtonCaption(Sender: TObject; var ButtonCaption: string);
    procedure GetButtonWidth(Sender: TObject; var ButtonWidth: Integer);
    function GetColVisible(ACol: Integer): boolean;
    function GetItemIndex(const ACol, ARow: integer): integer;
    procedure SetItemIndex(const ACol, ARow, Value: integer);
    procedure SetExtendedAutoDistributeText(const Value: boolean);
    procedure SetAutoIncreaseColCount(const Value: boolean);
    function GetRealValue(const ACol, ARow: integer): double;
    procedure SetRealValue(const ACol, ARow: integer; const Value: double);
    function GetIntegerValue(const ACol, ARow: integer): integer;
    procedure SetIntegerValue(const ACol, ARow: integer; const Value: integer);
    function GetRealValueDefault(const ACol, ARow: integer;
      DefaultValue: double = 0): double;
    function GetIntegerValueDefault(const ACol, ARow: integer;
      DefaultValue: Integer = 0): integer;
    procedure InvalidateCachedWidth; virtual;
    function GetIsUpdating: boolean;
  protected
    FColorSelectedColumnOrRow: boolean;
    FdgColumn: integer;
    FdgRow: integer;
    FFixedCols : integer;
    FSelectedRow: integer;
    function CanEditShow: Boolean; override;
    function CheckColumnDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; override;
    function CheckRowDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; override;
    function ColorSelectedRowOrColumn(ACol, ARow: integer)
      : boolean; virtual; abstract;
    function ColumnOrRow: integer; virtual; abstract;
    function CreateEditor: TInplaceEdit; override;
    function GetEditStyle(ACol, ARow: Longint): TEditStyle; override;
    procedure FontChanged(Sender: TObject);
    function GetCaptionFlags(const ACol, ARow: integer) : UINT; virtual;
    function GetCellFlags(const ACol, ARow: integer): UINT; virtual;
    function GetCells(ACol, ARow: Integer): string;
    function GetFixedCols : integer;
    function IsCaptionCell(ACol, ARow: integer): boolean; virtual; abstract;
    function SelectCell(ACol, ARow: Longint): Boolean;override;
    function ShouldAdjustColWidths(ACol: integer): boolean; virtual; abstract;
    procedure AdjustColWidths(const ACol: integer);virtual; abstract;
    procedure ColWidthsChanged;override;
    procedure DoExit;override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState);override;
    procedure DrawCheckBoxCell(ACol, ARow: Integer; ARect: TRect;
      AState: TGridDrawState);
    procedure DrawCaptionCell(ACol, ARow: Integer;
      ARect: TRect; AState: TGridDrawState); virtual;
    procedure DrawOrdinaryCell(ACol, ARow: Integer;
      ARect: TRect; AState: TGridDrawState);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Resize; override;
    procedure SetCells(ACol, ARow: Integer; const Value: string); virtual;
    procedure SetColorSelectedColumnOrRow(const Value: boolean);
    procedure SetEditText(ACol, ARow: Longint; const Value: string);override;
    procedure SetFixedCols(const Value : integer);
    property dgColumn : integer read FdgColumn write SetdgColumn;
    property dgRow : integer read FdgRow write SetdgRow;

    function RequiredCellWidth(const ACol, ARow: integer): integer;
    procedure SetEnabled(Value: boolean);override;
    procedure TopLeftChanged(); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function GetCheckStyle(const ACol, ARow: integer): TCheckStyle; virtual; abstract;
    procedure SetCheckState(const ACol, ARow: integer;
      const Value: TCheckBoxState); virtual;
    function PickListRequiredWidth(const ACol, ARow: integer): integer; virtual;
  public
    function WidthNeededToFitText(const ACol, ARow: Integer): integer;
    procedure SelectAll;
    procedure ClearSelection;
    procedure CopyAllCellsToClipboard(IncludeCaptions: Boolean = True);
    procedure CopySelectedCellsToClipboard;
    procedure UpdateEditor;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function DistributeText(const ACol, ARow: integer;
      CellContents: string): boolean;
    property DistributingText: boolean read FDistributingText;
    // @name returns the @link(TCustomRowOrColumn) for a cell.
    // Use @name to insert a column at position ACol.
    procedure InsertColumn(ACol: Integer); virtual;
    // Use @name to insert a row at position ARow.
    procedure InsertRow(ARow: Integer); virtual;
    property Cells[ACol, ARow: Integer]: string read GetCells write SetCells;
    // @name is used to determine whether the cell
    // designated by ACol and ARow is visible.
    property CellVisible[ACol, ARow: Integer]: boolean read GetCellVisible;
    property ColVisible[ACol: Integer]: boolean read GetColVisible;
    property Checked[const ACol, ARow: integer]: boolean read GetChecked
      write SetChecked;
    property CheckState[const ACol, ARow: integer]: TCheckBoxState
      read GetCheckState write SetCheckState;
    property Drawing: boolean read FDrawing;
    // same as CheckState
    property State[const ACol, ARow: integer]: TCheckBoxState read GetCheckState
      write SetCheckState;

    property Selection: TGridRect read GetSelection write SetSelection;
    property RangeSelection: TGridRect read GetRangeSelection;

    property Column: integer read FdgColumn write SetColumn;

    property Row: integer read FdgRow write SetRow;
    function IsSelectedCell(ACol, ARow: Integer): boolean;
    property UseSpecialFormat[ACol, ARow: Integer]: boolean
      read GetUseSpecialFormat write SetUseSpecialFormat;
    property SpecialFormat[ACol, ARow: Integer]: TRbwColumnFormat4
      read GetSpecialFormat write SetSpecialFormat;
    procedure AddRangeSelection(Selection: TGridRect);
    property ItemIndex[const ACol, ARow: integer]: integer read GetItemIndex
      write SetItemIndex;
    procedure SettingsChanged(Sender: TObject; Flag: Integer; const Section: string; var Result: Longint);
    property CheckStyle[const ACol, ARow: integer]: TCheckStyle
      read GetCheckStyle;
    property RealValue[const ACol, ARow: integer]: double read GetRealValue
      write SetRealValue;
    property RealValueDefault[const ACol, ARow: integer;
      DefaultValue: double = 0]: double read GetRealValueDefault;
    property IntegerValue[const ACol, ARow: integer]: integer read GetIntegerValue
      write SetIntegerValue;
    property IntegerValueDefault[const ACol, ARow: integer;
      DefaultValue: integer = 0]: integer read GetIntegerValueDefault;
    Property IsUpdating: boolean read GetIsUpdating;
    procedure SetEditorUpdateToEnd;
    procedure HideEditor;
  published
    // When distributing a text to multiple cells (@link(DistributeText),
    // if @name is @true, the procedure will use up multiple
    // lines of data to fill up one row of the grid if there is not enough
    // data to fill up the a single row of the grid.
    property ExtendedAutoDistributeText: boolean
      read FExtendedAutoDistributeText write SetExtendedAutoDistributeText;
    property AutoMultiEdit: boolean read FAutoMultiEdit write FAutoMultiEdit;
    property AutoDistributeText: boolean read FAutoDistributeText
      write FAutoDistributeText;
    property AutoIncreaseColCount: boolean read FAutoIncreaseColCount
      write SetAutoIncreaseColCount;
    property AutoIncreaseRowCount: boolean read FAutoIncreaseRowCount
      write FAutoIncreaseRowCount;
    property ColCount: Longint read GetColCount write SetColCount default 5;
    property FixedCols : integer read GetFixedCols write SetFixedCols;
    // @name reimplements TStringGrid.Options.  A local copy of @name is
    // stored in @link(FOptions).  Inherited @name is sometimes changed.
    property Options: TGridOptions read FOptions write SetOptions
      default [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
      goRangeSelect];
    property RowCount: Longint read GetRowCount write SetRowCount default 5;
    // @name is the color of the selected row (in @link(TRbwDataGrid4)
    // or selected column (in TRbwRowDataGrid).  It is only used if
    // @link(ColorSelectedRowOrColumn) is true.
    property SelectedRowOrColumnColor : TColor read FSelectedRowOrColumnColor
      write SetSelectedRowOrColumnColor;
    property UnselectableColor : TColor read FUnselectableColor
      write SetUnselectableColor;
    property OnBeforeDrawCell: TBeforeDrawCellEvent read FOnBeforeDrawCell
      write FOnBeforeDrawCell;
    Property OnButtonClick: TGridButtonEvent read FOnButtonClick
      write FOnButtonClick;
    // @name is called when a column is moved.
    property OnColMoving: TCheckMoveEvent read FOnColMoving write FOnColMoving;
    property OnColSize: TColSizeEvent read FOnColSize write FOnColSize;
    // @name is called when a row is moved.
    property OnRowMoving: TCheckMoveEvent read FOnRowMoving write FOnRowMoving;
    property OnStateChange: TChangeCheckEvent read FOnStateChange
      write FOnStateChange;
    property ColorRangeSelection: boolean read FColorRangeSelection
      write SetColorRangeSelection;
    property RangeSelectionColor: TColor read FRangeSelectionColor
      write SetRangeSelectionColor default clBlue;
    property RangeSelectionFontColor: TColor read FRangeSelectionFontColor
      write SetRangeSelectionFontColor default clWhite;
    property OnVerticalScroll: TNotifyEvent read FOnVScroll write FOnVScroll;
    property OnHorizontalScroll: TNotifyEvent read FOnHScroll write FOnHScroll;
    property OnDistributeTextProgress: TDistributeTextProgressEvent
      read FOnDistributeTextProgress write FOnDistributeTextProgress;
    property OnIsCaption: TOnIsCaptionEvent read FOnIsCaptionEvent
      write FOnIsCaptionEvent;
  end;

  TRbwDataGrid4 = class(TCustomRBWDataGrid)
  private
    FColumns: TRbwDataGridColumns4;
    FWordWrapColTitles: boolean;
    FOnEndUpdate: TNotifyEvent;
    FWordWrapRowCaptions: Boolean;
    FRequiredWidthCol: Integer;
    FRequiredWidth: integer;
    procedure SetColumns(const Value: TRbwDataGridColumns4);
    procedure SetWordWrapColTitles(const Value: boolean);
    procedure SetWordWrapRowCaptions(const Value: Boolean);

  { Private declarations }
  protected
    function ShouldAdjustColWidths(ACol: integer): boolean; override;
    function CollectionItem(const ACol, ARow: Longint)
      : TCustomRowOrColumn; override;
    function ColorSelectedRowOrColumn(ACol, ARow: integer): boolean; override;
    function ColumnOrRow: integer; override;
    function CreateColumns: TRbwDataGridColumns4; dynamic;
    function IsCaptionCell(ACol, ARow: integer): boolean; override;
    procedure AdjustColWidths(const ACol: integer);override;
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    procedure Loaded; override;
    procedure RowMoved(FromIndex, ToIndex: Longint); override;
    //    procedure SetCells(ACol, ARow: Integer; const Value: string); override;
    procedure SizeChanged(OldColCount, OldRowCount: Longint); override;
    function GetCheckStyle(const ACol, ARow: integer): TCheckStyle; override;
    procedure SetCheckState(const ACol, ARow: integer;
      const Value: TCheckBoxState); override;
    function PickListRequiredWidth(const ACol, ARow: integer): integer; override;
    procedure InvalidateCachedWidth; override;
    function GetCaptionFlags(const ACol, ARow: integer) : UINT; override;
    { Protected declarations }
  public
    procedure AdjustRowHeights(const ARow: integer);override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DeleteColumn(ACol: Longint); override;
    procedure DeleteRow(ARow: Longint); override;
    // Use @name to insert a column at position ACol.
    procedure InsertColumn(ACol: Integer); override;
    // Use @name to insert a row at position ARow.
    procedure InsertRow(ARow: Integer); override;

    Property SelectedRow : integer read FSelectedRow;
    { Public declarations }
  published
    Property ColorSelectedRow : boolean read FColorSelectedColumnOrRow
      write SetColorSelectedColumnOrRow default True;
    property Columns : TRbwDataGridColumns4 read FColumns write SetColumns;
    property OnEndUpdate: TNotifyEvent read FOnEndUpdate write FOnEndUpdate;
    property WordWrapRowCaptions: Boolean read FWordWrapRowCaptions
      write SetWordWrapRowCaptions;
    // @name is only for backwards compatibility.
    property WordWrapColTitles : boolean read FWordWrapColTitles
      write SetWordWrapColTitles Stored False;
    { Published declarations }
  end;

  TAutoAdjustColumn = class(TCollectionItem)
  private
    // See @link(AutoAdjustColWidths).
    FAutoAdjustColWidths: boolean;
    // See @link(AutoAdjustColWidths).
    procedure SetAutoAdjustColWidths(const Value: boolean);
  protected
    // See @link(Grid).
    function GetGrid: TCustomRBWDataGrid;
    // @name sets the number of columns in the grid.
    procedure SetGridCount(const Value: integer);
  public
    // @name creates and instance of TAutoAdjustColumn.
    constructor Create(Collection: TCollection); override;
    // @name destroys the current instance. Do not call @name directly.
    // Call Free instead.
    destructor Destroy; override;
    // If Source is a @link(TAutoAdjustColumn), @name copies the values of the
    // Source.
    procedure Assign(Source: TPersistent); override;
    // @name returns the TCustomRBWDataGrid that owns the
    // @link(TAutoAdjustColumns) that is the parent of the current instance.
    property Grid: TCustomRBWDataGrid read GetGrid;
  published
    // @name is used to specify whether the width of a column should
    // be automatically enlarged to fit all the text in the column.
    property AutoAdjustColWidths: boolean read FAutoAdjustColWidths
      write SetAutoAdjustColWidths;
  end;

  // TAutoAdjustColumnClass = class of TAutoAdjustColumn;
  TAutoAdjustColumnClass = class of TAutoAdjustColumn;

  // @abstract(@name specifies how columns are to adjust their width to fit the
  // text in the column.)
  TAutoAdjustColumns = class(TCollection)
  private
    // @name : @link(TCustomRBWDataGrid);
    //
    // @name is the TCustomRBWDataGrid that owns the TAutoAdjustColumns.
    FGrid: TCustomRBWDataGrid;
    // See @link(Items).
    function GetItems(Index: Integer): TAutoAdjustColumn;
    // See @link(Items).
    procedure SetItems(Index: Integer; const Value: TAutoAdjustColumn);
  protected
    // @name returns the grid that owns the TRbwDataGridColumns.
    function GetOwner: TPersistent; override;
    // @name invalidates the grid or part of it.
    procedure Update(Item: TCollectionItem); override;
  public
    // @name calls inherited Create and assigns the Grid.
    constructor Create(Grid: TCustomRbwDataGrid;
      ColumnClass: TAutoAdjustColumnClass);
    // @name adds a new new @link(TAutoAdjustColumn).
    function Add: TAutoAdjustColumn;
    // @name creates a new @link(TAutoAdjustColumn) at position Index.
    function Insert(Index: Integer): TAutoAdjustColumn;
    // @name is the grid that owns the collection
    property Grid: TCustomRBWDataGrid read FGrid;
    // @name accesses individual @link(TAutoAdjustColumn)s.
    property Items[Index: Integer]: TAutoAdjustColumn read GetItems
      write SetItems; default;
  end;

  // @abstract(@name is a TStringGrid that restricts the data displayed in a
  // cell based on the Format of the @link(TRbwRow) associated with the cell.
  // Cells can also display a combobox or button in a cell).
  TRbwRowDataGrid = class(TCustomRBWDataGrid)
  private
    // See @link(Columns).
    FColumns: TAutoAdjustColumns;
    // See @link(FixedRows).
    FFixedRows: integer;
    // See @link(Rows).
    FRows: TRbwDataGridRows;
//    FUpdateCount: Integer;
    FRequiredWidthRow: Integer;
    FRequiredWidth: Integer;
    // See @link(Columns).
    procedure SetColumns(const Value: TAutoAdjustColumns);
    // See @link(Rows).
    procedure SetRows(const Value: TRbwDataGridRows);
//    function GetUpdating: boolean;
  protected
    function ShouldAdjustColWidths(ACol: integer): boolean; override;
    // @name returns Rows[ARow].
    function CollectionItem(const ACol, ARow: Longint): TCustomRowOrColumn;
      override;
    function ColumnOrRow: integer; override;
    function ColorSelectedRowOrColumn(ACol, ARow: integer): boolean; override;
    // @name creates and returns an instance of TAutoAdjustColumns.
    function CreateColumns: TAutoAdjustColumns; dynamic;
    // @name creates and returns an instance of TRbwDataGridRows.
    function CreateRows: TRbwDataGridRows; dynamic;
    // See @link(FixedRows).
    function GetFixedRows: integer; virtual;
    function IsCaptionCell(ACol, ARow: integer): boolean; override;
    // @name is used to calculate the width of the cell required to fit
    // the text in the cell.
    // If  @link(TAutoAdjustColumn.AutoAdjustColWidths) is true, @name is
    // used to change the width of the columns.
    procedure AdjustColWidths(const ACol: integer); override;
    procedure AdjustRowHeights(const ARow: integer); override;
    // @name adjusts @link(Columns) when the user moves a column.
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    // @name sets the @link(FixedCols) and @link(FixedRows) and calls
    // @link(AdjustColWidths) for each column.
    procedure Loaded; override;
    // @name adjusts @link(Rows) when the user moves a row.
    procedure RowMoved(FromIndex, ToIndex: Longint); override;
    // See @link(FixedRows).
    procedure SetFixedRows(const Value: integer); virtual;
    // @name adds or removes items in @link(Columns) and @link(Rows)
    // if the number of columns or rows changes.
    procedure SizeChanged(OldColCount, OldRowCount: Longint); override;
    function GetCheckStyle(const ACol, ARow: integer): TCheckStyle; override;
    procedure SetCheckState(const ACol, ARow: integer;
      const Value: TCheckBoxState); override;
    function PickListRequiredWidth(const ACol, ARow: integer): integer; override;
    procedure InvalidateCachedWidth; override;
  public
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    // @name initializes grid.
    constructor Create(AOwner: TComponent); override;
    // @name destroys the grid.  Don't call @name directly. Call Free instead.
    destructor Destroy; override;
    procedure DeleteColumn(ACol: Longint); override;
    procedure DeleteRow(ARow: Longint); override;
    // Use @name to insert a column at position ACol.
    procedure InsertColumn(ACol: Integer); override;
    // Use @name to insert a row at position ARow.
    procedure InsertRow(ARow: Integer); override;
//    Property Updating: boolean read GetUpdating;
  published
    Property ColorSelectedColumn : boolean read FColorSelectedColumnOrRow
      write SetColorSelectedColumnOrRow;
    // @name is used to specify if the width of a column should be
    // automatically adjusted to fit its content.
    property Columns: TAutoAdjustColumns read FColumns write SetColumns;
    // @name is the number of fixed rows in the grid.
    property FixedRows: integer read GetFixedRows write SetFixedRows;
    // @name allows properties of individual rows to be specified.
    // See @link(TRbwDataGridRows).
    property Rows: TRbwDataGridRows read FRows write SetRows;
  end;

  TGetButtonCaptionEvent = procedure(Sender: TObject;
    var ButtonCaption: string) of object;
  TGetButtonWidthEvent = procedure(Sender: TObject;
    var ButtonWidth: integer) of object;

  // @name is used to gain access to the protected UpdateContents
  // procedure.
  TRbwInplaceEdit4 = class(TInplaceEditList)
  private
    FPushButtonWidth: integer;
    FPressed: Boolean;
    FTracking: Boolean;
    FOnGetButtonCaption: TGetButtonCaptionEvent;
    FOnGetButtonWidth: TGetButtonWidthEvent;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk);
      message wm_LButtonDblClk;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SetCursor;
    function GetPushButtonWidth: integer;
  protected
    procedure BoundsChanged; override;
    function ButtonRect: TRect;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function OverButton(const P: TPoint): Boolean;
    procedure PaintWindow(DC: HDC); override;
    procedure StopTracking;
    procedure TrackButton(X,Y: Integer);
    function GetButtonCaption(ACol, ARow: integer): string;
  public
    constructor Create(Owner: TComponent); override;
    property PushButtonWidth: integer read GetPushButtonWidth;
    property Pressed: Boolean read FPressed;
    property OnGetButtonCaption: TGetButtonCaptionEvent read FOnGetButtonCaption
      write FOnGetButtonCaption;
    property OnGetButtonWidth: TGetButtonWidthEvent
      read FOnGetButtonWidth write FOnGetButtonWidth;
  end;

procedure Register;

implementation

uses
  Math, Themes, {System.}StrUtils;

resourcestring
  StrThereWasAnErrorA = 'There was an error adjusting the column width.';
  StrAttemptingToRetrie = 'Attempting to retrieve an invalid column';
  StrErrorDeletingRow = 'Error deleting Row >= RowCount.';
  StrErrorDeletingCol = 'Error deleting Col >= ColCount.';

var
  DummyIntValue : integer;

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 23.0}
function SServices: TCustomStyleServices;
begin
  Result := StyleServices;
end;
  {$ELSE}
function SServices: TThemeServices;
begin
  result := ThemeServices;
end;
  {$IFEND}
{$ELSE}
function SServices: TThemeServices;
begin
  result := ThemeServices;
end;
{$ENDIF}


procedure Register;
begin
  RegisterComponents('RBW', [TRbwDataGrid4, TRbwRowDataGrid]);
end;

var
  // @name is a bitmap that represents a checkbox
  // that is checked and not disabled.
  FbmpChecked : TBitMap;
  // @name is a bitmap that represents a checkbox
  // that is unchecked and not disabled.
  FbmpUnchecked : TBitMap;
  // @name is a bitmap that represents a checkbox
  // that is checked and disabled.
  FBmpDisabledChecked: TBitMap;
  // @name is a bitmap that represents a checkbox
  // that is in a grayed or indeterminate state and disabled.
  FBmpDisabledGrayed: TBitMap;
  // @name is a bitmap that represents a checkbox
  // that is unchecked and disabled.
  FBmpDisabledUnchecked: TBitMap;
  // @name is a bitmap that represents a checkbox
  // that is in a grayed or indeterminate state and enabled.
  FBmpGrayed: TBitMap;

  FBmpRadioChecked : TBitMap;
  FBmpRadioUnChecked : TBitMap;
  FBmpRadioGrayed: TBitMap;
  FBmpDisabledRadioChecked : TBitMap;
  FBmpDisabledRadioUnChecked : TBitMap;
  FBmpDisabledRadioGrayed: TBitMap;

{ TRbwColumn4 }

procedure TRbwColumn4.Assign(Source: TPersistent);
begin
  if Source is TRbwColumn4 then
  begin
    if Assigned(Collection) then Collection.BeginUpdate;
    try
      AutoAdjustColWidths := TRbwColumn4(Source).AutoAdjustColWidths;
      inherited;
    finally
      if Assigned(Collection) then Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TRbwColumn4.SetAutoAdjustColWidths(const Value: boolean);
var
  ACol: integer;
  LocalGrid: TCustomRBWDataGrid;
begin
  if FAutoAdjustColWidths <> Value then
  begin
    FAutoAdjustColWidths := Value;
    if Value then
    begin
      ACol := Index;
      LocalGrid := Grid;
      if (LocalGrid <> nil) and (ACol >= 0) and (ACol < LocalGrid.ColCount)
        and ([csLoading, csReading] * LocalGrid.ComponentState = []) then
      begin
        try
          (LocalGrid as TRbwDataGrid4).AdjustColWidths(ACol);
        except on E:Exception do
          begin
            raise EColWidthError.Create(StrThereWasAnErrorA
              + sLineBreak + E.Message);
          end;
        end;
      end;
    end;
  end;
end;

procedure TRbwColumn4.SetGridRowOrColumnCount(const Value: integer);
begin
  Grid.ColCount := Value;
end;

{ TRbwDataGridColumns2 }
function TRbwDataGridColumns4.Add: TRbwColumn4;
begin
  Result := TRbwColumn4(inherited Add);
  if (Grid <> nil) and
    not Grid.FUpdating and not (csLoading in FGrid.ComponentState) then
  begin
    FGrid.FUpdating := True;
    FGrid.ColCount := Count;
    FGrid.FUpdating := False;
  end;
end;

constructor TRbwDataGridColumns4.Create(Grid: TRbwDataGrid4;
  ColumnClass: TRbwColumnClass4);
begin
  inherited Create(ColumnClass);
  FGrid := Grid;
end;

function TRbwDataGridColumns4.GetItems(Index: Integer): TRbwColumn4;
begin
  if (Index < 0) or (Index >= Count) then
  begin
    raise EInvalidColumn.Create(StrAttemptingToRetrie);
  end;
  Result := inherited Items[Index] as TRbwColumn4;
end;

function TRbwDataGridColumns4.GetOwner: TPersistent;
begin
  Result := FGrid;
end;

procedure TRbwDataGridColumns4.SetItems(Index: Integer;
  const Value: TRbwColumn4);
begin
  Items[Index].Assign(Value);
end;

procedure TRbwDataGridColumns4.Update(Item: TCollectionItem);
begin
  inherited;
  if (FGrid = nil) or (csLoading in FGrid.ComponentState) then Exit;

  if (csDesigning in FGrid.ComponentState) then FGrid.invalidate
  else FGrid.invalidatecol(FGrid.dgColumn);
end;

{ TRbwDataGrid2 }

procedure TRbwDataGrid4.AdjustColWidths(const ACol: integer);
var
  Index: integer;
  RequiredWidth, TestWidth: integer;
begin
  if FUpdateCount > 0 then
  begin
    Exit;
  end;
  if ACol >= Columns.Count then
  begin
    Exit;
  end;
  if Columns[ACol].AutoAdjustColWidths then
  begin
    RequiredWidth := 0;
    for Index := 0 to RowCount -1 do
    begin
      TestWidth := RequiredCellWidth(ACol, Index);
      if TestWidth > RequiredWidth then
      begin
        RequiredWidth := TestWidth
      end;
    end;

    if ColWidths[ACol] < RequiredWidth then
    begin
      ColWidths[ACol] := RequiredWidth;
    end;
  end;
end;

function TRbwDataGrid4.CollectionItem(const ACol,
  ARow: Integer): TCustomRowOrColumn;
begin
  if (Columns = nil) then
  begin
    result := nil;
  end
  else if (ACol < 0) or (ACol >= Columns.Count) then
  begin
    result := nil;
  end
  else
  begin
    result := Columns[ACol];
  end;
end;

function TRbwDataGrid4.ColorSelectedRowOrColumn(ACol, ARow: integer): boolean;
begin
  result := ColorSelectedRow and (ARow = dgRow);
end;

procedure TRbwDataGrid4.ColumnMoved(FromIndex, ToIndex: Integer);
begin
  inherited;
  Columns[FromIndex].Index := ToIndex;
  MoveCheckStateWithColumn(FromIndex, ToIndex);
end;

function TRbwDataGrid4.ColumnOrRow: integer;
begin
  result := Column;
end;

constructor TRbwDataGrid4.Create(AOwner: TComponent);
var
  Index : integer;
begin
  inherited;
  FDeletingRow := False;
  FComboColumn := -1;
  FComboRow := -1;


  FColumns := CreateColumns;
  for Index := 0 to ColCount-1 do
  begin
    FColumns.Add;
  end;
  FixedCols := 1;
  Options := Options + [goEditing] -[goDrawFocusSelected];
  UnselectableColor := FixedColor;
  ColorSelectedRow := True;

  FdgColumn := 1;
  FdgRow := 1;
  FSelectedRow := FdgRow;
  FRequiredWidthCol := -1;
end;

function TRbwDataGrid4.CreateColumns: TRbwDataGridColumns4;
begin
  Result := TRbwDataGridColumns4.Create(Self, TRbwColumn4);
end;

procedure TRbwDataGrid4.InsertColumn(ACol: Integer);
var
  ColIndex, RowIndex: integer;
  TempColumns: TRbwDataGridColumns4;
  Index: Integer;
  ColumnAdded: boolean;
begin
  TempColumns := TRbwDataGridColumns4.Create(nil, TRbwColumn4);
  try
    ColumnAdded := false;
    if ACol < 0 then
    begin
      ACol:= 0;
    end;
    for Index := 0 to Columns.Count - 1 do
    begin
      TempColumns.Add.Assign(Columns[Index]);
      if Index = ACol then
      begin
        TempColumns.Add.Assign(Columns[Index]);
        ColumnAdded := True;
      end;
    end;
    if not ColumnAdded then
    begin
      if ColCount = 0 then
      begin
        TempColumns.Add;
      end
      else
      begin
        TempColumns.Add.Assign(Columns[ColCount-1]);
      end;
    end;

    SetLength(FChecked, ColCount+1,RowCount);
    for ColIndex := ColCount downto ACol + 1 do
    begin
      for RowIndex := 0 to RowCount - 1 do
      begin
        FChecked[ColIndex, RowIndex] := FChecked[ColIndex - 1, RowIndex];
      end;
    end;
    for RowIndex := 0 to RowCount - 1 do
    begin
      FChecked[ACol, RowIndex] := cbUnchecked;
    end;
    inherited;
    // Each column needs to be assigned individually because
    // assigning the whole collection causes the collection that
    // is the destination to be cleared. That would affect the
    // CheckState of the cells.
    for Index := 0 to Columns.Count - 1 do
    begin
      Columns[Index].Assign(TempColumns[Index]);
    end;
  finally
    TempColumns.free;
  end;
end;

procedure TRbwDataGrid4.InsertRow(ARow: Integer);
var
  ColIndex, RowIndex: integer;
begin
  RowCount := RowCount + 1;
  for RowIndex := RowCount - 1 downto ARow + 1 do
  begin
    Rows[RowIndex] := Rows[RowIndex - 1];
    RowHeights[RowIndex] := RowHeights[RowIndex - 1];
  end;
  for ColIndex := 0 to ColCount - 1 do
  begin
    Cells[ColIndex, ARow] := '';
    Objects[ColIndex, ARow] := nil;
  end;

  for RowIndex := RowCount - 1 downto ARow + 1 do
  begin
    for ColIndex := 0 to ColCount - 1 do
    begin
      FChecked[ColIndex, RowIndex] := FChecked[ColIndex, RowIndex - 1];
    end;
  end;
  for ColIndex := 0 to ColCount - 1 do
  begin
    Checked[ColIndex, ARow] := False;
  end;
end;

procedure TRbwDataGrid4.InvalidateCachedWidth;
begin
  FRequiredWidthCol := -1;
end;

function TRbwDataGrid4.IsCaptionCell(ACol, ARow: integer): boolean;
begin
  result := (ARow < FixedRows) or (WordWrapRowCaptions and (ACol < FixedCols));
  if Assigned(OnIsCaption) then
  begin
    OnIsCaption(Self, ACol, ARow, result);
  end;
end;

procedure TRbwDataGrid4.Loaded;
begin
  inherited;
  FixedCols := FFixedCols;
  HideEditor;
end;

function TRbwDataGrid4.PickListRequiredWidth(const ACol,
  ARow: integer): integer;
begin
  if FRequiredWidthCol <> ACol then
  begin
    FRequiredWidthCol := ACol;
    FRequiredWidth := inherited PickListRequiredWidth(ACol, ARow);
  end;
  result := FRequiredWidth;
end;

procedure TRbwDataGrid4.SetCheckState(const ACol, ARow: integer;
  const Value: TCheckBoxState);
var
  RowIndex: Integer;
begin
  inherited;
  if (Value = cbChecked) and (CheckStyle[ACol, ARow] = csRadio)  then
  begin
    for RowIndex := 0 to RowCount - 1 do
    begin
      if RowIndex <> ARow then
      begin
        CheckState[ACol, RowIndex] := cbUnchecked
      end;
    end;
  end;
end;

procedure TRbwDataGrid4.SetColumns(const Value: TRbwDataGridColumns4);
var
  TempFixedCols: integer;
begin
  TempFixedCols := FixedCols;
  FColumns.Assign(Value);
  TempFixedCols := Min(TempFixedCols, ColCount-1);
  FixedCols := TempFixedCols;
end;

procedure TRbwDataGrid4.SetWordWrapColTitles(const Value: boolean);
var
  Index: integer;
begin
  FWordWrapColTitles := Value;
  for Index := 0 to Columns.Count -1 do
  begin
    Columns[Index].WordWrapCaptions := Value;
  end;
  Invalidate;
end;

procedure TRbwDataGrid4.SetWordWrapRowCaptions(const Value: Boolean);
begin
  if FWordWrapRowCaptions <> Value then
  begin
    FWordWrapRowCaptions := Value;
    Invalidate;
  end;
end;

function TRbwDataGrid4.ShouldAdjustColWidths(ACol: integer): boolean;
begin
  result := Columns[ACol].AutoAdjustColWidths;
end;

procedure TRbwDataGrid4.SizeChanged(OldColCount, OldRowCount: Integer);
begin
  inherited;
  if not (csLoading in ComponentState)
    and not (csReading in ComponentState)
    and not (csReadingState in ControlState)
    and not (csCreating in ControlState)
    and not FUpdating{and (FLayoutFlag = 0)} then
  begin
    FUpdating := True;
    while Columns.Count > ColCount do Columns[Columns.Count-1].Free;
    while Columns.count < ColCount do Columns.Add;
    FUpdating := False;
  end;
end;

procedure TRbwDataGrid4.DeleteColumn(ACol: Integer);
var
  TempColumns: TRbwDataGridColumns4;
  Index: Integer;
  ColIndex: Integer;
  RowIndex: Integer;
begin
  FDeleting := True;
  try
    if ACol = Col then
    begin
      HideEditor;
    end;
    TempColumns := TRbwDataGridColumns4.Create(nil, TRbwColumn4);
    try
      TempColumns.Assign(Columns);
      TempColumns.Delete(ACol);
      inherited;
      // Each column needs to be assigned individually because
      // assigning the whole collection causes the collection that
      // is the destinatin to be cleared. That would affected the
      // CheckState of the cells.
      for Index := 0 to Columns.Count - 1 do
      begin
        Columns[Index].Assign(TempColumns[Index]);
      end;
      if Col = ACol then
      begin
        if Col > 0 then
        begin
          Col := Col - 1;
        end;
      end;
    finally
      TempColumns.Free;
    end;
    if ACol < Length(FSpecialFormat) then
    begin
      for RowIndex := 0 to RowCount - 1 do
      begin
        if RowIndex < Length(FSpecialFormat[ACol]) then
        begin
          FSpecialFormat[ACol,RowIndex] := nil;
        end;
      end;
    end;
    for ColIndex := +1 to ColCount - 1 do
    begin
      if ColIndex < Length(FSpecialFormat) then
      begin
        for RowIndex := 0 to RowCount - 1 do
        begin
          if RowIndex < Length(FSpecialFormat[ColIndex]) then
          begin
            if Assigned(FSpecialFormat[ColIndex,RowIndex]) then
            begin
              UseSpecialFormat[ColIndex-1,RowIndex] :=
                UseSpecialFormat[ColIndex,RowIndex];
              SpecialFormat[ColIndex-1,RowIndex] :=
                SpecialFormat[ColIndex,RowIndex];
            end;
          end;
        end;
      end;
    end;
    ResizeSpecialFormat;
    Invalidate;
  finally
    FDeleting := False;
  end;
end;

procedure TRbwDataGrid4.DeleteRow(ARow: Integer);
var
  SelectedCell: TGridRect;
  ColIndex: Integer;
  RowIndex: Integer;
  TempRow: integer;
begin
  FDeleting := True;
  try
    if ARow = Row then
    begin
      HideEditor;
    end;
    if ARow >= RowCount then
    begin
      Raise ERangeError.Create(StrErrorDeletingRow);
    end;
    FDeletingRow := True;
    try
      TempRow := Row;
      inherited;
      if Row = ARow then
      begin
        if Row > 0 then
        begin
          SelectedCell := Selection;
          SelectedCell.Top := Row - 1;
          SelectedCell.Bottom := SelectedCell.Top;
          Selection := SelectedCell;
        end
      end;
      for ColIndex := 0 to ColCount - 1 do
      begin
        if (ColIndex < Length(FSpecialFormat))
          and (ARow < Length(FSpecialFormat[ColIndex])) then
        begin
          FSpecialFormat[ColIndex,ARow] := nil;
        end;
      end;
      for ColIndex := 0 to ColCount - 1 do
      begin
        if (ColIndex < Length(FSpecialFormat)) then
        begin
          for RowIndex := ARow+1 to RowCount - 1 do
          begin
            if RowIndex < Length(FSpecialFormat[ColIndex]) then
            begin
              if Assigned(FSpecialFormat[ColIndex,RowIndex]) then
              begin
                UseSpecialFormat[ColIndex,RowIndex-1] :=
                  UseSpecialFormat[ColIndex,RowIndex];
                SpecialFormat[ColIndex,RowIndex-1] :=
                  SpecialFormat[ColIndex,RowIndex];
              end;
            end;
          end;
        end;
      end;
      ResizeSpecialFormat;
      if (TempRow >= FixedRows) and (TempRow < RowCount) then
      begin
        Row := TempRow;
        TStringGrid(Self).Row := TempRow;
      end
      else
      begin
        Row := RowCount-1;
        TStringGrid(Self).Row := RowCount-1;
      end;
      Invalidate;
    finally
      FDeletingRow := False;
    end;
  finally
    FDeleting := False;
  end;
end;

destructor TRbwDataGrid4.Destroy;
begin
  FColumns.Free;
  inherited;
end;

procedure TRbwDataGrid4.EndUpdate;
var
  Index: Integer;
  AdjustRows: boolean;
  LocalSelection: TGridRect;
  PriorVisibleRowCount: Integer;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    repeat
      PriorVisibleRowCount := VisibleRowCount;
      for Index := 0 to ColCount - 1 do
      begin
        AdjustColWidths(Index);
      end;
      AdjustRows := False;
      for Index := 0 to ColCount - 1 do
      begin
        if Columns[Index].AutoAdjustRowHeights then
        begin
          AdjustRows := True;
          break;
        end;
      end;
      if AdjustRows then
      begin
        for Index := 0 to RowCount - 1 do
        begin
          AdjustRowHeights(Index);
        end;
      end;
      if Assigned(FOnEndUpdate) then
      begin
        FOnEndUpdate(self);
      end;
      // move editor to correct location.
      LocalSelection := Selection;
      Selection := LocalSelection;
    until PriorVisibleRowCount = VisibleRowCount;
  end;
end;

function TRbwDataGrid4.GetCaptionFlags(const ACol, ARow: integer): UINT;
begin
  result := inherited GetCaptionFlags(ACol, ARow);
  if WordWrapRowCaptions and IsCaptionCell(ACol, ARow) then
  begin
    result := result or DT_WORDBREAK;
  end;
end;

function TRbwDataGrid4.GetCheckStyle(const ACol, ARow: integer): TCheckStyle;
begin
  result := Columns[ACol].CheckStyle;
end;

procedure TRbwDataGrid4.RowMoved(FromIndex, ToIndex: Integer);
begin
  inherited;
  MoveCheckStateWithRow(FromIndex, ToIndex);
end;

procedure TCustomRBWDataGrid.AddRangeSelection(Selection: TGridRect);
begin
  FRangeSelections.Add(Selection);
  self.HideEditor;
  Invalidate;
end;

procedure TCustomRBWDataGrid.ClearSelection;
begin
  FRangeSelections.Clear;
  Invalidate;
end;

procedure TCustomRBWDataGrid.CopyAllCellsToClipboard(IncludeCaptions: Boolean = True);
var
  CopiedText: TStringList;
  RowIndex: Integer;
  Line: string;
  ColIndex: Integer;
  RowStart: Integer;
  ColumnStart: Integer;
begin
  CopiedText := TStringList.Create;
  try
    if IncludeCaptions then
    begin
      RowStart := 0;
      ColumnStart := 0;
    end
    else
    begin
      RowStart := FixedRows;
      ColumnStart := FixedCols;
    end;
    for RowIndex := RowStart to RowCount-1 do
    begin
      Line := '';
      for ColIndex := ColumnStart to ColCount-1 do
      begin
        if (GetCellFormat(ColIndex, RowIndex) = rcf4Boolean)
          and (ColIndex >= FixedCols) and (RowIndex >= FixedRows) then
        begin
          case State[ColIndex, RowIndex] of
            cbUnchecked:
              begin
                Line := Line + ''#9'' + 'False';
              end;
            cbChecked:
              begin
                Line := Line + ''#9'' + 'True';
              end;
            cbGrayed:
              begin
                Line := Line + ''#9'' + 'Undefined';
              end;
          else
            Assert(False);
          end;
        end
        else
        begin
          Line := Line + ''#9'' + Cells[ColIndex, RowIndex];
        end;
      end;
      Line := Copy(Line, 2, MAXINT);
      CopiedText.Add(Line);
    end;
    ClipBoard.AsText := CopiedText.Text;
  finally
    CopiedText.Free;
  end
end;

procedure TCustomRBWDataGrid.CopySelectedCellsToClipboard;
var
  CopiedText: TStringList;
  ColIndex: Integer;
  RowIndex: Integer;
  Right: Integer;
  Left: Integer;
  Bottom: Integer;
  Top: Integer;
  Line: string;
begin
  Top := -1;
  Bottom := -1;
  Left := -1;
  Right := -1;
  for RowIndex := FixedRows to RowCount - 1 do
  begin
    for ColIndex := FixedCols to ColCount - 1 do
    begin
      if IsSelectedCell(ColIndex, RowIndex) then
      begin
        if Top < 0 then
        begin
          Top := RowIndex;
          Bottom := RowIndex;
          Left := ColIndex;
          Right := ColIndex;
        end
        else
        begin
          if Top > RowIndex then
          begin
            Top := RowIndex;
          end;
          if Bottom < RowIndex then
          begin
            Bottom := RowIndex;
          end;
          if Left > ColIndex then
          begin
            Left := ColIndex;
          end;
          if Right < ColIndex then
          begin
            Right := ColIndex;
          end;
        end;
      end;
    end;
  end;
  if Top >= 0 then
  begin
    CopiedText := TStringList.Create;
    try
      for RowIndex := Top to Bottom do
      begin
        Line := '';
        for ColIndex := Left to Right do
        begin
          if IsSelectedCell(ColIndex, RowIndex) then
          begin
            if GetCellFormat(ColIndex, RowIndex) = rcf4Boolean then
            begin
              case State[ColIndex, RowIndex] of
                cbUnchecked:
                  begin
                    Line := Line + ''#9'' + 'False';
                  end;
                cbChecked:
                  begin
                    Line := Line + ''#9'' + 'True';
                  end;
                cbGrayed:
                  begin
                    Line := Line + ''#9'' + 'Undefined';
                  end;
              else
                Assert(False);
              end;
            end
            else
            begin
              Line := Line + ''#9'' + Cells[ColIndex, RowIndex];
            end;
          end
          else
          begin
            Line := Line + ''#9'';
          end;
        end;
        Line := Copy(Line, 2, MAXINT);
        CopiedText.Add(Line);
      end;
      ClipBoard.AsText := CopiedText.Text;
    finally
      CopiedText.Free;
    end;
  end;
end;

procedure TCustomRBWDataGrid.AssignSpecialFormat(ACol, ARow: Integer);
begin
  ResizeSpecialFormat;
  if not Assigned(FSpecialFormat[ACol, ARow]) then
  begin
    FSpecialFormat[ACol, ARow] := TSpecialFormatter.Create;
  end;
end;

procedure TCustomRBWDataGrid.BeginUpdate;
begin

end;

procedure TCustomRBWDataGrid.ButtonClick(Sender: TObject);
begin
  if Assigned(FOnButtonClick) then
  begin
    FOnButtonClick(self, dgColumn, dgRow);
  end;
end;

procedure TRbwDataGrid4.AdjustRowHeights(const ARow: integer);
var
  RequiredHeight, TestHeight: integer;
  ColIndex: integer;
  AColumn: TRbwColumn4;
begin
  if FUpdateCount > 0 then
  begin
    Exit;
  end;
  RequiredHeight := 0;
  for ColIndex := 0 to ColCount -1 do
  begin
    if ColIndex >= Columns.Count then
    begin
      break;
    end;

    AColumn := Columns[ColIndex];
    if AColumn.AutoAdjustRowHeights or
      (AColumn.AutoAdjustCaptionRowHeights and IsCaptionCell(ColIndex, ARow)) then
    begin
      Canvas.Font.Assign(Font);
      TestHeight := RequiredCellHeight(ColIndex, ARow);
      if TestHeight > RequiredHeight then
      begin
        RequiredHeight := TestHeight
      end;

    end;
  end;
  if RowHeights[ARow] < RequiredHeight then
  begin
    RowHeights[ARow] := RequiredHeight;
  end;
end;

procedure TRbwDataGrid4.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCustomRowOrColumn.SetFormat(const Value: TRbwColumnFormat4);
begin
  Assert(Value in [rcf4String, rcf4Integer, rcf4Real, rcf4Boolean]);
  if FFormat <> Value then
  begin
    FFormat := Value;
//    if FFormat = rcf4Boolean then
//    begin
//      ComboUsed := False;
//    end;
    Changed(False);
  end;
end;

procedure TCustomRowOrColumn.SetCheckMin(const Value: boolean);
begin
  if (FCheckMin <> Value) then FCheckMin := Value;
  if FCheckMin then CheckRange;
end;

procedure TCustomRowOrColumn.SetCheckStyle(const Value: TCheckStyle);
begin
  FCheckStyle := Value;
  if (Grid <> nil) then
  begin
    Grid.Invalidate;
  end;
end;

procedure TCustomRowOrColumn.SetComboUsed(const Value: boolean);
begin
  if FComboUsed <> Value then
  begin
    FComboUsed := Value;
    if FComboUsed then
    begin
      ButtonUsed := False;
    end;
    InvalidateCachedWidth(nil);
  end;
end;

procedure TCustomRowOrColumn.Assign(Source: TPersistent);
begin
  if Source is TCustomRowOrColumn then
  begin
    if Assigned(Collection) then Collection.BeginUpdate;
    try
      AutoAdjustRowHeights := TCustomRowOrColumn(Source).AutoAdjustRowHeights;
      ButtonCaption := TCustomRowOrColumn(Source).ButtonCaption;
      ButtonFont := TCustomRowOrColumn(Source).ButtonFont;
      ButtonUsed := TCustomRowOrColumn(Source).ButtonUsed;
      ButtonWidth := TCustomRowOrColumn(Source).ButtonWidth;
      CaptionAlignment := TCustomRowOrColumn(Source).CaptionAlignment;
      CellAlignment := TCustomRowOrColumn(Source).CellAlignment;
      CheckMax := TCustomRowOrColumn(Source).CheckMax;
      CheckMin := TCustomRowOrColumn(Source).CheckMin;
      ComboUsed := TCustomRowOrColumn(Source).ComboUsed;
      Format := TCustomRowOrColumn(Source).Format;
      LimitToList := TCustomRowOrColumn(Source).LimitToList;
      Max := TCustomRowOrColumn(Source).Max;
      MaxLength := TCustomRowOrColumn(Source).MaxLength;
      Min := TCustomRowOrColumn(Source).Min;
      ParentButtonFont := TCustomRowOrColumn(Source).ParentButtonFont;
      PickList := TCustomRowOrColumn(Source).PickList;
      WordWrapCaptions := TCustomRowOrColumn(Source).WordWrapCaptions;
      WordWrapCells := TCustomRowOrColumn(Source).WordWrapCells;
      CheckStyle := TCustomRowOrColumn(Source).CheckStyle;
      AutoAdjustCaptionRowHeights := TCustomRowOrColumn(Source).AutoAdjustCaptionRowHeights;
    finally
      if Assigned(Collection) then Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TCustomRowOrColumn.CheckCell(const ACol, ARow : integer);
begin
  CheckACell(ACol, ARow, CheckMax, CheckMin, Max, Min);
end;

procedure TCustomRowOrColumn.CheckACell(const ACol, ARow : integer;
  LocalCheckMax, LocalCheckMin: Boolean; LocalMax, LocalMin: extended);
var
  IntValue : integer;
  RealValue : extended;
  AGrid : TCustomRBWDataGrid;
  CellValue : string;
  SelStart: Integer;
  SelLength: Integer;
  LocalFormat: TRbwColumnFormat4;
begin
  if not LocalCheckMax and not LocalCheckMin then Exit;
  LocalFormat := Grid.GetCellFormat(ACol, ARow);
  if not (LocalFormat in [rcf4Integer, rcf4Real]) then Exit;
  AGrid := Grid;
  if (ACol < AGrid.FixedCols) or (ARow < AGrid.FixedRows) then Exit;
  CellValue := AGrid.Cells[ACol, ARow];
  if (CellValue = '') then Exit;
  if LocalFormat = rcf4Integer then
  begin
    IntValue := 0;
    if CellValue <> '-' then
    begin
      try
        IntValue := StrToInt(CellValue);
      except on EConvertError do
        begin
          AGrid.Cells[ACol, ARow] := '0';
          CheckACell(ACol, ARow, LocalCheckMax, LocalCheckMin, LocalMax, LocalMin);
        end;
      end;

      if LocalCheckMax and (IntValue > LocalMax) then
      begin
        IntValue := Trunc(LocalMax);
        if (LocalMax < 0) and (IntValue <> LocalMax) then
        begin
          Dec(IntValue);
        end;
        Beep;
        SelStart := -1;
        SelLength := -1;
        if AGrid.InplaceEditor <> nil then
        begin
          SelStart := AGrid.InplaceEditor.SelStart;
          SelLength := AGrid.InplaceEditor.SelLength;
        end;
        AGrid.Cells[ACol, ARow] := IntToStr(IntValue);
        if (not AGrid.Focused) and Assigned(AGrid.OnExit) then
        begin
          AGrid.OnExit(AGrid);
        end;
        if AGrid.InplaceEditor <> nil then
        begin
          AGrid.InplaceEditor.SelStart := SelStart;
          AGrid.InplaceEditor.SelLength := SelLength;
        end;
      end;
      if LocalCheckMin and (IntValue < LocalMin) then
      begin
        IntValue := Trunc(LocalMin);
        if (LocalMin > 0) and (IntValue <> LocalMin) then
        begin
          Inc(IntValue);
        end;
        Beep;
        SelStart := -1;
        SelLength := -1;
        if AGrid.InplaceEditor <> nil then
        begin
          SelStart := AGrid.InplaceEditor.SelStart;
          SelLength := AGrid.InplaceEditor.SelLength;
        end;
        AGrid.Cells[ACol, ARow] := IntToStr(IntValue);
        if (not AGrid.Focused) and Assigned(AGrid.OnExit) then
        begin
          AGrid.OnExit(AGrid);
        end;
        if AGrid.InplaceEditor <> nil then
        begin
          AGrid.InplaceEditor.SelStart := SelStart;
          AGrid.InplaceEditor.SelLength := SelLength;
        end;
      end;
    end;
  end
  else if LocalFormat = rcf4Real then
  begin
    RealValue := 0;
    if CellValue <> '-' then
    begin
      try
        RealValue := AGrid.LocalStrToFloat(CellValue);
      except on EConvertError do
        begin
          AGrid.Cells[ACol, ARow] := '0';
          CheckACell(ACol, ARow, LocalCheckMax, LocalCheckMin,
            LocalMax, LocalMin);
        end;
      end;
      if LocalCheckMax and (RealValue > LocalMax) then
      begin
        Beep;
        SelStart := -1;
        SelLength := -1;
        if AGrid.InplaceEditor <> nil then
        begin
          SelStart := AGrid.InplaceEditor.SelStart;
          SelLength := AGrid.InplaceEditor.SelLength;
        end;
        AGrid.Cells[ACol, ARow] := FloatToStr(LocalMax);
        if (not AGrid.Focused) and Assigned(AGrid.OnExit) then
        begin
          AGrid.OnExit(AGrid);
        end;
        if AGrid.InplaceEditor <> nil then
        begin
          AGrid.InplaceEditor.SelStart := SelStart;
          AGrid.InplaceEditor.SelLength := SelLength;
        end;
      end;
      if LocalCheckMin and (RealValue < LocalMin) then
      begin
        Beep;
        SelStart := -1;
        SelLength := -1;
        if AGrid.InplaceEditor <> nil then
        begin
          SelStart := AGrid.InplaceEditor.SelStart;
          SelLength := AGrid.InplaceEditor.SelLength;
        end;
        AGrid.Cells[ACol, ARow] := FloatToStr(LocalMin);
        if (not AGrid.Focused) and Assigned(AGrid.OnExit) then
        begin
          AGrid.OnExit(AGrid);
        end;
        if AGrid.InplaceEditor <> nil then
        begin
          AGrid.InplaceEditor.SelStart := SelStart;
          AGrid.InplaceEditor.SelLength := SelLength;
        end;
      end;
    end;
  end;
end;

procedure TCustomRowOrColumn.SetMin(const Value: extended);
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

procedure TCustomRowOrColumn.SetMax(const Value: extended);
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

procedure TCustomRowOrColumn.SetCheckMax(const Value: boolean);
begin
  if (FCheckMax <> Value) then FCheckMax := Value;
  if FCheckMax then CheckRange;
end;

procedure TCustomRowOrColumn.SetButtonCaption(const Value: string);
begin
  FButtonCaption := Value;
  Changed(False);
end;

procedure TCustomRowOrColumn.SetButtonUsed(const Value: boolean);
begin
  if FButtonUsed <> Value then
  begin
    FButtonUsed := Value;
    if FButtonUsed then
    begin
      ComboUsed := False;
    end;
    Changed(False);
  end;
end;

procedure TCustomRowOrColumn.SetParentButtonFont(const Value: boolean);
begin
  FParentButtonFont := Value;
  if ParentButtonFont and (Grid <> nil) then
  begin
    ButtonFont := Grid.Font;
  end;
end;

function TCustomRowOrColumn.GetButtonFont: TFont;
begin
  if ParentButtonFont and (Grid <> nil) and
    not(csLoading in Grid.ComponentState) and
    not (csDesigning in Grid.ComponentState) then
  begin
    result := Grid.Font;
  end
  else
  begin
    result := FButtonFont;
  end;
end;

function TCustomRowOrColumn.GetCaseSensitivePicklist: boolean;
begin
  result := (FPicklist as TStringList).CaseSensitive;
end;

procedure TCustomRowOrColumn.InvalidateCachedWidth(Sender: TObject);
var
  LocalGrid: TCustomRBWDataGrid;
begin
  LocalGrid := Grid;
  if LocalGrid <> nil then
  begin
    LocalGrid.InvalidateCachedWidth;
  end;
end;

procedure TCustomRowOrColumn.SetButtonFont(const Value: TFont);
begin
  FButtonFont.Assign(Value);
end;

constructor TCustomRowOrColumn.Create(Collection: TCollection);
begin
  inherited;
  FFormat := rcf4String;
  FCaptionAlignment := taCenter;
  FButtonFont := TFont.Create;
  FButtonCaption := '...';
  FButtonWidth := 20;
  FButtonUsed := False;
  FPickList := TStringList.Create;
  TStringList(FPickList).OnChange := InvalidateCachedWidth;
  if (Grid <> nil) and
    not Grid.FUpdating and not (csLoading in Grid.ComponentState) then
  begin
    Grid.FUpdating := True;
    SetGridRowOrColumnCount(Collection.Count);
    Grid.FUpdating := False;
  end;
end;

destructor TCustomRowOrColumn.Destroy;
begin
  FPickList.Free;
  if (Grid <> nil) and not Grid.FUpdating
    and (([csLoading, csDestroying] * Grid.ComponentState) = []) then
  begin
    Grid.FUpdating := True;
    SetGridRowOrColumnCount(Collection.Count - 1);
    Grid.FUpdating := False;
  end;
  FButtonFont.Free;
  inherited;
end;

procedure TCustomRowOrColumn.SetPickList(const Value: TStrings);
begin
  if FPickList <> Value then
  begin
    FPickList.Assign(Value);
    InvalidateCachedWidth(Self);
  end;
end;

procedure TCustomRowOrColumn.SetButtonWidth(const Value: integer);
begin
  if Value = FButtonWidth then Exit;
  FButtonWidth := Value;
  Changed(False);
end;

destructor TCustomRBWDataGrid.Destroy;
begin
  FRangeSelections.Free;
  Font.OnChange := FPriorOnFontChange;
  inherited;
end;

procedure TCustomRBWDataGrid.SetOptions(const Value: TGridOptions);
begin
  FOptions := Value;
  inherited Options := Value;
end;

function TCustomRBWDataGrid.GetCellVisible(ACol, ARow: Integer): boolean;
var
  DrawInfo: TGridDrawInfo;
begin
  CalcDrawInfo(DrawInfo);
  result := (ACol >= DrawInfo.Horz.FirstGridCell)
    and (ACol <= DrawInfo.Horz.LastFullVisibleCell)
    and (ARow >= DrawInfo.Vert.FirstGridCell)
    and (ARow <= DrawInfo.Vert.LastFullVisibleCell);
end;

function TCustomRBWDataGrid.GetColCount: Longint;
begin
  result := inherited ColCount;
end;

function TCustomRBWDataGrid.GetColVisible(ACol: Integer): boolean;
var
  DrawInfo: TGridDrawInfo;
begin
  CalcDrawInfo(DrawInfo);
  result := (ACol >= DrawInfo.Horz.FirstGridCell)
    and (ACol <= DrawInfo.Horz.LastFullVisibleCell);
end;

function TCustomRBWDataGrid.GetEditStyle(ACol, ARow: Integer): TEditStyle;
var
  Item: TCustomRowOrColumn;
begin
  Item := CollectionItem(ACol, ARow);
  if Item = nil then
  begin
    Result := esSimple;
    Exit;
  end;
  if Item.ButtonUsed then
  begin
    result := esEllipsis;
  end
  else if Item.ComboUsed then
  begin
    result := esPickList;
  end
  else
  begin
    result := esSimple;
  end;
end;

function TCustomRBWDataGrid.GetRangeSelection: TGridRect;
begin
  result.TopLeft := FAnchor;
  result.BottomRight := FOtherPoint;
end;

function TCustomRBWDataGrid.GetRealValue(const ACol, ARow: integer): double;
begin
  result := StrToFloat(Cells[ACol, ARow]);
end;

function TCustomRBWDataGrid.GetRealValueDefault(const ACol, ARow: integer;
  DefaultValue: double): double;
begin
  Result := StrToFloatDef(Cells[ACol, ARow], DefaultValue)
end;

function TCustomRBWDataGrid.GetRowCount: Longint;
begin
  result := inherited RowCount;
end;

procedure TCustomRBWDataGrid.InsertColumn(ACol: Integer);
var
  ColIndex, RowIndex: integer;
begin
  ColCount := ColCount + 1;
  for ColIndex := ColCount - 1 downto ACol + 1 do
  begin
    Cols[ColIndex] := Cols[ColIndex - 1];
    ColWidths[ColIndex] := ColWidths[ColIndex - 1];
  end;
  for RowIndex := 0 to RowCount - 1 do
  begin
    Cells[ACol, RowIndex] := '';
    Objects[ACol, RowIndex] := nil;
  end;

  ResizeSpecialFormat;
  for ColIndex := ColCount - 1 downto ACol + 1 do
  begin
    for RowIndex := 0 to RowCount - 1 do
    begin
      CheckState[ColIndex, RowIndex] := CheckState[ColIndex - 1, RowIndex];
      if Assigned(FSpecialFormat[ColIndex, RowIndex])
        or Assigned(FSpecialFormat[ColIndex -1, RowIndex]) then
      begin
        UseSpecialFormat[ColIndex, RowIndex] :=
          UseSpecialFormat[ColIndex - 1, RowIndex];
        SpecialFormat[ColIndex, RowIndex] :=
          SpecialFormat[ColIndex - 1, RowIndex];
      end;
    end;
  end;
  for RowIndex := 0 to RowCount - 1 do
  begin
    CheckState[ACol, RowIndex] := cbUnchecked;
    FSpecialFormat[ACol, RowIndex] := nil;
  end;
end;

procedure TCustomRBWDataGrid.InsertRow(ARow: Integer);
var
  ColIndex, RowIndex: integer;
begin
  RowCount := RowCount + 1;
  for RowIndex := RowCount - 1 downto ARow + 1 do
  begin
    Rows[RowIndex] := Rows[RowIndex - 1];
    RowHeights[RowIndex] := RowHeights[RowIndex - 1];
  end;
  for ColIndex := 0 to ColCount - 1 do
  begin
    Cells[ColIndex, ARow] := '';
    Objects[ColIndex, ARow] := nil;
  end;

  ResizeSpecialFormat;
  for RowIndex := RowCount - 1 downto ARow + 1 do
  begin
    for ColIndex := 0 to ColCount - 1 do
    begin
      FChecked[ColIndex, RowIndex] := FChecked[ColIndex, RowIndex - 1];
      if Assigned(FSpecialFormat[ColIndex, RowIndex])
        or Assigned(FSpecialFormat[ColIndex, RowIndex -1]) then
      begin
        UseSpecialFormat[ColIndex, RowIndex] :=
          UseSpecialFormat[ColIndex, RowIndex - 1];
        SpecialFormat[ColIndex, RowIndex] :=
          SpecialFormat[ColIndex, RowIndex - 1];
      end;
    end;
  end;
  for ColIndex := 0 to ColCount - 1 do
  begin
    CheckState[ColIndex, ARow] := cbUnchecked;
    FSpecialFormat[ColIndex, ARow] := nil;
  end;
end;

procedure TCustomRBWDataGrid.InvalidateCachedWidth;
begin

end;

function TCustomRBWDataGrid.IsSelectedCell(ACol, ARow: Integer): boolean;
var
  Index: Integer;
  Range: TGridRect;
begin
  result := False;
  for Index := 0 to FRangeSelections.Count - 1 do
  begin
    Range := FRangeSelections[Index];
    if (ACol >= Range.Left) and (ACol <= Range.Right)
      and (ARow >= Range.Top) and (ARow <= Range.Bottom) then
    begin
      result := not result;
    end;
  end;
end;

type
  TWinControlCrack = class(TWinControl);

procedure TCustomRBWDataGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  ACell: TGridCoord;
  RowIndex: Integer;
  ColIndex: Integer;
  CanSelect: Boolean;
  ParentForm: TWinControlCrack;
  function GetParentForm(AControl: TWinControl): TWinControlCrack;
  begin
    while (AControl <> nil) and not (AControl is TCustomForm) do
    begin
      AControl := AControl.Parent;
    end;
    Result := TWinControlCrack(AControl);
  end;
begin
  try
  if Key = 9 then
  begin
    if ssShift in Shift then
    begin
      ACell := Selection.TopLeft;
      for RowIndex := ACell.Y downto  FixedRows do
      begin
        for ColIndex := ColCount downto FixedCols do
        begin
          if (RowIndex = ACell.Y) and (ColIndex >= ACell.X) then
          begin
            Continue;
          end;
          CanSelect := True;
          if Assigned(OnSelectCell) then
          begin
            OnSelectCell(self, ColIndex, RowIndex, CanSelect);
          end;
          if CanSelect then
          begin
            Exit;
          end;
        end;
      end;
      ParentForm :=  GetParentForm(Parent);
      if ParentForm <> nil then
      begin
        ParentForm.SelectNext(Self, False, True);
      end;
    end
    else
    begin
      ACell := Selection.BottomRight;
      for RowIndex := ACell.Y to RowCount - 1 do
      begin
        for ColIndex := FixedCols to ColCount - 1 do
        begin
          if (RowIndex = ACell.Y) and (ColIndex <= ACell.X) then
          begin
            Continue;
          end;
          CanSelect := True;
          if Assigned(OnSelectCell) then
          begin
            OnSelectCell(self, ColIndex, RowIndex, CanSelect);
          end;
          if CanSelect then
          begin
            Exit;
          end;
        end;
      end;
      ParentForm :=  GetParentForm(Parent);
      if ParentForm <> nil then
      begin
        ParentForm.SelectNext(Self, True, True);
      end;
    end;
  end;
  finally
    inherited;
  end;
end;

procedure TCustomRBWDataGrid.KeyPress(var Key: Char);
begin
  inherited;

end;

procedure TCustomRBWDataGrid.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (FRangeSelections.Count > 0)
    and (Key In [Ord('c'), Ord('C')])
    and (Shift = [ssCtrl]) then
  begin
    CopySelectedCellsToClipboard;
  end;
end;

function TCustomRBWDataGrid.GetCellFormat(const ACol, ARow: Integer)
  : TRbwColumnFormat4;
begin
  if UseSpecialFormat[ACol, ARow] then
  begin
    result := SpecialFormat[ACol, ARow];
  end
  else
  begin
    result := CollectionItem(ACol, ARow).Format;
  end;
end;

procedure TCustomRBWDataGrid.SetColCount(const Value: Longint);
var
  Expand: boolean;
begin
  if Value <= Col then
  begin
    HideEditor;
  end;
  Expand := Value > ColCount;
  if Expand then
  begin
    SetLength(FChecked, Value, RowCount);
  end;
  inherited ColCount := Value;
  if not Expand then
  begin
    SetLength(FChecked, Value, RowCount);
  end;
end;

procedure TCustomRBWDataGrid.SetRangeSelectionColor(const Value: TColor);
begin
  if FRangeSelectionColor <> Value then
  begin
    FRangeSelectionColor := Value;
    Invalidate;
  end;
end;

procedure TCustomRBWDataGrid.SetRangeSelectionFontColor(const Value: TColor);
begin
  if FRangeSelectionFontColor <> Value then
  begin
    FRangeSelectionFontColor := Value;
    Invalidate;
  end;
end;

procedure TCustomRBWDataGrid.SetRealValue(const ACol, ARow: integer;
  const Value: double);
begin
  Cells[ACol, ARow] := FloatToStr(Value);
end;

procedure TCustomRBWDataGrid.SetRow(const Value: integer);
begin
  if dgRow <> Value then
  begin
    dgRow := Value;
    if (Value >= FixedRows) and (Value < RowCount) then
    begin
      inherited Row := Value;
    end;
  end;
end;

procedure TCustomRBWDataGrid.SetRowCount(const Value: Longint);
var
  Expand: Boolean;
begin
  if Value <= Row then
  begin
    HideEditor;
  end;
  Expand := Value > RowCount;
  if Expand then
  begin
    SetLength(FChecked, ColCount, Value);
  end;
  inherited RowCount := Value;
  if not Expand then
  begin
    SetLength(FChecked, ColCount, Value);
  end;
end;

function TRbwColumn4.GetGrid: TCustomRbwDataGrid;
begin
  if Assigned(Collection) and (Collection is TRbwDataGridColumns4) then
    Result := TRbwDataGridColumns4(Collection).Grid
  else
    Result := nil;
end;

procedure TRbwColumn4.CheckRange;
var
  ACol, ARow: integer;
  Index : Integer;
  AGrid : TRbwDataGrid4;
begin
  if not CheckMax and not CheckMin then Exit;
  AGrid := Grid as TRbwDataGrid4;
  if (Format in [rcf4Integer, rcf4Real]) and (AGrid <> nil) then
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
    if ACol >= AGrid.FixedCols then
    begin
      for ARow := AGrid.FixedRows to AGrid.RowCount -1 do
      begin
        CheckCell(ACol, ARow);
      end;
    end;
  end;
end;

function TRbwColumn4.SelectedRowOrColumn: integer;
begin
  result := Grid.Col;
end;

{ TRbwRow }

procedure TRbwRow.CheckRange;
var
  ACol, ARow: integer;
  Index: Integer;
  AGrid: TRbwRowDataGrid;
begin
  if not CheckMax and not CheckMin then
    Exit;
  AGrid := Grid as TRbwRowDataGrid;
  if (Format in [rcf4Integer, rcf4Real]) and (AGrid <> nil) then
  begin
    ARow := -1;
    for Index := 0 to AGrid.RowCount - 1 do
    begin
      if AGrid.Rows[Index] = self then
      begin
        ARow := Index;
        break;
      end;
    end;
    if ARow >= AGrid.FixedRows then
    begin
      for ACol := AGrid.FixedCols to AGrid.ColCount - 1 do
      begin
        CheckCell(ACol, ARow);
      end;
    end;
  end;
end;

function TRbwRow.CollectionItemPosition: integer;
var
  AGrid : TRbwRowDataGrid;
  Index : Integer;
begin
  AGrid := Grid as TRbwRowDataGrid;
  result := -1;
  for Index := 0 to AGrid.RowCount -1 do
  begin
    if AGrid.Rows[Index] = self then
    begin
      result := Index;
      break;
    end;
  end;
end;

function TRbwRow.GetGrid: TCustomRBWDataGrid;
begin
  if Assigned(Collection) and (Collection is TRbwDataGridRows) then
  begin
    Result := TRbwDataGridRows(Collection).Grid
  end
  else
    Result := nil;
end;

function TRbwRow.SelectedRowOrColumn: integer;
begin
  result := Grid.Row;
end;


procedure TRbwRow.SetGridRowOrColumnCount(const Value: integer);
begin
  Grid.RowCount := Value;
end;

{ TRbwDataGridRows }

function TRbwDataGridRows.Add: TRbwRow;
begin
  Result := TRbwRow(inherited Add);
  if not Grid.FUpdating and not (csLoading in FGrid.ComponentState) then
  begin
    FGrid.FUpdating := True;
    FGrid.RowCount := Count;
    FGrid.FUpdating := False;
  end;
end;

constructor TRbwDataGridRows.Create(Grid: TRbwRowDataGrid;
  RowClass: TRbwRowClass);
begin
  inherited Create(RowClass);
  FGrid := Grid;
end;

function TRbwDataGridRows.GetItems(Index: Integer): TRbwRow;
begin
  if (Index >= 0) and (Index < Count) then
  begin
    Result := inherited Items[Index] as TRbwRow;
  end
  else
  begin
    Result := nil;
  end;
end;

function TRbwDataGridRows.GetOwner: TPersistent;
begin
  Result := FGrid;
end;

procedure TRbwDataGridRows.SetItems(Index: Integer; const Value: TRbwRow);
begin
  Items[Index].Assign(Value);
end;

procedure TRbwDataGridRows.Update(Item: TCollectionItem);
begin
  inherited;
  if (FGrid = nil) or (csLoading in FGrid.ComponentState) then
    Exit;

  if (csDesigning in FGrid.ComponentState) then
  begin
    FGrid.Invalidate;
  end
  else
  begin
    FGrid.InvalidateRow(FGrid.dgRow);
  end;
end;

{ TAutoAdjustColumn }

procedure TAutoAdjustColumn.Assign(Source: TPersistent);
begin
  if Source is TAutoAdjustColumn then
  begin
    if Assigned(Collection) then
      Collection.BeginUpdate;
    try
      AutoAdjustColWidths := TAutoAdjustColumn(Source).AutoAdjustColWidths;
    finally
      if Assigned(Collection) then
        Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

constructor TAutoAdjustColumn.Create(Collection: TCollection);
begin
  inherited;
  if (Grid <> nil) and
    not Grid.FUpdating and not (csLoading in Grid.ComponentState) then
  begin
    Grid.FUpdating := True;
    SetGridCount(Collection.Count);
    Grid.FUpdating := False;
  end;
end;

destructor TAutoAdjustColumn.Destroy;
begin
  if (Grid <> nil) and
    not Grid.FUpdating
    and ([csLoading, csDestroying] * Grid.ComponentState = [])
    then
  begin
    Grid.FUpdating := True;
    SetGridCount(Collection.Count - 1);
    Grid.FUpdating := False;
  end;
  inherited;
end;

function TAutoAdjustColumn.GetGrid: TCustomRBWDataGrid;
begin
  if Assigned(Collection) and (Collection is TAutoAdjustColumns) then
    Result := TAutoAdjustColumns(Collection).Grid
  else
    Result := nil;
end;

procedure TAutoAdjustColumn.SetAutoAdjustColWidths(const Value: boolean);
var
  ACol: integer;
begin
  if FAutoAdjustColWidths <> Value then
  begin
    FAutoAdjustColWidths := Value;
    if Value then
    begin
      ACol := Index;
      if (Grid <> nil) and (ACol >= 0) then
      begin
        (Grid as TRbwRowDataGrid).AdjustColWidths(ACol);
      end;
    end;
  end;
end;

procedure TAutoAdjustColumn.SetGridCount(const Value: integer);
begin
  Grid.ColCount := Value;
end;

{ TAutoAdjustColumns }

function TAutoAdjustColumns.Add: TAutoAdjustColumn;
begin
  Result := TAutoAdjustColumn(inherited Add);
  if not Grid.FUpdating and not (csLoading in FGrid.ComponentState) then
  begin
    FGrid.FUpdating := True;
    FGrid.ColCount := Count;
    FGrid.FUpdating := False;
  end;
end;

constructor TAutoAdjustColumns.Create(Grid: TCustomRbwDataGrid;
  ColumnClass: TAutoAdjustColumnClass);
begin
  inherited Create(ColumnClass);
  FGrid := Grid;
end;

function TAutoAdjustColumns.GetItems(Index: Integer): TAutoAdjustColumn;
begin
  if Index >= 0 then
  begin
    Result := inherited Items[Index] as TAutoAdjustColumn;
  end
  else
  begin
    Result := nil;
  end;
end;

function TAutoAdjustColumns.GetOwner: TPersistent;
begin
  Result := FGrid;
end;

function TAutoAdjustColumns.Insert(Index: Integer): TAutoAdjustColumn;
begin
  result := TAutoAdjustColumn(inherited Insert(Index));
  if not Grid.FUpdating and not (csLoading in FGrid.ComponentState) then
  begin
    FGrid.FUpdating := True;
    FGrid.ColCount := Count;
    FGrid.FUpdating := False;
  end;
end;

procedure TAutoAdjustColumns.SetItems(Index: Integer;
  const Value: TAutoAdjustColumn);
begin
  Items[Index].Assign(Value);
end;

procedure TAutoAdjustColumns.Update(Item: TCollectionItem);
begin
  inherited;
  if (FGrid = nil) or (csLoading in FGrid.ComponentState) then
    Exit;

  if (csDesigning in FGrid.ComponentState) then
  begin
    FGrid.Invalidate;
  end
  else
  begin
    FGrid.InvalidateCol(FGrid.dgColumn);
  end;
end;

{ TRbwRowDataGrid }

procedure TRbwRowDataGrid.AdjustColWidths(const ACol: integer);
var
  Index: integer;
  RequiredWidth, TestWidth: integer;
begin
  if (ACol < Columns.Count) and (Columns[ACol].AutoAdjustColWidths) then
  begin
    RequiredWidth := 0;
    for Index := 0 to RowCount - 1 do
    begin
      if Index >= Rows.Count then break;
      TestWidth := RequiredCellWidth(ACol, Index);
      if TestWidth > RequiredWidth then
      begin
        RequiredWidth := TestWidth
      end;
    end;

    if ColWidths[ACol] < RequiredWidth then
    begin
      ColWidths[ACol] := RequiredWidth;
    end;
  end;
end;


function TRbwRowDataGrid.CollectionItem(const ACol,
  ARow: Integer): TCustomRowOrColumn;
begin
  if (Rows = nil) or (ARow < 0) or (ARow >= Rows.Count) then
  begin
    result := nil;
  end
  else
  begin
    result := Rows[ARow];
  end;
end;

function TRbwRowDataGrid.ColorSelectedRowOrColumn(ACol, ARow: integer): boolean;
begin
  result := ColorSelectedColumn and (ACol = dgColumn);
end;

procedure TRbwRowDataGrid.ColumnMoved(FromIndex, ToIndex: Integer);
begin
  inherited;
  Columns[FromIndex].Index := ToIndex;
  MoveCheckStateWithColumn(FromIndex, ToIndex);
end;

function TRbwRowDataGrid.ColumnOrRow: integer;
begin
  result := Row;
end;

constructor TRbwRowDataGrid.Create(AOwner: TComponent);
begin
  inherited;
  FUpdateCount := 0;
  FRows := CreateRows;
  FColumns := CreateColumns;
  FUpdating := True;
  while FRows.Count < RowCount do
  begin
    FRows.Add;
  end;
  while FColumns.Count < ColCount do
  begin
    FColumns.Add;
  end;
  FUpdating := False;
  FRequiredWidthRow := -1;
end;

function TRbwRowDataGrid.CreateColumns: TAutoAdjustColumns;
begin
  result := TAutoAdjustColumns.Create(self, TAutoAdjustColumn);
end;

function TRbwRowDataGrid.CreateRows: TRbwDataGridRows;
begin
  result := TRbwDataGridRows.Create(Self, TRbwRow);
end;

procedure TRbwRowDataGrid.DeleteColumn(ACol: Integer);
var
  TempColumns: TAutoAdjustColumns;
  Index: Integer;
  SelectedCell: TGridRect;
  ColIndex: Integer;
  RowIndex: Integer;
begin
  if ACol = Col then
  begin
    HideEditor;
  end;
  if ACol >= ColCount then
  begin
    Raise ERangeError.Create(StrErrorDeletingCol);
  end;
  TempColumns := TAutoAdjustColumns.Create(nil, TAutoAdjustColumn);
  try
    TempColumns.Assign(Columns);
    TempColumns.Delete(ACol);
    inherited;
    for Index := 0 to Columns.Count - 1 do
    begin
      Columns[Index].Assign(TempColumns[Index]);
    end;
    if Col = ACol then
    begin
      if Col > 0 then
      begin
        SelectedCell := Selection;
        SelectedCell.Left := Col - 1;
        SelectedCell.Right := SelectedCell.Left;
        Selection := SelectedCell;
      end
    end;
  finally
    TempColumns.Free;
  end;
  ResizeSpecialFormat;
  for ColIndex := ACol+1 to ColCount - 1 do
  begin
    for RowIndex := 0 to RowCount - 1 do
    begin
      if Assigned(FSpecialFormat[ColIndex,RowIndex])
        or Assigned(FSpecialFormat[ColIndex-1,RowIndex]) then
      begin
        UseSpecialFormat[ColIndex-1,RowIndex] :=
          UseSpecialFormat[ColIndex,RowIndex];
        SpecialFormat[ColIndex-1,RowIndex] :=
          SpecialFormat[ColIndex,RowIndex];
      end;
    end;
  end;
  Invalidate;
end;

procedure TRbwRowDataGrid.DeleteRow(ARow: Integer);
var
  TempRows: TRbwDataGridRows;
  Index: Integer;
  ColIndex: Integer;
  RowIndex: Integer;
begin
  if ARow = Row then
  begin
    HideEditor;
  end;
  TempRows := TRbwDataGridRows.Create(nil, TRbwRow);
  try
    TempRows.Assign(Rows);
    TempRows.Delete(ARow);
    inherited;
    // Each column needs to be assigned individually because
    // assigning the whole collection causes the collection that
    // is the destinatin to be cleared. That would affected the
    // CheckState of the cells.
    for Index := 0 to Rows.Count - 1 do
    begin
      Rows[Index].Assign(TempRows[Index]);
    end;
    if Row = ARow then
    begin
      if Row > 0 then
      begin
        FdgRow := Row - 1;
      end;
    end;
  finally
    TempRows.Free;
  end;
  ResizeSpecialFormat;
  for ColIndex := 0 to ColCount - 1 do
  begin
    for RowIndex := ARow+1 to RowCount - 1 do
    begin
      if Assigned(FSpecialFormat[ColIndex,RowIndex])
        or Assigned(FSpecialFormat[ColIndex,RowIndex-1]) then
      begin
        UseSpecialFormat[ColIndex,RowIndex-1] :=
          UseSpecialFormat[ColIndex,RowIndex];
        SpecialFormat[ColIndex,RowIndex-1] :=
          SpecialFormat[ColIndex,RowIndex];
      end;
    end;
  end;
  Invalidate;
end;

destructor TRbwRowDataGrid.Destroy;
begin
  FRows.Free;
  FColumns.Free;
  inherited;
end;

procedure TRbwRowDataGrid.EndUpdate;
var
  Index: Integer;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    for Index := 0 to ColCount - 1 do
    begin
      AdjustColWidths(Index);
    end;
    for Index := 0 to RowCount - 1 do
    begin
      AdjustRowHeights(Index);
    end;
  end;
end;

function TRbwRowDataGrid.GetCheckStyle(const ACol, ARow: integer): TCheckStyle;
begin
  result := Rows[ARow].CheckStyle;
end;

function TRbwRowDataGrid.GetFixedRows: integer;
begin
  result := inherited FixedRows;
end;

//function TRbwRowDataGrid.GetUpdating: boolean;
//begin
//  result := FUpdateCount > 0;
//end;

procedure TRbwRowDataGrid.InsertColumn(ACol: Integer);
begin
  inherited;
  Columns[Columns.Count - 1].Index := ACol;
end;

procedure TRbwRowDataGrid.InsertRow(ARow: Integer);
begin
  inherited;
  Rows[Rows.Count - 1].Index := ARow;
end;

procedure TRbwRowDataGrid.InvalidateCachedWidth;
begin
  FRequiredWidthRow := -1;
end;

function TRbwRowDataGrid.IsCaptionCell(ACol, ARow: integer): boolean;
begin
  result := ACol < FixedCols;
  if Assigned(OnIsCaption) then
  begin
    OnIsCaption(Self, ACol, ARow, result);
  end;
end;

procedure TRbwRowDataGrid.Loaded;
var
  Index: integer;
begin
  inherited;
  FixedRows := FFixedRows;
  FixedCols := FFixedCols;
  for Index := 0 to ColCount - 1 do
  begin
    AdjustColWidths(Index);
  end;
end;

function TRbwRowDataGrid.PickListRequiredWidth(const ACol,
  ARow: integer): integer;
begin
  if FRequiredWidthRow <> ARow then
  begin
    FRequiredWidthRow := ARow;
    FRequiredWidth := inherited PickListRequiredWidth(ACol, ARow);
  end;
  result := FRequiredWidth;
end;

function TRbwRowDataGrid.ShouldAdjustColWidths(ACol: integer): boolean;
begin
  result := Columns[ACol].AutoAdjustColWidths;
end;

procedure TRbwRowDataGrid.RowMoved(FromIndex, ToIndex: Integer);
begin
  inherited;
  Rows[FromIndex].Index := ToIndex;
  MoveCheckStateWithRow(FromIndex, ToIndex);
end;

procedure TRbwRowDataGrid.SetCheckState(const ACol, ARow: integer;
  const Value: TCheckBoxState);
var
  ColIndex: Integer;
begin
  inherited;
  if (Value = cbChecked) and (CheckStyle[ACol, ARow] = csRadio) then
  begin
    for ColIndex := 0 to ColCount - 1 do
    begin
      if ColIndex <> ACol then
      begin
        CheckState[ColIndex, ARow] := cbUnchecked
      end;
    end;
  end;
end;

procedure TRbwRowDataGrid.SetColumns(const Value: TAutoAdjustColumns);
var
  TempFixedCols: integer;
begin
  TempFixedCols := FixedCols;
  FColumns.Assign(Value);
  TempFixedCols := Min(TempFixedCols, ColCount-1);
  FixedCols := TempFixedCols;
end;

procedure TRbwRowDataGrid.SetFixedRows(const Value: integer);
begin
  FFixedRows := Value;
  inherited FixedRows := Value;
end;

procedure TRbwRowDataGrid.SetRows(const Value: TRbwDataGridRows);
var
  TempFixedRows: integer;
begin
  TempFixedRows := FixedRows;
  FRows.Assign(Value);
  TempFixedRows := Min(TempFixedRows, RowCount-1);
  FixedRows := TempFixedRows;
end;

procedure TRbwRowDataGrid.SizeChanged(OldColCount, OldRowCount: Integer);
begin
  inherited;
  if not (csLoading in ComponentState)
    and not (csReading in ComponentState)
    and not (csReadingState in ControlState)
    and not (csCreating in ControlState)
    and not FUpdating then
  begin
    FUpdating := True;
    while Rows.Count > RowCount do
      Rows[Rows.Count - 1].Free;
    while Rows.Count < RowCount do
      Rows.Add;
    while Columns.count > ColCount do
      Columns[Columns.count - 1].Free;
    while Columns.count < ColCount do
      Columns.Add;
    FUpdating := False;
  end;
end;

constructor TCustomRBWDataGrid.Create(AOwner: TComponent);
begin
  inherited;
  FPriorOnFontChange := Font.OnChange;
  Font.OnChange := FontChanged;
  FRangeSelections:= TRangeSelections.Create;
  FRangeSelectionColor := clBlue;
  FRangeSelectionFontColor := clWhite;
  SelectedRowOrColumnColor := clAqua;
  SetLength(FChecked, 5, 5);
  Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
      goRangeSelect];
  Options := Options + [goAlwaysShowEditor];
  {$IFDEF Delphi_2009_UP}
  FDecimalSeparator := FormatSettings.DecimalSeparator;
  {$ELSE}
  FDecimalSeparator := DecimalSeparator;
  {$ENDIF}
  FAppEvents := TApplicationEvents.Create(self);
  FAppEvents.OnSettingChange := SettingsChanged;
end;

procedure TCustomRBWDataGrid.SetdgRow(const Value: integer);
var
  ColorRow: TCustomRowOrColumn;
begin
  if (FdgColumn >= 0) and (FdgColumn < ColCount)
    and (FdgRow >= 0)and (FdgRow < RowCount)  then
  begin
    ColorRow := CollectionItem(FdgColumn, FdgRow);
    if ColorRow <> nil then
    begin
      ColorRow.CheckCell(FdgColumn, FdgRow);
    end;
  end;
  FdgRow := Value;
  FSelectedRow := Value;
end;

procedure TCustomRBWDataGrid.SetdgColumn(const Value: integer);
var
  ColorRow: TCustomRowOrColumn;
begin
  if (FdgColumn >= 0) and (FdgColumn < ColCount)
    and (FdgRow >= 0)and (FdgRow < RowCount)  then
  begin
    ColorRow := CollectionItem(FdgColumn, FdgRow);
    if ColorRow <> nil then
    begin
      ColorRow.CheckCell(FdgColumn, FdgRow);
    end;
  end;
  FdgColumn := Value;
end;

procedure TCustomRBWDataGrid.GetButtonCaption(Sender: TObject;
  var ButtonCaption: string);
var
  Item: TCustomRowOrColumn;
begin
  Item := CollectionItem(Col, Row);
  if Item = nil then
  begin
    ButtonCaption := '';
  end
  else
  begin
    ButtonCaption := Item.ButtonCaption;
  end;
end;

procedure TCustomRBWDataGrid.GetButtonWidth(Sender: TObject;
  var ButtonWidth: Integer);
var
  Item: TCustomRowOrColumn;
begin
  Item := CollectionItem(Col, Row);
  if Item = nil then
  begin
    ButtonWidth := 0;
  end
  else
  begin
    ButtonWidth := Item.ButtonWidth;
  end;
end;

function TCustomRBWDataGrid.GetCaptionFlags(const ACol, ARow: integer): UINT;
begin
  result := DT_NOPREFIX;
  case CollectionItem(ACol, ARow).CaptionAlignment of
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

  if CollectionItem(ACol, ARow).WordWrapCaptions then
  begin
    result := result or DT_WORDBREAK;
  end;
end;

function TCustomRBWDataGrid.GetCellFlags(const ACol, ARow: integer): UINT;
begin
  result := DT_NOPREFIX;
  case CollectionItem(ACol, ARow).CellAlignment of
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

  if CollectionItem(ACol, ARow).WordWrapCells then
  begin
    result := result or DT_WORDBREAK;
  end;

end;

function TCustomRBWDataGrid.GetCells(ACol, ARow: Integer): string;
begin
  result := inherited Cells[ACol, ARow];
end;

procedure TCustomRBWDataGrid.SetAutoIncreaseColCount(const Value: boolean);
begin
  FAutoIncreaseColCount := Value;
  if FAutoIncreaseColCount then
  begin
    ExtendedAutoDistributeText := False;
  end;
end;

procedure TCustomRBWDataGrid.SetCells(ACol, ARow: Integer; const Value: string);
begin
  if (ACol >= 0) and (ARow >= 0) then
  begin
    inherited Cells[ACol, ARow] := Value;
    AdjustColWidths(ACol);
    AdjustRowHeights(ARow);
  end;
end;

procedure TCustomRBWDataGrid.SetChecked(const ACol, ARow: integer;
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

function TCustomRBWDataGrid.GetChecked(const ACol, ARow: integer): boolean;
begin
  result := CheckState[ACol, ARow] = cbChecked;
end;

function TRbwColumn4.CollectionItemPosition: integer;
var
  AGrid : TRbwDataGrid4;
  Index : Integer;
begin
  AGrid := Grid as TRbwDataGrid4;
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

procedure TCustomRBWDataGrid.ColWidthsChanged;
var
  Index: integer;
  ResizedCols: TIntegerDynArray;
  OldLength: integer;
begin
  inherited;
  for Index := 0 to FixedRows-1 do
  begin
    InvalidateRow(Index);
  end;
    // make a copy of the old col widths
    OldLength := Length(FColWidths);
    SetLength(FColWidths, ColCount);
    for Index := OldLength to ColCount - 1 do
    begin
      FColWidths[Index] := DefaultColWidth;
    end;
    ResizedCols := FColWidths;
    SetLength(ResizedCols, ColCount);
    // update the existing copy of ColWidths
    for Index := 0 to ColCount - 1 do
    begin
      if FColWidths[Index] <> ColWidths[Index] then
      begin
        FColWidths[Index] := ColWidths[Index];
      end;
    end;
    for Index := 0 to ColCount - 1 do
    begin
      if ResizedCols[Index] <> ColWidths[Index] then
      begin
        if Assigned(FOnColSize) then
        begin
          FOnColSize(self, Index, ResizedCols[Index]);
        end;
      end;
    end;
    Invalidate;
end;

procedure TCustomRBWDataGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  ACol, ARow : integer;
  ARect : TRect;
  Offset : integer;
  CanSelect : boolean;
  NewCoord : TGridCoord;
  NewSelection : TGridRect;
  CaptionCell: Boolean;
begin
  inherited;
  if AutoMultiEdit then
  begin
    if ([ssShift, ssCtrl] * Shift) = [] then
    begin
      Options := Options + [goEditing];
    end
    else
    begin
      Options := Options - [goEditing];
    end;
  end;
  fMouseIsDown := True;
  MouseToCell(X, Y, ACol, ARow);
  CaptionCell := (ACol < FixedCols) or (ARow < FixedRows);
  if Assigned(OnIsCaption) then
  begin
    OnIsCaption(self, ACol, ARow, CaptionCell);
  end;
  if CaptionCell then
  begin
    HideEditor;
  end;
  CanSelect := inherited SelectCell(ACol, ARow);
  if CaptionCell then
  begin
    CanSelect := False;
  end;
  dgRow := ARow;
  dgColumn := ACol;
  if not CanSelect then
  begin
    if EditorMode then
    begin
      EditorMode := False;
    end;
  end;
  if CanSelect and (ARow < FixedRows) and (ACol < FixedCols) then
  begin
    dgRow := ARow;
    dgColumn := ACol;
  end;
  if (ACol >= FixedCols) and (ACol < ColCount) then
  begin
    if (ARow >= FixedRows) and (ARow < RowCount)
      and (CanSelect or (GetCellFormat(ACol, ARow) = rcf4Boolean)) then
    begin
      NewCoord.X := ACol;
      NewCoord.Y := ARow;
      NewSelection.TopLeft := NewCoord;
      NewSelection.BottomRight := NewCoord;
      if not (goEditing in Options) and (goRangeSelect in Options)
        and (ssShift in Shift) then
      begin
        FOtherPoint := FAnchor;
        if ARow < FAnchor.Y then
        begin
          FAnchor.Y := ARow;
        end
        else
        begin
          FOtherPoint.Y := ARow;
        end;
        if ACol < FAnchor.X then
        begin
          FAnchor.X := ACol;
        end
        else
        begin
          FOtherPoint.X := ACol;
        end;
        inherited Selection := NewSelection;
      end
      else
      begin
        inherited Selection := NewSelection;
        FAnchor := NewSelection.TopLeft;
        FOtherPoint := FAnchor;
      end;
    end;
    if (GetCellFormat(ACol, ARow) = rcf4Boolean)
      and (ACol >= FixedCols) and (ARow >= FixedRows) then
    begin
//      EditorMode := False;
//      HideEditor;
      if inherited SelectCell(ACol, ARow) then
      begin
        ARect :=CellRect(ACol, ARow);
        Offset := (ARect.Bottom - ARect.Top - CheckBoxSize) div 2;
        if (goEditing in Options) and
          (Y >= ARect.Top + Offset) and
          (Y <= ARect.Bottom - Offset) and
          (X >= ARect.Left + 2) and
          (X <= ARect.Left + 2 + CheckBoxSize) then
        begin
          EditorMode := False;
          HideEditor;
          Checked[ACol, ARow] := not Checked[ACol, ARow];
        end;
      end;
      DrawCell(ACol, ARow, ARect, [gdSelected]);
    end;
  end;
  Invalidate;
  fMouseIsDown := False;
end;

procedure TCustomRBWDataGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if ([ssShift, ssCtrl] * Shift) = [] then
  begin
    if FRangeSelections.Count > 0 then
    begin
      ClearSelection;
    end;
  end
  else
  begin
    if not (ssCtrl in Shift) then
    begin
      FRangeSelections.Clear;
    end;
    FRangeSelections.Add(RangeSelection);
    HideEditor;
    Invalidate;
  end;
  inherited;
end;

function TCustomRBWDataGrid.TextRect(const ACol, ARow: integer): TRect;
begin
  result := CellRect(ACol, ARow);
  InflateRect(result, -2, -2);
  if (GetCellFormat(ACol,ARow) = rcf4Boolean)
    and (ARow >= FixedRows) and (ACol >= FixedCols) then
  begin
    Inc(result.Left,CheckBoxSize + 2);
  end;
end;

procedure TCustomRBWDataGrid.TopLeftChanged;
begin
  inherited;
  Invalidate;
end;

procedure TCustomRBWDataGrid.UpdateEditor;
begin
  InvalidateEditor;
end;

function TCustomRBWDataGrid.GetFixedCols: integer;
begin
  result := inherited FixedCols;
end;

function TCustomRBWDataGrid.GetIntegerValue(const ACol, ARow: integer): integer;
begin
  Result := StrToInt(Cells[ACol, ARow]);
end;

function TCustomRBWDataGrid.GetIntegerValueDefault(const ACol, ARow: integer;
  DefaultValue: Integer ): integer;
begin
  Result := StrToIntDef(Cells[ACol, ARow], DefaultValue)
end;

function TCustomRBWDataGrid.GetItemIndex(const ACol, ARow: integer): integer;
begin
  result := CollectionItem(ACol, ARow).PickList.IndexOf(Cells[ACol, ARow])
end;

procedure TCustomRBWDataGrid.SetFixedCols(const Value : integer);
begin
  FFixedCols := Value;
  inherited FixedCols := Value;
end;

procedure TCustomRBWDataGrid.SetIntegerValue(const ACol, ARow: integer;
  const Value: integer);
begin
  Cells[ACol, ARow] := IntToStr(Value);
end;

procedure TCustomRBWDataGrid.SetItemIndex(const ACol, ARow, Value: integer);
begin
  if Value >= 0 then
  begin
    Cells[ACol, ARow] := CollectionItem(ACol, ARow).PickList[Value];
  end
  else
  begin
    Cells[ACol, ARow] := '';
  end;

end;

procedure TCustomRBWDataGrid.SetSelectedRowOrColumnColor(const Value: TColor);
begin
  if FSelectedRowOrColumnColor <> Value then
  begin
    FSelectedRowOrColumnColor := Value;
    Invalidate;
  end;
end;

procedure TCustomRBWDataGrid.DoExit;
begin
  inherited;
  if (dgColumn >= 0) and (dgColumn < ColCount)
    and (dgRow >= 0) and (dgRow < RowCount) then
  begin
    CollectionItem(dgColumn,dgRow).CheckCell(dgColumn, dgRow);
  end;
end;

procedure TCustomRowOrColumn.SetCaptionAlignment(const Value: TAlignment);
begin
  FCaptionAlignment := Value;
  if Grid <> nil then
  begin
    Grid.Invalidate;
  end;
end;

procedure TCustomRowOrColumn.SetCaseSensitivePicklist(const Value: boolean);
begin
  (FPicklist as TStringList).CaseSensitive := Value;
end;

procedure TCustomRowOrColumn.SetCellAlignment(const Value: TAlignment);
begin
  FCellAlignment := Value;
  if Grid <> nil then
  begin
    Grid.Invalidate;
  end;
end;

procedure TCustomRBWDataGrid.FillCaptionList(CellCaption: string;
  const CaptionList: TStringList; Width: integer);
var
  SpacePosition: integer;
  MaxWidth: integer;
  Index: integer;
  JoinedLine: Boolean;
  NewLine: string;
  StartLine: integer;
begin
  CellCaption := Trim(CellCaption);
  SpacePosition := Pos(' ', CellCaption);
  while SpacePosition > 0 do
  begin
    CaptionList.Add(Copy(CellCaption, 1, SpacePosition));
    CellCaption := Copy(CellCaption,SpacePosition + 1, MAXINT);
    SpacePosition := Pos(' ', CellCaption);
  end;
  CaptionList.Add(CellCaption);
  MaxWidth := Width;
  Canvas.Font := Font;

  StartLine := 0;
  repeat
    JoinedLine := False;
    for Index := StartLine to CaptionList.Count -2 do
    begin
      StartLine := Index;
      NewLine := CaptionList[Index] + CaptionList[Index+1];
      if Canvas.TextWidth(TrimRight(NewLine)) <= MaxWidth then
      begin
        CaptionList[Index] := NewLine;
        CaptionList.Delete(Index + 1);
        JoinedLine := True;
        Break;
      end;
    end;
  until not JoinedLine;
end;

procedure TCustomRBWDataGrid.FontChanged(Sender: TObject);
var
  ColIndex: Integer;
  RowIndex: Integer;
begin
  if Assigned(FPriorOnFontChange) then
  begin
    FPriorOnFontChange(Sender);
  end;
  for ColIndex := 0 to ColCount - 1 do
  begin
    AdjustColWidths(ColIndex);
  end;
  for RowIndex := 0 to RowCount - 1 do
  begin
    AdjustRowHeights(RowIndex);
  end;
end;

procedure TCustomRBWDataGrid.DrawOrdinaryCell(ACol, ARow: Integer;
  ARect: TRect; AState: TGridDrawState);
begin
  Canvas.FillRect(ARect);

  ARect := TextRect(ACol, ARow);

  DrawText(Canvas.Handle,PChar(Cells[ACol, ARow]),
    Length(Cells[ACol, ARow]),ARect, GetCellFlags(ACol, ARow));
end;

procedure TCustomRBWDataGrid.EndUpdate;
begin

end;

procedure TCustomRBWDataGrid.DrawCheckBoxCell(ACol, ARow: Integer;
  ARect: TRect; AState: TGridDrawState);
var
  OldStyle : TBrushStyle;
  Dest: TRect;
  ChkStyle: TCheckStyle;
  CheckBitMap: TBitMap;
begin
  if not (gdFixed in AState) then
  begin
    Canvas.FillRect(ARect);

    OldStyle := Canvas.Brush.Style;
    try
      Canvas.Brush.Style := bsSolid;

      ChkStyle := CheckStyle[ACol, ARow];
      CheckBitMap := nil;

      Dest.Left := ARect.Left + 2;
      Dest.Top := ARect.Top + (ARect.Bottom - ARect.Top - CheckBoxSize) div 2;
      Dest.Right := Dest.Left + CheckBoxSize;
      Dest.Bottom := Dest.Top + CheckBoxSize;
      if inherited SelectCell(ACol, ARow) then
      begin
        if State[ACol, ARow] = cbChecked then
        begin
          case ChkStyle of
            csCheck:
              begin
                CheckBitMap := FbmpChecked;
              end;
            csRadio:
              begin
                CheckBitMap := FBmpRadioChecked;
              end;
            else
              Assert(False);
          end;
          canvas.copyrect(Dest, CheckBitMap.canvas, Rect(0,0,
            Succ(CheckBoxSize),Succ(CheckBoxSize)));
        end
        else if State[ACol, ARow] = cbUnChecked then
        begin
          case ChkStyle of
            csCheck:
              begin
                CheckBitMap := FbmpUnchecked;
              end;
            csRadio:
              begin
                CheckBitMap := FBmpRadioUnChecked;
              end;
            else
              Assert(False);
          end;
          canvas.copyrect(Dest, CheckBitMap.canvas,
            Rect(0,0,CheckBoxSize,CheckBoxSize));
        end
        else
        begin
          case ChkStyle of
            csCheck:
              begin
                CheckBitMap := FBmpGrayed;
              end;
            csRadio:
              begin
                CheckBitMap := FBmpRadioGrayed;
              end;
            else
              Assert(False);
          end;
          canvas.copyrect(Dest, CheckBitMap.canvas,
            Rect(0,0,CheckBoxSize,CheckBoxSize));
        end;
      end
      else
      begin
        if State[ACol, ARow] = cbChecked then
        begin
          case ChkStyle of
            csCheck:
              begin
                CheckBitMap := FBmpDisabledChecked;
              end;
            csRadio:
              begin
                CheckBitMap := FBmpDisabledRadioChecked;
              end;
            else
              Assert(False);
          end;
          Canvas.Draw(ARect.Left + 2,
            ARect.Top + (ARect.Bottom - ARect.Top - 13) div 2,
            CheckBitMap);
        end
        else if State[ACol, ARow] = cbUnChecked then
        begin
          case ChkStyle of
            csCheck:
              begin
                CheckBitMap := FBmpDisabledUnchecked;
              end;
            csRadio:
              begin
                CheckBitMap := FBmpDisabledRadioUnChecked;
              end;
            else
              Assert(False);
          end;
          Canvas.Draw(ARect.Left + 2,
            ARect.Top + (ARect.Bottom - ARect.Top - 13) div 2,
            CheckBitMap);
        end
        else
        begin
          case ChkStyle of
            csCheck:
              begin
                CheckBitMap := FBmpDisabledGrayed;
              end;
            csRadio:
              begin
                CheckBitMap := FBmpDisabledRadioGrayed;
              end;
            else
              Assert(False);
          end;
          Canvas.Draw(ARect.Left + 2,
            ARect.Top + (ARect.Bottom - ARect.Top - 13) div 2,
            CheckBitMap);
        end;
      end;
      Canvas.Font := Font;

      ARect := TextRect(ACol, ARow);
      DrawText(Canvas.Handle,PChar(Cells[ACol, ARow]),
        Length(Cells[ACol, ARow]),ARect, GetCellFlags(ACol, ARow));
    finally
      Canvas.Brush.Style := OldStyle;
    end;
  end;
end;

procedure TCustomRBWDataGrid.DrawCaptionCell(ACol, ARow: Integer;
  ARect: TRect; AState: TGridDrawState);
var
  FontColor : TColor;
  NewRect: TRect;
begin
  NewRect := TextRect(ACol, ARow);

  FontColor := Canvas.Font.Color;
  try
    Canvas.Font.Color := Font.Color;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := FixedColor;

    if Assigned(FOnBeforeDrawCell) then
    begin
      FOnBeforeDrawCell(self, ACol, ARow);
    end;

    Canvas.Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
    DrawText(Canvas.Handle,PChar(Cells[ACol, ARow]),
      Length(Cells[ACol, ARow]),NewRect, GetCaptionFlags(ACol, ARow));
  finally
    Canvas.Font.Color := FontColor;
  end;
end;

procedure TCustomRBWDataGrid.SetColorRangeSelection(const Value: boolean);
begin
  if FColorRangeSelection <> Value then
  begin
    FColorRangeSelection := Value;
    Invalidate;
  end;
end;

procedure TCustomRBWDataGrid.SetColorSelectedColumnOrRow(const Value: boolean);
begin
  if FColorSelectedColumnOrRow <> Value then
  begin
    FColorSelectedColumnOrRow := Value;
    Invalidate;
  end;
end;

procedure TCustomRBWDataGrid.SetColumn(const Value: integer);
begin
  if dgColumn <> Value then
  begin
    dgColumn := Value;
    if (Value >= FixedCols) and (Value < ColCount) then
    begin
      Col := Value;
    end;
  end;
end;

procedure TCustomRBWDataGrid.SetUnselectableColor(const Value: TColor);
begin
  if FUnselectableColor <> Value then
  begin
    FUnselectableColor := Value;
    Invalidate;
  end;
end;

procedure TCustomRBWDataGrid.SetUseSpecialFormat(ACol, ARow: Integer;
  const Value: boolean);
begin
  AssignSpecialFormat(ACol, ARow);
  if FSpecialFormat[ACol, ARow].Used <> Value then
  begin
    FSpecialFormat[ACol, ARow].Used := Value;
    SetEditText(ACol, ARow, Cells[ACol, ARow]);
    Invalidate;
  end;
end;

procedure TCustomRBWDataGrid.DrawCell(ACol, ARow: Longint;
  ARect: TRect; AState: TGridDrawState);
var
  BrushColor : TColor;
  FontColor : TColor;
  CanSelect : boolean;
  CaptionCell: Boolean;
begin
  FDrawing := True;
  inherited;
//  Exit;
  BrushColor := Canvas.Brush.Color;
  FontColor := Canvas.Font.Color;
  try
    CaptionCell := (ARow < FixedRows) or (ACol < FixedCols);
    if Assigned(OnIsCaption) then
    begin
      OnIsCaption(Self, ACol, ARow, CaptionCell);
    end;
    if CaptionCell then
    begin
      DrawCaptionCell(ACol, ARow, ARect, AState);
    end
    else //if (ACol >= FixedCols) and (ARow >= FixedRows) then
    begin
      CanSelect := True;
      Canvas.Brush.Color := Color;
      Canvas.Font.Color := Font.Color;
      if {(goEditing in Options) and} Assigned(OnSelectCell) then
      begin
        OnSelectCell(self, ACol, ARow, CanSelect);
      end;
      if not CanSelect then
      begin
        Canvas.Brush.Color := UnselectableColor;
      end
      else if IsSelectedCell(ACol, ARow) then
      begin
        Canvas.Brush.Color := RangeSelectionColor;
        Canvas.Font.Color := RangeSelectionFontColor;
      end
      else if ColorSelectedRowOrColumn(ACol, ARow) then
      begin
        Canvas.Brush.Color := SelectedRowOrColumnColor;
      end;

      if Assigned(FOnBeforeDrawCell) then
      begin
        FOnBeforeDrawCell(self, ACol, ARow);
      end;

      if GetCellFormat(ACol, ARow) = rcf4Boolean then
      begin
        // draw checkbox;
        DrawCheckBoxCell(ACol, ARow, ARect, AState);
      end
      else if (ARow >= FixedRows) and (ACol >= FixedCols) then
      begin
//        inherited;
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

procedure TCustomRBWDataGrid.SetCheckState(const ACol, ARow: integer;
  const Value: TCheckBoxState);
var
  Changed: boolean;
begin
  if (GetCellFormat(ACol, ARow) = rcf4Boolean)
    and (ACol < ColCount)
    and (ARow < RowCount) then
  begin
    Changed := FChecked[ACol,ARow] <> Value;
    if Changed then
    begin
      BeginUpdate;
      try
        FChecked[ACol,ARow] := Value;
        if Assigned(FOnStateChange) then
        begin
          FOnStateChange(self, ACol, ARow, Value);
        end;
      finally
        EndUpdate
      end;
      Invalidate;
    end;
  end;
end;

function TCustomRBWDataGrid.GetCheckState(
  const ACol, ARow: integer): TCheckBoxState;
begin
  if (GetCellFormat(ACol, ARow) = rcf4Boolean)
    and (ACol < ColCount)
    and (ARow < RowCount) then
  begin
    result := FChecked[ACol,ARow];
  end
  else
  begin
    result := cbUnchecked;
  end;
end;

function TCustomRBWDataGrid.DistributeText(const ACol, ARow: integer;
  CellContents: string): boolean;
var
  AStringList: TStringList;
  LineIndex: integer;
  AString: string;
  NewRow: integer;
  NewString: String;
  WordIndex: integer;
  TabCount: Integer;
  MaxTabCount: Integer;
  CharIndex: Integer;
  CellFormat: TRbwColumnFormat4;
  NewCol: Integer;
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
  procedure AssignTextToCell;
  var
    PickIndex: Integer;
  begin
    if NewCol < ColCount then
    begin
      if SelectCell(NewCol, NewRow) then
      begin
        CellFormat := GetCellFormat(NewCol,NewRow);
//        if UseSpecialFormat[NewCol,NewRow] then
//        begin
//          CellFormat := SpecialFormat[NewCol,NewRow];
//        end;
        if CellFormat = rcf4Boolean then
        begin
          NewString := Trim(NewString);
          if UpperCase(NewString) = 'TRUE' then
          begin
            Checked[NewCol, NewRow] := True;
          end
          else if UpperCase(NewString) = 'UNDEFINED' then
          begin
            State[NewCol, NewRow] := cbGrayed;
          end
          else if NewString <> '' then
          begin
            Checked[NewCol, NewRow] := False;
          end;
        end
        else if CollectionItem(NewCol,NewRow).ComboUsed
          and CollectionItem(NewCol,NewRow).LimitToList then
        begin
          PickIndex := CollectionItem(NewCol,NewRow).PickList.IndexOf(NewString);
          if PickIndex >= 0 then
          begin
            Cells[NewCol, NewRow] := NewString;
            Objects[NewCol, NewRow] := CollectionItem(NewCol,NewRow).PickList.Objects[PickIndex];
            SetEditText(NewCol, NewRow, NewString);
          end
          else
          begin
            Cells[NewCol, NewRow] := '';
            Objects[NewCol, NewRow] := nil;
            SetEditText(NewCol, NewRow, '');
          end;
        end
        else
        begin;
          Cells[NewCol, Row] := NewString;
          SetEditText(NewCol, Row, NewString);
        end;
      end;
    end
  end;
begin
  BeginUpdate;
  AStringList := TStringList.Create;
  try
    AStringList.Text := CellContents;
    result := (AStringList.Count > 1) or (Pos(#9, CellContents) > 0);
    if result then
    begin
      if FDistributingText then
      begin
        Exit;
      end;
      FDistributingText := True;
      try
        if ExtendedAutoDistributeText then
        begin
          Assert(not AutoIncreaseColCount);
          NewRow := ARow;
          NewCol := ACol;
          LineIndex := 0;
          While LineIndex < AStringList.Count do
          begin
            AString := AStringList[LineIndex];
            while Length(AString) > 0 do
            begin
              NewString := ExtractWord(AString);
              AssignTextToCell;
              Inc(NewCol);
              if NewCol >= ColCount then
              begin
                NewCol := ACol;
                Inc(NewRow);
                if AutoIncreaseRowCount and (NewRow >= RowCount) then
                begin
                  RowCount := RowCount + 1;
                end;
                break;
              end;
            end;
            Inc(LineIndex);
            if Assigned(FOnDistributeTextProgress) then
            begin
              FOnDistributeTextProgress(self, LineIndex+1, AStringList.Count);
            end;
          end;
        end
        else
        begin
          if AutoIncreaseRowCount then
          begin
            if RowCount < ARow + AStringList.Count then
            begin
              RowCount := ARow + AStringList.Count;
            end;
          end;
          if AutoIncreaseColCount then
          begin
            MaxTabCount := 0;
            for LineIndex := 0 to AStringList.Count -1 do
            begin
              TabCount := 0;
              AString := AStringList[LineIndex];
              for CharIndex := 1 to Length(AString) do
              begin
                if AString[CharIndex] = #9 then
                begin
                  Inc(TabCount);
                end;
              end;
              if TabCount > MaxTabCount then
              begin
                MaxTabCount := TabCount;
              end;
            end;
            if ColCount <= ACol + MaxTabCount+1 then
            begin
              ColCount := ACol + MaxTabCount+1
            end;
          end;
          for LineIndex := 0 to AStringList.Count -1 do
          begin
            AString := AStringList[LineIndex];
            NewRow := ARow + LineIndex;
            WordIndex := 0;
            if AString = '' then
            begin
              if SelectCell(ACol, NewRow) then
              begin
                Cells[ACol, NewRow] := '';
                SetEditText(ACol, NewRow, '');
              end;
            end
            else
            begin
              while Length(AString) > 0 do
              begin
                NewString := ExtractWord(AString);
                NewCol := ACol + WordIndex;
                AssignTextToCell;
                Inc(WordIndex);
              end;
            end;
            if Assigned(FOnDistributeTextProgress) then
            begin
              FOnDistributeTextProgress(self, LineIndex+1, AStringList.Count);
            end;
          end;
        end;
      finally
        HideEditor;
        FDistributingText := False;
        Invalidate;
      end;
    end;

  finally
    FdgRow := ARow;
    fdgColumn := ACol;
    AStringList.Free;
    EndUpdate;
  end;
end;

procedure TCustomRBWDataGrid.SelectAll;
var
  ASelection: TGridRect;
begin
  ASelection.Left := FixedCols;
  ASelection.top := FixedRows;
  ASelection.Right := ColCount -1;
  ASelection.Bottom := RowCount -1;
  ClearSelection;
  AddRangeSelection(ASelection);

end;

function TCustomRBWDataGrid.SelectCell(ACol, ARow: Longint): Boolean;
var
  ColumnOrRow : TCustomRowOrColumn;
  CaptionCell: Boolean;
begin
  result := inherited SelectCell(ACol, ARow);

  if result and Assigned(OnIsCaption) then
  begin
    CaptionCell := False;
    OnIsCaption(self, ACol, ARow, CaptionCell);
    if CaptionCell then
    begin
      result := False;
    end;
  end;

  if result and not fMouseIsDown then
  begin
    if FSelectedRow <> ARow then
    begin
      FSelectedRow := ARow;
      Invalidate;
    end;

    if not FDeletingRow and not FDistributingText then
    begin
      ColumnOrRow := CollectionItem(ACol,ARow);
      if (ColumnOrRow = nil) or (GetCellFormat(ACol,ARow) = rcf4Boolean)
        then
      begin
        result := False;
      end
    end;
  end;
  dgColumn := ACol;
  dgRow := ARow;
end;

function TCustomRBWDataGrid.CanEditShow: Boolean;
var
  ColumnOrRow : TCustomRowOrColumn;
begin
  result := inherited CanEditShow and (Col >= FixedCols) and (Row >= FixedRows);
  if result then
  begin
    if (dgColumn >=0) and (dgColumn < ColCount) then
    begin
      ColumnOrRow := CollectionItem(dgColumn,dgRow);
      result := (ColumnOrRow <> nil)
//        and (GetCellFormat(dgColumn,dgRow) <> rcf4Boolean);
    end;
  end;
end;

function TCustomRBWDataGrid.CheckColumnDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean;
begin
  result := inherited CheckColumnDrag(Origin, Destination, MousePt);
  if Assigned(OnColMoving) then
  begin
    OnColMoving(self, Origin, Destination, result);
  end;
end;

function TCustomRBWDataGrid.CheckRowDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean;
begin
  result := inherited CheckRowDrag(Origin, Destination, MousePt);
  if Assigned(OnRowMoving) then
  begin
    OnRowMoving(self, Origin, Destination, result);
  end;
end;

procedure TCustomRBWDataGrid.SetEditorUpdateToEnd;
begin
  (InplaceEditor as TRbwInplaceEdit4).UpdateContents;
  (InplaceEditor as TRbwInplaceEdit4).SelStart := MaxInt;
//                ColumnOrRow.MaxLength;
end;

procedure TCustomRBWDataGrid.SetEditText(ACol, ARow: Longint;
  const Value: string);
var
  ColumnOrRow : TCustomRowOrColumn;
  NewValue : string;
  ConversionOK : boolean;
  AFloat : extended;
  E : integer;
begin
  if fDeleting then
  begin
    inherited;
    Exit;
  end;
  BeginUpdate;
  try
    if AutoDistributeText then
    begin
      if DistributeText(ACol, ARow, Value) then
      begin
        Exit;
      end;
    end;

    if (ACol < 0) or (ARow < 0) or (ACol >= ColCount) or (ARow >= RowCount) then
    begin
      inherited;
      Exit;
    end;

    ColumnOrRow := CollectionItem(ACol, ARow);
    if ColumnOrRow = nil then
    begin
      inherited;
      Exit;
    end;
    ColumnOrRow.CheckCell(ACol, ARow);

    if ColumnOrRow.ComboUsed and ColumnOrRow.LimitToList then
    begin
      if ColumnOrRow.PickList.IndexOf(Value) < 0 then
      begin
        Exit;
      end;
    end;
    case GetCellFormat(ACol, ARow) of
      rcf4String:
        begin
          if not (ColumnOrRow.ComboUsed and ColumnOrRow.LimitToList)
            and (ColumnOrRow.MaxLength > 0)
            and (Length(Value) > ColumnOrRow.MaxLength) then
          begin
            inherited SetEditText(ACol, ARow,
              Copy(Value, 1, ColumnOrRow.MaxLength));
            if not ColumnOrRow.ComboUsed and (Value <> NewValue) then
            begin
              (InplaceEditor as TRbwInplaceEdit4).UpdateContents;
              (InplaceEditor as TRbwInplaceEdit4).SelStart :=
                ColumnOrRow.MaxLength;
            end;
          end
          else
          begin
            inherited;
          end;
        end;
      rcf4Integer:
        begin
          NewValue := Value;
          if Value <> '' then
          begin
            Val(Value, DummyIntValue, E);
            if E <> 0 then
            begin
              NewValue := Copy(Value,1,E-1);
            end;
          end;
          inherited SetEditText(ACol, ARow, NewValue);
          if not ColumnOrRow.ComboUsed and (Value <> NewValue) then
          begin
            (InplaceEditor as TRbwInplaceEdit4).UpdateContents;
            (InplaceEditor as TRbwInplaceEdit4).SelStart := E;
          end;
        end;
      rcf4Real:
        begin
          NewValue := Value;
          if (Value <> '') and (Value <> '-') and (Value <> '+') then
          begin
            ConversionOK := false;
            While not ConversionOK do
            begin
              ConversionOK := TextToFloat(PChar(NewValue), AFloat, fvExtended);

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

          inherited SetEditText(ACol, ARow, NewValue);
          if not ColumnOrRow.ComboUsed and (Value <> NewValue) then
          begin
            (InplaceEditor as TRbwInplaceEdit4).UpdateContents;
            (InplaceEditor as TRbwInplaceEdit4).SelStart := Length(NewValue);
          end;
        end;
      rcf4Boolean:
        begin
          if (AnsiCompareText(Value, 'TRUE') = 0)
            or (AnsiCompareText(Value, 'T') = 0)
            or (AnsiCompareText(Value, 'YES') = 0)
            or (AnsiCompareText(Value, 'Y') = 0)
            then
          begin
            Checked[ACol, ARow] := true;
            inherited SetEditText(ACol, ARow, '');
          end
          else if (AnsiCompareText(Value, 'FALSE') = 0)
            or (AnsiCompareText(Value, 'F') = 0)
            or (AnsiCompareText(Value, 'NO') = 0)
            or (AnsiCompareText(Value, 'N') = 0)
            then
          begin
            Checked[ACol, ARow] := False;
            inherited SetEditText(ACol, ARow, '');
          end;
        end;
    else Assert(False);
    end;
    AdjustColWidths(ACol);
    AdjustRowHeights(ARow);
  finally
    EndUpdate;
  end;
end;

function TCustomRBWDataGrid.CreateEditor: TInplaceEdit;
begin
  Result := TRbwInplaceEdit4.Create(Self);
  TRbwInplaceEdit4(Result).OnGetPickListitems := GetPickListItems;
  TRbwInplaceEdit4(Result).OnGetButtonCaption := GetButtonCaption;
  TRbwInplaceEdit4(Result).OnEditButtonClick := ButtonClick;
  TRbwInplaceEdit4(Result).OnGetButtonWidth := GetButtonWidth;
end;

procedure TCustomRowOrColumn.SetAutoAdjustCaptionRowHeights(
  const Value: boolean);
var
  ARow: Integer;
begin
  if FAutoAdjustCaptionRowHeights <> Value then
  begin
    FAutoAdjustCaptionRowHeights := Value;
    if Value and (Grid <> nil) then
    begin
      ARow := Grid.Row;
      if ARow >= 0 then
      begin
        Grid.AdjustRowHeights(ARow);
      end;
    end;
  end;
end;

procedure TCustomRowOrColumn.SetAutoAdjustRowHeights(const Value: boolean);
var
  ARow: integer;
begin
  if FAutoAdjustRowHeights <> Value then
  begin
    FAutoAdjustRowHeights := Value;
    if Value and (Grid <> nil) then
    begin
      ARow := Grid.Row;
      if ARow >= 0 then
      begin
        Grid.AdjustRowHeights(ARow);
      end;
    end;
  end;
end;

procedure TCustomRowOrColumn.SetWordWrapCaptions(const Value: boolean);
begin
  FWordWrapCaptions := Value;
  if Grid <> nil then
  begin
    Grid.Invalidate;
  end;
end;

procedure TCustomRowOrColumn.SetWordWrapCells(const Value: boolean);
begin
  FWordWrapCells := Value;
  if Grid <> nil then
  begin
    Grid.Invalidate;
  end;
end;

procedure TRbwRowDataGrid.AdjustRowHeights(const ARow: integer);
var
  RequiredHeight, TestHeight: integer;
  ColIndex: integer;
begin
  RequiredHeight := 0;
  if (Rows[ARow] <> nil) and Rows[ARow].AutoAdjustRowHeights then
  begin
    for ColIndex := 0 to ColCount -1 do
    begin

      Canvas.Font.Assign(Font);
      TestHeight := RequiredCellHeight(ColIndex, ARow);
      if TestHeight > RequiredHeight then
      begin
        RequiredHeight := TestHeight
      end;

    end;
    if RowHeights[ARow] < RequiredHeight then
    begin
      RowHeights[ARow] := RequiredHeight;
    end;
  end;
end;

procedure TRbwRowDataGrid.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

function TCustomRBWDataGrid.RequiredCellHeight(
  const ACol, ARow: integer): integer;
var
  CellList: TStringList;
  CellCaption: string;
  AvailableWidth: integer;
  Flags: UINT;
  ARect: TRect;
  TempString: string;
  Index: Integer;
  AColumnOrRow: TCustomRowOrColumn;
begin
  result := 2;
  Canvas.Font.Assign(Font);
  AColumnOrRow := CollectionItem(ACol, ARow);
  if AColumnOrRow.AutoAdjustRowHeights or AColumnOrRow.AutoAdjustCaptionRowHeights then
  begin

    if (IsCaptionCell(ACol, ARow)
      and CollectionItem(ACol, ARow).WordWrapCaptions)
      or (not IsCaptionCell(ACol, ARow)
      and CollectionItem(ACol, ARow).WordWrapCells) then
    begin
      if IsCaptionCell(ACol, ARow)  then
      begin
        Flags := GetCaptionFlags(ACol, ARow);
      end
      else
      begin
        Flags := GetCellFlags(ACol, ARow);
      end;
      Flags := Flags or DT_CALCRECT;

      // can't use TextRect here because it doesn't work if the
      // cell is not visible.
      ARect.Left := 0;
      ARect.Right := ColWidths[ACol] - 4;
      ARect.Top := 0;
      ARect.Bottom := RowHeights[ARow] - 4;

      CellList := TStringList.Create;
      try
        CellCaption := Cells[ACol, ARow];
        AvailableWidth := ColWidths[ACol]-4;
        if not IsCaptionCell(ACol, ARow) then
        begin
          if GetCellFormat(ACol, ARow) = rcf4Boolean then
          begin
            Dec(AvailableWidth,CheckBoxSize+2);
          end
          else if (CollectionItem(ACol, ARow).ComboUsed)
            and ShouldAdjustColWidths(ACol) then
          begin
            Dec(AvailableWidth,20);
          end;
          if (ACol = ColCount -1) and (RowCount > VisibleRowCount + FixedRows) then
          begin
            // adjust for vertical scroll bar width.
            Dec(AvailableWidth,16);
          end;
        end;

        FillCaptionList(CellCaption, CellList, AvailableWidth);
        for Index := 0 to CellList.Count - 1 do
        begin
          CellList[Index] := '0';
        end;
        TempString := TrimRight(CellList.Text);
        result := DrawText(Canvas.Handle,PChar(TempString),
          Length(TempString),ARect, Flags) + 4;

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

procedure TCustomRBWDataGrid.Resize;
begin
  inherited;
  Invalidate;
end;

procedure TCustomRBWDataGrid.ResizeSpecialFormat;
var
  NCol, NRow: integer;
begin
  NCol := Length(FSpecialFormat);
  if NCol < ColCount then
  begin
    if NCol = 0 then
    begin
      NRow := RowCount
    end
    else
    begin
      NRow := Length(FSpecialFormat[0]);
    end;
    NCol := ColCount;
    SetLength(FSpecialFormat, NCol, NRow);
  end;
  NRow := Length(FSpecialFormat[0]);
  if NRow < RowCount then
  begin
    NRow := RowCount;
    SetLength(FSpecialFormat, NCol, NRow);
  end;
end;

procedure TCustomRBWDataGrid.MoveCheckStateWithColumn(FromColIndex,
  ToColIndex: Integer);
var
  ColIndex: Integer;
  Temp: TCheckBoxState;
  RowIndex: Integer;
begin
  Assert(FromColIndex < ColCount);
  Assert(ToColIndex < ColCount);
  if FromColIndex < ToColIndex then
  begin
    for RowIndex := 0 to RowCount - 1 do
    begin
      Temp := FChecked[FromColIndex, RowIndex];
      for ColIndex := FromColIndex to ToColIndex - 1 do
      begin
        FChecked[ColIndex, RowIndex] := FChecked[ColIndex + 1, RowIndex];
      end;
      FChecked[ToColIndex, RowIndex] := Temp;
    end;
  end
  else
  begin
    for RowIndex := 0 to RowCount - 1 do
    begin
      Temp := FChecked[FromColIndex, RowIndex];
      for ColIndex := FromColIndex downto ToColIndex + 1 do
      begin
        FChecked[ColIndex, RowIndex] := FChecked[ColIndex - 1, RowIndex];
      end;
      FChecked[ToColIndex, RowIndex] := Temp;
    end;
  end;
end;

procedure TCustomRBWDataGrid.MoveCheckStateWithRow(
  FromRowIndex, ToRowIndex: Integer);
var
  ColIndex: Integer;
  Temp: TCheckBoxState;
  RowIndex: Integer;
begin
  Assert(FromRowIndex < RowCount);
  Assert(ToRowIndex < RowCount);
  if FromRowIndex < ToRowIndex then
  begin
    for ColIndex := 0 to ColCount - 1 do
    begin
      Temp := FChecked[ColIndex, FromRowIndex];
      for RowIndex := FromRowIndex to ToRowIndex - 1 do
      begin
        FChecked[ColIndex, RowIndex] := FChecked[ColIndex, RowIndex + 1];
      end;
      FChecked[ColIndex, ToRowIndex] := Temp;
    end;
  end
  else
  begin
    for ColIndex := 0 to ColCount - 1 do
    begin
      Temp := FChecked[ColIndex, FromRowIndex];
      for RowIndex := FromRowIndex downto ToRowIndex + 1 do
      begin
        FChecked[ColIndex, RowIndex] := FChecked[ColIndex, RowIndex - 1];
      end;
      FChecked[ColIndex, ToRowIndex] := Temp;
    end;
  end;
end;

function TCustomRBWDataGrid.LocalizeString(ANumberString : string): string;
var
  DecimalPosition : integer;
  EndString: string;
begin
  {$IFDEF Delphi_2009_UP}
  if (FormatSettings.DecimalSeparator = '.') then
  {$ELSE}
  if (DecimalSeparator = '.') then
  {$ENDIF}
  begin
    DecimalPosition := Pos(',', ANumberString);
    if DecimalPosition > 0 then
    begin
      {$IFDEF Delphi_2009_UP}
      ANumberString[DecimalPosition] := FormatSettings.DecimalSeparator;
      {$ELSE}
      ANumberString[DecimalPosition] := DecimalSeparator;
      {$ENDIF}
    end;
  end
  else
  begin
    DecimalPosition := Pos('.', ANumberString);
    if DecimalPosition > 0 then
    begin
      {$IFDEF Delphi_2009_UP}
      ANumberString[DecimalPosition] := FormatSettings.DecimalSeparator;
      {$ELSE}
      ANumberString[DecimalPosition] := DecimalSeparator;
      {$ENDIF}
    end;
  end;
  result := ANumberString;
  EndString := LowerCase(RightStr(result, 2));
  if (EndString = 'e-') or (EndString = 'e+') then
  begin
    ANumberString := ANumberString + '0';
  end;
end;

function TCustomRBWDataGrid.LocalStrToFloat(S: string): Extended;
begin
  if (S = '') then
  begin
    result := 0;
    Exit;
  end;
  result := StrToFloat(LocalizeString(S));
end;

function TCustomRBWDataGrid.WidthNeededToFitText(
  const ACol, ARow: Integer): integer;
var
  Temp: Integer;
  CaptionIndex: Integer;
  CellCaption: string;
  CellList: TStringList;
  AvailableWidth: integer;
  CaptionCell: boolean;
begin
  Canvas.Font.Assign(Font);
  CaptionCell := IsCaptionCell(ACol, ARow);
  if (CaptionCell and CollectionItem(ACol, ARow).WordWrapCaptions)
    or ((not CaptionCell) and CollectionItem(ACol, ARow).WordWrapCells) then
  begin
    CellList := TStringList.Create;
    try
      CellCaption := Cells[ACol, ARow];
      AvailableWidth := ColWidths[ACol] - 4;
      if (ARow >= FixedRows)
        and (GetCellFormat(ACol, ARow) = rcf4Boolean) then
      begin
        Dec(AvailableWidth,CheckBoxSize+2);
      end;

      result := 0;
      FillCaptionList(CellCaption, CellList, AvailableWidth);
      for CaptionIndex := 0 to CellList.Count - 1 do
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
end;

function TCustomRBWDataGrid.PickListRequiredWidth(
  const ACol, ARow: integer): integer;
var
  PickIndex: Integer;
  TestWidth: Integer;
  ColOrRow: TCustomRowOrColumn;
begin
  result := 0;
  ColOrRow := CollectionItem(ACol, ARow);
  for PickIndex := 0 to ColOrRow.PickList.Count - 1 do
  begin
    TestWidth := Canvas.TextWidth(ColOrRow.PickList[PickIndex]);
    if TestWidth > result then
    begin
      result := TestWidth
    end;
  end;
  Inc(result, ComboAdustSize);

end;

function TCustomRBWDataGrid.RequiredCellWidth(
  const ACol, ARow: integer): integer;
var
  TestWidth: Integer;
begin
  result := 0;
  if ShouldAdjustColWidths(ACol) then
  begin

    Canvas.Font.Assign(Font);
    result := WidthNeededToFitText(ACol, ARow);
    if not IsCaptionCell(ACol, ARow) then
    begin
      if GetCellFormat(ACol, ARow) = rcf4Boolean then
      begin
        Inc(result, CheckBoxSize + 2);
      end
      else if CollectionItem(ACol, ARow).ComboUsed then
      begin
        TestWidth := PickListRequiredWidth(ACol, ARow);
//        for PickIndex := 0 to CollectionItem(ACol, ARow).PickList.Count - 1 do
//        begin
//          TestWidth := Canvas.TextWidth(
//            CollectionItem(ACol, ARow).PickList[PickIndex]);
          if TestWidth > result then
          begin
            result := TestWidth
          end;
//        end;
//        Inc(result, ComboAdustSize);
      end;
    end;
  end;
end;

function TCustomRBWDataGrid.GetSelection: TGridRect;
begin
  result := inherited Selection;
end;

function TCustomRBWDataGrid.GetSpecialFormat(ACol,
  ARow: Integer): TRbwColumnFormat4;
begin
  AssignSpecialFormat(ACol, ARow);
  result := FSpecialFormat[ACol, ARow].Format;
end;

procedure TCustomRBWDataGrid.SetSelection(const Value: TGridRect);
begin
  inherited Selection := Value;
  dgColumn := Value.Left;
  dgRow := Value.Top;
  FAnchor := inherited Selection.TopLeft;
  FOtherPoint := inherited Selection.BottomRight;
end;

procedure TCustomRBWDataGrid.SetSpecialFormat(ACol, ARow: Integer;
  const Value: TRbwColumnFormat4);
begin
  AssignSpecialFormat(ACol, ARow);
  if FSpecialFormat[ACol, ARow].Format <> Value then
  begin
    FSpecialFormat[ACol, ARow].Format := Value;
    if UseSpecialFormat[ACol, ARow] then
    begin
      SetEditText(ACol, ARow, Cells[ACol, ARow]);
      Invalidate;
    end;
  end;
end;

procedure TCustomRBWDataGrid.SettingsChanged(Sender: TObject; Flag: Integer;
  const Section: string; var Result: Integer);
var
  DecSep: Char;
  ColIndex: Integer;
  RowIndex: Integer;
begin
  {$IFDEF Delphi_2009_UP}
  DecSep := FormatSettings.DecimalSeparator;
  {$ELSE}
  DecSep := DecimalSeparator;
  {$ENDIF}
  if FDecimalSeparator <> DecSep then
  begin
    BeginUpdate;
    try
      for ColIndex := FixedCols to ColCount - 1 do
      begin
        for RowIndex := FixedRows to RowCount - 1 do
        begin
          if GetCellFormat(ColIndex, RowIndex) = rcf4Real then
          begin
            Cells[ColIndex,RowIndex] := StringReplace(Cells[ColIndex,RowIndex],
              FDecimalSeparator, DecSep, [rfReplaceAll]);
          end;
        end;
      end;
    finally
      EndUpdate;
    end;
    FDecimalSeparator := DecSep;
  end;
end;

procedure TCustomRBWDataGrid.GetPickListItems(ACol, ARow: Integer;
  Items: TStrings);
begin
  Items.Assign(CollectionItem(ACol, ARow).PickList);
end;

function TCustomRBWDataGrid.GetUseSpecialFormat(ACol, ARow: Integer): boolean;
begin
  ResizeSpecialFormat;
  result := (ACol < ColCount) and (ARow < RowCount)
    and (ACol >= 0) and (ARow >= 0)
    and Assigned(FSpecialFormat[ACol, ARow]);
  if result then
  begin
    result := FSpecialFormat[ACol, ARow].Used;
  end;
end;

procedure TCustomRBWDataGrid.HideEditor;
begin
  if EditorMode then
  begin
    inherited;
  end;
end;

procedure TCustomRBWDataGrid.WMHScroll(var Msg: TWMHScroll);
begin
  inherited;
  if Assigned(FOnHScroll) then
    FOnHScroll(Self);
end;

procedure TCustomRBWDataGrid.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  if Assigned(FOnVScroll) then
    FOnVScroll(Self);
end;

procedure TCustomRBWDataGrid.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);
  if (dgColumn >= 0) and (dgColumn < ColCount) then
  begin
    Col := dgColumn;
  end
  else
  begin
    dgColumn := Col
  end;
  if (dgRow >= 0) and (dgRow < RowCount) then
  begin
    inherited Row := dgRow;
  end
  else
  begin
    dgRow := Row;
  end;
end;

procedure TCustomRBWDataGrid.SetExtendedAutoDistributeText(
  const Value: boolean);
begin
  FExtendedAutoDistributeText := Value;
  if FExtendedAutoDistributeText then
  begin
    AutoIncreaseColCount := False;
  end;
end;

{ TRangeSelections }

procedure TRangeSelections.Add(const Range: TGridRect);
begin
  if FCount = Length(FRanges) then
  begin
    Grow;
  end;
  FRanges[FCount] := Range;
  Inc(FCount);
end;

procedure TRangeSelections.Clear;
begin
  FCount := 0;
  // It would be OK to set the length of FRanges to 0 here too.
  // That is not done because it is expected that the length of
  // FRanges is not expected to ever get large enough to be a real
  // problem and because it is likely that FRanges would soon have
  // to be made longer again.
end;

function TRangeSelections.GetRange(const Index: integer): TGridRect;
begin
  result := FRanges[Index];
end;

procedure TRangeSelections.Grow;
begin
  if Length(FRanges) < 4 then
  begin
    SetLength(FRanges, 4);
  end
  else
  begin
    SetLength(FRanges, Length(FRanges)*2);
  end;
end;

{ TSpecialFormatter }

function TSpecialFormatter.GetFormat: TRbwColumnFormat4;
begin
  result := FFormat;
end;

function TSpecialFormatter.GetUsed: boolean;
begin
  result := FUsed;
end;

procedure TSpecialFormatter.SetFormat(const Value: TRbwColumnFormat4);
begin
  FFormat := Value;
end;

procedure TSpecialFormatter.SetUsed(const Value: boolean);
begin
  FUsed := Value;
end;

{ TRbwInplaceEdit4 }

procedure TRbwInplaceEdit4.BoundsChanged;
var
  R: TRect;
begin
  if EditStyle <> esEllipsis then
  begin
    inherited
  end
  else
  begin
    SetRect(R, 2, 2, Width - 2, Height);
      if not Grid.UseRightToLeftAlignment then
        Dec(R.Right, PushButtonWidth)
      else
        Inc(R.Left, PushButtonWidth - 2);
    SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@R));
    SendMessage(Handle, EM_SCROLLCARET, 0, 0);
    if SysLocale.FarEast then
      SetImeCompositionWindow(Font, R.Left, R.Top);
  end;
end;

function TRbwInplaceEdit4.ButtonRect: TRect;
begin
  if EditStyle = esEllipsis then
  begin
    if not Grid.UseRightToLeftAlignment then
      Result := Rect(Width - PushButtonWidth, 0, Width, Height)
    else
      Result := Rect(0, 0, PushButtonWidth, Height);
  end
  else
  begin
    result := inherited ButtonRect;
  end;
end;

constructor TRbwInplaceEdit4.Create(Owner: TComponent);
begin
  inherited;
  FPushButtonWidth := 30;
  DropDownRows := 7;
end;

function TRbwInplaceEdit4.GetButtonCaption(ACol, ARow: integer): string;
begin
  result := '...';
  if Assigned(OnGetButtonCaption) then
  begin
    OnGetButtonCaption(self, result);
  end;
end;

function TRbwInplaceEdit4.GetPushButtonWidth: integer;
begin
  result := FPushButtonWidth;
  if Assigned(OnGetButtonWidth) then
  begin
    OnGetButtonWidth(self, result);
  end;
end;

procedure TRbwInplaceEdit4.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) and (EditStyle <> esSimple) and
    OverButton(Point(X,Y)) then
  begin
    if ListVisible then
      CloseUp(False)
    else
    begin
      MouseCapture := True;
      FTracking := True;
      TrackButton(X, Y);
      if Assigned(ActiveList) then
        DropDown;
    end;
  end;
  if Assigned(OnMouseDown) then OnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TRbwInplaceEdit4.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ListPos: TPoint;
  MousePos: TSmallPoint;
begin
  if FTracking then
  begin
    TrackButton(X, Y);
    if ListVisible then
    begin
      ListPos := ActiveList.ScreenToClient(ClientToScreen(Point(X, Y)));
      if PtInRect(ActiveList.ClientRect, ListPos) then
      begin
        StopTracking;
        MousePos := PointToSmallPoint(ListPos);
        SendMessage(ActiveList.Handle, WM_LBUTTONDOWN, 0, Integer(MousePos));
        Exit;
      end;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TRbwInplaceEdit4.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  WasPressed: Boolean;
begin
  WasPressed := Pressed;
  StopTracking;
  if (Button = mbLeft) and (EditStyle = esEllipsis) and WasPressed then
    DoEditButtonClick;
  if Assigned(OnMouseUp) then OnMouseUp(Self, Button, Shift, X, Y);
end;

function TRbwInplaceEdit4.OverButton(const P: TPoint): Boolean;
begin
  Result := PtInRect(ButtonRect, P);
end;

{$R-}

procedure TRbwInplaceEdit4.PaintWindow(DC: HDC);
const
  PaintMessage: array[Boolean] of Cardinal = (WM_PAINT, WM_PRINTCLIENT);
var
  Message: TMessage;
  R: TRect;
  NewRect: TRect;
  ButtonCaption: string;
  LocalGrid: TCustomRbwDataGrid;
  ButtonBmp: TBitmap;
  MaskBmp: TBitmap;
  TextSize: TSize;
  TempDrawing: boolean;
begin
  LocalGrid := (Grid as TCustomRbwDataGrid);
  {$IFDEF Delphi_2009_UP}
  LocalGrid.DrawCellBackground(LocalGrid.CellRect(LocalGrid.Col, LocalGrid.Row),
    LocalGrid.Color, [], LocalGrid.Col, LocalGrid.Row);
  {$ENDIF}

  if EditStyle <> esEllipsis then
  begin
    inherited;
  end
  else
  begin
    TempDrawing := LocalGrid.Drawing;
    try
      LocalGrid.FDrawing := True;
      if LocalGrid.SelectCell(LocalGrid.Col, LocalGrid.Row) then
      begin
        ButtonBmp := TBitMap.Create;
        MaskBmp := TBitMap.Create;
        try
          MaskBmp.Monochrome := True;

          R := ButtonRect;

          ButtonBmp.Width := R.Right - R.Left;
          ButtonBmp.Height := R.Bottom - R.Top;
          MaskBmp.Width := ButtonBmp.Width;
          MaskBmp.Height := ButtonBmp.Height;

          MaskBmp.Canvas.Brush.Color := clBlack;
          MaskBmp.Canvas.FillRect(Rect(0,0, MaskBmp.Width, MaskBmp.Height));

          NewRect := DrawButtonFace(ButtonBmp.Canvas,
            Rect(0,0, ButtonBmp.Width, ButtonBmp.Height), 1,
            bsAutoDetect, False, Pressed, False);
          ButtonBmp.Canvas.Font := LocalGrid.CollectionItem(
            LocalGrid.Col, LocalGrid.Row).ButtonFont;
          ButtonCaption := GetButtonCaption(LocalGrid.Col, LocalGrid.Row);
          TextSize := ButtonBmp.Canvas.TextExtent(ButtonCaption);
          ButtonBmp.Canvas.TextRect(NewRect,
            ((NewRect.Right + NewRect.Left) div 2) -(TextSize.cx div 2),
            (NewRect.Top + NewRect.Bottom) div 2 - (TextSize.cy div 2),
            ButtonCaption);
          InflateRect(NewRect, -2, -2);
          ButtonBmp.Canvas.DrawFocusRect(NewRect);

          TransparentStretchBlt(DC, R.Left, R.Top, ButtonBmp.Width,
            ButtonBmp.Height, ButtonBmp.Canvas.Handle, 0, 0, ButtonBmp.Width,
            ButtonBmp.Height, MaskBmp.Canvas.Handle, 0, 0);

          ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
        finally
          MaskBmp.Free;
          ButtonBmp.Free;
        end;
      end;
    finally
      LocalGrid.FDrawing := TempDrawing;
    end;
    {$IFDEF Delphi_2009_UP}
    Message.Msg := PaintMessage[csPrintClient in ControlState];
    {$ELSE}
    Message.Msg := PaintMessage[False];
    {$ENDIF}
    Message.WParam := DC;
    Message.LParam := 0;
    Message.Result := 0;
    DefaultHandler(Message);
  end;
end;

{$R+}

procedure TRbwInplaceEdit4.StopTracking;
begin
  if FTracking then
  begin
    TrackButton(-1, -1);
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TRbwInplaceEdit4.TrackButton(X, Y: Integer);
var
  NewState: Boolean;
  R: TRect;
begin
  R := ButtonRect;
  NewState := PtInRect(R, Point(X, Y));
  if Pressed <> NewState then
  begin
    FPressed := NewState;
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TRbwInplaceEdit4.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  with Message do
  if (EditStyle <> esSimple) and OverButton(Point(XPos, YPos)) then
    Exit;
  inherited;
end;

procedure TRbwInplaceEdit4.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := ScreenToClient(P);
  if (EditStyle <> esSimple) and OverButton(P) then
    Windows.SetCursor(LoadCursor(0, idc_Arrow))
  else
    inherited;
end;

function GetRadioButtonBitmap(Checked: TCheckBoxState; Hot : boolean; BgColor : TColor): TBitmap;
const
  CtrlState : array[TCheckBoxState] of integer = (DFCS_BUTTONRADIO,
    DFCS_BUTTONRADIO or DFCS_CHECKED, DFCS_BUTTON3STATE);
// modified from http://delphi.longzu.net/viewthread.php?tid=48097&extra=page%3D76
var
  CBRect : TRect;
  { $IFDEF VER150}
  Details : TThemedElementDetails;
  { $ENDIF}
//  BgOld : TColor;
  ChkBmp : TBitmap;
  ThemeOK : boolean;
  x, {x2,} y : integer;
begin
//  Result := nil;
  try
    Result := TBitmap.Create;
    ChkBmp := TBitmap.Create;
    try
      ThemeOK := False;
      with Result do
      begin
        Width := CheckBoxSize;
        Height := CheckBoxSize;
        with Canvas do
        begin
          Brush.Color := BgColor;
          FillRect(ClipRect);
          ChkBmp.Assign(Result);
          CBRect := ClipRect;
          CBRect.Top := 1;
          CBRect.Left := 1;
          { $IFDEF VER150}
          if SServices.ThemesAvailable then
          begin

            if Checked = cbChecked then
            begin
              if Hot = True then
                Details := SServices.GetElementDetails(tbRadioButtonCheckedHot)
              else
                Details :=
                  SServices.GetElementDetails(tbRadioButtonCheckedNormal);
            end
            else if Checked = cbUnChecked then
            begin
              if Hot = True then
                Details :=
                  SServices.GetElementDetails(tbRadioButtonUncheckedHot)
              else
                Details :=
                  SServices.GetElementDetails(tbRadioButtonUncheckedNormal);
            end
            else
            begin
              if Hot = True then
                Details :=
                  SServices.GetElementDetails(tbRadioButtonUncheckedHot)
              else
                Details :=
                  SServices.GetElementDetails(tbRadioButtonUncheckedNormal);
            end;
            SServices.DrawElement(Handle, Details, CBRect);

            for x := 15 downto 0 do
              for y := 15 downto 0 do
                if ChkBmp.Canvas.Pixels[x, y] <> Pixels[x, y] then
                begin
                  ThemeOK := True;
                  break;
                end;
          end;
          { $ENDIF}
          if ThemeOK = False then
          begin

            CBRect.Left := ClipRect.Left + 2;
            CBRect.Right := ClipRect.Right - 1;
            CBRect.Top := ClipRect.Top + 2;
            CBRect.Bottom := ClipRect.Bottom - 1;
            DrawFrameControl(Handle, CBRect, DFC_BUTTON, CtrlState[Checked]);
          end;
        end;
      end;
    finally
      ChkBmp.Free;
    end;
  finally
  end;
end;


procedure CreateBitmaps;
var
  ARect: TRect;
begin
  FbmpUnchecked := TBitMap.Create;
  with FbmpUnchecked do
  begin
    Width := CheckBoxSize;
    Height := CheckBoxSize;
    PixelFormat := pf8bit;
    Canvas.Brush.Color := clWhite;
    ARect.Top := 1;
    ARect.Left := 1;
    ARect.Right := Succ(CheckBoxSize);
    ARect.Bottom := Succ(CheckBoxSize);
    Canvas.FillRect(ARect);

    Canvas.Pen.Color := clDkGray;
    Canvas.MoveTo(1, Pred(CheckBoxSize));
    Canvas.LineTo(1, 1);
    Canvas.LineTo(Pred(CheckBoxSize), 1);
    Canvas.MoveTo(2, CheckBoxSize - 2);
    Canvas.LineTo(2, 2);
    Canvas.LineTo(CheckBoxSize - 2, 2);

    Canvas.Pen.Color := $C6C6C6;
    Canvas.MoveTo(2, Pred(CheckBoxSize));
    Canvas.LineTo(Pred(CheckBoxSize), Pred(CheckBoxSize));
    Canvas.LineTo(Pred(CheckBoxSize), 2);
  end;
  FbmpChecked := TBitMap.Create;
  with FbmpChecked do
  begin
    Assign(FbmpUnchecked);

    Canvas.Pen.Color := clBlack;
    Canvas.MoveTo(4, 6);
    Canvas.LineTo(6, 8);
    Canvas.LineTo(10, 4);
    Canvas.MoveTo(4, 7);
    Canvas.LineTo(6, 9);
    Canvas.LineTo(10, 5);
  end;
  FBmpGrayed := TBitMap.Create;
  with FBmpGrayed do
  begin
    Assign(FBmpUnchecked);

    Canvas.Brush.Color := clGray;
    ARect.Top := 4;
    ARect.Left := 4;
    ARect.Right := 10;
    ARect.Bottom := 10;
    Canvas.FillRect(ARect);
  end;
  FBmpDisabledUnchecked := TBitMap.Create;
  with FBmpDisabledUnchecked do
  begin
    Assign(FBmpUnchecked);

    Canvas.Brush.Color := $D0D0D0;
    ARect.Top := 2;
    ARect.Left := 2;
    ARect.Right := 12;
    ARect.Bottom := 12;
    Canvas.FillRect(ARect);
  end;
  FBmpDisabledChecked := TBitMap.Create;
  with FBmpDisabledChecked do
  begin
    Assign(FBmpDisabledUnchecked);

    Canvas.Pen.Color := clBlack;
    Canvas.MoveTo(4, 6);
    Canvas.LineTo(6, 8);
    Canvas.LineTo(10, 4);
    Canvas.MoveTo(4, 7);
    Canvas.LineTo(6, 9);
    Canvas.LineTo(10, 5);
  end;
  FBmpDisabledGrayed := TBitMap.Create;
  with FBmpDisabledGrayed do
  begin
    Assign(FBmpDisabledUnchecked);

    Canvas.Brush.Color := clBlack;
    ARect.Top := 4;
    ARect.Left := 4;
    ARect.Right := 10;
    ARect.Bottom := 10;
    Canvas.FillRect(ARect);
  end;

  FBmpRadioUnChecked := GetRadioButtonBitmap(cbUnChecked, False, clWhite);
{
  FBmpRadioUnChecked := TBitMap.Create;
  with FBmpRadioUnChecked do
  begin
    Width := CheckBoxSize;
    Height := CheckBoxSize;
    PixelFormat := pf8bit;
    Canvas.Brush.Color := clWhite;
    ARect.Top := 1;
    ARect.Left := 1;
    ARect.Right := Succ(CheckBoxSize);
    ARect.Bottom := Succ(CheckBoxSize);
    Canvas.FillRect(ARect);

    Canvas.Pen.Color := clBlack;
    Canvas.Ellipse(1, 1, Pred(CheckBoxSize), Pred(CheckBoxSize));
  end;
}

  FBmpRadioChecked := GetRadioButtonBitmap(cbChecked, False, clWhite);

(*  FBmpRadioChecked := TBitMap.Create;
  with FBmpRadioChecked do
  begin
{
    Assign(FBmpRadioUnChecked);
    Canvas.Brush.Color := clBlack;
    Canvas.Ellipse(3, 3, CheckBoxSize-3, CheckBoxSize-3);
}
    Canvas.Pen.Color := clBlack;
    Canvas.Brush.Color := clBlack;
    if not DrawFrameControl(Canvas.Handle, ARect, DFC_BUTTON,
      DFCS_BUTTONRADIO or DFCS_CHECKED)  then
    begin
      Beep;
    end;
  end;
  *)

  FBmpRadioGrayed := TBitMap.Create;
  with FBmpRadioGrayed  do
  begin
    Assign(FBmpRadioUnChecked);
    Canvas.Pen.Color := clDkGray;
    Canvas.Brush.Color := clDkGray;
    Canvas.Ellipse(3, 3, CheckBoxSize-3, CheckBoxSize-3);
  end;

  FBmpDisabledRadioUnChecked := TBitMap.Create;
  with FBmpDisabledRadioUnChecked do
  begin
    Width := CheckBoxSize;
    Height := CheckBoxSize;
    PixelFormat := pf8bit;
    Canvas.Brush.Color := clWhite;
    ARect.Top := 1;
    ARect.Left := 1;
    ARect.Right := Succ(CheckBoxSize);
    ARect.Bottom := Succ(CheckBoxSize);
    Canvas.FillRect(ARect);

    Canvas.Pen.Color := $D0D0D0;
    Canvas.Ellipse(1, 1, Pred(CheckBoxSize), Pred(CheckBoxSize));
  end;

  FBmpDisabledRadioChecked := TBitMap.Create;
  with FBmpDisabledRadioChecked do
  begin
    Assign(FBmpDisabledRadioUnChecked);
    Canvas.Brush.Color := $D0D0D0;
    Canvas.Ellipse(3, 3, CheckBoxSize-3, CheckBoxSize-3);
  end;

  FBmpDisabledRadioGrayed := TBitMap.Create;
  with FBmpDisabledRadioGrayed do
  begin
    Assign(FBmpDisabledRadioUnChecked);
    Canvas.Pen.Color := $D0D0D0;
    Canvas.Brush.Color := clDkGray;
    Canvas.Ellipse(3, 3, CheckBoxSize-3, CheckBoxSize-3);
  end;
end;

function TCustomRBWDataGrid.GetIsUpdating: boolean;
begin
  Result := FUpdateCount > 0;
end;

initialization

CreateBitmaps;

finalization


  FbmpUnchecked.Free;
  FbmpChecked.Free;
  FBmpGrayed.Free;
  FBmpDisabledUnchecked.Free;
  FBmpDisabledChecked.Free;
  FBmpDisabledGrayed.Free;

  FBmpRadioUnChecked.Free;
  FBmpRadioChecked.Free;
  FBmpRadioGrayed.Free;
  FBmpDisabledRadioUnChecked.Free;
  FBmpDisabledRadioChecked.Free;
  FBmpDisabledRadioGrayed.Free;

end.
