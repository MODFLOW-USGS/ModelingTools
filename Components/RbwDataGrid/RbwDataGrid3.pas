// @abstract(@name is used to define two components: @link(TRbwDataGrid3)
// and @link(TRbwRowDataGrid3).)
unit RbwDataGrid3;

interface

uses Windows,
  Types, SysUtils, Classes, Controls, StdCtrls, Grids, Graphics;

type
  // @name specifies the format used for the data in a column.
  TRbwDataFormat = (rcfString, rcfInteger, rcfReal, rcfBoolean, rcfCombo);

  TCustomRBWDataGrid3 = class;
  TRbwDataGrid3 = class;
  TRbwRowDataGrid3 = class;

  {
    @abstract(@name is the type of
    TCustomRBWDataGrid3.@link(TCustomRBWDataGrid3.OnStateChanged))

    @param(Sender = the @link(TCustomRBWDataGrid3).)
    @param(ACol = the column whose checked status has changed.)
    @param(ARow = the row whose checked status has changed.)
    @param(Value = the new TCheckBoxState.)
  }
  TChangeCheckEvent = procedure(Sender: TObject; ACol, ARow: Longint;
    const Value: TCheckBoxState) of object;

  {
    @abstract(@name indicates what the @link(TCustomRBWDataGrid3) is doing.)
    @value(gsDrawing = drawing the grid cells.)
    @value(gsClicking = responding to the user clicking on a grid cell.)
    @value(gsSettingControls = moving the combobox or button to a new location.)
  }
  TRBWGridState = (gsDrawing, gsClicking, gsSettingControls);

  // TRBWGridStates = set of TRBWGridState;
  TRBWGridStates = set of TRBWGridState;

  {
    @abstract(@name is the type of
    TCustomRBWDataGrid3.@link(TCustomRBWDataGrid3.OnColSize))
    @param(Sender = the @link(TCustomRBWDataGrid3).)
    @param(ACol = the column whose width has changed.)
    @param(PriorWidth = the old width of ACol.)
  }
  TColSizeEvent = procedure(Sender: TObject; ACol, PriorWidth: Longint)
    of object;

  {
    @abstract(@name is the type of
    TCustomRBWDataGrid3.@link(TCustomRBWDataGrid3.OnRowMoving)
    and TCustomRBWDataGrid3.@link(TCustomRBWDataGrid3.OnColMoving))

    @param(Sender = the @link(TCustomRBWDataGrid3).)
    @param(Origin = the column or row being moved.)
    @param(Destination = the new position of the column or row being moved.)
    @param(CanMove = Set CanMove to false to prevent the move.)
  }
  TCheckMoveEvent = procedure(Sender: TObject; const Origin,
    Destination: Longint; var CanMove: boolean) of object;

  {
  @abstract(@name defines the behavior of a column or row in a
  @link(TCustomRBWDataGrid3)).
  }
  TCustomRowOrColumn3 = class(TCollectionItem)
  private
    // See @link(ButtonCaption)
    FButtonCaption: string;
    // See @link(ButtonFont)
    FButtonFont: TFont;
    // See @link(ButtonUsed)
    FButtonUsed: boolean;
    // See @link(ButtonWidth)
    FButtonWidth: integer;
    // See @link(CheckMax)
    FCheckMax: boolean;
    // See @link(CheckMin)
    FCheckMin: boolean;
    // See @link(Format)
    FFormat: TRbwDataFormat;
    // See @link(LimitToList)
    FLimitToList: Boolean;
    // See @link(MaxLength)
    FMaxLength: Integer;
    // See @link(PickList)
    FPickList: TStrings;
    // See @link(Max)
    FMax: extended;
    // See @link(Min)
    FMin: extended;
    // See @link(ParentButtonFont)
    FParentButtonFont: boolean;
    // See @link(ButtonFont)
    function GetButtonFont: TFont;
    // See @link(ButtonCaption)
    procedure SetButtonCaption(const Value: string);
    // See @link(ButtonFont)
    procedure SetButtonFont(const Value: TFont);
    // See @link(ButtonUsed)
    procedure SetButtonUsed(const Value: boolean);
    // See @link(ButtonWidth)
    procedure SetButtonWidth(const Value: integer);
    // See @link(CheckRange)
    procedure CheckCell(const ACol, ARow: integer);
    // See @link(Format)
    procedure SetFormat(const Value: TRbwDataFormat);
    // See @link(CheckMax)
    procedure SetCheckMax(const Value: boolean);
    // See @link(CheckMin)
    procedure SetCheckMin(const Value: boolean);
    // See @link(Max)
    procedure SetMax(const Value: extended);
    // See @link(Min)
    procedure SetMin(const Value: extended);
    // See @link(ParentButtonFont)
    procedure SetParentButtonFont(const Value: boolean);
  protected
    // the OnChange event handler for PickList
    procedure PickListChange(Sender: TObject); virtual;
    // See @Link(PickList).
    procedure SetPickList(const Value: TStrings); virtual;
    // CheckRange ensures that the real-number or integer values for a
    // cell are within the limits specified using @link(CheckMin),
    // @link(CheckMax), @link(Min), and @link(Max).
    procedure CheckRange; virtual; abstract;
    // returns the TCustomRBWDataGrid3 that owns the column.
    function GetGrid: TCustomRBWDataGrid3; virtual; abstract;
    // Sets the number of rows or columns in the @link(TCustomRBWDataGrid3).
    // Which depends on whether the grid is a @link(TRbwDataGrid3) or a
    // @link(TRbwRowDataGrid3).
    procedure SetGridCount(const Value: integer); virtual; abstract;
    // @name sets the selected column or row of the @link(TCustomRBWDataGrid3).
    // Which depends on whether the grid is a @link(TRbwDataGrid3) or a
    // @link(TRbwRowDataGrid3).
    function SelectedRowOrColumn: integer; virtual; abstract;
  public
    // If Source is a TCustomRowOrColumn3, Assign copies the values of the
    // Source.
    procedure Assign(Source: TPersistent); override;
    // Creates an instance of the TCustomRowOrColumn3.
    constructor Create(Collection: TCollection); override;
    // Destroys the instance of TCustomRowOrColumn3.
    // Don't call Destroy; call Free instead.
    destructor Destroy; override;
    // returns the @link(TCustomRBWDataGrid3) that owns the column.
    property Grid: TCustomRBWDataGrid3 read GetGrid;
  published
    // @name is the caption on the button that appears if @link(ButtonUsed)
    // is true and the user selects a cell in this column.
    property ButtonCaption: string read FButtonCaption write SetButtonCaption;
    // @name is the font of the button that appears if @link(ButtonUsed)
    // is true
    // and the user selects a cell in this column.  See also
    // @link(ParentButtonFont).
    property ButtonFont: TFont read GetButtonFont write SetButtonFont;
    // @name determines whether a button appears when a user clicks on a
    // cell in this column.
    property ButtonUsed: boolean read FButtonUsed write SetButtonUsed;
    // @name is the width of the button that appears if @link(ButtonUsed)
    // is true
    // and the user selects a cell in this column.
    property ButtonWidth: integer read FButtonWidth write SetButtonWidth;
    // If @link(CheckMax) is true and the @link(Format) specifies
    // a real or integer number,
    // @name determines whether the number may exceed @link(Max).
    property CheckMax: boolean read FCheckMax write SetCheckMax;
    // If @link(CheckMin) is true and the @link(Format) specifies
    // a real or integer number,
    // @name determines whether the number may be less than @link(Min).
    property CheckMin: boolean read FCheckMin write SetCheckMin;
    // @name determines whether a cell must be a real number, integer,
    // boolean value, string, or display a combo-box.
    property Format: TRbwDataFormat read FFormat write SetFormat;
    // If the @link(Format) specifies a combo-box, @name specifies whether the
    // user may enter a value that is not in the list displayed by the
    // combo-box.
    property LimitToList: Boolean read FLimitToList write FLimitToList;
    // If @link(CheckMax) is true and the @link(Format) specifies
    // a real or integer number,
    // @link(CheckMax) determines whether the number may exceed @name.
    property Max: extended read FMax write SetMax;
    // If the @link(Format) specifies a string, @name is the maximum length of
    // that string.
    property MaxLength: Integer read FMaxLength write FMaxLength;
    // If @link(CheckMin) is true and the @link(Format) specifies
    // a real or integer number,
    // @link(CheckMin) determines whether the number may be less than @name.
    property Min: extended read Fmin write Setmin;
    // @name determines whether the font of the button is the same as the
    // font of the @Link(TCustomRBWDataGrid3) or is specified separately
    // in @link(ButtonFont).
    property ParentButtonFont: boolean read FParentButtonFont
      write SetParentButtonFont default True;
    // If a combobox is visible @name will determine what choices
    // are available to the user in it.
    property PickList: TStrings read FPickList write SetPickList;
  end;

  {
  @abstract(@name defines how the behavior of a column in a
  @link(TRbwDataGrid3).)
  }
  TRbwColumn3 = class(TCustomRowOrColumn3)
  private
    // See @link(AutoAdjustColWidths).
    FAutoAdjustColWidths: boolean;
    // See @link(AutoAdjustRowHeights).
    FAutoAdjustRowHeights: boolean;
    // See @link(WordWrapCaptions).
    FWordWrapCaptions: boolean;
    // See @link(WordWrapCells).
    FWordWrapCells: boolean;
    // See @link(CaptionAlignment).
    FCaptionAlignment: TAlignment;
    // See @link(CellAlignment).
    FCellAlignment: TAlignment;
    // @name calls @link(TRbwDataGrid3).@link(TRbwDataGrid3.AdjustColWidths).
    procedure AdjustColWidths;
    // See @link(AutoAdjustColWidths).
    procedure SetAutoAdjustColWidths(const Value: boolean);
    // See @link(WordWrapCaptions).
    procedure SetWordWrapCaptions(const Value: boolean);
    // See @link(AutoAdjustRowHeights).
    procedure SetAutoAdjustRowHeights(const Value: boolean);
    // See @link(CellAlignment).
    procedure SetCaptionAlignment(const Value: TAlignment);
    // See @link(CellAlignment).
    procedure SetCellAlignment(const Value: TAlignment);
    // See @link(WordWrapCells).
    procedure SetWordWrapCells(const Value: boolean);
  protected
    // See @link(TCustomRowOrColumn3.CheckRange).
    procedure CheckRange; override;
    // returns the @link(TRbwDataGrid3) that owns the column.
    function GetGrid: TCustomRBWDataGrid3; override;
    // @name causes the column width to be adjusted if the @link(PickList)
    // is changed and @link(AutoAdjustColWidths) is true.
    //
    // @name is the event handler for PickList.OnChange.
    procedure PickListChange(Sender: TObject); override;
    // See TCustomRowOrColumn3.@link(TCustomRowOrColumn3.SetGridCount).
    procedure SetGridCount(const Value: integer); override;
    // See TCustomRowOrColumn3.@link(TCustomRowOrColumn3.SetPickList).
    procedure SetPickList(const Value: TStrings); override;
    // See TCustomRowOrColumn3.@link(TCustomRowOrColumn3.SelectedRowOrColumn).
    function SelectedRowOrColumn: integer; override;
  public
    // If Source is a TRbwColumn3, Assign copies the values of the
    // Source.
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname.
    constructor Create(Collection: TCollection); override;
  published
    // If @name is true, columns will enlarge if that is required to
    // fully display the text in the column.
    property AutoAdjustColWidths: boolean read FAutoAdjustColWidths
      write SetAutoAdjustColWidths;
    // If @name is true, rows will enlarge if that is required to
    // fully display the text in the rows.
    property AutoAdjustRowHeights: boolean read FAutoAdjustRowHeights
      write SetAutoAdjustRowHeights;
    // @name specifies whether the captions for a column will be
    // left-justified, right-justified, or centered.  The default is centered.
    property CaptionAlignment: TAlignment read FCaptionAlignment
      write SetCaptionAlignment default taCenter;
    // @name specifies whether the text in an ordinary cell will be
    // left-justified, right-justified, or centered.  The default is
    // left-justified.
    property CellAlignment: TAlignment read FCellAlignment
      write SetCellAlignment default taLeftJustify;
    {
    If @name is true, the cells that are captions (Row <= FixedRows)
    will have their text wrapped if required to display
    it.
    }
    property WordWrapCaptions: boolean read FWordWrapCaptions
      write SetWordWrapCaptions;
    {
    If @name is true, the cells that are ordinary cells
    (Row > FixedRows)
    and (Col > FixedCols) will have their text wrapped if required to display
    it.
    }
    property WordWrapCells: boolean read FWordWrapCells
      write SetWordWrapCells;
  end;

  // TRbwColumnClass3 = class of TRbwColumn3;
  TRbwColumnClass3 = class of TRbwColumn3;

  {
  @abstract(@name defines how the behavior of a column in a
  @link(TRbwRowDataGrid3).)
  }
  TRbwRow3 = class(TCustomRowOrColumn3)
  private
    // See @link(AutoAdjustRowHeights).
    FAutoAdjustRowHeights: boolean;
    // See @link(CellAlignment).
    FCellAlignment: TAlignment;
    // See @link(WordWrapCells).
    FWordWrapCells: boolean;
    // See @link(AutoAdjustRowHeights).
    procedure SetAutoAdjustRowHeights(const Value: boolean);
    // See @link(CellAlignment).
    procedure SetCellAlignment(const Value: TAlignment);
    // See @link(WordWrapCells).
    procedure SetWordWrapCells(const Value: boolean);
  protected
    // See TCustomRowOrColumn3.@link(TCustomRowOrColumn3.CheckRange).
    procedure CheckRange; override;
    // See TCustomRowOrColumn3.@link(TCustomRowOrColumn3.GetGrid).
    function GetGrid: TCustomRBWDataGrid3; override;
    // See TCustomRowOrColumn3.@link(TCustomRowOrColumn3.SetGridCount).
    procedure SetGridCount(const Value: integer); override;
    // See TCustomRowOrColumn3.@link(TCustomRowOrColumn3.SelectedRowOrColumn).
    function SelectedRowOrColumn: integer; override;
  published
    // If @name is true, the row will enlarge if that is required to
    // fully display the text in the row.
    property AutoAdjustRowHeights: boolean read FAutoAdjustRowHeights
      write SetAutoAdjustRowHeights;
    // @name specifies whether the text in an ordinary cell will be
    // left-justified, right-justified, or centered.  The default is
    // left-justified.
    property CellAlignment: TAlignment read FCellAlignment write SetCellAlignment default taLeftJustify;
    { TODO : Move property to ancestor? }
    {
    If @name is true, the cells that are ordinary cells
    (Row > FixedRows)
    and (Col > FixedCols) will have their text wrapped if required to display
    it.
    }
    property WordWrapCells: boolean read FWordWrapCells
      write SetWordWrapCells;
  end;

  // TRbwRowClass = class of TRbwRow3;
  TRbwRowClass = class of TRbwRow3;

  // @abstract(@name is a collection of all the columns in
  // a @link(TRbwDataGrid3).)
  TRbwDataGridColumns3 = class(TCollection)
  private
    // See @link(Grid)
    FGrid: TCustomRBWDataGrid3;
    // See @link(Items)
    function GetItems(Index: Integer): TRbwColumn3;
    // See @link(Items)
    procedure SetItems(Index: Integer; const Value: TRbwColumn3);
  protected
    // GetOwner returns the @link(TRbwDataGrid3) that
    // owns the TRbwDataGridColumns3.
    function GetOwner: TPersistent; override;
    // Update invalidates all or part of the grid.
    procedure Update(Item: TCollectionItem); override;
  public
    // @name creates a new column.
    function Add: TRbwColumn3;
    // Call inherited Create and assigns the @link(TRbwDataGrid3).
    constructor Create(Grid: TRbwDataGrid3; ColumnClass: TRbwColumnClass3);
    // The @link(TCustomRBWDataGrid3). that owns the collection
    property Grid: TCustomRBWDataGrid3 read FGrid;
    // @name is used to access individual columns.
    property Items[Index: Integer]: TRbwColumn3 read GetItems
      write SetItems; default;
  end;

  // @abstract(@name is a collection of all the rows in
  // a @link(TRbwRowDataGrid3).)
  TRbwDataGridRows3 = class(TCollection)
  private
    // See @link(Grid)
    FGrid: TCustomRBWDataGrid3;
    // See @link(Items)
    function GetItems(Index: Integer): TRbwRow3;
    // See @link(Items)
    procedure SetItems(Index: Integer; const Value: TRbwRow3);
  protected
    // GetOwner returns the @link(TRbwRowDataGrid3) that owns the
    // TRbwDataGridRows3.
    function GetOwner: TPersistent; override;
    // Update invalidates all or part of the grid.
    procedure Update(Item: TCollectionItem); override;
  public
    // @name create a new row.
    function Add: TRbwRow3;
    // Call inherited Create and assigns the @link(TRbwRowDataGrid3).
    constructor Create(Grid: TRbwRowDataGrid3; RowClass: TRbwRowClass);
    // @name is the @link(TCustomRBWDataGrid3) that owns the collection.
    property Grid: TCustomRBWDataGrid3 read FGrid;
    // @name is used to access individual rows.
    property Items[Index: Integer]: TRbwRow3 read GetItems
      write SetItems; default;
  end;

  // @abstract(@name is the abstract ancestor of @Link(TRbwDataGrid3) and
  // @link(TRbwRowDataGrid3).)
  TCustomRBWDataGrid3 = class(TStringGrid)
  private
    // @name is a bitmap that represents a checkbox
    // that is checked and enabled.
    FBmpChecked: TBitMap;
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
    // @name is a bitmap that represents a checkbox
    // that is unchecked and enabled.
    FBmpUnchecked: TBitMap;
    // @name is a TButton that can appear in a cell if the
    // @link(TCustomRowOrColumn3.ButtonUsed) of the column is True.
    FButton: TButton;
    // @name stores the address of the event handler for the
    // @link(OnButtonClicked) event.
    FButtonClicked: TSetEditEvent;
    // @name is set in @link(SetControls)
    // and is used as the return value
    // or @link(CanEditModify).
    FCanEditModify: boolean;
    // @name is set to inherited SelectCell(ACol, ARow) in
    // @link(SelectCell).  It is then used in @link(SetControls)
    // to control whether a button or a combobox can appear
    // or to determine whether the state of a checkbox
    // should be changed.
    FCanSelect: boolean;
    // FChecked stores a TCheckBoxState of all the
    // cells in the grid. See @link(State).
    FChecked: array of array of TCheckBoxState;
    // See @link(ColorSelectedRowOrColumn).
    FColorSelectedRowOrColumn: boolean;
    // @name is a local copy of the inherited
    // ColWidths property. It is used in calling the
    // @link(OnColSize) event.
    FColWidths: TIntegerDynArray;
    // @name is a TComboBox that can appear in a cell if the
    // @link(TCustomRowOrColumn3.Format) of the column is rcfCombo.
    FCombo: TComboBox;
    // See @link(dgColumn).
    FdgColumn: integer;
    // See @link(dgRow).
    FdgRow: integer;
    // see @link(GridState)
    FGridState: TRBWGridStates;
    // @name stores the address of the event handler for the
    // @link(OnColMoving) event.
    FOnColMoving: TCheckMoveEvent;
    // @name stores the address of the event handler for the
    // @link(OnColSize) event.
    FOnColSize: TColSizeEvent;
    // @name stores the address of the event handler for the
    // @link(OnRowMoving) event.
    FOnRowMoving: TCheckMoveEvent;
    // @link(TCustomRBWDataGrid3) has to remove goEditing from the
    // inherited Options. @name is used to store a copy of the
    // original Options that can include goEditing so the grid
    // can behave properly.
    FOptions: TGridOptions;
    // See @link(SelectedRowOrColumnColor).
    FSelectedRowOrColumnColor: TColor;
    // @name stores the address of the event handler for the
    // @link(OnStateChanged) event.
    FStateChanged: TChangeCheckEvent;
    // See @link(UnselectableColor).
    FUnselectableColor: TColor;
    // @name is used when the number of columns or rows is being
    // changed to prevent infinite recursion.
    Updating: boolean;
    // @name sets the length of @link(FChecked)
    // and @link(FColWidths).
    procedure AdjustCheckedArray;
    // @name adjusts the size of @link(FCombo) and @link(FButton).
    procedure AdjustControls;
    // See @link(CellVisible);
    function GetCellVisible(ACol, ARow: Integer): boolean;
    // See @link(Checked).
    function GetChecked(const ACol, ARow: integer): boolean;
    // See @link(ColCount).
    function GetColCount: integer;
    // See @link(State).
    function GetState(const ACol, ARow: integer): TCheckBoxState;
    // @name replaces commas with periods if DecimalSeparator
    // is a period. Otherwise it replaces periods with DecimalSeparator.
    function LocalizeString(ANumberString: string): string;
    // LocalStrToFloat converts S to a number after
    // first calling @link(LocalizeString) on S.
    function LocalStrToFloat(S: string): Extended;
    // See @link(Cells).  SetCells calls inherited SetCells
    // after first updating @link(FCombo).
    procedure SetCells(ACol, ARow: Integer; const Value: string);
    // See @link(Checked).
    procedure SetChecked(const ACol, ARow: integer; const Value: boolean);
    // See @link(ColCount).
    procedure SetColCount(const Value: integer);
    // See @link(ColorSelectedRowOrColumn).
    procedure SetColorSelectedRowOrColumn(const Value: boolean);
    // @name sets the position and properties of @link(FCombo)
    // and @link(FButton).
    procedure SetControls(X, Y: integer);
    // Sets the inherited version of @link(Options) as
    // well as the local copy (@link(FOptions)).
    procedure SetOptions(const Value: TGridOptions);
    // See @link(SelectedRowOrColumnColor).
    procedure SetSelectedRowOrColumnColor(const Value: TColor);
    // See @link(State).
    procedure SetState(const ACol, ARow: integer;
      const Value: TCheckBoxState);
    // See @link(UnselectableColor).
    procedure SetUnselectableColor(const Value: TColor);
    { Private declarations }
  protected
    // ButtonClicked is the event handler
    // of the @link(FButton) OnClick event.  It calls
    // @link(OnButtonClicked).
    procedure ButtonClicked(Sender: TObject);
    // @name is used to determine whether the user
    // can type in a cell.
    function CanEditModify: Boolean; override;
    // @name calls the inherited @name
    // and then @link(OnColMoving).
    function CheckColumnDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; override;
    // @name calls the inherited @name
    // and then @link(OnRowMoving).
    function CheckRowDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; override;
    // @name calls inherited @name and @link(MoveControls).
    procedure Click; override;
    // @name returns the @link(TCustomRowOrColumn3) for a cell.
    function CollectionItem(const ACol, ARow: Longint): TCustomRowOrColumn3;
      virtual; abstract;
    // calls inherited ColumnMoved and then adjusts the Checked array.
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    // @name calls inherited @name and @link(MoveControls).
    // It also updates @link(FColWidths).
    procedure ColWidthsChanged; override;
    // ComboChange is the event handler for the Combobox OnChange event.
    procedure ComboChange(Sender: TObject);
    // Creates the cell editor.
    function CreateEditor: TInplaceEdit; override;
    // @name calls inherited @name and
    // removes goEditing from inherited @link(Options)
    // if the cell represents a boolean.
    procedure DoEnter; override;
    // DoExit hides the button and combobox and then checks the
    // contents of the last active cell.
    procedure DoExit; override;
    // Shows a check box if the Format is boolean.
    // Disables editing of boolean cells.
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;
    // @name is used to draw a cell when it should have a checkbox in it.
    procedure DrawCheckBoxCell(ACol, ARow: Integer; ARect: TRect;
      AState: TGridDrawState);
    // @name is used to draw a cell when it is the title of a column.
    procedure DrawColumnTitleCell(ACol, ARow: Integer; ARect: TRect;
      AState: TGridDrawState);
    // @name is used to draw a cell when it is not a title for a column and
    // doesn't have a checkbox in it.
    procedure DrawOrdinaryCell(ACol, ARow: Integer; ARect: TRect;
      AState: TGridDrawState);
    // @name is used in drawing text that is word-wrapped
    // or is not left aligned in a column caption.
    function GetCaptionFlags(const ACol, ARow: integer): UINT; virtual; abstract;
    // @name is used in drawing text that is word-wrapped
    // or is not left aligned in an ordinary cell.
    function GetCellFlags(const ACol, ARow: integer): UINT; virtual; abstract;
    // See @link(Cells)
    function GetCells(ACol, ARow: Integer): string;
    // Set the FixedCols and Options when the grid is loaded.
    procedure Loaded; override;
    // MouseDown shows the combo box or button and toggles Checked.
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    // If required, @name moves @link(FCombo) or @link(FButton)
    // to the current cell.
    procedure MoveControls; virtual;
    // @name calls inherited @name and @link(MoveControls).
    procedure RowHeightsChanged; override;
    // Calls the inherited RowMoved and then adjusts Checked.
    procedure RowMoved(FromIndex, ToIndex: Longint); override;
    // @name prevents the user from editing cells if it is False.
    // It also is sets the private variable @link(FCanSelect)
    // which is then used in @link(SetControls)
    // to control whether a button or a combobox can appear
    // or to determine whether the state of a checkbox
    // should be changed.
    //
    // @link(dgColumn) and @link(dgRow) are set in @name.
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    // See @link(dgColumn)
    procedure SetdgColumn(const Value: integer); virtual;
    // See @link(dgRow)
    procedure SetdgRow(const Value: integer); virtual;
    // Validate contents of the cell.
    procedure SetEditText(ACol, ARow: Longint;
      const Value: String); override;
    // @name sets the Parent of @link(FCombo) and @link(FButton) to the
    // parent of the grid.
    procedure SetParent(AParent: TWinControl); override;
    // @name is used in conjunction with DrawCell to determine the color
    // of a cell. @seealso(ColorSelectedRowOrColumn).
    function ShouldColorCellAsSelected(const ACol, ARow: Integer): boolean;
      virtual; abstract;
    // If the combo box or button is visible, their positions are adjusted.
    procedure TopLeftChanged; override;
    { Protected declarations }
  public
    // @name is the text in the cell designated by ACol, ARow.
    property Cells[ACol, ARow: Integer]: string read GetCells write SetCells;
    // @name is used to determine whether the cell
    // designated by ACol and ARow is visible.
    property CellVisible[ACol, ARow: Integer]: boolean read GetCellVisible;
    // Checked can only be true if a Format is boolean.
    property Checked[const ACol, ARow: integer]: boolean read GetChecked
    write SetChecked;
    // initializes grid.
    constructor Create(AOwner: TComponent); override;
    // @name deletes the column in the grid specified by ACol.
    // @link(Checked) is updated.
    procedure DeleteColumn(ACol: Longint); override;
    // @name deletes the row in the grid specified by ARow.
    // @link(Checked) is updated.
    procedure DeleteRow(ARow: Longint); override;
    // @name destroys the @link(TCustomRBWDataGrid3).
    // Do not call @name.  Call Free instead.
    destructor Destroy; override;
    // @name is the last active column.
    property dgColumn: integer read FdgColumn write SetdgColumn;
    // @name is the last active row.
    property dgRow: integer read FdgRow write SetdgRow;
    // Read @name to determine what the grid is doing.
    property GridState: TRBWGridStates read FGridState;
    // Use @name to insert a column at position ACol.
    procedure InsertColumn(ACol: Integer); virtual;
    // Use @name to insert a row at position ARow.
    procedure InsertRow(ARow: Integer); virtual;
    // State is the TCheckBoxState of the cell at ACol, ARow.
    property State[const ACol, ARow: integer]: TCheckBoxState read GetState
    write SetState;
    { Public declarations }
  published
    // @name is the number of columns in the grid.
    property ColCount: integer read GetColCount write SetColCount;
    // @name indicates whether or not the selected row (in @link(TRbwDataGrid3)
    // or selected column (in TRbwRowDataGrid3) will be colored with
    // @link(SelectedRowOrColumnColor).
    Property ColorSelectedRowOrColumn : boolean read FColorSelectedRowOrColumn
      write SetColorSelectedRowOrColumn;
    // @name is called when a button is clicked in a cell.
    property OnButtonClicked: TSetEditEvent read FButtonClicked
      write FButtonClicked;
    // @name is called when a column is moved.
    property OnColMoving: TCheckMoveEvent read FOnColMoving write FOnColMoving;
    // @name is called when a column is resized.
    property OnColSize: TColSizeEvent read FOnColSize write FOnColSize;
    // @name is called when a row is moved.
    property OnRowMoving: TCheckMoveEvent read FOnRowMoving write FOnRowMoving;
    // @name is called when the TCheckBoxState of a cell is changed.
    property OnStateChanged: TChangeCheckEvent read FStateChanged
      write FStateChanged;
    // @name reimplements TStringGrid.Options.  A local copy of @name is
    // stored in @link(FOptions).  Inherited @name is sometimes changed.
    property Options: TGridOptions read FOptions write SetOptions
      default [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
      goRangeSelect];
    // @name is the color of the selected row (in @link(TRbwDataGrid3)
    // or selected column (in TRbwRowDataGrid3).  It is only used if
    // @link(ColorSelectedRowOrColumn) is true.
    property SelectedRowOrColumnColor : TColor read FSelectedRowOrColumnColor
      write SetSelectedRowOrColumnColor;
    // If goEditing is in Options and a cell can not be selected,
    // @name is the color used to draw the background of the cell.
    property UnselectableColor : TColor read FUnselectableColor
      write SetUnselectableColor;
    { Published declarations }
  end;

  // @abstract(TRbwDataGrid3 is a TStringGrid that restricts the data
  // displayed in a
  // cell based on the Format of the @link(TRbwColumn3) associated with the cell.
  // Cells can also display a combobox or button in a cell).
  TRbwDataGrid3 = class(TCustomRBWDataGrid3)
  private
    // See @link(Columns).
    FColumns: TRbwDataGridColumns3;
    // In @link(SetEditText) FCurRow is set to Row.  FCurRow is then used to
    // adjust RowHeight.
    FCurRow: integer;
    // See @link(FixedCols).
    FFixedCols: integer;
    // If  @link(TRbwColumn3.AutoAdjustColWidths) is true, @name is
    // used to change the width of the columns.
    procedure AdjustColWidths(const ACol: integer);
    // If  @link(TRbwColumn3.AutoAdjustRowHeights
    // TRbwColumn3.AutoAdjustRowHeights) is true, @name is
    // used to change the height of the rows.
    procedure AdjustRowHeights(const ARow: integer);
    // @name breaks up the CellCaption into a group of lines that are
    // narrow enough to fit in the cell (unless a single word is too long).
    // The lines are put in CaptionList.
    procedure FillCaptionList(CellCaption: string;
      const CaptionList: TStringList; Width: integer);
    // @name is used to calculate the height of the cell required to fit
    // the text in the cell.
    function RequiredCellHeight(const ACol, ARow: integer): integer;
    // @name is used to calculate the width of the cell required to fit
    // the text in the cell.
    function RequiredCellWidth(const ACol, ARow: integer): integer;
    // See @link(Cells).
    procedure SetCells(ACol, ARow: Integer; const Value: string);
    // See @link(Columns).
    procedure SetColumns(const Value: TRbwDataGridColumns3);
    { Private declarations }
  protected
    // @name returns Columns[ACol];
    function CollectionItem(const ACol, ARow: Longint): TCustomRowOrColumn3;
      override;
    // @name calls inherited ColumnMoved and then adjusts the Checked array.
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    // @name creates the TRbwDataGridColumns3;
    function CreateColumns: TRbwDataGridColumns3; dynamic;
    // @name is used in drawing text that is word-wrapped
    // or is not left aligned in a column caption.
    function GetCaptionFlags(const ACol, ARow: integer): UINT; override;
    // @name is used in drawing text that is word-wrapped
    // or is not left aligned in an ordinary cell.
    function GetCellFlags(const ACol, ARow: integer): UINT; override;
    // See @link(FixedCols).
    function GetFixedCols: integer; virtual;
    // Set the FixedCols and Options when the grid is loaded.
    procedure Loaded; override;
    // See @link(TCustomRBWDataGrid3.SetdgRow TCustomRBWDataGrid3.SetdgRow).
    procedure SetdgRow(const Value: integer); override;
    // @name Validates contents of the cell and may adjust the cell height
    // or width if required.
    procedure SetEditText(ACol, ARow: Longint; const Value: String);
      override;
    // See @link(FixedCols).
    procedure SetFixedCols(const Value: integer); virtual;
    // See @link(TCustomRBWDataGrid3.ShouldColorCellAsSelected
    // TCustomRBWDataGrid3.ShouldColorCellAsSelected).
    function ShouldColorCellAsSelected(const ACol, ARow: Integer): boolean; override;
    // @name adds or removes items in @link(Columns) if the number
    // of columns changes.
    procedure SizeChanged(OldColCount, OldRowCount: Longint); override;
    { Protected declarations }
  public
    // @name is the text in the cell designated by ACol, ARow. The width
    // or height of the cell may be changed if required.
    property Cells[ACol, ARow: Integer]: string read GetCells write SetCells;
    // initializes grid.
    constructor Create(AOwner: TComponent); override;
    // @name uses tabs and line breaks to split CellContents into a series
    // of values which it uses to set Cells and Checked.  ACol and ARow
    // indicates the top left cell into which the values will be put.
    function DistributeText(const ACol, ARow: integer; CellContents: string): boolean;
    // @name destroys the grid.  Don't call @name directly. Call Free instead.
    destructor Destroy; override;
    // Use @name to insert a column at position ACol.
    procedure InsertColumn(ACol: Integer); override;
    { Public declarations }
  published
    // @name allows properties of individual columns to be specified.
    // See @link(TRbwDataGridColumns3).
    property Columns: TRbwDataGridColumns3 read FColumns write SetColumns;
    // @name  is used to update inherited FixedCols during Loaded.
    property FixedCols: integer read GetFixedCols write SetFixedCols;
    { Published declarations }
  end;

  // @abstract(@name specifies how a column is to adjust its width to fit the
  // text in the column.)  See @link(TAutoAdjustColumns3)
  // and @link(TRbwRowDataGrid3).
  TAutoAdjustColumn3 = class(TCollectionItem)
  private
    // See @link(AutoAdjustColWidths).
    FAutoAdjustColWidths: boolean;
    // See @link(CaptionAlignment).
    FCaptionAlignment: TAlignment;
    // See @link(WordWrapCaptions).
    FWordWrapCaptions: boolean;
    // See @link(AutoAdjustColWidths).
    procedure SetAutoAdjustColWidths(const Value: boolean);
    // See @link(CaptionAlignment).
    procedure SetCaptionAlignment(const Value: TAlignment);
    // See @link(WordWrapCaptions).
    procedure SetWordWrapCaptions(const Value: boolean);
  protected
    // See @link(Grid).
    function GetGrid: TCustomRBWDataGrid3;
    // @name sets the number of columns in the grid.
    procedure SetGridCount(const Value: integer);
  public
    // If Source is a @link(TAutoAdjustColumn3), @name copies the values of the
    // Source.
    procedure Assign(Source: TPersistent); override;
    // @name creates and instance of TAutoAdjustColumn3.
    constructor Create(Collection: TCollection); override;
    // @name destroys the current instance. Do not call @name directly.
    // Call Free instead.
    destructor Destroy; override;
    // @name returns the TCustomRBWDataGrid3 that owns the
    // @link(TAutoAdjustColumns3) that is the parent of the current instance.
    property Grid: TCustomRBWDataGrid3 read GetGrid;
  published
    // @name is used to specify whether the width of a column should
    // be automatically enlarged to fit all the text in the column.
    property AutoAdjustColWidths: boolean read FAutoAdjustColWidths
      write SetAutoAdjustColWidths;
    // @name specifies whether the captions for a column will be
    // left-justified, right-justified, or centered.  The default is centered.
    property CaptionAlignment: TAlignment read FCaptionAlignment
      write SetCaptionAlignment default taCenter;
    {
    If @name is true, the cells that are captions (Row <= FixedRows)
    will have their text wrapped if required to display
    it.
    }
    property WordWrapCaptions: boolean read FWordWrapCaptions
      write SetWordWrapCaptions;
  end;

  // TAutoAdjustColumnClass3 = class of TAutoAdjustColumn3;
  TAutoAdjustColumnClass3 = class of TAutoAdjustColumn3;

  // @abstract(@name specifies how columns are to adjust their width to fit the
  // text in the column.)
  TAutoAdjustColumns3 = class(TCollection)
  private
    // @name is the TCustomRBWDataGrid3 that owns the TAutoAdjustColumns3.
    FGrid: TCustomRBWDataGrid3;
    // See @link(Items).
    function GetItems(Index: Integer): TAutoAdjustColumn3;
    // See @link(Items).
    procedure SetItems(Index: Integer; const Value: TAutoAdjustColumn3);
  protected
    // @name returns the grid that owns the TRbwDataGridColumns3.
    function GetOwner: TPersistent; override;
    // @name invalidates the grid or part of it.
    procedure Update(Item: TCollectionItem); override;
  public
    // @name adds a new new @link(TAutoAdjustColumn3).
    function Add: TAutoAdjustColumn3;
    // @name creates a new @link(TAutoAdjustColumn3) at position Index.
    function Insert(Index: Integer): TAutoAdjustColumn3;
    // @name calls inherited Create and assigns the Grid.
    constructor Create(Grid: TCustomRBWDataGrid3;
      ColumnClass: TAutoAdjustColumnClass3);
    // @name is the grid that owns the collection
    property Grid: TCustomRBWDataGrid3 read FGrid;
    // @name accesses individual @link(TAutoAdjustColumn3)s.
    property Items[Index: Integer]: TAutoAdjustColumn3 read GetItems
      write SetItems; default;
  end;

  // @abstract(@name is a TStringGrid that restricts the data displayed in a
  // cell based on the Format of the @link(TRbwRow3) associated with the cell.
  // Cells can also display a combobox or button in a cell).
  TRbwRowDataGrid3 = class(TCustomRBWDataGrid3)
  private
    // See @link(Columns).
    FColumns: TAutoAdjustColumns3;
    // In @link(SetEditText) FCurRow is set to Row.  FCurRow is then used to
    // adjust RowHeight.
    FCurRow: integer;
    // See @link(FixedCols).
    FFixedCols: integer;
    // See @link(FixedRows).
    FFixedRows: integer;
    // See @link(Rows).
    FRows: TRbwDataGridRows3;
    // If  @link(TRbwColumn3.AutoAdjustColWidths) is true, @name is
    // used to change the width of the columns.
    procedure AdjustColWidths(const ACol: integer);
    // If  @link(TRbwRow3.AutoAdjustRowHeights TRbwRow3.AutoAdjustRowHeights)
    // is true, @name is
    // used to change the height of the rows.
    procedure AdjustRowHeights(const ARow: integer);
    // @name breaks up the CellCaption into a group of lines that are
    // narrow enough to fit in the cell (unless a single word is too long).
    // The lines are put in CaptionList.
    procedure FillCaptionList(CellCaption: string;
      const CaptionList: TStringList; Width: integer);
    // See @link(FixedCols).
    function GetFixedCols: integer;
    // @name is used to calculate the height of the cell required to fit
    // the text in the cell.
    function RequiredCellHeight(const ACol, ARow: integer): integer;
    // @name is used to calculate the width of the cell required to fit
    // the text in the cell.
    function RequiredCellWidth(const ACol, ARow: integer): integer;
    // See @link(Cells).
    procedure SetCells(ACol, ARow: Integer; const Value: string);
    // See @link(Columns).
    procedure SetColumns(const Value: TAutoAdjustColumns3);
    // See @link(FixedCols).
    procedure SetFixedCols(const Value: integer);
    // See @link(Rows).
    procedure SetRows(const Value: TRbwDataGridRows3);
  protected
    // @name returns Rows[ARow].
    function CollectionItem(const ACol, ARow: Longint): TCustomRowOrColumn3;
      override;
    // @name adjusts @link(Columns) when the user moves a column.
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    // @name creates and returns an instance of TAutoAdjustColumns3.
    function CreateColumns: TAutoAdjustColumns3; dynamic;
    // @name creates and returns an instance of TRbwDataGridRows3.
    function CreateRows: TRbwDataGridRows3; dynamic;
    // @name is used in drawing text that is word-wrapped
    // or is not left aligned in a column caption.
    function GetCaptionFlags(const ACol, ARow: integer): UINT; override;
    // @name is used in drawing text that is word-wrapped
    // or is not left aligned in an ordinary cell.
    function GetCellFlags(const ACol, ARow: integer): UINT; override;
    // See @link(FixedRows).
    function GetFixedRows: integer; virtual;
    // @name sets the @link(FixedCols) and @link(FixedRows) and calls
    // @link(AdjustColWidths) for each column.
    procedure Loaded; override;
    // @name adjusts @link(Rows) when the user moves a row.
    procedure RowMoved(FromIndex, ToIndex: Longint); override;
    // See @link(TCustomRBWDataGrid3.SetdgColumn TCustomRBWDataGrid3.SetdgColumn).
    procedure SetdgColumn(const Value: integer); override;
    // @name calls inherited @name and then may adjust the column widths
    // and row heights to fit the text.
    procedure SetEditText(ACol, ARow: Longint; const Value: String);
      override;
    // See @link(FixedRows).
    procedure SetFixedRows(const Value: integer); virtual;
    // See @link(TCustomRBWDataGrid3.ShouldColorCellAsSelected
    // TCustomRBWDataGrid3.ShouldColorCellAsSelected).
    function ShouldColorCellAsSelected(const ACol, ARow: Integer): boolean; override;
    // @name adds or removes items in @link(Columns) and @link(Rows)
    // if the number of columns or rows changes.
    procedure SizeChanged(OldColCount, OldRowCount: Longint); override;
  public
    // @name is the text in the cell designated by ACol, ARow. The width
    // of the cell may be changed if required.
    property Cells[ACol, ARow: Integer]: string read GetCells write SetCells;
    // @name initializes grid.
    constructor Create(AOwner: TComponent); override;
    // @name destroys the grid.  Don't call @name directly. Call Free instead.
    destructor Destroy; override;
    // Use @name to insert a column at position ACol.
    procedure InsertColumn(ACol: Integer); override;
    // Use @name to insert a row at position ARow.
    procedure InsertRow(ARow: Integer); override;
  published
    // @name is used to specify if the width of a column should be
    // automatically adjusted to fit its content.
    property Columns: TAutoAdjustColumns3 read FColumns write SetColumns;
    // @name is the number of fixed columns in the grid.
    property FixedCols: integer read GetFixedCols write SetFixedCols;
    // @name is the number of fixed rows in the grid.
    property FixedRows: integer read GetFixedRows write SetFixedRows;
    // @name allows properties of individual rows to be specified.
    // See @link(TRbwDataGridRows3).
    property Rows: TRbwDataGridRows3 read FRows write SetRows;
  end;

//@abstract(@name registers the component with Delphi.)
procedure Register;

implementation

type
  // @abstract(@name is used to gain access to the protected UpdateContents
  // procedure.)
  TRbwInplaceEdit3 = class(TInplaceEdit);

procedure Register;
begin
  RegisterComponents('RBW', [TRbwDataGrid3, TRbwRowDataGrid3]);
end;

{ TRbwColumn3 }

procedure TRbwColumn3.AdjustColWidths;
var
  ACol: integer;
begin
  ACol := Index;
  if ACol >= 0 then
  begin
    (Grid as TRbwDataGrid3).AdjustColWidths(ACol);
  end;
end;

procedure TRbwColumn3.Assign(Source: TPersistent);
begin
  if Source is TRbwColumn3 then
  begin
    if Assigned(Collection) then
      Collection.BeginUpdate;
    try
      AutoAdjustColWidths := TRbwColumn3(Source).AutoAdjustColWidths;
      AutoAdjustRowHeights := TRbwColumn3(Source).AutoAdjustRowHeights;
      CaptionAlignment := TRbwColumn3(Source).CaptionAlignment;
      CellAlignment := TRbwColumn3(Source).CellAlignment;
      WordWrapCaptions := TRbwColumn3(Source).WordWrapCaptions;
      WordWrapCells := TRbwColumn3(Source).WordWrapCells;
      inherited Assign(Source);
    finally
      if Assigned(Collection) then
        Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TRbwColumn3.CheckRange;
var
  ACol, ARow: integer;
  Index: Integer;
  AGrid: TRbwDataGrid3;
begin
  if not CheckMax and not CheckMin then
    Exit;
  AGrid := Grid as TRbwDataGrid3;
  if (Format in [rcfInteger, rcfReal]) and (AGrid <> nil) then
  begin
    ACol := -1;
    for Index := 0 to AGrid.ColCount - 1 do
    begin
      if AGrid.Columns[Index] = self then
      begin
        ACol := Index;
        break;
      end;
    end;
    if ACol > AGrid.FixedCols then
    begin
      for ARow := AGrid.FixedRows to AGrid.RowCount - 1 do
      begin
        CheckCell(ACol, ARow);
      end;
    end;
  end;
end;

constructor TRbwColumn3.Create(Collection: TCollection);
begin
  inherited;
  FCaptionAlignment := taCenter;
end;

function TRbwColumn3.GetGrid: TCustomRBWDataGrid3;
begin
  if Assigned(Collection) and (Collection is TRbwDataGridColumns3) then
    Result := TRbwDataGridColumns3(Collection).Grid
  else
    Result := nil;
end;

procedure TRbwColumn3.PickListChange(Sender: TObject);
begin
  inherited;
  if FAutoAdjustColWidths then
  begin
    AdjustColWidths;
  end;
end;

function TRbwColumn3.SelectedRowOrColumn: integer;
begin
  result := Grid.Col;
end;

procedure TRbwColumn3.SetAutoAdjustColWidths(const Value: boolean);
begin
  if FAutoAdjustColWidths <> Value then
  begin
    FAutoAdjustColWidths := Value;
    if Value then
    begin
      AdjustColWidths;
    end;
  end;
end;

procedure TRbwColumn3.SetAutoAdjustRowHeights(const Value: boolean);
var
  ARow: integer;
begin
  if FAutoAdjustRowHeights <> Value then
  begin
    FAutoAdjustRowHeights := Value;
    if Value then
    begin
      ARow := (Grid as TRbwDataGrid3).FCurRow;
      if ARow >= 0 then
      begin
        (Grid as TRbwDataGrid3).AdjustRowHeights(ARow);
      end;
    end;
  end;
end;

procedure TRbwColumn3.SetCaptionAlignment(const Value: TAlignment);
begin
  FCaptionAlignment := Value;
end;

procedure TRbwColumn3.SetCellAlignment(const Value: TAlignment);
begin
  FCellAlignment := Value;
end;

procedure TRbwColumn3.SetGridCount(const Value: integer);
begin
  Grid.ColCount := Value;
end;

procedure TRbwColumn3.SetPickList(const Value: TStrings);
begin
  inherited;
  if Grid <> nil then
  begin
    (Grid as TRbwDataGrid3).AdjustColWidths(Index);
  end
end;

procedure TRbwColumn3.SetWordWrapCaptions(const Value: boolean);
begin
  FWordWrapCaptions := Value;
  Grid.Invalidate;
end;

procedure TRbwColumn3.SetWordWrapCells(const Value: boolean);
begin
  FWordWrapCells := Value;
  Grid.Invalidate;
end;
{ TRbwDataGridColumns3 }

function TRbwDataGridColumns3.Add: TRbwColumn3;
begin
  Result := TRbwColumn3(inherited Add);
  if not Grid.Updating and not (csLoading in FGrid.ComponentState) then
  begin
    FGrid.Updating := True;
    FGrid.ColCount := Count;
    FGrid.Updating := False;
  end;
end;

constructor TRbwDataGridColumns3.Create(Grid: TRbwDataGrid3;
  ColumnClass: TRbwColumnClass3);
begin
  inherited Create(ColumnClass);
  FGrid := Grid;
end;

function TRbwDataGridColumns3.GetItems(Index: Integer): TRbwColumn3;
begin
  if Index >= 0 then
  begin
    Result := inherited Items[Index] as TRbwColumn3;
  end
  else
  begin
    Result := nil;
  end;
end;

function TRbwDataGridColumns3.GetOwner: TPersistent;
begin
  Result := FGrid;
end;

procedure TRbwDataGridColumns3.SetItems(Index: Integer;
  const Value: TRbwColumn3);
begin
  Items[Index].Assign(Value);
end;

procedure TRbwDataGridColumns3.Update(Item: TCollectionItem);
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

{ TRbwDataGrid3 }

procedure TRbwDataGrid3.ColumnMoved(FromIndex, ToIndex: Integer);
begin
  inherited;
  Columns[FromIndex].Index := ToIndex;
end;

constructor TRbwDataGrid3.Create(AOwner: TComponent);
begin
  inherited;
  FCurRow := -1;
  FColumns := CreateColumns;
  Updating := True;
  while FColumns.Count < ColCount do
  begin
    FColumns.Add;
  end;
  Updating := False;
end;

function TRbwDataGrid3.CreateColumns: TRbwDataGridColumns3;
begin
  Result := TRbwDataGridColumns3.Create(Self, TRbwColumn3);
end;

destructor TRbwDataGrid3.Destroy;
begin
  FColumns.Free;
  inherited;
end;

function TRbwDataGrid3.GetFixedCols: integer;
begin
  result := inherited FixedCols;
end;

procedure TRbwDataGrid3.Loaded;
begin
  inherited;
  FixedCols := FFixedCols;
end;

procedure TRbwDataGrid3.SetColumns(const Value: TRbwDataGridColumns3);
begin
  FColumns.Assign(Value);
end;

procedure TRbwDataGrid3.SetEditText(ACol, ARow: Integer;
  const Value: String);
var
  RequiredWidth: integer;
  RequiredHeight: integer;
begin
  inherited;
  if (ARow >= RowCount) or (ACol >= ColCount) then
    Exit;
  FCurRow := ARow;
  RequiredWidth := RequiredCellWidth(ACol, ARow);
  if RequiredWidth > ColWidths[ACol] then
  begin
    ColWidths[ACol] := RequiredWidth;
  end;
  RequiredHeight := RequiredCellHeight(ACol, ARow);
  if RequiredHeight > RowHeights[ARow] then
  begin
    RowHeights[ARow] := RequiredHeight;
  end;
end;

procedure TRbwDataGrid3.SetFixedCols(const Value: integer);
begin
  FFixedCols := Value;
  inherited FixedCols := Value;
end;

procedure TRbwDataGrid3.SizeChanged(OldColCount, OldRowCount: Integer);
begin
  inherited;
  if not (csLoading in ComponentState)
    and not (csReading in ComponentState)
    and not (csReadingState in ControlState)
    and not (csCreating in ControlState)
    and not Updating then
  begin
    Updating := True;
    while Columns.count > ColCount do
      Columns[ColCount - 1].Free;
    while Columns.count < ColCount do
      Columns.Add;
    Updating := False;
  end;
end;

function TRbwDataGrid3.RequiredCellWidth(const ACol, ARow: integer): integer;
var
  CellList: TStringList;
  CellCaption: string;
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

    if (ARow >= FixedRows)
      and (Columns[ACol].Format in [rcfBoolean, rcfCombo]) then
    begin
      Inc(result, 20);
    end;
  end;
end;

procedure TRbwDataGrid3.AdjustRowHeights(const ARow: integer);
var
  RequiredHeight, TestHeight: integer;
  ColIndex: integer;
begin
  for ColIndex := 0 to ColCount - 1 do
  begin
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

procedure TRbwDataGrid3.AdjustColWidths(const ACol: integer);
var
  Index: integer;
  RequiredWidth, TestWidth: integer;
begin
  if Columns[ACol].AutoAdjustColWidths then
  begin
    RequiredWidth := 0;
    Canvas.Font.Assign(Font);
    for Index := 0 to RowCount - 1 do
    begin
      TestWidth := RequiredCellWidth(ACol, Index);
      if TestWidth > RequiredWidth then
      begin
        RequiredWidth := TestWidth
      end;
    end;
    if Columns[ACol].Format = rcfCombo then
    begin
      for Index := 0 to Columns[ACol].PickList.Count - 1 do
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

procedure TRbwDataGrid3.SetCells(ACol, ARow: Integer; const Value: string);
var
  RequiredWidth, RequiredHeight: integer;
begin
  if (ACol >= 0) and (ARow >= 0) then
  begin
    inherited Cells[ACol, ARow] := Value;
    RequiredWidth := RequiredCellWidth(ACol, ARow);
    if RequiredWidth > ColWidths[ACol] then
    begin
      ColWidths[ACol] := RequiredWidth;
    end;
    RequiredHeight := RequiredCellHeight(ACol, ARow);
    if RequiredHeight > RowHeights[ARow] then
    begin
      RowHeights[ARow] := RequiredHeight;
    end;
  end;
end;

procedure TRbwDataGrid3.InsertColumn(ACol: Integer);
begin
  inherited;
  Columns[Columns.Count - 1].Index := ACol;
end;

procedure TRbwDataGrid3.FillCaptionList(CellCaption: string;
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
    CaptionList.Add(Copy(CellCaption, 1, SpacePosition - 1));
    CellCaption := Trim(Copy(CellCaption, SpacePosition + 1, MAXINT));
    SpacePosition := Pos(' ', CellCaption);
  end;
  CaptionList.Add(CellCaption);
  MaxWidth := Width;
  Canvas.Font := Font;
  for Index := 0 to CaptionList.Count - 1 do
  begin
    if Canvas.TextWidth(CaptionList[Index]) > MaxWidth then
    begin
      MaxWidth := Canvas.TextWidth(CaptionList[Index]);
    end;
  end;
  repeat
    JoinedLine := False;
    for Index := 0 to CaptionList.Count - 2 do
    begin
      NewLine := CaptionList[Index] + ' ' + CaptionList[Index + 1];
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

function TRbwDataGrid3.RequiredCellHeight(const ACol,
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
function TRbwDataGrid3.DistributeText(const ACol, ARow: integer;
  CellContents: string): boolean;
var
  AStringList: TStringList;
  LineIndex: integer;
  AString: string;
  Row: integer;
  NewString: String;
  WordIndex: integer;
  DummyInt: integer;
  DummyReal: double;
  Code: Integer;
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
          if (ACol < ColCount) and (Row < RowCount) then
          begin
            if Columns[ACol].Format = rcfBoolean then
            begin
              Checked[ACol, Row] := False;
            end
            else
            begin
              Cells[ACol, Row] := '';
              SetEditText(ACol, Row, '');
            end;
          end;
        end
        else
        begin
          while Length(AString) > 0 do
          begin
            NewString := ExtractWord(AString);
            if (ACol + WordIndex < ColCount) and (Row < RowCount) then
            begin
              case Columns[ACol + WordIndex].Format of
                rcfString:
                  begin
                    Cells[ACol + WordIndex, Row] := NewString;
                    SetEditText(ACol + WordIndex, Row, NewString);
                  end;
                rcfInteger:
                  begin
                    Val(NewString, DummyInt, Code);
                    if Code = 0 then
                    begin
                      Cells[ACol + WordIndex, Row] := NewString;
                      SetEditText(ACol + WordIndex, Row, NewString);
                    end;
                  end;
                rcfReal:
                  begin
                    Val(NewString, DummyReal, Code);
                    if Code = 0 then
                    begin
                      Cells[ACol + WordIndex, Row] := NewString;
                      SetEditText(ACol + WordIndex, Row, NewString);
                    end;
                  end;
                rcfBoolean:
                  begin
                    If Trim(UpperCase(NewString)) = 'TRUE' then
                    begin
                        Checked[ACol + WordIndex, Row] := True;
                    end
                    else If Trim(UpperCase(NewString)) = 'False' then
                    begin
                        Checked[ACol + WordIndex, Row] := False;
                    end
                    else
                    begin
                      Val(NewString, DummyReal, Code);
                      if Code = 0 then
                      begin
                        Checked[ACol + WordIndex, Row] := DummyReal <> 0;
                      end
                      else
                      begin
                        Checked[ACol + WordIndex, Row] := False;
                      end;
                    end;
                  end;
                rcfCombo:
                  begin
                    if Columns[ACol + WordIndex].LimitToList then
                    begin
                      if Columns[ACol + WordIndex].
                        PickList.IndexOf(NewString) >= 0 then
                      begin
                        Cells[ACol + WordIndex, Row] := NewString;
                        SetEditText(ACol + WordIndex, Row, NewString);
                      end
                      else
                      begin
                        Cells[ACol + WordIndex, Row] := '';
                        SetEditText(ACol + WordIndex, Row, '');
                      end;
                    end
                    else
                    begin
                      Cells[ACol + WordIndex, Row] := NewString;
                      SetEditText(ACol + WordIndex, Row, NewString);
                    end;
                  end;
              else
                Assert(False);
              end;
            end;
            Inc(WordIndex);
          end;
        end;
      end;
    end;
  finally
    AStringList.Free;
  end;
end;

function TRbwDataGrid3.GetCellFlags(const ACol, ARow: integer): UINT;
begin
  result := DT_NOPREFIX;
  case Columns[ACol].CellAlignment of
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

  if Columns[ACol].WordWrapCells then
  begin
    result := result or DT_WORDBREAK;
  end;
end;

function TRbwDataGrid3.ShouldColorCellAsSelected(const ACol,
  ARow: Integer): boolean;
begin
  result := ColorSelectedRowOrColumn and (ARow = dgRow);
end;

procedure TRbwDataGrid3.SetdgRow(const Value: integer);
var
  ShouldInvalidate: boolean;
begin
  ShouldInvalidate := Value <> dgRow;
  inherited;
  if ShouldInvalidate then
  begin
    Invalidate;
  end;
end;

function TRbwDataGrid3.GetCaptionFlags(const ACol, ARow: integer): UINT;
begin
  result := DT_NOPREFIX;
  case Columns[ACol].CaptionAlignment of
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

  if Columns[ACol].WordWrapCaptions then
  begin
    result := result or DT_WORDBREAK;
  end;
end;

{ TCustomRowOrColumn3 }

procedure TCustomRowOrColumn3.Assign(Source: TPersistent);
begin
  if Source is TCustomRowOrColumn3 then
  begin
    if Assigned(Collection) then
      Collection.BeginUpdate;
    try
      ButtonCaption := TCustomRowOrColumn3(Source).ButtonCaption;
      ButtonFont := TCustomRowOrColumn3(Source).ButtonFont;
      ParentButtonFont := TCustomRowOrColumn3(Source).ParentButtonFont;
      ButtonUsed := TCustomRowOrColumn3(Source).ButtonUsed;
      ButtonWidth := TCustomRowOrColumn3(Source).ButtonWidth;
      LimitToList := TCustomRowOrColumn3(Source).LimitToList;
      MaxLength := TCustomRowOrColumn3(Source).MaxLength;
      Format := TCustomRowOrColumn3(Source).Format;
      PickList := TCustomRowOrColumn3(Source).PickList;
      Max := TCustomRowOrColumn3(Source).Max;
      Min := TCustomRowOrColumn3(Source).Min;
      CheckMax := TCustomRowOrColumn3(Source).CheckMax;
      CheckMin := TCustomRowOrColumn3(Source).CheckMin;
    finally
      if Assigned(Collection) then
        Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TCustomRowOrColumn3.CheckCell(const ACol, ARow: integer);
var
  IntValue: integer;
  RealValue: extended;
  AGrid: TCustomRBWDataGrid3;
  CellValue: string;
begin
  if not CheckMax and not CheckMin then
    Exit;
  if not (Format in [rcfInteger, rcfReal]) then
    Exit;
  AGrid := Grid;
  if (ACol < AGrid.FixedCols) or (ARow < AGrid.FixedRows) then
    Exit;
  CellValue := AGrid.Cells[ACol, ARow];
  if (CellValue = '') then
    Exit;
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

constructor TCustomRowOrColumn3.Create(Collection: TCollection);
begin
  inherited;
  FPickList := TStringList.Create;
  TStringList(FPickList).OnChange := PickListChange;
  FButtonFont := TFont.Create;
  FParentButtonFont := True;
  ButtonCaption := '...';
  ButtonWidth := 25;
  if not Grid.Updating and not (csLoading in Grid.ComponentState) then
  begin
    Grid.Updating := True;
    SetGridCount(Collection.Count);
    Grid.Updating := False;
  end;
end;

destructor TCustomRowOrColumn3.Destroy;
begin
  FPickList.Free;
  FButtonFont.Free;
  if not Grid.Updating and ([csLoading, csDestroying] * Grid.ComponentState = [])
    then
  begin
    Grid.Updating := True;
    SetGridCount(Collection.Count - 1);
    Grid.Updating := False;
  end;
  inherited;
end;

function TCustomRowOrColumn3.GetButtonFont: TFont;
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

procedure TCustomRowOrColumn3.PickListChange(Sender: TObject);
begin

end;

procedure TCustomRowOrColumn3.SetButtonCaption(const Value: string);
begin
  FButtonCaption := Value;
  if Grid.FButton.Visible and (Index = SelectedRowOrColumn) then
  begin
    Grid.FButton.Caption := Value;
  end;
end;

procedure TCustomRowOrColumn3.SetButtonFont(const Value: TFont);
begin
  FButtonFont.Assign(Value);
  if Grid.FButton.Visible and (Index = SelectedRowOrColumn)
    and not ParentButtonFont then
  begin
    Grid.FButton.Font := Value;
  end;
end;

procedure TCustomRowOrColumn3.SetButtonUsed(const Value: boolean);
begin
  if FButtonUsed <> Value then
  begin
    FButtonUsed := Value;
    if FButtonUsed and (Format = rcfCombo) then
    begin
      Format := rcfString;
    end;
    Changed(False);
  end;
end;

procedure TCustomRowOrColumn3.SetButtonWidth(const Value: integer);
begin
  FButtonWidth := Value;
  if Grid.FButton.Visible and (Index = SelectedRowOrColumn) then
  begin
    Grid.FButton.Width := Value;
  end;
end;

procedure TCustomRowOrColumn3.SetCheckMax(const Value: boolean);
begin
  if (FCheckMax <> Value) then
    FCheckMax := Value;
  if FCheckMax then
    CheckRange;
end;

procedure TCustomRowOrColumn3.SetCheckMin(const Value: boolean);
begin
  if (FCheckMin <> Value) then
    FCheckMin := Value;
  if FCheckMin then
    CheckRange;
end;

procedure TCustomRowOrColumn3.SetFormat(const Value: TRbwDataFormat);
begin
  if Value = FFormat then
    Exit;
  FFormat := Value;
  if FFormat = rcfCombo then
  begin
    ButtonUsed := False;
  end;

  Changed(False);
end;

procedure TCustomRowOrColumn3.SetMax(const Value: extended);
begin
  if FMax <> Value then
  begin
    if Value >= FMin then
    begin
      FMax := Value;
      if CheckMax then
      begin
        CheckRange;
      end;
    end
    else
    begin
      FMin := Value;
      FMax := Value;
      if CheckMax or CheckMin then
      begin
        CheckRange;
      end;
    end;
  end;
end;

procedure TCustomRowOrColumn3.SetMin(const Value: extended);
begin
  if FMin <> Value then
  begin
    if Value <= FMax then
    begin
      FMin := Value;
      if CheckMin then
      begin
        CheckRange;
      end;
    end
    else
    begin
      FMin := Value;
      FMax := Value;
      if CheckMax or CheckMin then
      begin
        CheckRange;
      end;
    end;
  end;
end;

procedure TCustomRowOrColumn3.SetParentButtonFont(const Value: boolean);
begin
  FParentButtonFont := Value;
  if ParentButtonFont then
  begin
    ButtonFont := Grid.Font;
  end;
end;

procedure TCustomRowOrColumn3.SetPickList(const Value: TStrings);
begin
  FPickList.Assign(Value);
end;



{ TCustomRBWDataGrid3 }

procedure TCustomRBWDataGrid3.AdjustCheckedArray;
var
  Index: integer;
begin
  if ColCount > 0 then
  begin
    if (Length(FChecked) < ColCount)
      or (Length(FChecked[0]) < RowCount) then
    begin
      SetLength(FChecked, ColCount, RowCount);
    end;
    if length(FColWidths) <> ColCount then
    begin
      SetLength(FColWidths, ColCount);
      for Index := 0 to ColCount - 1 do
      begin
        FColWidths[Index] := ColWidths[Index];
      end;
    end;
  end;
end;

procedure TCustomRBWDataGrid3.AdjustControls;
var
  ARect: TRect;
  RowOrCol: TCustomRowOrColumn3;
begin
  if (Col >= 0) and (FCombo <> nil) and (FButton <> nil) then
  begin
    RowOrCol := CollectionItem(Col, Row);
    if FCombo.Visible then
    begin
      ARect := CellRect(Col, Row);
      FCombo.Left := ARect.Left;
      FCombo.Top := ARect.Top;
      FCombo.Width := ARect.Right - ARect.Left;
      FCombo.Height := ARect.Bottom - ARect.Top;
      if (FCombo.Left < 0) or (FCombo.Top < 0) or
        (FCombo.Top + FCombo.Height > Height)
        or (FCombo.Left + FCombo.Width > Width) then
      begin
        FCombo.Visible := False;
      end;
      FCombo.Font := Font;
    end;
    if FButton.Visible then
    begin
      ARect := CellRect(Col, Row);
      FButton.Left := ARect.Left + Left + 2;
      FButton.Top := ARect.Top + Top + 2;
      FButton.Width := ARect.Right - ARect.Left;
      if FButton.Width > RowOrCol.ButtonWidth then
      begin
        FButton.Width := RowOrCol.ButtonWidth;
        FButton.Left := ARect.Right - FButton.Width + Left + 2;
      end;
      FButton.Height := ARect.Bottom - ARect.Top;
      if (FButton.Left < 0) or (FButton.Top < 0) or
        (FButton.Top + FButton.Height > Height)
        or (FButton.Left + FButton.Width > Width) then
      begin
        FButton.Visible := False;
      end;
    end;
  end;
end;

{procedure TCustomRBWDataGrid3.BoundsChanged;
begin
  inherited;
  AdjustControls;
end;   }

procedure TCustomRBWDataGrid3.ButtonClicked(Sender: TObject);
begin
  if Assigned(FButtonClicked) then
  begin
    FButtonClicked(self, Col, Row, Cells[Col, Row]);
  end;
end;

function TCustomRBWDataGrid3.CanEditModify: Boolean;
begin
  result := inherited CanEditModify;
  if result then
  begin
    result := FCanEditModify;
    if result and (Col >= 0) then
    begin
      result := CollectionItem(Col, Row).Format <> rcfBoolean;
      EditorMode := result;
    end;
  end;
end;

procedure TCustomRBWDataGrid3.Click;
begin
  include(FGridState, gsClicking);
  try
    inherited;
    MoveControls;
  finally
    exclude(FGridState, gsClicking);
  end;
end;

procedure TCustomRBWDataGrid3.ColumnMoved(FromIndex, ToIndex: Integer);
var
  Temp: array of TCheckBoxState;
  CheckIndex: integer;
begin
  inherited;
  if FromIndex = ToIndex then
    Exit;
  AdjustCheckedArray;
  SetLength(Temp, Length(FChecked[FromIndex]));
  for CheckIndex := 0 to Length(Temp) - 1 do
  begin
    Temp[CheckIndex] := FChecked[FromIndex, CheckIndex];
  end;
  if FromIndex < ToIndex then
  begin
    for CheckIndex := FromIndex + 1 to ToIndex do
    begin
      FChecked[CheckIndex - 1] := FChecked[CheckIndex];
    end;
  end
  else
  begin
    for CheckIndex := FromIndex - 1 downto ToIndex do
    begin
      FChecked[CheckIndex + 1] := FChecked[CheckIndex];
    end;
  end;
  SetLength(FChecked[ToIndex], Length(Temp));
  for CheckIndex := 0 to Length(Temp) - 1 do
  begin
    FChecked[ToIndex, CheckIndex] := Temp[CheckIndex];
  end;
end;

procedure TCustomRBWDataGrid3.ColWidthsChanged;
var
  Index: integer;
  ResizedCols: TIntegerDynArray;
  OldGridState: TRBWGridStates;
begin
  inherited;
  OldGridState := GridState;
  try
    FGridState := GridState + [gsDrawing];
    MoveControls;
    ResizedCols := FColWidths;
    SetLength(ResizedCols, ColCount);
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
  finally
    FGridState := OldGridState;
  end;
end;

procedure TCustomRBWDataGrid3.ComboChange(Sender: TObject);
begin
  Cells[Col, Row] := FCombo.Text;
  SetEditText(Col, Row, Cells[Col, Row]);
end;

constructor TCustomRBWDataGrid3.Create(AOwner: TComponent);
var
  ARect: TRect;
begin
  inherited;
  FSelectedRowOrColumnColor := clAqua;
  FUnselectableColor := clBtnFace;

//  FButtonColor := clBtnFace;
  FOptions := inherited Options;
  FCanSelect := True;
  FCombo := TComboBox.Create(self);
  FCombo.Parent := self;
  FCombo.Visible := false;
  FCombo.Height := 0;
  FCombo.Width := 0;
  FCombo.OnChange := ComboChange;

  FButton := TButton.Create(self);
  FButton.Parent := Parent;
  FButton.Visible := false;
  FButton.Height := 0;
  FButton.Width := 0;
  FButton.OnClick := ButtonClicked;

  FBmpUnchecked := TBitMap.Create;
  with FBmpUnchecked do
  begin
    Width := 13;
    Height := 13;
    PixelFormat := pf8bit;

    Canvas.Brush.Color := clWhite;
    ARect.Top := 1;
    ARect.Left := 1;
    ARect.Right := 13;
    ARect.Bottom := 13;
    Canvas.FillRect(ARect);

    Canvas.Pen.Color := clDkGray;
    Canvas.MoveTo(0, 12);
    Canvas.LineTo(0, 0);
    Canvas.LineTo(12, 0);
    Canvas.MoveTo(1, 12);
    Canvas.LineTo(1, 1);
    Canvas.LineTo(12, 1);

    Canvas.Pen.Color := $C6C6C6;
    Canvas.MoveTo(2, 12);
    Canvas.LineTo(12, 12);
    Canvas.LineTo(12, 2);
  end;

  FBmpChecked := TBitMap.Create;
  with FBmpChecked do
  begin
    Assign(FBmpUnchecked);

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

    Canvas.Brush.Color := $808080;
    ARect.Top := 3;
    ARect.Left := 3;
    ARect.Right := 11;
    ARect.Bottom := 11;
    Canvas.FillRect(ARect);
  end;

  FBmpDisabledUnchecked := TBitMap.Create;
  with FBmpDisabledUnchecked do
  begin
    Assign(FBmpUnchecked);

    Canvas.Brush.Color := clDkGray;
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

    Canvas.Brush.Color := $808080;
    ARect.Top := 3;
    ARect.Left := 3;
    ARect.Right := 11;
    ARect.Bottom := 11;
    Canvas.FillRect(ARect);

    Canvas.Brush.Color := clBlack;
    ARect.Top := 4;
    ARect.Left := 4;
    ARect.Right := 10;
    ARect.Bottom := 10;
    Canvas.FillRect(ARect);
  end;
end;

function TCustomRBWDataGrid3.CreateEditor: TInplaceEdit;
begin
  Result := TRbwInplaceEdit3.Create(Self);
  (Result as TRbwInplaceEdit3).Font := Font;
end;

procedure TCustomRBWDataGrid3.DeleteColumn(ACol: Integer);
var
  ColIndex, RowIndex: integer;
  OldColCount: integer;
begin
  OldColCount := ColCount;
  inherited;
  for ColIndex := ACol + 1 to OldColCount - 1 do
  begin
    for RowIndex := 0 to RowCount - 1 do
    begin
      Checked[ColIndex - 1, RowIndex] := Checked[ColIndex, RowIndex];
    end;
  end;
  if Col = ACol then
  begin
    FCombo.Visible := False;
    FButton.Visible := False;
    if Col > 0 then
    begin
      Col := Col - 1;
    end;
  end;
end;

procedure TCustomRBWDataGrid3.DeleteRow(ARow: Integer);
var
  ColIndex, RowIndex: integer;
  OldRowCount: integer;
begin
  OldRowCount := RowCount;
  inherited;
  for RowIndex := ARow + 1 to OldRowCount - 1 do
  begin
    for ColIndex := 0 to ColCount - 1 do
    begin
      Checked[ColIndex, RowIndex - 1] := Checked[ColIndex, RowIndex];
    end;
  end;
  if Row = ARow then
  begin
    FCombo.Visible := False;
    FButton.Visible := False;
    if Row > 0 then
    begin
      Row := Row - 1;
    end
  end;
end;

procedure TCustomRBWDataGrid3.InsertColumn(ACol: Integer);
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
  end;

  for ColIndex := ColCount - 1 downto ACol + 1 do
  begin
    for RowIndex := 0 to RowCount - 1 do
    begin
      Checked[ColIndex, RowIndex] := Checked[ColIndex - 1, RowIndex];
    end;
  end;
  for RowIndex := 0 to RowCount - 1 do
  begin
    Checked[ACol, RowIndex] := False;
  end;
end;

procedure TCustomRBWDataGrid3.InsertRow(ARow: Integer);
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
  end;

  for RowIndex := RowCount - 1 downto ARow + 1 do
  begin
    for ColIndex := 0 to ColCount - 1 do
    begin
      Checked[ColIndex, RowIndex] := Checked[ColIndex, RowIndex - 1];
    end;
  end;
  for ColIndex := 0 to ColCount - 1 do
  begin
    Checked[ColIndex, ARow] := False;
  end;
end;

destructor TCustomRBWDataGrid3.Destroy;
begin
  FBmpUnchecked.Free;
  FBmpChecked.Free;
  FBmpGrayed.Free;
  FBmpDisabledChecked.Free;
  FBmpDisabledUnchecked.Free;
  FBmpDisabledGrayed.Free;
  inherited;
end;

procedure TCustomRBWDataGrid3.DoEnter;
begin
  inherited;
  if (Col >= 0) and (Row >= 0)
    and (CollectionItem(Col, Row).Format = rcfBoolean) then
  begin
    inherited Options := Options - [goEditing];
  end;
end;

procedure TCustomRBWDataGrid3.DoExit;
begin
  inherited;
//  FCombo.Visible := False;
  //FButton.Visible := False;
  if (FdgColumn >= 0) and (FdgRow >= 0) then
  begin
    CollectionItem(FdgColumn, FdgRow).CheckCell(FdgColumn, FdgRow);
  end;
end;

procedure TCustomRBWDataGrid3.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
var
  RowOrCol: TCustomRowOrColumn3;
  CanSelect: boolean;
begin
  inherited;
  include(FGridState, gsDrawing);
  try
    Canvas.Font.Assign(Font);

    // When the grid is not selected but the cell is selectd,
    // it is colored dark blue.  Make the text white for
    // better contrast.
    if (gdSelected in AState) and not (gdFocused in AState) then
    begin
      Canvas.Font.Color := clWhite;
    end;

    CanSelect := True;
    if (goEditing in Options) and Assigned(OnSelectCell) then
    begin
      OnSelectCell(self, ACol, ARow, CanSelect);
    end;

    RowOrCol := CollectionItem(ACol, ARow);

    Canvas.Brush.Color := Color;
    if not CanSelect then
    begin
      Canvas.Brush.Color := UnselectableColor;
    end
    else if ShouldColorCellAsSelected(ACol, ARow) then
    begin
      Canvas.Brush.Color := SelectedRowOrColumnColor;
    end;


    if (ARow < FixedRows) then
    begin
      DrawColumnTitleCell(ACol, ARow, ARect, AState);
    end
    else if RowOrCol.Format = rcfBoolean then
    begin
      DrawCheckBoxCell(ACol, ARow, ARect, AState);
    end
    else if (ARow >= FixedRows) and (ACol >= FixedCols) then
    begin
      inherited;
      DrawOrdinaryCell(ACol, ARow, ARect, AState);
    end;

    if Assigned(OnDrawCell) then
    begin
      OnDrawCell(Self, ACol, ARow, ARect, AState);
    end;
  finally
    exclude(FGridState, gsDrawing);
  end;
end;

function TCustomRBWDataGrid3.GetCells(ACol, ARow: Integer): string;
begin
  result := inherited Cells[ACol, ARow];
end;

function TCustomRBWDataGrid3.GetCellVisible(ACol, ARow: Integer): boolean;
var
  DrawInfo: TGridDrawInfo;
begin
  CalcDrawInfo(DrawInfo);
  result := (ACol >= DrawInfo.Horz.FirstGridCell)
    and (ACol <= DrawInfo.Horz.LastFullVisibleCell)
    and (ARow >= DrawInfo.Vert.FirstGridCell)
    and (ARow <= DrawInfo.Vert.LastFullVisibleCell);
end;

function TCustomRBWDataGrid3.GetChecked(const ACol, ARow: integer): boolean;
begin
  AdjustCheckedArray;
  result := FChecked[ACol, ARow] = cbChecked;
end;

function TCustomRBWDataGrid3.GetColCount: integer;
begin
  result := inherited ColCount;
end;

function TCustomRBWDataGrid3.GetState(const ACol,
  ARow: integer): TCheckBoxState;
begin
  AdjustCheckedArray;
  result := FChecked[ACol, ARow];
end;

procedure TCustomRBWDataGrid3.Loaded;
var
  Index: integer;
begin
  inherited;
  FOptions := inherited Options;
  if CollectionItem(Col, Row).Format = rcfCombo then
  begin
    FCombo.Visible := True;
    MoveControls;
  end;
  SetLength(FColWidths, ColCount);
  for Index := 0 to ColCount - 1 do
  begin
    FColWidths[Index] := ColWidths[Index];
  end;
  ColWidthsChanged;
end;

function TCustomRBWDataGrid3.LocalizeString(ANumberString: string): string;
var
  DecimalPosition: integer;
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

function TCustomRBWDataGrid3.LocalStrToFloat(S: string): Extended;
begin
  if (S = '') then
  begin
    result := 0;
    Exit;
  end;
  result := StrToFloat(LocalizeString(S));
end;

procedure TCustomRBWDataGrid3.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  SetControls(X, Y);
end;

procedure TCustomRBWDataGrid3.MoveControls;
var
  Rect: TRect;
begin
  Rect := CellRect(Col, Row);
  SetControls(
    (Rect.Left + Rect.Right) div 2,
    (Rect.Top + Rect.Bottom) div 2);
end;

procedure TCustomRBWDataGrid3.RowHeightsChanged;
var
  OldGridState: TRBWGridStates;
begin
  inherited;
  OldGridState := GridState;
  try
    FGridState := GridState + [gsDrawing];
    MoveControls;
  finally
    FGridState := OldGridState;
  end;
end;

procedure TCustomRBWDataGrid3.RowMoved(FromIndex, ToIndex: Integer);
var
  ColIndex, RowIndex: integer;
  Temp: TCheckBoxState;
begin
  inherited;
  if FromIndex = ToIndex then
    Exit;
  AdjustCheckedArray;
  for ColIndex := 0 to ColCount - 1 do
  begin
    Temp := FChecked[ColIndex, FromIndex];
    if FromIndex < ToIndex then
    begin
      for RowIndex := FromIndex + 1 to ToIndex do
      begin
        FChecked[ColIndex, RowIndex - 1] := FChecked[ColIndex, RowIndex];
      end;
    end
    else
    begin
      for RowIndex := FromIndex - 1 downto ToIndex do
      begin
        FChecked[ColIndex, RowIndex + 1] := FChecked[ColIndex, RowIndex];
      end;
    end;
    FChecked[ColIndex, ToIndex] := Temp;
  end;
end;

function TCustomRBWDataGrid3.SelectCell(ACol, ARow: Integer): Boolean;
var
  RowOrColumn: TCustomRowOrColumn3;
begin
  FCombo.OnChange := nil;
  result := inherited SelectCell(ACol, ARow);
  FCanSelect := Result;
  if result then
  begin
    RowOrColumn := self.CollectionItem(ACol, ARow);
    if not (csDesigning in ComponentState) then
    begin
      inherited Options := Options;
      if RowOrColumn.Format = rcfBoolean then
      begin
        inherited Options := Options - [goEditing];
      end
      else if (RowOrColumn.Format <> rcfCombo)
        or (ARow < FixedRows) or (ACol < FixedCols) then
      begin
        FCombo.Visible := False;
      end;
      if FCombo.Visible then
      begin
        if RowOrColumn.LimitToList then
        begin
          FCombo.Style := csDropDownList;
          FCombo.ItemIndex := FCombo.Items.IndexOf(Cells[ACol, ARow]);
        end
        else
        begin
          FCombo.Style := csDropDown;
          FCombo.Text := Cells[ACol, ARow];
        end;
      end;
    end;
  end;
  dgColumn := ACol;
  dgRow := ARow;
  FCombo.OnChange := ComboChange;
end;

procedure TCustomRBWDataGrid3.SetCells(ACol, ARow: Integer;
  const Value: string);
begin
  if (ACol >= 0) and (ARow >= 0) then
  begin
    if (Col = ACol) and (Row = ARow)
      and FCombo.Visible then
    begin
      case FCombo.Style of
        csDropDown:
          begin
            FCombo.Text := Value;
          end;
        csDropDownList:
          begin
            FCombo.Text := Value;
            FCombo.ItemIndex := FCombo.Items.IndexOf(Value)
          end;
      else
        Assert(False);
      end;
    end;
    inherited Cells[ACol, ARow] := Value;
  end;
end;

procedure TCustomRBWDataGrid3.SetChecked(const ACol, ARow: integer;
  const Value: boolean);
var
  NewState: TCheckBoxState;
begin
  AdjustCheckedArray;
  if Value then
  begin
    NewState := cbChecked;
  end
  else
  begin
    NewState := cbUnchecked;
  end;

  if FChecked[ACol, ARow] <> NewState then
  begin
    State[ACol, ARow] := NewState;
    Invalidate;
  end;
end;

procedure TCustomRBWDataGrid3.SetColCount(const Value: integer);
var
  Index: integer;
begin
  inherited ColCount := Value;
  if length(FColWidths) <> ColCount then
  begin
    SetLength(FColWidths, ColCount);
    for Index := 0 to ColCount - 1 do
    begin
      FColWidths[Index] := ColWidths[Index];
    end;
  end;
end;

//type TButtonCrack = class(TButton);

procedure TCustomRBWDataGrid3.SetControls(X, Y: integer);
var
  ACol, ARow: integer;
  ARect: TRect;
  Offset: integer;
  CanSelectCell: boolean;
  RowOrCol: TCustomRowOrColumn3;
begin
  MouseToCell(X, Y, ACol, ARow);
  dgRow := ARow;
  dgColumn := ACol;
  if (ACol >= 0) and (ACol < ColCount) then
  begin
    RowOrCol := CollectionItem(ACol, ARow);
    CanSelectCell := True;
    if Assigned(OnSelectCell) then
    begin
      include(FGridState, gsSettingControls);
      try
        OnSelectCell(self, ACol, ARow, CanSelectCell);
      finally
        exclude(FGridState, gsSettingControls);
      end;
    end;

    FCanEditModify := FCanSelect
      and (RowOrCol.Format <> rcfCombo)
      and (RowOrCol.Format <> rcfBoolean)
      and (ACol >= FixedCols)
      and (ARow >= FixedRows)
      and (goEditing in Options);
    if (RowOrCol.Format = rcfCombo) and FCanSelect and CanSelectCell
      and (ACol >= FixedCols) and (ARow >= FixedRows) then
    begin
      FCombo.OnChange := nil;
      FCombo.Visible := True;
      FCombo.Items := RowOrCol.PickList;
      if RowOrCol.LimitToList then
      begin
        FCombo.Style := csDropDownList;
        FCombo.ItemIndex := FCombo.Items.IndexOf(Cells[ACol, ARow]);
      end
      else
      begin
        FCombo.Style := csDropDown;
        FCombo.Text := Cells[ACol, ARow];
      end;

      ARect := CellRect(ACol, ARow);
      FCombo.Left := ARect.Left + Left + 2;
      FCombo.Top := ARect.Top + Top + 2;
      FCombo.Width := ARect.Right - ARect.Left;
      if ARect.Bottom - ARect.Top < FCombo.Height then
      begin
        if RowHeights[ARow] < FCombo.Height then
        begin
          RowHeights[ARow] := RowHeights[ARow] + FCombo.Height
            - (ARect.Bottom - ARect.Top);
        end;
      end;
      FCombo.Font := Font;
      FCombo.OnChange := ComboChange;
    end
    else
    begin
      FCombo.Visible := False;
    end;

    if RowOrCol.ButtonUsed and FCanSelect and CanSelectCell
      and (ACol >= FixedCols) and (ARow >= FixedRows) then
    begin
//      TButtonCrack(FButton).Color := FButtonColor;
      FButton.Visible := True;
      ARect := CellRect(ACol, ARow);
      FButton.Left := ARect.Left + Left + 2;
      FButton.Top := ARect.Top + Top + 2;
      FButton.Width := ARect.Right - ARect.Left;
      if FButton.Width > RowOrCol.ButtonWidth then
      begin
        FButton.Width := RowOrCol.ButtonWidth;
        FButton.Left := ARect.Right - FButton.Width + Left + 2;
      end;
      FButton.Height := ARect.Bottom - ARect.Top;
      FButton.Font := RowOrCol.ButtonFont;
      FButton.Caption := RowOrCol.ButtonCaption;
    end
    else
    begin
      FButton.Visible := False;
    end;

    if (RowOrCol.Format = rcfBoolean) and FCanSelect and CanSelectCell
      and (ACol >= FixedCols) and (ARow >= FixedRows) then
    begin
      ARect := CellRect(ACol, ARow);
      Offset := (ARect.Bottom - ARect.Top - 13) div 2;
      if (Y >= ARect.Top + Offset) and
        (Y <= ARect.Bottom - Offset) and
        (X >= ARect.Left + 2) and
        (X <= ARect.Left + 2 + 13) then
      begin
        Checked[ACol, ARow] := not Checked[ACol, ARow];
        Invalidate;
      end;
    end;
  end;
end;

procedure TCustomRBWDataGrid3.SetdgColumn(const Value: integer);
begin
  if (FdgColumn >= 0) and (FdgRow >= 0) then
  begin
    CollectionItem(FdgColumn, FdgRow).CheckCell(FdgColumn, FdgRow);
  end;
  FdgColumn := Value;
end;

procedure TCustomRBWDataGrid3.SetdgRow(const Value: integer);
begin
  if (FdgColumn >= 0) and (FdgRow >= 0) then
  begin
    CollectionItem(FdgColumn, FdgRow).CheckCell(FdgColumn, FdgRow);
  end;
  FdgRow := Value;
end;

procedure TCustomRBWDataGrid3.SetEditText(ACol, ARow: Integer;
  const Value: String);
var
  RowOrColumn: TCustomRowOrColumn3;
  IntValue: integer;
  NewValue: string;
  ValString: PChar;
  ConversionOK: boolean;
  AFloat: double;
  E: integer;
begin
  RowOrColumn := CollectionItem(ACol, ARow);
  case RowOrColumn.Format of
    rcfString:
      begin
        if (RowOrColumn.MaxLength > 0) and (Length(Value) >
          RowOrColumn.MaxLength) then
        begin
          inherited SetEditText(ACol, ARow, Copy(Value, 1,
            RowOrColumn.MaxLength));
          (InplaceEditor as TRbwInplaceEdit3).UpdateContents;
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
            NewValue := Copy(Value, 1, E - 1);
          end;
        end;
        inherited SetEditText(ACol, ARow, NewValue);
        if Value <> NewValue then
        begin
          (InplaceEditor as TRbwInplaceEdit3).UpdateContents;
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
            while not ConversionOK do
            begin
              StrPCopy(ValString, NewValue);
              ConversionOK := TextToFloat(ValString, AFloat, fvExtended);

              if not ConversionOK and (Length(NewValue) > 0) then
              begin
                NewValue := Copy(NewValue, 1, Length(NewValue) - 1);
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
          (InplaceEditor as TRbwInplaceEdit3).UpdateContents;
        end;
      end;
    rcfBoolean:
      begin
        inherited;
      end;
    rcfCombo:
      begin
        if not RowOrColumn.LimitToList and (RowOrColumn.MaxLength > 0)
          and (Length(Value) > RowOrColumn.MaxLength) then
        begin
          inherited SetEditText(ACol, ARow, Copy(Value, 1,
            RowOrColumn.MaxLength));
          (InplaceEditor as TRbwInplaceEdit3).UpdateContents;
        end
        else
        begin
          inherited;
        end;
      end;
  else
    Assert(False);
  end;
end;

procedure TCustomRBWDataGrid3.SetOptions(const Value: TGridOptions);
begin
  FOptions := Value;
  inherited Options := Value;
end;

procedure TCustomRBWDataGrid3.SetState(const ACol, ARow: integer;
  const Value: TCheckBoxState);
begin
  AdjustCheckedArray;
  if FChecked[ACol, ARow] <> Value then
  begin
    FChecked[ACol, ARow] := Value;
    if Assigned(FStateChanged) then
    begin
      FStateChanged(self, ACol, ARow, Value);
    end;
    Invalidate;
  end;
end;

procedure TCustomRBWDataGrid3.TopLeftChanged;
begin
  inherited;
  AdjustControls;
end;

function TRbwDataGrid3.CollectionItem(const ACol,
  ARow: Integer): TCustomRowOrColumn3;
begin
  result := Columns[ACol];
end;

function TCustomRBWDataGrid3.CheckColumnDrag(var Origin,
  Destination: Integer; const MousePt: TPoint): Boolean;
begin
  result := inherited CheckColumnDrag(Origin, Destination, MousePt);
  if Assigned(OnColMoving) then
  begin
    OnColMoving(self, Origin, Destination, result);
  end;
end;

function TCustomRBWDataGrid3.CheckRowDrag(var Origin, Destination: Integer;
  const MousePt: TPoint): Boolean;
begin
  result := inherited CheckRowDrag(Origin, Destination, MousePt);
  if Assigned(OnRowMoving) then
  begin
    OnRowMoving(self, Origin, Destination, result);
  end;
end;

procedure TCustomRBWDataGrid3.DrawCheckBoxCell(ACol, ARow: Integer;
  ARect: TRect; AState: TGridDrawState);
var
  OldStyle : TBrushStyle;
  Dest: TRect;
  TextRect: TRect;
begin
  // draw checkbox;
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
      Canvas.FillRect(ARect);
      if inherited SelectCell(ACol, ARow) then
      begin
        case State[ACol, ARow] of
          cbUnChecked:
            begin
              Canvas.CopyRect(Dest, FBmpUnchecked.canvas, Rect(0,0,14,14));
            end;
          cbChecked:
            begin
              Canvas.CopyRect(Dest, FBmpChecked.canvas, Rect(0,0,14,14));
            end;
          cbGrayed:
            begin
              Canvas.CopyRect(Dest, FBmpGrayed.canvas, Rect(0,0,14,14));
            end;
        else
          Assert(False);
        end;
      end
      else
      begin
        case State[ACol, ARow] of
          cbUnChecked:
            begin
              Canvas.CopyRect(Dest, FBmpDisabledUnchecked.canvas, Rect(0,0,14,14));
            end;
          cbChecked:
            begin
              Canvas.CopyRect(Dest, FBmpDisabledChecked.canvas, Rect(0,0,14,14));
            end;
          cbGrayed:
            begin
              Canvas.CopyRect(Dest, FBmpDisabledGrayed.canvas, Rect(0,0,14,14));
            end;
        else
          Assert(False);
        end;
      end;
    finally
      Canvas.Brush.Style := OldStyle;
    end;
  end;

  Canvas.Font := Font;
  TextRect := ARect;
  TextRect.Left := TextRect.Left + 17;
  DrawText(Canvas.Handle,PChar(Cells[ACol, ARow]),
    Length(Cells[ACol, ARow]),TextRect, GetCellFlags(ACol, ARow));
end;

procedure TCustomRBWDataGrid3.SetParent(AParent: TWinControl);
begin
  inherited;
  if (AParent <> nil) then
  begin
    if (FCombo <> nil) then
      FCombo.Parent := AParent;
    if (FButton <> nil) then
      FButton.Parent := AParent;
    AdjustControls;
  end;
end;

procedure TCustomRBWDataGrid3.SetUnselectableColor(const Value: TColor);
begin
  if FUnselectableColor <> Value then
  begin
    FUnselectableColor := Value;
    Invalidate;
  end;
end;

procedure TCustomRBWDataGrid3.DrawOrdinaryCell(ACol, ARow: Integer;
  ARect: TRect; AState: TGridDrawState);
var
  FontColor : TColor;
begin
  InflateRect(ARect, 1, 1);

  Canvas.Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);

  InflateRect(ARect, -2, -2);

  FontColor := Canvas.Font.Color;
  try
    Canvas.Font.Color := Font.Color;
    DrawText(Canvas.Handle,PChar(Cells[ACol, ARow]),
      Length(Cells[ACol, ARow]),ARect, GetCellFlags(ACol, ARow));
  finally
    Canvas.Font.Color := FontColor;
  end;
end;


procedure TCustomRBWDataGrid3.SetColorSelectedRowOrColumn(
  const Value: boolean);
begin
  if FColorSelectedRowOrColumn <> Value then
  begin
    FColorSelectedRowOrColumn := Value;
    Invalidate;
  end;
end;

procedure TCustomRBWDataGrid3.SetSelectedRowOrColumnColor(
  const Value: TColor);
begin
  if FSelectedRowOrColumnColor <> Value then
  begin
    FSelectedRowOrColumnColor := Value;
    Invalidate;
  end;
end;

procedure TCustomRBWDataGrid3.DrawColumnTitleCell(ACol, ARow: Integer;
  ARect: TRect; AState: TGridDrawState);
var
  FontColor : TColor;
begin
  Canvas.Brush.Color := FixedColor;
  InflateRect(ARect, 1, 1);

  Canvas.Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);

  InflateRect(ARect, -2, -2);

  FontColor := Canvas.Font.Color;
  try
    Canvas.Font.Color := Font.Color;
    DrawText(Canvas.Handle,PChar(Cells[ACol, ARow]),
      Length(Cells[ACol, ARow]),ARect, GetCaptionFlags(ACol, ARow));
  finally
    Canvas.Font.Color := FontColor;
  end;
end;

{ TRbwRow3 }

procedure TRbwRow3.CheckRange;
var
  ACol, ARow: integer;
  Index: Integer;
  AGrid: TRbwRowDataGrid3;
begin
  if not CheckMax and not CheckMin then
    Exit;
  AGrid := Grid as TRbwRowDataGrid3;
  if (Format in [rcfInteger, rcfReal]) and (AGrid <> nil) then
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
    if ARow > AGrid.FixedRows then
    begin
      for ACol := AGrid.FixedCols to AGrid.ColCount - 1 do
      begin
        CheckCell(ACol, ARow);
      end;
    end;
  end;
end;

function TRbwRow3.GetGrid: TCustomRBWDataGrid3;
begin
  if Assigned(Collection) and (Collection is TRbwDataGridRows3) then
    Result := TRbwDataGridRows3(Collection).Grid
  else
    Result := nil;
end;

function TRbwRow3.SelectedRowOrColumn: integer;
begin
  result := Grid.Row;
end;

procedure TRbwRow3.SetAutoAdjustRowHeights(const Value: boolean);
var
  ARow: integer;
begin
  if FAutoAdjustRowHeights <> Value then
  begin
    FAutoAdjustRowHeights := Value;
    if Value then
    begin
      ARow := (Grid as TRbwRowDataGrid3).FCurRow;
      if ARow >= 0 then
      begin
        (Grid as TRbwRowDataGrid3).AdjustRowHeights(ARow);
      end;
    end;
  end;
end;

procedure TRbwRow3.SetCellAlignment(const Value: TAlignment);
begin
  FCellAlignment := Value;
end;

procedure TRbwRow3.SetGridCount(const Value: integer);
begin
  Grid.RowCount := Value;
end;
procedure TRbwRow3.SetWordWrapCells(const Value: boolean);
begin
  FWordWrapCells := Value;
  Grid.Invalidate;
end;

{ TRbwDataGridRows3 }

function TRbwDataGridRows3.Add: TRbwRow3;
begin
  Result := TRbwRow3(inherited Add);
  if not Grid.Updating and not (csLoading in FGrid.ComponentState) then
  begin
    FGrid.Updating := True;
    FGrid.RowCount := Count;
    FGrid.Updating := False;
  end;
end;

constructor TRbwDataGridRows3.Create(Grid: TRbwRowDataGrid3;
  RowClass: TRbwRowClass);
begin
  inherited Create(RowClass);
  FGrid := Grid;
end;

function TRbwDataGridRows3.GetItems(Index: Integer): TRbwRow3;
begin
  if Index >= 0 then
  begin
    Result := inherited Items[Index] as TRbwRow3;
  end
  else
  begin
    Result := nil;
  end;
end;

function TRbwDataGridRows3.GetOwner: TPersistent;
begin
  Result := FGrid;
end;


procedure TRbwDataGridRows3.SetItems(Index: Integer; const Value: TRbwRow3);
begin
  Items[Index].Assign(Value);
end;

procedure TRbwDataGridRows3.Update(Item: TCollectionItem);
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
{ TRbwRowDataGrid3 }

procedure TRbwRowDataGrid3.AdjustColWidths(const ACol: integer);
var
  Index, PickIndex: integer;
  RequiredWidth, TestWidth: integer;
begin

  if (ComponentState = []) and (Columns[ACol].AutoAdjustColWidths) then
  begin
    RequiredWidth := 0;
    Canvas.Font.Assign(Font);
    for Index := 0 to RowCount - 1 do
    begin
      TestWidth := RequiredCellWidth(ACol, Index);
      if TestWidth > RequiredWidth then
      begin
        RequiredWidth := TestWidth
      end;
      if (Rows[Index].Format = rcfCombo) and (ACol >= FixedCols) then
      begin
        for PickIndex := 0 to Rows[Index].PickList.Count - 1 do
        begin
          TestWidth := Canvas.TextWidth(Rows[Index].PickList[PickIndex]) + 24;
          if TestWidth > RequiredWidth then
          begin
            RequiredWidth := TestWidth
          end;
        end;
      end;
    end;

    if ColWidths[ACol] < RequiredWidth then
    begin
      ColWidths[ACol] := RequiredWidth
    end;
  end;
end;

procedure TRbwRowDataGrid3.AdjustRowHeights(const ARow: integer);
var
  RequiredHeight, TestHeight: integer;
  ColIndex: integer;
begin
  if Rows[ARow].AutoAdjustRowHeights then
  begin
    RequiredHeight := 0;
    for ColIndex := 0 to ColCount - 1 do
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
      RowHeights[ARow] := RequiredHeight
    end;
  end;
end;

function TRbwRowDataGrid3.CollectionItem(const ACol,
  ARow: Integer): TCustomRowOrColumn3;
begin
  result := Rows[ARow];
end;

procedure TRbwRowDataGrid3.ColumnMoved(FromIndex, ToIndex: Integer);
begin
  inherited;
  Columns[FromIndex].Index := ToIndex;
end;

constructor TRbwRowDataGrid3.Create(AOwner: TComponent);
begin
  inherited;
  FRows := CreateRows;
  FColumns := CreateColumns;
  Updating := True;
  while FRows.Count < RowCount do
  begin
    FRows.Add;
  end;
  while FColumns.Count < ColCount do
  begin
    FColumns.Add;
  end;
  Updating := False;
end;

function TRbwRowDataGrid3.CreateColumns: TAutoAdjustColumns3;
begin
  result := TAutoAdjustColumns3.Create(self, TAutoAdjustColumn3);
end;

function TRbwRowDataGrid3.CreateRows: TRbwDataGridRows3;
begin
  result := TRbwDataGridRows3.Create(Self, TRbwRow3);
end;

destructor TRbwRowDataGrid3.Destroy;
begin
  FRows.Free;
  FColumns.Free;
  inherited;
end;

procedure TRbwRowDataGrid3.FillCaptionList(CellCaption: string;
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
    CaptionList.Add(Copy(CellCaption, 1, SpacePosition - 1));
    CellCaption := Trim(Copy(CellCaption, SpacePosition + 1, MAXINT));
    SpacePosition := Pos(' ', CellCaption);
  end;
  CaptionList.Add(CellCaption);
  MaxWidth := Width;
  Canvas.Font := Font;
  for Index := 0 to CaptionList.Count - 1 do
  begin
    if Canvas.TextWidth(CaptionList[Index]) > MaxWidth then
    begin
      MaxWidth := Canvas.TextWidth(CaptionList[Index]);
    end;
  end;
  repeat
    JoinedLine := False;
    for Index := 0 to CaptionList.Count - 2 do
    begin
      NewLine := CaptionList[Index] + ' ' + CaptionList[Index + 1];
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

function TRbwRowDataGrid3.GetCaptionFlags(const ACol, ARow: integer): UINT;
begin
  result := DT_NOPREFIX;
  case Columns[ACol].CaptionAlignment of
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

  if Columns[ACol].WordWrapCaptions then
  begin
    result := result or DT_WORDBREAK;
  end;
end;

function TRbwRowDataGrid3.GetCellFlags(const ACol, ARow: integer): UINT;
begin
  result := DT_NOPREFIX;
  case Rows[ARow].CellAlignment of
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

  if Rows[ARow].WordWrapCells then
  begin
    result := result or DT_WORDBREAK;
  end;
end;

function TRbwRowDataGrid3.GetFixedCols: integer;
begin
  result := inherited FixedCols;
end;

function TRbwRowDataGrid3.GetFixedRows: integer;
begin
  result := inherited FixedRows;
end;

procedure TRbwRowDataGrid3.InsertColumn(ACol: Integer);
begin
  inherited;
  Columns[Columns.Count - 1].Index := ACol;
end;

procedure TRbwRowDataGrid3.InsertRow(ARow: Integer);
begin
  inherited;
  Rows[Rows.Count - 1].Index := ARow;
end;

procedure TRbwRowDataGrid3.Loaded;
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

function TRbwRowDataGrid3.RequiredCellHeight(const ACol,
  ARow: integer): integer;
var
  CellList: TStringList;
  CellCaption: string;
begin
  result := 2;
  Canvas.Font.Assign(Font);
  if Rows[ARow].AutoAdjustRowHeights then
  begin
    if ((ARow < FixedRows) and Columns[ACol].WordWrapCaptions)
      or ((ARow >= FixedRows) and Rows[ARow].WordWrapCells) then
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

function TRbwRowDataGrid3.RequiredCellWidth(const ACol,
  ARow: integer): integer;
begin
  result := 0;
  if Columns[ACol].AutoAdjustColWidths then
  begin
    Canvas.Font.Assign(Font);
    result := Canvas.TextWidth(Cells[ACol, ARow]) + 4;
    if (ARow >= FixedRows) and (ACol >= FixedCols)
      and (Rows[ARow].Format in [rcfBoolean, rcfCombo]) then
    begin
      Inc(result, 20);
    end;
  end;
end;

procedure TRbwRowDataGrid3.RowMoved(FromIndex, ToIndex: Integer);
begin
  inherited;
  Rows[FromIndex].Index := ToIndex;
end;

procedure TRbwRowDataGrid3.SetCells(ACol, ARow: Integer;
  const Value: string);
var
  RequiredWidth, RequiredHeight: integer;
begin
  if (ACol >= 0) and (ARow >= 0) then
  begin
    inherited Cells[ACol, ARow] := Value;
    RequiredWidth := RequiredCellWidth(ACol, ARow);
    if RequiredWidth > ColWidths[ACol] then
    begin
      ColWidths[ACol] := RequiredWidth;
    end;
    RequiredHeight := RequiredCellHeight(ACol, ARow);
    if RequiredHeight > RowHeights[ARow] then
    begin
      RowHeights[ARow] := RequiredHeight;
    end;
  end;
end;

procedure TRbwRowDataGrid3.SetColumns(const Value: TAutoAdjustColumns3);
begin
  FColumns.Assign(Value);
end;

procedure TRbwRowDataGrid3.SetdgColumn(const Value: integer);
var
  ShouldInvalidate: boolean;
begin
  ShouldInvalidate := Value <> dgColumn;
  inherited;
  if ShouldInvalidate then
  begin
    Invalidate;
  end;
end;

procedure TRbwRowDataGrid3.SetEditText(ACol, ARow: Integer;
  const Value: String);
var
  RequiredWidth: integer;
  RequiredHeight: integer;
begin
  inherited;
  if (ARow >= RowCount) or (ACol >= ColCount) then
    Exit;
  FCurRow := ARow;
  RequiredWidth := RequiredCellWidth(ACol, ARow);
  if RequiredWidth > ColWidths[ACol] then
  begin
    ColWidths[ACol] := RequiredWidth;
  end;
  RequiredHeight := RequiredCellHeight(ACol, ARow);
  if RequiredHeight > RowHeights[ARow] then
  begin
    RowHeights[ARow] := RequiredHeight;
  end;
end;

procedure TRbwRowDataGrid3.SetFixedCols(const Value: integer);
begin
  FFixedCols := Value;
  inherited FixedCols := Value;
end;

procedure TRbwRowDataGrid3.SetFixedRows(const Value: integer);
begin
  FFixedRows := Value;
  inherited FixedRows := Value;
end;

procedure TRbwRowDataGrid3.SetRows(const Value: TRbwDataGridRows3);
begin
  FRows.Assign(Value);
end;

function TRbwRowDataGrid3.ShouldColorCellAsSelected(const ACol,
  ARow: Integer): boolean;
begin
  result := ColorSelectedRowOrColumn and (ACol = dgColumn);
end;

procedure TRbwRowDataGrid3.SizeChanged(OldColCount, OldRowCount: Integer);
begin
  inherited;
  if not (csLoading in ComponentState)
    and not (csReading in ComponentState)
    and not (csReadingState in ControlState)
    and not (csCreating in ControlState)
    and not Updating then
  begin
    Updating := True;
    while Rows.Count > RowCount do
      Rows[RowCount - 1].Free;
    while Rows.Count < RowCount do
      Rows.Add;
    while Columns.count > ColCount do
      Columns[ColCount - 1].Free;
    while Columns.count < ColCount do
      Columns.Add;
    Updating := False;
  end;
end;
{ TAutoAdjustColumn3 }

procedure TAutoAdjustColumn3.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TAutoAdjustColumn3 then
  begin
    if Assigned(Collection) then
      Collection.BeginUpdate;
    try
      AutoAdjustColWidths := TAutoAdjustColumn3(Source).AutoAdjustColWidths;
    finally
      if Assigned(Collection) then
        Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

constructor TAutoAdjustColumn3.Create(Collection: TCollection);
begin
  inherited;
  FCaptionAlignment := taCenter;
  if not Grid.Updating and not (csLoading in Grid.ComponentState) then
  begin
    Grid.Updating := True;
    SetGridCount(Collection.Count);
    Grid.Updating := False;
  end;
end;

destructor TAutoAdjustColumn3.Destroy;
begin
  if not Grid.Updating and ([csLoading, csDestroying] * Grid.ComponentState = [])
    then
  begin
    Grid.Updating := True;
    SetGridCount(Collection.Count - 1);
    Grid.Updating := False;
  end;
  inherited;
end;

function TAutoAdjustColumn3.GetGrid: TCustomRBWDataGrid3;
begin
  if Assigned(Collection) and (Collection is TAutoAdjustColumns3) then
    Result := TAutoAdjustColumns3(Collection).Grid
  else
    Result := nil;
end;

procedure TAutoAdjustColumn3.SetAutoAdjustColWidths(const Value: boolean);
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
        (Grid as TRbwRowDataGrid3).AdjustColWidths(ACol);
      end;
    end;
  end;
end;

procedure TAutoAdjustColumn3.SetCaptionAlignment(const Value: TAlignment);
begin
  FCaptionAlignment := Value;
end;

procedure TAutoAdjustColumn3.SetGridCount(const Value: integer);
begin
  Grid.ColCount := Value;
end;

procedure TAutoAdjustColumn3.SetWordWrapCaptions(const Value: boolean);
begin
  FWordWrapCaptions := Value;
  Grid.Invalidate;
end;

{ TAutoAdjustColumns3 }

function TAutoAdjustColumns3.Add: TAutoAdjustColumn3;
begin
  Result := TAutoAdjustColumn3(inherited Add);
  if not Grid.Updating and not (csLoading in FGrid.ComponentState) then
  begin
    FGrid.Updating := True;
    FGrid.ColCount := Count;
    FGrid.Updating := False;
  end;
end;

constructor TAutoAdjustColumns3.Create(Grid: TCustomRBWDataGrid3;
  ColumnClass: TAutoAdjustColumnClass3);
begin
  inherited Create(ColumnClass);
  FGrid := Grid;
end;

function TAutoAdjustColumns3.GetItems(Index: Integer): TAutoAdjustColumn3;
begin
  if Index >= 0 then
  begin
    Result := inherited Items[Index] as TAutoAdjustColumn3;
  end
  else
  begin
    Result := nil;
  end;
end;

function TAutoAdjustColumns3.GetOwner: TPersistent;
begin
  Result := FGrid;
end;

function TAutoAdjustColumns3.Insert(Index: Integer): TAutoAdjustColumn3;
begin
  result := TAutoAdjustColumn3(inherited Insert(Index));
  if not Grid.Updating and not (csLoading in FGrid.ComponentState) then
  begin
    FGrid.Updating := True;
    FGrid.ColCount := Count;
    FGrid.Updating := False;
  end;
end;

procedure TAutoAdjustColumns3.SetItems(Index: Integer;
  const Value: TAutoAdjustColumn3);
begin
  Items[Index].Assign(Value);
end;

procedure TAutoAdjustColumns3.Update(Item: TCollectionItem);
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

end.

