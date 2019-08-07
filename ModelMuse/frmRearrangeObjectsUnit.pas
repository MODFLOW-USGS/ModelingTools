{@abstract(The main purpose of @name is to define @link(TfrmRearrangeObjects)
  which is used to change the order of @link(TScreenObject)s.
  The user can also rename them in @name.)}
unit frmRearrangeObjectsUnit;

interface

uses
  System.UITypes, SysUtils, Types, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Grids, frmCustomGoPhastUnit, Buttons,
  UndoItemsScreenObjects;

type
  {@abstract(@name is used to change the order of @link(TScreenObject)s.
    The user can also rename them in @name.)}
  TfrmRearrangeObjects = class(TfrmCustomGoPhast)
    // Clicking @name closes the @classname without doing anything.
    btnCancel: TBitBtn;
    // Clicking @name displays help on the @classname.
    btnHelp: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name displays text that tells how to use @classname.
    lblInstructions: TLabel;
    // @name holds the buttons at the bottom of @classname.
    pnlBottom: TPanel;
    // @name holds @link(lblInstructions) at the top of @classname.
    pnlInstructions: TPanel;
    // @name lists the @link(TScreenObject)s.
    sgObjects: TStringGrid;
    // @name is used to determine whether, all,
    // the visible, or the selected objects are listed.
    rgShow: TRadioGroup;
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
    // @name initializes @classname and calls @link(GetData).
    procedure FormCreate(Sender: TObject); override;
    // @name draws the selected @link(TScreenObject)s with a bold font.
    procedure sgObjectsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    // @name changes the cursor to give a visual
    // indication that a row is being dragged.
    procedure sgObjectsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    // @name changes the cursor to give a visual
    // indication that a row is being dragged.
    procedure sgObjectsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    // @name changes the cursor to give a visual
    // indication that a row is no longer being dragged.
    procedure sgObjectsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    // This prevents the user from renaming deleted @link(TScreenObject)s.
    procedure sgObjectsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rgShowClick(Sender: TObject);
    procedure sgObjectsMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure sgObjectsMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
  private
    // @name is set to true in @link(sgObjectsMouseDown) to indicate that the
    // user is dragging a row to a new position.
    FDraggingRows: boolean;
    // @name retrieves the @link(TScreenObject)s and displays them in
    // @link(sgObjects). If a @link(TScreenObject) has been deleted,
    // the height of its cell will be zero.
    procedure GetData;
    // If the cursor is over the left hand column, the user can
    // drag the rows to rearrange the objects. Use @link(crHandGrab)
    // to indicate that the rows are being rearranged or @link(crHandFlat)
    // to indicate that they can be moved.  Otherwise, just use
    // the arrow cursor.
    procedure SetCursor(const ACol, ARow: integer);
    // @name sets the new order of the @link(TScreenObject)s.  The user
    // can also rename @link(TScreenObject)s.
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses frmGoPhastUnit, ScreenObjectUnit, CursorsFoiledAgain;

resourcestring
  StrObjects = 'Objects';

{$R *.dfm}

procedure TfrmRearrangeObjects.FormCreate(Sender: TObject);
begin
  inherited;
  sgObjects.ColWidths[1] := 300;
  sgObjects.Cells[1, 0] := StrObjects;
  lblInstructions.Width := pnlInstructions.Width - 2*lblInstructions.Left; 
  GetData;
end;

procedure TfrmRearrangeObjects.GetData;
var
  Index: integer;
  AScreenObject: TScreenObject;
  ShowObject: Boolean;
begin
  // Set the size of the table to a large enough size.
  sgObjects.RowCount := frmGoPhast.PhastModel.ScreenObjectCount + 1;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    // get each screen object.
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    // Display the name of each screen object but hide the names of
    // deleted ones.
    ShowObject := True;
    if AScreenObject.Deleted then
    begin
      ShowObject := False;
    end;
    case rgShow.ItemIndex of
      0:
        begin
          // Show all
          // do nothing
        end;
      1:
        begin
          // Show visible objects
          if not AScreenObject.Visible then
          begin
            ShowObject := False;
          end;
        end;
      2:
        begin
          // Show selected objects
          if not AScreenObject.Selected then
          begin
            ShowObject := False;
          end;
        end;
    end;

    if ShowObject then
    begin
      sgObjects.Cells[1, Index + 1] := AScreenObject.Name;
      sgObjects.RowHeights[Index + 1] := sgObjects.DefaultRowHeight;
    end
    else
    begin
      sgObjects.Cells[1, Index + 1] := '(' + AScreenObject.Name + ')';
      sgObjects.RowHeights[Index + 1] := 0;
    end;
    // Store the object so that it gets moved when the row gets moved.
    sgObjects.Objects[1, Index + 1] := AScreenObject;
  end;
end;

procedure TfrmRearrangeObjects.rgShowClick(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmRearrangeObjects.SetData;
var
  Index: integer;
  AScreenObject: TScreenObject;
  Undo: TUndoRearrangeScreenObjects;
begin
  // Create an object that will allow the action to be undone.
  Undo := TUndoRearrangeScreenObjects.Create;
  try
    // store the screen objects in the Undo object.
    for Index := 1 to sgObjects.RowCount - 1 do
    begin
      AScreenObject := sgObjects.Objects[1, Index] as TScreenObject;
      Undo.FNewList.Add(AScreenObject);
      if AScreenObject.Deleted then
      begin
        Undo.FNewNames.Add(AScreenObject.Name);
      end
      else
      begin
        // allow the users to rename objects.
        Undo.FNewNames.Add(TScreenObject.ValidName(sgObjects.Cells[1, Index]));
      end;
    end;
    // Record the selected objects.
    Undo.SetPostSelection;
  except
    Undo.Free;
    raise;
  end;
  // Perform the action.
  frmGoPhast.UndoStack.Submit(Undo);
end;

procedure TfrmRearrangeObjects.btnOKClick(Sender: TObject);
begin
  // Rearrange the objects.
  SetData;
end;

procedure TfrmRearrangeObjects.sgObjectsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  AScreenObject: TScreenObject;
begin
  inherited;
  // This prevents users from renaming deleted screen objects.
  // This may not be required because the heights to cells
  // with deleted screen objects is 0;  The user never sees them.
  AScreenObject := sgObjects.Objects[ACol, ARow] as TScreenObject;
  CanSelect := not AScreenObject.Deleted;
end;

procedure TfrmRearrangeObjects.sgObjectsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Longint;
begin
  inherited;
  // rearranging the cells is handled by the control so it
  // doesn't need to be done here.  However, set the cursor
  // to give a visual indication of what is happening.
  sgObjects.MouseToCell(X, Y, ACol, ARow);
  SetCursor(ACol, ARow);
end;

procedure TfrmRearrangeObjects.SetCursor(const ACol, ARow: integer);
begin
  // If the cursor is over the left hand column, the user can
  // drag the rows to rearrange the objects. Use crHandGrab
  // to indicate that the rows are being rearranged or crHandFlat
  // to indicate that they can be moved.  Otherwise, just use
  // the arrow cursor.
  if (ARow > 0) and (ACol = 0) then
  begin
    if FDraggingRows then
    begin
      sgObjects.Cursor := crHandGrab;
    end
    else
    begin
      sgObjects.Cursor := crHandFlat;
    end;
  end
  else
  begin
    sgObjects.Cursor := crArrow;
  end;
end;

procedure TfrmRearrangeObjects.sgObjectsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Longint;
  NewSelection: TGridRect;
begin
  inherited;
  // rearranging the cells is handled by the control so it
  // doesn't need to be done here.  However, set the cursor
  // to give a visual indication of what is happening.
  sgObjects.MouseToCell(X, Y, ACol, ARow);
  FDraggingRows := (ARow > 0) and (ACol = 0);

  // This keeps the selected cell from being shown as blank.
  if FDraggingRows then
  begin
    NewSelection.Left := -1;
    NewSelection.Right := -1;
    NewSelection.Top := -1;
    NewSelection.Bottom := -1;
    sgObjects.Selection := NewSelection;
  end;

  SetCursor(ACol, ARow);
end;

procedure TfrmRearrangeObjects.sgObjectsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Longint;
begin
  inherited;
  // rearranging the cells is handled by the control so it
  // doesn't need to be done here.  However, set the cursor
  // to give a visual indication of what is happening.
  sgObjects.MouseToCell(X, Y, ACol, ARow);
  FDraggingRows := False;
  SetCursor(ACol, ARow);
end;

procedure TfrmRearrangeObjects.sgObjectsMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  inherited;
  if (sgObjects.Col < 0) or (sgObjects.Row < 0) then
  begin
    Handled := True;
  end;
end;

procedure TfrmRearrangeObjects.sgObjectsMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  inherited;
  if (sgObjects.Col < 0) or (sgObjects.Row < 0) then
  begin
    Handled := True;
  end;
end;

procedure TfrmRearrangeObjects.sgObjectsDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  AScreenObject: TScreenObject;
  AFont: TFont;
begin
  inherited;
  if (ACol = 1) and (ARow > 0) then
  begin
    AScreenObject := sgObjects.Objects[ACol, ARow] as TScreenObject;
    AFont := TFont.Create;
    try
      AFont.Assign(sgObjects.Canvas.Font);
      if AScreenObject.Selected then
      begin
        AFont.Style := sgObjects.Canvas.Font.Style + [fsBold];
      end
      else
      begin
        AFont.Style := sgObjects.Canvas.Font.Style - [fsBold];
      end;
      sgObjects.Canvas.Font.Assign(AFont);
    finally
      AFont.Free;
    end;
    sgObjects.Canvas.FillRect(Rect);
    sgObjects.Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + 2,
      sgObjects.Cells[ACol, ARow]);

  end;
end;

end.

