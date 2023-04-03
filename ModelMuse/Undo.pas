{ This unit contains the base classes for a command pattern-based
  undo Stack.

  Author : @author(Warren Kovach <wlk@kovcomp.co.uk>)
  Published in The Delphi Magazine

 Modified by @author(Richard B. Winston <rbwinst@usgs.gov>)}
unit Undo;

interface

uses
  SysUtils, Classes, Forms, Menus, ActnList, Controls, ComCtrls;

// Use const for version 9.0 and earlier.  Otherwise, use resourcestring.
{$IFDEF MSWINDOWS}
{$IFDEF WIN32}
{$IFDEF VER90}
const
{$ELSE}
resourcestring
{$ENDIF}
{$ELSE}
const
{$ENDIF}
{$ELSE}
resourcestring
{$ENDIF}
  sUndoDescr = 'Undo last action';
  sShortUndoDescr = 'Undo last';
  sRedoDescr = 'Redo last action';
  sShortRedoDescr = 'Redo last';
  sUndoMenu = '&Undo';
  sRedoMenu = '&Redo';
  sNoUndoDescr = 'Command not available; nothing to undo.';
  sNoRedoDescr = 'Command not available; nothing to redo.';
  sStackFull = 'Undo Stack is full; only the last %d actions can be undone';

type
  {@abstract(@name is called when an inherited method of TList is called that
    should never be called.)}
  EHiddenProc = class(Exception);

  {@abstract(@name specifies whether the stack is full or not.)}
  TStackStatus = (ssFull, ssNotFull);

  { @abstract(@name is an abstract interface for a command.
    Descendants define what the command does and how to undo and redo
    the command.)
    This is an ancestral type; different forms can inherit
    this and modify it to meet needs of data on that form. }
  TUndoItem = class(TObject)
  protected
    // @name is a description of what is to be undone.
    function GetUndoDescription: string; virtual;
    // @name is a short description of what is to be undone.
    function GetShortUndoDescription: string; virtual;
    // @name is a description of what is to be redone.
    function GetRedoDescription: string; virtual;
    // @name is a short description of what is to be redone.
    function GetShortRedoDescription: string; virtual;
    // @name is text to display for a menu item that undoes the action.
    function GetUndoMenuText: string; virtual;
    // @name is text to display for a menu item that redoes the action.
    function GetRedoMenuText: string; virtual;
  public
    // @name does the command for the first time.
    procedure DoCommand; virtual; abstract;
    // @name undoes the command.
    procedure Undo; virtual; abstract;
    // @name redoes the command after being undone.
    procedure Redo; virtual; abstract;
    // @name is a description of what is to be undone.
    property UndoDescription: string read GetUndoDescription;
    // @name is a short description of what is to be undone.
    property ShortUndoDescription: string read GetShortUndoDescription;
    // @name is a description of what is to be redone.
    property RedoDescription: string read GetRedoDescription;
    // @name is a short description of what is to be redone.
    property ShortRedoDescription: string read GetShortRedoDescription;
    // @name is text to display for a menu item that undoes the action.
    property UndoMenuText: string read GetUndoMenuText;
    // @name is text to display for a menu item that redoes the action.
    property RedoMenuText: string read GetRedoMenuText;
  end;

  // @abstract(@name is used to manage the @link(TUndoItem)s.)
  // @name allows commands to be undone and then redone when they
  // are encapsulated as @link(TUndoItem)s.
  TUndoStack = class(TList)
  private
    // @name: integer;
    // See @link(MaxItems).
    FMaxItems: integer;
    FUndoToolButton: TToolButton;
    FRedoToolButton: TToolButton;
    FUndoEvent: TNotifyEvent;
    FReDoEvent: TNotifyEvent;
    // @name raises an exception.
    procedure HiddenProcExcept;
    // See @link(MaxItems).
    procedure SetMaxItems(AMaxItems: integer);
    // @name is the item to undo.
    function GetCurrentItem: TUndoItem;
    // @name is the item to redo.
    function GetCurrentRedoItem: TUndoItem;
    // @name returns true if it is possible to undo the last action.
    function CanUndo: boolean;
    // @name returns true if it is possible to redo an action.
    function CanRedo: boolean;
    // @name shows a hint for the next undo.
    procedure UndoClick(Sender: TObject);
    // @name shows a hint for the next redo.
    procedure RedoClick(Sender: TObject);
    procedure UpdateButtons;
  protected
    // @name: integer;
    // @name is the position in the stack to undo or redo.
    FCurrentUndo: integer;
  public
    // @name creates an instance of @classname and sets @link(MaxItems).
    constructor Create(AMaxItems: integer);
    // @name removes all @link(TUndoItem)s from the @classname
    procedure Clear; override;
    // @name deletes and frees the @link(TUndoItem)s at Item from @classname.
    procedure DeleteAndFree(Item: integer);
    // @name adds a @link(TUndoItem)s to the @classname.
    function Submit(Item: TUndoItem): TStackStatus;
    // @name undoes Num @link(TUndoItem)s in the @classname.
    procedure Undo(Num: integer);
    // @name redoes Num @link(TUndoItem)s in the @classname.
    procedure Redo(Num: integer);
    // @name removes the last @link(TUndoItem)from the @classname.
    procedure RemoveLastItem;
    // Set the properties of the TActions used for undoing and redoing things.
    procedure SetUndoActions(const UndoAction, RedoAction: TAction);
    // Set the properties of the TMenuItems used for undoing and redoing things.
    procedure SetUndoMenuItems(const UndoItem, RedoItem: TMenuItem);
    // Set the properties of the TToolButtons used for undoing and redoing things.
    procedure SetUndoToolButtons(const Undo, Redo: TToolButton);
    // @name is the maximum number of items allowed in the @classname.
    property MaxItems: integer read FMaxItems write SetMaxItems;
    // @name is the current TUndoItem for undoing something.
    property CurrentItem: TUndoItem read GetCurrentItem;
    // @name is the current TUndoItem for redoing something.
    property CurrentRedoItem: TUndoItem read GetCurrentRedoItem;
    { disable other access methods }
    // @name raises an exception.
    procedure Delete(Index: Integer);
    // @name raises an exception.
    function Add(Item: Pointer): Integer;
    // @name raises an exception.
    procedure Insert(Index: Integer; const S: string);
    // @name raises an exception.
    procedure Move(CurIndex, NewIndex: Integer);
    // @name raises an exception.
    procedure Exchange(Index1, Index2: Integer);
    property UndoToolButton: TToolButton
      read FUndoToolButton write FUndoToolButton;
    property RedoToolButton: TToolButton
      read FRedoToolButton write FRedoToolButton;
    Property UndoEvent : TNotifyEvent read FUndoEvent;
    property ReDoEvent : TNotifyEvent read FReDoEvent;
  end;

// @abstract(@name disables UndoItem and RedoItem.)
procedure DisableUndoMenus(UndoItem, RedoItem: TMenuItem);
// @abstract(@name disables UndoAction and RedoAction.)
procedure DisableUndoActions(UndoAction, RedoAction: TAction);
// @abstract(@name disables UndoButton and RedoButton.)
procedure DisableUndoToolButtons(UndoButton, RedoButton: TToolButton);

implementation

resourcestring
  StrErrorAccessToSt = 'Error - access to stack only allowed through Submit ' +
  'and Clear';

Type
  // use this to get access to MouseLeave.
  TControlCrack = class(TControl);

function TUndoItem.GetUndoDescription: string;
begin
  Result := sUndoDescr;
end;

function TUndoItem.GetShortUndoDescription: string;
begin
  Result := sShortUndoDescr;
end;

function TUndoItem.GetRedoDescription: string;
begin
  Result := sRedoDescr;
end;

function TUndoItem.GetShortRedoDescription: string;
begin
  Result := sShortRedoDescr;
end;

function TUndoItem.GetUndoMenuText: string;
begin
  Result := sUndoMenu;
end;

function TUndoItem.GetRedoMenuText: string;
begin
  Result := sRedoMenu;
end;
{ -------------------------------------------------- }

constructor TUndoStack.Create(AMaxItems: integer);
begin
  inherited Create;

  FUndoEvent := UndoClick;
  FReDoEvent := RedoClick;
  FMaxItems := AMaxItems;
  if FMaxItems > MaxListSize then
    FMaxItems := MaxListSize;
  FCurrentUndo := -1;
end;

procedure TUndoStack.Clear;
var
  i: Integer;
begin
  for i := pred(Count) downto 0 do
    DeleteAndFree(i);
  //  Count := 0;
  inherited Clear;
  FCurrentUndo := -1;
end;

procedure TUndoStack.DeleteAndFree(Item: integer);
var
  UndoItem: TUndoItem;
begin
  UndoItem := Items[Item];
  inherited Delete(Item);
  UndoItem.Free;
end;

procedure TUndoStack.SetMaxItems(AMaxItems: integer);
var
  i: integer;
begin
  { delete oldest entries if list is shrinking }
  if AMaxItems < 0 then
  begin
    AMaxItems := 0;
  end;
  if AMaxItems < Count then
  begin
    for i := 0 to pred(Count - AMaxItems) do
    begin
      if FCurrentUndo < 0 then
        break;
      DeleteAndFree(0);
      Dec(FCurrentUndo);
    end;
  end;
  if AMaxItems < Count then
  begin
    for i := pred(Count) downto AMaxItems do
    begin
      DeleteAndFree(i);
    end;
  end;
  FMaxItems := AMaxItems;
  //  CurrentUndo := pred(Count);
end;

function TUndoStack.GetCurrentItem: TUndoItem;
begin
  if CanUndo then
    Result := Items[FCurrentUndo]
  else
    Result := nil;
end;

function TUndoStack.GetCurrentRedoItem: TUndoItem;
begin
  if CanRedo then
    Result := Items[succ(FCurrentUndo)]
  else
    Result := nil;
end;

function TUndoStack.CanUndo: boolean;
begin
  Result := (FCurrentUndo >= 0) and (FCurrentUndo < Count);
end;

function TUndoStack.CanRedo: boolean;
begin
  Result := (Count > 0) and (FCurrentUndo < pred(Count));
end;

function TUndoStack.Submit(Item: TUndoItem): TStackStatus;
var
  i: integer;
begin
  Item.DoCommand;
  { Check to see if we have undone one or more commands }
  if CanRedo then
    { if so then get rid of ones in Redo list (those above the pointer) }
    for i := pred(Count) downto succ(FCurrentUndo) do
      DeleteAndFree(i);
  { check if stack is full; if so, pop off oldest command }
  if Count >= MaxItems then
  begin
    DeleteAndFree(0);
    Result := ssFull;
  end
  else
    Result := ssNotFull;
  inherited Add(Item);
  { point at top of stack (the size of which may have been modified
    above; can't just inc(CurrentUndo) }
  FCurrentUndo := pred(Count);
  UpdateButtons;
end;

procedure TUndoStack.Undo(Num: integer);
var
  i: integer;
begin
  try
  if CanUndo then
  begin
     for i := 1 to Num do
     begin
       CurrentItem.Undo;
       dec(FCurrentUndo);
       if not CanUndo then
         exit;
     end;
  end;
  finally
    UpdateButtons;
  end;
end;

procedure TUndoStack.Redo(Num: integer);
var
  i: integer;
begin
  try
    for i := 1 to Num do
    begin
      if CanRedo then
      begin
        CurrentRedoItem.Redo;
        inc(FCurrentUndo);
      end
      else
      begin
        Exit;
      end;
    end;
  finally
    UpdateButtons;
  end;
end;

procedure TUndoStack.RemoveLastItem;
begin
  if Count > 0 then
  begin
    DeleteAndFree(pred(Count));
    dec(FCurrentUndo);
  end;
end;

procedure TUndoStack.UndoClick(Sender: TObject);
begin
    if not CanUndo then Exit;
    Undo(1);
    if Sender is TControl then
    begin
      if CanUndo then
      begin
        TControl(Sender).Hint := CurrentItem.UndoDescription;
        Application.HideHint;

        // This doesn't work on Linux
        Application.ActivateHint(Mouse.CursorPos);

        // clx version below
  {      Application.HintMouseMessage(TControl(Sender), [ssLeft],
          TControl(Sender).Width div 2, TControl(Sender).Height div 2);
        TControlCrack(TControl(Sender)).MouseLeave(TControl(Sender));
        Application.ActivateHint(Mouse.CursorPos); }

      end
      else
      begin
        Application.HideHint;
      end;
    end;
end;

procedure TUndoStack.RedoClick(Sender: TObject);
begin
    Redo(1);
    if Sender is TControl then
    begin
      if CanRedo then
      begin
        TControl(Sender).Hint := CurrentRedoItem.RedoDescription;
        Application.HideHint;

        // This doesn't work on Linux
         Application.ActivateHint(Mouse.CursorPos)

        // clx version below
        {Application.HintMouseMessage(TControl(Sender), [ssLeft],
          TControl(Sender).Width div 2, TControl(Sender).Height div 2);
        TControlCrack(TControl(Sender)).MouseLeave(TControl(Sender));
        Application.ActivateHint(Mouse.CursorPos); }

      end
      else
      begin
        Application.HideHint;
      end;
    end;

end;

procedure TUndoStack.SetUndoActions(const UndoAction, RedoAction: TAction);
begin
  if CanRedo then
  begin
    RedoAction.Caption := CurrentRedoItem.RedoMenuText;
    RedoAction.OnExecute := RedoClick;
    RedoAction.Enabled := true;
    RedoAction.Hint := CurrentRedoItem.RedoDescription;
  end
  else
  begin
    DisableUndoActions(nil, RedoAction);
  end;
  if CurrentItem <> nil then
  begin
    UndoAction.Caption := CurrentItem.UndoMenuText;
    UndoAction.OnExecute := UndoClick;
    UndoAction.Enabled := true;
    UndoAction.Hint := CurrentItem.UndoDescription;
  end
  else
  begin
    DisableUndoActions(UndoAction, nil);
  end;
end;

procedure TUndoStack.SetUndoMenuItems(const UndoItem, RedoItem: TMenuItem);
begin
  if CanRedo then
  begin
    RedoItem.Caption := CurrentRedoItem.RedoMenuText;
    RedoItem.Enabled := true;
    RedoItem.Hint := CurrentRedoItem.RedoDescription;
    RedoItem.OnClick := RedoClick;
  end
  else
  begin
    DisableUndoMenus(nil, RedoItem);
  end;
  if CurrentItem <> nil then
  begin
    UndoItem.Caption := CurrentItem.UndoMenuText;
    UndoItem.Enabled := true;
    UndoItem.Hint := CurrentItem.UndoDescription;
    UndoItem.OnClick := UndoClick;
  end
  else
    DisableUndoMenus(UndoItem, nil);
end;

{ disable other access methods }

procedure TUndoStack.HiddenProcExcept;
begin
  raise EHiddenProc.Create(StrErrorAccessToSt);
end;

function TUndoStack.Add(Item: Pointer): Integer;
begin
  HiddenProcExcept;
  result := 0;
end;

procedure TUndoStack.Delete(Index: Integer);
begin
  HiddenProcExcept;
end;

procedure TUndoStack.Insert(Index: Integer; const S: string);
begin
  HiddenProcExcept;
end;

procedure TUndoStack.Move(CurIndex, NewIndex: Integer);
begin
  HiddenProcExcept;
end;

procedure TUndoStack.Exchange(Index1, Index2: Integer);
begin
  HiddenProcExcept;
end;

procedure TUndoStack.UpdateButtons;
begin
  if Assigned(FUndoToolButton) then
  begin
    FUndoToolButton.Enabled := CanUndo;
    if FUndoToolButton.Enabled then
    begin
      FUndoToolButton.Caption := CurrentItem.UndoMenuText;
      FUndoToolButton.Hint := CurrentItem.UndoDescription;
      FUndoToolButton.OnClick := UndoClick;
    end
    else
    begin
      FUndoToolButton.Caption := sUndoMenu;
      FUndoToolButton.Hint := sNoUndoDescr;
      FUndoToolButton.OnClick := nil;
    end;
  end;
  if Assigned(self.FRedoToolButton) then
  begin
    FRedoToolButton.Enabled := CanRedo;
    if FRedoToolButton.Enabled then
    begin
      FRedoToolButton.Caption := CurrentRedoItem.RedoMenuText;
      FRedoToolButton.Hint := CurrentRedoItem.RedoDescription;
      FRedoToolButton.OnClick := RedoClick;
    end
    else
    begin
      FRedoToolButton.Caption := sRedoMenu;
      FRedoToolButton.Hint := sNoRedoDescr;
      FRedoToolButton.OnClick := nil;
    end;
  end;
end;

{ ----------------------------------------------- }

procedure DisableUndoMenus(UndoItem, RedoItem: TMenuItem);
begin
  if UndoItem <> nil then
  begin
    UndoItem.Caption := sUndoMenu;
    UndoItem.Enabled := false;
    UndoItem.Hint := sNoUndoDescr;
    UndoItem.OnClick := nil;
  end;
  if RedoItem <> nil then
  begin
    RedoItem.Caption := sRedoMenu;
    RedoItem.Enabled := false;
    RedoItem.Hint := sNoRedoDescr;
    RedoItem.OnClick := nil;
  end;
end;

procedure DisableUndoActions(UndoAction, RedoAction: TAction);
begin
  if UndoAction <> nil then
  begin
    UndoAction.Caption := sUndoMenu;
    UndoAction.Hint := sNoUndoDescr;
    UndoAction.OnExecute := nil;
    UndoAction.Enabled := false;
  end;
  if RedoAction <> nil then
  begin
    RedoAction.Caption := sRedoMenu;
    RedoAction.Hint := sNoRedoDescr;
    RedoAction.OnExecute := nil;
    RedoAction.Enabled := false;
  end;
end;

procedure DisableUndoToolButtons(UndoButton, RedoButton: TToolButton);
begin
  if UndoButton <> nil then
  begin
    UndoButton.Caption := sUndoMenu;
    UndoButton.Hint := sNoUndoDescr;
    UndoButton.OnClick := nil;
    UndoButton.Enabled := false;
  end;
  if RedoButton <> nil then
  begin
    RedoButton.Caption := sRedoMenu;
    RedoButton.Hint := sNoRedoDescr;
    RedoButton.OnClick := nil;
    RedoButton.Enabled := false;
  end;
end;

procedure TUndoStack.SetUndoToolButtons(const Undo, Redo: TToolButton);
begin
  if CanRedo then
  begin
    Redo.Caption := CurrentRedoItem.RedoMenuText;
    Redo.Enabled := true;
    Redo.Hint := CurrentRedoItem.RedoDescription;
    Redo.OnClick := RedoClick;
  end
  else
  begin
    DisableUndoToolButtons(nil, Redo);
  end;
  if (CurrentItem <> nil) and (Undo <> nil) then
  begin
    Undo.Caption := CurrentItem.UndoMenuText;
    Undo.Enabled := true;
    Undo.Hint := CurrentItem.UndoDescription;
    Undo.OnClick := UndoClick;
  end
  else
  begin
    DisableUndoToolButtons(Undo, nil);
  end;
end;

end.

