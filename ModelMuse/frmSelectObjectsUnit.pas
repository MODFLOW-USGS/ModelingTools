{@abstract(The main purpose of @name is to define @link(TfrmSelectObjects)
  which is used to select @link(TScreenObject)s by selecting
  them by name.)}
unit frmSelectObjectsUnit;

interface

uses
  System.UITypes, SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, ComCtrls, Buttons, ExtCtrls, Grids,
  GrayTabs;

{ TODO : Consider using a two panel selector (Tidwell pattern 13). }  

type
  {@abstract(@name is used to select @link(TScreenObject)s by selecting
    them by name.)}
  TfrmSelectObjects = class(TfrmCustomGoPhast)
    // Clicking @name closes @classname without changing anything.
    btnCancel: TBitBtn;
    // Clicking @name displays help on @classname.
    btnHelp: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // See @link(btnSelectClick).
    btnSelectAll: TButton;
    // See @link(btnSelectClick).
    btnSelectNone: TButton;
    // See @link(btnToggleClick).
    btnToggle: TButton;
    // @name controls whether or not @link(TScreenObject)s whose
    // @link(TScreenObject.Visible) property is @false will be
    // displayed.
    // See @link(cbIncludeHiddenObjectsClick) and @link(GetData).
    cbIncludeHiddenObjects: TCheckBox;
    // @name holds @link(tabFront), @link(tabSide), and @link(tabTop).
    pcObjects: TPageControl;
    // @name holds the controls on the bottom of the @classname.
    pnlBottom: TPanel;
    // @name displays the @link(TScreenObject)s on the front view of the model.
    lvFront: TListView;
    // @name displays the @link(TScreenObject)s on the side view of the model.
    lvSide: TListView;
    // @name displays the @link(TScreenObject)s on the top view of the model.
    lvTop: TListView;
    // @name holds @link(lvFront).
    tabFront: TTabSheet;
    // @name holds @link(lvSide).
    tabSide: TTabSheet;
    // @name holds @link(lvTop).
    tabTop: TTabSheet;
    // @name holds the search term that will be used for selecting
    // @link(TScreenObject)s
    // @seealso(btnSelectByName)
    // @seealso(btnSelectByNameClick).
    edSearchTerm: TEdit;
    // Clicking @name causes all @link(TScreenObject)s that contain
    // the search term as part of their name to be selected.
    // @seealso(btnSelectByNameClick).
    // @seealso(edSearchTerm).
    btnSelectByName: TButton;
    // @name displays the number of selected objects.
    lblCount: TLabel;
    // @name calls @link(SetData).
    // @seealso(UpdateCount)
    procedure btnOKClick(Sender: TObject);
    // @name checks or unchecks all the checkboxes on the TListView
    // (@link(lvFront), @link(lvSide), or @link(lvTop)) that is on
    // the active page of @link(pcObjects).
    procedure btnSelectClick(Sender: TObject);
    // @name toggles the checked state of all the checkboxes on the TListView
    // (@link(lvFront), @link(lvSide), or @link(lvTop)) that is on
    // the active page of @link(pcObjects).
    procedure btnToggleClick(Sender: TObject);
    // @name calls @link(GetData).
    procedure cbIncludeHiddenObjectsClick(Sender: TObject);
    // @name initializes @classname and calls @link(GetData).
    procedure FormCreate(Sender: TObject); override;
    // @name causes all @link(TScreenObject)s that contain
    // the search term as part of their name to be selected.
    // @seealso(btnSelectByName)
    // @seealso(edSearchTerm).
    procedure btnSelectByNameClick(Sender: TObject);
    // @name is used with @link(lvTop), @link(lvFront), and @link(lvSide),
    // @name causes the @link(TScreenObject) represented by the
    // selected item to be opened in @link(TfrmScreenObjectProperties).
    procedure lvTopDblClick(Sender: TObject);
    // @name is used with @link(lvTop), @link(lvFront), and @link(lvSide),
    // @name calls @link(UpdateCount).
    procedure lvTopChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  private
    // @name is set to @True while getting data so as to cause
    // @link(UpdateCount) to abort.
    FGettingData: Boolean;
    // @name is set to @True while editing data so as to cause
    // @link(UpdateCount) to abort.
    FEditingData: Boolean;
    // @name displays the @link(TScreenObject)s in
    // @link(lvFront), @link(lvSide), and @link(lvTop).
    // See @link(cbIncludeHiddenObjects).
    procedure GetData;
    // @name uses a @link(TUndoChangeSelection) to set the selected
    // @link(TScreenObject)s to the ones that are checked on the
    // active page of @link(pcObjects).
    procedure SetData;
    function GetCurrentListView: TListView;
    // @name displays the number of selected @link(TScreenObject)s in
    // @link(lblCount).
    procedure UpdateCount;
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses frmGoPhastUnit, ScreenObjectUnit, GoPhastTypes, UndoItemsScreenObjects, 
  Windows;

resourcestring
  StrYourModelHasNoOb = 'Your model has no objects.';
  StrYourModelHasNoVi = 'Your model has no visible objects.';

{$R *.dfm}

{ TfrmSelectObjects }

procedure TfrmSelectObjects.GetData;
var
  Index: Integer;
  AScreenObject: TScreenObject;
  Item: TListItem;
  ListView: TListView;
  IncludeHiddenObjects: boolean;
  Objects: TList;
begin
  FGettingData := True;
  try
    IncludeHiddenObjects := cbIncludeHiddenObjects.Checked;
    lvTop.Items.Clear;
    lvFront.Items.Clear;
    lvSide.Items.Clear;
    Objects:= TList.Create;
    try
      for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
      begin
        AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
        if not AScreenObject.Deleted and (IncludeHiddenObjects or
          AScreenObject.Visible) then
        begin
          Objects.Add(AScreenObject);
        end;
      end;
      Objects.Sort(ScreenObjectCompare);
      ListView := nil;
      for Index := 0 to Objects.Count - 1 do
      begin
        AScreenObject := Objects[Index];
        case AScreenObject.ViewDirection of
          vdTop:
            begin
              ListView := lvTop;
            end;
          vdFront:
            begin
              ListView := lvFront;
            end;
          vdSide:
            begin
              ListView := lvSide;
            end
          else Assert(False);
        end;
        Item := ListView.Items.Add;
        Item.Caption := AScreenObject.Name;
        Item.Data := AScreenObject;
        Item.Checked := AScreenObject.Selected;
      end;
    finally
      Objects.Free;
    end;
    tabTop.TabVisible := lvTop.Items.Count <> 0;
    tabFront.TabVisible := lvFront.Items.Count <> 0;
    tabSide.TabVisible := lvSide.Items.Count <> 0;
    if not (tabTop.TabVisible or tabFront.TabVisible or tabSide.TabVisible) then
    begin
      if cbIncludeHiddenObjects.Checked then
      begin
        MessageDlg(StrYourModelHasNoOb, mtInformation, [mbOK], 0);
      end
      else
      begin
        MessageDlg(StrYourModelHasNoVi, mtInformation, [mbOK], 0);
      end;
      Exit;
    end;
  finally
    FGettingData := False;
  end;
  UpdateCount;
end;

procedure TfrmSelectObjects.lvTopChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  inherited;
  UpdateCount;
end;

procedure TfrmSelectObjects.lvTopDblClick(Sender: TObject);
var
  ListView: TListView;
  Item: TListItem;
  AScreenObject : TScreenObject;
  Index: Integer;
begin
  inherited;
   ListView := GetCurrentListView;
   if ListView <> nil then
   begin
     Item := ListView.Selected;
     if Item <> nil then
     begin
        AScreenObject := Item.Data;
        if AScreenObject <> nil then
        begin
          SelectAScreenObject(AScreenObject);
          frmGoPhast.EditScreenObjects;
          GetData;
          for Index := 0 to ListView.Items.Count - 1 do
          begin
            Item := ListView.Items[Index];
            if Item.Data = AScreenObject then
            begin
              ListView.Selected := Item;
              Item.MakeVisible(False);
              break;
            end;
          end;
        end;
     end;
   end;
end;

procedure TfrmSelectObjects.FormCreate(Sender: TObject);
begin
  inherited;
  pcObjects.ActivePageIndex := 0;
  GetData;
end;

procedure TfrmSelectObjects.SetData;
var
  ListView: TListView;
  Index: integer;
  Item: TListItem;
  Undo: TUndoChangeSelection;
  AScreenObject: TScreenObject;
begin
  ListView := GetCurrentListView;

  Undo := TUndoChangeSelection.Create;
  frmGoPhast.ResetSelectedScreenObjects;

  for Index := 0 to ListView.Items.Count - 1 do
  begin
    Item := ListView.Items[Index];
    if Item.Checked then
    begin
      AScreenObject := Item.Data;
      AScreenObject.Selected := True;
    end;
  end;

  Undo.SetPostSelection;

  if Undo.SelectionChanged then
  begin
    frmGoPhast.UndoStack.Submit(Undo);
  end
  else
  begin
    Undo.Free;
  end;
end;

procedure TfrmSelectObjects.UpdateCount;
var
  List: TListView;
  Index: Integer;
  Item: TListItem;
  Count: Integer;
begin
  if FGettingData or FEditingData then Exit;

  List := GetCurrentListView;
  Count := 0;
  for Index := 0 to List.Items.Count - 1 do
  begin
    Item := List.Items[Index];
    if Item.Checked then
    begin
      Inc(Count);
    end;
  end;

  lblCount.Caption := Format('Selected objects = %d', [Count]);
end;

function TfrmSelectObjects.GetCurrentListView: TListView;
begin
  result := nil;
  case pcObjects.ActivePageIndex of
    0:
      // Top
      begin
        result := lvTop;
      end;
    1:
      // Front
      begin
        result := lvFront;
      end;
    2:
      // Side
      begin
        result := lvSide;
      end;
  else
    Assert(False);
  end;
end;

procedure TfrmSelectObjects.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmSelectObjects.btnSelectClick(Sender: TObject);
var
  ListView: TListView;
  Index: integer;
  Item: TListItem;
  ShouldSelect: boolean;
begin
  inherited;
  FEditingData := True;
  try
  ShouldSelect := Sender = btnSelectAll;
  ListView := GetCurrentListView;
  for Index := 0 to ListView.Items.Count - 1 do
  begin
    Item := ListView.Items[Index];
    Item.Checked := ShouldSelect;
  end;
  finally
    FEditingData := False;
  end;
  UpdateCount;
end;

procedure TfrmSelectObjects.btnToggleClick(Sender: TObject);
var
  ListView: TListView;
  Index: integer;
  Item: TListItem;
begin
  inherited;
  FEditingData := True;
  try
  ListView := GetCurrentListView;
  for Index := 0 to ListView.Items.Count - 1 do
  begin
    Item := ListView.Items[Index];
    Item.Checked := not Item.Checked;
  end;

  finally
    FEditingData := False;
  end;

  UpdateCount
end;

procedure TfrmSelectObjects.cbIncludeHiddenObjectsClick(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmSelectObjects.btnSelectByNameClick(Sender: TObject);
var
  ListView: TListView;
  Index: integer;
  Item: TListItem;
  ShouldSelect: boolean;
  SearchTerm: string;
begin
  inherited;
//  OutputDebugString('SAMPLING ON');
  FEditingData := True;
  try
  ListView := GetCurrentListView;

  SearchTerm := edSearchTerm.Text;
  for Index := 0 to ListView.Items.Count - 1 do
  begin
    Item := ListView.Items[Index];
    ShouldSelect := Pos(SearchTerm, Item.Caption) > 0;
    Item.Checked := ShouldSelect;
  end;
  UpdateCount;

  finally
    FEditingData := False;
  end;
  UpdateCount;

//  OutputDebugString('SAMPLING OFF')

end;

end.

