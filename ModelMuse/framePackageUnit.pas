unit framePackageUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, JvExStdCtrls, JvCheckBox, JvPageList,
  ModflowPackageSelectionUnit, RbwController, Grids, RbwDataGrid4;

type
  TframePackage = class(TFrame)
    lblComments: TLabel;
    memoComments: TMemo;
    lblPackage: TLabel;
    rcSelectionController: TRbwController;
  private
    FNode: TTreeNode;
    FAlternateNode: TTreeNode;
    FSelected: boolean;
    FSelectionType: TSelectionType;
    FOnSelectedChange: TNotifyEvent;
    FCanSelect: boolean;
    FControlsMoved: Boolean;
    procedure SetNodeStateIndex;
    procedure SetCanSelect(const Value: boolean);
  { Private declarations }
  protected
    procedure SetSelected(const Value: boolean); virtual;

    procedure EnableMultiEditControl(Control: TControl;
      DataGrid: TRbwDataGrid4);
    procedure GetFarmOption(combo: TComboBox; Option: TFarmOption);
    function SetFarmOption(combo: TComboBox): TFarmOption;
    //    procedure EnableNwt;
  public
    // @name moves the control on @classname to the first
    // tab of ParentPageControl.
    // @name is used in @link(TframeGMG).
    procedure MoveControlsToTab(ParentPageControl: TPageControl); overload;
    procedure MoveControlsToTab(ParentPageList: TJvPageList); overload;
    property CanSelect: boolean read FCanSelect write SetCanSelect;
    procedure GetData(Package: TModflowPackageSelection); virtual;
    procedure SetData(Package: TModflowPackageSelection); virtual;
    property Selected: boolean read FSelected write SetSelected;
    property SelectionType: TSelectionType read FSelectionType
      write FSelectionType;
    constructor Create(AOwner: TComponent); override;
    procedure NilNode;
  published
    property OnSelectedChange: TNotifyEvent read FOnSelectedChange
      write FOnSelectedChange;
    property Node: TTreeNode read FNode;// write FNode;
    { Public declarations }
  end;

var
  StaticTransient: TStringlist;
  DontUseStaticTransient: TStringlist;
  DontUseStatic: TStringlist;

implementation

uses
  frmGoPhastUnit, GoPhastTypes;

{$R *.dfm}

{ TframePackage }

constructor TframePackage.Create(AOwner: TComponent);
begin
  inherited;
  FCanSelect := True;
end;

procedure TframePackage.SetNodeStateIndex;
begin
  if FNode <> nil then
  begin
    case SelectionType of
      stCheckBox:
        begin
          if CanSelect then
          begin
            FNode.StateIndex := Ord(FSelected) + 1;
          end
          else
          begin
            FNode.StateIndex := 6;
          end;
        end;
      stRadioButton:
        begin
          if CanSelect then
          begin
            FNode.StateIndex := Ord(FSelected) + 4;
          end
          else
          begin
            FNode.StateIndex := 7
          end;
        end;
    end;
    if FAlternateNode <> nil then
    begin
      FAlternateNode.StateIndex := FNode.StateIndex;
    end;

    FNode.TreeView.Invalidate;
  end;
end;

procedure TframePackage.GetData(Package: TModflowPackageSelection);
begin
  SelectionType := Package.SelectionType;
  FNode := Package.Node;
  FAlternateNode := Package.AlternateNode;
  Selected := Package.IsSelected;
  memoComments.Lines := Package.Comments;
  lblPackage.Width := Width - lblPackage.Left - 8;
  lblPackage.Caption := Package.PackageIdentifier;


//  cbNwt.Visible := (frmGoPhast.ModelSelection = msModflow2015)
//    and ((Package is TWellPackage)
//    or (Package is TGhbPackage)
//    or (Package is TDrnPackage)
//    or (Package is TRivPackage)
//    or (Package is TEtsPackageSelection)
//    or (Package is TSfrModflow6PackageSelection));
//
//  cbNwt.Checked := (Package.NewtonFormulation = nfOn);
end;

procedure TframePackage.MoveControlsToTab(ParentPageList: TJvPageList);
var
  AControl: TControl;
  OldHeight: Integer;
  Index: Integer;
  DeltaHeight: Integer;
  ControlsToMoveDown: TList;
  DestinationTab: TJvCustomPage;
  memoHeight: Integer;
begin
  if FControlsMoved then
  begin
    Exit;
  end;
  if not (csDesigning in ComponentState) then
  begin
    DestinationTab := ParentPageList.Pages[0];
    ControlsToMoveDown := TList.Create;
    try
      DestinationTab.Handle;
      for Index := 0 to DestinationTab.ControlCount - 1 do
      begin
        ControlsToMoveDown.Add(DestinationTab.Controls[Index]);
      end;
      OldHeight := ParentPageList.Height;
      lblPackage.Parent := DestinationTab;
      lblComments.Parent := DestinationTab;
      memoHeight := memoComments.Height;
      memoComments.Parent := DestinationTab;
      ParentPageList.Align := alClient;
      memoComments.Height := memoHeight;
      DeltaHeight := ParentPageList.Height - OldHeight;
      for Index := 0 to ControlsToMoveDown.Count - 1 do
      begin
        AControl := ControlsToMoveDown[Index];
        AControl.Top := AControl.Top + DeltaHeight;
        AControl.Anchors := [akLeft, akBottom];
      end;
    finally
      ControlsToMoveDown.Free;
    end;
    FControlsMoved := True;
  end;
end;

procedure TframePackage.SetCanSelect(const Value: boolean);
begin
  if FCanSelect <> Value then
  begin
    FCanSelect := Value;
    SetNodeStateIndex;
  end;
end;

procedure TframePackage.SetData(Package: TModflowPackageSelection);
begin
  Package.IsSelected := Selected;
  Package.Comments := memoComments.Lines;
//  Package.NewtonFormulation := TNewtonFormulation(cbNwt.Checked);
end;

procedure TframePackage.SetSelected(const Value: boolean);
begin
  // FOnSelectedChange should be called even if
  // FSelected has not changed.
  FSelected := Value;
  rcSelectionController.Enabled := Value;
  if Assigned(FOnSelectedChange) then
  begin
    FOnSelectedChange(self);
  end;
  SetNodeStateIndex;
end;

procedure TframePackage.MoveControlsToTab(ParentPageControl: TPageControl);
var
  AControl: TControl;
  OldHeight: Integer;
  Index: Integer;
  DeltaHeight: Integer;
  ControlsToMoveDown: TList;
  DestinationTab: TTabSheet;
  memoHeight: integer;
begin
  if FControlsMoved then
  begin
    Exit;
  end;
  if not (csDesigning in ComponentState) then
  begin
    DestinationTab := ParentPageControl.Pages[0];
    ControlsToMoveDown := TList.Create;
    try
      DestinationTab.Handle;
      for Index := 0 to DestinationTab.ControlCount - 1 do
      begin
        ControlsToMoveDown.Add(DestinationTab.Controls[Index]);
      end;
      OldHeight := ParentPageControl.Height;
      lblPackage.Parent := DestinationTab;
      lblComments.Parent := DestinationTab;
      memoHeight := memoComments.Height;
      memoComments.Parent := DestinationTab;
      ParentPageControl.Align := alClient;
      memoComments.Height := memoHeight;
      DeltaHeight := ParentPageControl.Height - OldHeight;
      for Index := 0 to ControlsToMoveDown.Count - 1 do
      begin
        AControl := ControlsToMoveDown[Index];
        AControl.Top := AControl.Top + DeltaHeight;
        AControl.Anchors := [akLeft, akBottom];
      end;
    finally
      ControlsToMoveDown.Free;
    end;
    FControlsMoved := True;
  end;
end;

procedure TframePackage.NilNode;
begin
  FNode := nil;
  FAlternateNode := nil;
end;


procedure TframePackage.EnableMultiEditControl(Control: TControl;
  DataGrid: TRbwDataGrid4);
var
  RowIndex: Integer;
  ShouldEnable: Boolean;
  ColIndex: Integer;
begin
  ShouldEnable := False;
  for RowIndex := DataGrid.FixedRows to DataGrid.RowCount - 1 do
  begin
    for ColIndex := 2 to DataGrid.ColCount - 1 do
    begin
      ShouldEnable := DataGrid.IsSelectedCell(ColIndex, RowIndex);
      if ShouldEnable then
      begin
        break;
      end;
    end;
    if ShouldEnable then
    begin
      break;
    end;
  end;
  Control.Enabled := ShouldEnable;
end;

procedure TframePackage.GetFarmOption(combo: TComboBox; Option: TFarmOption);
begin
  combo.ItemIndex := Ord(Option);
end;

function TframePackage.SetFarmOption(combo: TComboBox): TFarmOption;
begin
  result := TFarmOption(combo.ItemIndex);
end;

initialization
  StaticTransient := TStringlist.Create;
  StaticTransient.Add('Static');
  StaticTransient.Add('Transient');

  DontUseStaticTransient := TStringlist.Create;
  DontUseStaticTransient.Add('don''t use');
  DontUseStaticTransient.Add('Static');
  DontUseStaticTransient.Add('Transient');

  DontUseStatic := TStringlist.Create;
  DontUseStatic.Add('don''t use');
  DontUseStatic.Add('Static');

finalization
  StaticTransient.Free;
  DontUseStaticTransient.Free;
  DontUseStatic.Free;

end.
