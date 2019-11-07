
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit treecombo;

{$I OFFICEVER.INC}
{$P+,S-,W-,R-,H+}
{$C PRELOAD}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, ImgList, Buttons, BaseCombo, BaseComboForm;

type

{ TksoTreeComboBox }

  TksoTreeComboBox = class(TksoAbstractComboBox)
  private
    FTreeView: TTreeView;
    // rbw begin change
    FFormShown: boolean;
    // rbw end change
    procedure SetAutoExpand(const Value: boolean);
    procedure SetChangeDelay(const Value: Integer);
    procedure SetHideSelection(const Value: boolean);
    procedure SetHotTrack(const Value: boolean);
    procedure SetIndent(const Value: Integer);
    procedure SetImages(const Value: TImageList);
    procedure SetItems(const Value: TTreeNodes);
    procedure SetReadOnly(const Value: boolean);
    procedure SetRightClickSelect(const Value: Boolean);
    procedure SetRowSelect(const Value: Boolean);
    procedure SetSelected(const Value: TTreeNode);
    procedure SetShowButtons(const Value: Boolean);
    procedure SetShowLines(const Value: Boolean);
    procedure SetShowRoot(const Value: Boolean);
    procedure SetSortType(const Value: TSortType);
    procedure SetStateImages(const Value: TCustomImageList);
    procedure SetToolTips(const Value: Boolean);
    procedure SetTopItem(const Value: TTreeNode);
    function GetAutoExpand: boolean;
    function GetChangeDelay: Integer;
    function GetHideSelection: boolean;
    function GetHotTrack: boolean;
    function GetImages: TImageList;
    function GetIndent: Integer;
    function GetItems: TTreeNodes;
    function GetReadOnly: boolean;
    function GetRightClickSelect: Boolean;
    function GetRowSelect: Boolean;
    function GetSelected: TTreeNode;
    function GetShowButtons: Boolean;
    function GetShowLines: Boolean;
    function GetShowRoot: Boolean;
    function GetSortType: TSortType;
    function GetStateImages: TCustomImageList;
    function GetToolTips: Boolean;
    function GetTopItem: TTreeNode;
  protected
    { Override declarations }
    procedure DoDropDown(Sender: TObject); override;
    function GetDropDownForm: TCustomForm; override;
    procedure DrawBox(Canvas: TCanvas; R: TRect; State: TComboState); override;
    // rbw begin change
    // Users should not change FormShown.  It is for internal use only.
    property FormShown: boolean read FFormShown write FFormShown;
    // rbw end change
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Change; override;
    procedure Previous; override;
    procedure Next; override;
    property Selected: TTreeNode read GetSelected write SetSelected;
    property TopItem: TTreeNode read GetTopItem write SetTopItem;
    property TreeView: TTreeView read FTreeView;
  published
    property AutoExpand: boolean read GetAutoExpand write SetAutoExpand;
    property ChangeDelay: Integer read GetChangeDelay write SetChangeDelay;
    property HideSelection: boolean read GetHideSelection write SetHideSelection;
    property HotTrack: boolean read GetHotTrack write SetHotTrack;
    property Images: TImageList read GetImages write SetImages;
    property Indent: Integer read GetIndent write SetIndent;
    property Items: TTreeNodes read GetItems write SetItems;
    property ReadOnly: boolean read GetReadOnly write SetReadOnly;
    property RightClickSelect: Boolean read GetRightClickSelect write SetRightClickSelect;
    property RowSelect: Boolean read GetRowSelect write SetRowSelect;
    property ShowButtons: Boolean read GetShowButtons write SetShowButtons;
    property ShowLines: Boolean read GetShowLines write SetShowLines;
    property ShowRoot: Boolean read GetShowRoot write SetShowRoot;
    property SortType: TSortType read GetSortType write SetSortType;
    property StateImages: TCustomImageList read GetStateImages write SetStateImages;
    property ToolTips: Boolean read GetToolTips write SetToolTips;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property BorderStyle;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnChange;
    property OnDropDown;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

implementation {===============================================================}

uses TreeComboForm;

{ TksoTreeComboBox }

constructor TksoTreeComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTreeView := TTreeView.Create(Self);
  FTreeView.Parent := Self;
  FTreeView.Top := Screen.Height;
  FTreeView.BorderStyle := bsNone;
  FormShown := False;
end;

destructor TksoTreeComboBox.Destroy;
begin
  FTreeView.Free;
  inherited Destroy;
end;

procedure TksoTreeComboBox.DoDropDown(Sender: TObject);
var
  Node: TTreeNode;
  DoChange: TTVChangedEvent;
begin
  inherited;
  DoChange := TreeView.OnChange;
  TreeView.OnChange := nil;
  Node := TreeView.Selected;
  FTreeView.Selected := nil;
  FTreeView.Selected := Node;
  TreeView.OnChange := DoChange;
end;

procedure TksoTreeComboBox.Loaded;
begin
  inherited Loaded;
  FTreeView.Visible := false;
end;

procedure TksoTreeComboBox.Change;
begin
  inherited Change;
end;

procedure TksoTreeComboBox.DrawBox(Canvas: TCanvas; R: TRect; State: TComboState);
var
  B: TBitmap;
begin
  Canvas.Font := Font;
  with Canvas do
  begin
    case State of
      csNormal, csDown:
      begin
        Canvas.Pen.Color := clWindowText;
        Brush.Color := Color;
        Font.Color := clWindowText;
      end;
      csFocused:
      begin
        Canvas.Pen.Color := clHighlightText;
        Brush.Color := clHighlight;
        Font.Color := clHighlightText;
      end;
      csDisable:
      begin
        Pen.Color := clSilver;
        Brush.Color := clSilver;
        Font.Color := clGray;
      end;
    end;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Style := psSolid;
    // Draw
    FillRect(R);
    if Selected <> nil then
    begin
      if (Images <> nil) and (Selected.ImageIndex >= 0) then
      begin
        B := TBitmap.Create;
        Images.GetBitmap(Selected.ImageIndex, B);
        B.Transparent := true;
        Draw(R.left+2, R.top, B);
        TextOut(R.left+B.Width+4, R.top + (R.bottom-R.top-TextHeight(Selected.Text)) div 2,
          Selected.Text);
        B.Free;
      end
      else
        TextOut(R.left+1, R.top + (R.bottom-R.top-TextHeight(Selected.Text)) div 2,
          Selected.Text);
    end;
    // End Draw
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := clHighlightText;
    if State = csFocused then
      Canvas.DrawFocusRect(R);
  end;
end;

function TksoTreeComboBox.GetDropDownForm: TCustomForm;
begin
  Result := TfrmTreeCombo.Create(Self);
end;

procedure TksoTreeComboBox.Next;
begin
end;

procedure TksoTreeComboBox.Previous;
begin
end;

procedure TksoTreeComboBox.SetAutoExpand(const Value: boolean);
begin
  if FTreeView <> nil then
    FTreeView.AutoExpand := Value;
end;

procedure TksoTreeComboBox.SetChangeDelay(const Value: Integer);
begin
  if FTreeView <> nil then
    FTreeView.ChangeDelay := Value;
end;

procedure TksoTreeComboBox.SetHideSelection(const Value: boolean);
begin
  if FTreeView <> nil then
    FTreeView.HideSelection := Value;
end;

procedure TksoTreeComboBox.SetHotTrack(const Value: boolean);
begin
  if FTreeView <> nil then
    FTreeView.HotTrack := Value;
end;

procedure TksoTreeComboBox.SetIndent(const Value: Integer);
begin
  if FTreeView <> nil then
    FTreeView.Indent := Value;
end;

procedure TksoTreeComboBox.SetImages(const Value: TImageList);
begin
  if FTreeView <> nil then
    FTreeView.Images := Value;
end;

procedure TksoTreeComboBox.SetItems(const Value: TTreeNodes);
begin
  if FTreeView <> nil then
    FTreeView.Items := Value;
end;

procedure TksoTreeComboBox.SetReadOnly(const Value: boolean);
begin
  if FTreeView <> nil then
    FTreeView.ReadOnly := Value;
end;

procedure TksoTreeComboBox.SetRightClickSelect(const Value: Boolean);
begin
  if FTreeView <> nil then
    FTreeView.RightClickSelect := Value;
end;

procedure TksoTreeComboBox.SetRowSelect(const Value: Boolean);
begin
  if FTreeView <> nil then
    FTreeView.RowSelect := Value;
end;

procedure TksoTreeComboBox.SetSelected(const Value: TTreeNode);
begin
  if FTreeView <> nil then
    FTreeView.Selected := Value;
end;

procedure TksoTreeComboBox.SetShowButtons(const Value: Boolean);
begin
  if FTreeView <> nil then
    FTreeView.ShowButtons := Value;
end;

procedure TksoTreeComboBox.SetShowLines(const Value: Boolean);
begin
  if FTreeView <> nil then
    FTreeView.ShowLines := Value;
end;

procedure TksoTreeComboBox.SetShowRoot(const Value: Boolean);
begin
  if FTreeView <> nil then
    FTreeView.ShowRoot := Value;
end;

procedure TksoTreeComboBox.SetSortType(const Value: TSortType);
begin
  if FTreeView <> nil then
    FTreeView.SortType := Value;
end;

procedure TksoTreeComboBox.SetStateImages(const Value: TCustomImageList);
begin
  if FTreeView <> nil then
    FTreeView.StateImages := Value;
end;

procedure TksoTreeComboBox.SetToolTips(const Value: Boolean);
begin
  if FTreeView <> nil then
    FTreeView.ToolTips := Value;
end;

procedure TksoTreeComboBox.SetTopItem(const Value: TTreeNode);
begin
  if FTreeView <> nil then
    FTreeView.TopItem := Value;
end;

function TksoTreeComboBox.GetAutoExpand: boolean;
begin
  Result := true;
  if FTreeView <> nil then
    Result := FTreeView.AutoExpand;
end;

function TksoTreeComboBox.GetChangeDelay: Integer;
begin
  Result := 0;
  if FTreeView <> nil then
    Result := FTreeView.ChangeDelay;
end;

function TksoTreeComboBox.GetHideSelection: boolean;
begin
  Result := true;
  if FTreeView <> nil then
    Result := FTreeView.HideSelection;
end;

function TksoTreeComboBox.GetHotTrack: boolean;
begin
  Result := true;
  if FTreeView <> nil then
    Result := FTreeView.HotTrack;
end;

function TksoTreeComboBox.GetImages: TImageList;
begin
  Result := nil;
  if FTreeView <> nil then
    Result := (FTreeView.Images as TImageList);
end;

function TksoTreeComboBox.GetIndent: Integer;
begin
  Result := 0;
  if FTreeView <> nil then
    Result := FTreeView.Indent;
end;

function TksoTreeComboBox.GetItems: TTreeNodes;
begin
  Result := nil;
  if FTreeView <> nil then
    Result := FTreeView.Items;
end;

function TksoTreeComboBox.GetReadOnly: boolean;
begin
  Result := true;
  if FTreeView <> nil then
    Result := FTreeView.ReadOnly;
end;

function TksoTreeComboBox.GetRightClickSelect: Boolean;
begin
  Result := true;
  if FTreeView <> nil then
    Result := FTreeView.RightClickSelect;
end;

function TksoTreeComboBox.GetRowSelect: Boolean;
begin
  Result := true;
  if FTreeView <> nil then
    Result := FTreeView.RowSelect;
end;

function TksoTreeComboBox.GetSelected: TTreeNode;
begin
  Result := nil;
  if FTreeView <> nil then
    Result := FTreeView.Selected;
end;

function TksoTreeComboBox.GetShowButtons: Boolean;
begin
  Result := true;
  if FTreeView <> nil then
    Result := FTreeView.ShowButtons;
end;

function TksoTreeComboBox.GetShowLines: Boolean;
begin
  Result := true;
  if FTreeView <> nil then
    Result := FTreeView.ShowLines;
end;

function TksoTreeComboBox.GetShowRoot: Boolean;
begin
  Result := true;
  if FTreeView <> nil then
    Result := FTreeView.ShowRoot;
end;

function TksoTreeComboBox.GetSortType: TSortType;
begin
  Result := stNone;
  if FTreeView <> nil then
    Result := FTreeView.SortType;
end;

function TksoTreeComboBox.GetStateImages: TCustomImageList;
begin
  Result := nil;
  if FTreeView <> nil then
    Result := FTreeView.StateImages;
end;

function TksoTreeComboBox.GetToolTips: Boolean;
begin
  Result := true;
  if FTreeView <> nil then
    Result := FTreeView.ToolTips;
end;

function TksoTreeComboBox.GetTopItem: TTreeNode;
begin
  Result := nil;
  if FTreeView <> nil then
    Result := FTreeView.TopItem;
end;

end.
