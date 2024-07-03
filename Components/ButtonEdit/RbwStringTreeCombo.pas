{
  This file is in the public domain.  It can be used by anyone for any purpose
  with or without acknowledgement. However it depends on two freeware units
  that are not in the public domain: SsButtonEd and VirtualTrees.  See the
  following sites for those files.
  http://www.sadmansoftware.com/delphi/freeware/buttoned.htm
  http://www.soft-gems.net/index.php?option=com_content&task=view&id=12&Itemid=33

  Written by Richard B. Winston
  rbwinst@usgs.gov
  March 10, 2011.
}
unit RbwStringTreeCombo;

interface

{#BACKUP buttoned.zip}
{#BACKUP TreeComboDemo\*.*}

uses
  SysUtils, Classes, Controls, StdCtrls, SsButtonEd, VirtualTrees, frmTreeUnit;

type

  TRbwStringTreeCombo = class(TssCustomButtonEdit)
  private
    FTree: TVirtualStringTree;
    FDropDownHeight: integer;
    FfrmTree: TfrmTree;
    FFocusedNode: PVirtualNode;
    FOnCanClose: TCanCloseEvent;
    procedure SetDropDownHeight(const Value: integer);
    procedure EnsureForm;
    procedure AssignDefaultGlyph;
    procedure SetFocusedNode(const Value: PVirtualNode);
    procedure SetOnCanClose(const Value: TCanCloseEvent);
    { Private declarations }
  protected
     procedure DoButtonClick; override;
     procedure Loaded; override;
    { Protected declarations }
  public
    Constructor Create(AnOwner: TComponent); override;
    property FocusedNode: PVirtualNode read FFocusedNode write SetFocusedNode;
    { Public declarations }
  published
    property Tree: TVirtualStringTree read FTree;
    property DropDownHeight: integer read FDropDownHeight
      write SetDropDownHeight default 200;
    property Anchors;
    property AutoSelect;
    property BorderStyle;
    property Color;
    property Ctl3d;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Glyph;
    property HideSelection;
    property NumGlyphs;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFNDEF VER80}
    property OnStartDrag;
    {$ENDIF}
    property OnCanClose: TCanCloseEvent read FOnCanClose write SetOnCanClose;
    { Published declarations }
  end;

procedure Register;

implementation

{$R *.res}
{#BACKUP *.ICO}

uses
  Types, Graphics;

type
  TVirtualStringTreeCrack = class(TVirtualStringTree);


procedure Register;
begin
  RegisterComponents('RBW', [TRbwStringTreeCombo]);
end;

{ TRbwStringTreeCombo }

constructor TRbwStringTreeCombo.Create(AnOwner: TComponent);
begin
  inherited;
  FTree := TVirtualStringTree.Create(self);
  FTree.SetSubComponent(True);
  FTree.Name := 'Tree';
  FDropDownHeight := 200;
  EnsureForm;
  AssignDefaultGlyph
end;

procedure TRbwStringTreeCombo.AssignDefaultGlyph;
var
  Index: Integer;
  TrianglePoints: array[0..2] of TPoint;
  AGlyph: TBitmap;
begin
  AGlyph := TBitMap.Create;
  try
    AGlyph.Height := 8;
    AGlyph.Width := 16;
    AGlyph.Canvas.Pen.Color := clBtnFace;
    AGlyph.Canvas.Brush.Color := clBtnFace;
    AGlyph.Canvas.Rectangle(0, 0, 16, 8);
    TrianglePoints[0].X := 0;
    TrianglePoints[0].Y := 2;
    TrianglePoints[1].X := 6;
    TrianglePoints[1].Y := 2;
    TrianglePoints[2].X := 3;
    TrianglePoints[2].Y := 5;
    AGlyph.Canvas.Pen.Color := clBlack;
    AGlyph.Canvas.Brush.Color := clBlack;
    AGlyph.Canvas.Polygon(TrianglePoints);
    AGlyph.Canvas.Pen.Color := clLtGray;
    AGlyph.Canvas.Brush.Color := clLtGray;
    for Index := 0 to 2 do
    begin
      TrianglePoints[Index].X := TrianglePoints[Index].X + 8;
    end;
    AGlyph.Canvas.Polygon(TrianglePoints);
    Button.Glyph := AGlyph;
  finally
    AGlyph.Free;
  end;
end;

procedure TRbwStringTreeCombo.EnsureForm;
begin
  if FfrmTree = nil then
  begin
    FfrmTree := TfrmTree.Create(self);
    FfrmTree.OnCanClose := OnCanClose;
    FTree.Parent := FfrmTree;
    FTree.Align := alClient;
    // TVirtualStringTree does not currently publish OnMouseEnter
    // or OnMouseLeave. If that changes, this will need to change too.
    // It can be handled in DoButtonClick in the same way OnMouseDown
    // is handled.
    TVirtualStringTreeCrack(FTree).OnMouseEnter := FfrmTree.TreeEnter;
    TVirtualStringTreeCrack(FTree).OnMouseLeave := FfrmTree.TreeLeave;
  end;
end;

procedure TRbwStringTreeCombo.Loaded;
begin
  inherited;
  FfrmTree.Font := Font;
  FTree.ParentFont := True;
end;

procedure TRbwStringTreeCombo.DoButtonClick;
var
  APoint: TPoint;
  CellText: UnicodeString;
begin
  inherited;
  EnsureForm;
  APoint.X := 0;
  APoint.Y := Height;
  APoint := ClientToScreen(APoint);
  FfrmTree.Height := DropDownHeight;
  FfrmTree.Width := Width;
  FfrmTree.Left := APoint.X-2;
  FfrmTree.Top := APoint.Y-2;
  FfrmTree.StoredMouseDown := FTree.OnMouseDown;
  try
    FTree.OnMouseDown := FfrmTree.TreeMouseDown;
    if FfrmTree.ShowModal = mrOK then
    begin
      FFocusedNode := FTree.FocusedNode;
      if FFocusedNode = nil then
      begin
        Text := '';
      end
      else
      begin
        if Assigned(FTree.OnGetText) then
        begin
          FTree.OnGetText(FTree, FFocusedNode, 0, ttNormal, CellText);
          Text := CellText
        end;
      end;
    end;
  finally
    FTree.OnMouseDown := FfrmTree.StoredMouseDown;
  end;
end;

procedure TRbwStringTreeCombo.SetDropDownHeight(const Value: integer);
begin
  FDropDownHeight := Value;
end;

procedure TRbwStringTreeCombo.SetFocusedNode(const Value: PVirtualNode);
var
  CellText: UnicodeString;
begin
  FFocusedNode := Value;
  FTree.Selected[FFocusedNode] := True;
  if Assigned(FTree.OnGetText) then
  begin
    FTree.OnGetText(FTree, FFocusedNode, 0, ttNormal, CellText);
    Text := CellText
  end;
end;

procedure TRbwStringTreeCombo.SetOnCanClose(const Value: TCanCloseEvent);
begin
  FOnCanClose := Value;
  if FfrmTree <> nil then
  begin
    FfrmTree.OnCanClose := OnCanClose;
  end;
end;

end.
