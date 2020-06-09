unit RbwCheckTreeView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls;

type
  TRbwCheckTreeView = class(TTreeView)
  private
    { Private declarations }
  protected
    procedure MouseDown(Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RBW', [TRbwCheckTreeView]);
end;

{ TRbwCheckTreeView }

procedure TRbwCheckTreeView.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ANode: TTreeNode;
  NewStateIndex: integer;
  procedure UpdateChildStateIndex(ANode: TTreeNode; NewStateIndex: integer);
  var
    ChildNode: TTreeNode;

  begin
    if ANode.STateIndex <> NewStateIndex then
    begin
      ANode.STateIndex := NewStateIndex;
      Change(ANode);
      ChildNode := ANode.GetFirstChild;
      while ChildNode <> nil do
      begin
        UpdateChildStateIndex(ChildNode, NewStateIndex);
        ChildNode := ChildNode.getNextSibling;
      end;
    end;

  end;
  procedure UpdateParentStateIndex(ANode: TTreeNode);
  var
    ParentNode: TTreeNode;
    ChildNode: TTreeNode;
    StateIndex: integer;
  begin
    ParentNode := ANode.Parent;
    if ParentNode <> nil then
    begin
      ChildNode := ParentNode.GetFirstChild;
      StateIndex := ChildNode.StateIndex;
      ChildNode := ChildNode.getNextSibling;
      while ChildNode <> nil do
      begin
        if ChildNode.StateIndex <> StateIndex then
        begin
          StateIndex := 3;
          break;
        end;
        ChildNode := ChildNode.getNextSibling;
      end;
      if ParentNode.StateIndex <> StateIndex then
      begin
        ParentNode.StateIndex := StateIndex;
        Change(ParentNode);
        UpdateParentStateIndex(ParentNode);
      end;
    end;
  end;
begin
  inherited;
  if (htOnStateIcon in GetHitTestInfoAt(X, Y)) then
  begin
    ANode := Selected;
    Assert(ANode <> nil);
    if ANode.StateIndex = 1 then
    begin
      NewStateIndex := 2;
    end
    else
    begin
      NewStateIndex := 1;
    end;
    UpdateChildStateIndex(ANode, NewStateIndex);
    UpdateParentStateIndex(ANode);
  end;
end;

end.
