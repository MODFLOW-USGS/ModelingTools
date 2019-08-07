unit frameScreenObjectCfpFixedUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frameScreenObjectUnit, StdCtrls,
  ExtCtrls, UndoItemsScreenObjects;

type
  TframeScreenObjectCfpFixed = class(TframeScreenObject)
    pnlCaption: TPanel;
    edFixedHead: TLabeledEdit;
    btnFixedHead: TButton;
    lblHint: TLabel;
    procedure edFixedHeadChange(Sender: TObject);
  private
    FChanging: Boolean;
    FOnChange: TNotifyEvent;
    property Changing: Boolean read FChanging write FChanging;
    procedure DoChange;
    procedure InitializeControls;
    { Private declarations }
  public
    procedure GetData(ScreenObjectList: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    { Public declarations }
  end;

var
  frameScreenObjectCfpFixed: TframeScreenObjectCfpFixed;

implementation

uses
  ScreenObjectUnit, ModflowCfpFixedUnit;

{$R *.dfm}

{ TframeScreenObjectCfpFixed }

procedure TframeScreenObjectCfpFixed.DoChange;
begin
  if Changing then
  begin
    Exit;
  end;
  if Assigned(OnChange) then
  begin
    OnChange(Self);
  end;
end;

procedure TframeScreenObjectCfpFixed.edFixedHeadChange(Sender: TObject);
begin
  inherited;
  DoChange;
end;

procedure TframeScreenObjectCfpFixed.GetData(
  ScreenObjectList: TScreenObjectEditCollection);
var
  ListOfScreenObjects: TScreenObjectList;
  Index: Integer;
  Item: TScreenObjectEditItem;
  AScreenObject: TScreenObject;
  ABoundary: TCfpFixedBoundary;
  ScreenObjectIndex: Integer;
begin
  Assert(ScreenObjectList.Count >= 1);
  Changing := True;
  try
    InitializeControls;
    ListOfScreenObjects := TScreenObjectList.Create;
    try
      for Index := 0 to ScreenObjectList.Count - 1 do
      begin
        Item := ScreenObjectList[Index];
        AScreenObject := Item.ScreenObject;
        ABoundary := AScreenObject.ModflowCfpFixedHeads;
        if (ABoundary <> nil) and ABoundary.Used then
        begin
          ListOfScreenObjects.Add(AScreenObject);
        end;
      end;
      if ListOfScreenObjects.Count > 0 then
      begin
        ABoundary := ListOfScreenObjects[0].ModflowCfpFixedHeads;
        edFixedHead.Text := ABoundary.FixedHead;
      end;
      for ScreenObjectIndex := 1 to ListOfScreenObjects.Count - 1 do
      begin
        ABoundary := ListOfScreenObjects[ScreenObjectIndex].ModflowCfpFixedHeads;
        if edFixedHead.Text <> ABoundary.FixedHead then
        begin
          edFixedHead.Text := ''
        end;
      end;
    finally
      ListOfScreenObjects.Free;
    end;
  finally
    Changing := False;
  end;
end;

procedure TframeScreenObjectCfpFixed.InitializeControls;
begin
  edFixedHead.Text := '';
end;

procedure TframeScreenObjectCfpFixed.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
var
  ScreenObjectIndex: Integer;
  Item: TScreenObjectEditItem;
  ScreenObject: TScreenObject;
  Boundary: TCfpFixedBoundary;
  BoundaryUsed: Boolean;
begin
  for ScreenObjectIndex := 0 to List.Count - 1 do
  begin
    Item := List[ScreenObjectIndex];
    ScreenObject := Item.ScreenObject;
    Boundary := ScreenObject.ModflowCfpFixedHeads;
    BoundaryUsed := (Boundary <> nil) and Boundary.Used;
    if ClearAll then
    begin
      if BoundaryUsed then
      begin
        Boundary.IsUsed := False;
      end;
    end
    else if SetAll or BoundaryUsed then
    begin
      if Boundary = nil then
      begin
        ScreenObject.CreateCfpFixedHeads;
        Boundary := ScreenObject.ModflowCfpFixedHeads;
      end;
      if Boundary <> nil then
      begin
        Boundary.IsUsed := True;
        if edFixedHead.Text <> '' then
        begin
          Boundary.FixedHead := edFixedHead.Text;
        end;
      end;
    end;
  end;
end;

end.
