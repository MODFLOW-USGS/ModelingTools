unit frmFishnetElementPropertiesUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, Grids,
  RbwDataGrid4, StdCtrls, ArgusDataEntry, Buttons, FishnetMeshGenerator,
  ComCtrls, frameDiscretizationUnit, UndoItems,
  GrayTabs;

type
  TUndoFishnetElementProperties = class(TCustomUndo)
  private
    FOldProperties: TFishnetMeshGenerator;
    FNewProperties: TFishnetMeshElement;
    FElement: TFishnetMeshElement;
    FOldNodes: TFishnetNodeList;
    FNewNodes: TFishnetNodeList;
  protected
    function Description: string; override;
  public
    constructor Create(AnElement: TFishnetMeshElement;
      var NewElement: TFishnetMeshElement;
      var OldNodes, NewNodes: TFishnetNodeList);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

  TfrmFishnetElementProperties = class(TfrmCustomGoPhast)
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    pc1: TPageControl;
    tabFirst: TTabSheet;
    tabSecond: TTabSheet;
    tabCornerCoordinates: TTabSheet;
    rdgCornerCoordinates: TRbwDataGrid4;
    frameDiscretization1: TframeDiscretization;
    frameDiscretization2: TframeDiscretization;
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject); override;
  private
    FElement: TFishnetMeshElement;
    FExistingElement: TFishnetMeshElement;
    { Private declarations }
    procedure SetData;
  public
    procedure GetData(Element: TFishnetMeshElement);
    { Public declarations }
  end;

var
  frmFishnetElementProperties: TfrmFishnetElementProperties;

implementation

uses
  frmGoPhastUnit;

resourcestring
  StrChangeFishnetQuadr = 'change fishnet quadrilateral properties';
  StrBoundaryPosition = 'Boundary Position';

{$R *.dfm}

procedure TfrmFishnetElementProperties.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmFishnetElementProperties.FormCreate(Sender: TObject);
begin
  inherited;
  rdgCornerCoordinates.Cells[0,0] := 'X';
  rdgCornerCoordinates.Cells[1,0] := 'Y';
  FElement := TFishnetMeshElement.Create(nil);
  pc1.ActivePage := tabFirst;
end;

procedure TfrmFishnetElementProperties.FormDestroy(Sender: TObject);
begin
  inherited;
  FElement.Free;
end;

procedure TfrmFishnetElementProperties.GetData(Element: TFishnetMeshElement);
var
  NodeIndex: Integer;
  ANode: TFishnetMeshNode;
  List: TList;
begin
  Assert(Assigned(Element));
  FExistingElement := Element;
  FElement.Assign(Element);

  frameDiscretization1.rdgSubLayerBoundaries.Cells[0,0] := StrBoundaryPosition;
  frameDiscretization2.rdgSubLayerBoundaries.Cells[0,0] := StrBoundaryPosition;

//  seFirstCount.AsInteger := FElement.FirstControl.Count;
//  seSecondCount.AsInteger := FElement.SecondControl.Count;
  rdgCornerCoordinates.BeginUpdate;
  try
    for NodeIndex := 0 to Element.Nodes.Count - 1 do
    begin
      ANode := Element.Nodes[NodeIndex];
      rdgCornerCoordinates.Cells[0,NodeIndex+1] := FloatToStr(ANode.X);
      rdgCornerCoordinates.Cells[1,NodeIndex+1] := FloatToStr(ANode.Y);
    end;
  finally
    rdgCornerCoordinates.EndUpdate;
  end;
  List := TList.Create;
  try
    List.Add(FElement.FirstControl);
    frameDiscretization1.UpdateSelectedUnits(List);
    frameDiscretization1.SetControlValues;
    List.Clear;
    List.Add(FElement.SecondControl);
    frameDiscretization2.UpdateSelectedUnits(List);
    frameDiscretization2.SetControlValues;
  finally
    List.Free;
  end;
end;

procedure TfrmFishnetElementProperties.SetData;
var
  X: double;
  Y: double;
  NodeIndex: Integer;
  ANode: TFishnetMeshNode;
  Generator: TFishnetMeshGenerator;
  OldNodes, NewNodes: TFishnetNodeList;
begin
  OldNodes := TFishnetNodeList.Create;
  NewNodes := TFishnetNodeList.Create;
  try
    Generator := (FExistingElement.Collection
      as TFishnetMeshElementCollection).Generator;
  //  FElement.FirstControl.Count := seFirstCount.AsInteger;
  //  FElement.SecondControl.Count := seSecondCount.AsInteger;
    for NodeIndex := 1 to rdgCornerCoordinates.RowCount - 1 do
    begin
      if TryStrToFloat(rdgCornerCoordinates.Cells[0,NodeIndex], X)
      and TryStrToFloat(rdgCornerCoordinates.Cells[1,NodeIndex], Y) then
      begin
        if NodeIndex <= FElement.Nodes.Count then
        begin
          ANode := FElement.Nodes[NodeIndex-1];
        end
        else
        begin
          ANode := Generator.Nodes.Add;
          FElement.Nodes.Add(ANode);
        end;
        ANode.X := X;
        ANode.Y := Y;
        OldNodes.Add(FExistingElement.Nodes[NodeIndex-1]);
        NewNodes.Add(ANode);
      end;
    end;
    frmGoPhast.UndoStack.Submit(TUndoFishnetElementProperties.
      Create(FExistingElement, FElement, OldNodes, NewNodes));
  //  Generator.UpdateCount1(FElement);
  //  Generator.UpdateCount2(FElement);
    frmGoPhast.InvalidateTop;
  finally
    OldNodes.Free;
    NewNodes.Free;
  end;
end;

{ TUndoFishnetElementProperties }

constructor TUndoFishnetElementProperties.Create(
  AnElement: TFishnetMeshElement; var NewElement: TFishnetMeshElement;
  var OldNodes, NewNodes: TFishnetNodeList);
begin
  FOldProperties := TFishnetMeshGenerator.Create(nil);
  FOldProperties.Assign(frmGoPhast.PhastModel.FishnetMeshGenerator);
  FNewProperties := NewElement;
  NewElement := nil;
  FElement := AnElement;
  FOldNodes := OldNodes;
  OldNodes := nil;
  FNewNodes := NewNodes;
  NewNodes := nil;
  Assert(FOldNodes.Count = FNewNodes.Count);
end;

function TUndoFishnetElementProperties.Description: string;
begin
  Result := StrChangeFishnetQuadr;
end;

destructor TUndoFishnetElementProperties.Destroy;
begin
  FOldNodes.Free;
  FNewNodes.Free;
  FOldProperties.Free;
  FNewProperties.Free;
  inherited;
end;

procedure TUndoFishnetElementProperties.DoCommand;
var
  NodeIndex: Integer;
begin
  inherited;
  FElement.Assign(FNewProperties);
  FElement.Generator.UpdateCount1(FElement);
  FElement.Generator.UpdateCount2(FElement);
  for NodeIndex := 0 to FOldNodes.Count - 1 do
  begin
    FOldNodes[NodeIndex].Assign(FNewNodes[NodeIndex]);
  end;
end;

procedure TUndoFishnetElementProperties.Undo;
begin
  inherited;
  frmGoPhast.PhastModel.FishnetMeshGenerator := FOldProperties;
end;

end.
