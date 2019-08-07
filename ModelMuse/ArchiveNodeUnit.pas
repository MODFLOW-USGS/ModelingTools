unit ArchiveNodeUnit;

interface

uses
  ArchiveNodeInterface, System.Generics.Collections;

type
  TArchiveNode = class(TInterfacedObject, IArchiveNodeInterface)
  private
    FNodeText: string;
    FNodeType: TNodeType;
    FChildren: TObjectList<TArchiveNode>;
    FParentNode: IArchiveNodeInterface;
    FModelDirectory: string;
    FDescription: string;
    function GetParentNode: IArchiveNodeInterface;
    function GetNodeText: string;
    function GetNodeType: TNodeType;
    function GetCount: Integer;
    function GetChild(Index: Integer): IArchiveNodeInterface;
    procedure SetNodeType(const Value: TNodeType);
    procedure SetParentNode(const Value: IArchiveNodeInterface);
    procedure SetNodeText(const Value: string);
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function GetModelDirectory: string;
    procedure SetModelDirectory(const Value: string);
    procedure SetDescription(const Value: string);
    function GetDescription: string;
  public
    constructor Create;
    destructor Destroy; override;
    function AddChild: TArchiveNode;
    property ParentNode: IArchiveNodeInterface read GetParentNode
      write SetParentNode;
    property NodeText: string read GetNodeText write SetNodeText;
    property ModelDirectory: string read GetModelDirectory write SetModelDirectory;
    property NodeType: TNodeType read GetNodeType write SetNodeType;
    property Count: Integer read GetCount;
    property Children[Index: Integer]: IArchiveNodeInterface read GetChild;
    property Description: string read GetDescription write SetDescription;
  end;

implementation

{ TArchiveNode }

function TArchiveNode.AddChild: TArchiveNode;
begin
  Result := TArchiveNode.Create;
  FChildren.Add(Result);
  Result.ParentNode := self;
end;

constructor TArchiveNode.Create;
begin
  inherited;
  FChildren := TObjectList<TArchiveNode>.Create;
end;

destructor TArchiveNode.Destroy;
begin
  FChildren.Free;
  inherited;
end;

function TArchiveNode.GetChild(Index: Integer): IArchiveNodeInterface;
begin
  result := FChildren[Index];
end;

function TArchiveNode.GetCount: Integer;
begin
  result := FChildren.Count;
end;

function TArchiveNode.GetDescription: string;
begin
  result := FDescription;
end;

function TArchiveNode.GetModelDirectory: string;
begin
  Result := FModelDirectory;
end;

function TArchiveNode.GetNodeText: string;
begin
  result := FNodeText;
end;

function TArchiveNode.GetNodeType: TNodeType;
begin
  result := FNodeType;
end;

function TArchiveNode.GetParentNode: IArchiveNodeInterface;
begin
  result := FParentNode;
end;

procedure TArchiveNode.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TArchiveNode.SetModelDirectory(const Value: string);
begin
  FModelDirectory := Value;
end;

procedure TArchiveNode.SetNodeText(const Value: string);
begin
  FNodeText := Value;
end;

procedure TArchiveNode.SetNodeType(const Value: TNodeType);
begin
  FNodeType := Value;
end;

procedure TArchiveNode.SetParentNode(const Value: IArchiveNodeInterface);
begin
  FParentNode := Value;
end;

function TArchiveNode._AddRef: Integer;
begin
  Result := 1;
  // do nothing;
end;

function TArchiveNode._Release: Integer;
begin
  result := 1;
  // do nothing;
end;

end.
