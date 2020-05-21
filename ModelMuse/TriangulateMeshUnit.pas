unit TriangulateMeshUnit;

interface

uses
  MeshRenumberingTypes, System.Generics.Collections, System.Generics.Defaults,
  FastGEO, TriPackRoutines;

type
  TIsItemActiveEvent = procedure (Sender: TObject; Item: ITriangulatable;
    var IsActive: Boolean) of object;

  TTriangulateList = TList<ITriangulatable>;

  TMeshTriangulator = class(TObject)
  private
    FOnIsItemActive: TIsItemActiveEvent;
    FMesh: IMesh2D;
    // Arrange all the elements (except CenterElement) that share this
    // node in counterclockwise
    // order around the node starting at CenterElement;
    procedure ArrangeItemsAroundCenter(CenterItem: ITriangulatable;
      NeighborList: TTriangulateList; Center: ITriangulatable);
    procedure TriangulateItems(
      TTriangulatablesList: TTriangulateList; 
      var IADJ: TIntArray; var IEND: TIntArray);
  protected
    property Mesh: IMesh2D read FMesh;
  public
    constructor Create(AMesh: IMesh2D);
    property OnIsItemActive: TIsItemActiveEvent read FOnIsItemActive
      write FOnIsItemActive;
    procedure TriangulateNodes(var IADJ, IEND: TIntArray);
    procedure TriangulateElements(var IADJ, IEND: TIntArray);
  end;


implementation

uses
  System.Math;

type
  TTriangulatableComparer = class(TComparer<ITriangulatable>)
  private
    FCenterPostion: TPoint2D;
  public
    function Compare(const Left, Right: ITriangulatable): Integer; override;
    constructor Create(CenterPostion: TPoint2D);
  end;

  TTriangulatableNeighbors = class(TObject)
  private
    FCenterItem: ITriangulatable;
    FNeighbors: TObjectList<TTriangulateList>;
  public
    property CenterItem: ITriangulatable read FCenterItem write FCenterItem;
    property Neighbors: TObjectList<TTriangulateList> read FNeighbors;
    constructor Create;
    destructor Destroy; override;
  end;

  TTriangulateNeighborsList = TObjectList<TTriangulatableNeighbors>;

{ TMeshTriangulator }

constructor TMeshTriangulator.Create(AMesh: IMesh2D);
begin
  FMesh := AMesh;
end;

procedure TMeshTriangulator.TriangulateNodes(var IADJ, IEND: TIntArray);
var
  NIndex: Integer;
  Node: ITriangulatable;
  TTriangulatablesList: TTriangulateList;
  IsActive: Boolean;
begin
  Assert(Assigned(Mesh));
  Assert(Assigned(OnIsItemActive));

  TTriangulatablesList := TTriangulateList.Create;
  try  
    for NIndex := 0 to Mesh.NodeCount - 1 do
    begin
      Node := Mesh.NodesI2D[NIndex] as ITriangulatable;
      OnIsItemActive(Self, Node, IsActive);
      if IsActive then
      begin
        Node.TriangNumber := TTriangulatablesList.Add(Node) + 1;
      end;
    end;
    TriangulateItems(TTriangulatablesList, IADJ, IEND);
  finally
    TTriangulatablesList.Free;
  end;
end;

procedure TMeshTriangulator.TriangulateElements(var IADJ, IEND: TIntArray);
var
  EIndex: Integer;
  Element: ITriangulatable;
  TTriangulatablesList: TTriangulateList;
  IsActive: Boolean;
begin
  Assert(Assigned(Mesh));
  Assert(Assigned(OnIsItemActive));

  TTriangulatablesList := TTriangulateList.Create;
  try  
    for EIndex := 0 to Mesh.ElementCount - 1 do
    begin
      Element := Mesh.ElementsI2D[EIndex] as ITriangulatable;
      OnIsItemActive(Self, Element, IsActive);
      if IsActive then
      begin
        Element.TriangNumber := TTriangulatablesList.Add(Element)+1;
      end;
    end;
    TriangulateItems(TTriangulatablesList, IADJ, IEND);
  finally
    TTriangulatablesList.Free;
  end;
end;

procedure TMeshTriangulator.TriangulateItems(
  TTriangulatablesList: TTriangulateList; 
  var IADJ: TIntArray; var IEND: TIntArray);
var
  TriangulationNeighbors: TTriangulateNeighborsList;
  EdgeList: TTriangulateList;
  IsActive: Boolean;
  Neighbors: TTriangulatableNeighbors;
  EdgePointIndex: Integer;
  ItemComparer: TTriangulatableComparer;
  NeighborList: TTriangulateList;
  EdgePoint: ITriangulatable;
  IntList: TList<Integer>;
  IEndList: TList<Integer>;
  TriangulatableNeighborList: TList<Integer>;
  PriorItem: Integer;
  NeighborIndex: Integer;
  AnItem: ITriangulatable;
  MaxCount: Integer;
  MoveCount: Integer;
  FirstItem: ITriangulatable;
  LastItem: ITriangulatable;
  Item2: ITriangulatable;
  Item1: ITriangulatable;
  NextIndex: Integer;
  OtherItemIndex: Integer;
  OtherElement: ITriangulatable;
  ElStart: Integer;
  ElEnd: Integer;
  IntersectIndex: Integer;
  TestIndex: Integer;
  InnerElementIndex: Integer;
  ConnectTestIndex: Integer;
  ConnectedItem: ITriangulatable;
  OtherConnectedItem: ITriangulatable;
  Seg1: TSegment2D;
  Seg2: TSegment2D;
  IntIndex: Integer;
  TriangulationItemIndex: Integer;
  TriangulationItem: ITriangulatable;
begin
  TriangulationNeighbors := TTriangulateNeighborsList.Create;
  EdgeList := TTriangulateList.Create;
  try
    for TriangulationItemIndex := 0 to TTriangulatablesList.Count - 1 do
    begin
      TriangulationItem := TTriangulatablesList[TriangulationItemIndex];
      OnIsItemActive(Self, TriangulationItem, IsActive);
      Neighbors := TTriangulatableNeighbors.Create;
      TriangulationNeighbors.Add(Neighbors);
      Neighbors.CenterItem := TriangulationItem;
      EdgeList.Clear;
      if not IsActive then
      begin
        Continue;
      end;
      for EdgePointIndex := 0 to TriangulationItem.ItemCount - 1 do
      begin
        EdgeList.Add(TriangulationItem.ItemTri[EdgePointIndex]);
      end;
      ItemComparer := TTriangulatableComparer.Create(TriangulationItem.Location);
      try
        EdgeList.Sort(ItemComparer);
      finally
        ItemComparer.Free;
      end;
      for EdgePointIndex := 0 to EdgeList.Count - 1 do
      begin
        NeighborList := TTriangulateList.Create;
        Neighbors.Neighbors.Add(NeighborList);
        EdgePoint := EdgeList[EdgePointIndex];
        ArrangeItemsAroundCenter(TriangulationItem, NeighborList, EdgePoint);
      end;
    end;
    IntList := TList<Integer>.Create;
    IEndList := TList<Integer>.Create;
    TriangulatableNeighborList := TList<Integer>.Create;
    try
      for TriangulationItemIndex := 0 to TriangulationNeighbors.Count - 1 do
      begin
        Neighbors := TriangulationNeighbors[TriangulationItemIndex];
        TriangulationItem := Neighbors.CenterItem;
        TriangulatableNeighborList.Clear;
        if Neighbors.FNeighbors.Count = 0 then
        begin
          IntList.Add(0);
          IEndList.Add(IntList.Count);
          Continue;
        end;
        while (Neighbors.FNeighbors.Count > 0) and (Neighbors.FNeighbors.Last.Count = 0) do
        begin
          Neighbors.FNeighbors.Delete(Neighbors.FNeighbors.Count - 1);
        end;
        if Neighbors.FNeighbors.Count > 0 then
        begin
          PriorItem := Neighbors.FNeighbors.Last.Last.TriangNumber;
        end
        else
        begin
          PriorItem := -1;
        end;
        for EdgePointIndex := 0 to Neighbors.FNeighbors.Count - 1 do
        begin
          //          ANode := Element.NodesI[NodeIndex];
          NeighborList := Neighbors.FNeighbors[EdgePointIndex];
          for NeighborIndex := 0 to NeighborList.Count - 1 do
          begin
            AnItem := NeighborList[NeighborIndex];
            if (NeighborIndex = 0) and (PriorItem >= 0) then
            begin
              if (PriorItem <> AnItem.TriangNumber) then
              begin
                TriangulatableNeighborList.Add(-1);
                TriangulatableNeighborList.Add(AnItem.TriangNumber);
              end;
            end
            else
            begin
              TriangulatableNeighborList.Add(AnItem.TriangNumber);
            end;
            PriorItem := AnItem.TriangNumber;
          end;
          if (TriangulatableNeighborList.Count > 1)
            and (TriangulatableNeighborList.First = TriangulatableNeighborList.Last) then
          begin
            TriangulatableNeighborList.Delete(TriangulatableNeighborList.Count - 1);
          end;
        end;
        MaxCount := TriangulatableNeighborList.Count;
        MoveCount := 0;
        if (TriangulatableNeighborList.Count > 1) 
          and (TriangulatableNeighborList.First >= 1) 
          and (TriangulatableNeighborList.Last >= 1) then
        begin
          FirstItem := TTriangulatablesList[TriangulatableNeighborList.First - 1];
          LastItem := TTriangulatablesList[TriangulatableNeighborList.Last - 1];
          while FirstItem.IsNeighbor(LastItem) do
          begin
            TriangulatableNeighborList.Move(TriangulatableNeighborList.Count - 1, 0);
            if (TriangulatableNeighborList.First >= 1) 
              and (TriangulatableNeighborList.Last >= 1) then
            begin
              FirstItem := TTriangulatablesList[TriangulatableNeighborList.First - 1];
              LastItem := TTriangulatablesList[TriangulatableNeighborList.Last - 1];
              Inc(MoveCount);
              if MoveCount >= MaxCount then
              begin
                break;
              end;
            end
            else
            begin
              break;
            end;
          end;
        end;
        while (TriangulatableNeighborList.Count > 0) 
          and (TriangulatableNeighborList.First < 0) do
        begin
          TriangulatableNeighborList.Move(0, TriangulatableNeighborList.Count - 1);
        end;
        for NeighborIndex := TriangulatableNeighborList.Count - 1 downto 1 do
        begin
          if (TriangulatableNeighborList[NeighborIndex] >= 1) 
            and (TriangulatableNeighborList[NeighborIndex - 1] >= 1) then
          begin
            Item2 := TTriangulatablesList[TriangulatableNeighborList[NeighborIndex] - 1];
            Item1 := TTriangulatablesList[TriangulatableNeighborList[NeighborIndex - 1] - 1];
            if not Item1.IsNeighbor(Item2) then
            begin
              TriangulatableNeighborList.Insert(NeighborIndex, -1);
            end;
          end;
        end;
        if (TriangulatableNeighborList.Count > 1) 
          and (TriangulatableNeighborList.First >= 1) 
          and (TriangulatableNeighborList.Last >= 1) then
        begin
          FirstItem := TTriangulatablesList[TriangulatableNeighborList.First - 1];
          LastItem := TTriangulatablesList[TriangulatableNeighborList.Last - 1];
          if not FirstItem.IsNeighbor(LastItem) then
          begin
            TriangulatableNeighborList.Add(-1);
          end;
        end;
        for NeighborIndex := TriangulatableNeighborList.Count - 1 downto 1 do
        begin
          if TriangulatableNeighborList[NeighborIndex] < 0 then
          begin
            while TriangulatableNeighborList[NeighborIndex - 1] - 1 > 0 do
            begin
              AnItem := TTriangulatablesList[TriangulatableNeighborList[NeighborIndex - 1] - 1];
              if not AnItem.IsNeighbor(TriangulationItem) then
              begin
                TriangulatableNeighborList.Delete(NeighborIndex - 1);
              end
              else
              begin
                break;
              end;
            end;
          end;
        end;
        for NeighborIndex := TriangulatableNeighborList.Count - 1 downto 0 do
        begin
          if (NeighborIndex < TriangulatableNeighborList.Count) 
            and (TriangulatableNeighborList[NeighborIndex] < 0) then
          begin
            while True do
            begin
              NextIndex := NeighborIndex + 1;
              if NextIndex >= TriangulatableNeighborList.Count then
              begin
                NextIndex := 0;
              end;
              if TriangulatableNeighborList[NextIndex] >= 1 then
              begin
                AnItem := TTriangulatablesList[TriangulatableNeighborList[NextIndex] - 1];
                if not AnItem.IsNeighbor(TriangulationItem) then
                begin
                  TriangulatableNeighborList.Delete(NextIndex);
                end
                else
                begin
                  break;
                end;
              end
              else
              begin
                Break;
              end;
            end;
          end;
        end;
        for NeighborIndex := TriangulatableNeighborList.Count - 1 downto 0 do
        begin
          if (NeighborIndex < TriangulatableNeighborList.Count) 
            and (TriangulatableNeighborList.Count > 1) then
          begin
            NextIndex := NeighborIndex + 2;
            if NextIndex >= TriangulatableNeighborList.Count then
            begin
              NextIndex := NextIndex - TriangulatableNeighborList.Count;
            end;
            if (TriangulatableNeighborList[NeighborIndex] < 0) 
              and (TriangulatableNeighborList[NextIndex] < 0) then
            begin
              TriangulatableNeighborList.Delete(NextIndex);
              Dec(NextIndex);
              if NextIndex < 0 then
              begin
                NextIndex := TriangulatableNeighborList.Count - 1;
              end;
              TriangulatableNeighborList.Delete(NextIndex);
            end;
          end;
        end;
        for NeighborIndex := TriangulatableNeighborList.Count - 1 downto 1 do
        begin
          if (TriangulatableNeighborList[NeighborIndex] < 0) 
            and (TriangulatableNeighborList[NeighborIndex - 1] < 0) then
          begin
            TriangulatableNeighborList.Delete(NeighborIndex);
          end;
        end;
        for NeighborIndex := TriangulatableNeighborList.Count - 1 downto 0 do
        begin
          if (NeighborIndex < TriangulatableNeighborList.Count)
            and (TriangulatableNeighborList[NeighborIndex] > 0) then
          begin
            OtherItemIndex := TriangulatableNeighborList[NeighborIndex] - 1;
            if OtherItemIndex < TriangulationItemIndex then
            begin
              OtherElement := TTriangulatablesList[OtherItemIndex];
              if OtherItemIndex = 0 then
              begin
                ElStart := 0;
              end
              else
              begin
                ElStart := IEndList[OtherItemIndex - 1];
              end;
              ElEnd := IEndList[OtherItemIndex];
              for IntersectIndex := ElStart to ElEnd - 1 do
              begin
                TestIndex := IntList[IntersectIndex];
                if (TestIndex > 0) and (TestIndex <> TriangulationItem.TriangNumber)
                  and (TriangulatableNeighborList.IndexOf(TestIndex) >= 0) then
                begin
                  for InnerElementIndex := TriangulatableNeighborList.Count - 1 downto 0 do
                  begin
                    if InnerElementIndex = NeighborIndex then
                    begin
                      Continue;
                    end;
                    ConnectTestIndex := TriangulatableNeighborList[InnerElementIndex];
                    if (ConnectTestIndex > 0) and (ConnectTestIndex <> TestIndex) 
                      and (ConnectTestIndex <> OtherElement.TriangNumber) then
                    begin
                      ConnectedItem := TTriangulatablesList[TestIndex - 1];
                      OtherConnectedItem := TTriangulatablesList[ConnectTestIndex - 1];
                      Seg1[1] := TriangulationItem.Location;
                      Seg1[2] := OtherConnectedItem.Location;
                      Seg2[1] := OtherElement.Location;
                      Seg2[2] := ConnectedItem.Location;
                      if Intersect(Seg1, Seg2) then
                      begin
                        TriangulatableNeighborList.Remove(ConnectTestIndex);
                        break;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
        for NeighborIndex := 0 to TriangulatableNeighborList.Count - 1 do
        begin
          if TriangulatableNeighborList[NeighborIndex] > 0 then
          begin
            IntList.Add(TriangulatableNeighborList[NeighborIndex]);
          end
          else
          begin
            IntList.Add(0);
          end;
        end;
        if TriangulatableNeighborList.Count = 0 then
        begin
          IntList.Add(0);
        end;
        IEndList.Add(IntList.Count);
      end;
      SetLength(IADJ, IntList.Count);
      for IntIndex := 0 to IntList.Count - 1 do
      begin
        IADJ[IntIndex] := IntList[IntIndex];
      end;
      SetLength(IEnd, IEndList.Count);
      for IntIndex := 0 to IEndList.Count - 1 do
      begin
        IEnd[IntIndex] := IEndList[IntIndex];
      end;
    finally
      IntList.Free;
      IEndList.Free;
      TriangulatableNeighborList.Free;
    end;
  finally
    TriangulationNeighbors.Free;
    EdgeList.Free;
  end;
end;

procedure TMeshTriangulator.ArrangeItemsAroundCenter(CenterItem: ITriangulatable;
  NeighborList: TTriangulateList; Center: ITriangulatable);
var
  ItemIndex: Integer;
  AnItem:  ITriangulatable;
  ItemComparer: TTriangulatableComparer;
  IsActive: Boolean;
begin
  for ItemIndex := 0 to Center.ItemCount - 1 do
  begin
    AnItem := Center.ItemTri[ItemIndex];
    OnIsItemActive(Self, AnItem, IsActive);
    if IsActive then
    begin
      NeighborList.Add(AnItem);
    end;
  end;
  ItemComparer := TTriangulatableComparer.Create(Center.Location);
  try
    NeighborList.Sort(ItemComparer);
  finally
    ItemComparer.Free;
  end;
  Assert(NeighborList.IndexOf(CenterItem) >= 0);
  while NeighborList.Last <> CenterItem do
  begin
    NeighborList.Move(0, NeighborList.Count - 1);
  end;
  NeighborList.Delete(NeighborList.Count - 1);
end;

{ TTriangulatableComparer }

function TTriangulatableComparer.Compare(const Left,
  Right: ITriangulatable): Integer;
var
  Location1: TPoint2D;
  Angle1: Double;
  Location2: TPoint2D;
  Angle2: Double;
begin
  Location1 := Left.Location;
  Angle1 := ArcTan2(Location1.y-FCenterPostion.y, Location1.x-FCenterPostion.x);
  Location2 := Right.Location;
  Angle2 := ArcTan2(Location2.y-FCenterPostion.y, Location2.x-FCenterPostion.x);
  result := Sign(Angle1-Angle2);
end;

constructor TTriangulatableComparer.Create(CenterPostion: TPoint2D);
begin
  FCenterPostion := CenterPostion;
end;

{ TTriangulatableNeighbors }

constructor TTriangulatableNeighbors.Create;
begin
  FNeighbors := TObjectList<TTriangulateList>.Create
end;

destructor TTriangulatableNeighbors.Destroy;
begin
  FNeighbors.Free;
  inherited;
end;

end.
