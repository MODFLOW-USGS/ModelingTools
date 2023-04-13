unit CrossSectionUnit;

interface

uses
  Vcl.Graphics, System.Generics.Collections, System.Classes, System.Types,
  System.Math,
  ZoomBox2, RbwParser,
  GoPhastTypes, ModflowOptionsUnit, DrawMeshTypesUnit, MeshRenumberingTypes,
  DataSetUnit, AbstractGridUnit, SubscriptionUnit;

type
  ICustomModelInterfaceForCrosssection = interface(IModelMuseModel)
    function GetModflowOptions: TModflowOptions;
    procedure SetModflowOptions(const Value: TModflowOptions);
    property ModflowOptions: TModflowOptions read GetModflowOptions
      write SetModflowOptions;
    function GetDrawMesh: IDrawMesh;
    property DrawMesh: IDrawMesh read GetDrawMesh;
    function GetMesh3D: IMesh3D;
    property Mesh3D: IMesh3D read GetMesh3D;
    function GetDataSetByName(const DataSetName: string): TDataArray;
    function GetGrid: TCustomModelGrid;
    property Grid: TCustomModelGrid read GetGrid;
  end;

  TCrossSection = class(TObserver)
  private
    FLayersToUse: TGenericIntegerList;
    FDataArrays: TDataArrayObjectList;
    FAllLayers: boolean;
    FModel: ICustomModelInterfaceForCrosssection;
    FColors: TList<TColor>;
    FLineThickness: Integer;
    procedure SetAllLayers(const Value: boolean);
    procedure SetDataArrays(const Value: TDataArrayObjectList);
    procedure SetLayersToUse(const Value: TList<integer>);
    procedure DataArrayChanged (Sender: TObject; const Item: TDataArray;
      Action: TCollectionNotification);
    procedure LayerChanged (Sender: TObject; const Item: integer;
      Action: TCollectionNotification);
    procedure SetColors(const Value: TList<TColor>);
    procedure ColorChanged (Sender: TObject; const Item: TColor;
      Action: TCollectionNotification);
    procedure SetLineThickness(const Value: Integer);
  public
    Constructor Create(AModel: ICustomModelInterfaceForCrosssection); reintroduce;
    destructor Destroy; override;
    procedure Assign(Source: TCrossSection); reintroduce;
    procedure Clear;
    property AllLayers: boolean read FAllLayers write SetAllLayers;
    property LayersToUse: TList<integer> read FLayersToUse write SetLayersToUse;
    property DataArrays: TDataArrayObjectList read FDataArrays write SetDataArrays;
    property Colors: TList<TColor> read FColors write SetColors;
    procedure Draw(ABitMap: TPersistent; ViewDirection: TViewDirection);
    procedure RemoveDataArray(ADataArray: TDataArray);
    property LineThickness: Integer read FLineThickness write SetLineThickness default 1;
  end;



implementation

uses
  System.Generics.Defaults, GR32, FastGEO, frmGoPhastUnit, DataSetNamesUnit,
  BigCanvasMethods, PhastModelUnit;

function NearlyTheSame(const X, Y, Epsilon: extended): boolean;
begin
  result := (X = Y) or (Abs(X - Y) < Epsilon) or
    (Abs(X - Y) / (Abs(X) + Abs(Y) + Epsilon) < Epsilon);
end;



{ TCrossSection }

procedure TCrossSection.Assign(Source: TCrossSection);
begin
  AllLayers := Source.AllLayers;
  LayersToUse := Source.LayersToUse;
  DataArrays := Source.DataArrays;
  Colors := Source.Colors;
  LineThickness := Source.LineThickness;
end;

procedure TCrossSection.Clear;
begin
  FAllLayers := True;
  FColors.Clear;
  FLayersToUse.Clear;
  FDataArrays.Clear;
  LineThickness := 1;
end;

procedure TCrossSection.ColorChanged(Sender: TObject; const Item: TColor;
  Action: TCollectionNotification);
begin
  UpToDate := False;
end;

constructor TCrossSection.Create(AModel: ICustomModelInterfaceForCrosssection);
begin
  inherited Create(nil);
  FModel := AModel;
  FLayersToUse := TGenericIntegerList.Create;
  FLayersToUse.OnNotify := LayerChanged;

  FDataArrays := TDataArrayObjectList.Create;
  FDataArrays.OwnsObjects := False;
  FDataArrays.OnNotify := DataArrayChanged;

  FColors := TList<TColor>.Create;
  FColors.OnNotify := ColorChanged;

  FLineThickness := 1;

  FAllLayers := True;
end;

procedure TCrossSection.DataArrayChanged(Sender: TObject;
  const Item: TDataArray; Action: TCollectionNotification);
begin
  case Action of
    cnAdded: Item.TalksTo(self);
    cnRemoved, cnExtracted: Item.StopsTalkingTo(self);
    else Assert(False);
  end;
end;

destructor TCrossSection.Destroy;
begin
  FColors.Free;
  FLayersToUse.Free;
  FDataArrays.Free;
  inherited;
end;

procedure TCrossSection.Draw(ABitMap: TPersistent; ViewDirection: TViewDirection);
const
  Epsilon = 1e-6;
//  Mf2005HnoFlo = 1E30;
//  Mf2005HDry = -1E30;
var
  ADataArray: TDataArray;
  ActiveDataArray: TDataArray;
  DataArrayIndex: Integer;
  LayerIndex: Integer;
  AColor: TColor;
  IDomain: TDataArray;
//  AModel: TCustomModel;
  function UseCell(Layer, Row, Col: Integer; Orientation: TDataSetOrientation): boolean;
  var
    LayerIndex: Integer;
    Value: double;
  begin
    result := False;
    case Orientation of
      dsoTop:
        begin
          if IDomain <> nil then
          begin
            Value := ADataArray.RealData[0,Row,Col];
            if (Not NearlyTheSame(Value, FModel.ModflowOptions.HNoFlow, Epsilon))
              and (Not NearlyTheSame(Value, FModel.ModflowOptions.HDry, Epsilon)) then
            begin
              result := True;
              Exit;
            end;
          end
          else if Not NearlyTheSame(ADataArray.RealData[0,Row,Col],
            FModel.ModflowOptions.HDry, Epsilon) then
          begin
            if ActiveDataArray = nil then
            begin
              result := True;
              Exit;
            end
            else
            begin
              for LayerIndex := 0 to ActiveDataArray.LayerCount - 1 do
              begin
                if ActiveDataArray.BooleanData[LayerIndex, Row, Col] then
                begin
                  result := True;
                  Exit;
                end;
              end;
            end;
          end;
        end;
      dso3D:
        begin
          if IDomain <> nil then
          begin
            Value := ADataArray.RealData[Layer,Row,Col];
            if (Not NearlyTheSame(Value, FModel.ModflowOptions.HNoFlow, Epsilon))
              and (Not NearlyTheSame(Value, FModel.ModflowOptions.HDry, Epsilon)) then
            begin
              result := True;
              Exit;
            end;
          end
          else if ActiveDataArray = nil then
          begin
            result := True;
            Exit;
          end
          else if ActiveDataArray.BooleanData[Layer, Row, Col] then
          begin
            if FModel.ModelSelection in ModflowSelection then
            begin
              if not NearlyTheSame(ADataArray.RealData[Layer,Row,Col],
                FModel.ModflowOptions.HDry, Epsilon) then
              begin
                result := True;
                Exit;
              end;
            end
            else
            begin
              result := True;
              Exit;
            end;
          end
          else
          begin
            result := False;
          end;
        end
      else Assert(False);
    end;
  end;
  procedure DrawACrossSectionLine(LayerIndex: integer; AColor: TColor);
  var
    Points: array of TPoint;
    ColIndex: Integer;
    ARow: Integer;
    DrawLine: Boolean;
    APoint: TPoint;
    StartIndex: integer;
    NumPoints: Integer;
    LastIndex: Integer;
    ACol: Integer;
    RowIndex: Integer;
    ZoomBox: TQRbwZoomBox2;
    CellList: TIElement2DList;
    CellIndex: Integer;
    ACell2D: IElement2D;
    ClosestPoint: TPoint2D;
    OriginOffset: double;
//    OffSet: double;
    ADistance: double;
    CrossSectionSegment: TSegment2D;
    DrawMesh: IDrawMesh;
    SegmentLine: TLine2D;
    SegmentAngle: double;
    OriginAngle: Double;
    procedure ComputeCrossSectionDistance(ALine: TLine2D; ALocation: TPoint2D);
    var
      ClosestPoint: TPoint2D;
      PointAngle: Double;
    begin
      ClosestPoint := ClosestPointOnLineFromPoint(
        ALine, ALocation);
      ADistance := {-}Distance(ALine[1], ClosestPoint);
      if ADistance <> 0 then
      begin
        PointAngle := ArcTan2(ALine[1].Y - ClosestPoint.y, ALine[1].x - ClosestPoint.x);
  //      if FastGEO.Orientation(ALocation,
  //        CrossSectionSegment[1], CrossSectionSegment[2]) =
  //        LeftHandSide then
        if Abs(SegmentAngle - PointAngle) > 0.001 then
        begin
          ADistance := -ADistance;
        end;
      end;
  //    ADistance := ADistance + OffSet;
    end;
  begin
    case ViewDirection of
      vdFront:
        begin
          ZoomBox := frmGoPhast.frameFrontView.ZoomBox;
          StartIndex := 0;
          LastIndex := -1;
          if (FModel.Grid <> nil) then
          begin
            ARow := FModel.Grid.SelectedRow;
            SetLength(Points, FModel.Grid.ColumnCount);
            for ColIndex := 0 to FModel.Grid.ColumnCount - 1 do
            begin
              DrawLine := False;
              if UseCell(LayerIndex, ARow, ColIndex, ADataArray.Orientation)  then
              begin
                APoint.x := ZoomBox.XCoord(FModel.Grid.ColumnCenter(ColIndex));
                APoint.y := ZoomBox.YCoord(ADataArray.RealData[LayerIndex,ARow,ColIndex]);
                Points[ColIndex] := APoint;
                LastIndex := ColIndex;
              end
              else
              begin
                DrawLine := True;
              end;
              if DrawLine or (ColIndex = FModel.Grid.ColumnCount - 1) then
              begin
                NumPoints := LastIndex-StartIndex+1;
                if NumPoints > 1 then
                begin
                  DrawBigPolyline32(ABitMap, Color32(AColor), LineThickness, Points, True, False,
                    StartIndex, NumPoints);
                end;
                StartIndex := Succ(ColIndex);
              end;
            end;
          end
          else
          begin
            DrawMesh := FModel.DrawMesh;
            CrossSectionSegment := DrawMesh.CrossSection.Segment;
            SegmentLine := EquateLine(CrossSectionSegment[1],CrossSectionSegment[2]);
            SegmentAngle := ArcTan2(SegmentLine[1].Y - SegmentLine[2].y, SegmentLine[1].x - SegmentLine[2].x);

            ClosestPoint := ClosestPointOnLineFromPoint(
              SegmentLine, EquatePoint(0.0, 0.0));
            OriginOffset := Distance(ClosestPoint, CrossSectionSegment[1]);
            if OriginOffset <> 0 then
            begin
              OriginAngle := ArcTan2(SegmentLine[1].Y - ClosestPoint.y, SegmentLine[1].x - ClosestPoint.x);
              if Abs(SegmentAngle - OriginAngle) > 0.001 then
              {if FastGEO.Orientation(EquatePoint(0.0, 0.0),
                CrossSectionSegment[1], CrossSectionSegment[2]) =
                LeftHandSide then}
              begin
                OriginOffset := -OriginOffset;
              end;
            end;
//            OffSet := Distance(CrossSectionSegment[1], ClosestPoint);
//            if OffSet <> 0 then
//            begin
//              OriginAngle := ArcTan2(SegmentLine[1].Y - ClosestPoint.y, SegmentLine[1].x - ClosestPoint.x);
//              if Abs(SegmentAngle - OriginAngle) > 0.001 then
//              {if FastGEO.Orientation(EquatePoint(0.0, 0.0),
//                CrossSectionSegment[1], CrossSectionSegment[2]) =
//                LeftHandSide then}
//              begin
//                OffSet := -OffSet;
//              end;
//            end;

            CellList := TIElement2DList.Create;
            try
              FModel.Mesh3D.GetElementsIntfOnCrossSection(CellList);
              SetLength(Points, CellList.Count);

              CellList.Sort(TComparer<IElement2D>.Construct(
                function (const L, R: IElement2D): integer
                begin
                  result := Sign(Distance(CrossSectionSegment[1], L.Center)
                    - Distance(CrossSectionSegment[1], R.Center));
                end
                ));

              for CellIndex := 0 to CellList.Count -1 do
              begin
                ACell2D := CellList[CellIndex];
                DrawLine := False;
                if UseCell(LayerIndex, 0, ACell2D.ElementNumber, ADataArray.Orientation)  then
                begin
                  ComputeCrossSectionDistance(SegmentLine, ACell2D.Center);
                  APoint.x := ZoomBox.XCoord(ADistance - OriginOffset);
                  APoint.y := ZoomBox.YCoord(ADataArray.RealData[LayerIndex,0,ACell2D.ElementNumber]);
                  Points[CellIndex] := APoint;
                  LastIndex := CellIndex;
                end
                else
                begin
                  DrawLine := True;
                end;
                if DrawLine or (CellIndex = CellList.Count - 1) then
                begin
                  NumPoints := LastIndex-StartIndex+1;
                  if NumPoints > 1 then
                  begin
                    DrawBigPolyline32(ABitMap, Color32(AColor), LineThickness, Points, True, False,
                      StartIndex, NumPoints);
                  end;
                  StartIndex := Succ(CellIndex);
                end;
              end;
            finally
              CellList.Free;
            end;
          end;
        end;
      vdSide:
        begin
          if FModel.Grid = nil then
          begin
            Exit;
          end;
          ZoomBox := frmGoPhast.frameSideView.ZoomBox;
          StartIndex := 0;
          LastIndex := -1;

          ACol := FModel.Grid.SelectedColumn;
          SetLength(Points, FModel.Grid.RowCount);
          for RowIndex := 0 to FModel.Grid.RowCount - 1 do
          begin
            DrawLine := False;
            if UseCell(LayerIndex, RowIndex, ACol, ADataArray.Orientation)  then
            begin
              APoint.y := ZoomBox.YCoord(FModel.Grid.RowCenter(RowIndex));
              APoint.x := ZoomBox.XCoord(ADataArray.RealData[LayerIndex,RowIndex,ACol]);
              Points[RowIndex] := APoint;
              LastIndex := RowIndex;
            end
            else
            begin
              DrawLine := True;
            end;
            if DrawLine or (RowIndex = FModel.Grid.RowCount - 1) then
            begin
              NumPoints := LastIndex-StartIndex+1;
              if NumPoints > 1 then
              begin
                DrawBigPolyline32(ABitMap, Color32(AColor), LineThickness, Points, True, False,
                  StartIndex, NumPoints);
              end;
              StartIndex := Succ(RowIndex);
            end;
          end
        end;
      else
        Assert(False);
    end;
  end;
begin
  FModel := FModel as TCustomModel;
  if ViewDirection = vdTop then
  begin
    Exit;
  end;
  if DataArrays.Count = 0 then
  begin
    Exit;
  end;
  if (FModel.Grid = nil) and (FModel.Mesh3D = nil) then
  begin
    Exit;
  end;
  if (FModel.Grid <> nil)
    and ((FModel.Grid.LayerCount = 0)
    or (FModel.Grid.RowCount = 0)
    or (FModel.Grid.ColumnCount = 0))
    then
  begin
    Exit;
  end;
  if (FModel.Mesh3D <> nil)
    and ((FModel.Mesh3D.LayerCount = 0)
    or (FModel.Mesh3D.Mesh2DI.ElementCount = 0))
    then
  begin
    Exit;
  end;
  if not (FModel.ModelSelection in SutraSelection) then
  begin
    ActiveDataArray := (FModel as TCustomModel).DataArrayManager.GetDataSetByName(rsActive);
    ActiveDataArray.Initialize;
  end;
  if FModel.ModelSelection = msModflow2015 then
  begin
    IDomain := (FModel as TCustomModel).DataArrayManager.GetDataSetByName(StrIDOMAIN);
//    IDomain.Initialize;
  end
  else
  begin
    IDomain := nil;
  end;
  for DataArrayIndex := 0 to DataArrays.Count - 1 do
  begin
    ADataArray := DataArrays[DataArrayIndex];
    AColor := Colors[DataArrayIndex];
    Assert(ADataArray.DataType = rdtDouble);
    ADataArray.Initialize;
    case ADataArray.Orientation of
      dsoTop:
        begin
          DrawACrossSectionLine(0, AColor);
        end;
      dso3D:
        begin
          for LayerIndex := 0 to ADataArray.LayerCount - 1 do
          begin
            if AllLayers
              or (LayersToUse.IndexOf(LayerIndex) >= 0) then
            begin
              DrawACrossSectionLine(LayerIndex, AColor)
            end;
          end;
        end;
      else Assert(False);
    end;
  end;
end;

procedure TCrossSection.LayerChanged(Sender: TObject; const Item: integer;
  Action: TCollectionNotification);
begin
  UpToDate := False;
end;

procedure TCrossSection.RemoveDataArray(ADataArray: TDataArray);
var
  DataSetIndex: Integer;
begin
  DataSetIndex := FDataArrays.IndexOf(ADataArray);
  if DataSetIndex >= 0 then
  begin
    FDataArrays.Delete(DataSetIndex);
    FColors.Delete(DataSetIndex);
  end;
end;

procedure TCrossSection.SetAllLayers(const Value: boolean);
begin
  if FAllLayers <> Value then
  begin
    FAllLayers := Value;
    UpToDate := False;
  end;
end;

procedure TCrossSection.SetColors(const Value: TList<TColor>);
begin
  FColors.Clear;
  FColors.AddRange(Value.ToArray);
end;

procedure TCrossSection.SetDataArrays(const Value: TDataArrayObjectList);
begin
  FDataArrays.Clear;
  FDataArrays.AddRange(Value.ToArray);
end;

procedure TCrossSection.SetLayersToUse(const Value: TList<integer>);
begin
  FLayersToUse.Clear;
  FLayersToUse.AddRange(Value.ToArray);
end;

procedure TCrossSection.SetLineThickness(const Value: Integer);
begin
  if FLineThickness <> Value then
  begin
    FLineThickness := Value;
    UpToDate := False;
  end;
end;



end.
