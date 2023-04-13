unit VectorDisplayUnit;

interface

uses
  Classes, FastGEO, DataSetUnit, GoPhastTypes, ZoomBox2, Graphics, GR32,
  SubscriptionUnit, OpenGL;

type
  TLayerPoints = array of array of array of TPoint3D;
  TVectorMethod = (vcComponents, vcMagnitude);
  TScaleType = (st2D, st3D);

  TCustomVectors = class(TGoPhastPersistent)
  strict private
    FModel: TBaseModel;
  private
    FStoredScale: TRealStorage;
    FColor: TColor;
    FVisible: boolean;
    FListsCreated: boolean;
    FVectorGLIndex: GLuint;
    FNeedToRecordVectors: boolean;
    FStoredScale3D: TRealStorage;
    FMinimumSeparation2D: Integer;
    FStoredMinSeparationVertical3D: TRealStorage;
    FStoredMinSeparationHorizontal3D: TRealStorage;
    FLogScaled: boolean;
    FLineThickness: single;
    procedure SetStoredScale(const Value: TRealStorage);
    function GetScale: Double;
    procedure SetScale(const Value: Double);
    procedure SetColor(const Value: TColor);
    procedure SetVisible(const Value: boolean);
    procedure DataSetChanged(Sender: TObject);
    procedure InvalidateAllViews;
    function GetScale3D: Double;
    procedure SetScale3D(const Value: Double);
    procedure SetStoredScale3D(const Value: TRealStorage);
    procedure SetMinimumSeparation2D(const Value: Integer);
    procedure SetStoredMinSeparationHorizontal3D(const Value: TRealStorage);
    procedure SetStoredMinSeparationVertical3D(const Value: TRealStorage);
    function GetMinSeparationHorizontal3D: double;
    function GetMinSeparationVertical3D: double;
    procedure SetMinSeparationHorizontal3D(const Value: double);
    procedure SetMinSeparationVertical3D(const Value: double);
    procedure SetLogScaled(const Value: boolean);
    procedure SetLineThickness(const Value: single);
  protected
    // @name contains the starting points of each vector
    FOrigins: TLayerPoints;
    FValues: TLayerPoints;
    FMagnification: double;
    FDefaultScale: Extended;
    FDataSetObserver: TObserver;
    FVectorMethod: TVectorMethod;
    FListening: boolean;
    function GetVector(const Layer, Row, Column: integer;
      ScaleType: TScaleType): TLine3D;
    procedure UpdateVectors; virtual;
    function GetZoomBox(ViewDirection: TViewDirection): TQRbwZoomBox2;
    procedure GetObservers(List: TObserverList); virtual; abstract;
    procedure StopListening;
    procedure StartListening;
    procedure OnChangeEventHander(Sender: TObject); override;
    procedure RecordVectors3D;
  public
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    property Model: TBaseModel read FModel;
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    Constructor Create(Model: TBaseModel);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Vectors[const Layer, Row, Column: integer; ScaleType: TScaleType]:
      TLine3D read GetVector;
    procedure PlotVectors2D(ViewDirection: TViewDirection;
      const BitMap: TPersistent);
    procedure PlotVectors3D(Magnification: Double);
    procedure Invalidate;
    property Scale: Double read GetScale write SetScale;
    property Scale3D: Double read GetScale3D write SetScale3D;
    procedure InitializeVariables;
    property MinSeparationHorizontal3D: double
      read GetMinSeparationHorizontal3D write SetMinSeparationHorizontal3D;
    property MinSeparationVertical3D: double read GetMinSeparationVertical3D
      write SetMinSeparationVertical3D;
    property LineThickness: single read FLineThickness write SetLineThickness;
  published
    property Visible: boolean read FVisible write SetVisible;
    property StoredScale: TRealStorage read FStoredScale write SetStoredScale;
    property StoredScale3D: TRealStorage read FStoredScale3D
      write SetStoredScale3D;
    property Color: TColor read FColor write SetColor;
    property MinimumSeparation2D: Integer read FMinimumSeparation2D
      write SetMinimumSeparation2D;
    property StoredMinSeparationHorizontal3D: TRealStorage
      read FStoredMinSeparationHorizontal3D
      write SetStoredMinSeparationHorizontal3D;
    property StoredMinSeparationVertical3D: TRealStorage
      read FStoredMinSeparationVertical3D
      write SetStoredMinSeparationVertical3D;
    property LogScaled: boolean read FLogScaled write SetLogScaled;
  end;

  TVelocityVectors = class(TCustomVectors)
  private
    FModel: TBaseModel;
    FZVelocityName: string;
    FXVelocityName: string;
    FYVelocityName: string;
    procedure SetXVelocityName(const Value: string);
    procedure SetYVelocityName(const Value: string);
    procedure SetZVelocityName(const Value: string);
    procedure GetDataSets(out XDataArray, YDataArray, ZDataArray: TDataArray);
  protected
    procedure UpdateVectors; override;
    procedure GetObservers(List: TObserverList); override;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel);
  published
    property XVelocityName: string read FXVelocityName write SetXVelocityName;
    property YVelocityName: string read FYVelocityName write SetYVelocityName;
    property ZVelocityName: string read FZVelocityName write SetZVelocityName;
  end;

  TPredefinedVectorType = (pvtPermeability, pvtConductivity,
    pvtLongitudianalDispersivity, pvtTransverseDispersivity);

  TPredefinedVectorDirection = (pvdMax, pvdMid, pvdMin);

  TPredefinedVectors = class(TCustomVectors)
  private
    FModel: TBaseModel;
    FVectorType: TPredefinedVectorType;
    FVectorDirection: TPredefinedVectorDirection;
    procedure SetVectorType(const Value: TPredefinedVectorType);
    procedure SetVectorDirection(const Value: TPredefinedVectorDirection);
    procedure GetDataSets(out Angle1, Angle2, Angle3, DataArray,
      MaxDataArray: TDataArray);
  protected
    procedure UpdateVectors; override;
    procedure GetObservers(List: TObserverList); override;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel);
  published
    property VectorType: TPredefinedVectorType read FVectorType
      write SetVectorType;
    property VectorDirection: TPredefinedVectorDirection read FVectorDirection
      write SetVectorDirection;
  end;

  TVectorItem = class(TPhastCollectionItem)
  private
    FVectors: TVelocityVectors;
    FDescription: string;
    procedure SetVectors(const Value: TVelocityVectors);
    procedure SetDescription(const Value: string);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function IsValid: boolean;
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    function Model: TBaseModel;
  published
    property Vectors: TVelocityVectors read FVectors write SetVectors;
    property Description: string read FDescription write SetDescription;
  end;

  TVectorCollection = class(TPhastCollection)
  strict private
    { TODO -cRefactor : Consider replacing FModel with a TNotifyEvent or interface. }
    //
    FModel: TBaseModel;
  private
    FSelectedItem: Integer;
    procedure SetSelectedItem(const Value: Integer);
    function StoreSelected: boolean;
  public
    constructor Create(Model: TBaseModel);
    procedure Assign(Source: TPersistent); override;
    procedure CheckDataSets;
    procedure EndUpdate; override;
    procedure SetItemByName(const ADescription: string);
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    property Model: TBaseModel read FModel;
    function Add: TVectorItem;
  published
    property SelectedItem: Integer read FSelectedItem write SetSelectedItem
      stored StoreSelected;
  end;

implementation

uses
  frmGoPhastUnit, PhastModelUnit, SutraMeshUnit, BigCanvasMethods, Math,
  ModelMuseUtilities, QuadtreeClass, OctTreeClass, MeshRenumberingTypes,
  ModflowIrregularMeshUnit, AbstractGridUnit, DataArrayManagerUnit,
  DataSetNamesUnit;

const
  DegToRadiansFactor = Pi/180;
  DefaultMaxVectorLength = 20;
  // @name is the thickness of vectors when drawn in the 3D view.
  ThinLine = 1.0;


procedure ROTMAT(const A1,A2,A3: Double;
  out G11,G12,G13,G21,G22,G23,G31,G32,G33: double);
(*
Adapted from SUTRA version 2.2
 PURPOSE :
  TO COMPUTE A TRANSFORMATION MATRIX, [G], THAT CONVERTS
  COORDINATES OF A VECTOR, {v}, FROM A COORDINATE SYSTEM (X, Y, Z)
  TO A NEW COORDINATE SYSTEM (X', Y', Z'):  {v'} = [G]{v}.
  THE OVERALL TRANSFORMATION IS THE RESULT OF THREE ROTATIONS
  APPLIED CONSECUTIVELY:
  A1 = ROTATION IN THE XY-PLANE, COUNTER-CLOCKWISE FROM THE
     +X-AXIS (LOOKING DOWN THE +Z-AXIS TOWARD THE ORIGIN),
  A2 = ROTATION IN THE NEW XZ-PLANE, COUNTER-CLOCKWISE FROM THE
     NEW +X-AXIS (LOOKING DOWN THE NEW +Y-AXIS TOWARD THE ORIGIN),
  A3 = ROTATION IN THE NEW YZ-PLANE, COUNTER-CLOCKWISE FROM THE
     NEW +Y-AXIS (LOOKING DOWN THE NEW +X-AXIS TOWARD THE ORIGIN).
*)
var
  S1: Extended;
  C1: Extended;
  S2: Extended;
  C2: Extended;
  S3: Extended;
  C3: Extended;
begin
//.....COMPUTE SINES AND COSINES OF ANGLES.
      S1 := Sin(A1);
      C1 := Cos(A1);
      S2 := Sin(A2);
      C2 := Cos(A2);
      S3 := Sin(A3);
      C3 := Cos(A3);

//.....COMPUTE ROTATION MATRIX.
      G11  :=  C1*C2;
      G12  :=  -C1*S2*S3 - S1*C3;
      G13  :=  -C1*S2*C3 + S1*S3;
      G21  :=  S1*C2;
      G22  :=  -S1*S2*S3 + C1*C3;
      G23  :=  -S1*S2*C3 - C1*S3;
      G31  :=  S2;
      G32  :=  C2*S3;
      G33  :=  C2*C3;

end;


procedure Rotate(const G11,G12,G13,G21,G22,G23,G31,G32,G33,X,Y,Z: double;
  out XP,YP,ZP: double);
(*
Adapted from SUTRA version 2.2
 PURPOSE :
  TO TRANSFORM THE COORDINATES OF A VECTOR, {x}, BY APPLYING THE
  ROTATION MATRIX, [G]:  {xp}=[G]{x}.

*)
begin
//.....COMPUTE VECTOR {xp} AS THE PRODUCT OF MATRIX [G] AND VECTOR {x}
      XP := G11*X + G12*Y + G13*Z;
      YP := G21*X + G22*Y + G23*Z;
      ZP := G31*X + G32*Y + G33*Z;

end;

{ TCustomVectors }

procedure TCustomVectors.Assign(Source: TPersistent);
var
  SourceVectors: TCustomVectors;
begin
  if Source is TCustomVectors then
  begin
    SourceVectors := TCustomVectors(Source);
    Scale := SourceVectors.Scale;
    Scale3D := SourceVectors.Scale3D;
    Color := SourceVectors.Color;
    Visible := SourceVectors.Visible;
    MinimumSeparation2D := SourceVectors.MinimumSeparation2D;
    MinSeparationHorizontal3D := SourceVectors.MinSeparationHorizontal3D;
    MinSeparationVertical3D := SourceVectors.MinSeparationVertical3D;
    LogScaled := SourceVectors.LogScaled;
    LineThickness := SourceVectors.LineThickness;
  end
  else
  begin
    inherited;
  end;
end;

procedure TCustomVectors.DataSetChanged(Sender: TObject);
begin
  if not FDataSetObserver.UpToDate then
  begin
    Invalidate;
  end;
end;

constructor TCustomVectors.Create(Model: TBaseModel);
begin
  if Model = nil then
  begin
    inherited Create(nil);
  end
  else
  begin
    inherited Create(Model.DoInvalidate);
  end;
  Assert((Model = nil) or (Model is TCustomModel));
  FModel := Model;
  FListening := False;
  FListsCreated := False;
  FNeedToRecordVectors := True;

  FStoredScale := TRealStorage.Create;
  FStoredScale.OnChange := OnChangeEventHander;

  FStoredScale3D := TRealStorage.Create;
  FStoredScale3D.OnChange := OnChangeEventHander;

  FStoredMinSeparationHorizontal3D := TRealStorage.Create;
  FStoredMinSeparationHorizontal3D.OnChange := OnChangeEventHander;

  FStoredMinSeparationVertical3D := TRealStorage.Create;
  FStoredMinSeparationVertical3D.OnChange := OnChangeEventHander;

  FDataSetObserver := TObserver.Create(nil);
  FDataSetObserver.UpToDate := True;
  FDataSetObserver.OnUpToDateSet := DataSetChanged;

  StartListening;

  InitializeVariables;
end;

destructor TCustomVectors.Destroy;
begin
  StopListening;
  FDataSetObserver.Free;
  FStoredScale3D.Free;
  FStoredScale.Free;
  FStoredMinSeparationVertical3D.Free;
  FStoredMinSeparationHorizontal3D.Free;
  inherited;
end;

function TCustomVectors.GetMinSeparationHorizontal3D: double;
begin
  result := StoredMinSeparationHorizontal3D.Value;
end;

function TCustomVectors.GetMinSeparationVertical3D: double;
begin
  result := StoredMinSeparationVertical3D.Value;
end;

function TCustomVectors.GetScale: Double;
begin
  result := FStoredScale.Value;
end;

function TCustomVectors.GetScale3D: Double;
begin
  result := FStoredScale3D.Value;
end;

function TCustomVectors.GetVector(const Layer, Row, Column: integer;
  ScaleType: TScaleType): TLine3D;
var
  Factor: Extended;
  LocalScale: double;
begin
  if FMagnification = 0 then
  begin
    FMagnification := 1;
  end;
  LocalScale := 1;
  case ScaleType of
    st2D: LocalScale := Scale;
    st3D: LocalScale := Scale3D;
    else Assert(False);
  end;
  Factor := FDefaultScale*LocalScale/FMagnification;
  result[1] := FOrigins[Layer, Row, Column];

  result[2].x := FOrigins[Layer, Row, Column].x + FValues[Layer, Row, Column].x*Factor;
  result[2].y := FOrigins[Layer, Row, Column].y + FValues[Layer, Row, Column].y*Factor;
  result[2].z := FOrigins[Layer, Row, Column].z + FValues[Layer, Row, Column].z*Factor;
end;

function TCustomVectors.GetZoomBox(
  ViewDirection: TViewDirection): TQRbwZoomBox2;
begin
  result := nil;
  case ViewDirection of
    vdTop: result := frmGoPhast.frameTopView.ZoomBox;
    vdFront: result := frmGoPhast.frameFrontView.ZoomBox;
    vdSide: result := frmGoPhast.frameSideView.ZoomBox;
    else Assert(False);
  end;
end;

procedure TCustomVectors.Invalidate;
begin
  SetLength(FOrigins, 0, 0);
  SetLength(FValues, 0, 0);
  InvalidateAllViews;
  FNeedToRecordVectors := True;
end;

procedure TCustomVectors.InitializeVariables;
begin
  FMinimumSeparation2D := 0;
  MinSeparationHorizontal3D := 0;
  MinSeparationVertical3D := 0;
  FLogScaled := False;
  FLineThickness := 2;
end;

procedure TCustomVectors.InvalidateAllViews;
begin
  if Visible and (FModel <> nil) then
  begin
    frmGoPhast.TopScreenObjectsChanged := True;
    frmGoPhast.FrontScreenObjectsChanged := True;
    frmGoPhast.SideScreenObjectsChanged := True;
    FNeedToRecordVectors := True;
    frmGoPhast.frame3DView.glWidModelView.Invalidate;
  end;
end;

procedure TCustomVectors.OnChangeEventHander(Sender: TObject);
begin
  inherited;
  InvalidateAllViews;
end;

procedure TCustomVectors.PlotVectors2D(ViewDirection: TViewDirection;
  const BitMap: TPersistent);
var
  LayerIndex: Integer;
  ColIndex: Integer;
  AVector: TLine3D;
  Points: array of TPoint;
  ZoomBox: TQRbwZoomBox2;
  AColor: TColor32;
  Element2D_Index: Integer;
  StartPoint: TPoint2D;
  SegmentAngle: Extended;
  APoint: TPoint2D;
  Angle: Extended;
  X1: FastGEO.TFloat;
  Y1: FastGEO.TFloat;
  X2: FastGEO.TFloat;
  Y2: FastGEO.TFloat;
  Magnitude: Double;
  SpacingTree: TRbwQuadTree;
  LocalModel: TCustomModel;
  ElementListI: TIElement2DList;
  Element2DI: IElement2D;
  RowIndex: Integer;
  LoopMax: Integer;
  Grid: TCustomModelGrid;
  GridAngle: Double;
  function ShouldPlotVector: boolean;
  var
    X: double;
    Y: double;
    NewX: Double;
    NewY: Double;
    Data: Pointer;
    DeltaX: double;
    DeltaY: double;
  begin
    result := True;
    if MinimumSeparation2D > 0 then
    begin
      X := Points[0].X;
      Y := Points[0].Y;
      if SpacingTree.Count > 0 then
      begin
        NewX := X;
        NewY := Y;
        SpacingTree.FirstNearestPoint(NewX, NewY, Data);
        DeltaX := Abs(X-NewX);
        DeltaY := Abs(Y-NewY);
        if (DeltaX <= MinimumSeparation2D)
          and (DeltaY <= MinimumSeparation2D) then
        begin
          if Sqrt(Sqr(DeltaX) + Sqr(DeltaY)) < MinimumSeparation2D then
          begin
            result := False;
            Exit;
          end;
        end;
      end;
      // It doesn't matter what data is added to SpacingTree because
      // only the coordinates are used. The data is never used.
      SpacingTree.AddPoint(X,Y, Pointer(1));
    end;
  end;
begin
  if not Visible then
  begin
    Exit;
  end;
  if FModel.ModelSelection in SutraSelection + [msModflow2015] then
  begin
    if Length(FOrigins) = 0 then
    begin
      UpdateVectors;
      if Length(FOrigins) = 0 then
      begin
        Exit;
      end;
    end;
    LocalModel := FModel as TCustomModel;
    SetLength(Points, 3);
    ZoomBox := GetZoomBox(ViewDirection);
    FMagnification := ZoomBox.Magnification;
    AColor := Color32(Color);
    if LocalModel.ModelSelection in SutraSelection then
    begin
      if LocalModel.SutraMesh.MeshType in [mt2D, mtProfile] then
      begin
        if ViewDirection <> vdTop then
        begin
          Exit;
        end;
      end;
    end
    else if FModel.ModelSelection <> msModflow2015 then
    begin
      Exit;
    end;

    SpacingTree := TRbwQuadTree.Create(nil);
    try
      case ViewDirection of
        vdTop:
          begin
            if (LocalModel.ModelSelection = msModflow2015)
              and not LocalModel.DisvUsed then
            begin
              GridAngle := LocalModel.Grid.GridAngle;
            end
            else
            begin
              GridAngle := 0;
            end;
            LayerIndex := LocalModel.SelectedLayer;
            if LayerIndex >= LocalModel.LayerCount then
            begin
              LayerIndex := LocalModel.LayerCount - 1;
            end;

            for RowIndex := 0 to LocalModel.RowCount - 1 do
            begin
              for ColIndex := 0 to LocalModel.ColumnCount - 1 do
              begin
                if LocalModel.ActiveElement[LayerIndex, RowIndex, ColIndex] then
                begin
                  AVector := Vectors[LayerIndex, RowIndex, ColIndex, st2D];
                  X1 := AVector[1].x;
                  Y1 := AVector[1].y;
                  if GridAngle <> 0 then
                  begin
                    LocalModel.Grid.
                      RotateFromGridCoordinatesToRealWorldCoordinates(X1, Y1);
                  end;
                  Points[0].X := ZoomBox.XCoord(X1);
                  Points[0].Y := ZoomBox.YCoord(Y1);

                  if not ShouldPlotVector then
                  begin
                    Continue;
                  end;

                  X2 := AVector[2].x;
                  Y2 := AVector[2].y;
                  if GridAngle <> 0 then
                  begin
                    LocalModel.Grid.
                      RotateFromGridCoordinatesToRealWorldCoordinates(X2, Y2);
                  end;

                  Points[1].X := ZoomBox.XCoord((X1+X2)/2);
                  Points[1].Y := ZoomBox.YCoord((Y1+Y2)/2);
                  DrawBigPolyline32(BitMap, clBlack32, LineThickness,
                    Points, True, False, 0, 2);

                  Points[2].X := ZoomBox.XCoord(X2);
                  Points[2].Y := ZoomBox.YCoord(Y2);
                  DrawBigPolyline32(BitMap, AColor, LineThickness,
                    Points, True, False, 1, 2);
                end;
              end;
            end;
          end;
        vdFront:
          begin
            StartPoint := EquatePoint(0.0, 0.0);

            ElementListI := nil;
            try
              if LocalModel.ModelSelection in SutraSelection then
              begin
                SegmentAngle := LocalModel.SutraMesh.CrossSection.Angle;
                ElementListI := TIElement2DList.Create;
                LocalModel.SutraMesh.GetElementsIntfOnCrossSection(ElementListI);
                LoopMax := ElementListI.Count;
                RowIndex := 0;
              end
              else if LocalModel.DisvUsed then
              begin
                SegmentAngle := LocalModel.DisvGrid.CrossSection.Angle;
                ElementListI := TIElement2DList.Create;
                LocalModel.DisvGrid.GetElementsIntfOnCrossSection(ElementListI);
                LoopMax := ElementListI.Count;
                RowIndex := 0;
              end
              else
              begin
                SegmentAngle := 0;
                LoopMax := LocalModel.Grid.ColumnCount;
                RowIndex := LocalModel.Grid.SelectedRow;
                if RowIndex >= LocalModel.Grid.RowCount then
                begin
                  RowIndex := LocalModel.Grid.RowCount -1;
                end;
              end;

              for Element2D_Index := 0 to LoopMax - 1 do
              begin
                if (LocalModel.ModelSelection in SutraSelection)
                  or LocalModel.DisvUsed then
                begin
                  Element2DI := ElementListI[Element2D_Index];
                  ColIndex := Element2DI.ElementNumber;
                end
                else
                begin
                  ColIndex := Element2D_Index;
                end;
                for LayerIndex := 0 to LocalModel.LayerCount - 1 do
                begin
                  if not LocalModel.ActiveElement[LayerIndex, RowIndex, ColIndex] then
                  begin
                    Continue;
                  end;
                  AVector := Vectors[LayerIndex, RowIndex, ColIndex, st2D];

                  APoint := EquatePoint(AVector[1].x, AVector[1].y);
                  Angle := ArcTan2(APoint.y - StartPoint.y,
                    APoint.x - StartPoint.x) - SegmentAngle;
                  X1 := Distance(StartPoint, APoint)*Cos(Angle)
                    + StartPoint.x;
                  Y1 := AVector[1].Z;
                  Points[0].X := ZoomBox.XCoord(X1);
                  Points[0].Y := ZoomBox.YCoord(Y1);

                  if not ShouldPlotVector then
                  begin
                    Continue;
                  end;

                  APoint := EquatePoint(AVector[2].x, AVector[2].y);
                  Angle := ArcTan2(APoint.y - StartPoint.y,
                    APoint.x - StartPoint.x) - SegmentAngle;
                  X2 := Distance(StartPoint, APoint)*Cos(Angle)
                    + StartPoint.x;

                  Y2 := AVector[2].Z;
                  if FVectorMethod = vcMagnitude then
                  begin
                    Magnitude := Distance(AVector[1], AVector[2]);
                    Y2 := (AVector[2].Z - AVector[1].Z)
                      *ZoomBox.Exaggeration + AVector[1].Z;
                    Angle := ArcTan2(Y2-Y1, X2-X1);
                    X2 := X1 + Cos(Angle)*Magnitude;
                    Y2 := Y1 + Sin(Angle)*Magnitude/ZoomBox.Exaggeration;
                  end;

                  Points[2].X := ZoomBox.XCoord(X2);
                  Points[2].Y := ZoomBox.YCoord(Y2);

                  Points[1].X := ZoomBox.XCoord((X1 + X2)/2);
                  Points[1].Y := ZoomBox.YCoord((Y1 + Y2)/2);

                  DrawBigPolyline32(BitMap, clBlack32, LineThickness,
                    Points, True, False, 0, 2);
                  DrawBigPolyline32(BitMap, AColor, LineThickness,
                    Points, True, False, 1, 2);
                end;
              end;
            finally
              ElementListI.Free;
            end;
          end;
        vdSide:
          begin
            Assert((LocalModel.ModelSelection = msModflow2015)
              and not LocalModel.DisvUsed);
            Grid := LocalModel.Grid;
            StartPoint := EquatePoint(0.0, 0.0);

            LoopMax := LocalModel.Grid.RowCount;
            ColIndex := LocalModel.Grid.SelectedColumn;
            if ColIndex >= LocalModel.Grid.ColumnCount then
            begin
              ColIndex := LocalModel.Grid.ColumnCount -1;
            end;

            for RowIndex := 0 to LoopMax - 1 do
            begin
              for LayerIndex := 0 to LocalModel.LayerCount - 1 do
              begin
                if not LocalModel.ActiveElement[LayerIndex, RowIndex, ColIndex] then
                begin
                  Continue;
                end;
                AVector := Vectors[LayerIndex, RowIndex, ColIndex, st2D];

                X1 := AVector[1].y;
                Y1 := AVector[1].Z;

                Points[0] := Convert2D_SidePoint(ZoomBox, EquatePoint(X1, Y1));

                if not ShouldPlotVector then
                begin
                  Continue;
                end;

                X2 := AVector[2].y;
                Y2 := AVector[2].Z;
                if FVectorMethod = vcMagnitude then
                begin
                  Magnitude := Distance(AVector[1], AVector[2]);
                  Y2 := (AVector[2].Z - AVector[1].Z)
                    *ZoomBox.Exaggeration + AVector[1].Z;
                  Angle := ArcTan2(Y2-Y1, X2-X1);
                  X2 := X1 + Cos(Angle)*Magnitude;
                  Y2 := Y1 + Sin(Angle)*Magnitude/ZoomBox.Exaggeration;
                end;

                Points[2] := Convert2D_SidePoint(ZoomBox,
                  EquatePoint(X2, Y2));

                Points[1] := Convert2D_SidePoint(ZoomBox,
                  EquatePoint((X1 + X2)/2, (Y1 + Y2)/2));

                DrawBigPolyline32(BitMap, clBlack32, LineThickness,
                  Points, True, False, 0, 2);
                DrawBigPolyline32(BitMap, AColor, LineThickness,
                  Points, True, False, 1, 2);
              end;
            end;

          end;
        else Assert(False);
      end;
    finally
      SpacingTree.Free;
    end;
  end;
end;

procedure TCustomVectors.PlotVectors3D(Magnification: Double);
const
  NumberOfLists = 1;
begin
  if not Visible then
  begin
    Exit;
  end;
  if not (FModel.ModelSelection in SutraSelection + [msModflow2015]) then
  begin
    Exit;
  end;

  if (FModel.ModelSelection in SutraSelection)
    and ((FModel as TPhastModel).SutraMesh.MeshType <> mt3D) then
  begin
    Exit;
  end;
  if Length(FOrigins) = 0 then
  begin
    UpdateVectors;
    if Length(FOrigins) = 0 then
    begin
      Exit;
    end;
  end;
  FMagnification := Magnification;

  glDisable(GL_LIGHTING);
  glDisable(GL_LIGHT0);
  glMatrixMode(GL_MODELVIEW);

  glPushMatrix;
  try
    glEnable(GL_LINE_SMOOTH);

    if not FListsCreated then
    begin
      FVectorGLIndex := glGenLists(NumberOfLists);
      FListsCreated := True;
    end;

    if FNeedToRecordVectors then
    begin
      RecordVectors3D;
    end;

    glCallList(FVectorGLIndex);

  finally
    glPopMatrix;
  end;
end;

procedure TCustomVectors.RecordVectors3D;
var
  LocalModel: TCustomModel;
  ColIndex: Integer;
  LayerIndex: Integer;
  AVector: TLine3D;
  X1, Y1, Z1: single;
  X2, Y2, Z2: single;
  Red: GLubyte;
  Green: GLubyte;
  Blue: GLubyte;
  TotalDistance: TFloat;
  SpacingTree: TRbwOctTree;
  HorizontalDistance: TFloat;
  Angle: Extended;
  StartPointHorizontal: TPoint2D;
  EndPointHorizontal: TPoint2D;
  RowIndex: Integer;
  function ShouldPlotVector: boolean;
  var
    X1Dble: double;
    Y1Dble: double;
    Z1Dble: double;
    NewX1: Double;
    NewY1: Double;
    NewZ1: Double;
    Data: Pointer;
    DeltaX: double;
    DeltaY: double;
  begin
    result := True;
    if (MinSeparationHorizontal3D > 0) or (MinSeparationVertical3D > 0)then
    begin
      X1Dble := X1;
      Y1Dble := Y1;
      Z1Dble := Z1;
      if SpacingTree.Count > 0 then
      begin
        NewX1 := X1Dble;
        NewY1 := Y1Dble;
        NewZ1 := Z1Dble;
        SpacingTree.FirstNearestPoint(NewX1, NewY1, NewZ1, Data);

        if MinSeparationHorizontal3D > 0 then
        begin
          DeltaX := Abs(X1Dble-NewX1);
          DeltaY := Abs(Y1Dble-NewY1);
          if (DeltaX <= MinSeparationHorizontal3D)
            and (DeltaY <= MinSeparationHorizontal3D) then
          begin
            if Sqrt(Sqr(DeltaX) + Sqr(DeltaY)) <= MinSeparationHorizontal3D then
            begin
              if MinSeparationVertical3D > 0 then
              begin
                if Abs(Z1Dble-NewZ1) < MinSeparationVertical3D then
                begin
                  result := False;
                  Exit;
                end;
              end
              else
              begin
                result := False;
                Exit;
              end;
            end;
          end;
        end
        else
        begin
          if Abs(Z1Dble-NewZ1) < MinSeparationVertical3D then
          begin
            result := False;
            Exit;
          end;
        end;
      end;
      SpacingTree.AddPoint(X1Dble, Y1Dble, Z1Dble, Pointer(1));
    end;
  end;
begin
  ExtractColorComponents(Color, Red, Green, Blue);
  LocalModel := Model as TCustomModel;
  SpacingTree := TRbwOctTree.Create(nil);
  try
    glNewList(FVectorGLIndex, GL_COMPILE);
    try
      glLineWidth(ThinLine);

      glBegin(GL_LINES);
      try
        for ColIndex := 0 to LocalModel.ColumnCount - 1 do
        begin
          for RowIndex := 0 to LocalModel.RowCount - 1 do
          begin
            for LayerIndex := 0 to LocalModel.LayerCount - 1 do
            begin
              if not LocalModel.ActiveElement[
                LayerIndex, RowIndex, ColIndex] then
              begin
                Continue;
              end;
              AVector := Vectors[LayerIndex, RowIndex, ColIndex, st3D];

              X1 := AVector[1].x;
              Y1 := AVector[1].y;
              Z1 := AVector[1].z;

              if not ShouldPlotVector then
              begin
                Continue;
              end;

              X2 := AVector[2].x;
              Y2 := AVector[2].y;
              Z2 := AVector[2].z;

              if FVectorMethod = vcMagnitude then
              begin
                TotalDistance := Distance(AVector[1], AVector[2]);
                Z2 := (Z2 - AVector[1].Z)
                  *LocalModel.Exaggeration + AVector[1].Z;
                StartPointHorizontal := EquatePoint(AVector[1].x, AVector[1].y);
                EndPointHorizontal := EquatePoint(AVector[2].x, AVector[2].y);
                HorizontalDistance := Distance(StartPointHorizontal,
                  EndPointHorizontal);
                Angle := ArcTan2(Z2-Z1, HorizontalDistance);
                if HorizontalDistance <> 0 then
                begin
                  EndPointHorizontal := ProjectPoint(StartPointHorizontal,
                    EndPointHorizontal, Cos(Angle)*TotalDistance);
                end;
                Z2 := Z1 + Sin(Angle)*TotalDistance/LocalModel.Exaggeration;
                X2 := EndPointHorizontal.x;
                Y2 := EndPointHorizontal.y;
              end;

              glColor3f(0.0, 0.0, 0.0);
              glVertex3f(X1, Y1, Z1);
              glVertex3f((X1+X2)/2, (Y1+Y2)/2, (Z1+Z2)/2);
              glColor3ub(Red, Green, Blue);
              glVertex3f((X1+X2)/2, (Y1+Y2)/2, (Z1+Z2)/2);
              glVertex3f(X2, Y2, Z2);

            end;
          end;
        end;
      finally
        glEnd;
      end;
    finally
      glEndList;
    end
  finally
    FNeedToRecordVectors := False;
    SpacingTree.Free;
  end;
end;

procedure TCustomVectors.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    InvalidateModel;
    InvalidateAllViews;
  end;
end;

procedure TCustomVectors.SetLineThickness(const Value: single);
begin
  if FLineThickness <> Value then
  begin
    FLineThickness := Value;
    InvalidateModel;
    InvalidateAllViews;
  end;
end;

procedure TCustomVectors.SetLogScaled(const Value: boolean);
begin
  if FLogScaled <> Value then
  begin
    SetBooleanProperty(FLogScaled, Value);
    Invalidate;
  end;
end;

procedure TCustomVectors.SetMinimumSeparation2D(const Value: Integer);
begin
  if FMinimumSeparation2D <> Value then
  begin
    SetIntegerProperty(FMinimumSeparation2D, Value);
    Invalidate;
  end;
end;

procedure TCustomVectors.SetMinSeparationHorizontal3D(const Value: double);
begin
  StoredMinSeparationHorizontal3D.Value := Value;
end;

procedure TCustomVectors.SetMinSeparationVertical3D(const Value: double);
begin
  StoredMinSeparationVertical3D.Value := Value;
end;

procedure TCustomVectors.SetScale(const Value: Double);
begin
  FStoredScale.Value := Value;
end;

procedure TCustomVectors.SetScale3D(const Value: Double);
begin
  FStoredScale3D.Value := Value;
end;

procedure TCustomVectors.SetStoredMinSeparationHorizontal3D(
  const Value: TRealStorage);
begin
  FStoredMinSeparationHorizontal3D.Assign(Value);
end;

procedure TCustomVectors.SetStoredMinSeparationVertical3D(
  const Value: TRealStorage);
begin
  FStoredMinSeparationVertical3D.Assign(Value);
end;

procedure TCustomVectors.SetStoredScale(const Value: TRealStorage);
begin
  FStoredScale.Assign(Value);
end;

procedure TCustomVectors.SetStoredScale3D(const Value: TRealStorage);
begin
  FStoredScale3D.Assign(Value);
end;

procedure TCustomVectors.SetVisible(const Value: boolean);
var
  Changed: Boolean;
begin
  Changed := FVisible <> Value;
  SetBooleanProperty(FVisible, Value);
  if FVisible and not FListening then
  begin
    StartListening;
  end;
  if Changed and (FModel <> nil) then
  begin
    frmGoPhast.TopScreenObjectsChanged := True;
    frmGoPhast.FrontScreenObjectsChanged := True;
    frmGoPhast.SideScreenObjectsChanged := True;
    frmGoPhast.frame3DView.glWidModelView.Invalidate;
  end;
end;

procedure TCustomVectors.StartListening;
var
  List: TObserverList;
  ObserverIndex: integer;
  AnObserver: TObserver;
begin
  List:= TObserverList.Create;
  try
    GetObservers(List);
    for ObserverIndex := 0 to List.Count - 1 do
    begin
      AnObserver := List[ObserverIndex];
      if AnObserver <> nil then
      begin
        AnObserver.TalksTo(FDataSetObserver);
        FListening := True;
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TCustomVectors.StopListening;
var
  List: TObserverList;
  ObserverIndex: integer;
  AnObserver: TObserver;
begin
  List:= TObserverList.Create;
  try
    GetObservers(List);
    for ObserverIndex := 0 to List.Count - 1 do
    begin
      AnObserver := List[ObserverIndex];
      if AnObserver <> nil then
      begin
        AnObserver.StopsTalkingTo(FDataSetObserver);
      end;
    end;
  finally
    List.Free;
  end;
  FListening := False;
end;

procedure TCustomVectors.UpdateVectors;
var
  ColIndex: Integer;
  LayerIndex: Integer;
  LocalModel: TCustomModel;
  RowIndex: Integer;
begin
  if FModel.ModelSelection  in SutraSelection + [msModflow2015] then
  begin
    LocalModel := FModel as TCustomModel;
    SetLength(FOrigins, LocalModel.LayerCount,
      LocalModel.RowCount, LocalModel.ColumnCount);
    SetLength(FValues, LocalModel.LayerCount,
      LocalModel.RowCount, LocalModel.ColumnCount);
    for LayerIndex := 0 to LocalModel.LayerCount - 1 do
    begin
      for RowIndex := 0 to LocalModel.RowCount - 1 do
      begin
        for ColIndex := 0 to LocalModel.ColumnCount - 1 do
        begin
          FOrigins[LayerIndex,RowIndex,ColIndex] :=
            LocalModel.ElementLocation[LayerIndex, RowIndex, ColIndex].UnRotatedLocation;
        end;
      end;
    end;
  end
  else
  begin
    SetLength(FOrigins, 0, 0, 0);
    SetLength(FValues, 0, 0, 0);
  end;
end;

{ TVelocityVectors }

procedure TVelocityVectors.Assign(Source: TPersistent);
var
  VelocitySource: TVelocityVectors;
begin
  if Source is TVelocityVectors then
  begin
    VelocitySource := TVelocityVectors(Source);
    XVelocityName := VelocitySource.XVelocityName;
    YVelocityName := VelocitySource.YVelocityName;
    ZVelocityName := VelocitySource.ZVelocityName;
  end;
  inherited;

end;

procedure TVelocityVectors.SetXVelocityName(const Value: string);
begin
  StopListening;
  SetStringProperty(FXVelocityName, Value);
  StartListening;
  InvalidateAllViews;
end;

procedure TVelocityVectors.SetYVelocityName(const Value: string);
begin
  StopListening;
  SetStringProperty(FYVelocityName, Value);
  StartListening;
  InvalidateAllViews;
end;

procedure TVelocityVectors.SetZVelocityName(const Value: string);
begin
  StopListening;
  SetStringProperty(FZVelocityName, Value);
  StartListening;
  InvalidateAllViews;
end;

procedure TVelocityVectors.UpdateVectors;
var
  Mesh: TSutraMesh3D;
  LocalModel: TPhastModel;
  XDataArray: TDataArray;
  YDataArray: TDataArray;
  ZDataArray: TDataArray;
  ColIndex: Integer;
  LayerIndex: Integer;
  MaxLength: Extended;
  X: double;
  Y: double;
  Z: double;
  TestLength: double;
  MinPositive: double;
  TestDistance: double;
  Factor: double;
  RowIndex: Integer;
begin
  LocalModel := FModel as TPhastModel;
  GetDataSets(XDataArray, YDataArray, ZDataArray);

  if (XDataArray = nil) or (YDataArray = nil) then
  begin
    SetLength(FOrigins, 0, 0, 0);
    SetLength(FValues, 0, 0, 0);
    Exit;
  end;
  if LocalModel.ModelSelection in SutraSelection then
  begin
    Mesh := LocalModel.SutraMesh;
    if (Mesh.MeshType = mt3D) and (ZDataArray = nil) then
    begin
      SetLength(FOrigins, 0, 0, 0);
      SetLength(FValues, 0, 0, 0);
      Exit;
    end;
  end
  else
  begin
    if ZDataArray = nil then
    begin
      SetLength(FOrigins, 0, 0, 0);
      SetLength(FValues, 0, 0, 0);
      Exit;
    end;
  end;
  XDataArray.Initialize;
  YDataArray.Initialize;
  if ZDataArray <> nil then
  begin
    ZDataArray.Initialize;
  end;

  inherited;

  if Length(FValues) = 0 then
  begin
    Exit;
  end;

  MaxLength := 0;

  MinPositive := 0;
  if LogScaled then
  begin
    for ColIndex := 0 to LocalModel.ColumnCount - 1 do
    begin
      for RowIndex := 0 to LocalModel.RowCount - 1 do
      begin
        for LayerIndex := 0 to LocalModel.LayerCount - 1 do
        begin
          if LocalModel.ActiveElement[LayerIndex, RowIndex, ColIndex] then
          begin
            X := XDataArray.RealData[LayerIndex,RowIndex,ColIndex];
            Y := YDataArray.RealData[LayerIndex,RowIndex,ColIndex];
            if ZDataArray <> nil then
            begin
              Z := ZDataArray.RealData[LayerIndex,RowIndex,ColIndex];
              TestDistance := Sqrt(Sqr(X)+ Sqr(Y) + Sqr(Z));
              if (TestDistance > 0) and ((MinPositive = 0)
                or (TestDistance < MinPositive)) then
              begin
                MinPositive := TestDistance;
              end;
            end
            else
            begin
              TestDistance := Sqrt(Sqr(X)+ Sqr(Y));
              if (TestDistance > 0) and ((MinPositive = 0) or (TestDistance < MinPositive)) then
              begin
                MinPositive := TestDistance;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  MinPositive := MinPositive/10;

  for ColIndex := 0 to LocalModel.ColumnCount - 1 do
  begin
    for RowIndex := 0 to LocalModel.RowCount - 1 do
    begin
      for LayerIndex := 0 to LocalModel.LayerCount - 1 do
      begin
        if LocalModel.ActiveElement[LayerIndex, RowIndex, ColIndex] then
        begin
          X := XDataArray.RealData[LayerIndex,RowIndex,ColIndex];
          Y := YDataArray.RealData[LayerIndex,RowIndex,ColIndex];
        end
        else
        begin
          X := 0;
          Y := 0;
        end;
        FValues[LayerIndex,RowIndex,ColIndex].x := X;
        FValues[LayerIndex,RowIndex,ColIndex].y := Y;

        if ZDataArray <> nil then
        begin
          if LocalModel.ActiveElement[LayerIndex, RowIndex, ColIndex] then
          begin
            Z := ZDataArray.RealData[LayerIndex,RowIndex,ColIndex];
          end
          else
          begin
            Z := 0;
          end;
          FValues[LayerIndex,RowIndex,ColIndex].z := Z;
          TestLength := Sqrt(Sqr(X) + Sqr(Y) + Sqr(Z));
          if LogScaled and (TestLength > 0) then
          begin
            Factor := Ln(TestLength/MinPositive)/(TestLength/MinPositive);
            X := X * Factor;
            Y := Y * Factor;
            Z := Z * Factor;
            TestLength := Sqrt(Sqr(X) + Sqr(Y) + Sqr(Z));
            FValues[LayerIndex,RowIndex,ColIndex].x := X;
            FValues[LayerIndex,RowIndex,ColIndex].y := Y;
            FValues[LayerIndex,RowIndex,ColIndex].z := Z;
          end;
          if TestLength > MaxLength then
          begin
            MaxLength := TestLength;
          end;
        end
        else
        begin
          FValues[LayerIndex,RowIndex,ColIndex].z := 0;
          TestLength := Sqrt(Sqr(X) + Sqr(Y));
          if LogScaled and (TestLength > 0) then
          begin
            Factor := Ln(TestLength/MinPositive)/(TestLength/MinPositive);
            X := X * Factor;
            Y := Y * Factor;
            TestLength := Sqrt(Sqr(X) + Sqr(Y));
            FValues[LayerIndex,RowIndex,ColIndex].x := X;
            FValues[LayerIndex,RowIndex,ColIndex].y := Y;
          end;
          if TestLength > MaxLength then
          begin
            MaxLength := TestLength;
          end;
        end;
      end;
    end;
  end;
  if MaxLength <> 0 then
  begin
    FDefaultScale := DefaultMaxVectorLength/MaxLength;
  end
  else
  begin
    FDefaultScale := 1;
  end;
  FDataSetObserver.UpToDate := True;
end;

constructor TVelocityVectors.Create(Model: TBaseModel);
begin
  FModel := Model;
  inherited;
  FVectorMethod := vcMagnitude;
end;

procedure TVelocityVectors.GetDataSets(out XDataArray, YDataArray, ZDataArray: TDataArray);
var
  LocalModel: TPhastModel;
begin
  LocalModel := FModel as TPhastModel;
  if XVelocityName = '' then
  begin
    XDataArray := nil;
  end
  else
  begin
    XDataArray := LocalModel.DataArrayManager.GetDataSetByName(XVelocityName);
  end;

  if YVelocityName = '' then
  begin
    YDataArray := nil;
  end
  else
  begin
    YDataArray := LocalModel.DataArrayManager.GetDataSetByName(YVelocityName);
  end;

  if ZVelocityName = '' then
  begin
    ZDataArray := nil;
  end
  else
  begin
    ZDataArray := LocalModel.DataArrayManager.GetDataSetByName(ZVelocityName);
  end;
end;

procedure TVelocityVectors.GetObservers(List: TObserverList);
var
  XDataArray, YDataArray, ZDataArray: TDataArray;
begin
  if FModel <> nil then
  begin
    GetDataSets(XDataArray, YDataArray, ZDataArray);
    List.Add(XDataArray);
    List.Add(YDataArray);
    if ZDataArray <> nil then
    begin
      List.Add(ZDataArray);
    end;
  end;
end;

{ TPredefinedVectors }

procedure TPredefinedVectors.Assign(Source: TPersistent);
var
  VectorSource: TPredefinedVectors;
begin
  if Source is TPredefinedVectors then
  begin
    VectorSource := TPredefinedVectors(Source);
    VectorType := VectorSource.VectorType;
    VectorDirection := VectorSource.VectorDirection;
  end;
  inherited;

end;

constructor TPredefinedVectors.Create(Model: TBaseModel);
begin
  FModel := Model;
  inherited;
  FVectorMethod := vcMagnitude;
end;

procedure TPredefinedVectors.GetDataSets(out Angle1, Angle2, Angle3, DataArray,
  MaxDataArray: TDataArray);
var
  LocalModel: TPhastModel;
  Mesh: TSutraMesh3D;
begin
  LocalModel := FModel as TPhastModel;
  Mesh := LocalModel.SutraMesh;
  Angle1 := LocalModel.DataArrayManager.GetDataSetByName(KHorizontalAngle);
  if Mesh.MeshType = mt3D then
  begin
    Angle2 := LocalModel.DataArrayManager.GetDataSetByName(KVerticalAngle);
    Angle3 := LocalModel.DataArrayManager.GetDataSetByName(KRotationalAngle);
  end
  else
  begin
    Angle2 := nil;
    Angle3 := nil;
  end;
  case VectorType of
    pvtPermeability:
      begin
        MaxDataArray := LocalModel.DataArrayManager.GetDataSetByName(KMaximumPermeability);
        case VectorDirection of
          pvdMax:
            DataArray := LocalModel.DataArrayManager.GetDataSetByName(KMaximumPermeability);
          pvdMid:
            DataArray := LocalModel.DataArrayManager.GetDataSetByName(KMiddlePermeability);
          pvdMin:
            DataArray := LocalModel.DataArrayManager.GetDataSetByName(KMinimumPermeability);
        else
          Assert(False);
        end;
      end;
    pvtConductivity:
      begin
        MaxDataArray := LocalModel.DataArrayManager.GetDataSetByName(KMaximumK);
        case VectorDirection of
          pvdMax:
            DataArray := LocalModel.DataArrayManager.GetDataSetByName(KMaximumK);
          pvdMid:
            DataArray := LocalModel.DataArrayManager.GetDataSetByName(KMiddleK);
          pvdMin:
            DataArray := LocalModel.DataArrayManager.GetDataSetByName(KMinimumK);
        else
          Assert(False);
        end;
      end;
    pvtLongitudianalDispersivity:
      begin
        MaxDataArray := LocalModel.DataArrayManager.GetDataSetByName(KMaxLongitudinalDisp);
        case VectorDirection of
          pvdMax:
            DataArray := LocalModel.DataArrayManager.GetDataSetByName(KMaxLongitudinalDisp);
          pvdMid:
            DataArray := LocalModel.DataArrayManager.GetDataSetByName(KMidLongitudinalDisp);
          pvdMin:
            DataArray := LocalModel.DataArrayManager.GetDataSetByName(KMinLongitudinalDisp);
        else
          Assert(False);
        end;
      end;
    pvtTransverseDispersivity:
      begin
        MaxDataArray := LocalModel.DataArrayManager.GetDataSetByName(KMaxTransverseDisp);
        case VectorDirection of
          pvdMax:
            DataArray := LocalModel.DataArrayManager.GetDataSetByName(KMaxTransverseDisp);
          pvdMid:
            DataArray := LocalModel.DataArrayManager.GetDataSetByName(KMidTransverseDisp);
          pvdMin:
            DataArray := LocalModel.DataArrayManager.GetDataSetByName(KMinTransverseDisp);
        else
          Assert(False);
        end;
      end;
  else
    Assert(False);
  end;
end;

procedure TPredefinedVectors.GetObservers(List: TObserverList);
var
  Angle1, Angle2, Angle3, DataArray, MaxDataArray: TDataArray;
begin
  if FModel <> nil then
  begin
    GetDataSets(Angle1, Angle2, Angle3, DataArray, MaxDataArray);
    List.Add(Angle1);
    if Angle2 <> nil then
    begin
      List.Add(Angle2);
    end;
    if Angle3 <> nil then
    begin
      List.Add(Angle3);
    end;
    List.Add(DataArray);
    if DataArray <> MaxDataArray then
    begin
      List.Add(MaxDataArray);
    end;
  end;
end;

procedure TPredefinedVectors.SetVectorDirection(
  const Value: TPredefinedVectorDirection);
begin
  if FVectorDirection <> Value then
  begin
    StopListening;
    FVectorDirection := Value;
    StartListening;
    InvalidateModel;
    InvalidateAllViews;
  end;
  if FVisible and not FListening then
  begin
    StartListening;
  end;
end;

procedure TPredefinedVectors.SetVectorType(const Value: TPredefinedVectorType);
begin
  if FVectorType <> Value then
  begin
    StopListening;
    FVectorType := Value;
    StartListening;
    InvalidateModel;
    InvalidateAllViews;
  end;
  if FVisible and not FListening then
  begin
    StartListening;
  end;
end;

procedure TPredefinedVectors.UpdateVectors;
var
  LocalModel: TPhastModel;
//  Mesh: TSutraMesh3D;
  Angle1: TDataArray;
  Angle2: TDataArray;
  Angle3: TDataArray;
  ColIndex: Integer;
  LayerIndex: Integer;
  Element: TSutraElement3D;
  A1: Double;
  A2: Double;
  A3: Double;
  G11,G12,G13,G21,G22,G23,G31,G32,G33: double;
  X,Y,Z, XP,YP,ZP: double;
  Value: double;
  DataArray: TDataArray;
  MaxDataArray: TDataArray;
  MinPositive: double;
  TestDistance: double;
  MaxLength: double;
  TestLength: double;
  Factor: double;
  Mesh: TSutraMesh3D;
  procedure GetRotatedValues;
  begin
    A1 := A1*DegToRadiansFactor;
    A2 := A2*DegToRadiansFactor;
    A3 := A3*DegToRadiansFactor;
    ROTMAT(A1,A2,A3, G11,G12,G13,G21,G22,G23,G31,G32,G33);
    case VectorDirection of
      pvdMax:
        begin
          X := Value;
          Y := 0;
          Z := 0;
        end;
      pvdMid:
        begin
          X := 0;
          Y := Value;
          Z := 0;
        end;
      pvdMin:
        begin
          case Mesh.MeshType of
            mt2D, mtProfile:
              begin
                X := 0;
                Y := Value;
                Z := 0;
              end;
            mt3D:
              begin
                X := 0;
                Y := 0;
                Z := Value;
              end;
            else Assert(False);
          end;
        end;
      else Assert(False);
    end;
    Rotate(G11,G12,G13,G21,G22,G23,G31,G32,G33,X,Y,Z, XP,YP,ZP);
  end;
begin
  LocalModel := FModel as TPhastModel;
  Mesh := LocalModel.SutraMesh;
  if Mesh.Mesh2D.Nodes.Count = 0 then
  begin
    Exit;
  end;
  GetDataSets(Angle1, Angle2, Angle3, DataArray, MaxDataArray);
  Assert(Angle1 <> nil);
  Assert(DataArray <> nil);
  Assert(MaxDataArray <> nil);
  if Mesh.MeshType = mt3D then
  begin
    if (Angle2 = nil) or (Angle3 = nil) then
    begin
      Exit;
    end;
  end;

  Angle1.Initialize;
  DataArray.Initialize;
  MaxDataArray.Initialize;
  if Mesh.MeshType = mt3D then
  begin
    Assert(Angle2 <> nil);
    Angle2.Initialize;

    Assert(Angle3 <> nil);
    Angle3.Initialize;
  end;

  inherited;

  if Length(FValues) = 0 then
  begin
    Exit;
  end;


  if MaxDataArray.MaxReal > 0 then
  begin
    FDefaultScale := DefaultMaxVectorLength/MaxDataArray.MaxReal;
  end
  else
  begin
    FDefaultScale := 1;
  end;


  MinPositive := 0;
  if LogScaled then
  begin
    for ColIndex := 0 to Mesh.Mesh2D.Elements.Count - 1 do
    begin
      for LayerIndex := 0 to Mesh.LayerCount - 1 do
      begin
        if Mesh.MeshType = mt3D then
        begin
          Element := Mesh.ElementArray[LayerIndex,ColIndex];
          if Element.Active then
          begin
            A1 := Angle1.RealData[LayerIndex,0,ColIndex];
            A2 := Angle2.RealData[LayerIndex,0,ColIndex];
            A3 := Angle3.RealData[LayerIndex,0,ColIndex];
            Value := DataArray.RealData[LayerIndex,0,ColIndex];
            GetRotatedValues;
            TestDistance := Sqrt(Sqr(XP) + Sqr(YP) + Sqr(ZP));
            if (TestDistance > 0) and ((MinPositive = 0) or (TestDistance < MinPositive)) then
            begin
              MinPositive := TestDistance
            end;
          end;
        end
        else
        begin
          A1 := Angle1.RealData[LayerIndex,0,ColIndex];
          A2 := 0;
          A3 := 0;
          Value := DataArray.RealData[LayerIndex,0,ColIndex];
          GetRotatedValues;
          TestDistance := Sqrt(Sqr(XP) + Sqr(YP) + Sqr(ZP));
          if (TestDistance > 0) and ((MinPositive = 0) or (TestDistance < MinPositive)) then
          begin
            MinPositive := TestDistance
          end;
        end;
      end;
    end
  end;

  MinPositive := MinPositive/10;

  MaxLength := 0;
  for ColIndex := 0 to Mesh.Mesh2D.Elements.Count - 1 do
  begin
    for LayerIndex := 0 to Mesh.LayerCount - 1 do
    begin
      if Mesh.MeshType = mt3D then
      begin
        Element := Mesh.ElementArray[LayerIndex,ColIndex];
        if Element.Active then
        begin
          A1 := Angle1.RealData[LayerIndex,0,ColIndex];
          A2 := Angle2.RealData[LayerIndex,0,ColIndex];
          A3 := Angle3.RealData[LayerIndex,0,ColIndex];
          Value := DataArray.RealData[LayerIndex,0,ColIndex];
          GetRotatedValues;
          TestLength := Sqrt(Sqr(XP) + Sqr(YP) + Sqr(ZP));
          if LogScaled and (MinPositive > 0) then
          begin
            Factor := Ln(TestLength/MinPositive)/(TestLength/MinPositive);
            XP := XP * Factor;
            YP := YP * Factor;
            ZP := ZP * Factor;
            TestLength := Sqrt(Sqr(XP) + Sqr(YP) + Sqr(ZP));
          end;
          FValues[LayerIndex,0,ColIndex].x := XP;
          FValues[LayerIndex,0,ColIndex].y := YP;
          FValues[LayerIndex,0,ColIndex].z := ZP;
          if TestLength > MaxLength then
          begin
            MaxLength := TestLength;
          end;
        end
        else
        begin
          FValues[LayerIndex,0,ColIndex].x := 0;
          FValues[LayerIndex,0,ColIndex].y := 0;
          FValues[LayerIndex,0,ColIndex].z := 0;
        end;
      end
      else
      begin
        A1 := Angle1.RealData[LayerIndex,0,ColIndex];
        A2 := 0;
        A3 := 0;
        Value := DataArray.RealData[LayerIndex,0,ColIndex];
        GetRotatedValues;
        TestLength := Sqrt(Sqr(XP) + Sqr(YP) + Sqr(ZP));
        if LogScaled and (MinPositive > 0) then
        begin
          Factor := Ln(TestLength/MinPositive)/(TestLength/MinPositive);
          XP := XP * Factor;
          YP := YP * Factor;
          ZP := ZP * Factor;
          TestLength := Sqrt(Sqr(XP) + Sqr(YP) + Sqr(ZP));
        end;
        FValues[LayerIndex,0,ColIndex].x := XP;
        FValues[LayerIndex,0,ColIndex].y := YP;
        FValues[LayerIndex,0,ColIndex].z := ZP;
        if TestLength > MaxLength then
        begin
          MaxLength := TestLength;
        end;
      end;
    end;
  end;

  FDataSetObserver.UpToDate := True;
end;

{ TVectorItem }

procedure TVectorItem.Assign(Source: TPersistent);
var
  SourceItem: TVectorItem;
begin
  if Source is TVectorItem then
  begin
    SourceItem := TVectorItem(Source);
    Vectors := SourceItem.Vectors;
    Description := SourceItem.Description;
  end
  else
  begin
    inherited;
  end;
end;

constructor TVectorItem.Create(Collection: TCollection);
begin
  inherited;
  FVectors := TVelocityVectors.Create(Model);
end;

destructor TVectorItem.Destroy;
begin
  FVectors.Free;
  inherited;
end;

function TVectorItem.IsValid: boolean;
var
  MeshType: TMeshType;
  LocalModel: TPhastModel;
  DataArrayManager: TDataArrayManager;
begin
  result := True;
  LocalModel := frmGoPhast.PhastModel;
  MeshType := LocalModel.SutraMesh.MeshType;
  DataArrayManager := LocalModel.DataArrayManager;
  if LocalModel.ModelSelection in SutraSelection then
  begin
    case MeshType of
      mt2D, mtProfile:
        begin
          if Vectors.ZVelocityName <> '' then
          begin
            result := False;
          end
          else if DataArrayManager.GetDataSetByName(
            Vectors.XVelocityName) = nil then
          begin
            result := False;
          end
          else if DataArrayManager.GetDataSetByName(
            Vectors.YVelocityName) = nil then
          begin
            result := False;
          end;
        end;
      mt3D:
        begin
          if DataArrayManager.GetDataSetByName(
            Vectors.XVelocityName) = nil then
          begin
            result := False;
          end
          else if DataArrayManager.GetDataSetByName(
            Vectors.YVelocityName) = nil then
          begin
            result := False;
          end
          else if (Vectors.ZVelocityName = '')
            or (DataArrayManager.GetDataSetByName(
            Vectors.ZVelocityName) = nil) then
          begin
            result := False;
          end;
        end;
      else
        Assert(False);
    end;
  end
  else
  begin
    if DataArrayManager.GetDataSetByName(
      Vectors.XVelocityName) = nil then
    begin
      result := False;
    end
    else if DataArrayManager.GetDataSetByName(
      Vectors.YVelocityName) = nil then
    begin
      result := False;
    end
    else if DataArrayManager.GetDataSetByName(
      Vectors.ZVelocityName) = nil then
    begin
      result := False;
    end
  end;
end;

function TVectorItem.Model: TBaseModel;
begin
  result := (Collection as TVectorCollection).Model;
end;

procedure TVectorItem.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TVectorItem.SetVectors(const Value: TVelocityVectors);
begin
  FVectors.Assign(Value);
end;

{ TVectorCollection }

function TVectorCollection.Add: TVectorItem;
begin
  result := inherited Add as TVectorItem;
end;

procedure TVectorCollection.Assign(Source: TPersistent);
begin
  inherited;
  BeginUpdate;
  try
    if Source is TVectorCollection then
    begin
      SelectedItem := TVectorCollection(Source).SelectedItem
    end;
  finally
    EndUpdate
  end;
end;

procedure TVectorCollection.CheckDataSets;
var
  index: Integer;
  AnItem: TVectorItem;
begin
  for index := Count - 1 downto 0 do
  begin
    AnItem := Items[index] as TVectorItem;
    if not AnItem.IsValid then
    begin
      if Index = FSelectedItem then
      begin
        FSelectedItem := -1
      end
      else if Index < FSelectedItem then
      begin
        Dec(FSelectedItem);
      end;
      Delete(Index);
    end;
  end;
end;

constructor TVectorCollection.Create(Model: TBaseModel);
var
  InvalidateModelEvent: TNotifyEvent;
begin
  FModel := Model;
  if Model = nil then
  begin
    InvalidateModelEvent := nil;
  end
  else
  begin
    InvalidateModelEvent := Model.DoInvalidate;
  end;
  inherited Create(TVectorItem, InvalidateModelEvent);
  FSelectedItem := -1;
end;

procedure TVectorCollection.EndUpdate;
var
  index: Integer;
begin
  inherited;
  if UpdateCount = 0 then
  begin
    for index := 0 to Count - 1 do
    begin
      (Items[index] as TVectorItem).Vectors.Visible := index = FSelectedItem;
    end;
  end;
end;

procedure TVectorCollection.SetItemByName(const ADescription: string);
var
  NewIndex: integer;
  index: integer;
begin
  NewIndex := -1;
  for index := 0 to Count - 1 do
  begin
    if (Items[FSelectedItem] as TVectorItem).Description = ADescription then
    begin
      NewIndex := index;
      break;
    end;
  end;
  SelectedItem := NewIndex;
end;

procedure TVectorCollection.SetSelectedItem(const Value: Integer);
begin
  if FSelectedItem <> Value then
  begin
    if (FSelectedItem >= 0) and (FSelectedItem < Count) then
    begin
      (Items[FSelectedItem] as TVectorItem).Vectors.Visible := False;
    end;
    FSelectedItem := Value;
    if (FSelectedItem >= 0) and (FSelectedItem < Count) then
    begin
      (Items[FSelectedItem] as TVectorItem).Vectors.Visible := True;
    end;
  end;
end;

function TVectorCollection.StoreSelected: boolean;
begin
  // Save the selected velocity vector in the display settings
  // but not for the model in general.
  result := FModel = nil;
end;

end.
