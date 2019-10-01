// @name defines classes that are used in the display of model properties
// that are
// on the edges between model cells.  Currently, the Horizontal Flow
// Barrier package is the only example of this supported in ModelMuse.
unit EdgeDisplayUnit;

interface

uses Types, SysUtils, Classes, Contnrs , Graphics, GR32, SubscriptionUnit,
  DataSetUnit, FastGEO, GoPhastTypes, ModflowIrregularMeshUnit;

type
  // @name is a base class used in the display of model properties that are
  // on the edges between model cells.  Currently, the Horizontal Flow
  // Barrier package is the only example of this supported in ModelMuse.
  // @seealso(TBarrier).
  TCustomModflowGridEdgeFeature = class(TObject)
  private
    function GetSharedNode(Index: Integer): TModflowNode;
  protected
    // See @link(Col1).
    FCol1: integer;
    // See @link(Col2).
    FCol2: integer;
    // See @link(Row1).
    FRow1: integer;
    // See @link(Row2).
    FRow2: integer;
    // See @link(Layer).
    FLayer: integer;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    FModel: TBaseModel;
    FDisvSharedNodes: TModflowNodeList;
    // See @link(RealAnnotation).
    function GetRealAnnotation(Index: integer): string; virtual; abstract;
    // See @link(RealValue).
    function GetRealValue(Index: integer): double; virtual; abstract;
    // @name is the starting point of the @classname in screen coordinates.
    function StartPoint: TPoint;
    // @name is the ending point of the @classname in screen coordinates.
    function EndPoint: TPoint;
    function GetHasData: Boolean; virtual;
  public
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    constructor Create(AModel: TBaseModel);
    destructor  Destroy; override;
    // @name is the number of the first column
    // defining the location of @classname.
    property Col1: integer Read FCol1;
    // @name is the number of the second column
    // defining the location of @classname.
    property Col2: integer Read FCol2;
    // @name is the number of the first row
    // defining the location of @classname.
    property Row1: integer Read FRow1;
    // @name is the number of the second row
    // defining the location of @classname.
    property Row2: integer Read FRow2;
    // @name is the number of the layer
    // defining the location of @classname.
    property Layer: integer Read FLayer;
    // @name returns true if ALayer equals @link(Layer) and
    // the point defined by X and Y is sufficiently close to the
    // @classname.
    function Select(Const X, Y, ALayer: integer): boolean;
    // @name returns a real number.  Index is used to determine which
    // value to return.
    property RealValue[Index: integer]: double read GetRealValue;
    // @name returns a description of how RealValue[Index] was assigned.
    // Index is used to determine which value to return.
    property RealAnnotation[Index: integer]: string read GetRealAnnotation;
    // @name returns the ending location of the
    // @classname in real-world coordinates.
    function EndingLocation: TPoint2D;
    // @name returns the starting location of the
    // @classname in real-world coordinates.
    function StartingLocation: TPoint2D;
    procedure UpdateDisvSharedNodes;
    property SharedNodes[Index: Integer]: TModflowNode read GetSharedNode;
    property HasData: Boolean read GetHasData;
  end;

  TEdgeDisplaySettings = class(TPersistent)
  private
    FLimits: TColoringLimits;
    FDataToPlot: integer;
    FVisible: boolean;
    procedure SetDataToPlot(const Value: integer);
    procedure SetLimits(const Value: TColoringLimits);
    procedure SetVisible(const Value: boolean);
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
  published
    property Limits: TColoringLimits read FLimits write SetLimits;
    property DataToPlot: integer read FDataToPlot write SetDataToPlot;
    property Visible: boolean read FVisible write SetVisible;
  end;

  // @name stores but does not own a list of
  // @link(TCustomModflowGridEdgeFeature)s.
  // It is used to display the @link(TCustomModflowGridEdgeFeature)s.
  TCustomModflowGridEdgeDisplay = class(TObserver)
  private
    // @name holds the @link(TCustomModflowGridEdgeFeature)s.
    FList: TList;
    // @name holds a @link(TColoringLimits) for each type of data
    // that can be displayed.
    FLimits: TList;
    // See @link(DataToPlot).
    FDataToPlot: integer;
    // See @link(OnNeedToUpdate).
    FOnNeedToUpdate: TNotifyEvent;
    // See @link(MaxValue).
    FMaxValue: string;
    // See @link(MinValue).
    FMinValue: string;
    // @name is set to @true in @link(UpdateMinMax) to prevent recursion.
    FUpdatingMinMax: Boolean;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    FModel: TBaseModel;
    FDisplayTime: Double;
    // See @link(RealAnnotation).
    function GetRealAnnotation(X, Y, ALayer: integer): string;
    // See @link(RealValue).
    function GetRealValue(X, Y, ALayer: integer): double;
    // See @link(Limits).
    function GetLimits(Index: integer): TColoringLimits;
    // See @link(Limits).
    procedure SetLimits(Index: integer; const Value: TColoringLimits);
    // See @link(DataToPlot).
    procedure SetDataToPlot(const Value: integer);
    // See @link(Count).
    function GetCount: integer;
    // See @link(Edges).
    function GetEdge(Index: integer): TCustomModflowGridEdgeFeature;
    // See @link(MaxValue).
    function GetMaxValue: string;
    // See @link(MinValue).
    function GetMinValue: string;
    // @name sets MinValue, MaxValue to the minimum and maximum value
    // of the data indicated by @link(DataToPlot).
    procedure GetMinimumAndMaximumValues(out MinValue, MaxValue, MinPositive: Double);
    procedure SetDisplayTime(const Value: Double);
  protected
    // See @link(RealDescription).
    function GetDescription(DataIndex: integer): string; virtual; abstract;
    // See @link(RealValueTypeCount).
    function GetRealValueTypeCount: integer; virtual; abstract;
  public
    function UseEdge(ActiveDataArray: TDataArray;
      Feature: TCustomModflowGridEdgeFeature): boolean;
    // @name returns the minimum and maximum value allowed when drawing the
    // @link(TCustomModflowGridEdgeFeature)s.
    procedure GetValueRangeToDisplay(out MinValue, MaxValue, MinPositive: Double);
    // @name indicates the range of values that can be displayed in
    // the data indicated by @link(DataToPlot).
    property Limits[Index: integer]: TColoringLimits read GetLimits
      write SetLimits;
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    // remove all the @link(TCustomModflowGridEdgeFeature)s from FList.
    procedure Clear;
    // add a @link(TCustomModflowGridEdgeFeature)s to FList.
    procedure Add(EdgeFeature: TCustomModflowGridEdgeFeature);
    // @name draws the @link(TCustomModflowGridEdgeFeature)s that
    // are on Layer on Bitmap32.
    Procedure Draw(Layer: integer; Const Bitmap32: TPersistent);
    // @name returns @true if any of the @link(TCustomModflowGridEdgeFeature)s
    // would be selected by the values X, Y and Layer.
    // See @link(TCustomModflowGridEdgeFeature.Select).
    function Select(Const X, Y, Layer: integer): boolean;
    // @name is the value at X, Y, and ALayer.
    property RealValue[X, Y, ALayer: integer]: double read GetRealValue;
    // @name returns the explanation of how @link(RealValue) was assigned.
    property RealAnnotation[X, Y, ALayer: integer]: string
      read GetRealAnnotation;
    // @name returns a description suitable for display in a GUI
    // of the data indicated by DataIndex.
    property RealDescription[DataIndex: integer]: string read GetDescription;
    // @name is the number of different types of data that can be displayed by
    // the @classname.
    property RealValueTypeCount: integer read GetRealValueTypeCount;
    // @name is used to indicate which
    // @link(TCustomModflowGridEdgeFeature.RealValue) to plot.
    property DataToPlot: integer read FDataToPlot write SetDataToPlot;
    // @name is used to update the data.
    property OnNeedToUpdate: TNotifyEvent read FOnNeedToUpdate
      write FOnNeedToUpdate;
    // @name is the number of @link(TCustomModflowGridEdgeFeature)s
    // in @classname.
    property Count: integer read GetCount;
    // @name is used to read the @link(TCustomModflowGridEdgeFeature)s
    // in @classname.
    property Edges[Index: integer]: TCustomModflowGridEdgeFeature
      read GetEdge; default;
    // @name sets @link(TObserver.UpToDate) to @false.
    procedure Invalidate; virtual;
    // @name will update the data if @link(OnNeedToUpdate) is assigned.
    procedure UpdateData;
    // @name updates @link(MinValue) and @link(MaxValue)
    procedure UpdateMinMax;
    // @name is a string representation of the minimum value among the
    // @link(TCustomModflowGridEdgeFeature)s for @link(DataToPlot).
    property MinValue: string read GetMinValue;
    // @name is a string representation of the maximum value among the
    // @link(TCustomModflowGridEdgeFeature)s for @link(DataToPlot).
    property MaxValue: string read GetMaxValue;
    property DisplayTime: Double read FDisplayTime write SetDisplayTime;
  end;

implementation

uses Math, frmGoPhastUnit, AbstractGridUnit,
  BigCanvasMethods, PhastModelUnit, frmDisplayDataUnit;

resourcestring
  StrAverageOfSeveralV = 'Average of several values:';

{ TCustomModflowGridEdgeDisplay }

procedure TCustomModflowGridEdgeDisplay.Add(EdgeFeature: TCustomModflowGridEdgeFeature);
begin
  FList.Add(EdgeFeature);
end;

procedure TCustomModflowGridEdgeDisplay.Clear;
begin
  FList.Clear;
end;

constructor TCustomModflowGridEdgeDisplay.Create(AnOwner: TComponent);
var
  Index: Integer;
  Limit: TColoringLimits;
begin
  Assert(AnOwner <> nil);
  FModel := AnOwner as TBaseModel;
  inherited Create(nil);
  FList := TList.Create;
  FLimits := TObjectList.Create;
  for Index := 0 to RealValueTypeCount - 1 do
  begin
    Limit := TColoringLimits.Create;
    FLimits.Add(Limit);
  end;
end;

destructor TCustomModflowGridEdgeDisplay.Destroy;
begin
  FList.Free;
  FLimits.Free;
  inherited;
end;

procedure TCustomModflowGridEdgeDisplay.Draw(Layer: integer;
  Const Bitmap32: TPersistent);
var
  FeatureIndex: integer;
  Feature: TCustomModflowGridEdgeFeature;
  MinValue, MaxValue, MinPositive: double;
  Value: Double;
  List: TList;
  Color: TColor;
  LineColor32: TColor32;
  CanvasCoordinates: TPointArray;
  LineWidth: Integer;
  ColoringLimits: TColoringLimits;
  ActiveDataArray: TDataArray;
  LogMin, LogMax: double;
begin
  UpdateData;
  LineWidth := 3;
  SetLength(CanvasCoordinates, 2);
  List := TList.Create;
  try
    GetValueRangeToDisplay(MinValue, MaxValue, MinPositive);

    ColoringLimits := Limits[DataToPlot];
    LogMin := 0;
    LogMax := 0;
    if ColoringLimits.LogTransform then
    begin
      if (MinValue <= 0) then
      begin
        MinValue := MinPositive;
      end;
      if (MaxValue <= 0) then
      begin
        MaxValue := MinPositive;
      end;
      if (MinValue > 0) then
      begin
        LogMin := Log10(MinValue);
        LogMax := Log10(MaxValue);
      end;
    end;
    ActiveDataArray := nil;
    if ColoringLimits.ActiveOnly then
    begin
//      ActiveDataArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(rsActive);
      ActiveDataArray := (FModel as TCustomModel).DataArrayManager.GetDataSetByName(rsActive);
      ActiveDataArray.Initialize;
    end;

    for FeatureIndex := 0 to FList.Count - 1 do
    begin
      Feature := FList[FeatureIndex];
      if not Feature.HasData then
      begin
        Continue;
      end;
      Value := Feature.RealValue[DataToPlot];
      if (Feature.Layer = Layer)
        and UseEdge(ActiveDataArray, Feature)
        and ColoringLimits.ValueOk(Value) then
      begin
        List.Add(Feature)
      end;
    end;
//    Color := clWhite;
    for FeatureIndex := 0 to List.Count - 1 do
    begin
      Feature := List[FeatureIndex];
      Value := Feature.RealValue[DataToPlot];
      if (Value < MinValue) or (Value > MaxValue) then
      begin
        Continue;
      end
      else if MaxValue = MinValue then
      begin
        Color := GridFracToRainbow(0.5);
      end
      else
      begin
        if ColoringLimits.LogTransform then
        begin
          Assert(Value > 0);
          Value := Log10(Value);
          Color := GridFracToRainbow((LogMax - Value)/(LogMax - LogMin));
        end
        else
        begin
          Color := GridFracToRainbow((MaxValue - Value)/(MaxValue - MinValue));
        end;
      end;
      LineColor32 := Color32(Color);
      Feature.UpdateDisvSharedNodes;
      CanvasCoordinates[0] := Feature.StartPoint;
      CanvasCoordinates[1] := Feature.EndPoint;
      DrawBigPolyline32(Bitmap32, LineColor32, LineWidth,
        CanvasCoordinates, True);
    end;
  finally
    List.Free;
  end;
end;

function TCustomModflowGridEdgeDisplay.GetCount: integer;
begin
  result := FList.Count;
end;

function TCustomModflowGridEdgeDisplay.GetEdge(
  Index: integer): TCustomModflowGridEdgeFeature;
begin
  result := FList[Index];
end;

function TCustomModflowGridEdgeDisplay.GetLimits(Index: integer): TColoringLimits;
begin
  result := FLimits[Index];
end;

function TCustomModflowGridEdgeDisplay.GetMaxValue: string;
begin
  if UpToDate then
  begin
    result := FMaxValue;
  end
  else
  begin
    result := '?';
  end;
end;

function TCustomModflowGridEdgeDisplay.GetMinValue: string;
begin
  if UpToDate then
  begin
    result := FMinValue;
  end
  else
  begin
    result := '?';
  end;
end;

function TCustomModflowGridEdgeDisplay.GetRealAnnotation(X, Y,
  ALayer: integer): string;
var
  ItemIndex: integer;
  Annotations: TStringList;
  Edge: TCustomModflowGridEdgeFeature;
begin
  result := '';
  Assert((DataToPlot >= 0) and (DataToPlot < RealValueTypeCount));
  Annotations := TStringList.Create;
  try
    for ItemIndex := 0 to FList.Count - 1 do
    begin
      Edge := FList[ItemIndex];
      if Edge.Select(X, Y, ALayer) then
      begin
        Annotations.Add(Edge.RealAnnotation[DataToPlot])
      end;
    end;
    if Annotations.Count = 1 then
    begin
      result := Annotations[0];
    end
    else if Annotations.Count > 1 then
    begin
      Annotations.Insert(0, StrAverageOfSeveralV);
      result := Annotations.Text
    end;
  finally
    Annotations.Free;
  end;
end;

function TCustomModflowGridEdgeDisplay.GetRealValue(X, Y,
  ALayer: integer): double;
var
  ItemIndex: integer;
  Edge: TCustomModflowGridEdgeFeature;
  Count: integer;
begin
  Count:= 0;
  result := 0;
  Assert((DataToPlot >= 0) and (DataToPlot < RealValueTypeCount));
  for ItemIndex := 0 to FList.Count - 1 do
  begin
    Edge := FList[ItemIndex];
    if Edge.Select(X, Y, ALayer) then
    begin
      result := result + Edge.RealValue[DataToPlot];
      Inc(Count);
    end;
  end;
  if Count > 1 then
  begin
    result := result/Count;
  end;
end;

function TCustomModflowGridEdgeDisplay.Select(const X, Y, Layer: integer): boolean;
var
  Index: Integer;
  Feature: TCustomModflowGridEdgeFeature;
begin
  result := False;
  for Index := 0 to FList.Count - 1 do
  begin
    Feature := FList[Index];
    result := Feature.Select(X, Y, Layer);
    if result then
      Exit;
  end;
end;

procedure TCustomModflowGridEdgeDisplay.UpdateData;
begin
  if not UpToDate and Assigned(FOnNeedToUpdate) then
  begin
    FOnNeedToUpdate(self);
    UpdateMinMax;
  end;
end;

procedure TCustomModflowGridEdgeDisplay.UpdateMinMax;
var
  MinValue, MaxValue, MinPositive: double;
begin
  if not UpToDate then
  begin
    Exit;
  end;
  if FUpdatingMinMax then
  begin
    Exit;
  end;
  FUpdatingMinMax := True;
  try
    GetMinimumAndMaximumValues(MinValue, MaxValue, MinPositive);
    FMinValue := FloatToStrF(MinValue, ffGeneral, 7, 0);
    FMaxValue := FloatToStrF(MaxValue, ffGeneral, 7, 0);
//    if frmGridColor <> nil then
//    begin
//      frmGridColor.UpdateLabelsAndLegend
//    end;
    if frmDisplayData <> nil then
    begin
      frmDisplayData.UpdateLabelsAndLegend;
    end;
  finally
    FUpdatingMinMax := False;
  end;
end;

function TCustomModflowGridEdgeDisplay.UseEdge(ActiveDataArray: TDataArray;
  Feature: TCustomModflowGridEdgeFeature): boolean;
begin
  result := Feature.HasData;
  if result and (ActiveDataArray <> nil) then
  begin
    result := ActiveDataArray.BooleanData[
      Feature.Layer, Feature.Row1, Feature.Col1]
      and ActiveDataArray.BooleanData[
      Feature.Layer, Feature.Row2, Feature.Col2];
  end;
end;

procedure TCustomModflowGridEdgeDisplay.GetValueRangeToDisplay(
  out MinValue, MaxValue, MinPositive: Double);
begin
  GetMinimumAndMaximumValues(MinValue, MaxValue, MinPositive);
  if Limits[DataToPlot].LowerLimit.UseLimit then
  begin
    MinValue := Limits[DataToPlot].LowerLimit.RealLimitValue;
  end;
  if Limits[DataToPlot].UpperLimit.UseLimit then
  begin
    MaxValue := Limits[DataToPlot].UpperLimit.RealLimitValue;
  end;
end;


procedure TCustomModflowGridEdgeDisplay.Invalidate;
begin
  UpToDate := False;
end;

procedure TCustomModflowGridEdgeDisplay.GetMinimumAndMaximumValues(
  out MinValue, MaxValue, MinPositive: Double);
var
  Value: Double;
  Feature: TCustomModflowGridEdgeFeature;
  FeatureIndex: Integer;
  ColoringLimits: TColoringLimits;
  ActiveDataArray: TDataArray;
  FirstFound: boolean;
begin
  MinValue := 0;
  MaxValue := 0;
  MinPositive := 0;
  ColoringLimits := Limits[DataToPlot];
  ActiveDataArray := nil;
  if ColoringLimits.ActiveOnly then
  begin
//    ActiveDataArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(rsActive);
    ActiveDataArray := (FModel as TCustomModel).DataArrayManager.GetDataSetByName(rsActive);
    ActiveDataArray.Initialize;
  end;
  FirstFound := False;
  for FeatureIndex := 0 to FList.Count - 1 do
  begin
    Feature := FList[FeatureIndex];
    if UseEdge(ActiveDataArray, Feature) then
    begin
      if not FirstFound then
      begin
        MinValue := Feature.RealValue[DataToPlot];
        MaxValue := MinValue;
        if MinValue > 0 then
        begin
          MinPositive := MinValue;
        end;
        FirstFound := True;
      end
      else
      begin
        Value := Feature.RealValue[DataToPlot];
        if MinValue > Value then
        begin
          MinValue := Value;
        end;
        if MaxValue < Value then
        begin
          MaxValue := Value;
        end;
        if Value > 0 then
        begin
          if (MinPositive = 0) or (Value < MinPositive) then
          begin
            MinPositive := Value;
          end;
        end;
      end;
    end;
  end;
end;

procedure TCustomModflowGridEdgeDisplay.SetDataToPlot(const Value: integer);
begin
  FDataToPlot := Value;
end;

procedure TCustomModflowGridEdgeDisplay.SetDisplayTime(const Value: Double);
begin
  if FDisplayTime <> Value then
  begin
    FDisplayTime := Value;
    Invalidate;
  end;
end;

procedure TCustomModflowGridEdgeDisplay.SetLimits(Index: integer;
  const Value: TColoringLimits);
begin
  Limits[Index].Assign(Value);
end;

{ TCustomModflowGridEdgeFeature }

function TCustomModflowGridEdgeFeature.EndPoint: TPoint;
var
  Point: TPoint2D;
begin
  Point := EndingLocation;
  result.X := frmGoPhast.frameTopView.ZoomBox.XCoord(Point.x);
  result.Y := frmGoPhast.frameTopView.ZoomBox.YCoord(Point.y);
end;

function TCustomModflowGridEdgeFeature.GetHasData: Boolean;
begin
  result := True;
end;

function TCustomModflowGridEdgeFeature.GetSharedNode(
  Index: Integer): TModflowNode;
begin
  result := FDisvSharedNodes[Index];
end;

function TCustomModflowGridEdgeFeature.Select(const X, Y, ALayer: integer): boolean;
var
  SelectionPoint: TPoint2D;
  DisplayLine: TSegment2D;
  Point: TPoint;
begin
  result := (ALayer = Layer) and HasData;
  if not result then Exit;
  SelectionPoint.x := x;
  SelectionPoint.y := y;
  Point := StartPoint;
  DisplayLine[1].x := Point.x;
  DisplayLine[1].y := Point.y;
  Point := EndPoint;
  DisplayLine[2].x := Point.x;
  DisplayLine[2].y := Point.y;
  result := Distance(SelectionPoint, DisplayLine) < SelectEpsilon
end;

function TCustomModflowGridEdgeFeature.StartingLocation: TPoint2D;
var
  Row: Integer;
  Col: Integer;
begin
  if (FModel as TCustomModel).DisvUsed then
  begin
    result := FDisvSharedNodes[0].Location;
  end
  else
  begin
    if Col1 = Col2 then
    begin
      Assert(Abs(Row1 - Row2) = 1);
      Row := Max(Row1, Row2);
  //    result := frmGoPhast.ModflowGrid.TwoDElementCorner(Col1, Row);
      result := (FModel as TCustomModel).ModflowGrid.TwoDElementCorner(Col1, Row);
    end
    else
    begin
      Assert(Row1 = Row2);
      Assert(Abs(Col1 - Col2) = 1);
      Col := Max(Col1, Col2);
  //    result := frmGoPhast.ModflowGrid.TwoDElementCorner(Col, Row1);
      result := (FModel as TCustomModel).ModflowGrid.TwoDElementCorner(Col, Row1);
    end;
  end;
end;

constructor TCustomModflowGridEdgeFeature.Create(AModel: TBaseModel);
begin
  Assert(AModel <> nil);
  FModel := AModel;
  FDisvSharedNodes := TModflowNodeList.Create;
end;

destructor TCustomModflowGridEdgeFeature.Destroy;
begin
  FDisvSharedNodes.Free;
  inherited;
end;

function TCustomModflowGridEdgeFeature.EndingLocation: TPoint2D;
var
  Col: Integer;
  Row: Integer;
begin
  if (FModel as TCustomModel).DisvUsed then
  begin
    result := FDisvSharedNodes[1].Location;
  end
  else
  begin
    if Col1 = Col2 then
    begin
      Assert(Abs(Row1 - Row2) = 1);
      Row := Max(Row1, Row2);
      result := (FModel as TCustomModel).ModflowGrid.TwoDElementCorner(Col1 + 1, Row);
    end
    else
    begin
      Assert(Row1 = Row2);
      Assert(Abs(Col1 - Col2) = 1);
      Col := Max(Col1, Col2);
      result := (FModel as TCustomModel).ModflowGrid.TwoDElementCorner(Col, Row1 + 1);
    end;
  end;
end;

function TCustomModflowGridEdgeFeature.StartPoint: TPoint;
var
  Point: TPoint2D;
begin
  Point := StartingLocation;
  result.X := frmGoPhast.frameTopView.ZoomBox.XCoord(Point.x);
  result.Y := frmGoPhast.frameTopView.ZoomBox.YCoord(Point.y);
end;

procedure TCustomModflowGridEdgeFeature.UpdateDisvSharedNodes;
var
  LocalModel: TCustomModel;
  TwoDGrid: TModflowIrregularGrid2D;
  Cell1: TModflowIrregularCell2D;
  Cell2: TModflowIrregularCell2D;
  NodeIndex: Integer;
  ANode: TModflowNode;
begin
  LocalModel := FModel as TCustomModel;
  if LocalModel.DisvUsed then
  begin
    TwoDGrid := LocalModel.DisvGrid.TwoDGrid;
    Cell1 := TwoDGrid.Cells[Col1];
    Cell2 := TwoDGrid.Cells[Col2];
    FDisvSharedNodes.Clear;
    for NodeIndex := 0 to Cell1.NodeCount - 1 do
    begin
      ANode := Cell1.ElementCorners[NodeIndex];
      if Cell2.ElementCorners.IndexOf(ANode) >= 0 then
      begin
        FDisvSharedNodes.Add(ANode)
      end;
    end;
    Assert(FDisvSharedNodes.Count = 2);
  end;
end;

{ TEdgeDisplaySettings }

procedure TEdgeDisplaySettings.Assign(Source: TPersistent);
var
  SourceSettings: TEdgeDisplaySettings;
  SourceDisplay: TCustomModflowGridEdgeDisplay;
begin
  if Source = nil then
  begin
    Visible := False;
  end
  else if Source is TEdgeDisplaySettings then
  begin
    SourceSettings := TEdgeDisplaySettings(Source);
    Limits := SourceSettings.Limits;
    DataToPlot := SourceSettings.DataToPlot;
    Visible := SourceSettings.Visible;
  end
  else if Source is TCustomModflowGridEdgeDisplay then
  begin
    SourceDisplay := TCustomModflowGridEdgeDisplay(Source);
    Limits := SourceDisplay.Limits[SourceDisplay.DataToPlot];
    DataToPlot := SourceDisplay.DataToPlot;
    Visible := True;
  end
  else
  begin
    inherited;
  end;

end;

procedure TEdgeDisplaySettings.AssignTo(Dest: TPersistent);
var
  DestDisplay: TCustomModflowGridEdgeDisplay;
begin
  if Dest is TCustomModflowGridEdgeDisplay then
  begin
    DestDisplay := TCustomModflowGridEdgeDisplay(Dest);
    DestDisplay.DataToPlot := DataToPlot;
    DestDisplay.Limits[DataToPlot] := Limits;
  end
  else
  begin
    inherited;
  end;
end;

constructor TEdgeDisplaySettings.Create;
begin
  inherited;
  FLimits := TColoringLimits.Create;
end;

destructor TEdgeDisplaySettings.Destroy;
begin
  FLimits.Free;
  inherited;
end;

procedure TEdgeDisplaySettings.SetDataToPlot(const Value: integer);
begin
  FDataToPlot := Value;
end;

procedure TEdgeDisplaySettings.SetLimits(const Value: TColoringLimits);
begin
  FLimits.Assign(Value);
end;

procedure TEdgeDisplaySettings.SetVisible(const Value: boolean);
begin
  FVisible := Value;
end;

end.
