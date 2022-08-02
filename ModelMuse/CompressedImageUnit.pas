{@abstract(@name provides a bitmap that can be saved in compressed
 form as well as a TCollection to save them in and a method for determining
 where the image should be plotted.)}
unit CompressedImageUnit;

interface

uses System.UITypes, Windows, SysUtils, Classes, Graphics, GoPhastTypes, GR32, Dialogs;

type
  {@abstract(@name reads and writes bitmaps to and from streams using a
   TCompressionStream and a TDecompressionStream.)}
  TCompressedBitmap = class(TBitmap)
  protected
    // @name overrides inherited @name to read data from a compressed
    // stream using @link(LoadFromCompressedStream).
    procedure ReadData(Stream: TStream); override;
    // @name overrides inherited @name to write data to a compressed
    // stream using @link(SaveToCompressedStream).
    procedure WriteData(Stream: TStream); override;
  public
    // @name uses a TDecompressionStream internally to read a compressed
    // bitmap from Stream.
    procedure LoadFromCompressedStream(Stream: TStream);
    // @name uses a TCompressionStream internally to save a
    // bitmap in compressed form to Stream.
    procedure SaveToCompressedStream(Stream: TStream);
  end;

  {@abstract(@name is used to specify the
   real-world coordinates of a pixel in a
   bitmap.)}
  TMeasurementPointItem = class(TCollectionItem)
  private
    // See @link(PixelX).
    FPixelX: integer;
    // See @link(PixelY).
    FPixelY: integer;
    // See @link(X).
    FX: double;
    // See @link(Y).
    FY: double;
    FID: string;
    // @name invalidates the @link(TCompressedBitmapItem) that owns it.
    procedure InvalidateParentItem;
    // See @link(PixelX).
    procedure SetPixelX(const Value: integer);
    // See @link(PixelY).
    procedure SetPixelY(const Value: integer);
    // See @link(X).
    procedure SetX(const Value: double);
    // See @link(Y).
    procedure SetY(const Value: double);
  public
    // If Source is a @classname, @name copies the data of Source.
    procedure Assign(Source: TPersistent); override;
  published
    // @name is the X pixel coordinate.
    property PixelX: integer read FPixelX write SetPixelX;
    // @name is the Y pixel coordinate.
    property PixelY: integer read FPixelY write SetPixelY;
    // @name is the X real-world coordinate.
    property X: double read FX write SetX;
    // @name is the Y real-world coordinate.
    property Y: double read FY write SetY;
    property ID: string read FID write FID;
  end;

  TCompressedBitmapItem = class;
  TTCompressedBitmapItemProperties = class;

  // @abstract(@name stores a series of
  // @link(TMeasurementPointItem)s that determine
  // how a bitmap should be resized when it is drawn.)
  TMeasurementPointCollection = class(TCollection)
  private
    // @name has the bitmap to which the @link(TMeasurementPointItem)s
    // in @classname refer.
    FCompressedBitmapItem: TTCompressedBitmapItemProperties;
  public
    // @name creates an instance of @classname.  Item is the
    // @link(TCompressedBitmapItem) to which the @link(TMeasurementPointItem)s
    // in @classname refer.
    constructor Create(const Item: TTCompressedBitmapItemProperties);
  end;

  TTCompressedBitmapItemProperties = class(TCollectionItem)
  private
    // @name is the name of the bitmap.
    FName: string;
    // See @link(MeasurementPoints).
    FMeasurementPoints: TMeasurementPointCollection;
    // See @link(ViewDirection).
    FViewDirection: TViewDirection;
    // See @link(Visible).
    FVisible: boolean;
    // See @link(MeasurementPoints).
    procedure SetMeasurementPoints(const Value: TMeasurementPointCollection);
    // See @link(ViewDirection).
    procedure SetViewDirection(const Value: TViewDirection);
    // See @link(Visible).
    procedure SetVisible(const Value: boolean);
  public
    // @name copies the published properties from Source.
    procedure Assign(Source: TPersistent); override;
    // @name creates and initializes an instance of @classname.
    constructor Create(Collection: TCollection); override;
    // @name destroys the current instance of @classname.
    // Do not call Destroy directly.  Call Free instead.
    destructor Destroy; override;
    // Invalidate causes @link(X), @link(Y), @link(ScaleX),
    // and @link(ScaleY) to be recalculated the next time they are read.
    procedure Invalidate; virtual;
  published
    // @name is the name used to refer to the bitmap by the user.
    property Name: string read FName write FName;
    // @name is the collection of @link(TMeasurementPointItem)s that
    // determine how the pixels relate to real-world coordinates.
    property MeasurementPoints: TMeasurementPointCollection
      read FMeasurementPoints write SetMeasurementPoints;
    // @name determines whether the bitmap should be viewed on the top,
    // front, or side view of the model.
    property ViewDirection: TViewDirection read FViewDirection
      write SetViewDirection;
    // @name determines whether or not the bitmap is visible.
    property Visible: boolean read FVisible write SetVisible;
  end;

  // @abstract(@name holds a @link(TCompressedBitmap).)
  // @name is used to store bitmaps in GoPhast.
  TCompressedBitmapItem = class(TTCompressedBitmapItemProperties)
  private
    // See @link(Bitmap).
    FBitmap: TCompressedBitmap;
    // See @link(Calculate).
    FCalculatedValues: boolean;
    // See @link(ScaleX).
    FScaleX: double;
    // See @link(ScaleY).
    FScaleY: double;
    // See @link(X).
    FX: double;
    // See @link(Y).
    FY: double;
    FCanShow: boolean;
    FDisplayMessage: boolean;
    // @name checks whether @link(FCalculatedValues) is true.
    // If not, it calculates and stores @link(X), @link(Y), @link(ScaleX),
    // and @link(ScaleY) and sets @link(FCalculatedValues) to true.
    // Otherwise it uses the stored values.
    // It uses @link(MeasurementPoints) to make the calculation.
    //
    // @name is called whenever @link(X), @link(Y), @link(ScaleX),
    // or @link(ScaleY) is read.
    procedure Calculate;
    // See @link(ScaleX).
    function GetScaleX: double;
    // See @link(ScaleY).
    function GetScaleY: double;
    // See @link(X).
    function GetX: double;
    // See @link(Y).
    function GetY: double;
    // See @link(Bitmap).
    procedure SetBitmap(const Value: TCompressedBitmap);
    // @name returns the pixel X-coordinate corresponding to X.
    function XCoord(const X: double): double;
    // @name returns the pixel Y-coordinate corresponding to Y.
    function YCoord(const Y: double): double;
  public
    // @name copies the published properties from Source.
    procedure Assign(Source: TPersistent); override;
    // @name creates and initializes an instance of @classname.
    constructor Create(Collection: TCollection); override;
    // @name destroys the current instance of @classname.
    // Do not call Destroy directly.  Call Free instead.
    destructor Destroy; override;
    // Invalidate causes @link(X), @link(Y), @link(ScaleX),
    // and @link(ScaleY) to be recalculated the next time they are read.
    procedure Invalidate; override;
    // @name is the ratio of the real-world
    // scale to the pixel-scale in the
    // X direction.
    property ScaleX: double read GetScaleX;
    // @name is the ratio of the real-world
    // scale to the pixel-scale in the
    // Y direction.
    property ScaleY: double read GetScaleY;
    // @name is the position where the left edge of the bitmap should
    // be drawn on the appropriate
    // Image32 as determined by @link(ViewDirection).
    property X: double read GetX;
    // @name is the position where the top edge of the bitmap should
    // be drawn on the appropriate
    // Image32 as determined by @link(ViewDirection).
    property Y: double read GetY;
    property CanShow: boolean read FCanShow write FCanShow;
    property DisplayMessage: boolean read FDisplayMessage write FDisplayMessage;
    procedure DrawCompressedImage(Destination: TPersistent;
      DestWidth, DestHeight: integer);
  published
    // @name is the bitmap that will be drawn.
    property Bitmap: TCompressedBitmap read FBitmap write SetBitmap;
  end;

  // @abstract(@name is a collection of @link(TCompressedBitmapItem)s.)
  //
  TCompressedBitmapCollection = class(TCollection)
  public
    // @name creates an instance of @classname.
    constructor Create;
    // @name invalidates all or its @link(TCompressedBitmapItem)s.
    // whose @link(TCompressedBitmapItem.ViewDirection)s.
    // match AViewDirection.
    procedure InvalidateView(const AViewDirection: TViewDirection);
  end;

implementation

uses Math, ZLib, LinRegression, frmGoPhastUnit;

resourcestring
  StrTheSImageCanNot = 'The %s image can not be shown at this magnification ' +
  'and has been turned off. You can turn it back on later after decreasing t' +
  'he magnification.';

{ TCompressedBitmap }

procedure TCompressedBitmap.LoadFromCompressedStream(Stream: TStream);
const
  BufferSize = 65536;
var
  DecompressionStream: TStream;
  Dest: TStream;
  Count: Integer;
  Buffer: array[0..BufferSize - 1] of Byte;
begin
  Dest := TMemoryStream.Create;
  try
    DecompressionStream := TDecompressionStream.Create(Stream);
    try
      while True do
      begin
        Count := DecompressionStream.Read(Buffer, BufferSize);
        if Count <> 0 then
          Dest.WriteBuffer(Buffer, Count)
        else
          Break;
      end;
    finally
      DecompressionStream.Free;
    end;
    Dest.Position := 0;
    LoadFromStream(Dest);
  finally
    Dest.Free;
  end;
end;

procedure TCompressedBitmap.ReadData(Stream: TStream);
var
  Dummy: Integer;
begin
  //for VCL stream compatibility
  Stream.Read(Dummy, SizeOf(Dummy));
  LoadFromCompressedStream(Stream);
end;

procedure TCompressedBitmap.SaveToCompressedStream(Stream: TStream);
var
  Source: TStream;
  CompressionStream: TStream;
begin
  Source := TMemoryStream.Create;
  try
    SaveToStream(Source);
    CompressionStream := TCompressionStream.Create(clMax, Stream);
    try
      CompressionStream.CopyFrom(Source, 0);
    finally
      CompressionStream.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TCompressedBitmap.WriteData(Stream: TStream);
var
  StartPos, Size: Integer;
begin
  StartPos := Stream.Position;
  Stream.Write(StartPos, SizeOf(StartPos));
  SaveToCompressedStream(Stream);
  Size := Stream.Position - StartPos;
  Stream.Position := StartPos;
  Stream.Write(Size, SizeOf(Size));
  Stream.Position := StartPos + Size;
end;

{ TCompressedBitmapItem }

procedure TCompressedBitmapItem.Assign(Source: TPersistent);
var
  SourceItem: TCompressedBitmapItem;
begin
  if Source is TCompressedBitmapItem then
  begin
    SourceItem := TCompressedBitmapItem(Source);
    begin
      self.Bitmap := SourceItem.Bitmap;
    end;
  end;
  inherited;
end;

procedure TCompressedBitmapItem.Calculate;
var
  OuterIndex: integer;
  Angle1, Angle2: double;
  XIntArray: array of Double;
  YIntArray: array of Double;
  XArray: array of Double;
  YArray: array of Double;
  LinResult: TStLinEst;
  XInt, YInt, Distance: double;
  MeasurementPoint: TMeasurementPointItem;
  XCenter, YCenter: integer;
begin
  if FCalculatedValues then
    Exit;
  if MeasurementPoints.Count < 2 then
  begin
    FScaleX := 1;
    FScaleY := 1;
    FX := 0;
    FY := 0;
  end
  else
  begin

    Angle1 := 0;

    // Use linear regression to determine how the bitmap should be
    // resized.
    XCenter := 0;
    YCenter := 0;

    SetLength(XIntArray, MeasurementPoints.Count);
    SetLength(YIntArray, MeasurementPoints.Count);
    SetLength(XArray, MeasurementPoints.Count);
    SetLength(YArray, MeasurementPoints.Count);
    for OuterIndex := 0 to MeasurementPoints.Count - 1 do
    begin
      MeasurementPoint := MeasurementPoints.Items[OuterIndex] as
        TMeasurementPointItem;
      XInt := MeasurementPoint.PixelX - XCenter;
      YInt := MeasurementPoint.PixelY - YCenter;
      Distance := Sqrt(Sqr(XInt) + Sqr(YInt));
      Angle2 := ArcTan2(YInt, XInt);
      XIntArray[OuterIndex] := Cos(Angle2 + Angle1) * Distance;
      YIntArray[OuterIndex] := Sin(Angle2 + Angle1) * Distance;
      XArray[OuterIndex] := XCoord(MeasurementPoint.X);
      YArray[OuterIndex] := YCoord(MeasurementPoint.Y);
    end;

    try
      LinEst(XArray, XIntArray, LinResult, False);
      FScaleX := LinResult.B1;
      FX := LinResult.B0;

      LinEst(YArray, YIntArray, LinResult, False);
      FScaleY := LinResult.B1;
      FY := LinResult.B0;
    except on EStStatError do
      begin
        FScaleX := 0;
        FX := 0;
        FScaleY := 0;
        FY := 0;
      end
    end;
  end;
  FCalculatedValues := True;
end;

constructor TCompressedBitmapItem.Create(Collection: TCollection);
begin
  FBitmap := TCompressedBitmap.Create;
  inherited;
  FCanShow := True;
  FDisplayMessage := True;
  if Collection <> nil then
  begin
    if Collection.Count = 1 then
    begin
      frmGoPhast.miShowHideBitmaps.Caption := 'Show or Hide Image';
    end
    else
    begin
      frmGoPhast.miShowHideBitmaps.Caption := 'Show or Hide Images...';
    end;
  end;
end;

destructor TCompressedBitmapItem.Destroy;
begin
  FBitmap.Free;
//  FMeasurementPoints.Free;
  inherited;
end;

procedure TCompressedBitmapItem.DrawCompressedImage(Destination: TPersistent;
  DestWidth, DestHeight: integer);
var
//  X, Y: double;
  NewBmp: TBitmap;
  XMult, YMult: double;
  NewWidth, NewHeight: Int64;
  NewBitMap32: TBitmap32;
  DestTopLeft: TPoint;
  DestBottomRight: TPoint;
  SourceTopLeft: TPoint;
  SourceBottomRight: TPoint;
//  DestWidth: integer;
//  ClipWidth: Integer;
//  ClipHeight: Integer;
  NewBmp2: TBitmap;
  procedure ShowErrorMessage;
  begin
    Beep;
    MessageDlg(Format(StrTheSImageCanNot, [Name]), mtInformation, [mbOK], 0);
  end;
begin
    // @name will draw an imported image () on a bitmap (Dest)
    // at its proper location.
//  Assert(( <> nil) {and (Dest <> nil)});

//  X := .X;
//  Y := .Y;
  XMult := ScaleX;
  YMult := ScaleY;

  NewBmp := TBitmap.Create;
  try
    NewWidth := Round(BitMap.Width * XMult);
    NewHeight := Round(BitMap.Height * YMult);

    if (NewWidth < 2) or (NewHeight < 2) then
      Exit;

    DestTopLeft.X := Max(0, Round(X));
    DestTopLeft.Y := Max(0, Round(Y));
    DestBottomRight.X := Min(DestTopLeft.X + DestWidth, Round(X)+NewWidth);
    DestBottomRight.Y := Min(DestTopLeft.Y + DestHeight, Round(Y)+NewHeight);

    if DestTopLeft.X > DestWidth then
    begin
      Exit;
    end;
    if DestTopLeft.Y > DestHeight then
    begin
      Exit;
    end;
    if DestBottomRight.X < 0 then
    begin
      Exit;
    end;
    if DestBottomRight.Y < 0 then
    begin
      Exit;
    end;

    try
//      ClipWidth := Min(DestWidth, DestBottomRight.X) - DestTopLeft.X;
//      ClipHeight := Min(DestHeight, DestBottomRight.Y) - DestTopLeft.Y;

      if Round(X) < 0 then
      begin
        SourceTopLeft.X := Round(-X/NewWidth*BitMap.Width);
      end
      else
      begin
        SourceTopLeft.X := 0;
      end;
//      if XMult < 1 then
//      begin
//        SourceBottomRight.X := SourceTopLeft.X + Max(ClipWidth, BitMap.Width);
//      end
//      else
//      begin
//        SourceBottomRight.X := SourceTopLeft.X + Min(ClipWidth, BitMap.Width);
//      end;
//        SourceBottomRight.X := SourceTopLeft.X + ClipWidth;
        SourceBottomRight.X := SourceTopLeft.X + BitMap.Width;

      if Round(Y) < 0 then
      begin
        SourceTopLeft.Y := Round(-Y/NewHeight*BitMap.Height);
      end
      else
      begin
        SourceTopLeft.Y := 0;
      end;
//      if YMult < 1 then
//      begin
//        SourceBottomRight.Y := SourceTopLeft.Y + Max(ClipHeight, BitMap.Height);
//      end
//      else
//      begin
//        SourceBottomRight.Y := SourceTopLeft.Y + Min(ClipHeight, BitMap.Height);
//      end;
//        SourceBottomRight.Y := SourceTopLeft.Y + ClipHeight;
        SourceBottomRight.Y := SourceTopLeft.Y + BitMap.Height;

      NewBmp.Width := SourceBottomRight.X - SourceTopLeft.X;
      NewBmp.Height := SourceBottomRight.Y - SourceTopLeft.Y;

      NewBmp.Canvas.CopyMode := cmSrcCopy;
      NewBmp.Canvas.CopyRect(Rect(0,0,NewBmp.Width, NewBmp.Height), Bitmap.Canvas,
        Rect(SourceTopLeft, SourceBottomRight));

      NewBmp2 := TBitmap.Create;
      try
        NewBmp2.Width := DestBottomRight.X - DestTopLeft.X;
        NewBmp2.Height := DestBottomRight.Y - DestTopLeft.Y;

        while (NewWidth >= High(SmallInt)) or (NewHeight >= High(SmallInt)) do
        begin
          NewBmp.SetSize(NewBmp.Width div 2, NewBmp.Height div 2);
          NewWidth := NewWidth div 2;
          NewHeight := NewHeight div 2;
        end;


        NewBmp2.Canvas.StretchDraw(Rect(0, 0, NewWidth, NewHeight),
          NewBmp);

        if Destination is TBitmap32 then
        begin
          NewBitMap32 := TBitmap32.Create;
          try
            NewBitMap32.Assign(NewBmp2);
            { TODO : rotate bitmap? }
            TBitmap32(Destination).Draw(DestTopLeft.X, DestTopLeft.Y, NewBitMap32);
          finally
            NewBitMap32.Free;
          end;
        end
        else
        begin
          (Destination as TCanvas).Draw(DestTopLeft.X, DestTopLeft.Y, NewBmp2);
        end;
      finally
        NewBmp2.Free;
      end;


    except on E: Exception do
      begin
        Visible := False;
        ShowErrorMessage;
      end;
    end;

{
    if (NewWidth > 5000) or (NewHeight > 5000) then
    begin
      Visible := False;
      ShowErrorMessage;
    end
    else
    begin
      try
        NewBmp.Width := NewWidth;
        NewBmp.Height := NewHeight;
        NewBmp.Canvas.StretchDraw(Rect(0, 0, NewWidth, NewHeight),
          BitMap);

        if Destination is TBitmap32 then
        begin
          NewBitMap32 := TBitmap32.Create;
          try
            NewBitMap32.Assign(NewBmp);
            // TODO : rotate bitmap?
            TBitmap32(Destination).Draw(Round(X), Round(Y), NewBitMap32);
          finally
            NewBitMap32.Free;
          end;
        end
        else
        begin
          (Destination as TCanvas).Draw(Round(X), Round(Y), NewBmp);
        end;
      Except
        Visible := False;
        ShowErrorMessage;
      end;
    end;
  }
  finally
    NewBmp.Free;
  end;
  DisplayMessage := True;
//  FPreviousMagnification := ZoomBox.Magnification;
end;

function TCompressedBitmapItem.GetScaleX: double;
begin
  Calculate;
  Result := FScaleX;
end;

function TCompressedBitmapItem.GetScaleY: double;
begin
  Calculate;
  Result := FScaleY;
end;

function TCompressedBitmapItem.GetX: double;
begin
  Calculate;
  Result := FX;
end;

function TCompressedBitmapItem.GetY: double;
begin
  Calculate;
  Result := FY;
end;

procedure TCompressedBitmapItem.Invalidate;
begin
  FCalculatedValues := False;
end;

procedure TCompressedBitmapItem.SetBitmap(const Value: TCompressedBitmap);
begin
  FBitmap.Assign(Value);
end;

function TCompressedBitmapItem.XCoord(const X: double): double;
begin
  result := 0;
  case ViewDirection of
    vdTop:
      begin
        result := frmGoPhast.frameTopView.ZoomBox.XCoord(X);
      end;
    vdFront:
      begin
        result := frmGoPhast.frameFrontView.ZoomBox.XCoord(X);
      end;
    vdSide:
      begin
        result := frmGoPhast.frameSideView.ZoomBox.XCoord(X);
      end;
  else
    Assert(False);
  end;
end;

function TCompressedBitmapItem.YCoord(const Y: double): double;
begin
  result := 0;
  case ViewDirection of
    vdTop:
      begin
        result := frmGoPhast.frameTopView.ZoomBox.YCoord(Y);
      end;
    vdFront:
      begin
        result := frmGoPhast.frameFrontView.ZoomBox.YCoord(Y);
      end;
    vdSide:
      begin
        result := frmGoPhast.frameSideView.ZoomBox.YCoord(Y);
      end;
  else
    Assert(False);
  end;
end;

{ TCompressedBitmapCollection }

constructor TCompressedBitmapCollection.Create;
begin
  inherited Create(TCompressedBitmapItem);
end;

procedure TCompressedBitmapCollection.InvalidateView(
  const AViewDirection: TViewDirection);
var
  Index: integer;
  Item: TCompressedBitmapItem;
begin
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TCompressedBitmapItem;
    if Item.ViewDirection = AViewDirection then
    begin
      Item.Invalidate;
    end;
  end;
end;

{ TMeasurementPointItem }

procedure TMeasurementPointItem.Assign(Source: TPersistent);
var
  SourceItem: TMeasurementPointItem;
begin
  if Source is TMeasurementPointItem then
  begin
    SourceItem := TMeasurementPointItem(Source);
//    with TMeasurementPointItem(Source) do
    begin
      self.PixelX := SourceItem.PixelX;
      self.PixelY := SourceItem.PixelY;
      self.X := SourceItem.X;
      self.Y := SourceItem.Y;
      self.ID := SourceItem.ID;
    end;
  end
  else
  begin
    inherited
  end;
end;

procedure TMeasurementPointItem.InvalidateParentItem;
begin
  if (Collection <> nil)
    and ((Collection as TMeasurementPointCollection).
    FCompressedBitmapItem <> nil) then
  begin
    TMeasurementPointCollection(Collection).
      FCompressedBitmapItem.Invalidate;
  end;
end;

procedure TMeasurementPointItem.SetPixelX(const Value: integer);
begin
  if FPixelX <> Value then
  begin
    FPixelX := Value;
    InvalidateParentItem
  end;
end;

procedure TMeasurementPointItem.SetPixelY(const Value: integer);
begin
  if FPixelY <> Value then
  begin
    FPixelY := Value;
    InvalidateParentItem
  end;
end;

procedure TMeasurementPointItem.SetX(const Value: double);
begin
  if FX <> Value then
  begin
    FX := Value;
    InvalidateParentItem
  end;
end;

procedure TMeasurementPointItem.SetY(const Value: double);
begin
  if FY <> Value then
  begin
    FY := Value;
    InvalidateParentItem
  end;
end;

{ TMeasurementPointCollection }

constructor TMeasurementPointCollection.Create(const Item:
  TTCompressedBitmapItemProperties);
begin
  inherited Create(TMeasurementPointItem);
  FCompressedBitmapItem := Item;
end;

procedure TTCompressedBitmapItemProperties.Assign(Source: TPersistent);
var
  SourceItem: TTCompressedBitmapItemProperties;
begin
  if Source is TTCompressedBitmapItemProperties then
  begin
    SourceItem := TTCompressedBitmapItemProperties(Source);
//    with TTCompressedBitmapItemProperties(Source) do
    begin
//      self.Bitmap := SourceItem.Bitmap;
      self.Name := SourceItem.Name;
      self.ViewDirection := SourceItem.ViewDirection;
      self.MeasurementPoints := SourceItem.MeasurementPoints;
      self.Visible := SourceItem.Visible;
    end;
  end
  else
  begin
    inherited;
  end;
end;

constructor TTCompressedBitmapItemProperties.Create(Collection: TCollection);
begin
//  FCanShow := True;
//  FDisplayMessage := True;
  FVisible := True;
  if Collection = nil then
  begin
    Name := 'Image';
  end
  else
  begin
    Name := 'Image' + IntToStr(Collection.Count);
  end;
  FMeasurementPoints := TMeasurementPointCollection.Create(self);
  inherited;
end;

destructor TTCompressedBitmapItemProperties.Destroy;
begin
  FMeasurementPoints.Free;
  inherited;
end;

procedure TTCompressedBitmapItemProperties.Invalidate;
begin
  // do nothing.
end;

procedure TTCompressedBitmapItemProperties.SetMeasurementPoints
  (const Value: TMeasurementPointCollection);
begin
  FMeasurementPoints.Assign(Value);
end;

procedure TTCompressedBitmapItemProperties.SetViewDirection
  (const Value: TViewDirection);
begin
  FViewDirection := Value;
end;

procedure TTCompressedBitmapItemProperties.SetVisible(const Value: boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    case ViewDirection of
      vdTop:
        begin
          frmGoPhast.frameTopView.ScreenObjectsHaveChanged := True;
        end;
      vdFront:
        begin
          frmGoPhast.frameFrontView.ScreenObjectsHaveChanged := True;
        end;
      vdSide:
        begin
          frmGoPhast.frameSideView.ScreenObjectsHaveChanged := True;
        end;
    else
      Assert(False);
    end;
  end;
end;

initialization

RegisterClass(TCompressedBitmap);

end.
