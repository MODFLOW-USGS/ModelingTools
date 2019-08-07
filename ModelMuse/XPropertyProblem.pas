unit XPropertyProblem;

interface

uses Classes;

type
  {@abstract(@name is used to specify the
   real-world coordinates of a pixel in a
   bitmap.)}
  TMeasurementPointItem = class(TCollectionItem)
  private
    // @name: integer;
    // See @link(PixelX).
    FPixelX: integer;
    // @name: integer;
    // See @link(PixelY).
    FPixelY: integer;
    // @name: double;
    // See @link(X).
    FX: double;
    // @name: double;
    // See @link(Y).
    FY: double;
    // no comment
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
  end;



implementation

{ TMeasurementPointItem }

procedure TMeasurementPointItem.Assign(Source: TPersistent);
begin
  if Source is TMeasurementPointItem then
  begin
    with TMeasurementPointItem(Source) do
    begin
      self.PixelX := PixelX;
      self.PixelY := PixelY;
      self.X := X;
      self.Y := Y;
    end;
  end
  else
  begin
    inherited
  end;
end;

procedure TMeasurementPointItem.InvalidateParentItem;
begin
{  if (Collection <> nil)
    and ((Collection as TMeasurementPointCollection).
    FCompressedBitmapItem <> nil) then
  begin
    TMeasurementPointCollection(Collection).
      FCompressedBitmapItem.Invalidate;
  end;  }
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

end.
 