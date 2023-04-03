unit ScreenObjectOwnerCollectionUnit;

interface

uses
  Classes, GoPhastTypes;

type
  TScreenObjectOwnerCollection = class(TPhastCollection)
  private
    FScreenObject: TObject;
  public
    constructor Create(ItemClass: TCollectionItemClass;
      InvalidateModelEvent: TNotifyEvent; ScreenObject: TObject);
    property ScreenObject: TObject read FScreenObject;
  end;

implementation

uses
  ScreenObjectUnit;

{ TScreenObjectOwnerCollection }

constructor TScreenObjectOwnerCollection.Create(ItemClass: TCollectionItemClass;
  InvalidateModelEvent: TNotifyEvent; ScreenObject: TObject);
begin
  inherited Create(ItemClass, InvalidateModelEvent);
  if ScreenObject <> nil then
  begin
    Assert(ScreenObject is TScreenObject);
  end;
  FScreenObject := ScreenObject;
end;

end.
