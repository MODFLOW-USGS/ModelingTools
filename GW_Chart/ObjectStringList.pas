unit ObjectStringList;

interface

uses
  Classes;

type
  TObjectStringList = class(TStringList)
    procedure Clear; override;
    destructor Destroy; override;
  end;

implementation

{ TObjectStringList }

procedure TObjectStringList.Clear;
var
  Index: Integer;
begin
  for Index := 0 to Count-1 do
  begin
    Objects[Index].Free;
  end;
  inherited;

end;

destructor TObjectStringList.Destroy;
begin
  Clear;
  inherited;
end;

end.
