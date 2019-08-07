{@name defines procedures that are used in helping to ensure
that the names of @link(TScreenObject)s are unique.}
unit CountObjectsUnit;

interface

uses SysUtils;

// @abstract(When a @link(TScreenObject) is first created,
// it is assigned a default name that is unique.
// Part of that default name depends on ObjectsCreated (declared in
// the implementation section).
// When reading an existing model, ObjectsCreated must be reset so that
// new @link(TScreenObject)s will still have unique names.)
procedure ResetScreenObjectCount;

// @name returns a number suitable for appending the default name
// for an object and creating an object with a new name.
Function ObjectsCreated: integer;

// @name increaments the stored number of objects by 1.
procedure IncrementObjectsCreated;

implementation

uses frmGoPhastUnit, ScreenObjectUnit;

var
  FObjectsCreated: integer = 0;

Function ObjectsCreated: integer;
begin
  result := FObjectsCreated;
end;

procedure IncrementObjectsCreated;
begin
  Inc(FObjectsCreated);
end;

procedure ResetScreenObjectCount;
var
  Index: integer;
  AScreenObject: TScreenObject;
  Name: string;
  Count: integer;
begin
  // When an TScreenObject is first created it is assigned a default name
  // that is unique.
  // Part of that default name depends on ObjectsCreated.
  // When reading an existing model, ObjectsCreated must be reset so that
  // new TScreenObject will still have unique names.
  FObjectsCreated := 0;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    Name := AScreenObject.Name;
    // If the name of the screen object starts with ObjectPrefix, it may be
    // using a default name.
    if Copy(Name, 1, Length(ObjectPrefix)) = ObjectPrefix then
    begin
      Name := Copy(Name, Length(ObjectPrefix) + 1, MAXINT);
      // If the TScreenObject is using a default name, the part after
      // ObjectPrefix will be an integer.
      if TryStrToInt(Name, Count) then
      begin
        // This is a default name. Check to see if ObjectsCreated needs
        // to be updated and, if so, update it.
        if Count >= ObjectsCreated then
        begin
          FObjectsCreated := Count + 1
        end;
      end;
    end;
  end;
end;

end.
