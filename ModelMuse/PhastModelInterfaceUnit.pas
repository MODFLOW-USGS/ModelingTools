unit PhastModelInterfaceUnit;

interface

uses
  GoPhastTypes, OrderedCollectionInterfaceUnit;

var
  IGlobalModel: IModelMuseModel;
  IGlobalModelForOrderedCollection: ICustomModelInterfaceForTOrderedCollection;

procedure SetGlobals(Model: IModelMuseModel);

implementation


procedure SetGlobals(Model: IModelMuseModel);
begin
  IGlobalModel := Model;
  IGlobalModelForOrderedCollection := Model as ICustomModelInterfaceForTOrderedCollection;
end;

initialization

  IGlobalModelForOrderedCollection := nil;

end.
