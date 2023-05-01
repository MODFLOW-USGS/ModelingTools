unit PhastModelInterfaceUnit;

interface

uses
  GoPhastTypes, OrderedCollectionInterfaceUnit;

var
  IGlobalModel: IModelMuseModel;
  IGlobalModelForOrderedCollection: IModelForTOrderedCollection;

procedure SetGlobals(Model: IModelMuseModel);

implementation


procedure SetGlobals(Model: IModelMuseModel);
begin
  IGlobalModel := Model;
  IGlobalModelForOrderedCollection := Model as IModelForTOrderedCollection;
end;

initialization

  IGlobalModel := nil;
  IGlobalModelForOrderedCollection := nil;

end.
