unit PestFeatureDisplayerUnit;

interface

uses Modflow6Importer, GoPhastTypes, frmImportShapefileUnit, System.Classes,
  System.SysUtils;

type
  TUndoImportPestModelFeatureDisplay = class(TUndoImportShapefile)
  protected
    // @name describes what @classname does.
    function Description: string; override;
  end;

  TPestFeatureDisplayer = class(TObject)
  private
    FModel: TBaseModel;
    FFeatures: TModflowFeatureList;
    FFeatureType: TModflow6FeatureType;
  public
    constructor Create(Model: TBaseModel);
    destructor Destroy; override;
    procedure ImportFeatures(const FileName: string; GridType: TModflow6GridType;
      StressPeriod: integer);
    property FeatureType: TModflow6FeatureType read FFeatureType;
  end;

implementation

uses
  ModflowBoundaryDisplayUnit, PhastModelUnit;


{ TPestFeatureDisplayer }

constructor TPestFeatureDisplayer.Create(Model: TBaseModel);
begin
  FModel := Model;
end;

destructor TPestFeatureDisplayer.Destroy;
begin
  FFeatures.Free;
  inherited;
end;

procedure TPestFeatureDisplayer.ImportFeatures(const FileName: string;
  GridType: TModflow6GridType; StressPeriod: integer);
var
  HeadDataArray: TModflowBoundaryDisplayDataArray;
  NewDataSets: TList;
  FeatureReader: TModflow6FileReader;
  InvalidNames: TStringList;
  Undo: TUndoImportPestModelFeatureDisplay;
  LocalModel: TCustomModel;
begin
  FeatureReader := TModflow6FileReader.Create(GridType);
  try
    FeatureReader.OpenFile(FileName);
    FFeatures := FeatureReader.ReadStressPeriod(StressPeriod);
    FFeatureType := FeatureReader.FeatureType;
  finally
    FeatureReader.Free;
  end;

  if FFeatures.Count = 0 then
  begin
    Exit;
  end;

  LocalModel := FModel as TCustomModel;
  InvalidNames:= TStringList.Create;
  NewDataSets := TList.Create;
  Undo := TUndoImportPestModelFeatureDisplay.Create;
  try
    case FFeatureType of
      m6ftChd:
        begin
          HeadDataArray := TModflowBoundaryDisplayDataArray.Create(LocalModel);
          HeadDataArray.Orientation := dso3D;
          HeadDataArray.EvaluatedAt := eaBlocks;
          HeadDataArray.UpdateDimensions(LocalModel.LayerCount,
            LocalModel.RowCount, LocalModel.ColumnCount);
          NewDataSets.Add(HeadDataArray);
          HeadDataArray.Name := GenerateNewName(Format('CHD_Head_SP_%d',
            [StressPeriod]), InvalidNames, '_');
        end;
      m6ftWell:
        begin

        end;
      else
        Assert(False);
    end;
  finally
    NewDataSets.Free;
    Undo.Free;
    InvalidNames.Free;
  end;

end;

{ TUndoImportPestModelFeatureDisplay }

function TUndoImportPestModelFeatureDisplay.Description: string;
begin
  result := 'import model feature data sets.';
end;

end.
