unit ModflowGwtAdvWriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowPackageSelectionUnit, GoPhastTypes,
  PhastModelUnit;

type
  TModflowGwtAdvWriter = class(TCustomPackageWriter)
  private
    FAdvPackage: TGwtAdvectionPackage;
    FModelType: TModelType;
    procedure WriteOptions;
    procedure SetModelType(const Value: TModelType);
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); override;
    property ModelType: TModelType read FModelType write SetModelType;
    procedure WriteFile(AFileName: string);
  end;

implementation

uses
  System.IOUtils;

{ TModflowGwtAdvWriter }

constructor TModflowGwtAdvWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FModelType := mtGroundWaterTransport;
end;

class function TModflowGwtAdvWriter.Extension: string;
begin
  result := '.adv';
end;

function TModflowGwtAdvWriter.Package: TModflowPackageSelection;
begin
  result := nil;
  case FModelType of
    mtGroundWaterTransport:
      begin
        result := Model.ModflowPackages.GwtAdvectionPackage;
      end;
    mtEnergyTransport:
      begin
        result := Model.ModflowPackages.GweAdvectionPackage;
      end;
    else
      Assert(False);
  end;
end;

procedure TModflowGwtAdvWriter.SetModelType(const Value: TModelType);
begin
  FModelType := Value;
end;

procedure TModflowGwtAdvWriter.WriteFile(AFileName: string);
var
  Abbreviation: string;
  GwtFile: string;
  SpeciesIndex: Integer;
  SpeciesGwtFile: string;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  case ModelType of
    mtGroundWaterFlow: Assert(False);
    mtGroundwaterTransport:
      begin
        if not Model.GwtUsed then
        begin
          Exit
        end;
      end;
    mtEnergyTransport:
      begin
        if not Model.GweUsed then
        begin
          Exit
        end;
      end;
    else
      Assert(False);
  end;
  FAdvPackage := Package as TGwtAdvectionPackage;

  Abbreviation := 'ADV6';
  GwtFile := GwtFileName(AFileName, 0);
  FNameOfFile := GwtFile;
  FInputFileName := GwtFile;

  WriteToGwtNameFile(Abbreviation, FNameOfFile, 0);

  FPestParamUsed := False;
  WritingTemplate := False;

  OpenFile(FNameOfFile);
  try
    WriteDataSet0;
    WriteOptions;
  finally
    CloseFile;
  end;

  if ModelType = mtGroundwaterTransport then
  begin
    for SpeciesIndex := 1 to Model.MobileComponents.Count - 1 do
    begin
      if Model.MobileComponents[SpeciesIndex].UsedForGwt then
      begin
        SpeciesGwtFile := GwtFileName(AFileName, SpeciesIndex);
        WriteToGwtNameFile(Abbreviation, SpeciesGwtFile, SpeciesIndex);
        TFile.Copy(GwtFile, SpeciesGwtFile, True);
      end;
    end;
  end;
end;

procedure TModflowGwtAdvWriter.WriteOptions;
begin
  WriteBeginOptions;

  WriteString('  SCHEME ');
  case FAdvPackage.Scheme of
    gsUpstream:
      begin
        WriteString('upstream');
      end;
    gsCentral:
      begin
        WriteString('central');
      end;
    gsTVD:
      begin
        WriteString('tvd');
      end;
    else
      Assert(False);
  end;
  NewLine;

  if FAdvPackage.AtsPercel <> 0 then
  begin
    WriteString('  ATS_PERCEL');
    WriteFloat(FAdvPackage.AtsPercel);
    NewLine;
  end;

  WriteEndOptions;
end;

end.
