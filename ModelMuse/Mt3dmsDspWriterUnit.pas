unit Mt3dmsDspWriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowPackageSelectionUnit, Forms, PhastModelUnit,
  GoPhastTypes;

type
  TMt3dmsDspWriter = class(TCustomPackageWriter)
  private
    procedure WriteDataSet0;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
    procedure WriteDataSet4;
    procedure Export1DRealArray(TRPT: TOneDRealArray);
    procedure WriteOptions;
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); override;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  frmProgressUnit, ModflowUnitNumbers, DataSetUnit, SysUtils,
  Mt3dmsChemSpeciesUnit;

resourcestring
  StrALLayerD = 'Data Set 1: AL, Layer: %d';
  StrDMCOEFLayerD = 'Data Set 4: DMCOEF; Species: %0:s; Layer: %1:d';
  StrWritingMT3DMSDSPP = 'Writing MT3DMS or MT3D-USGS DSP Package input.';
//  StrWritingDataSet0 = '  Writing Data Set 0.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
//  StrWritingDataSet2 = '  Writing Data Set 2.';
//  StrWritingDataSet3 = '  Writing Data Set 3.';
//  StrWritingDataSet4 = '  Writing Data Set 4.';

{ TMt3dmsDspWriter }

constructor TMt3dmsDspWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FArrayWritingFormat := awfMt3dms;
end;

class function TMt3dmsDspWriter.Extension: string;
begin
  Result := '.dsp';
end;

function TMt3dmsDspWriter.Package: TModflowPackageSelection;
begin
  Result := Model.ModflowPackages.Mt3dmsDispersion;
end;

procedure TMt3dmsDspWriter.WriteDataSet0;
begin
  if Model.ModflowPackages.Mt3dBasic.Mt3dVersion = mvUSGS then
  begin
    inherited;
  end;
  WriteOptions;
end;

procedure TMt3dmsDspWriter.WriteDataSet1;
var
  LayerIndex: Integer;
  DataArray: TDataArray;
begin
  DataArray := Model.DataArrayManager.GetDataSetByName(rsLong_Dispersivity);
  DataArray.Initialize;
  for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
  begin
    if Model.IsLayerSimulated(LayerIndex) then
    begin
      WriteArray(DataArray, LayerIndex, Format(StrALLayerD,
        [Model.DataSetLayerToModflowLayer(LayerIndex)]), StrNoValueAssigned, 'AL');
    end;
  end;
end;

procedure TMt3dmsDspWriter.WriteDataSet2;
begin
  WriteU2DRELHeader(' # Data Set 2: TRPT', matStructured, 'TRPT');
  Export1DRealArray(Model.TRPT);
end;

procedure TMt3dmsDspWriter.WriteDataSet3;
begin
  WriteU2DRELHeader(' # Data Set 3: TRPV', matStructured, 'TRPV');
  Export1DRealArray(Model.TRPV);
end;

procedure TMt3dmsDspWriter.WriteDataSet4;
var
  DispPkg: TMt3dmsDispersion;
  SpeciesIndex: Integer;
  ChemItem: TMobileChemSpeciesItem;
  DataArray: TDataArray;
  LayerIndex: Integer;
begin
  DispPkg := Model.ModflowPackages.Mt3dmsDispersion;
  if DispPkg.MultiDifussion then
  begin
    for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
    begin
      ChemItem := Model.MobileComponents[SpeciesIndex];
      DataArray := Model.DataArrayManager.GetDataSetByName(
        ChemItem.DiffusionCoefDataArrayName);
      DataArray.Initialize;
      for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
      begin
        if Model.IsLayerSimulated(LayerIndex) then
        begin
          WriteArray(DataArray, LayerIndex, Format(StrDMCOEFLayerD,
            [ChemItem.Name, Model.DataSetLayerToModflowLayer(LayerIndex)]), StrNoValueAssigned, 'DMCOEF');
        end;
      end;
    end;
  end
  else
  begin
    WriteU2DRELHeader(' # Data Set 4: DMCOEF', matStructured, 'DMCOEF');
    Export1DRealArray(Model.DMCOEF);
  end;
end;

procedure TMt3dmsDspWriter.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  if not Model.ModflowPackages.Mt3dmsDispersion.IsSelected then
  begin
    Exit;
  end;
  // remove errors and warnings
//  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrFileForTheInitial);

  if Model.PackageGeneratedExternally(StrDSP) then
  begin
    Exit;
  end;

  NameOfFile := FileName(AFileName);

  // write to MT3DMS or MT3D-USGS name file.
  WriteToMt3dMsNameFile(StrDSP, Mt3dDsp,
    NameOfFile, foInput, Model);
//  WriteToNameFile(StrBAS, Model.UnitNumbers.UnitNumber(StrBAS),
//    NameOfFile, foInput);

  // PackageGeneratedExternally needs to be updated for MT3DMS
  FInputFileName := NameOfFile;
  OpenFile(NameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingMT3DMSDSPP);

    frmProgressMM.AddMessage(StrWritingDataSet0);
    WriteDataSet0;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet1);
    WriteDataSet1;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet2);
    WriteDataSet2;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet3);
    WriteDataSet3;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet4);
    WriteDataSet4;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

  finally
    CloseFile;
  end;
end;

procedure TMt3dmsDspWriter.WriteOptions;
var
  DispPkg: TMt3dmsDispersion;
  NewLineNeeded: Boolean;
begin
  NewLineNeeded := False;
  DispPkg := Model.ModflowPackages.Mt3dmsDispersion;
  if DispPkg.MultiDifussion then
  begin
    WriteString('$ MULTIDIFFUSION');
    NewLineNeeded := True;
  end;
  if (not DispPkg.CrossTermsUsed)
    and (Model.ModflowPackages.Mt3dBasic.Mt3dVersion = mvUSGS) then
  begin
    if not NewLineNeeded then
    begin
      WriteString('$');
    end;
    WriteString(' NOCROSS');
    NewLineNeeded := True;
  end;
  if NewLineNeeded then
  begin
    NewLine;
  end;
end;

procedure TMt3dmsDspWriter.Export1DRealArray(TRPT: TOneDRealArray);
var
  Index: Integer;
  NewLineNeeded: Boolean;
begin
  NewLineNeeded := True;
  for Index := 0 to Length(TRPT) - 1 do
  begin
    WriteFloat(TRPT[Index]);
    if ((Index + 1) mod 10) = 0 then
    begin
      NewLine;
      NewLineNeeded := False;
    end
    else
    begin
      NewLineNeeded := True;
    end;
  end;
  if NewLineNeeded then
  begin
    NewLine;
  end;
end;

end.
