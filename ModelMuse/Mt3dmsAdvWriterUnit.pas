unit Mt3dmsAdvWriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowPackageSelectionUnit, Forms, PhastModelUnit;

type
  TMt3dmsAdvWriter = class(TCustomModflowWriter)
  private
    MIXELM: integer;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
    procedure WriteDataSet4;
    procedure WriteDataSet5;
  protected
    class function Extension: string; override;
  public
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); override;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  ModflowUnitNumbers, frmProgressUnit, SysUtils, GoPhastTypes;

resourcestring
  rsAdvID = '%s in the MT3DMS ADV or MT3D-USGS package';
  StrWritingMT3DMSADVP = 'Writing MT3DMS or MT3D-USGS ADV Package input.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
//  StrWritingDataSet2 = '  Writing Data Set 2.';
//  StrWritingDataSet3 = '  Writing Data Set 3.';
//  StrWritingDataSet4 = '  Writing Data Set 4.';
//  StrWritingDataSet5 = '  Writing Data Set 5.';

{ TMt3dmsAdvWriter }

constructor TMt3dmsAdvWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FArrayWritingFormat := awfMt3dms;
end;

class function TMt3dmsAdvWriter.Extension: string;
begin
  result := '.adv';
end;

procedure TMt3dmsAdvWriter.WriteDataSet1;
var
  PERCEL: double;
  MXPART: integer;
  NADVFD: integer;
  AdvPkg: TMt3dmsAdvection;
begin
  AdvPkg := Model.ModflowPackages.Mt3dmsAdvection;
  // MIXELM for TVE should be -1. For other choices it should be from 0 to 3.
  MIXELM := Ord(AdvPkg.AdvectionSolution) -1;
  PERCEL := AdvPkg.Courant;
  MXPART := AdvPkg.MaximumParticles;
  NADVFD := Ord(AdvPkg.WeightingScheme)+1;
  WriteI10Integer(MIXELM, Format(rsAdvID, ['MIXELM']));
  WriteF10Float(PERCEL);
  WriteI10Integer(MXPART, Format(rsAdvID, ['MXPART']));
  WriteI10Integer(NADVFD, Format(rsAdvID, ['NADVFD']));
  WriteString(' # Data Set 1: MIXELM PERCEL MXPART NADVFD');
  NewLine;
end;

procedure TMt3dmsAdvWriter.WriteDataSet2;
var
  AdvPkg: TMt3dmsAdvection;
  ITRACK: integer;
  WD: double;
begin
  if MIXELM in [1,2,3] then
  begin
    AdvPkg := Model.ModflowPackages.Mt3dmsAdvection;
    ITRACK := Ord(AdvPkg.ParticleTrackMethod)+1;
    WD := AdvPkg.ConcWeight;
    WriteI10Integer(ITRACK, Format(rsAdvID, ['ITRACK']));
    WriteF10Float(WD);
    WriteString(' # Data Set 2: ITRACK WD');
    NewLine;
  end;
end;

procedure TMt3dmsAdvWriter.WriteDataSet3;
var
  AdvPkg: TMt3dmsAdvection;
  DCEPS: double;
  NPLANE, NPL, NPH, NPMIN, NPMAX: integer;
begin
  if MIXELM in [1,3] then
  begin
    AdvPkg := Model.ModflowPackages.Mt3dmsAdvection;
    DCEPS := AdvPkg.RelCelConcGrad;
    NPLANE := Ord(AdvPkg.ParticlePlacementMethod)
      *AdvPkg.NumberOfParticlePlanes;
    NPL := AdvPkg.LowGradientParticleCount;
    NPH := AdvPkg.HighGradientParticleCount;
    NPMIN := AdvPkg.MinParticlePerCell;
    NPMAX := AdvPkg.MaxParticlesPerCell;
    WriteF10Float(DCEPS);
    WriteI10Integer(NPLANE, Format(rsAdvID, ['NPLANE']));
    WriteI10Integer(NPL, Format(rsAdvID, ['NPL']));
    WriteI10Integer(NPH, Format(rsAdvID, ['NPH']));
    WriteI10Integer(NPMIN, Format(rsAdvID, ['NPMIN']));
    WriteI10Integer(NPMAX, Format(rsAdvID, ['NPMAX']));
    WriteString(' # Data Set 3: DCEPS NPLANE NPL NPH NPMIN NPMAX');
    NewLine;
  end;
end;

procedure TMt3dmsAdvWriter.WriteDataSet4;
var
  AdvPkg: TMt3dmsAdvection;
  INTERP, NLSINK, NPSINK: integer;
begin
  if MIXELM in [2,3] then
  begin
    AdvPkg := Model.ModflowPackages.Mt3dmsAdvection;
    INTERP := 1;
    NLSINK := Ord(AdvPkg.SinkParticlePlacementMethod)
      * AdvPkg.SinkNumberOfParticlePlanes;
    NPSINK := AdvPkg.SinkParticleCount;
    WriteI10Integer(INTERP, Format(rsAdvID, ['INTERP']));
    WriteI10Integer(NLSINK, Format(rsAdvID, ['NLSINK']));
    WriteI10Integer(NPSINK, Format(rsAdvID, ['NPSINK']));
    WriteString(' # Data Set 4: INTERP NLSINK NPSINK');
    NewLine;
  end;
end;

procedure TMt3dmsAdvWriter.WriteDataSet5;
var
  AdvPkg: TMt3dmsAdvection;
  DCHMOC: double;
begin
  if MIXELM = 3 then
  begin
    AdvPkg := Model.ModflowPackages.Mt3dmsAdvection;
    DCHMOC := AdvPkg.CriticalConcGradient;
    WriteF10Float(DCHMOC);
    WriteString(' # Data Set 5: DCHMOC');
    NewLine;
  end;
end;

procedure TMt3dmsAdvWriter.WriteFile(const AFileName: string);
//var
//  NameOfFile: string;
begin
  if not Model.ModflowPackages.Mt3dmsAdvection.IsSelected then
  begin
    Exit;
  end;
  // remove errors and warnings
//  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrFileForTheInitial);

  FNameOfFile := FileName(AFileName);
  // PackageGeneratedExternally needs to be updated for MT3DMS
  if Model.PackageGeneratedExternally(StrADV) then
  begin
    Exit;
  end;

  // write to MT3DMS or MT3D-USGS name file.
  WriteToMt3dMsNameFile(StrADV, Mt3dAdv,
    FNameOfFile, foInput, Model);

  FInputFileName := FNameOfFile;
  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingMT3DMSADVP);

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

    frmProgressMM.AddMessage(StrWritingDataSet5);
    WriteDataSet5;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

  finally
    CloseFile;
  end;

end;

end.
