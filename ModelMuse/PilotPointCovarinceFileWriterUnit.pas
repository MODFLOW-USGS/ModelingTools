unit PilotPointCovarinceFileWriterUnit;

interface

uses
  System.Classes, PilotPointDataUnit;

type
  TPilotPointCovarinceFileWriter = class(TObject)
  public
    procedure WriteConvarianceFiles(PilotPointFileObject: TPilotPointFileObject);
  end;

const
  StrCov = '.Cov.txt';

implementation

uses
  frmGoPhastUnit, PestPropertiesUnit, System.SysUtils, PhastModelUnit,
  ModelMuseUtilities, System.IOUtils, frmErrorsAndWarningsUnit;

resourcestring
  StrRequiredPESTUtilit = 'Required PEST Utility Program missing';
  Str0sIsMissingFrom = '%0:s is missing from %1:s.';
  StrRemAfterThePrevio = 'rem After the previous line executes, sppcov_sva s' +
  'hould show that a covariance file was written.';
  StrRemCreateCovarianc = 'rem Create Covariance file.';
  StrRemCreateVariogram = 'rem Create Variogram file.';

{ TPilotPointCovarinceFileWriter }

procedure TPilotPointCovarinceFileWriter.WriteConvarianceFiles(
  PilotPointFileObject: TPilotPointFileObject);
var
  Lines: TStringList;
  PestProp: TPestProperties;
  VariogramFileName: string;
  MkppstatInputFileName: string;
  CovarianceFileName: string;
  Ppcov_svaFileName: string;
  Locations: TProgramLocations;
  PestDirectory: string;
  BatchFileName: string;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(frmGoPhast.PhastModel,
    StrRequiredPESTUtilit);
  PestProp := frmGoPhast.PhastModel.PestProperties;
  Locations := frmGoPhast.PhastModel.ProgramLocations;
  Lines := TStringList.Create;
  try
    // MKPPSTAT Input
    Lines.Add(ExtractFileName(PilotPointFileObject.FileName));
    Lines.Add(IntToStr(PestProp.MaxPilotPointsInRange));
    Lines.Add('1');
    VariogramFileName := ChangeFileExt(PilotPointFileObject.FileName,
      '.Variogram.txt');
    Lines.Add(ExtractFileName(VariogramFileName));
    MkppstatInputFileName := ChangeFileExt(PilotPointFileObject.FileName,
      '.Mkppstat_Input.txt');
    Lines.WriteBOM := False;
    Lines.SaveToFile(MkppstatInputFileName);

    // PPCOV-SVA input
    Lines.Clear;
    Lines.Add(ExtractFileName(VariogramFileName));
    Lines.Add('y');
    Lines.Add('0.0');
    Lines.Add('g');
    CovarianceFileName := ChangeFileExt(PilotPointFileObject.FileName, StrCov);
    Lines.Add(ExtractFileName(CovarianceFileName));
    Lines.Add('');
    Ppcov_svaFileName := ChangeFileExt(PilotPointFileObject.FileName,
      '.Ppcov_SVA_Input.txt');
    Lines.WriteBOM := False;
    Lines.SaveToFile(Ppcov_svaFileName);

    PestDirectory := IncludeTrailingPathDelimiter(Locations.PestDirectory);
    if not TFile.Exists(PestDirectory + 'mkppstat.exe') then
    begin
      frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel,
        StrRequiredPESTUtilit, Format(Str0sIsMissingFrom,
        ['mkppstat.exe', PestDirectory]), nil);
    end;
    if not TFile.Exists(PestDirectory + 'ppcov_sva.exe') then
    begin
      frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel,
        StrRequiredPESTUtilit, Format(Str0sIsMissingFrom,
        ['ppcov_sva.exe', PestDirectory]), nil);
    end;

    // Batchfile
    Lines.Clear;
    Lines.Add(StrRemCreateVariogram);
    Lines.Add('');
    Lines.Add(Format('"%0:smkppstat.exe" <%1:s', [PestDirectory,
      ExtractFileName(MkppstatInputFileName)]));
    Lines.Add('');
    Lines.Add(StrRemCreateCovarianc);
    Lines.Add('');
    Lines.Add(Format('"%0:sppcov_sva.exe" <%1:s', [PestDirectory,
      ExtractFileName(Ppcov_svaFileName)]));
    Lines.Add('');
    Lines.Add(StrRemAfterThePrevio);
//    if not frmGoPhast.ExportingFromCommandLine then
//    begin
//      Lines.Add('pause');
//    end;
    BatchFileName := ChangeFileExt(PilotPointFileObject.FileName,
      '.CreateCov.Bat');
    Lines.WriteBOM := False;
    Lines.SaveToFile(BatchFileName);
  finally
    Lines.Free;
  end;

  RunAProgram(BatchFileName);
end;

end.
