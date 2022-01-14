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
  ModelMuseUtilities;

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
  PestProp := frmGoPhast.PhastModel.PestProperties;
  Locations := frmGoPhast.PhastModel.ProgramLocations;
  Lines := TStringList.Create;
  try
    // MKPPSTAT Input
    Lines.Add(PilotPointFileObject.FileName);
    Lines.Add(IntToStr(PestProp.MaxPilotPointsInRange));
    Lines.Add('1');
    VariogramFileName := ChangeFileExt(PilotPointFileObject.FileName, '.Variogram.txt');
    Lines.Add(ExtractFileName(VariogramFileName));
    MkppstatInputFileName := ChangeFileExt(PilotPointFileObject.FileName, '.Mkppstat_Input.txt');
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
    Ppcov_svaFileName := ChangeFileExt(PilotPointFileObject.FileName, '.Ppcov_SVA_Input.txt');
    Lines.SaveToFile(Ppcov_svaFileName);

    // Batchfile
    Lines.Clear;
    PestDirectory := IncludeTrailingPathDelimiter(Locations.PestDirectory);
    Lines.Add('rem Create Variogram file.');
    Lines.Add('');
    Lines.Add(Format('%0:smkppstat.exe <%1:s', [PestDirectory, ExtractFileName(MkppstatInputFileName)]));
    Lines.Add('');
    Lines.Add('rem Create Covariance file.');
    Lines.Add('');
    Lines.Add(Format('%0:sppcov_sva.exe <%1:s', [PestDirectory, ExtractFileName(Ppcov_svaFileName)]));
    Lines.Add('');
    Lines.Add('rem After the previous line executes, sppcov_sva should show that a covariance file was written.');
    Lines.Add('pause');
    BatchFileName := ChangeFileExt(PilotPointFileObject.FileName, '.CreateCov.Bat');
    Lines.SaveToFile(BatchFileName);
  finally
    Lines.Free;
  end;

  RunAProgram(BatchFileName);
end;

end.
