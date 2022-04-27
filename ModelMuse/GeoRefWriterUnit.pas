unit GeoRefWriterUnit;

interface

uses
  CustomModflowWriterUnit, System.SysUtils;

type
  TSubModelType = (smtMain, smtZoneBudget2, smtModpath5, smtModpath6, smtMt3dms);

  TGeoRefWriter = class(TCustomFileWriter)
  private
    FFileName: string;
    procedure WriteDate(Date: TDate);
    procedure WriteTime(Date: TTime);
  protected
    class function Extension: string; override;
  public
    procedure WriteFile(AFileName: string; SubModelType: TSubModelType);
  end;


implementation

uses
  GeoRefUnit, frmErrorsAndWarningsUnit, ModflowPackageSelectionUnit,
  System.IOUtils;

resourcestring
  StrTheSOfYourModel = 'The %s of your model is undefined. You can define it' +
  ' in "Model|Edit Geo Ref."';
  StrMissingGeoReferenc = 'Missing Geo Reference data';

{ TGeoRefWriter }

class function TGeoRefWriter.Extension: string;
begin
  Result := '.reference'
end;

procedure TGeoRefWriter.WriteDate(Date: TDate);
var
  Year: Word;
  Month: Word;
  Day: Word;
begin
  DecodeDate(Date, Year, Month, Day);
  WriteString(' ' + Format('%0:d/%1:d/%2:d', [Month, Day, Year]));
end;

procedure TGeoRefWriter.WriteFile(AFileName: string; SubModelType: TSubModelType);
var
  Directory: string;
  FGeoRef: TGeoRef;
  LengthUnit: string;
  TimeUnit: string;
begin
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrMissingGeoReferenc);


  FFileName := ChangeFileExt(AFileName, '');
  case SubModelType of
    smtMain: ;
    smtZoneBudget2: FFileName := FFileName + '_ZoneBudget';
    smtModpath5, smtModpath6: FFileName := FFileName + '_Modpath';
    smtMt3dms:
      begin
        if Model.ModflowPackages.Mt3dBasic.Mt3dVersion = mvUSGS then
        begin
          FFileName := FFileName + '_MT3D-USGS';
        end
        else
        begin
          FFileName := FFileName + '_MT3DMS';
        end;
      end
  else
    Assert(False);
  end;

  FFileName := FileName(FFileName);
  Directory := IncludeTrailingPathDelimiter(ExtractFileDir(FFileName));
  FFileName := ExtractFileName(FFileName);
  FFileName := Directory + 'usgs.' + FFileName;

  case SubModelType of
    smtMain: Model.AddModelInputFile(FFileName);
    smtZoneBudget2: Model.AddZoneBudgetInputFile(FFileName);
    smtModpath5, smtModpath6: Model.AddModpathInputFile(FFileName);
    smtMt3dms: Model.AddMt3dmsInputFile(FFileName);
  else
    Assert(False);
  end;

  FGeoRef := Model.GeoRef;
//  FInputFileName := FFileName;
  OpenFile(FFileName);
  try
    WriteString('xul');
    WriteFloat(FGeoRef.UpperLeftX);
    NewLine;

    WriteString('yul');
    WriteFloat(FGeoRef.UpperLeftY);
    NewLine;

    WriteString('rotation');
    WriteFloat(FGeoRef.Rotation*180/Pi);
    NewLine;

    WriteString('length_units ');
    LengthUnit := FGeoRef.LengthUnit;
    if LengthUnit = '' then
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrMissingGeoReferenc,
        Format(StrTheSOfYourModel, ['length unit']));
    end;
    WriteString(LengthUnit);
    NewLine;

    WriteString('time_units ');
    TimeUnit := FGeoRef.TimeUnit;
    if TimeUnit = '' then
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrMissingGeoReferenc,
        Format(StrTheSOfYourModel, ['time unit']));
    end;
    WriteString(TimeUnit);
    NewLine;

    WriteString('start_date');
    if FGeoRef.StartDate = 0 then
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrMissingGeoReferenc,
        Format(StrTheSOfYourModel, ['starting date']));
    end;
    WriteDate(FGeoRef.StartDate);
    NewLine;

    WriteString('start_time');
    WriteTime(FGeoRef.StartTime);
    NewLine;

    WriteString('model ');
    case SubModelType of
      smtMain: WriteString(FGeoRef.ModelProgramName);
      smtZoneBudget2: WriteString('Zonebudget');
      smtModpath5: WriteString('Modpath version 5');
      smtModpath6: WriteString('Modpath version 6');
      smtMt3dms: WriteString('MT3DMS');
    else
      Assert(False);
    end;

    NewLine;

    case FGeoRef.ProjectionType of
      ptEpsg: WriteString('epsg ');
      ptProj4: WriteString('proj4 ');
      else
       WriteString('unknown ');
    end;
    if FGeoRef.Projection = '' then
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrMissingGeoReferenc,
        Format(StrTheSOfYourModel, ['projection']));
    end;
    WriteString(FGeoRef.Projection);
    NewLine;
  finally
    CloseFile;
  end;

  if SubModelType = smtMain then
  begin
    try
      TFile.Copy(FFileName, Directory + 'usgs.model.reference', True);
    except on EInOutError do
      begin
        // ignore
      end;
    end;
  end;
end;

procedure TGeoRefWriter.WriteTime(Date: TTime);
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(Date, Hour, Min, Sec, MSec);
  WriteString(' ' + Format('%0:d:%1:d:%2:d', [Hour, Min, Sec]));
end;

end.
