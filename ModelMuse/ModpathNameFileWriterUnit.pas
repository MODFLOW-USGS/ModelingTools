unit ModpathNameFileWriterUnit;

interface

uses SysUtils, Classes, PhastModelUnit;

Type
  TModpathNameFileWriter = class(TObject)
  private
    FModel: TCustomModel;
    FEmbeddedExport: Boolean;
    FFileName: string;
    procedure CheckFileExists(const AFileName: string);
//  private
//    function ArchiveExtension: string;
  public
    procedure WriteFile(const FileName: string; Model: TCustomModel;
      EmbeddedExport: boolean);
    procedure WriteFileVersion6(const FileName: string; Model: TCustomModel;
      EmbeddedExport: boolean);
    procedure WriteFileVersion7(const FileName: string; Model: TCustomModel;
      EmbeddedExport: boolean);
  end;


implementation

uses ModpathMainFileWriterUnit, ModflowDiscretizationWriterUnit,
  ModpathStartingLocationsWriter, ModflowPackageSelectionUnit,
  ModflowOutputControlUnit, frmErrorsAndWarningsUnit, frmGoPhastUnit,
  ArchiveNodeInterface, GoPhastTypes, ModflowTDisWriterUnit,
  ModpathGridMetaDataWriterUnit;

resourcestring
  CbfFileExistsError = 'The following MODFLOW input or output files are '
    + 'required by MODPATH to run but they are not in the directory in which '
    + 'MODPATH is being run: "%s".';

{ TModpathNameFileWriter }

//function TModpathNameFileWriter.ArchiveExtension: string;
//begin
// Result := '_Modpath'
//end;
//
procedure TModpathNameFileWriter.WriteFile(const FileName: string;
  Model: TCustomModel; EmbeddedExport: boolean);
var
  NameFile: TStringList;
  NameArchiveFile: TStringList;
  AFileName: string;
  Options: TModpathSelection;
  Modelname: string;
  ModflowInputPrefix: string;
  OutputPrefix: string;
  ModflowOutputPrefix: string;
  procedure CheckFileExists(const AFileName: string);
  var
    FullFileName: string;
  begin
    if EmbeddedExport then
    begin
      Exit;
    end;

    FullFileName := ExpandFileName(AFileName);
    if not FileExists(FullFileName) then
    begin
      frmErrorsAndWarnings.AddError(Model, Format(CbfFileExistsError,
        [ExtractFilePath(FileName)]),
        AFileName);
    end;
  end;
begin
  Modelname := ChangeFileExt(ExtractFileName(FileName), '');
  Modelname := StringReplace(Modelname, ' ', '_', [rfReplaceAll, rfIgnoreCase]);
  ModflowInputPrefix := '..\..\model\model.' + Modelname + '\';
  ModflowOutputPrefix := '..\..\output\output.' + Modelname + '\';
  OutputPrefix := '..\..\output\output.' + Modelname + '_Modpath\';

  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, CbfFileExistsError);
    Options := Model.ModflowPackages.ModPath;

    NameFile := TStringList.Create;
    NameArchiveFile := TStringList.Create;
    try
      AFileName := ChangeFileExt(FileName, '.mplst');
      Model.AddModpathOutputFile(AFileName);
      AFileName := ExtractFileName(AFileName);
      NameFile.Add('LIST 11 ' + AFileName);
      NameArchiveFile.Add('LIST 11 ' + OutputPrefix + AFileName);

      AFileName := ChangeFileExt(FileName,
        TModpathMainFileWriter.Extension);
      Model.AddModpathInputFile(AFileName);
      AFileName := ExtractFileName(AFileName);
      NameFile.Add('MAIN 12 ' + AFileName);
      NameArchiveFile.Add('MAIN 12 ' + AFileName);

      AFileName := ChangeFileExt(FileName,
        TModflowDiscretizationWriter.Extension);
//      Model.AddModpathInputFile(AFileName);
      AFileName := ExtractFileName(AFileName);
      NameFile.Add('DIS 13 ' + AFileName);
      NameArchiveFile.Add('DIS 13 ' + ModflowInputPrefix + AFileName);
      CheckFileExists(AFileName);

      if Model.ModflowStressPeriods.TransientModel then
      begin
        AFileName := ChangeFileExt(FileName, '.cbf');
        Model.AddModpathOutputFile(AFileName);
        AFileName := ExtractFileName(AFileName);
        NameFile.Add('CBF 14 ' + AFileName);
        NameArchiveFile.Add('CBF 14 ' + OutputPrefix + AFileName);
        // The response file will direct MODPATH to generated
        // the CBF file if it doesn't exist.
  //      CheckFileExists(AFileName);
      end;

      if Options.Binary then
      begin
        AFileName := ChangeFileExt(FileName, '.end_bin');
      end
      else
      begin
        AFileName := ChangeFileExt(FileName, '.end');
      end;
      Model.AddModpathOutputFile(AFileName);
      AFileName := ExtractFileName(AFileName);
      NameFile.Add('ENDPOINT 15 ' + AFileName);
      NameArchiveFile.Add('ENDPOINT 15 ' + OutputPrefix + AFileName);
      case Options.OutputMode of
        mopEndpoints:
          begin
            // do nothing
          end;
        mopPathline:
          begin
            if Options.Binary then
            begin
              AFileName := ChangeFileExt(FileName, '.path_bin');
            end
            else
            begin
              AFileName := ChangeFileExt(FileName, '.path');
            end;
            Model.AddModpathOutputFile(AFileName);
            AFileName := ExtractFileName(AFileName);
            NameFile.Add('PATHLINE 16 ' + AFileName);
            NameArchiveFile.Add('PATHLINE 16 ' + OutputPrefix + AFileName);
          end;
        mopTimeSeries:
          begin
            if Options.Binary then
            begin
              AFileName := ChangeFileExt(FileName, '.ts_bin');
            end
            else
            begin
              AFileName := ChangeFileExt(FileName, '.ts');
            end;
            Model.AddModpathOutputFile(AFileName);
            AFileName := ExtractFileName(AFileName);
            NameFile.Add('TIME-SERIES 17 ' + AFileName);
            NameArchiveFile.Add('TIME-SERIES 17 ' + OutputPrefix + AFileName);
          end;
        else Assert(False);
      end;

      if Options.ShouldCreateTimeFile then
      begin
        AFileName := ChangeFileExt(FileName, '.tim');
        Model.AddModpathOutputFile(AFileName);
        AFileName := ExtractFileName(AFileName);
        NameFile.Add('TIME 18 ' + AFileName);
        NameArchiveFile.Add('TIME 18 ' + OutputPrefix + AFileName);
      end;

      AFileName := ChangeFileExt(FileName,
        TModpathStartingLocationsWriter.Extension);
      Model.AddModpathInputFile(AFileName);
      AFileName := ExtractFileName(AFileName);
      NameFile.Add('LOCATIONS 19 ' + AFileName);
      NameArchiveFile.Add('LOCATIONS 19 ' + AFileName);

      AFileName := ChangeFileExt(FileName, StrCbcExt);
//      Model.AddModpathInputFile(AFileName);
      AFileName := ExtractFileName(AFileName);
      NameFile.Add('BUDGET 20 ' + AFileName);
      NameArchiveFile.Add('BUDGET 20 ' + ModflowOutputPrefix + AFileName);
      CheckFileExists(AFileName);

      if Model.ModflowOutputControl.HeadOC.SaveInExternalFile then
      begin
        case Model.ModflowOutputControl.HeadOC.OutputFileType of
          oftText:
            begin
              AFileName := ChangeFileExt(FileName, StrFhd);
//              Model.AddModpathInputFile(AFileName);
              AFileName := ExtractFileName(AFileName);
              NameFile.Add('HEAD 21 ' + AFileName);
              NameArchiveFile.Add('HEAD 21 ' + ModflowOutputPrefix + AFileName);
              CheckFileExists(AFileName);
            end;
          oftBinary:
            begin
              AFileName := ChangeFileExt(FileName, StrBhd);
//              Model.AddModpathInputFile(AFileName);
              AFileName := ExtractFileName(AFileName);
              NameFile.Add('HEAD(BINARY) 22 ' + AFileName);
              NameArchiveFile.Add('HEAD(BINARY) 22 ' + ModflowOutputPrefix + AFileName);
              CheckFileExists(AFileName);
            end;
          else Assert(False);
        end;
      end;

      if Model.ModflowOutputControl.DrawdownOC.SaveInExternalFile then
      begin
        case Model.ModflowOutputControl.DrawdownOC.OutputFileType of
          oftText:
            begin
              AFileName := ChangeFileExt(FileName, StrFdn);
//              Model.AddModpathInputFile(AFileName);
              AFileName := ExtractFileName(AFileName);
              NameFile.Add('DRAWDOWN 23 ' + AFileName);
              NameArchiveFile.Add('DRAWDOWN 23 ' + ModflowOutputPrefix + AFileName);
              CheckFileExists(AFileName);
            end;
          oftBinary:
            begin
              AFileName := ChangeFileExt(FileName, StrBdn);
//              Model.AddModpathInputFile(AFileName);
              AFileName := ExtractFileName(AFileName);
              NameFile.Add('DRAWDOWN(BINARY) 24 ' + AFileName);
              NameArchiveFile.Add('DRAWDOWN(BINARY) 24 ' + ModflowOutputPrefix + AFileName);
              CheckFileExists(AFileName);
            end;
          else Assert(False);
        end;
      end;

      NameFile.SaveToFile(FileName);
      NameArchiveFile.SaveToFile(FileName + ArchiveExt);
      Model.AddModpathInputFile(FileName + ArchiveExt)
    finally
      NameFile.Free;
      NameArchiveFile.Free;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModpathNameFileWriter.CheckFileExists(const AFileName: string);
var
  FullFileName: string;
begin
  if FEmbeddedExport then
  begin
    Exit;
  end;

  FullFileName := ExpandFileName(AFileName);
  if not FileExists(FullFileName) then
  begin
    frmErrorsAndWarnings.AddError(FModel, Format(CbfFileExistsError,
      [ExtractFilePath(FFileName)]),
      AFileName);
  end;
end;


procedure TModpathNameFileWriter.WriteFileVersion6(const FileName: string;
  Model: TCustomModel; EmbeddedExport: boolean);
var
  NameFile: TStringList;
  NameArchiveFile: TStringList;
  AFileName: string;
  Modelname: string;
  ModflowInputPrefix: string;
  OutputPrefix: string;
  ModflowOutputPrefix: string;
//  Options: TModpathSelection;
begin
  FModel := Model;
  FEmbeddedExport := EmbeddedExport;
  FFileName := FileName;
//  FInputFileName := FFileName;
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, CbfFileExistsError);
  //  Options := Model.ModflowPackages.ModPath;

    Modelname := ChangeFileExt(ExtractFileName(FileName), '');
    ModflowInputPrefix := '..\..\model\model.' + Modelname + '\';
    ModflowOutputPrefix := '..\..\output\output.' + Modelname + '\';
    OutputPrefix := '..\..\output\output.' + Modelname + '_Modpath\';

    NameFile := TStringList.Create;
    NameArchiveFile := TStringList.Create;
    try
      AFileName := ChangeFileExt(FileName,
        TModpathBasicFileWriter.Extension);
      Model.AddModpathInputFile(AFileName);
      AFileName := ExtractFileName(AFileName);
      NameFile.Add('MPBAS 12 ' + AFileName);
      NameArchiveFile.Add('MPBAS 12 ' + AFileName);

      AFileName := ChangeFileExt(FileName,
        TModflowDiscretizationWriter.Extension);
//      frmGoPhast.PhastModel.AddModpathInputFile(AFileName);
      AFileName := ExtractFileName(AFileName);
      NameFile.Add('DIS 13 ' + AFileName);
      NameArchiveFile.Add('DIS 13 ' + ModflowInputPrefix +  AFileName);
      CheckFileExists(AFileName);

      AFileName := ChangeFileExt(FileName, StrCbcExt);
//      frmGoPhast.PhastModel.AddModpathInputFile(AFileName);
      AFileName := ExtractFileName(AFileName);
      NameFile.Add('BUDGET 20 ' + AFileName);
      NameArchiveFile.Add('BUDGET 20 ' + ModflowOutputPrefix + AFileName);
      CheckFileExists(AFileName);

      if Model.ModflowOutputControl.HeadOC.SaveInExternalFile then
      begin
        case Model.ModflowOutputControl.HeadOC.OutputFileType of
          oftText:
            begin
            end;
          oftBinary:
            begin
              AFileName := ChangeFileExt(FileName, StrBhd);
//              frmGoPhast.PhastModel.AddModpathInputFile(AFileName);
              AFileName := ExtractFileName(AFileName);
              NameFile.Add('HEAD 22 ' + AFileName);
              NameArchiveFile.Add('HEAD 22 ' + ModflowOutputPrefix + AFileName);
              CheckFileExists(AFileName);
            end;
          else Assert(False);
        end;
      end;

      Model.AddModpathInputFile(FileName + ArchiveExt);
      NameFile.SaveToFile(FileName);
      NameArchiveFile.SaveToFile(FileName + ArchiveExt);
    finally
      NameFile.Free;
      NameArchiveFile.Free;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModpathNameFileWriter.WriteFileVersion7(const FileName: string;
  Model: TCustomModel; EmbeddedExport: boolean);
var
  NameFile: TStringList;
  NameArchiveFile: TStringList;
  AFileName: string;
  Modelname: string;
  ModflowInputPrefix: string;
  OutputPrefix: string;
  ModflowOutputPrefix: string;
begin
  FModel := Model;
  FEmbeddedExport := EmbeddedExport;
  FFileName := FileName;
//  FInputFileName := FFileName;
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, CbfFileExistsError);

    Modelname := ChangeFileExt(ExtractFileName(FileName), '');
    ModflowInputPrefix := '..\..\model\model.' + Modelname + '\';
    ModflowOutputPrefix := '..\..\output\output.' + Modelname + '\';
    OutputPrefix := '..\..\output\output.' + Modelname + '_Modpath\';

    NameFile := TStringList.Create;
    NameArchiveFile := TStringList.Create;
    try
      AFileName := ChangeFileExt(FileName,
        TModpathBasicFileWriter.Extension);
      Model.AddModpathInputFile(AFileName);
      AFileName := ExtractFileName(AFileName);
      NameFile.Add('MPBAS ' + AFileName);
      NameArchiveFile.Add('MPBAS ' + AFileName);

      if Model.ModelSelection <> msModflow2015 then
      begin
        AFileName := ChangeFileExt(FileName,
          TModflowDiscretizationWriter.Extension);
  //      frmGoPhast.PhastModel.AddModpathInputFile(AFileName);
        AFileName := ExtractFileName(AFileName);
        NameFile.Add('DIS ' + AFileName);
        NameArchiveFile.Add('DIS ' + ModflowInputPrefix +  AFileName);
        CheckFileExists(AFileName);

      {$IFDEF MetaData}
        AFileName := ChangeFileExt(FileName,
          TGridMetaDataWriter.Extension);
        AFileName := ExtractFileName(AFileName);
        NameFile.Add('GRIDMETA ' + AFileName);
        NameArchiveFile.Add('GRIDMETA '  +  AFileName);
      {$ENDIF}

      end
      else
      begin
        if Model.DisvUsed then
        begin
          AFileName := ChangeFileExt(FileName, StrDisvgrb);
        end
        else
        begin
          AFileName := ChangeFileExt(FileName, StrDisgrb);
        end;
        AFileName := ExtractFileName(AFileName);
        if Model.DisvUsed then
        begin
          NameFile.Add('GRBDISV ' + AFileName);
          NameArchiveFile.Add('GRBDISV ' + ModflowOutputPrefix +  AFileName);
        end
        else
        begin
          NameFile.Add('GRBDIS ' + AFileName);
          NameArchiveFile.Add('GRBDIS ' + ModflowOutputPrefix +  AFileName);
        end;
        CheckFileExists(AFileName);
      end;

      if Model.ModelSelection = msModflow2015 then
      begin
        AFileName := ChangeFileExt(FileName,
          TTemporalDiscretizationWriter.Extension);
  //      frmGoPhast.PhastModel.AddModpathInputFile(AFileName);
        AFileName := ExtractFileName(AFileName);
        NameFile.Add('TDIS ' + AFileName);
        NameArchiveFile.Add('TDIS ' + ModflowInputPrefix +  AFileName);
        CheckFileExists(AFileName);
      end;

      AFileName := ChangeFileExt(FileName, StrCbcExt);
//      frmGoPhast.PhastModel.AddModpathInputFile(AFileName);
      AFileName := ExtractFileName(AFileName);
      NameFile.Add('BUDGET ' + AFileName);
      NameArchiveFile.Add('BUDGET ' + ModflowOutputPrefix + AFileName);
      CheckFileExists(AFileName);

      if Model.ModflowOutputControl.HeadOC.SaveInExternalFile then
      begin
        case Model.ModflowOutputControl.HeadOC.OutputFileType of
          oftText:
            begin
            end;
          oftBinary:
            begin
              AFileName := ChangeFileExt(FileName, StrBhd);
//              frmGoPhast.PhastModel.AddModpathInputFile(AFileName);
              AFileName := ExtractFileName(AFileName);
              NameFile.Add('HEAD ' + AFileName);
              NameArchiveFile.Add('HEAD ' + ModflowOutputPrefix + AFileName);
              CheckFileExists(AFileName);
            end;
          else Assert(False);
        end;
      end;

      Model.AddModpathInputFile(FileName + ArchiveExt);
      NameFile.SaveToFile(FileName);
      NameArchiveFile.SaveToFile(FileName + ArchiveExt);
    finally
      NameFile.Free;
      NameArchiveFile.Free;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

end.
