program Mf6InputReader;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Vcl.Dialogs,
  System.IOUtils,
  ModelMuseUtilities in '..\ModelMuse\ModelMuseUtilities.pas',
  FastGEO in '..\ModelMuse\FastGEO.pas',
  Mf6.SimulationNameFileReaderUnit in 'Mf6.SimulationNameFileReaderUnit.pas',
  Mf6.CustomMf6PersistentUnit in 'Mf6.CustomMf6PersistentUnit.pas',
  Mf6.TDisFileReaderUnit in 'Mf6.TDisFileReaderUnit.pas',
  Mf6.AtsFileReaderUnit in 'Mf6.AtsFileReaderUnit.pas',
  Mf6.NameFileReaderUnit in 'Mf6.NameFileReaderUnit.pas',
  Mf6.ImsFileReaderUnit in 'Mf6.ImsFileReaderUnit.pas',
  ReadModflowArrayUnit in '..\ModelMuse\ReadModflowArrayUnit.pas',
  Mf6.DisFileReaderUnit in 'Mf6.DisFileReaderUnit.pas',
  Mf6.DisvFileReaderUnit in 'Mf6.DisvFileReaderUnit.pas',
  Mf6.DisuFileReaderUnit in 'Mf6.DisuFileReaderUnit.pas',
  Mf6.IcFileReaderUnit in 'Mf6.IcFileReaderUnit.pas',
  Mf6.OcFileReaderUnit in 'Mf6.OcFileReaderUnit.pas',
  Mf6.ObsFileReaderUnit in 'Mf6.ObsFileReaderUnit.pas',
  Mf6.NpfFileReaderUnit in 'Mf6.NpfFileReaderUnit.pas',
  Mf6.TvkFileReaderUnit in 'Mf6.TvkFileReaderUnit.pas',
  Mf6.TimeSeriesFileReaderUnit in 'Mf6.TimeSeriesFileReaderUnit.pas',
  Mf6.HfbFileReaderUnit in 'Mf6.HfbFileReaderUnit.pas',
  Mf6.StoFileReaderUnit in 'Mf6.StoFileReaderUnit.pas',
  Mf6.TvsFileReaderUnit in 'Mf6.TvsFileReaderUnit.pas',
  Mf6.CSubFileReaderUnit in 'Mf6.CSubFileReaderUnit.pas',
  Mf6.BuyFileReaderUnit in 'Mf6.BuyFileReaderUnit.pas',
  Mf6.VscFileReaderUnit in 'Mf6.VscFileReaderUnit.pas',
  Mf6.ChdFileReaderUnit in 'Mf6.ChdFileReaderUnit.pas',
  Mf6.WelFileReaderUnit in 'Mf6.WelFileReaderUnit.pas',
  Mf6.DrnFileReaderUnit in 'Mf6.DrnFileReaderUnit.pas',
  Mf6.RivFileReaderUnit in 'Mf6.RivFileReaderUnit.pas',
  Mf6.GhbFileReaderUnit in 'Mf6.GhbFileReaderUnit.pas',
  Mf6.RchFileReaderUnit in 'Mf6.RchFileReaderUnit.pas',
  Mf6.TimeArraySeriesFileReaderUnit in 'Mf6.TimeArraySeriesFileReaderUnit.pas',
  Mf6.EvtFileReaderUnit in 'Mf6.EvtFileReaderUnit.pas',
  Mf6.MawFileReaderUnit in 'Mf6.MawFileReaderUnit.pas',
  Mf6.SfrFileReaderUnit in 'Mf6.SfrFileReaderUnit.pas',
  Mf6.CrossSectionFileReaderUnit in 'Mf6.CrossSectionFileReaderUnit.pas',
  Mf6.LakFileReaderUnit in 'Mf6.LakFileReaderUnit.pas',
  Mf6.LakeTableFileReaderUnit in 'Mf6.LakeTableFileReaderUnit.pas',
  Mf6.UzfFileReaderUnit in 'Mf6.UzfFileReaderUnit.pas',
  Mf6.MvrFileReaderUnit in 'Mf6.MvrFileReaderUnit.pas',
  Mf6.GncFileReaderUnit in 'Mf6.GncFileReaderUnit.pas',
  Mf6.ExchangeFileReaderUnit in 'Mf6.ExchangeFileReaderUnit.pas',
  Mf6.MvtFileReaderUnit in 'Mf6.MvtFileReaderUnit.pas',
  Mf6.AdvFileReaderUnit in 'Mf6.AdvFileReaderUnit.pas',
  Mf6.DspFileReaderUnit in 'Mf6.DspFileReaderUnit.pas',
  Mf6.SsmFileReaderUnit in 'Mf6.SsmFileReaderUnit.pas',
  Mf6.SpcFileReaderUnit in 'Mf6.SpcFileReaderUnit.pas',
  Mf6.MstFileReaderUnit in 'Mf6.MstFileReaderUnit.pas',
  Mf6.IstFileReaderUnit in 'Mf6.IstFileReaderUnit.pas',
  Mf6.SrcFileReaderUnit in 'Mf6.SrcFileReaderUnit.pas',
  Mf6.CncFileReaderUnit in 'Mf6.CncFileReaderUnit.pas',
  Mf6.SftFileReaderUnit in 'Mf6.SftFileReaderUnit.pas',
  Mf6.LktFileReaderUnit in 'Mf6.LktFileReaderUnit.pas',
  Mf6.MwtFileReaderUnit in 'Mf6.MwtFileReaderUnit.pas',
  Mf6.UztFileReaderUnit in 'Mf6.UztFileReaderUnit.pas',
  Mf6.FmiFileReaderUnit in 'Mf6.FmiFileReaderUnit.pas';

var
  Mf6Simulation: TMf6Simulation;
  OpenDialog: TFileOpenDialog;
  FileName: string;
begin
  try
    FileName := '';
    if ParamCount >= 1 then
    begin
      FileName := ParamStr(1);
    end
    else
    begin
      OpenDialog :=  TFileOpenDialog.Create(nil);
      try
        if OpenDialog.Execute then
        begin
          FileName := OpenDialog.FileName;
        end
        else
        begin
          Exit;
        end;
      finally
        OpenDialog.Free;
      end;
    end;
    if TFile.Exists(FileName) then
    begin
      Mf6Simulation := TMf6Simulation.Create('Simulation');
      try
        Mf6Simulation.ReadSimulation(FileName);
      finally
        Mf6Simulation.Free;
      end;
    end
    else
    begin
      WriteLn(Format('The file "FileName" does not exist.', [FileName]));
    end;
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
