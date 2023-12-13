program Mf6InputReader;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Vcl.Dialogs,
  System.IOUtils,
  SimulationNameFileReaderUnit in 'SimulationNameFileReaderUnit.pas',
  CustomMf6PersistentUnit in 'CustomMf6PersistentUnit.pas',
  TDisFileReaderUnit in 'TDisFileReaderUnit.pas',
  ModelMuseUtilities in '..\ModelMuse\ModelMuseUtilities.pas',
  FastGEO in '..\ModelMuse\FastGEO.pas',
  AtsFileReaderUnit in 'AtsFileReaderUnit.pas',
  NameFileReaderUnit in 'NameFileReaderUnit.pas',
  ImsFileReaderUnit in 'ImsFileReaderUnit.pas',
  ReadModflowArrayUnit in '..\ModelMuse\ReadModflowArrayUnit.pas',
  DisFileReaderUnit in 'DisFileReaderUnit.pas',
  DisvFileReaderUnit in 'DisvFileReaderUnit.pas',
  DisuFileReaderUnit in 'DisuFileReaderUnit.pas',
  IcFileReaderUnit in 'IcFileReaderUnit.pas',
  OcFileReaderUnit in 'OcFileReaderUnit.pas',
  ObsFileReaderUnit in 'ObsFileReaderUnit.pas',
  NpfFileReaderUnit in 'NpfFileReaderUnit.pas',
  TvkFileReaderUnit in 'TvkFileReaderUnit.pas',
  TimeSeriesFileReaderUnit in 'TimeSeriesFileReaderUnit.pas',
  HfbFileReaderUnit in 'HfbFileReaderUnit.pas',
  StoFileReaderUnit in 'StoFileReaderUnit.pas',
  TvsFileReaderUnit in 'TvsFileReaderUnit.pas',
  CSubFileReaderUnit in 'CSubFileReaderUnit.pas',
  BuyFileReaderUnit in 'BuyFileReaderUnit.pas',
  VscFileReaderUnit in 'VscFileReaderUnit.pas',
  ChdFileReaderUnit in 'ChdFileReaderUnit.pas',
  WelFileReaderUnit in 'WelFileReaderUnit.pas',
  DrnFileReaderUnit in 'DrnFileReaderUnit.pas',
  RivFileReaderUnit in 'RivFileReaderUnit.pas',
  GhbFileReaderUnit in 'GhbFileReaderUnit.pas',
  RchFileReaderUnit in 'RchFileReaderUnit.pas',
  TimeArraySeriesFileReaderUnit in 'TimeArraySeriesFileReaderUnit.pas',
  EvtFileReaderUnit in 'EvtFileReaderUnit.pas',
  MawFileReaderUnit in 'MawFileReaderUnit.pas',
  SfrFileReaderUnit in 'SfrFileReaderUnit.pas',
  CrossSectionFileReaderUnit in 'CrossSectionFileReaderUnit.pas',
  LakFileReaderUnit in 'LakFileReaderUnit.pas',
  LakeTableFileReaderUnit in 'LakeTableFileReaderUnit.pas',
  UzfFileReaderUnit in 'UzfFileReaderUnit.pas',
  MvrFileReaderUnit in 'MvrFileReaderUnit.pas',
  GncFileReaderUnit in 'GncFileReaderUnit.pas',
  ExchangeFileReaderUnit in 'ExchangeFileReaderUnit.pas',
  MvtFileReaderUnit in 'MvtFileReaderUnit.pas',
  AdvFileReaderUnit in 'AdvFileReaderUnit.pas',
  DspFileReaderUnit in 'DspFileReaderUnit.pas',
  SsmFileReaderUnit in 'SsmFileReaderUnit.pas',
  SpcFileReaderUnit in 'SpcFileReaderUnit.pas',
  MstFileReaderUnit in 'MstFileReaderUnit.pas',
  IstFileReaderUnit in 'IstFileReaderUnit.pas',
  SrcFileReaderUnit in 'SrcFileReaderUnit.pas',
  CncFileReaderUnit in 'CncFileReaderUnit.pas',
  SftFileReaderUnit in 'SftFileReaderUnit.pas',
  LktFileReaderUnit in 'LktFileReaderUnit.pas',
  MwtFileReaderUnit in 'MwtFileReaderUnit.pas';

var
  Mf6Simulation: TMf6Simulation;
  OpenDialog: TFileOpenDialog;
  FileName: string;
begin
  try
    FileName := '';
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
