program Mf6InputReader;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
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
  GWFlowExchangeReaderUnit in 'GWFlowExchangeReaderUnit.pas',
  DisFileReaderUnit in 'DisFileReaderUnit.pas',
  DisvFileReaderUnit in 'DisvFileReaderUnit.pas',
  DisuFileReaderUnit in 'DisuFileReaderUnit.pas',
  IcFileReaderUnit in 'IcFileReaderUnit.pas',
  OcFileReaderUnit in 'OcFileReaderUnit.pas',
  ObsFileReaderUnit in 'ObsFileReaderUnit.pas',
  NpfFileReaderUnit in 'NpfFileReaderUnit.pas',
  TvkFileReaderUnit in 'TvkFileReaderUnit.pas';

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
      Mf6Simulation := TMf6Simulation.Create;
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
