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
  SimulationNameFileReaderUnit in 'SimulationNameFileReaderUnit.pas';

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
    end;
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
