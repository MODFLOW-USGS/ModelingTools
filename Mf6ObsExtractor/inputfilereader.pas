unit InputFileReader;

{$IFDEF FPC}
  {$mode DELPHI}
{$endif}
{$H+}
{$ASSERTIONS ON}

interface

uses
  Classes, SysUtils, Generics.Defaults, CustomOutputFileReader,
  OutputFileReader, RbwParser, FastGEO, CustomInputReader;

type
  { TInputHandler }

  TInputHandler = class(TCustomInputHandler)
  //private
    //FID: string;
  protected
    function CreateObsFile(const FileName: string): TCustomOutputFile; override;
    //procedure HandleIdentifiers;
    function ApplicationTitle: string; override;
  public
    //procedure ReadAndProcessInputFile(const FileName: string);
  end;

implementation

resourceString
  rsMODFLOW6Obse = 'MODFLOW 6 Observation Extractor';

{ TInputHandler }

function TInputHandler.CreateObsFile(const FileName: string): TCustomOutputFile;
begin
  result := TOutputFile.Create(FileName, FFileType, FObservationDictionary);
end;


function TInputHandler.ApplicationTitle: string;
begin
  result := rsMODFLOW6Obse;
end;


end.

