unit ModpathTimeFileWriterUnit;

interface

uses CustomModflowWriterUnit;

type
  TModpathTimeFileWriter = class(TCustomModflowWriter)
  private
    procedure WriteDataSet1;
    procedure WriteDataSet2;
  protected
    class function Extension: string; override;
  public
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  ModpathParticleUnit;

{ TModpathTimeFileWriter }

class function TModpathTimeFileWriter.Extension: string;
begin
  result := '.tim';
end;

procedure TModpathTimeFileWriter.WriteDataSet1;
begin
  WriteInteger(Model.ModflowPackages.ModPath.OutputTimes.Count);
  WriteInteger(1);
  NewLine;
end;

procedure TModpathTimeFileWriter.WriteDataSet2;
var
  Index: Integer;
  Item: TModpathTimeItem;
begin
  for Index := 0 to Model.ModflowPackages.ModPath.OutputTimes.Count - 1 do
  begin
    Item := Model.ModflowPackages.ModPath.
      OutputTimes.Items[Index] as TModpathTimeItem;
    WriteFloat(Item.Time);
    NewLine;
  end;
end;

procedure TModpathTimeFileWriter.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  NameOfFile := FileName(AFileName);
  Model.AddModpathInputFile(NameOfFile);
  OpenFile(NameOfFile);
  try
    WriteDataSet1;
    WriteDataSet2;
  finally
    CloseFile;
  end;
end;

end.
