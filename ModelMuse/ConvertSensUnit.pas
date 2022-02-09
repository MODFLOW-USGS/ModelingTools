{ The pupose of @name is to provide a method for converting MODFLOW-2000
  Sensitivity files to MODFLOW-2005 PVAL files.
}
unit ConvertSensUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils;

procedure ConvertSens(const SensFileName: string; out PvalFile: string);

implementation

uses
  PhastModelUnit;

type
  TSensDataSet = (sds1, sds2, sds3);

procedure ConvertSens(const SensFileName: string; out PvalFile: string);
var
  SensFile: TStringList;
  LineIndex: Integer;
  DataSet: TSensDataSet;
  Line2Position: Integer;
  Splitter: TStringList;
  NewName: string;
  Index: Integer;
  ALine: string;
begin
  SensFile := TStringList.Create;
  Splitter := TStringList.Create;
  try
    DataSet := sds1;
    Line2Position := -1;
    Splitter.Delimiter := ' ';
    SensFile.LoadFromFile(SensFileName);
    for LineIndex := 0 to SensFile.Count - 1 do
    begin
      ALine := SensFile[LineIndex];
      if (ALine = '') or (ALine[1] = '#') then
      begin
        Continue;
      end;
      case DataSet of
        sds1: Inc(DataSet);
        sds2:
          begin
            Line2Position := LineIndex;
            Inc(DataSet);
          end;
        sds3:
          begin
            Splitter.DelimitedText := ALine;
            Assert(Splitter.Count >= 6);
            ALine := Format('%0:s %1:s', [Splitter[0], Splitter[3]]);
            SensFile[LineIndex] := ALine;
          end;
      end;
    end;
    Assert(Line2Position > 0);
    SensFile.Delete(Line2Position);
    NewName := ChangeFileExt(SensFileName, StrPvalExt);
    PvalFile := NewName;
    Index := 1;
    while TFile.Exists(PvalFile) do
    begin
      PvalFile := Format('%0:s%1:d', [NewName, Index]);
      Inc(Index);
    end;
    SensFile.WriteBOM := False;
    SensFile.SaveToFile(PvalFile);
  finally
    SensFile.Free;
    Splitter.Free;
  end;
end;


end.
