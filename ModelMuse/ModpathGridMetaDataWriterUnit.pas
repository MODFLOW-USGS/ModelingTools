unit ModpathGridMetaDataWriterUnit;

interface

uses
  CustomModflowWriterUnit;

type
  TGridMetaDataWriter = class(TCustomFileWriter)
  private
    procedure WriteDataSet0;
    procedure WriteDataSet1;
  public
    class function Extension: string; override;
    procedure WriteFile(AFileName: string);
  end;

implementation

uses
  FastGEO, ModflowGridUnit;

{ TGridMetaDataWriter }

class function TGridMetaDataWriter.Extension: string;
begin
  result := '.gridmeta';
end;

procedure TGridMetaDataWriter.WriteDataSet0;
begin
  WriteString(File_Comment('Grid metadata input'));
  NewLine
end;

procedure TGridMetaDataWriter.WriteDataSet1;
var
  FGrid: TModflowGrid;
  APoint: TPoint2D;
begin
  FGrid := Model.ModflowGrid;
  APoint := FGrid.TwoDElementCorner(0, FGrid.RowCount);
  WriteFloat(APoint.x);
  WriteFloat(APoint.y);
  WriteFloat(FGrid.GridAngle * 180 / Pi);
  NewLine;
end;

procedure TGridMetaDataWriter.WriteFile(AFileName: string);
begin
  AFileName := FileName(AFileName);
  Model.AddModpathInputFile(AFileName);
  OpenFile(AFileName);
  try
    WriteDataSet0;
    WriteDataSet1;
  finally
    CloseFile;
  end;
end;

end.
