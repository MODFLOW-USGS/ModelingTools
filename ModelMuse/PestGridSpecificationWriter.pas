unit PestGridSpecificationWriter;

interface

uses SysUtils, CustomModflowWriterUnit, PhastModelUnit;

type
  TPestGridSpecificationWriter = class(TCustomModflowWriter)
  private
    FArrayLength: integer;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
    procedure WriteDataSet4;
  public
    procedure WriteConstantU2DREL(const Comment: string;
      const Value: double; ArrayType: TModflowArrayType;
      const MF6_ArrayName: string); override;
    Procedure WriteU2DRELHeader(const Comment: string;
      ArrayType: TModflowArrayType; const MF6_ArrayName: string); override;
    class function Extension: string; override;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  FastGEO, GoPhastTypes;

{ TPestGridSpecificationWriter }

class function TPestGridSpecificationWriter.Extension: string;
begin
  result := '.gsf'
end;

procedure TPestGridSpecificationWriter.WriteConstantU2DREL(
  const Comment: string; const Value: double; ArrayType: TModflowArrayType;
  const MF6_ArrayName: string);
begin
//  inherited;
  WriteInteger(FArrayLength);
  WriteString('*');
  WriteString(FortranFloatToStr(Value));
  NewLine;
end;

procedure TPestGridSpecificationWriter.WriteDataSet1;
begin
  WriteInteger(Model.ModflowGrid.RowCount);
  WriteInteger(Model.ModflowGrid.ColumnCount);
  NewLine;
end;

procedure TPestGridSpecificationWriter.WriteDataSet2;
var
  APoint: TPoint2D;
  GridAngle: double;
begin
  APoint := Model.Grid.TwoDElementCorner(0,0);
  WriteFloat(APoint.x);
  WriteFloat(APoint.y);
  GridAngle := Model.Grid.GridAngle * 180 / Pi;
  WriteFloat(GridAngle);
  NewLine;
end;

procedure TPestGridSpecificationWriter.WriteDataSet3;
begin
  FArrayLength := Model.ModflowGrid.ColumnCount;
  Model.ModflowGrid.WriteDELR(self);
end;

procedure TPestGridSpecificationWriter.WriteDataSet4;
begin
  FArrayLength := Model.ModflowGrid.RowCount;
  Model.ModflowGrid.WriteDELC(self);
end;

procedure TPestGridSpecificationWriter.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  Assert(not Model.DisvUsed);
  NameOfFile := FileName(AFileName);
  FInputFileName := NameOfFile;
  OpenFile(NameOfFile);
  try
    WriteDataSet1;
    WriteDataSet2;
    WriteDataSet3;
    WriteDataSet4;
  finally
    CloseFile;
  end;
  Model.AddModelInputFile(NameOfFile)
end;

procedure TPestGridSpecificationWriter.WriteU2DRELHeader(const Comment: string;
  ArrayType: TModflowArrayType; const MF6_ArrayName: string);
begin
  // do nothing.
end;

end.
