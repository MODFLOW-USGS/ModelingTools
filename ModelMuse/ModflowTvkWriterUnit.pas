unit ModflowTvkWriterUnit;

interface

uses
  System.SysUtils, System.Classes, Vcl.Forms, CustomModflowWriterUnit,
  ModflowBoundaryDisplayUnit, ModflowPackageSelectionUnit;

type
  TModflowTvk_Writer = class(TCustomTransientWriter)
  private
    procedure WriteOptions;
    procedure WriteStressPeriods;
    procedure WriteFileInternal;
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
    procedure Evaluate; override;
  public
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
    function WriteFile(const AFileName: string): string;
  end;

implementation

uses
  frmProgressUnit, ScreenObjectUnit, ModflowTvkUnit, CellLocationUnit,
  ModflowCellUnit, GoPhastTypes, frmErrorsAndWarningsUnit;

resourcestring
  StrEvaluatingTVKPacka = 'Evaluating TVK Package data.';
  StrTVKPackageSkipped = 'TVK package skipped.';
  StrTheTimeVaryingHyd = 'The Time-Varying Hydraulic Conductivity package wa' +
  's not included in the model because no time-varying hydraulic conductivit' +
  'y values have been defined.';

{ TModflowTvk_Writer }

procedure TModflowTvk_Writer.Evaluate;
var
  StartTime: Double;
  EndTime: Double;
  Dummy: TStringList;
  ObjectIndex: Integer;
  AScreenObject: TScreenObject;
  Boundary: TTvkBoundary;
begin
  inherited;
  StartTime := Model.ModflowFullStressPeriods.First.StartTime;
  EndTime := Model.ModflowFullStressPeriods.Last.Endtime;
  Dummy := TStringList.Create;
  try
    for ObjectIndex := 0 to Model.ScreenObjectCount - 1 do
    begin
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      AScreenObject := Model.ScreenObjects[ObjectIndex];
      if AScreenObject.Deleted then
      begin
        Continue;
      end;
      if not AScreenObject.UsedModels.UsesModel(Model) then
      begin
        Continue;
      end;
      Boundary := AScreenObject.ModflowTvkBoundary;
      if (Boundary = nil) or not Boundary.Used then
      begin
        Continue;
      end;

      frmProgressMM.AddMessage(Format(StrEvaluatingS, [AScreenObject.Name]));
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      Boundary.GetCellValues(Values, Dummy, Model, self);
      FTimeSeriesNames.AddStrings(Boundary.Mf6TimeSeriesNames);
    end;

  finally
    Dummy.Free;
  end;

end;

class function TModflowTvk_Writer.Extension: string;
begin
  result := '.tvk';
end;

function TModflowTvk_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.TvkPackage;
end;

procedure TModflowTvk_Writer.UpdateDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
begin

end;

function TModflowTvk_Writer.WriteFile(const AFileName: string): string;
begin
  result := '';
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.ModelSelection <> msModflow2015 then
  begin
    Exit;
  end;

  frmProgressMM.AddMessage(StrEvaluatingTVKPacka);
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  Evaluate;

  if Values.Count = 0 then
  begin
    frmErrorsAndWarnings.AddWarning(Model, StrTVKPackageSkipped,
      StrTheTimeVaryingHyd);
    Exit;
  end;

  FNameOfFile := FileName(AFileName);
  result := FNameOfFile;
  Model.AddModelInputFile(FNameOfFile);

  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  FInputFileName := FNameOfFile;
  WriteFileInternal;

  if Model.PestUsed and FPestParamUsed then
  begin
    FNameOfFile := FNameOfFile + '.tpl';
    WritePestTemplateLine(FNameOfFile);
    WritingTemplate := True;
    WriteFileInternal;
  end;

end;

procedure TModflowTvk_Writer.WriteFileInternal;
begin
  WriteOptions;
  WriteStressPeriods;
end;

procedure TModflowTvk_Writer.WriteOptions;
begin
  WriteBeginOptions;
  PrintListInputOption;
  WriteTimeSeriesFiles(FInputFileName);
  WriteEndOptions
end;

procedure TModflowTvk_Writer.WriteStressPeriods;
var
  StressPeriodIndex: Integer;
  Cells: TValueCellList;
  CellIndex: Integer;
  ACell: TTvk_Cell;
  AScreenObject: TScreenObject;
  procedure WriteCellID;
  begin
    WriteInteger(ACell.CellLocation.Layer);
    if not Model.DisvUsed then
    begin
      WriteInteger(ACell.CellLocation.Row);
    end;
    WriteInteger(ACell.CellLocation.Column);
  end;
begin
  for StressPeriodIndex := 0 to Model.ModflowFullStressPeriods.Count -1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    frmProgressMM.AddMessage(Format(
      '    Writing TVK Stress Period %d', [StressPeriodIndex+1]));

    WriteBeginPeriod(StressPeriodIndex);

    Cells := Values[StressPeriodIndex];

    for CellIndex := 0 to Cells.Count - 1 do
    begin
      ACell := Cells[CellIndex] as TTvk_Cell;

      WriteCellID;
      WriteString(' K');
      WriteValueOrFormula(ACell, KPosition);
      NewLine;

      WriteCellID;
      WriteString(' K22');
      WriteValueOrFormula(ACell, K22Position);
      NewLine;

      WriteCellID;
      WriteString(' K33');
      WriteValueOrFormula(ACell, K33Position);
      NewLine;
    end;

    WriteEndPeriod;
  end;
end;

end.
