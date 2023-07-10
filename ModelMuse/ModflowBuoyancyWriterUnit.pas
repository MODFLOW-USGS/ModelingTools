unit ModflowBuoyancyWriterUnit;

interface

uses
  CustomModflowWriterUnit, Vcl.Forms, ModflowPackageSelectionUnit;

type
  TBuoyancyWriter = class(TCustomPackageWriter)
  private
    procedure WriteOptions;
    procedure WriteDimensions;
    procedure WritePackageData;
  protected
    function Package: TModflowPackageSelection; override;
  public
    class function Extension: string; override;
    procedure WriteFile(const AFileName: string);

  end;

implementation

uses
  ModflowUnitNumbers, frmProgressUnit, GoPhastTypes;

resourcestring
  StrEvaluatingBUY6Pack = 'Evaluating BUY6 Package data.';

{ TBuoyancyWriter }

class function TBuoyancyWriter.Extension: string;
begin
  result := '.buy';
end;

function TBuoyancyWriter.Package: TModflowPackageSelection;
begin
  Result := Model.ModflowPackages.BuoyancyPackage;
end;

procedure TBuoyancyWriter.WriteDimensions;
begin

end;

procedure TBuoyancyWriter.WriteFile(const AFileName: string);
begin
  if Model.ModelSelection <> msModflow2015 then
  begin
    Exit
  end;
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrBuy) then
  begin
    Exit;
  end;

  frmProgressMM.AddMessage(StrEvaluatingBUY6Pack);
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  FNameOfFile := FileName(AFileName);
  WriteToNameFile(StrBuy, 0, FNameOfFile, foInput, Model, False, StrBuy);
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  FInputFileName := FNameOfFile;

  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage('Writing Buoyancy Package');
    Application.ProcessMessages;

    WriteTemplateHeader;

    WriteDataSet0;

    frmProgressMM.AddMessage(StrWritingOptions);
    WriteOptions;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDimensions);
    WriteDimensions;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('Writing Package Data');
    WritePackageData;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
  finally
    CloseFile;
  end;
end;

procedure TBuoyancyWriter.WriteOptions;
begin

end;

procedure TBuoyancyWriter.WritePackageData;
begin

end;

end.
