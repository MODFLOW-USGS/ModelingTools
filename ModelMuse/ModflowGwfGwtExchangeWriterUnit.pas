unit ModflowGwfGwtExchangeWriterUnit;

interface

uses
  System.SysUtils, CustomModflowWriterUnit, ModflowPackageSelectionUnit;

type
  TModflowGwfGwtExchangeWriter = class(TCustomPackageWriter)
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    procedure WriteFile(const AFileName: string; SpeciesIndex: Integer);
  end;


implementation

{ TModflowGwfGwtExchangeWriter }

class function TModflowGwfGwtExchangeWriter.Extension: string;
begin
  result := '.gwfgwt';
end;

function TModflowGwfGwtExchangeWriter.Package: TModflowPackageSelection;
begin
  result := nil;
end;

procedure TModflowGwfGwtExchangeWriter.WriteFile(
  const AFileName: string; SpeciesIndex: Integer);
var
//  Abbreviation: string;
  SpeciesName: string;
  GwtFile: string;
  Exchange: string;
begin
//  Abbreviation := 'GWF6-GWT6';
  SpeciesName := Model.MobileComponents[SpeciesIndex].Name;
  GwtFile := GwtFileName(AFileName, SpeciesIndex);
  FNameOfFile := GwtFile;
  Exchange := Format('GWF6-GWT6 %0:s MODFLOW %1:s', [ExtractFileName(GwtFile), SpeciesName]);
  Model.SimNameWriter.AddExchange(Exchange);
  OpenFile(FNameOfFile);
  try
    WriteCommentLine('GWF-GWT file created by ModelMuse');
  finally
    CloseFile;
  end;

end;

end.
