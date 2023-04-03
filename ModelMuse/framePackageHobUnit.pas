unit framePackageHobUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, StdCtrls, ArgusDataEntry, RbwController,
  ModflowPackageSelectionUnit;

type
  TframePackageHob = class(TframePackage)
    rdeDryHead: TRbwDataEntry;
    lblDryHead: TLabel;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageHob: TframePackageHob;

implementation

uses
  GoPhastTypes;

{$R *.dfm}

{ TframePackageHob }

procedure TframePackageHob.GetData(Package: TModflowPackageSelection);
begin
  Assert(Package  is THobPackageSelection);
  rdeDryHead.Text := FloatToStr(THobPackageSelection(Package).DryHead);
  inherited;
end;

procedure TframePackageHob.SetData(Package: TModflowPackageSelection);
begin
  Assert(Package  is THobPackageSelection);
  THobPackageSelection(Package).DryHead := FortranStrToFloat(rdeDryHead.Text);
  inherited;
end;

end.
