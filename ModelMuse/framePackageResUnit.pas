unit framePackageResUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageTransientLayerChoiceUnit, Mask, JvExMask, JvSpin, StdCtrls,
  ExtCtrls, JvExStdCtrls, JvCheckBox, ModflowPackageSelectionUnit, RbwController;

type
  TframePackageRes = class(TframePackageTransientLayerChoice)
    cbPrintStage: TCheckBox;
    seTableSize: TJvSpinEdit;
    lblTableSize: TLabel;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

//var
//  framePackageRes: TframePackageRes;

implementation

{$R *.dfm}

{ TframePackageRes }

procedure TframePackageRes.GetData(Package: TModflowPackageSelection);
var
  ResPackge: TResPackageSelection;
begin
  inherited;
  ResPackge := Package as TResPackageSelection;
  cbPrintStage.Checked := ResPackge.PrintStage;
  seTableSize.AsInteger := ResPackge.TableStages;
end;

procedure TframePackageRes.SetData(Package: TModflowPackageSelection);
var
  ResPackge: TResPackageSelection;
begin
  inherited;
  ResPackge := Package as TResPackageSelection;
  ResPackge.PrintStage := cbPrintStage.Checked;
  ResPackge.TableStages := seTableSize.AsInteger;
end;

end.
