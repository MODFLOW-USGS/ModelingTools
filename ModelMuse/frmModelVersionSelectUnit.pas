unit frmModelVersionSelectUnit;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls, frmCustomGoPhastUnit, QButtons, QExtCtrls, BasicTypes;

type
  TfrmModelVersionSelect = class(TfrmCustomGoPhast)
    rgChoice: TRadioGroup;
    BitBtn1: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function GetPhastVersion: TPhastModelVersion;

var
  frmModelVersionSelect: TfrmModelVersionSelect;

implementation

{$R *.dfm}

function GetPhastVersion: TPhastModelVersion;
begin
  with TfrmModelVersionSelect.Create(nil) do
  begin
    try
      ShowModal;
      result := TPhastModelVersion(rgChoice.ItemIndex);
    finally
      Free;
    end;
  end;
end;


end.
