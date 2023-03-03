unit frmLayersToExportUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.CheckLst, Vcl.Buttons;

type
  TfrmLayersToExport = class(TfrmCustomGoPhast)
    clbSelectedLayers: TCheckListBox;
    rgLayersToExport: TRadioGroup;
    pnlBase: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure rgLayersToExportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
  private
    procedure GetData;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmLayersToExport: TfrmLayersToExport;

implementation

uses
  frmGoPhastUnit, DataSetUnit;

{$R *.dfm}

procedure TfrmLayersToExport.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmLayersToExport.GetData;
var
  ContourDataSet: TDataArray;
  LayerIndex: Integer;
begin
  ContourDataSet := frmGoPhast.ContourDataSet;
  Assert(ContourDataSet <> nil);
  clbSelectedLayers.Items.BeginUpdate;
  try
    for LayerIndex := 1 to ContourDataSet.LayerCount do
    begin
      clbSelectedLayers.Items.Add(IntToStr(LayerIndex))
    end;
  finally
    clbSelectedLayers.Items.EndUpdate;
  end;
end;

procedure TfrmLayersToExport.rgLayersToExportClick(Sender: TObject);
begin
  inherited;
  clbSelectedLayers.Enabled := rgLayersToExport.ItemIndex = 1;
end;

end.
