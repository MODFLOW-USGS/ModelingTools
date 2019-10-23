unit frmEditSelectedWellsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ExtCtrls, frameAvailableObjectsUnit;

type
  TfrmEditSelectedWells = class(TfrmCustomGoPhast)
    frameWells: TframeAvailableObjects;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnCancelBtn: TBitBtn;
    btnOkBtn: TBitBtn;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure FormShow(Sender: TObject);
  private
    FAvailableWells: TStringList;
    FSelectedWells: TStringList;
    { Private declarations }
  public
    procedure GetData(AllWells, SelectedWells: TStringList);
    procedure SetData(SelectedWells: TStringList);
    { Public declarations }
  end;

var
  frmEditSelectedWells: TfrmEditSelectedWells;

implementation

{$R *.dfm}

{ TfrmEditSelectedWells }

procedure TfrmEditSelectedWells.FormCreate(Sender: TObject);
begin
  inherited;
  FAvailableWells := TStringList.Create;
  FSelectedWells := TStringList.Create;
end;

procedure TfrmEditSelectedWells.FormDestroy(Sender: TObject);
begin
  inherited;
  FAvailableWells.Free;
  FSelectedWells.Free;
end;

procedure TfrmEditSelectedWells.FormShow(Sender: TObject);
begin
  inherited;
  frameWells.FrameResize(nil);
end;

procedure TfrmEditSelectedWells.GetData(AllWells,
  SelectedWells: TStringList);
var
  WellIndex: Integer;
  WellPosition: Integer;
begin
  FAvailableWells.Assign(AllWells);
  FSelectedWells.Assign(SelectedWells);
  for WellIndex := FSelectedWells.Count - 1 downto 0 do
  begin
    WellPosition := FAvailableWells.IndexOf(FSelectedWells[WellIndex]);
    if WellPosition >= 0 then
    begin
      FAvailableWells.Delete(WellPosition);
    end
    else
    begin
      FSelectedWells.Delete(WellIndex);
    end;
  end;
  frameWells.lbSrcObjects.Items.Assign(FAvailableWells);
  frameWells.lbDstObjects.Items.Assign(FSelectedWells);
end;

procedure TfrmEditSelectedWells.SetData(SelectedWells: TStringList);
begin
  SelectedWells.Assign(frameWells.lbDstObjects.Items);
end;

end.
