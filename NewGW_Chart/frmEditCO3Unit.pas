unit frmEditCO3Unit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ArgusDataEntry, Buttons, ComCtrls;

type
  TfrmEditCO3 = class(TForm)
    adeKHCO3: TArgusDataEntry;
    adeHCO3: TArgusDataEntry;
    adePH: TArgusDataEntry;
    rgTemperature: TRadioGroup;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    btnHelp: TBitBtn;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    StatusBar1: TStatusBar;
    procedure rgTemperatureClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure adeKHCO3Change(Sender: TObject);
  private
    procedure SetData;
    function GetValue: string;
    function GetUnits: string;
    procedure SetStatusBar;
    { Private declarations }
  public
    procedure GetData;
    { Public declarations }
  end;

var
  frmEditCO3: TfrmEditCO3;

implementation

uses Math, PiperGraphUnit, frmFormatUnit;

{$R *.DFM}

procedure TfrmEditCO3.GetData;
var
  Concentration : double;
begin
  with frmPiperGraph.DataGrid1 do
  begin
    try
      Concentration := InternationalStrToFloat(Cells[Col+1,Row]);
    except on EConvertError do
      begin
        Beep;
        MessageDlg('You must first enter the concentration of HCO3.',
          mtError, [mbOK], 0);
        Concentration := 0;
      end;
    end;
  end;
  case frmPiperGraph.rgDataType.ItemIndex of
    2 :
      begin
      end;
    3 :
      begin
        Concentration := Concentration/wtHCO3
      end;
  else
    begin
      Assert(False);
    end;
  end;
  adeHCO3.Text := FloatToStr(Concentration);
  SetStatusBar;
end;

procedure TfrmEditCO3.rgTemperatureClick(Sender: TObject);
begin
  adeKHCO3.Enabled := False;
  case rgTemperature.ItemIndex of
    0:
      begin
        adeKHCO3.Text := FloatToStr(Power(10,-10.63));
      end;
    1:
      begin
        adeKHCO3.Text := FloatToStr(Power(10,-10.55));
      end;
    2:
      begin
        adeKHCO3.Text := FloatToStr(Power(10,-10.49));
      end;
    3:
      begin
        adeKHCO3.Text := FloatToStr(Power(10,-10.43));
      end;
    4:
      begin
        adeKHCO3.Text := FloatToStr(Power(10,-10.38));
      end;
    5:
      begin
        adeKHCO3.Text := FloatToStr(Power(10,-10.33));
      end;
    6:
      begin
        adeKHCO3.Text := FloatToStr(Power(10,-10.29));
      end;
    7:
      begin
        adeKHCO3.Text := FloatToStr(Power(10,-10.20));
      end;
    8:
      begin
        adeKHCO3.Text := FloatToStr(Power(10,-10.14));
      end;
    9:
      begin
        adeKHCO3.Enabled := True;
      end;
  else
    begin
      Assert(False);
    end;
  end;

end;

function TfrmEditCO3.GetValue : string;
var
  Concentration : double;
begin
  Concentration := InternationalStrToFloat(adeKHCO3.Text)
    * InternationalStrToFloat(adeHCO3.Text) / Power(10,-InternationalStrToFloat(adePH.Text));
  case frmPiperGraph.rgDataType.ItemIndex of
    2 :
      begin
      end;
    3 :
      begin
        Concentration := Concentration*wtHCO3
      end;
  else
    begin
      Assert(False);
    end;
  end;
  result := FloatToStr(Concentration);

end;

procedure TfrmEditCO3.SetData;
begin
  with frmPiperGraph.DataGrid1 do
  begin
    Cells[Col,Row] := GetValue
  end;
end;

procedure TfrmEditCO3.btnOKClick(Sender: TObject);
begin
  SetData;
end;

procedure TfrmEditCO3.FormCreate(Sender: TObject);
begin
  rgTemperatureClick(Sender);
end;

function TfrmEditCO3.GetUnits : string;
begin
  case frmPiperGraph.rgDataType.ItemIndex of
    2 :
      begin
        result := 'meq/l'
      end;
    3 :
      begin
        result := 'mg/l'
      end;
  else
    begin
      Assert(False);
    end;
  end;
end;

procedure TfrmEditCO3.SetStatusBar;
begin
    Try
      StatusBar1.SimpleText := GetValue + ' ' + GetUnits;
    Except on EConvertError do
      begin
        StatusBar1.SimpleText := '';
      end;
    end;
end;

procedure TfrmEditCO3.adeKHCO3Change(Sender: TObject);
begin
  if (fsModal in FormState) then
  begin
    SetStatusBar;
  end;
end;

end.
