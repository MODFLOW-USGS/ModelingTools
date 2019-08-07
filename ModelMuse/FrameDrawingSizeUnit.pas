unit FrameDrawingSizeUnit;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, clxDataEntry;

type
  TframeDrawingSize = class(TFrame)
    gbDrawingArea: TGroupBox;
    adeMin1: TArgusDataEntry;
    lblMin1: TLabel;
    adeMax1: TArgusDataEntry;
    lblMax1: TLabel;
    adeMin2: TArgusDataEntry;
    lblMin2: TLabel;
    lblMax2: TLabel;
    adeMax2: TArgusDataEntry;
    procedure adeMax1Change(Sender: TObject);
    procedure adeMax2Change(Sender: TObject);
    function IsValid: boolean;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TframeDrawingSize.adeMax1Change(Sender: TObject);
var
  XMax, XMin: double;
begin
  if (adeMax1.Text <> '') and (adeMin1.Text <> '') then
  begin
    try
      XMax := StrToFloat(adeMax1.Text);
      XMin := StrToFloat(adeMin1.Text);
      if XMin >= XMax then
      begin
        adeMax1.Color := clRed;
        adeMin1.Color := clRed;
      end
      else
      begin
        adeMax1.Color := clWhite;
        adeMin1.Color := clWhite;
      end;
    except on EConvertError do
      begin
      end;
    end;
  end;
end;

procedure TframeDrawingSize.adeMax2Change(Sender: TObject);
var
  YMax, YMin: double;
begin
  if (adeMax2.Text <> '') and (adeMin2.Text <> '') then
  begin
    try
      YMax := StrToFloat(adeMax2.Text);
      YMin := StrToFloat(adeMin2.Text);
      if YMin >= YMax then
      begin
        adeMax2.Color := clRed;
        adeMin2.Color := clRed;
      end
      else
      begin
        adeMax2.Color := clWhite;
        adeMin2.Color := clWhite;
      end;
    except on EConvertError do
      begin
      end;
    end;
  end;
end;

function TframeDrawingSize.IsValid: boolean;
var
  XMax, XMin: double;
  YMax, YMin: double;
begin
  result := false;
  try
    XMax := StrToFloat(adeMax1.Text);
    XMin := StrToFloat(adeMin1.Text);
    result := XMax > XMin;
  except on EConvertError do
    begin
      Exit;
    end;
  end;
  if result then
  begin
    try
      YMax := StrToFloat(adeMax2.Text);
      YMin := StrToFloat(adeMin2.Text);
      result := YMax > YMin;
    except on EConvertError do
      begin
        result := False;
        Exit;
      end;
    end;
  end;
end;

end.
