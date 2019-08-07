unit frameNumericUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  frameCustomMetaDataUnit, FMX.Edit, FMX.EditBox, FMX.NumberBox,
  FMX.Controls.Presentation, MetaDataInterfacesUnit, FMX.ScrollBox, FMX.Memo;

type
  TframeNumeric = class(TframeCustomMetaData)
    numbxValue: TNumberBox;
    procedure numbxValueChange(Sender: TObject);
    procedure numbxValuePaint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
  private
    FData: INumericInterface;
    { Private declarations }
  public
    procedure GetMetaData(Data: ICustomMetaData); override;
    { Public declarations }
  end;

var
  frameNumeric: TframeNumeric;

implementation

{$R *.fmx}

{ TframeNumeric }

procedure TframeNumeric.GetMetaData(Data: ICustomMetaData);
begin
  inherited GetMetaData(Data);
  FData := Data as INumericInterface;
  numbxValue.Min := FData.MinValue;
  numbxValue.Max := FData.MaxValue;
  numbxValue.Value := FData.Content;
  numbxValue.HorzIncrement := 0;
  numbxValue.VertIncrement := 0;
end;

procedure TframeNumeric.numbxValueChange(Sender: TObject);
begin
  inherited;
  if Assigned(FData) then
  begin
    FData.Content := numbxValue.Value;
  end;
end;

procedure TframeNumeric.numbxValuePaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  inherited;
  if (FData.MaxLimitType = mltLess) and (FData.Content = FData.MaxValue) then
  begin
    Canvas.Fill.Color := TAlphaColorRec.Red;
    Canvas.FillRect(ARect, 0, 0, AllCorners, 0.2);
  end
  else if (FData.MinLimitType = mltGreater) and (FData.Content = FData.MinValue) then
  begin
    Canvas.Fill.Color := TAlphaColorRec.Red;
    Canvas.FillRect(ARect, 0, 0, AllCorners, 0.2);
  end;
end;

end.
