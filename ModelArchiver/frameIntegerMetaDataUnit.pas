unit frameIntegerMetaDataUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  frameCustomMetaDataUnit, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.Edit, FMX.EditBox, FMX.NumberBox, MetaDataInterfacesUnit;

type
  TframeIntegerMetaData = class(TframeCustomMetaData)
    numbxValue: TNumberBox;
    procedure numbxValueChange(Sender: TObject);
    procedure numbxValuePaint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
  private
    FData: IIntegerInterface;
    FGettingData: Boolean;
    { Private declarations }
  public
    procedure GetMetaData(Data: ICustomMetaData); override;
    { Public declarations }
  end;

var
  frameIntegerMetaData: TframeIntegerMetaData;

implementation

{$R *.fmx}

{ TframeIntegerMetaData }

procedure TframeIntegerMetaData.GetMetaData(Data: ICustomMetaData);
begin
  inherited GetMetaData(Data);
  FGettingData := True;
  try
    FData := Data as IIntegerInterface;
    numbxValue.Min := FData.MinValue;
    numbxValue.Max := FData.MaxValue;
    numbxValue.Value := FData.Content;
    numbxValue.HorzIncrement := 0;
    numbxValue.VertIncrement := 0;
  finally
    FGettingData := False;
  end;
end;

procedure TframeIntegerMetaData.numbxValueChange(Sender: TObject);
begin
  inherited;
  if FGettingData then
  begin
    Exit;
  end;
  FData.Content := Round(numbxValue.Value);
end;

procedure TframeIntegerMetaData.numbxValuePaint(Sender: TObject;
  Canvas: TCanvas; const ARect: TRectF);
var
  AValue: Integer;
  Index: integer;
  IllegalValue: Integer;
begin
  inherited;
  if FData.IllegalValueCount > 0 then
  begin
    AValue := Round(numbxValue.Value);
    for Index := 0 to FData.IllegalValueCount - 1 do
    begin
      IllegalValue := FData.IllegalValues[index];
      if AValue = IllegalValue then
      begin
        Canvas.Fill.Color := TAlphaColorRec.Red;
        Canvas.FillRect(ARect, 0, 0, AllCorners, 0.2);
        Break;
      end;
    end;
  end;
end;

end.
