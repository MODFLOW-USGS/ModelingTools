unit frameExtentUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  frameCustomMetaDataUnit, FMX.Edit, FMX.EditBox, FMX.NumberBox,
  FMX.Controls.Presentation, MetaDataInterfacesUnit, FMX.ScrollBox, FMX.Memo;

type
  TframeExtent = class(TframeCustomMetaData)
    numbxNorth: TNumberBox;
    numbxWest: TNumberBox;
    numbxEast: TNumberBox;
    numbxSouth: TNumberBox;
    lblNorth: TLabel;
    lblWest: TLabel;
    lblEast: TLabel;
    lblSouth: TLabel;
    procedure numbxNorthChange(Sender: TObject);
    procedure numbxWestChange(Sender: TObject);
    procedure numbxEastChange(Sender: TObject);
    procedure numbxSouthChange(Sender: TObject);
    procedure numbxWestPaint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure numbxEastPaint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure numbxNorthSouthPaint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
  private
    FData: IExtentDataItem;
    { Private declarations }
  public
    procedure GetMetaData(Data: ICustomMetaData); override;
    { Public declarations }
  end;

var
  frameExtent: TframeExtent;

implementation

{$R *.fmx}

{ TframeExtent }

procedure TframeExtent.GetMetaData(Data: ICustomMetaData);
begin
  inherited;
  FData := Data as IExtentDataItem;
  numbxNorth.Value := FData.North;
  numbxSouth.Value := FData.South;
  numbxEast.Value := FData.East;
  numbxWest.Value := FData.West;

  numbxNorth.HorzIncrement := 0;
  numbxNorth.VertIncrement := 0;
  numbxSouth.HorzIncrement := 0;
  numbxSouth.VertIncrement := 0;
  numbxEast.HorzIncrement := 0;
  numbxEast.VertIncrement := 0;
  numbxWest.VertIncrement := 0;
  numbxWest.HorzIncrement := 0;
end;

procedure TframeExtent.numbxEastChange(Sender: TObject);
begin
  inherited;
  FData.East := numbxEast.Value;
  numbxWest.Repaint;
  numbxEast.Repaint;
end;

procedure TframeExtent.numbxEastPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  inherited;
  if (numbxWest.Value > numbxEast.Value) then
  begin
    Canvas.Fill.Color := TAlphaColorRec.Red;
    Canvas.FillRect(ARect, 0, 0, AllCorners, 0.2);
  end
end;

procedure TframeExtent.numbxNorthChange(Sender: TObject);
begin
  inherited;
  FData.North := numbxNorth.Value;
  numbxNorth.Repaint;
  numbxSouth.Repaint;
end;

procedure TframeExtent.numbxNorthSouthPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  inherited;
  if (numbxNorth.Value < numbxSouth.Value) then
  begin
    Canvas.Fill.Color := TAlphaColorRec.Red;
    Canvas.FillRect(ARect, 0, 0, AllCorners, 0.2);
  end
end;

procedure TframeExtent.numbxSouthChange(Sender: TObject);
begin
  inherited;
  FData.South := numbxSouth.Value;
  numbxNorth.Repaint;
  numbxSouth.Repaint;
end;

procedure TframeExtent.numbxWestChange(Sender: TObject);
begin
  inherited;
  FData.West := numbxWest.Value;
  numbxWest.Repaint;
  numbxEast.Repaint;
end;

procedure TframeExtent.numbxWestPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  inherited;
  if (numbxWest.Value = 180) or (numbxWest.Value > numbxEast.Value) then
  begin
    Canvas.Fill.Color := TAlphaColorRec.Red;
    Canvas.FillRect(ARect, 0, 0, AllCorners, 0.2);
  end
end;

end.
