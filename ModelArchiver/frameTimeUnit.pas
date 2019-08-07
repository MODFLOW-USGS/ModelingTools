unit frameTimeUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  frameCustomMetaDataUnit, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo, MetaDataInterfacesUnit, frameTimeEditsUnit;

type
  TframeTime = class(TframeCustomMetaData)
    spl1: TSplitter;
    frameTimeEdits: TframeTimeEdits;
  private
    FData: ITimeDataItem;
    { Private declarations }
  public
    procedure GetMetaData(Data: ICustomMetaData); override;
    { Public declarations }
  end;

var
  frameTime: TframeTime;

implementation



{$R *.fmx}

{ TframeTime }

procedure TframeTime.GetMetaData(Data: ICustomMetaData);
begin
  inherited;
  FData := Data as ITimeDataItem;
  frameTimeEdits.GetMetaData(FData.TimeIntf);
end;

end.

