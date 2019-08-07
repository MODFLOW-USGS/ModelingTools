unit frameDateUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  frameCustomMetaDataUnit, FMX.Controls.Presentation, FMX.DateTimeCtrls,
  MetaDataInterfacesUnit, FMX.Calendar, FMX.ListBox, FMX.Edit, FMX.EditBox,
  FMX.NumberBox, FMX.TabControl, frameDateEditsUnit, FMX.ScrollBox, FMX.Memo;

type
  TframeDate = class(TframeCustomMetaData)
    frameDate: TframeDateEdits;
  private
    FData: IDateMetaDataItem;
    { Private declarations }
  public
    procedure GetMetaData(Data: ICustomMetaData); override;
    { Public declarations }
  end;

var
  frameDate: TframeDate;

implementation

{$R *.fmx}

{ TframeDate }

procedure TframeDate.GetMetaData(Data: ICustomMetaData);
begin
  inherited;
  FData := Data as IDateMetaDataItem;
  frameDate.GetDate(FData.DateIntf);
end;

end.
