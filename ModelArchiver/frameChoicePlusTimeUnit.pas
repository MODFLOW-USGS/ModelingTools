unit frameChoicePlusTimeUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  frameCustomMetaDataUnit, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  frameTimeEditsUnit, FMX.ListBox, MetaDataInterfacesUnit;

type
  TframeChoicePlusTime = class(TframeCustomMetaData)
    comboChoices: TComboBox;
    frameTimeEdits: TframeTimeEdits;
    procedure comboChoicesChange(Sender: TObject);
  private
    FData: IChoicePlusTimeMetaDataItem;
    procedure TimeChanged(Sender: TObject);
    { Private declarations }
  public
    procedure GetMetaData(Data: ICustomMetaData); override;
    { Public declarations }
  end;

var
  frameChoicePlusTime: TframeChoicePlusTime;

implementation

{$R *.fmx}

{ TframeChoicePlusTime }

procedure TframeChoicePlusTime.comboChoicesChange(Sender: TObject);
begin
  inherited;
  if comboChoices.ItemIndex >= 0 then
  begin
    FData.Choice := comboChoices.Items[comboChoices.ItemIndex];
  end
  else
  begin
    FData.Choice := '';
  end;
end;

procedure TframeChoicePlusTime.GetMetaData(Data: ICustomMetaData);
begin
  inherited;
  FData := Data as IChoicePlusTimeMetaDataItem;
  comboChoices.Items := FData.Choices;
  comboChoices.ItemIndex := FData.Choices.IndexOf(FData.Choice);

  frameTimeEdits.GetMetaData(FData.TimeIntf);
  frameTimeEdits.OnTimeChanged := TimeChanged;
end;

procedure TframeChoicePlusTime.TimeChanged(Sender: TObject);
begin
  comboChoices.ItemIndex := -1;
end;

end.
