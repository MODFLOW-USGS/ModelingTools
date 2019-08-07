unit frameChoicePlusDateUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  MetaDataInterfacesUnit, frameCustomMetaDataUnit, FMX.ListBox, FMX.Calendar,
  FMX.Controls.Presentation, frameDateEditsUnit, FMX.ScrollBox, FMX.Memo;

type
  TframeChoicePlusDate = class(TframeCustomMetaData)
    comboChoices: TComboBox;
    frameDate: TframeDateEdits;
    procedure comboChoicesChange(Sender: TObject);
    procedure DateChanged(Sender: TObject);
  private
    FData: IChoicePlusDateMetaDataItem;
    { Private declarations }
  public
    procedure GetMetaData(Data: ICustomMetaData); override;
    { Public declarations }
  end;

var
  frameChoicePlusDate: TframeChoicePlusDate;

implementation

{$R *.fmx}

procedure TframeChoicePlusDate.comboChoicesChange(Sender: TObject);
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

procedure TframeChoicePlusDate.GetMetaData(Data: ICustomMetaData);
var
  ItemIndex: Integer;
  AnItem: TListBoxItem;
begin
  inherited;
  FData := Data as IChoicePlusDateMetaDataItem;
  comboChoices.Items := FData.Choices;
  comboChoices.ItemIndex := FData.Choices.IndexOf(FData.Choice);

  frameDate.GetDate(FData.DateIntf);
  frameDate.OnDateChanged := DateChanged;

  // the following doesn't work.
  for ItemIndex := 0 to comboChoices.ListBox.Count - 1 do
  begin
    AnItem := comboChoices.ListBox.ListItems[ItemIndex];
    AnItem.TextSettings.Font.Size := 16;
//    AnItem.StyledSettings := [];// AnItem.StyledSettings - [TStyledSetting.Other];
  end;
end;

procedure TframeChoicePlusDate.DateChanged(Sender: TObject);
begin
  comboChoices.ItemIndex := -1;
end;

end.
