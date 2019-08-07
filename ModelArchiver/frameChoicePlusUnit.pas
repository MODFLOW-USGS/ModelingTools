unit frameChoicePlusUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  frameCustomMetaDataUnit, FMX.ScrollBox, FMX.Memo, FMX.ListBox,
  FMX.Controls.Presentation, MetaDataInterfacesUnit, FMX.Edit;

type
  TframeChoicePlus = class(TframeCustomMetaData)
    comboChoices: TComboBox;
    memoFreeText: TMemo;
    procedure comboChoicesChange(Sender: TObject);
    procedure memoFreeTextChange(Sender: TObject);
  private
    FData: IChoicePlusMetaDataItem;
    { Private declarations }
  public
    procedure GetMetaData(Data: ICustomMetaData); override;
    { Public declarations }
  end;

var
  frameChoicePlus: TframeChoicePlus;

implementation

{$R *.fmx}

{ TframeChoicePlus }

procedure TframeChoicePlus.comboChoicesChange(Sender: TObject);
begin
  inherited;
  if comboChoices.ItemIndex >= 0 then
  begin
    memoFreeText.Text := '';
    FData.FreeContent := ''
  end;
  if comboChoices.ItemIndex >= 0 then
  begin
    FData.Choice := FData.Choices[comboChoices.ItemIndex];
  end
  else
  begin
    FData.Choice := '';
  end;
end;

procedure TframeChoicePlus.GetMetaData(Data: ICustomMetaData);
var
  ItemIndex: Integer;
begin
  inherited;
  FData := Data as IChoicePlusMetaDataItem;
  if FData.AlternativeChoices.Count > 0 then
  begin
    Assert(FData.AlternativeChoices.Count = FData.Choices.Count);
    comboChoices.Items := FData.AlternativeChoices;
  end
  else
  begin
    comboChoices.Items := FData.Choices;
  end;
//  comboChoices.Items := FData.Choices;
  comboChoices.ItemIndex := FData.Choices.IndexOf(FData.Choice);
  memoFreeText.Text := FData.FreeContent;
  for ItemIndex := 0 to comboChoices.Items.Count - 1 do
  begin
    comboChoices.ListBox.ListItems[ItemIndex].Font.Size := 16;
  end;
end;

procedure TframeChoicePlus.memoFreeTextChange(Sender: TObject);
begin
  inherited;
  FData.FreeContent := memoFreeText.Text;
  if FData.FreeContent <> '' then
  begin
    comboChoices.ItemIndex := -1;
    FData.Choice := '';
  end;
end;

end.
