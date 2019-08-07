unit frameChoiceUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  frameCustomMetaDataUnit, FMX.ListBox, FMX.Controls.Presentation,
  MetaDataInterfacesUnit, FMX.ScrollBox, FMX.Memo;

type
  TframeChoice = class(TframeCustomMetaData)
    comboChoices: TComboBox;
    procedure comboChoicesChange(Sender: TObject);
  private
    FData: IChoiceMetaDataItem;
    { Private declarations }
  public
    procedure GetMetaData(Data: ICustomMetaData); override;
    { Public declarations }
  end;

var
  frameChoice: TframeChoice;

implementation

{$R *.fmx}

{ TframeChoice }

procedure TframeChoice.comboChoicesChange(Sender: TObject);
begin
  inherited;
  if comboChoices.ItemIndex >= 0 then
  begin
    FData.Choice := FData.Choices[comboChoices.ItemIndex];
  end
  else
  begin
    FData.Choice := '';
  end;
end;

procedure TframeChoice.GetMetaData(Data: ICustomMetaData);
var
  ItemIndex: Integer;
  ChoiceString: string;
begin
  inherited;
  FData := Data as IChoiceMetaDataItem;
  if Assigned(FData.OnUpdateChoices) then
  begin
    FData.OnUpdateChoices(FData.GetObject);
  end;
  if FData.AlternativeChoices.Count > 0 then
  begin
    Assert(FData.AlternativeChoices.Count = FData.Choices.Count);
    comboChoices.Items := FData.AlternativeChoices;
  end
  else
  begin
    comboChoices.Items := FData.Choices;
  end;
  ChoiceString := FData.GetStringContent;
  comboChoices.ItemIndex := FData.Choices.IndexOf(ChoiceString);;
  for ItemIndex := 0 to comboChoices.Items.Count - 1 do
  begin
    comboChoices.ListBox.ListItems[ItemIndex].Font.Size := 16;
  end;

end;

end.
