unit frameTextUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  frameCustomMetaDataUnit, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation,
  MetaDataInterfacesUnit;

type
  TframeTextMetaData = class(TframeCustomMetaData)
    memoContent: TMemo;
    procedure memoContentChange(Sender: TObject);
  private
    FData: ITextMetaDataItem;
    { Private declarations }
  public
    procedure GetMetaData(Data: ICustomMetaData); override;
    { Public declarations }
  end;

var
  frameTextMetaData: TframeTextMetaData;

implementation

{$R *.fmx}

{ TframeCustomMetaData2 }

procedure TframeTextMetaData.GetMetaData(Data: ICustomMetaData);
begin
  inherited;
  FData := Data as ITextMetaDataItem;
  memoContent.Text := FData.Content;
end;

procedure TframeTextMetaData.memoContentChange(Sender: TObject);
begin
  inherited;
  FData.Content := memoContent.Text;
end;

end.
