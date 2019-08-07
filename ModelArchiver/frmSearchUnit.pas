unit frmSearchUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit;

type
  TSearchItem = (siValue, siDescription, siName, siAbbreviation);
  TSearchItems = set of TSearchItem;
  TSearchDirection = (sdForward, sdBackward);
  TSearchRestriction = (srAll, srUsed, srUnused);

  TfrmSearch = class(TForm)
    edFindWhat: TEdit;
    lblFindWhat: TLabel;
    btnFindBack: TButton;
    btnFindForward: TButton;
    cbMatchCase: TCheckBox;
    grpItemsToSearch: TGroupBox;
    rbAll: TRadioButton;
    rbSelected: TRadioButton;
    rbUnselected: TRadioButton;
    grpPartsToSearch: TGroupBox;
    cbValue: TCheckBox;
    cbDescription: TCheckBox;
    cbLongName: TCheckBox;
    cbShortName: TCheckBox;
    procedure btnFindBackClick(Sender: TObject);
    procedure btnFindForwardClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FSearchItems: TSearchItems;
    FSearchRestriction: TSearchRestriction;
    FSearchTerm: string;
    FCaseSensitive: Boolean;
    procedure SetSearchParameters;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSearch: TfrmSearch;

implementation

uses
  frameMetaDataEditorUnit;

{$R *.fmx}

{ TfrmSearch }

procedure TfrmSearch.btnFindBackClick(Sender: TObject);
begin
  SetSearchParameters;
  Frame.DoSearch(FSearchTerm, FSearchItems, sdBackward, FSearchRestriction,
    FCaseSensitive);
end;

procedure TfrmSearch.btnFindForwardClick(Sender: TObject);
begin
  SetSearchParameters;
  Frame.DoSearch(FSearchTerm, FSearchItems, sdForward, FSearchRestriction,
    FCaseSensitive);
end;

procedure TfrmSearch.FormShow(Sender: TObject);
begin
  Focused := edFindWhat;
end;

procedure TfrmSearch.SetSearchParameters;
begin
  FSearchItems := [];
  if cbValue.IsChecked then
  begin
    FSearchItems := FSearchItems + [siValue];
  end;
  if cbDescription.IsChecked then
  begin
    FSearchItems := FSearchItems + [siDescription];
  end;
  if cbLongName.IsChecked then
  begin
    FSearchItems := FSearchItems + [siName];
  end;
  if cbShortName.IsChecked then
  begin
    FSearchItems := FSearchItems + [siAbbreviation];
  end;

  FSearchRestriction := srAll;
  if rbAll.IsChecked then
  begin
    FSearchRestriction := srAll;
  end
  else  if rbSelected.IsChecked then
  begin
    FSearchRestriction := srUsed;
  end
  else if rbUnselected.IsChecked then
  begin
    FSearchRestriction := srUnused;
  end;

  FSearchTerm := edFindWhat.Text;

  FCaseSensitive := cbMatchCase.IsChecked;
end;

end.
