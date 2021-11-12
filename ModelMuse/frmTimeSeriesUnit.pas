unit frmTimeSeriesUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, JvPageList,
  Vcl.ExtCtrls, JvExControls, Vcl.ComCtrls, JvExComCtrls, JvPageListTreeView,
  frameModflow6TimeSeriesUnit, Vcl.StdCtrls, Vcl.Buttons;

type
  TfrmTimeSeries = class(TfrmCustomGoPhast)
    tvTimeSeries: TJvPageListTreeView;
    plTimeSeries: TJvPageList;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnAddGroup: TButton;
    btnDeleteGroup: TButton;
    procedure btnAddGroupClick(Sender: TObject);
    procedure btnDeleteGroupClick(Sender: TObject);
  private
    procedure edGroupNameChange(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmTimeSeries: TfrmTimeSeries;

implementation

{$R *.dfm}

procedure TfrmTimeSeries.btnAddGroupClick(Sender: TObject);
var
  NewName: string;
  NewFrame: TframeModflow6TimeSeries;
  NewPage: TJvCustomPage;
  NewNode: TJvPageIndexNode;
begin
  inherited;

  NewName := 'NewGroup' + IntToStr(tvTimeSeries.Items.Count + 1);
  NewPage := TJvStandardPage.Create(plTimeSeries);
  NewPage.PageList := plTimeSeries;
//  NewPage := plTimeSeries.Pages[plTimeSeries.PageCount-1];
  NewNode := tvTimeSeries.Items.AddChild(nil, NewName) as TJvPageIndexNode;
  NewNode.PageIndex := plTimeSeries.PageCount-1;
  NewFrame := TframeModflow6TimeSeries.Create(NewPage);
  NewFrame.Parent := NewPage;
  NewFrame.Align := AlClient;
  NewFrame.edGroupName.Tag := plTimeSeries.PageCount-1;
  NewFrame.edGroupName.OnChange := edGroupNameChange;
  NewFrame.edGroupName.Text := NewName;
  NewFrame.InitializeGrid;
  plTimeSeries.ActivePage := NewPage;
end;

procedure TfrmTimeSeries.btnDeleteGroupClick(Sender: TObject);
var
  PageIndex: Integer;
  AFrame: TframeModflow6TimeSeries;
  APage: TJvCustomPage;
begin
  inherited;
  if tvTimeSeries.Selected <> nil then
  begin
    plTimeSeries.ActivePage.Free;
    if tvTimeSeries.Items.Count > 0 then
    begin
      tvTimeSeries.Selected := tvTimeSeries.Items[0];
      plTimeSeries.ActivePageIndex := 0;
      for PageIndex := 0 to plTimeSeries.PageCount - 1 do
      begin
        APage := plTimeSeries.Pages[PageIndex];
        AFrame := APage.Controls[0] as TframeModflow6TimeSeries;
        AFrame.edGroupName.Tag := PageIndex;
      end;
    end
    else
    begin
      plTimeSeries.ActivePageIndex := -1;
    end;
  end;
end;

procedure TfrmTimeSeries.edGroupNameChange(Sender: TObject);
var
  Edit: TEdit;
begin
  Edit := Sender as TEdit;
  tvTimeSeries.Items[Edit.Tag].Text := Edit.Text;
end;

end.
