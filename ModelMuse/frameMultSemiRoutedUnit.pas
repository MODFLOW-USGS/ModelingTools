unit frameMultSemiRoutedUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  JvExComCtrls, JvPageListTreeView, JvExControls, JvPageList, Vcl.StdCtrls,
  Vcl.Mask, JvExMask, JvSpin, Vcl.Buttons, Vcl.ExtCtrls, frameGridUnit,
  frameFormulaGridUnit, frameFarmDiversionUnit, ModflowFmpFarmUnit,
  RbwController;

type
  TframeMultSemiRouted = class(TFrame)
    pnlBottom: TPanel;
    lbNumber: TLabel;
    sbAdd: TSpeedButton;
    sbDelete: TSpeedButton;
    seNumber: TJvSpinEdit;
    frameFarmDiversions: TframeFarmDiversion;
    Panel1: TPanel;
    pnlName: TPanel;
    tvSRCollections: TTreeView;
    edSemiRouteName: TEdit;
    lblSemiRouteName: TLabel;
    Controller: TRbwController;
    procedure tvSRCollectionsChange(Sender: TObject; Node: TTreeNode);
    procedure seNumberChange(Sender: TObject);
  private
    FSrItem: TMultiSrdItem;
    FFarm: TFarm;
    FDiversionType: TDiversionType;
    FSrList: TSrCollList;
    FSfrNode: TTreeNode;
    FFarmNode: TTreeNode;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitializeControls;
    // ScreenObjectList contains only objects that define farms.
    procedure GetData(FarmList: TFarmList;
      DiversionType: TDiversionType);
    procedure SetData(FarmList: TFarmList; DiversionType: TDiversionType);
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TframeMultSemiRouted }

constructor TframeMultSemiRouted.Create(AOwner: TComponent);
begin
  inherited;
  FSrList := TSrCollList.Create;
end;

destructor TframeMultSemiRouted.Destroy;
begin
  FSrList.Free;
  inherited;
end;

procedure TframeMultSemiRouted.GetData(FarmList: TFarmList;
  DiversionType: TDiversionType);
var
  SrList: TSrCollList;
  FarmIndex: Integer;
  AFarm: TFarm;
  SRIndex: Integer;
  SRCollection: TSemiRoutedDeliveriesAndReturnFlowCollection;
  FarmNode: TTreeNode;
  SRItem: TMultiSrdItem;
begin
  FDiversionType := DiversionType;
//  SrList := TSrCollList.Create;
//  try
    FarmNode := nil;
    for FarmIndex := 0 to FarmList.Count - 1 do
    begin
      AFarm := FarmList[FarmIndex];
      FarmNode := tvSRCollections.Items.AddObject(FarmNode, AFarm.FarmName, AFarm);
      for  SRIndex := 0 to AFarm.MultiSrd.Count - 1 do
      begin
        SRItem := AFarm.MultiSrd[SRIndex];
        SRCollection := AFarm.MultiSrd[SRIndex].SemiRouted;
        tvSRCollections.Items.AddChildObject(FarmNode, SRItem.Name, SRItem);
//        SRNode.Data := SRCollection;
//        SrList.Add(SRCollection);
      end;
    end;
//  finally
//    SrList.Free;
//  end;
end;

procedure TframeMultSemiRouted.InitializeControls;
begin
  frameFarmDiversions.InitializeControls
end;

procedure TframeMultSemiRouted.seNumberChange(Sender: TObject);
begin
  if FSfrNode <> nil then
  begin
  end;
end;

procedure TframeMultSemiRouted.SetData(FarmList: TFarmList;
  DiversionType: TDiversionType);
begin
end;

procedure TframeMultSemiRouted.tvSRCollectionsChange(Sender: TObject;
  Node: TTreeNode);
var
  AnObject: TObject;
  SrList: TSrCollList;
  ANode: TTreeNode;
  SfrNode: TTreeNode;
begin
  if FSrList.Count >= 0 then
  begin
    frameFarmDiversions.SetDataForListOfSemiRoutedLists(SrList, FDiversionType);
  end;

  FFarm := nil;
  FSrItem := nil;
  FSfrNode := Nil;
  edSemiRouteName.Enabled := False;
  Controller.Enabled := False;
  if tvSRCollections.SelectionCount = 1 then
  begin
    AnObject := tvSRCollections.Selected.Data
    if AnObject is TFarm then
    begin
      FFarm := TFarm(AnObject);
      FFarmNode := tvSRCollections.Selected;
      Controller.Enabled := True;
      Exit;
    end;
  end;


  if tvSRCollections.SelectionCount >= 1 then
  begin
    FSrList.Clear;
    SrItem := nil;
    for Index := 0 to tvSRCollections.Items.Count - 1 do
    begin
      ANode := tvSRCollections.Items[Index];
      AnObject := ANode.Data;
      if AnObject is TMultiSrdItem then
      begin
        SfrNode := ANode;
        SrItem := TMultiSrdItem(AnObject);
        SrList.Add((AnObject as TMultiSrdItem).SemiRouted);
      end;
    end;
    if SrList.Count = 1 then
    begin
      FSfrNode := SfrNode;
      FSrItem := SrItem;
      edSemiRouteName.Enabled := True;
    end;
    frameFarmDiversions.GetDataFromListOfSemiRoutedLists(SrList, FDiversionType);
  end;
end;

end.
