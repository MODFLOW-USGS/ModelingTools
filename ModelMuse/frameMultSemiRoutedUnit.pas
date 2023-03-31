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
    Splitter1: TSplitter;
    procedure tvSRCollectionsChange(Sender: TObject; Node: TTreeNode);
    procedure seNumberChange(Sender: TObject);
    procedure sbAddClick(Sender: TObject);
    procedure sbDeleteClick(Sender: TObject);
    procedure edSemiRouteNameChange(Sender: TObject);
  private
    FSrItem: TMultiSrdItem;
    FFarm: TFarm;
    FDiversionType: TDiversionType;
//  TSrCollList = TList<TSemiRoutedDeliveriesAndReturnFlowCollection>;
    FSrList: TSrCollList;
    FSfrNode: TTreeNode;
    FFarmNode: TTreeNode;
    function GetMultiSrd(AFarm: TFarm): TMultiSrdCollection;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitializeControls;
    procedure GetData(FarmList: TFarmList; DiversionType: TDiversionType);
    procedure SetData(FarmList: TFarmList; DiversionType: TDiversionType);
    { Public declarations }
  end;

implementation

uses
  System.Generics.Collections;

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

procedure TframeMultSemiRouted.edSemiRouteNameChange(Sender: TObject);
begin
  if FSrItem <> nil then
  begin
    FSrItem.Name := edSemiRouteName.Text;
    Assert(FSfrNode <> nil);
    FSfrNode.Text := FSrItem.Name;
  end;
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
  MultiSrd: TMultiSrdCollection;
begin
  tvSRCollections.Items.Clear;
  FDiversionType := DiversionType;
  FarmNode := nil;
  for FarmIndex := 0 to FarmList.Count - 1 do
  begin
    AFarm := FarmList[FarmIndex];
    FarmNode := tvSRCollections.Items.AddObject(FarmNode, AFarm.FarmName, AFarm);
    MultiSrd := GetMultiSrd(AFarm);

    for SRIndex := 0 to MultiSrd.Count - 1 do
    begin
      SRItem := MultiSrd[SRIndex];
      SRCollection := MultiSrd[SRIndex].SemiRouted;
      tvSRCollections.Items.AddChildObject(FarmNode, SRItem.Name, SRItem);
    end;
  end;
end;

function TframeMultSemiRouted.GetMultiSrd(AFarm: TFarm): TMultiSrdCollection;
begin
  result := nil;
  case FDiversionType of
    dtDiversion:
      begin
        result := AFarm.MultiSrDeliveries;
      end;
    dtReturnFlow:
      begin
        result := AFarm.MultiSrReturns;
      end;
  end;
end;

procedure TframeMultSemiRouted.InitializeControls;
begin
  frameFarmDiversions.InitializeControls
end;

procedure TframeMultSemiRouted.sbAddClick(Sender: TObject);
var
  AFarm: TFarm;
  MultiSrd: TMultiSrdCollection;
  ANode: TTreeNode;
begin
  seNumber.AsInteger := seNumber.AsInteger + 1;
  seNumberChange(nil);
end;

procedure TframeMultSemiRouted.sbDeleteClick(Sender: TObject);
var
  SrdItem: TMultiSrdItem;
  NodeIndex: Integer;
  ANode: TTreeNode;
  NodeList: TList<TTreeNode>;
  AnObject: TObject;
begin
  NodeList := TList<TTreeNode>.Create;
  try
    for NodeIndex := 0 to tvSRCollections.Items.Count - 1 do
    begin
      ANode := tvSRCollections.Items[NodeIndex];
      if ANode.Selected then
      begin
        AnObject := ANode.Data;
        if AnObject is TMultiSrdItem then
        begin
          NodeList.Add(ANode);
        end;
      end;
    end;
    for NodeIndex := 0 to NodeList.Count - 1 do
    begin
      ANode := NodeList[NodeIndex];
      AnObject := ANode.Data;
      AnObject.Free;
      tvSRCollections.Items.Delete(ANode);
    end;
  finally
    NodeList.Free;
  end;
end;

procedure TframeMultSemiRouted.seNumberChange(Sender: TObject);
var
  AFarm: TFarm;
  MultiSrd: TMultiSrdCollection;
  ANode: TTreeNode;
  Item: TMultiSrdItem;
begin
  if (FFarmNode <> nil) then
  begin
    AFarm := FFarmNode.Data;
    MultiSrd := GetMultiSrd(AFarm);
    Assert(seNumber.AsInteger >= 0);
    MultiSrd.Count := seNumber.AsInteger;
    while MultiSrd.Count > FFarmNode.Count do
    begin
      Item := MultiSrd[FFarmNode.Count];
      Item.Name := 'Item ' + IntToStr(FFarmNode.Count+1);
      tvSRCollections.Items.AddChildObject(FFarmNode, Item.Name, Item);
      FFarmNode.Expanded := True;
    end;
    while MultiSrd.Count < FFarmNode.Count do
    begin
      ANode := tvSRCollections.Items[FFarmNode.AbsoluteIndex + FFarmNode.Count];
      tvSRCollections.Items.Delete(ANode);
    end;
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
  ANode: TTreeNode;
  SfrNode: TTreeNode;
  Index: Integer;
  SrItem: TMultiSrdItem;
begin
  if FSrList.Count >= 0 then
  begin
    frameFarmDiversions.SetDataForListOfSemiRoutedLists(FSrList, FDiversionType);
  end;

  frameFarmDiversions.Enabled := False;
  FFarm := nil;
  FSrItem := nil;
  FSfrNode := Nil;
  edSemiRouteName.Enabled := False;
  Controller.Enabled := False;
  if tvSRCollections.SelectionCount = 1 then
  begin
    AnObject := tvSRCollections.Selected.Data;
    if AnObject is TFarm then
    begin
      FFarm := TFarm(AnObject);
      FFarmNode := tvSRCollections.Selected;
      Controller.Enabled := True;
      seNumber.AsInteger := FFarmNode.Count;
      Exit;
    end;
  end;

  FSrList.Clear;
  if tvSRCollections.SelectionCount >= 1 then
  begin
    SfrNode := nil;
    SrItem := nil;
    for Index := 0 to tvSRCollections.Items.Count - 1 do
    begin
      ANode := tvSRCollections.Items[Index];
      if ANode.Selected then
      begin
        AnObject := ANode.Data;
        if AnObject is TMultiSrdItem then
        begin
          SfrNode := ANode;
          SrItem := TMultiSrdItem(AnObject);
          FSrList.Add((AnObject as TMultiSrdItem).SemiRouted);
        end;
      end;
    end;
    if FSrList.Count = 1 then
    begin
      FSfrNode := SfrNode;
      FSrItem := SrItem;
      edSemiRouteName.Enabled := True;
    end;
    if FSrItem <> nil then
    begin
      edSemiRouteName.Text := FSrItem.Name;
    end
    else
    begin
      edSemiRouteName.Text := '';
    end;
    frameFarmDiversions.GetDataFromListOfSemiRoutedLists(FSrList, FDiversionType);
  end;
  frameFarmDiversions.Enabled := FSrList.Count >= 1;

end;

end.
