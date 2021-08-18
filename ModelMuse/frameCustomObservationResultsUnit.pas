unit frameCustomObservationResultsUnit;

interface

uses
  GoPhastTypes, OrderedCollectionUnit,
  PhastModelUnit,
  frmGoPhastUnit, Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Grids, RbwDataGrid4, JvSpin, JvExControls,
  JvColorBox, JvColorButton, frameDisplayLimitUnit, Vcl.Mask, JvExMask,
  JvToolEdit, QuadTreeClass;

type
  TframeCustomObservationResults = class abstract(TFrame)
    pnlBottom: TPanel;
    lblRMS: TLabel;
    comboModels: TComboBox;
    pgcObservations: TPageControl;
    tabControls: TTabSheet;
    lblNegativeColor: TLabel;
    lblColorPositive: TLabel;
    lblMaxSymbolSize: TLabel;
    lblHeadObsResults: TLabel;
    flnmedHeadObsResults: TJvFilenameEdit;
    grpbxFilter: TGroupBox;
    lblMaximumTime: TLabel;
    lblMaxResidual: TLabel;
    lblMinimumTime: TLabel;
    lblMinResidual: TLabel;
    lblMinLayer: TLabel;
    lblMaxLayer: TLabel;
    framelmtMinimumTime: TframeDisplayLimit;
    framelmtMaxResidual: TframeDisplayLimit;
    framelmtMaximumTime: TframeDisplayLimit;
    framelmtMinResidual: TframeDisplayLimit;
    framelmtMinLayer: TframeDisplayLimit;
    framelmtMaxLayer: TframeDisplayLimit;
    clrbtnNegative: TJvColorButton;
    clrbtnPositive: TJvColorButton;
    spinSymbolSize: TJvSpinEdit;
    cbShow: TCheckBox;
    tabValues: TTabSheet;
    rdgHeadObs: TRbwDataGrid4;
    pnlValueControls: TPanel;
    btnCopy: TButton;
    btnHightlightObjects: TButton;
    btnRestore: TButton;
    tabLegend: TTabSheet;
    shpMax: TShape;
    shpHalfMax: TShape;
    lblMax: TLabel;
    lblHalfMax: TLabel;
    tabGraph: TTabSheet;
    pbHeadObs: TPaintBox;
    pnlGraphControls: TPanel;
    lblGraphInstructions: TLabel;
    rgGraphType: TRadioGroup;
    qtreeObservations: TRbwQuadTree;
  public
    procedure UpdateChildModels;
  protected
    procedure UpdateObsLinkList; virtual; abstract;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TframeCustomObservationResults.UpdateChildModels;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
  LocalModel: TPhastModel;
  Index: Integer;
  ModelList: TList;
  APointer: TObject;
begin
  ModelList := TList.Create;
  try
    ModelList.Add(frmGoPhast.PhastModel);
    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ModelList.Add(frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel);
    end;
    for Index := comboModels.Items.Count - 1 downto 0 do
    begin
      APointer := comboModels.Items.Objects[Index];
      if ModelList.IndexOf(APointer) < 0 then
      begin
        comboModels.Items.Delete(Index);
      end;
    end;
  finally
    ModelList.Free;
  end;

  UpdateObsLinkList;

  LocalModel := frmGoPhast.PhastModel;
  // comboModels.Clear;
  if comboModels.Items.IndexOfObject(LocalModel) < 0 then
  begin
    comboModels.Items.InsertObject(0, LocalModel.DisplayName, LocalModel);
  end;
  if LocalModel.LgrUsed then
  begin
    comboModels.Visible := True;
    for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
    begin
      ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
      if comboModels.Items.IndexOfObject(ChildModel) < 0 then
      begin
        comboModels.Items.InsertObject(ChildIndex + 1, ChildModel.DisplayName,
          ChildModel);
      end;
    end;
  end
  else
  begin
    comboModels.Visible := False;
  end;
  comboModels.ItemIndex := 0;
end;

end.
