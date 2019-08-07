unit frameScreenObjectMt3dSftUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectNoParamUnit, Vcl.Grids,
  RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, Vcl.ComCtrls, UndoItemsScreenObjects, JvToolEdit;

type
  TframeScreenObjectMt3dSft = class(TframeScreenObjectNoParam)
    pgcSft: TPageControl;
    tsSteady: TTabSheet;
    tsHeadWaters: TTabSheet;
    tsPrecipitation: TTabSheet;
    tsRunoff: TTabSheet;
    tsConstantConcentration: TTabSheet;
    pnlBottomPrecip: TPanel;
    lblNumberOfTimesPrecip: TLabel;
    seNumberOfTimesPrecip: TJvSpinEdit;
    btnDeletePrecip: TBitBtn;
    btnInsertPrecip: TBitBtn;
    pnlPrecip: TPanel;
    pnlPrecipFormula: TPanel;
    lblPrecipFormula: TLabel;
    rdePrecipFormula: TRbwDataEntry;
    rdgPrecip: TRbwDataGrid4;
    pnlBottomRunoff: TPanel;
    lblNumberOfTimesRunoff: TLabel;
    seNumberOfTimesRunoff: TJvSpinEdit;
    btnDeleteRunoff: TBitBtn;
    btnInsertRunoff: TBitBtn;
    pnlRunoff: TPanel;
    pnlFormulaRunoff: TPanel;
    lblFormulaRunoff: TLabel;
    rdeFormulaRunoff: TRbwDataEntry;
    rdgRunoff: TRbwDataGrid4;
    pnlBottomConstConc: TPanel;
    lblNumberOfTimesConstConc: TLabel;
    seNumberOfTimesConstConc: TJvSpinEdit;
    btnDeleteConstConc: TBitBtn;
    btnInsertConstConc: TBitBtn;
    pnlGridConstConc: TPanel;
    pnlFormulaConstConc: TPanel;
    lblFormulaConstConc: TLabel;
    rdeFormulaConstConc: TRbwDataEntry;
    rdgConstConc: TRbwDataGrid4;
    edInitialConcentration: TJvComboEdit;
    lblInitialConcentration: TLabel;
    edDispersion: TJvComboEdit;
    lblDispersion: TLabel;
  private
    { Private declarations }
  public
    procedure GetData(ScreenObjectList: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    { Public declarations }
  end;

var
  frameScreenObjectMt3dSft: TframeScreenObjectMt3dSft;

implementation

{$R *.dfm}

{ TframeScreenObjectMt3dSft }

procedure TframeScreenObjectMt3dSft.GetData(
  ScreenObjectList: TScreenObjectEditCollection);
begin
  pnlBottom.Parent := tsHeadWaters;
  pnlGrid.Parent := tsHeadWaters;
  pgcSft.Align := alClient;

end;

procedure TframeScreenObjectMt3dSft.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
begin

end;

end.
