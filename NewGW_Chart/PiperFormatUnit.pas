unit PiperFormatUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Spin, ExtCtrls, Grids, DataGrid, PiperGraphUnit,
  addbtn95, ComCtrls;

type
  TfrmPiperFormat = class(TForm)
    FontDialog1: TFontDialog;
    ColorDialog1: TColorDialog;
    PageControl1: TPageControl;
    tabGeneral: TTabSheet;
    tabLegend: TTabSheet;
    dgLegend: TECDataGrid;
    Panel1: TPanel;
    seLegendCount: TSpinEdit;
    Label12: TLabel;
    GroupBox2: TGroupBox;
    edTitle: TEdit;
    BitBtn4: TBitBtn;
    gbColors: TGroupBox;
    Label5: TLabel;
    shpCircle: TShape;
    Label6: TLabel;
    shpSquare: TShape;
    Label7: TLabel;
    shpTriangle: TShape;
    Label8: TLabel;
    shpInvertedTriangle: TShape;
    Label9: TLabel;
    shpStar: TShape;
    Label10: TLabel;
    Label11: TLabel;
    shpX: TShape;
    shpCross: TShape;
    shpOpenCircle: TShape;
    Label14: TLabel;
    shpOpenSquare: TShape;
    Label15: TLabel;
    shpOpenTriangle: TShape;
    Label16: TLabel;
    shpOpenInvertedTriangle: TShape;
    Label17: TLabel;
    shpOpenStar: TShape;
    Label18: TLabel;
    btnCircle: TButton;
    btnSquare: TButton;
    btnTriangle: TButton;
    btnInvertedTriangle: TButton;
    btnStar: TButton;
    btnCross: TButton;
    btnX: TButton;
    btnOpenCircle: TButton;
    btnOpenSquare: TButton;
    btnOpenTriangle: TButton;
    btnOpenInvertedTriangle: TButton;
    btnOpenStar: TButton;
    rgColor: TRadioGroup;
    Label1: TLabel;
    Label2: TLabel;
    lblSymbolSize: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label13: TLabel;
    seMargin: TSpinEdit;
    BitBtn3: TBitBtn;
    seLineThickness: TSpinEdit;
    seSymbolRadius: TSpinEdit;
    cbGridLines: TCheckBox;
    seInteriorMargin: TSpinEdit;
    cbShowNumbers: TCheckBox;
    cbSymbolSize: TCheckBox95;
    seMinSymb: TSpinEdit;
    seMaxSymb: TSpinEdit;
    Panel2: TPanel;
    BitBtn5: TBitBtn;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    rgGridLines: TRadioGroup;
    procedure BitBtn3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure btnCircleClick(Sender: TObject);
    procedure seLegendCountChange(Sender: TObject);
    procedure dgLegendDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure dgLegendEditButtonClick(Sender: TObject);
    procedure dgLegendSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure dgLegendSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure rgColorClick(Sender: TObject);
    procedure cbSymbolSizeClick(Sender: TObject);
    procedure cbGridLinesClick(Sender: TObject);
  private
    { Private declarations }
  public
    TextFont : TFont;
    PrintFont : TFont;
    TitleFont : TFont;
    PrintTitleFont : TFont;
    Colors : Array[Low(TGraphSymbol)..High(TGraphSymbol)] of TColor;
    { Public declarations }
  end;

var
  frmPiperFormat: TfrmPiperFormat;

implementation

{$R *.DFM}

procedure TfrmPiperFormat.BitBtn3Click(Sender: TObject);
begin
  FontDialog1.Font := TextFont;
  if FontDialog1.Execute then
  begin
    TextFont.Assign(FontDialog1.Font);
    PrintFont.Assign(FontDialog1.Font);
//    PrintFont.Size := PrintFont.Size * 2;
  end;

end;

procedure TfrmPiperFormat.FormCreate(Sender: TObject);
begin
  TextFont := TFont.Create;
  PrintFont := TFont.Create;
  TitleFont := TFont.Create;
  PrintTitleFont := TFont.Create;
  frmPiperGraph.Colors[gsCircle] := shpCircle.Brush.Color;
  frmPiperGraph.Colors[gsCircleOpen] := shpOpenCircle.Brush.Color;
  frmPiperGraph.Colors[gsSquare] := shpSquare.Brush.Color;
  frmPiperGraph.Colors[gsSquareOpen] := shpOpenSquare.Brush.Color;
  frmPiperGraph.Colors[gsTriangle] := shpTriangle.Brush.Color;
  frmPiperGraph.Colors[gsTriangleOpen] := shpOpenTriangle.Brush.Color;
  frmPiperGraph.Colors[gsInvertedTriangle] := shpInvertedTriangle.Brush.Color;
  frmPiperGraph.Colors[gsInvertedTriangleOpen] := shpOpenInvertedTriangle.Brush.Color;
  frmPiperGraph.Colors[gsStar] := shpStar.Brush.Color;
  frmPiperGraph.Colors[gsStarOpen] := shpOpenStar.Brush.Color;
  frmPiperGraph.Colors[gsCross] := shpCross.Brush.Color;
  frmPiperGraph.Colors[gsX] := shpX.Brush.Color;
  dgLegend.Cells[0,1] := '0';
  dgLegend.Cells[1,1] := frmPiperGraph.DataGrid1.Columns[SymbolIndex].PickList[0];
  PageControl1.ActivePageIndex := 0;
end;

procedure TfrmPiperFormat.FormDestroy(Sender: TObject);
begin
  TextFont.Free;
  PrintFont.Free;
  TitleFont.Free;
  PrintTitleFont.Free;

end;

procedure TfrmPiperFormat.BitBtn4Click(Sender: TObject);
begin
  FontDialog1.Font := TitleFont;
  if FontDialog1.Execute then
  begin
    TitleFont.Assign(FontDialog1.Font);
    PrintTitleFont.Assign(FontDialog1.Font);
//    PrintTitleFont.Size := PrintTitleFont.Size * 2;
  end;

end;

procedure TfrmPiperFormat.btnCircleClick(Sender: TObject);
var
  AShape : TShape;
  ColorIndex : TGraphSymbol;
begin
  AShape := nil;
  ColorIndex := gsCircle;
  if Sender = btnCircle then
  begin
    AShape := shpCircle;
    ColorIndex := gsCircle;
  end
  else if Sender = btnOpenCircle then
  begin
    AShape := shpOpenCircle;
    ColorIndex := gsCircleOpen;
  end
  else if Sender = btnSquare then
  begin
    AShape := shpSquare;
    ColorIndex := gsSquare;
  end
  else if Sender = btnOpenSquare then
  begin
    AShape := shpOpenSquare;
    ColorIndex := gsSquareOpen;
  end
  else if Sender = btnTriangle then
  begin
    AShape := shpTriangle;
    ColorIndex := gsTriangle;
  end
  else if Sender = btnOpenTriangle then
  begin
    AShape := shpOpenTriangle;
    ColorIndex := gsTriangleOpen;
  end
  else if Sender = btnInvertedTriangle then
  begin
    AShape := shpInvertedTriangle;
    ColorIndex := gsInvertedTriangle;
  end
  else if Sender = btnOpenInvertedTriangle then
  begin
    AShape := shpOpenInvertedTriangle;
    ColorIndex := gsInvertedTriangleOpen;
  end
  else if Sender = btnStar then
  begin
    AShape := shpStar;
    ColorIndex := gsStar;
  end
  else if Sender = btnOpenStar then
  begin
    AShape := shpOpenStar;
    ColorIndex := gsStarOpen;
  end
  else if Sender = btnCross then
  begin
    AShape := shpCross;
    ColorIndex := gsCross;
  end
  else if Sender = btnX then
  begin
    AShape := shpX;
    ColorIndex := gsX;
  end
  else
  begin
    Assert(False);
  end;
  ColorDialog1.Color := AShape.Brush.Color;
  if ColorDialog1.Execute then
  begin
    AShape.Brush.Color := ColorDialog1.Color;
  end;
  Colors[ColorIndex] := ColorDialog1.Color;
end;

procedure TfrmPiperFormat.seLegendCountChange(Sender: TObject);
var
  LineIndex : integer;
  ASymbol : string;
begin
  if seLegendCount.Value > 0 then
  begin
    dgLegend.Enabled := True;
    dgLegend.RowCount := seLegendCount.Value + 1;
    dgLegend.Color := clWindow;
  end
  else
  begin
    dgLegend.Enabled := False;
    dgLegend.RowCount := 2;
    dgLegend.Color := clBtnFace;
  end;
  ASymbol :='';
  if dgLegend.Columns[1].PickList.Count > 0 then
  begin
    ASymbol := dgLegend.Columns[1].PickList[0];
  end;
  for LineIndex := 1 to dgLegend.RowCount -1 do
  begin
    if dgLegend.Cells[1,LineIndex] = '' then
    begin
      dgLegend.Cells[1,LineIndex] := ASymbol;
    end;
    if dgLegend.Cells[0,LineIndex] = '' then
    begin
      if rgColor.ItemIndex = 1 then
      begin
        dgLegend.Cells[0,LineIndex] := IntToStr(shpCircle.Brush.Color);
      end
      else
      begin
        dgLegend.Cells[0,LineIndex] := '0';
      end;
    end;
  end;

end;

procedure TfrmPiperFormat.dgLegendDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  AColor : TColor;
  AnInt: integer;
begin
  with dgLegend do
  begin
    if ARow > 0 then
    begin
      if ACol = 0 then
      begin
        with Canvas do
        begin
          if rgColor.ItemIndex = 0 then
          begin
            AColor := clBlack;
          end
          else
          begin
            if TryStrToInt(Cells[ACol, ARow], AnInt) then
            begin
              AColor := AnInt;
            end
            else
            begin
              AColor := clBlack;
            end;
          end;
          Brush.Color := AColor;
          FillRect(Rect);
        end;
      end;
    end;
  end;

end;

procedure TfrmPiperFormat.dgLegendEditButtonClick(Sender: TObject);
begin
  ColorDialog1.Color := StrToInt(dgLegend.Cells[dgLegend.Col,dgLegend.Row]);
  if ColorDialog1.Execute then
  begin
    dgLegend.Cells[dgLegend.Col,dgLegend.Row] := IntToStr(ColorDialog1.Color);
  end;
end;

procedure TfrmPiperFormat.dgLegendSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  if (ACol = 0) and (rgColor.ItemIndex <> 2) then
  begin
    CanSelect := False;
  end;
end;

procedure TfrmPiperFormat.dgLegendSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
const
  Epsilon = 1e-8;
var
  ColorIndex : integer;
  ColorSymbolIndex : TGraphSymbol;
  AColor : TColor;
begin
  With dgLegend do
  begin
    if (ACol = 1) and (rgColor.ItemIndex <> 2) then
    begin
      ColorIndex := Columns[1].Picklist.IndexOf(Cells[ACol,ARow]);
      ColorSymbolIndex := TGraphSymbol(ColorIndex);
      AColor := Colors[ColorSymbolIndex];
      Cells[0,ARow] := IntToStr(AColor);
    end;
    Invalidate;
  end;
end;

procedure TfrmPiperFormat.rgColorClick(Sender: TObject);
var
  RowIndex: integer;
begin
  btnCircle.Enabled := rgColor.ItemIndex = 1;
  btnOpenCircle.Enabled := rgColor.ItemIndex = 1;
  btnSquare.Enabled := rgColor.ItemIndex = 1;
  btnOpenSquare.Enabled := rgColor.ItemIndex = 1;
  btnTriangle.Enabled := rgColor.ItemIndex = 1;
  btnOpenTriangle.Enabled := rgColor.ItemIndex = 1;
  btnInvertedTriangle.Enabled := rgColor.ItemIndex = 1;
  btnOpenInvertedTriangle.Enabled := rgColor.ItemIndex = 1;
  btnStar.Enabled := rgColor.ItemIndex = 1;
  btnOpenStar.Enabled := rgColor.ItemIndex = 1;
  btnCross.Enabled := rgColor.ItemIndex = 1;
  btnX.Enabled := rgColor.ItemIndex = 1;
  for RowIndex := 1 to dgLegend.RowCount -1 do
  begin
    dgLegendSetEditText(dgLegend, 1, RowIndex, dgLegend.Cells[1, RowIndex]);
  end;
end;

procedure TfrmPiperFormat.cbSymbolSizeClick(Sender: TObject);
begin
  seMinSymb.Enabled := cbSymbolSize.Checked;
  seMaxSymb.Enabled := cbSymbolSize.Checked;
  seSymbolRadius.Enabled := not cbSymbolSize.Checked;
end;

procedure TfrmPiperFormat.cbGridLinesClick(Sender: TObject);
begin
  rgGridLines.Enabled := cbGridLines.Checked;
end;

end.
