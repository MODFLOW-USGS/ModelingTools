{ **********************************************************************
  This program demonstrates the behavior of a dynamical system: the
  quadratic iterator (also known as the logistic sequence).

  The system is defined by the relationship:

                       x(n+1) = a.x(n).[1 - x(n)]

  According to the value of a the sequence may be:

    * periodic (e.g. for a = 3.5 the period is 4)
    * chaotic (e.g. a = 4)
    * presenting an alternance of periodic and chaotic phases,
       a phenomenon known as intermittency (e.g. a = 3.82812)

  The program plots a "bifurcation" diagram, in which the values of x(n)
  are plotted for each value of a (after an initial run of some hundred
  iterations). The bifurcation diagram shows the progressive transition
  from order to chaos via a period-doubling route as a increases. It also
  shows the existence of periodic regions inside the chaotic domain.
  Magnifying these regions reveals a structure similar to the whole
  plot (i.e. the fractal nature of the bifurcation diagram).

  For more information about the quadratic iterator and other chaotic
  systems, see "The Chaos Hypertextbook" (http://hypertextbook.com/chaos/)
  by Glenn Elert.
  ********************************************************************** }

unit bifur1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;

    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;

    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;

    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;

    CheckBox1: TCheckBox;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;

    Image1: TImage;

    SaveDialog1: TSaveDialog;

    procedure FormActivate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    procedure ClearGraphics;
    procedure PlotGraph(Canvas : TCanvas);
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  utypes, umath, uwinplot, GraphOpt;

procedure TForm1.ClearGraphics;
begin
  with Form1.Image1 do
    begin
      Canvas.Brush.Color := GraphOptDlg.BorderColorShape.Brush.Color;
      Canvas.FillRect(Rect(0, 0, Width, Height));
      Canvas.Brush.Color := GraphOptDlg.GraphColorShape.Brush.Color;
      Canvas.FillRect(Rect(GraphOptDlg.XminSpinEdit.Value * Width  div 100,
                           GraphOptDlg.YminSpinEdit.Value * Height div 100,
                           GraphOptDlg.XmaxSpinEdit.Value * Width  div 100,
                           GraphOptDlg.YmaxSpinEdit.Value * Height div 100));
    end;
end;

procedure TForm1.PlotGraph(Canvas : TCanvas);
begin
  ClearGraphics;

  InitGraphics(Image1.Width, Image1.Height);

  Canvas.Brush.Color := GraphOptDlg.GraphColorShape.Brush.Color;
  Canvas.Pen.Color := GraphOptDlg.AxisColorShape.Brush.Color;
  Canvas.Pen.Width := GraphOptDlg.AxisWidthSpinEdit.Value;

  SetWindow(Canvas,
            GraphOptDlg.XminSpinEdit.Value,
            GraphOptDlg.XmaxSpinEdit.Value,
            GraphOptDlg.YminSpinEdit.Value,
            GraphOptDlg.YmaxSpinEdit.Value,
            GraphOptDlg.GraphBorderCheckBox.Checked);

  Canvas.Brush.Color := GraphOptDlg.BorderColorShape.Brush.Color;
  Canvas.Font := GraphOptDlg.AxesFontDialog.Font;

  if GraphOptDlg.XplotCheckBox.Checked then PlotOxAxis(Canvas);
  if GraphOptDlg.YplotCheckBox.Checked then PlotOyAxis(Canvas);

  PlotGrid(Canvas, TGrid(GraphOptDlg.GridRadioGroup.ItemIndex));

  Canvas.Font := GraphOptDlg.TitleFontDialog.Font;
  WriteGraphTitle(Canvas);
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  { Hide the graphic options which are not relevant }
  with GraphOptDlg do
    begin
      XScaleCheckBox.Visible := False;
      StepGroupBox.Visible := False;
      LineGroupBox.Visible := False;
      PointComboBox.Visible := False;
      Label14.Visible := False;
      PointSizeSpinEdit.Visible := False;
    end;

  SetOxScale(LinScale, 1, 4, 1);
  SetOyScale(LinScale, 0, 1, 0.2);

  SetOxTitle('a');
  SetOyTitle('x');

  SetMaxCurv(1);

  Randomize;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  Edit3.Enabled := not CheckBox1.Checked;
end;

procedure TForm1.Button1Click(Sender: TObject);
{ Bifurcation diagram }
var
  N_hide, N_show    : Integer;  { Number of points (hidden and shown) }
  A1, A2            : Float;    { Range of 'a' values (1 <= A1 < A2 <= 4) }
  X0                : Float;    { Initial 'x' value }
  I, J              : Integer;
  A, X, Y           : Float;
  XScale, YScale    : TScale;
  Xmin, Xmax, Xstep : Float;
  Ymin, Ymax, Ystep : Float;
  Color             : TColor;

begin
  A1 := StrToFloat(Edit1.Text);
  A2 := StrToFloat(Edit2.Text);

  if (A1 < 1.0) or (A1 > 4.0) or (A2 < 1.0) or (A2 > 4.0) or (A1 >= A2) then
    begin
      MessageDlg('a1 and a2 must be such that: 1 <= a1 < a2 <= 4', mtError, [mbOk], 0);
      Exit;
    end;

  if CheckBox1.Checked then
    X0 := Random
  else
    X0 := StrToFloat(Edit3.Text);

  if (X0 <= 0) or (X0 >= 1.0) then
    begin
      MessageDlg('x0 must be in (0, 1)', mtError, [mbOk], 0);
      Exit;
    end;

  N_hide := SpinEdit1.Value;
  N_show  := SpinEdit2.Value;

  GetOxScale(XScale, Xmin, Xmax, Xstep);
  GetOyScale(YScale, Ymin, Ymax, Ystep);

  Color := GraphOptDlg.PointColorShape.Brush.Color;

  PlotGraph(Image1.Canvas);

  for I := Xpixel(A1) to Xpixel(A2) do
    begin
      A := Xuser(I);
      if (A >= Xmin) and (A <= Xmax) then
        begin
          X := X0;
          for J := 1 to N_hide do
            X := A * X * (1.0 - X);
          for J := 1 to N_show do
            begin
              X := A * X * (1.0 - X);
              if YScale = LinScale then Y := X else Y := Log10(X);
              if (Y >= Ymin) and (Y <= Ymax) then
                Image1.Canvas.Pixels[I, Ypixel(Y)] := Color;
            end;
        end;
    end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  GraphOptDlg.ShowModal;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    Image1.Picture.SaveToFile(SaveDialog1.FileName);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Form1.Close;
end;

end.
