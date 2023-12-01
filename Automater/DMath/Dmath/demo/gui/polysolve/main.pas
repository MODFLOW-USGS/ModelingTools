unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, Grids, ValEdit;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label4: TLabel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    StringGrid1: TStringGrid;
    StringGrid2: TStringGrid;
    Button1: TButton;
    Button2: TButton;
    Button5: TButton;
    Button3: TButton;
    Button4: TButton;
    Button6: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    GroupBox1: TGroupBox;

    procedure FormActivate(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);

  private
    procedure LabelCoef;
    procedure ClearRoots;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  { DMath units }
  utypes, umath, urootpol, upolutil, ucomplex, ustrings,
  { Dialogs }
  Format;

procedure TForm1.LabelCoef;
var
  I : Integer;
begin
  with StringGrid1 do
    begin
      RowCount := SpinEdit1.Value + 1;
      for I := 0 to RowCount do
        Cells[0, I] := 'a(' + IntToStr(I) + ')';
    end;
end;

procedure TForm1.ClearRoots;
var
  I : Integer;
begin
  with StringGrid2 do
    begin
      RowCount := SpinEdit1.Value + 1;
      for I := 1 to RowCount do
        begin
          Cells[0, I] := '';
          Cells[1, I] := '';
        end;
    end;
end;

procedure TForm1.FormActivate(Sender: TObject);
const
  CRLF  = #10#13;
  CRLF2 = CRLF + CRLF;
begin
  SpinEdit1.Value := 6;

  LabelCoef;

  with StringGrid1 do
    begin
      Cells[1, 0] :=   '720';
      Cells[1, 1] := '-1764';
      Cells[1, 2] :=  '1624';
      Cells[1, 3] :=  '-735';
      Cells[1, 4] :=   '175';
      Cells[1, 5] :=   '-21';
      Cells[1, 6] :=     '1';

      ColWidths[0] := 40;
      ColWidths[1] := Width - ColWidths[0];
    end;

  with StringGrid2 do
    begin
      RowCount := SpinEdit1.Value + 1;
      Cells[0, 0] := 'x Real';
      Cells[1, 0] := 'x Imag.';
      Cells[2, 0] := 'P(x) Real';
      Cells[3, 0] := 'P(x) Imag.';
    end;

  Label5.Caption := 'A complex root (x + i y)' + CRLF2 +
                    'is considered real if' + CRLF2 +
                    'abs(y / x) < 10^';

  SpinEdit2.Value := Trunc(Log10(MachEp));
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  LabelCoef;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  F      : TextFile;
  Deg, I : Integer;
  Coef   : Float;
begin
  if not OpenDialog1.Execute then Exit;

  AssignFile(F, OpenDialog1.FileName);
  Reset(F);

  Readln(F, Deg);
  with SpinEdit1 do
    if not (Deg in [MinValue..MaxValue]) then
      begin
        MessageDlg('Degree must be between ' + IntToStr(MinValue) +
                   ' and'  + IntToStr(MaxValue), mtError, [mbOK], 0);
        Exit;
      end;

  SpinEdit1.Value := Deg;
  LabelCoef;

  for I := 0 to Deg do
    begin
      Readln(F, Coef);
      StringGrid1.Cells[1, I] := FloatToStr(Coef);
    end;

  CloseFile(F);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  F      : TextFile;
  Deg, I : Integer;
begin
  if not SaveDialog1.Execute then Exit;

  AssignFile(F, SaveDialog1.FileName);
  Rewrite(F);

  Deg := SpinEdit1.Value;
  Writeln(F, Deg);

  for I := 0 to Deg do
    Writeln(F, StringGrid1.Cells[1, I]);

  CloseFile(F);
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  Deg, I : Integer;
  Coef   : TVector;
  Root   : TCompVector;
  Eps    : Float;
  P      : Complex;
begin
  ClearRoots;

  Deg := SpinEdit1.Value;

  DimVector(Coef, Deg);
  DimCompVector(Root, Deg);

  { Read coefficients }
  for I := 0 to Deg do
    Coef[I] := StrToFloat(StringGrid1.Cells[1, I]);

  { Solve polynomial }
  if RootPol(Coef, Deg, Root) < 0 then
    begin
      Label4.Font.Color := clRed;
      Label4.Caption := 'Unable to solve !';
      Button3.Enabled := False;
      Exit;
    end
  else
    begin
      Label4.Font.Color := clLime;
      Label4.Caption := 'Roots';
      Button3.Enabled := True;
    end;

  { Set the small imaginary parts to zero }
  Eps := Exp10(SpinEdit2.Value);
  SetRealRoots(Deg, Root, Eps);

  { Sort roots: first real roots, in ascending order,
    then complex roots (unordered)                    }
  SortRoots(Deg, Root);

  { Display roots and polynomial values }
  with StringGrid2 do
    for I := 1 to Deg do
      begin
        P := CPoly(Root[I], Coef, Deg);
        Cells[0, I] := FloatStr(Root[I].X);
        Cells[2, I] := FloatStr(P.X);
        if Abs(Root[I].Y) > 0.0 then
          begin
            Cells[1, I] := FloatStr(Root[I].Y);
            Cells[3, I] := FloatStr(P.Y);
          end;
      end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  F      : TextFile;
  Deg, I : Integer;
begin
  if not SaveDialog2.Execute then Exit;

  AssignFile(F, SaveDialog2.FileName);
  Rewrite(F);

  Deg := SpinEdit1.Value;
  Writeln(F, Deg);

  with StringGrid2 do
    for I := 1 to Deg do
      begin
        Write(F, Cells[0, I], #9);
        if Cells[1, I] = '' then
          Writeln(F, '0')
        else
          Writeln(F, Cells[1, I]);
      end;

  CloseFile(F);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  FormatDlg.ShowModal;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  Form1.Close;
end;

end.
