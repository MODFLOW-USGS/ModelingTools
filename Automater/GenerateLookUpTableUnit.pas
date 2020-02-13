unit GenerateLookUpTableUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TForm3 = class(TForm)
    btn1: TButton;
    dlgSave1: TSaveDialog;
    pb1: TProgressBar;
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  HantushUnit, RealListUnit;

{$R *.dfm}

procedure TForm3.btn1Click(Sender: TObject);
const
  Precison = 4e-9;
//  TableMax = 46;
//  Values: array[0..TableMax-1] of double =
//    (0, 0.02, 0.04, 0.06, 0.08, 0.1, 0.14,
//    0.18, 0.22, 0.26, 0.30, 0.34, 0.38, 0.42, 0.46, 0.50, 0.54, 0.58, 0.62,
//    0.66, 0.70, 0.74, 0.78, 0.82, 0.86, 0.90, 0.94, 0.98,
//    1.00, 1.05, 1.1, 1.15, 1.2, 1.25, 1.3, 1.35, 1.4, 1.45, 1.5, 1.6, 1.7, 1.8, 2.0, 2.2, 2.5, 3);
var
  Lines: TStringList;
  AlphaIndex: Integer;
  Alpha: Double;
  BetaIndex: Integer;
  Beta: Double;
  AValue: Double;
  StringB: TStringBuilder;
  ValIndex: Integer;
  RealList: TRealList;
  Index: Integer;
  Value: double;
  TableMax: integer;
begin
  if dlgSave1.Execute then
  begin
    Lines := TStringList.Create;
    StringB := TStringBuilder.Create;
    RealList := TRealList.Create;
    try
      for Index := 0 to 50 do
      begin
        RealList.Add(2*Index/100);
      end;
      for Index := 0 to 1000 do
      begin
        Value := 1 + Index/40;
        RealList.Add(Value);
        if Value >= 3 then
        begin
          Break;
        end;
      end;
      TableMax := RealList.Count;
      pb1.Max := Sqr(TableMax);
      Lines.Add('unit S_StarValues;');
      Lines.Add('');
      Lines.Add('interface');
      Lines.Add('');
      Lines.Add('const');
      Lines.Add(Format('  TableMax = %d;', [TableMax]));
      Lines.Add('  Values: array[0..TableMax-1] of double = ');
      StringB.Clear;
      StringB.Capacity := 300;
      StringB.Append('    (');
      for ValIndex := 0 to RealList.Count - 1 do
      begin
        StringB.Append(RealList[ValIndex]);
        if ValIndex < RealList.Count - 1 then
        begin
          StringB.Append(', ');
        end;
        if ((ValIndex+1) mod 10) = 0 then
        begin
          Lines.Add(StringB.ToString);
          StringB.Clear;
          StringB.Capacity := 300;
          StringB.Append('     ')
        end;
      end;
      StringB.Append(');');
      Lines.Add(StringB.ToString);
//
//
//      Lines.Add('    (0, 0.02, 0.04, 0.06, 0.08, 0.1, 0.14,');
//      Lines.Add('    0.18, 0.22, 0.26, 0.30, 0.34, 0.38, 0.42, 0.46, 0.50, 0.54, 0.58, 0.62,');
//      Lines.Add('    0.66, 0.70, 0.74, 0.78, 0.82, 0.86, 0.90, 0.94, 0.98,');
//      Lines.Add('    1.00, 10.5, 1.1, 1.15, 1.2, 1.25, 1.3, 1.35, 1.4, 1.45, 1.5, 1.6, 1.7, 1.8, 2.0, 2.2, 2.5, 3);');
      Lines.Add('  SValues: array[0..TableMax-1,0..TableMax-1] of double =');
      Lines.Add('    (');
      for AlphaIndex := 0 to RealList.Count - 1 do
      begin
        Alpha := RealList[AlphaIndex];
        StringB.Clear;
        StringB.Capacity := 300;
        StringB.Append('    (');
        for BetaIndex := 0 to RealList.Count - 1 do
        begin
          Beta := RealList[BetaIndex];
          AValue := IntS(Alpha,Beta, Precison);
          StringB.Append(AValue);
          if BetaIndex < RealList.Count - 1 then
          begin
            StringB.Append(', ');
          end;
          if ((BetaIndex+1) mod 10) = 0 then
          begin
            Lines.Add(StringB.ToString);
            StringB.Clear;
            StringB.Capacity := 300;
            StringB.Append('     ')
          end;
          pb1.StepIt;
          Application.ProcessMessages;
        end;
        StringB.Append(')');
        if AlphaIndex < RealList.Count - 1 then
        begin
          StringB.Append(',');
        end;
        Lines.Add(StringB.ToString);
      end;
      Lines.Add('    );');
      Lines.Add('');
      Lines.Add('implementation');
      Lines.Add('');
      Lines.Add('end.');
      Lines.SaveToFile(dlgSave1.FileName);
    finally
      Lines.Free;
      StringB.Free;
      RealList.Free;
    end;
  end;
  ShowMessage('Done');
end;

end.
