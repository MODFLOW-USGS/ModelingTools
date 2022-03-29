unit frmGeneratePointsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, QuadTreeClass;

type
  TForm1 = class(TForm)
    SaveDialog1: TSaveDialog;
    RbwQuadTree1: TRbwQuadTree;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.Generics.Collections;



{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
const
  UltimateGoal = 1000000;
var
  Goal: Integer;
  X: Double;
  Y: Double;
  Value: Double;
  Dummy: Pointer;
  NearX: Double;
  NearY: Double;
  Lines: TStringList;
  FileRoot: string;
  FileName: string;
  Multipliers: TList<Integer>;
  GoalIndex: Integer;
  CurrentGoal: Integer;
begin
  if SaveDialog1.Execute then
  begin
    FileRoot := ChangeFileExt(SaveDialog1.FileName, '');
    Lines := TStringList.Create;
    Multipliers := TList<Integer>.Create;
    try
      Multipliers.Add(1);
      Multipliers.Add(2);
      Multipliers.Add(3);
      Multipliers.Add(4);
      Multipliers.Add(5);
      Multipliers.Add(6);
      Goal := 100;
      Randomize;
      while Goal <= UltimateGoal do
      begin
        X := Random * 102 -1;
        Y := Random * 102 -1;
        Value := Random;
        RbwQuadTree1.AddPoint(X, Y, Dummy);
        Lines.Add(Format('%0:g'#9'%1:g'#9'%2:g', [X, Y, Value]));
        for GoalIndex := 0 to Multipliers.Count - 1 do
        begin
          CurrentGoal := Goal * Multipliers[GoalIndex];
          while Lines.Count < CurrentGoal do
          begin
            X := Random * 102 -1;
            Y := Random * 102 -1;
            NearX := X;
            NearY := Y;
            Value := Random;
            RbwQuadTree1.FirstNearestPoint(NearX, NearY, Dummy);
            if (NearX <> X) or (NearY <> Y) then
            begin
              Lines.Add(Format('%0:g'#9'%1:g'#9'%2:g', [X, Y, Value]));
              RbwQuadTree1.AddPoint(X, Y, Dummy);
            end;
          end;
          FileName := FileRoot + CurrentGoal.ToString + '.txt';
          Lines.SaveToFile(FileName);
          if CurrentGoal >= UltimateGoal then
          begin
            break;
          end;
        end;
        Goal := Goal * 10;
      end;
    finally
      Lines.Free;
      Multipliers.Free;
    end;
    ShowMessage('Done');
  end;
end;

end.
