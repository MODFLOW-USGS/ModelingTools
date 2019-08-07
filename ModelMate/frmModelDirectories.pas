unit frmModelDirectories;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, JvExMask, JvToolEdit, Buttons,
  GlobalBasicData, GlobalData, GlobalTypesUnit, Utilities;

type
  TFormModelDir = class(TForm)
    Label1: TLabel;
    dedModelDir: TJvDirectoryEdit;
    lblPredModelDir: TLabel;
    dedPredModelDir: TJvDirectoryEdit;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    procedure btnOKClick(Sender: TObject);
    procedure dedModelDirChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure dedPredModelDirChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    LocalChange: boolean;
    procedure GetData;
    procedure SetData;
    procedure HighlightControls;
  public
    { Public declarations }
  end;

var
  FormModelDir: TFormModelDir;

implementation

{$R *.dfm}

{ TFormModelDir }

procedure TFormModelDir.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
  if LocalChange then
    ProjChanged := True;
end;

procedure TFormModelDir.dedModelDirChange(Sender: TObject);
begin
  inherited;
  HighlightControls;
end;

procedure TFormModelDir.dedPredModelDirChange(Sender: TObject);
begin
  HighlightControls;
end;

procedure TFormModelDir.FormCreate(Sender: TObject);
begin
  CenterForm(self);
end;

procedure TFormModelDir.FormShow(Sender: TObject);
begin
  LocalChange := False;
  GetData;
  HighlightControls;
end;

procedure TFormModelDir.GetData;
begin
  dedModelDir.InitialDir := PCurrent.AbsModelDirectory(muCalib);
  dedModelDir.Text := PCurrent.AbsModelDirectory(muCalib);
  dedPredModelDir.InitialDir := PCurrent.AbsModelDirectory(muPred);
  dedPredModelDir.Text := PCurrent.AbsModelDirectory(muPred);
end;

procedure TFormModelDir.HighlightControls;
var
  DirDoesExist, OK: Boolean;
  I: integer;
  dedTemp: TJvDirectoryEdit;
  PredictionsDefined : Boolean;
begin
  if PCurrent.PredSet.Count > 0 then
    PredictionsDefined := True
  else
    PredictionsDefined := False;
  OK := True;
  for I := 1 to 2 do
    begin
      case I of
        // pointer assignments
        1: dedTemp := dedModelDir;
        2: dedTemp := dedPredModelDir;
      else
        dedTemp := dedModelDir;
      end;
      DirDoesExist := DirectoryExists(dedTemp.Text);
      if DirDoesExist then
        begin
          dedTemp.Color := clWindow;
        end
      else
        begin
          if I = 2 then
            if PredictionsDefined then
              begin
                dedTemp.Color := clRed;
                OK := False;
              end
          else
            begin
                dedTemp.Color := clRed;
                OK := False;
            end;
        end;
    end;
  btnOK.Enabled := OK;
end;

procedure TFormModelDir.SetData;
begin
  if dedModelDir.Text <> PCurrent.ModelDirectory then
    begin
      PCurrent.ModelDirectory := dedModelDir.Text;
      LocalChange := True;
    end;
  if dedPredModelDir.Text <> PCurrent.ModelDirectoryPred then
    begin
      PCurrent.ModelDirectoryPred := dedPredModelDir.Text;
      LocalChange := True;
    end;
end;

end.
