{ @abstract(@name defines @link(TframeIface).  @link(TframeIface) is used to
 define the IFACE for a boundary condition in MODPATH.)
@author(Richard B. Winston <rbwinst@usgs.gov>)
}
unit frameIfaceUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvExStdCtrls, JvRadioButton, ExtCtrls, JvExExtCtrls,
  JvImage, GLWin32Viewer, GLScene, GLGeomObjects, GLObjects, GLColor,
  GoPhastTypes, GLCoordinates, GLCrossPlatform, GLMaterial,
{$IF CompilerVersion < 23}
  BaseClasses;
{$ELSE}
  GLBaseClasses;
{$IFEND}

type
  TframeIface = class(TFrame)
    gbIface: TGroupBox;
    rbBottom: TJvRadioButton;
    rbFront: TJvRadioButton;
    rbRight: TJvRadioButton;
    rbLeft: TJvRadioButton;
    rbTop: TJvRadioButton;
    rbBack: TJvRadioButton;
    rbInternal: TJvRadioButton;
    rbHorizontal: TJvRadioButton;
    glsIface: TGLScene;
    GLDummyCube1: TGLDummyCube;
    LeftFace: TGLPlane;
    GLCube1: TGLCube;
    GLLightSource2: TGLLightSource;
    GLSphere1: TGLSphere;
    FrontFace: TGLPlane;
    TopFace: TGLPlane;
    BottomFace: TGLPlane;
    RightFace: TGLPlane;
    BackFace: TGLPlane;
    CentralSphere: TGLSphere;
    Tube1: TGLCylinder;
    Tube2: TGLCylinder;
    Tube3: TGLCylinder;
    GLCamera1: TGLCamera;
    glsvViewer: TGLSceneViewer;
    lblMessage: TLabel;
    procedure rbHorizontalClick(Sender: TObject);
  private
    FIFace: TIface;
    FSettingIFace: Boolean;
    procedure SetIFace(const Value: TIface);
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    property IFACE: TIface read FIFace write SetIFace;
    { Public declarations }
  end;

implementation

uses GLTexture;

{$R *.dfm}

constructor TframeIface.Create(AOwner: TComponent);
begin
  inherited;
  FSettingIFace := False;
  IFACE := iInternal;
end;

procedure TframeIface.rbHorizontalClick(Sender: TObject);
var
  RadioButton: TRadioButton;
begin
  RadioButton := Sender as TRadioButton;
  if RadioButton.Checked then
  begin
    IFACE := TIface(RadioButton.Tag + 2);
  end;
end;

procedure TframeIface.SetIFace(const Value: TIface);
var
  List: TList;
  RadioButton: TRadioButton;
  Index: Integer;
  SelectedFaces: TList;
  AllFaces: TList;
  AFace: TGLPlane;
begin
  if FSettingIFace then
  begin
    Exit;
  end;

  FSettingIFace := True;
  FIFace := Value;
  SelectedFaces := TList.Create;
  AllFaces := TList.Create;
  List := TList.Create;
  try
    List.Add(nil);
    List.Add(rbHorizontal);
    List.Add(rbInternal);
    List.Add(rbLeft);
    List.Add(rbRight);
    List.Add(rbFront);
    List.Add(rbBack);
    List.Add(rbBottom);
    List.Add(rbTop);

    AllFaces.Add(TopFace);
    AllFaces.Add(BottomFace);
    AllFaces.Add(FrontFace);
    AllFaces.Add(BackFace);
    AllFaces.Add(LeftFace);
    AllFaces.Add(RightFace);

    RadioButton := List[Ord(FIFace)];

    if RadioButton = nil then
    begin
      for Index := 0 to List.Count - 1 do
      begin
        RadioButton := List[Index];
        if RadioButton <> nil then
        begin
          RadioButton.Checked := False;
        end;
      end;
    end
    else
    begin
      Assert(RadioButton.tag = Ord(FIFace) -2);
      RadioButton.Checked:= True;
    end;

    if IFACE in [iHorizontal,iRight,iFront,iTop] then
    begin
      FrontFace.Visible := True;
      TopFace.Visible := True;
      RightFace.Visible := True;
      Tube1.Visible := False;
      Tube2.Visible := False;
      Tube3.Visible := False;
    end
    else
    begin
      FrontFace.Visible := False;
      TopFace.Visible := False;
      RightFace.Visible := False;
      Tube1.Visible := True;
      Tube2.Visible := True;
      Tube3.Visible := True;
    end;
    case IFACE of
      iIndeterminant: ;
      iHorizontal:
        begin
          SelectedFaces.Add(FrontFace);
          SelectedFaces.Add(RightFace);
        end;
      iInternal: ;
      iLeft: SelectedFaces.Add(LeftFace);
      iRight: SelectedFaces.Add(RightFace);
      iFront: SelectedFaces.Add(FrontFace);
      iBack: SelectedFaces.Add(BackFace);
      iBottom: SelectedFaces.Add(BottomFace);
      iTop: SelectedFaces.Add(TopFace);
      else Assert(False);
    end;

    for Index := 0 to AllFaces.Count - 1 do
    begin
      AFace := AllFaces[Index];
      if SelectedFaces.IndexOf(AFace) >= 0 then
      begin
        AFace.Material.BackProperties.Diffuse.Color := clrSpringGreen;
        AFace.Material.FrontProperties.Diffuse.Color := clrSpringGreen;
      end
      else
      begin
        AFace.Material.BackProperties.Diffuse.Color := clrGray80;
        AFace.Material.FrontProperties.Diffuse.Color := clrGray80;
      end;
    end;
//    BackSphere.Visible := IFACE = iBack;
//    BottomSphere.Visible := IFACE = iBottom;
    CentralSphere.Visible := IFACE = iInternal;
//    FrontSphere.Visible := IFACE in [iHorizontal,iFront];
//    LeftSphere.Visible := IFACE = iLeft;
//    RightSphere.Visible := IFACE  in [iHorizontal,iRight];
//    TopSphere.Visible := IFACE = iTop;
  finally
    List.Free;
    SelectedFaces.Free;
    AllFaces.Free;
    FSettingIFace := False;
  end;
end;

end.
