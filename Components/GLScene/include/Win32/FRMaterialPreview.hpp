// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FRMaterialPreview.pas' rev: 36.00 (Windows)

#ifndef FrmaterialpreviewHPP
#define FrmaterialpreviewHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Types.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <GLScene.hpp>
#include <GLObjects.hpp>
#include <GLTexture.hpp>
#include <GLHUDObjects.hpp>
#include <GLWin32Viewer.hpp>
#include <GLTeapot.hpp>
#include <GLGeomObjects.hpp>
#include <GLColor.hpp>
#include <GLCoordinates.hpp>
#include <GLCrossPlatform.hpp>
#include <GLBaseClasses.hpp>
#include <GLMaterial.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Frmaterialpreview
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TRMaterialPreview;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TRMaterialPreview : public Vcl::Forms::TFrame
{
	typedef Vcl::Forms::TFrame inherited;
	
__published:
	Glscene::TGLScene* GLScene;
	Vcl::Stdctrls::TComboBox* CBObject;
	Glscene::TGLCamera* Camera;
	Globjects::TGLCube* Cube;
	Globjects::TGLSphere* Sphere;
	Glscene::TGLLightSource* LightSource;
	Vcl::Stdctrls::TComboBox* CBBackground;
	Glhudobjects::TGLHUDSprite* BackGroundSprite;
	Glgeomobjects::TGLCone* Cone;
	Glteapot::TGLTeapot* Teapot;
	Globjects::TGLDummyCube* World;
	Globjects::TGLDummyCube* Light;
	Globjects::TGLSphere* FireSphere;
	Glmaterial::TGLMaterialLibrary* GLMaterialLibrary;
	Glwin32viewer::TGLSceneViewer* GLSceneViewer;
	void __fastcall CBObjectChange(System::TObject* Sender);
	void __fastcall CBBackgroundChange(System::TObject* Sender);
	void __fastcall SceneViewerMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall SceneViewerMouseDown(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall SceneViewerMouseWheel(System::TObject* Sender, System::Classes::TShiftState Shift, int WheelDelta, const System::Types::TPoint &MousePos, bool &Handled);
	
private:
	Glmaterial::TGLAbstractLibMaterial* FLibMaterial;
	Glmaterial::TGLMaterial* __fastcall GetMaterial();
	void __fastcall SetMaterial(Glmaterial::TGLMaterial* const Value);
	Glmaterial::TGLAbstractLibMaterial* __fastcall GetLibMaterial();
	void __fastcall SetLibMaterial(Glmaterial::TGLAbstractLibMaterial* const Value);
	
public:
	__fastcall virtual TRMaterialPreview(System::Classes::TComponent* AOwner);
	__property Glmaterial::TGLMaterial* Material = {read=GetMaterial, write=SetMaterial};
	__property Glmaterial::TGLAbstractLibMaterial* LibMaterial = {read=GetLibMaterial, write=SetLibMaterial};
public:
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TRMaterialPreview() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TRMaterialPreview(HWND ParentWindow) : Vcl::Forms::TFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Frmaterialpreview */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FRMATERIALPREVIEW)
using namespace Frmaterialpreview;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FrmaterialpreviewHPP
