// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FLibMaterialPicker.pas' rev: 36.00 (Windows)

#ifndef FLibMaterialPickerHPP
#define FLibMaterialPickerHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.Controls.hpp>
#include <GLWin32Viewer.hpp>
#include <GLMaterial.hpp>
#include <FRMaterialPreview.hpp>

//-- user supplied -----------------------------------------------------------

namespace Flibmaterialpicker
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLLibMaterialPicker;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLLibMaterialPicker : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Stdctrls::TListBox* LBMaterials;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Buttons::TBitBtn* BBOk;
	Vcl::Buttons::TBitBtn* BBCancel;
	Frmaterialpreview::TRMaterialPreview* MPPreview;
	void __fastcall LBMaterialsClick(System::TObject* Sender);
	void __fastcall LBMaterialsKeyPress(System::TObject* Sender, System::WideChar &Key);
	void __fastcall LBMaterialsDblClick(System::TObject* Sender);
	
public:
	bool __fastcall Execute(Glmaterial::TGLLibMaterialName &materialName, Glmaterial::TGLAbstractMaterialLibrary* materialLibrary);
public:
	/* TCustomForm.Create */ inline __fastcall virtual TGLLibMaterialPicker(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TGLLibMaterialPicker(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.CreateScaledNew */ inline __fastcall virtual TGLLibMaterialPicker(System::Classes::TComponent* AOwner, int ADPI, int Dummy) : Vcl::Forms::TForm(AOwner, ADPI, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TGLLibMaterialPicker() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TGLLibMaterialPicker(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TGLLibMaterialPicker* __fastcall GLLibMaterialPicker(void);
extern DELPHI_PACKAGE void __fastcall ReleaseLibMaterialPicker(void);
}	/* namespace Flibmaterialpicker */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FLIBMATERIALPICKER)
using namespace Flibmaterialpicker;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FLibMaterialPickerHPP
