// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSkyBox.pas' rev: 36.00 (Windows)

#ifndef GlskyboxHPP
#define GlskyboxHPP

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
#include <GLScene.hpp>
#include <GLMaterial.hpp>
#include <GLVectorGeometry.hpp>
#include <OpenGLTokens.hpp>
#include <XOpenGL.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLVectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glskybox
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLSkyBox;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLSkyBoxStyle : unsigned char { sbsFull, sbsTopHalf, sbsBottomHalf, sbTopTwoThirds, sbsTopHalfClamped };

class PASCALIMPLEMENTATION TGLSkyBox : public Glscene::TGLCameraInvariantObject
{
	typedef Glscene::TGLCameraInvariantObject inherited;
	
private:
	System::UnicodeString FMatNameTop;
	System::UnicodeString FMatNameRight;
	System::UnicodeString FMatNameFront;
	System::UnicodeString FMatNameLeft;
	System::UnicodeString FMatNameBack;
	System::UnicodeString FMatNameBottom;
	System::UnicodeString FMatNameClouds;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	float FCloudsPlaneOffset;
	float FCloudsPlaneSize;
	TGLSkyBoxStyle FStyle;
	Glmaterial::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	
protected:
	void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const Value);
	void __fastcall SetMatNameBack(const System::UnicodeString Value);
	void __fastcall SetMatNameBottom(const System::UnicodeString Value);
	void __fastcall SetMatNameFront(const System::UnicodeString Value);
	void __fastcall SetMatNameLeft(const System::UnicodeString Value);
	void __fastcall SetMatNameRight(const System::UnicodeString Value);
	void __fastcall SetMatNameTop(const System::UnicodeString Value);
	void __fastcall SetMatNameClouds(const System::UnicodeString Value);
	void __fastcall SetCloudsPlaneOffset(const float Value);
	void __fastcall SetCloudsPlaneSize(const float Value);
	void __fastcall SetStyle(const TGLSkyBoxStyle value);
	
public:
	__fastcall virtual TGLSkyBox(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSkyBox();
	virtual void __fastcall DoRender(Glrendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &ARci);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
__published:
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property Glmaterial::TGLLibMaterialName MatNameTop = {read=FMatNameTop, write=SetMatNameTop};
	__property Glmaterial::TGLLibMaterialName MatNameBottom = {read=FMatNameBottom, write=SetMatNameBottom};
	__property Glmaterial::TGLLibMaterialName MatNameLeft = {read=FMatNameLeft, write=SetMatNameLeft};
	__property Glmaterial::TGLLibMaterialName MatNameRight = {read=FMatNameRight, write=SetMatNameRight};
	__property Glmaterial::TGLLibMaterialName MatNameFront = {read=FMatNameFront, write=SetMatNameFront};
	__property Glmaterial::TGLLibMaterialName MatNameBack = {read=FMatNameBack, write=SetMatNameBack};
	__property Glmaterial::TGLLibMaterialName MatNameClouds = {read=FMatNameClouds, write=SetMatNameClouds};
	__property float CloudsPlaneOffset = {read=FCloudsPlaneOffset, write=SetCloudsPlaneOffset};
	__property float CloudsPlaneSize = {read=FCloudsPlaneSize, write=SetCloudsPlaneSize};
	__property TGLSkyBoxStyle Style = {read=FStyle, write=FStyle, default=0};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLSkyBox(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLCameraInvariantObject(aParentOwner) { }
	
private:
	void *__IGLMaterialLibrarySupported;	// Glmaterial::IGLMaterialLibrarySupported 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {8E442AF9-D212-4A5E-8A88-92F798BABFD1}
	operator Glmaterial::_di_IGLMaterialLibrarySupported()
	{
		Glmaterial::_di_IGLMaterialLibrarySupported intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Glmaterial::IGLMaterialLibrarySupported*(void) { return (Glmaterial::IGLMaterialLibrarySupported*)&__IGLMaterialLibrarySupported; }
	#endif
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glskybox */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSKYBOX)
using namespace Glskybox;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlskyboxHPP
