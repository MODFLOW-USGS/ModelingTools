// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFeedback.pas' rev: 36.00 (Windows)

#ifndef GLFeedbackHPP
#define GLFeedbackHPP

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
#include <System.SysUtils.hpp>
#include <OpenGLTokens.hpp>
#include <GLVectorGeometry.hpp>
#include <GLPersistentClasses.hpp>
#include <GLPipelineTransformation.hpp>
#include <GLVectorLists.hpp>
#include <GLScene.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLTexture.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLContext.hpp>
#include <GLState.hpp>
#include <GLMeshUtils.hpp>
#include <GLVectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfeedback
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLFeedback;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TFeedbackMode : unsigned char { fm2D, fm3D, fm3DColor, fm3DColorTexture, fm4DColorTexture };

class PASCALIMPLEMENTATION TGLFeedback : public Glscene::TGLBaseSceneObject
{
	typedef Glscene::TGLBaseSceneObject inherited;
	
private:
	bool FActive;
	Glvectorlists::TSingleList* FBuffer;
	unsigned FMaxBufferSize;
	bool FBuffered;
	float FCorrectionScaling;
	TFeedbackMode FMode;
	
protected:
	void __fastcall SetMaxBufferSize(const unsigned Value);
	void __fastcall SetMode(const TFeedbackMode Value);
	
public:
	__fastcall virtual TGLFeedback(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLFeedback();
	virtual void __fastcall DoRender(Glrendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	void __fastcall BuildMeshFromBuffer(Glvectorlists::TAffineVectorList* Vertices = (Glvectorlists::TAffineVectorList*)(0x0), Glvectorlists::TAffineVectorList* Normals = (Glvectorlists::TAffineVectorList*)(0x0), Glvectorlists::TVectorList* Colors = (Glvectorlists::TVectorList*)(0x0), Glvectorlists::TAffineVectorList* TexCoords = (Glvectorlists::TAffineVectorList*)(0x0), Glvectorlists::TIntegerList* VertexIndices = (Glvectorlists::TIntegerList*)(0x0));
	__property bool Buffered = {read=FBuffered, nodefault};
	__property Glvectorlists::TSingleList* Buffer = {read=FBuffer};
	__property float CorrectionScaling = {read=FCorrectionScaling};
	
__published:
	__property unsigned MaxBufferSize = {read=FMaxBufferSize, write=SetMaxBufferSize, nodefault};
	__property bool Active = {read=FActive, write=FActive, nodefault};
	__property TFeedbackMode Mode = {read=FMode, write=SetMode, nodefault};
	__property Visible = {default=1};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLFeedback(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLBaseSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glfeedback */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFEEDBACK)
using namespace Glfeedback;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GLFeedbackHPP
