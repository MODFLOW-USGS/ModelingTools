// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLExplosionFx.pas' rev: 34.00 (Windows)

#ifndef GlexplosionfxHPP
#define GlexplosionfxHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <OpenGLTokens.hpp>
#include <GLVectorGeometry.hpp>
#include <GLScene.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLVectorTypes.hpp>
#include <GLVectorLists.hpp>
#include <GLXCollection.hpp>
#include <GLCoordinates.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLContext.hpp>
#include <GLState.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glexplosionfx
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLBExplosionFX;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLBExplosionFX : public Glscene::TGLObjectPreEffect
{
	typedef Glscene::TGLObjectPreEffect inherited;
	
private:
	Glvectorlists::TAffineVectorList* FTriList;
	Glvectorlists::TAffineVectorList* FRotList;
	Glvectorlists::TAffineVectorList* FDirList;
	Glvectorlists::TAffineVectorList* FPosList;
	bool FEnabled;
	int FFaceCount;
	float FSpeed;
	Glcoordinates::TGLCoordinates3* FDirection;
	int FMaxSteps;
	int FStep;
	void __fastcall SetTriList(Glvectorlists::TAffineVectorList* Value);
	void __fastcall SetRotList(Glvectorlists::TAffineVectorList* Value);
	void __fastcall SetDirList(Glvectorlists::TAffineVectorList* Value);
	void __fastcall SetPosList(Glvectorlists::TAffineVectorList* Value);
	void __fastcall SetDirection(Glcoordinates::TGLCoordinates3* value);
	void __fastcall SetEnabled(bool value);
	
protected:
	__property Glvectorlists::TAffineVectorList* TriList = {read=FTriList, write=SetTriList};
	__property Glvectorlists::TAffineVectorList* RotList = {read=FRotList, write=SetRotList};
	__property Glvectorlists::TAffineVectorList* DirList = {read=FDirList, write=SetDirList};
	__property Glvectorlists::TAffineVectorList* PosList = {read=FPosList, write=SetPosList};
	__property int FaceCount = {read=FFaceCount, write=FFaceCount, nodefault};
	void __fastcall CacheInfo();
	
public:
	__property bool Enabled = {read=FEnabled, write=SetEnabled, nodefault};
	__property int Step = {read=FStep, nodefault};
	__fastcall virtual TGLBExplosionFX(Glxcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLBExplosionFX();
	virtual void __fastcall Render(Glrendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall Reset();
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	
__published:
	__property int MaxSteps = {read=FMaxSteps, write=FMaxSteps, nodefault};
	__property float Speed = {read=FSpeed, write=FSpeed};
	__property Glcoordinates::TGLCoordinates3* Direction = {read=FDirection, write=SetDirection};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glexplosionfx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLEXPLOSIONFX)
using namespace Glexplosionfx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlexplosionfxHPP
