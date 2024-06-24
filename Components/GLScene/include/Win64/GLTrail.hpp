// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Gltrail.pas' rev: 36.00 (Windows)

#ifndef GltrailHPP
#define GltrailHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <Sysinit.hpp>
#include <System.Classes.hpp>
#include <System.Sysutils.hpp>
#include <Glscene.hpp>
#include <Glvectortypes.hpp>
#include <Glmeshutils.hpp>
#include <Glvectorgeometry.hpp>
#include <Glvectorfileobjects.hpp>
#include <Glmesh.hpp>
#include <Globjects.hpp>
#include <Glmaterial.hpp>
#include <Glstrings.hpp>
#include <Glbaseclasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gltrail
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLTrail;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TMarkStyle : unsigned char { msUp, msDirection, msFaceCamera, msRight };

class PASCALIMPLEMENTATION TGLTrail : public Glmesh::TGLMesh
{
	typedef Glmesh::TGLMesh inherited;
	
private:
	int fVertLimit;
	float fTimeLimit;
	float fMinDistance;
	float fAlpha;
	bool fAlphaFade;
	float fUVScale;
	System::StaticArray<Glvectortypes::TVector3f, 2000> fVerts;
	System::StaticArray<Glvectorgeometry::TTexPoint, 2000> fUVs;
	System::StaticArray<double, 2000> fTimeStamps;
	int fVertStart;
	int fVertEnd;
	int fVertCount;
	Glvectortypes::TVector3f fLastV0Pos;
	Glvectortypes::TVector3f fLastPos;
	Glvectortypes::TVector3f fLastDir;
	Glvectortypes::TVector3f fLastUp;
	float FLastUVs;
	Glvectortypes::TVector3f fLastP1;
	Glvectortypes::TVector3f fLastP2;
	Glscene::TGLBaseSceneObject* FTrailObject;
	TMarkStyle FMarkStyle;
	float FMarkWidth;
	bool FEnabled;
	float FAntiZFightOffset;
	void __fastcall SetTrailObject(Glscene::TGLBaseSceneObject* const Value);
	void __fastcall SetMarkStyle(const TMarkStyle Value);
	void __fastcall SetAlpha(const float Value);
	void __fastcall SetAlphaFade(const bool Value);
	void __fastcall SetMinDistance(const float Value);
	void __fastcall SetTimeLimit(const float Value);
	void __fastcall SetUVScale(const float Value);
	void __fastcall SetVertLimit(const int Value);
	void __fastcall SetMarkWidth(const float Value);
	void __fastcall SetEnabled(const bool Value);
	bool __fastcall StoreAntiZFightOffset();
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	virtual void __fastcall DoProgress(const Glbaseclasses::TGLProgressTimes &progressTime);
	__fastcall virtual TGLTrail(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLTrail();
	void __fastcall CreateMark(Glscene::TGLBaseSceneObject* obj, float width, double CurrentTime)/* overload */;
	void __fastcall CreateMark(const Glvectortypes::TVector3f &APos, const Glvectortypes::TVector3f &ADir, const Glvectortypes::TVector3f &AUp, float AWidth, double ACurrentTime)/* overload */;
	bool __fastcall CreateMark(const Glvectortypes::TVector3f &p1, const Glvectortypes::TVector3f &p2, double CurrentTime)/* overload */;
	void __fastcall ClearMarks();
	
__published:
	__property float AntiZFightOffset = {read=FAntiZFightOffset, write=FAntiZFightOffset, stored=StoreAntiZFightOffset};
	__property int VertLimit = {read=fVertLimit, write=SetVertLimit, default=150};
	__property float TimeLimit = {read=fTimeLimit, write=SetTimeLimit};
	__property float MinDistance = {read=fMinDistance, write=SetMinDistance};
	__property float Alpha = {read=fAlpha, write=SetAlpha};
	__property bool AlphaFade = {read=fAlphaFade, write=SetAlphaFade, default=1};
	__property float UVScale = {read=fUVScale, write=SetUVScale};
	__property TMarkStyle MarkStyle = {read=FMarkStyle, write=SetMarkStyle, default=2};
	__property Glscene::TGLBaseSceneObject* TrailObject = {read=FTrailObject, write=SetTrailObject, default=0};
	__property float MarkWidth = {read=FMarkWidth, write=SetMarkWidth};
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=1};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLTrail(Glscene::TGLBaseSceneObject* aParentOwner) : Glmesh::TGLMesh(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static _DELPHI_CONST System::Word cMaxVerts = System::Word(0x7d0);
}	/* namespace Gltrail */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLTRAIL)
using namespace Gltrail;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GltrailHPP
