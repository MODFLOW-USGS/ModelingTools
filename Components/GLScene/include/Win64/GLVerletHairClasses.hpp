// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLVerletHairClasses.pas' rev: 36.00 (Windows)

#ifndef GlverlethairclassesHPP
#define GlverlethairclassesHPP

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
#include <GLVerletTypes.hpp>
#include <GLVectorTypes.hpp>
#include <GLVectorLists.hpp>
#include <GLVectorGeometry.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glverlethairclasses
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLVerletHair;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TVHStiffness : unsigned char { vhsFull, vhsSkip1Node, vhsSkip2Node, vhsSkip3Node, vhsSkip4Node, vhsSkip5Node, vhsSkip6Node, vhsSkip7Node, vhsSkip8Node, vhsSkip9Node };

typedef System::Set<TVHStiffness, TVHStiffness::vhsFull, TVHStiffness::vhsSkip9Node> TVHStiffnessSet;

class PASCALIMPLEMENTATION TGLVerletHair : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Glverlettypes::TVerletNodeList* FNodeList;
	int FLinkCount;
	float FRootDepth;
	Glverlettypes::TGLVerletWorld* FVerletWorld;
	float FHairLength;
	void *FData;
	TVHStiffnessSet FStiffness;
	System::Classes::TList* FStiffnessList;
	Glverlettypes::TVerletNode* __fastcall GetAnchor();
	Glverlettypes::TVerletNode* __fastcall GetRoot();
	float __fastcall GetLinkLength();
	void __fastcall AddStickStiffness(const int ANodeSkip);
	void __fastcall SetStiffness(const TVHStiffnessSet Value);
	
public:
	void __fastcall BuildHair(const Glvectorgeometry::TAffineVector &AAnchorPosition, const Glvectorgeometry::TAffineVector &AHairDirection);
	void __fastcall BuildStiffness();
	void __fastcall ClearStiffness();
	void __fastcall Clear();
	__fastcall TGLVerletHair(Glverlettypes::TGLVerletWorld* const AVerletWorld, const float ARootDepth, const float AHairLength, int ALinkCount, const Glvectorgeometry::TAffineVector &AAnchorPosition, const Glvectorgeometry::TAffineVector &AHairDirection, const TVHStiffnessSet AStiffness);
	__fastcall virtual ~TGLVerletHair();
	__property Glverlettypes::TVerletNodeList* NodeList = {read=FNodeList};
	__property Glverlettypes::TGLVerletWorld* VerletWorld = {read=FVerletWorld};
	__property float RootDepth = {read=FRootDepth};
	__property float LinkLength = {read=GetLinkLength};
	__property int LinkCount = {read=FLinkCount, nodefault};
	__property float HairLength = {read=FHairLength};
	__property TVHStiffnessSet Stiffness = {read=FStiffness, write=SetStiffness, nodefault};
	__property void * Data = {read=FData, write=FData};
	__property Glverlettypes::TVerletNode* Anchor = {read=GetAnchor};
	__property Glverlettypes::TVerletNode* Root = {read=GetRoot};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glverlethairclasses */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLVERLETHAIRCLASSES)
using namespace Glverlethairclasses;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlverlethairclassesHPP
