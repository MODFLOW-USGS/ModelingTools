// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Q3MD3.pas' rev: 36.00 (Windows)

#ifndef Q3md3HPP
#define Q3md3HPP

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
#include <GLApplicationFileIO.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLVectorLists.hpp>
#include <GLMaterial.hpp>
#include <GLPersistentClasses.hpp>
#include <FileMD3.hpp>
#include <GLVectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Q3md3
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TMD3TagList;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TMD3TagList : public System::TObject
{
	typedef System::TObject inherited;
	
	
private:
	typedef System::DynamicArray<Filemd3::TMD3Tag> _TMD3TagList__1;
	
	
private:
	_TMD3TagList__1 FTags;
	int FNumTags;
	int FNumFrames;
	Filemd3::TMD3Tag __fastcall GetTag(int index);
	
public:
	void __fastcall LoadFromFile(const System::UnicodeString FileName);
	void __fastcall LoadFromStream(System::Classes::TStream* AStream);
	Glvectorgeometry::TMatrix __fastcall GetTransform(const System::UnicodeString TagName, int Frame);
	__property int TagCount = {read=FNumTags, nodefault};
	__property int FrameCount = {read=FNumFrames, nodefault};
	__property Filemd3::TMD3Tag Tags[int index] = {read=GetTag};
public:
	/* TObject.Create */ inline __fastcall TMD3TagList() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TMD3TagList() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall LoadQ3Anims(Glvectorfileobjects::TGLActorAnimations* Animations, const System::UnicodeString FileName, const System::UnicodeString NamePrefix)/* overload */;
extern DELPHI_PACKAGE void __fastcall LoadQ3Anims(Glvectorfileobjects::TGLActorAnimations* Animations, System::Classes::TStrings* Strings, const System::UnicodeString NamePrefix)/* overload */;
extern DELPHI_PACKAGE void __fastcall LoadQ3Skin(const System::UnicodeString FileName, Glvectorfileobjects::TGLActor* Actor);
}	/* namespace Q3md3 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_Q3MD3)
using namespace Q3md3;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Q3md3HPP
