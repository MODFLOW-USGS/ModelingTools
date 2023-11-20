// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLHeightTileFileHDS.pas' rev: 35.00 (Windows)

#ifndef GlheighttilefilehdsHPP
#define GlheighttilefilehdsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLHeightData.hpp>
#include <GLHeightTileFile.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glheighttilefilehds
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLHeightTileFileHDS;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLHeightTileFileHDS : public Glheightdata::TGLHeightDataSource
{
	typedef Glheightdata::TGLHeightDataSource inherited;
	
private:
	bool FInfiniteWrap;
	bool FInverted;
	System::UnicodeString FHTFFileName;
	Glheighttilefile::TGLHeightTileFile* FHTF;
	int FMinElevation;
	
protected:
	void __fastcall SetHTFFileName(const System::UnicodeString val);
	void __fastcall SetInfiniteWrap(bool val);
	void __fastcall SetInverted(bool val);
	void __fastcall SetMinElevation(int val);
	
public:
	__fastcall virtual TGLHeightTileFileHDS(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLHeightTileFileHDS();
	virtual void __fastcall StartPreparingData(Glheightdata::TGLHeightData* HeightData);
	virtual int __fastcall Width();
	virtual int __fastcall Height();
	Glheighttilefile::TGLHeightTileFile* __fastcall OpenHTF();
	
__published:
	__property System::UnicodeString HTFFileName = {read=FHTFFileName, write=SetHTFFileName};
	__property bool InfiniteWrap = {read=FInfiniteWrap, write=SetInfiniteWrap, default=1};
	__property bool Inverted = {read=FInverted, write=SetInverted, default=1};
	__property int MinElevation = {read=FMinElevation, write=SetMinElevation, default=-32768};
	__property MaxPoolSize;
	__property DefaultHeight = {default=0};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glheighttilefilehds */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLHEIGHTTILEFILEHDS)
using namespace Glheighttilefilehds;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlheighttilefilehdsHPP
