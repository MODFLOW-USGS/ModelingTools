// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLAsyncHDS.pas' rev: 36.00 (Windows)

#ifndef GlasynchdsHPP
#define GlasynchdsHPP

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
#include <GLHeightData.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glasynchds
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLAsyncHDS;
class DELPHICLASS TGLAsyncHDThread;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TIdleEvent)(TGLAsyncHDS* Sender, bool TilesUpdated);

typedef void __fastcall (__closure *TNewTilePreparedEvent)(TGLAsyncHDS* Sender, Glheightdata::TGLHeightData* HeightData);

enum DECLSPEC_DENUM TUseDirtyTiles : unsigned char { dtNever, dtUntilReplaced, dtUntilAllReplaced };

class PASCALIMPLEMENTATION TGLAsyncHDS : public Glheightdata::TGLHeightDataSourceFilter
{
	typedef Glheightdata::TGLHeightDataSourceFilter inherited;
	
private:
	TIdleEvent FOnIdleEvent;
	TNewTilePreparedEvent FOnNewTilePrepared;
	TUseDirtyTiles FUseDirtyTiles;
	bool FTilesUpdated;
	
public:
	__fastcall virtual TGLAsyncHDS(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLAsyncHDS();
	virtual void __fastcall BeforePreparingData(Glheightdata::TGLHeightData* HeightData);
	virtual void __fastcall StartPreparingData(Glheightdata::TGLHeightData* HeightData);
	virtual void __fastcall ThreadIsIdle();
	void __fastcall NewTilePrepared(Glheightdata::TGLHeightData* HeightData);
	int __fastcall ThreadCount();
	void __fastcall WaitFor(int TimeOut = 0x7d0);
	bool __fastcall TilesUpdated();
	void __fastcall TilesUpdatedFlagReset();
	
__published:
	__property TIdleEvent OnIdle = {read=FOnIdleEvent, write=FOnIdleEvent};
	__property TNewTilePreparedEvent OnNewTilePrepared = {read=FOnNewTilePrepared, write=FOnNewTilePrepared};
	__property TUseDirtyTiles UseDirtyTiles = {read=FUseDirtyTiles, write=FUseDirtyTiles, nodefault};
	__property MaxThreads;
	__property Active;
};


class PASCALIMPLEMENTATION TGLAsyncHDThread : public Glheightdata::TGLHeightDataThread
{
	typedef Glheightdata::TGLHeightDataThread inherited;
	
public:
	TGLAsyncHDS* Owner;
	Glheightdata::TGLHeightDataSource* HDS;
	virtual void __fastcall Execute();
	void __fastcall Sync();
public:
	/* TGLHeightDataThread.Destroy */ inline __fastcall virtual ~TGLAsyncHDThread() { }
	
public:
	/* TThread.Create */ inline __fastcall TGLAsyncHDThread()/* overload */ : Glheightdata::TGLHeightDataThread() { }
	/* TThread.Create */ inline __fastcall TGLAsyncHDThread(bool CreateSuspended)/* overload */ : Glheightdata::TGLHeightDataThread(CreateSuspended) { }
	/* TThread.Create */ inline __fastcall TGLAsyncHDThread(bool CreateSuspended, System::NativeUInt ReservedStackSize)/* overload */ : Glheightdata::TGLHeightDataThread(CreateSuspended, ReservedStackSize) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glasynchds */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLASYNCHDS)
using namespace Glasynchds;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlasynchdsHPP
