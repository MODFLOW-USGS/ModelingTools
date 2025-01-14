// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'File3DS.pas' rev: 36.00 (Windows)

#ifndef File3dsHPP
#define File3dsHPP

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
#include <Types3DS.hpp>

//-- user supplied -----------------------------------------------------------

namespace File3ds
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TMaterialList;
class DELPHICLASS TObjectList;
class DELPHICLASS TKeyFramer;
class DELPHICLASS TFile3DS;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TLoadProgress)(System::LongInt StreamPos, System::LongInt StreamMax);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TMaterialList : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	Types3ds::PMaterial3DS operator[](int Index) { return this->Material[Index]; }
	
private:
	TFile3DS* FOwner;
	System::Classes::TList* FLocalList;
	int __fastcall GetCount();
	Types3ds::PMaterial3DS __fastcall GetMaterial(int Index);
	Types3ds::PMaterial3DS __fastcall GetMaterialByName(const System::UnicodeString Name);
	
public:
	__fastcall virtual TMaterialList(TFile3DS* AOwner);
	__fastcall virtual ~TMaterialList();
	void __fastcall ClearList();
	__property int Count = {read=GetCount, nodefault};
	__property Types3ds::PMaterial3DS Material[int Index] = {read=GetMaterial/*, default*/};
	__property Types3ds::PMaterial3DS MaterialByName[const System::UnicodeString Name] = {read=GetMaterialByName};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TObjectList : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TFile3DS* FOwner;
	System::Classes::TList* FMeshList;
	System::Classes::TList* FOmniList;
	System::Classes::TList* FSpotList;
	System::Classes::TList* FCameraList;
	Types3ds::PCamera3DS __fastcall GetCamera(int Index);
	int __fastcall GetCamCount();
	int __fastcall GetMeshObjectCount();
	Types3ds::PMesh3DS __fastcall GetMesh(int Index);
	int __fastcall GetOmniCount();
	Types3ds::PLight3DS __fastcall GetOmniLight(int Index);
	int __fastcall GetSpotCount();
	Types3ds::PLight3DS __fastcall GetSpotLight(int Index);
	
public:
	__fastcall virtual TObjectList(TFile3DS* AOwner);
	__fastcall virtual ~TObjectList();
	void __fastcall ClearLists();
	__property int CameraCount = {read=GetCamCount, nodefault};
	__property int MeshCount = {read=GetMeshObjectCount, nodefault};
	__property int OmniLightCount = {read=GetOmniCount, nodefault};
	__property int SpotLightCount = {read=GetSpotCount, nodefault};
	__property Types3ds::PMesh3DS Mesh[int Index] = {read=GetMesh};
	__property Types3ds::PCamera3DS Camera[int Index] = {read=GetCamera};
	__property Types3ds::PLight3DS OmniLight[int Index] = {read=GetOmniLight};
	__property Types3ds::PLight3DS SpotLight[int Index] = {read=GetSpotLight};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TKeyFramer : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	Types3ds::PKFMesh3DS operator[](int Index) { return this->MeshMotion[Index]; }
	
private:
	TFile3DS* FOwner;
	System::Classes::TList* FMeshMotionList;
	System::Classes::TList* FOmniMotionList;
	System::Classes::TList* FSpotMotionList;
	System::Classes::TList* FCameraMotionList;
	Types3ds::PKFAmbient3DS FAmbientMotion;
	Types3ds::PKFAmbient3DS __fastcall GetAmbientMotion();
	Types3ds::PKFCamera3DS __fastcall GetCameraMotion(int Index);
	int __fastcall GetCamMotionCount();
	Types3ds::TKFSets3DS __fastcall GetKFSets();
	int __fastcall GetMeshMotionCount();
	Types3ds::PKFMesh3DS __fastcall GetMeshMotion(int Index);
	int __fastcall GetOmniMotionCount();
	Types3ds::PKFOmni3DS __fastcall GetOmniLightMotion(int Index);
	int __fastcall GetSpotMotionCount();
	Types3ds::PKFSpot3DS __fastcall GetSpotLightMotion(int Index);
	
public:
	__fastcall virtual TKeyFramer(TFile3DS* AOwner);
	__fastcall virtual ~TKeyFramer();
	void __fastcall ClearLists();
	__property Types3ds::PKFAmbient3DS AmbientLightMotion = {read=GetAmbientMotion};
	__property int CameraMotionCount = {read=GetCamMotionCount, nodefault};
	__property int MeshMotionCount = {read=GetMeshMotionCount, nodefault};
	__property int OmniLightMotionCount = {read=GetOmniMotionCount, nodefault};
	__property int SpotLightMotionCount = {read=GetSpotMotionCount, nodefault};
	__property Types3ds::PKFMesh3DS MeshMotion[int Index] = {read=GetMeshMotion/*, default*/};
	__property Types3ds::PKFCamera3DS CameraMotion[int Index] = {read=GetCameraMotion};
	__property Types3ds::PKFOmni3DS OmniLightMotion[int Index] = {read=GetOmniLightMotion};
	__property Types3ds::TKFSets3DS Settings = {read=GetKFSets};
	__property Types3ds::PKFSpot3DS SpotLightMotion[int Index] = {read=GetSpotLightMotion};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TFile3DS : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Types3ds::PNodeList FNodeList;
	Types3ds::TDatabase3DS FDatabase;
	System::Classes::TStream* FStream;
	bool FOwnStream;
	TMaterialList* FMaterialList;
	TObjectList* FObjectList;
	TKeyFramer* FKeyFramer;
	System::UnicodeString FFileName;
	TLoadProgress FOnLoadProgress;
	Types3ds::TAtmosphere3DS __fastcall GetAtmosphereData();
	Types3ds::TBackground3DS __fastcall GetBackgroundData();
	Types3ds::TDBType3DS __fastcall GetDatabaseType();
	Types3ds::TMeshSet3DS __fastcall GetMeshSettings();
	Types3ds::TViewport3DS __fastcall GetViewportData();
	Types3ds::TReleaseLevel __fastcall GetDatabaseRelease();
	Types3ds::TReleaseLevel __fastcall GetMeshRelease();
	
protected:
	void __fastcall AddToNodeList(Types3ds::PChunk3DS Chunk);
	void __fastcall AssignParentNames();
	void __fastcall CheckListNodeIDs();
	void __fastcall CreateDatabase();
	Types3ds::PNodeList __fastcall FindNodeByID(short ID);
	short __fastcall GetChunkNodeID(Types3ds::PChunk3DS Chunk);
	void __fastcall InitDatabase();
	bool __fastcall IsNode(System::Word Tag);
	void __fastcall KFAddParentName(Types3ds::PChunk3DS Chunk, const Types3ds::String3DS Name);
	void __fastcall MakeNode(Types3ds::PNodeList &Node);
	void __fastcall ParseDatabase();
	void __fastcall ReadChildren(Types3ds::PChunk3DS Parent);
	void __fastcall ReadXDataEntryChildren(Types3ds::PChunk3DS Parent);
	void __fastcall ReleaseDatabase();
	void __fastcall ReleaseNodeList();
	void __fastcall ReleaseStream();
	
public:
	__fastcall virtual TFile3DS();
	__fastcall virtual TFile3DS(const System::UnicodeString FileName);
	__fastcall virtual ~TFile3DS();
	void __fastcall ClearLists();
	void __fastcall DumpDataBase(System::Classes::TStrings* Strings, Types3ds::TDumpLevel DumpLevel);
	void __fastcall LoadFromFile(const System::UnicodeString FileName);
	void __fastcall LoadFromStream(System::Classes::TStream* const aStream);
	System::Byte __fastcall ReadByte();
	unsigned __fastcall ReadCardinal();
	void __fastcall ReadChunkData(Types3ds::PChunk3DS Chunk);
	void __fastcall ReadData(int Size, void * Data);
	double __fastcall ReadDouble();
	Types3ds::TFace3DS __fastcall ReadFace();
	void __fastcall ReadHeader(System::Word &ChunkType, unsigned &ChunkSize);
	int __fastcall ReadInteger();
	Types3ds::TKeyHeader3DS __fastcall ReadKeyHeader();
	Types3ds::TPoint3DS __fastcall ReadPoint();
	short __fastcall ReadShort();
	float __fastcall ReadSingle();
	Types3ds::PChar3DS __fastcall ReadString();
	Types3ds::TTexVert3DS __fastcall ReadTexVert();
	Types3ds::TTrackHeader3DS __fastcall ReadTrackHeader();
	System::Word __fastcall ReadWord();
	void __fastcall FinishHeader(unsigned StartPos, unsigned EndPos);
	void * __fastcall InitChunkData(Types3ds::PChunk3DS Chunk);
	void __fastcall SeekChild(Types3ds::PChunk3DS Chunk);
	void __fastcall Skip(int AValue);
	void __fastcall WriteByte(System::Byte AValue);
	void __fastcall WriteCardinal(unsigned AValue);
	void __fastcall WriteData(int Size, void * Data);
	void __fastcall WriteDouble(double AValue);
	void __fastcall WriteFace(const Types3ds::TFace3DS &F);
	void __fastcall WriteFixedString(const Types3ds::String3DS AValue, int Len);
	void __fastcall WriteHeader(System::Word ChunkType, unsigned ChunkSize);
	void __fastcall WriteInteger(int AValue);
	void __fastcall WriteKeyHeader(const Types3ds::TKeyHeader3DS &K);
	void __fastcall WritePoint(const Types3ds::TPoint3DS &P);
	void __fastcall WriteShort(short AValue);
	void __fastcall WriteSingle(float AValue);
	void __fastcall WriteString(const Types3ds::String3DS AValue);
	void __fastcall WriteTexVertex(const Types3ds::TTexVert3DS &T);
	void __fastcall WriteTrackHeader(const Types3ds::TTrackHeader3DS &T);
	void __fastcall WriteWord(System::Word AValue);
	__property Types3ds::TAtmosphere3DS Atmosphere = {read=GetAtmosphereData};
	__property Types3ds::TBackground3DS Background = {read=GetBackgroundData};
	__property Types3ds::TReleaseLevel DatabaseRelease = {read=GetDatabaseRelease, nodefault};
	__property Types3ds::TDBType3DS DatabaseType = {read=GetDatabaseType, nodefault};
	__property System::UnicodeString FileName = {read=FFileName};
	__property TKeyFramer* KeyFramer = {read=FKeyFramer};
	__property TMaterialList* Materials = {read=FMaterialList};
	__property Types3ds::TReleaseLevel MeshRelease = {read=GetMeshRelease, nodefault};
	__property Types3ds::TMeshSet3DS MeshSettings = {read=GetMeshSettings};
	__property TObjectList* Objects = {read=FObjectList};
	__property Types3ds::TViewport3DS Viewport = {read=GetViewportData};
	__property TLoadProgress OnLoadProgress = {read=FOnLoadProgress, write=FOnLoadProgress};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace File3ds */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FILE3DS)
using namespace File3ds;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// File3dsHPP
