// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'fmod.pas' rev: 36.00 (Windows)

#ifndef FmodHPP
#define FmodHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <fmodtypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmod
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool __fastcall FMOD_Load(System::WideChar * LibName);
extern DELPHI_PACKAGE void __fastcall FMOD_Unload(void);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetOutput(Fmodtypes::TFSoundOutputTypes OutputType);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetDriver(int Driver);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetMixer(Fmodtypes::TFSoundMixerTypes Mixer);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetBufferSize(int LenMs);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetHWND(Winapi::Windows::THandle Hwnd);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetMinHardwareChannels(int Min);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetMaxHardwareChannels(int Max);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetMemorySystem(void * Pool, int PoolLen, Fmodtypes::TFSoundAllocCallback UserAlloc, Fmodtypes::TFSoundReallocCallback UserRealloc, Fmodtypes::TFSoundFreeCallback UserFree);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Init(int MixRate, int MaxSoftwareChannels, unsigned Flags);
extern DELPHI_PACKAGE void __stdcall FSOUND_Close(void);
extern DELPHI_PACKAGE void __stdcall FSOUND_Update(void);
extern DELPHI_PACKAGE void __stdcall FSOUND_SetSpeakerMode(unsigned SpeakerMode);
extern DELPHI_PACKAGE void __stdcall FSOUND_SetSFXMasterVolume(int Volume);
extern DELPHI_PACKAGE void __stdcall FSOUND_SetPanSeperation(float PanSep);
extern DELPHI_PACKAGE Fmodtypes::TFModErrors __stdcall FSOUND_GetError(void);
extern DELPHI_PACKAGE float __stdcall FSOUND_GetVersion(void);
extern DELPHI_PACKAGE Fmodtypes::TFSoundOutputTypes __stdcall FSOUND_GetOutput(void);
extern DELPHI_PACKAGE void * __stdcall FSOUND_GetOutputHandle(void);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetDriver(void);
extern DELPHI_PACKAGE Fmodtypes::TFSoundMixerTypes __stdcall FSOUND_GetMixer(void);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetNumDrivers(void);
extern DELPHI_PACKAGE char * __stdcall FSOUND_GetDriverName(int Id);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_GetDriverCaps(int Id, unsigned &Caps);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetOutputRate(void);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetMaxChannels(void);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetMaxSamples(void);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetSFXMasterVolume(void);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_GetNumHWChannels(int &num2d, int &num3d, int &total);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetChannelsPlaying(void);
extern DELPHI_PACKAGE float __stdcall FSOUND_GetCPUUsage(void);
extern DELPHI_PACKAGE void __stdcall FSOUND_GetMemoryStats(unsigned &CurrentAlloced, unsigned &MaxAlloced);
extern DELPHI_PACKAGE Fmodtypes::PFSoundSample __stdcall FSOUND_Sample_Load(int Index, const char * NameOrData, unsigned Mode, int Offset, int Length);
extern DELPHI_PACKAGE Fmodtypes::PFSoundSample __stdcall FSOUND_Sample_Alloc(int Index, int Length, unsigned Mode, int DefFreq, int DefVol, int DefPan, int DefPri);
extern DELPHI_PACKAGE void __stdcall FSOUND_Sample_Free(Fmodtypes::PFSoundSample Sptr);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Sample_Upload(Fmodtypes::PFSoundSample Sptr, void * SrcData, unsigned Mode);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Sample_Lock(Fmodtypes::PFSoundSample Sptr, int Offset, int Length, void * &Ptr1, void * &Ptr2, unsigned &Len1, unsigned &Len2);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Sample_Unlock(Fmodtypes::PFSoundSample Sptr, void * Ptr1, void * Ptr2, unsigned Len1, unsigned Len2);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Sample_SetMode(Fmodtypes::PFSoundSample Sptr, unsigned Mode);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Sample_SetLoopPoints(Fmodtypes::PFSoundSample Sptr, int LoopStart, int LoopEnd);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Sample_SetDefaults(Fmodtypes::PFSoundSample Sptr, int DefFreq, int DefVol, int DefPan, int DefPri);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Sample_SetDefaultsEx(Fmodtypes::PFSoundSample Sptr, int DefFreq, int DefVol, int DefPan, int DefPri, int VarFreq, int VarVol, int VarPan);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Sample_SetMinMaxDistance(Fmodtypes::PFSoundSample Sptr, float Min, float Max);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Sample_SetMaxPlaybacks(Fmodtypes::PFSoundSample Sptr, int Max);
extern DELPHI_PACKAGE Fmodtypes::PFSoundSample __stdcall FSOUND_Sample_Get(int SampNo);
extern DELPHI_PACKAGE char * __stdcall FSOUND_Sample_GetName(Fmodtypes::PFSoundSample Sptr);
extern DELPHI_PACKAGE unsigned __stdcall FSOUND_Sample_GetLength(Fmodtypes::PFSoundSample Sptr);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Sample_GetLoopPoints(Fmodtypes::PFSoundSample Sptr, int &LoopStart, int &LoopEnd);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Sample_GetDefaults(Fmodtypes::PFSoundSample Sptr, int &DefFreq, int &DefVol, int &DefPan, int &DefPri);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Sample_GetDefaultsEx(Fmodtypes::PFSoundSample Sptr, int &DefFreq, int &DefVol, int &DefPan, int &DefPri, int &VarFreq, int &VarVol, int &VarPan);
extern DELPHI_PACKAGE unsigned __stdcall FSOUND_Sample_GetMode(Fmodtypes::PFSoundSample Sptr);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Sample_GetMinMaxDistance(Fmodtypes::PFSoundSample Sptr, float &Min, float &Max);
extern DELPHI_PACKAGE int __stdcall FSOUND_PlaySound(int Channel, Fmodtypes::PFSoundSample Sptr);
extern DELPHI_PACKAGE int __stdcall FSOUND_PlaySoundEx(int Channel, Fmodtypes::PFSoundSample Sptr, Fmodtypes::PFSoundDSPUnit Dsp, System::ByteBool StartPaused);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_StopSound(int Channel);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetFrequency(int Channel, int Freq);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetVolume(int Channel, int Vol);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetVolumeAbsolute(int Channel, int Vol);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetPan(int Channel, int Pan);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetSurround(int Channel, System::ByteBool Surround);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetMute(int Channel, System::ByteBool Mute);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetPriority(int Channel, int Priority);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetReserved(int Channel, System::ByteBool Reserved);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetPaused(int Channel, System::ByteBool Paused);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetLoopMode(int Channel, unsigned LoopMode);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_SetCurrentPosition(int Channel, unsigned Offset);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_3D_SetAttributes(int Channel, Fmodtypes::PFSoundVector Pos, Fmodtypes::PFSoundVector Vel);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_3D_SetMinMaxDistance(int Channel, float Min, float Max);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_IsPlaying(int Channel);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetFrequency(int Channel);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetVolume(int Channel);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetAmplitude(int Channel);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetPan(int Channel);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_GetSurround(int Channel);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_GetMute(int Channel);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetPriority(int Channel);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_GetReserved(int Channel);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_GetPaused(int Channel);
extern DELPHI_PACKAGE unsigned __stdcall FSOUND_GetLoopMode(int Channel);
extern DELPHI_PACKAGE unsigned __stdcall FSOUND_GetCurrentPosition(int Channel);
extern DELPHI_PACKAGE Fmodtypes::PFSoundSample __stdcall FSOUND_GetCurrentSample(int Channel);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_GetCurrentLevels(int Channel, Winapi::Windows::PSingle l, Winapi::Windows::PSingle r);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetNumSubChannels(int Channel);
extern DELPHI_PACKAGE int __stdcall FSOUND_GetSubChannel(int Channel, int SubChannel);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_3D_GetAttributes(int Channel, Fmodtypes::PFSoundVector Pos, Fmodtypes::PFSoundVector Vel);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_3D_GetMinMaxDistance(int Channel, float &Min, float &Max);
extern DELPHI_PACKAGE void __stdcall FSOUND_3D_Listener_SetCurrent(int current);
extern DELPHI_PACKAGE void __stdcall FSOUND_3D_Listener_SetAttributes(Fmodtypes::PFSoundVector Pos, Fmodtypes::PFSoundVector Vel, float fx, float fy, float fz, float tx, float ty, float tz);
extern DELPHI_PACKAGE void __stdcall FSOUND_3D_Listener_GetAttributes(Fmodtypes::PFSoundVector Pos, Fmodtypes::PFSoundVector Vel, Winapi::Windows::PSingle fx, Winapi::Windows::PSingle fy, Winapi::Windows::PSingle fz, Winapi::Windows::PSingle tx, Winapi::Windows::PSingle ty, Winapi::Windows::PSingle tz);
extern DELPHI_PACKAGE void __stdcall FSOUND_3D_SetDopplerFactor(float Scale);
extern DELPHI_PACKAGE void __stdcall FSOUND_3D_SetDistanceFactor(float Scale);
extern DELPHI_PACKAGE void __stdcall FSOUND_3D_SetRolloffFactor(float Scale);
extern DELPHI_PACKAGE int __stdcall FSOUND_FX_Enable(int Channel, Fmodtypes::TFSoundFXModes Fx);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_FX_Disable(int Channel);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_FX_SetChorus(int FXId, float WetDryMix, float Depth, float Feedback, float Frequency, int Waveform, float Delay, int Phase);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_FX_SetCompressor(int FXId, float Gain, float Attack, float Release, float Threshold, float Ratio, float Predelay);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_FX_SetDistortion(int FXId, float Gain, float Edge, float PostEQCenterFrequency, float PostEQBandwidth, float PreLowpassCutoff);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_FX_SetEcho(int FXId, float WetDryMix, float Feedback, float LeftDelay, float RightDelay, int PanDelay);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_FX_SetFlanger(int FXId, float WetDryMix, float Depth, float Feedback, float Frequency, int Waveform, float Delay, int Phase);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_FX_SetGargle(int FXId, int RateHz, int WaveShape);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_FX_SetI3DL2Reverb(int FXId, int Room, int RoomHF, float RoomRolloffFactor, float DecayTime, float DecayHFRatio, int Reflections, float ReflectionsDelay, int Reverb, float ReverbDelay, float Diffusion, float Density, float HFReference);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_FX_SetParamEQ(int FXId, float Center, float Bandwidth, float Gain);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_FX_SetWavesReverb(int FXId, float InGain, float ReverbMix, float ReverbTime, float HighFreqRTRatio);
extern DELPHI_PACKAGE Fmodtypes::PFSoundStream __stdcall FSOUND_Stream_Open(const char * name_or_data, unsigned Mode, int Offset, int Length);
extern DELPHI_PACKAGE Fmodtypes::PFSoundStream __stdcall FSOUND_Stream_Create(Fmodtypes::TFSoundStreamCallback Callback, int Length, unsigned Mode, int SampleRate, int UserData);
extern DELPHI_PACKAGE int __stdcall FSOUND_Stream_Play(int Channel, Fmodtypes::PFSoundStream Stream);
extern DELPHI_PACKAGE int __stdcall FSOUND_Stream_PlayEx(int Channel, Fmodtypes::PFSoundStream Stream, Fmodtypes::PFSoundDSPUnit Dsp, System::ByteBool StartPaused);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_Stop(Fmodtypes::PFSoundStream Stream);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_Close(Fmodtypes::PFSoundStream Stream);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_SetEndCallback(Fmodtypes::PFSoundStream Stream, Fmodtypes::TFSoundStreamCallback Callback, int UserData);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_SetSyncCallback(Fmodtypes::PFSoundStream Stream, Fmodtypes::TFSoundStreamCallback Callback, int UserData);
extern DELPHI_PACKAGE Fmodtypes::PFSoundSample __stdcall FSOUND_Stream_GetSample(Fmodtypes::PFSoundStream Stream);
extern DELPHI_PACKAGE Fmodtypes::PFSoundDSPUnit __stdcall FSOUND_Stream_CreateDSP(Fmodtypes::PFSoundStream Stream, Fmodtypes::TFSoundDSPCallback Callback, int Priority, int Param);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_SetBufferSize(int Ms);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_SetPosition(Fmodtypes::PFSoundStream Stream, unsigned Position);
extern DELPHI_PACKAGE unsigned __stdcall FSOUND_Stream_GetPosition(Fmodtypes::PFSoundStream Stream);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_SetTime(Fmodtypes::PFSoundStream Stream, int Ms);
extern DELPHI_PACKAGE int __stdcall FSOUND_Stream_GetTime(Fmodtypes::PFSoundStream Stream);
extern DELPHI_PACKAGE int __stdcall FSOUND_Stream_GetLength(Fmodtypes::PFSoundStream Stream);
extern DELPHI_PACKAGE int __stdcall FSOUND_Stream_GetLengthMs(Fmodtypes::PFSoundStream Stream);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_SetMode(Fmodtypes::PFSoundStream Stream, int mode);
extern DELPHI_PACKAGE int __stdcall FSOUND_Stream_GetMode(Fmodtypes::PFSoundStream Stream);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_SetLoopPoints(Fmodtypes::PFSoundStream Stream, int LoopStartPCM, int LoopEndPCM);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_SetLoopCount(Fmodtypes::PFSoundStream Stream, int Count);
extern DELPHI_PACKAGE Fmodtypes::PFSyncPoint __stdcall FSOUND_Stream_AddSyncPoint(Fmodtypes::PFSoundStream Stream, unsigned PCMOffset, char * Name);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_DeleteSyncPoint(Fmodtypes::PFSyncPoint Point);
extern DELPHI_PACKAGE int __stdcall FSOUND_Stream_GetNumSyncPoints(Fmodtypes::PFSoundStream Stream);
extern DELPHI_PACKAGE Fmodtypes::PFSyncPoint __stdcall FSOUND_Stream_GetSyncPoint(Fmodtypes::PFSoundStream Stream, int Index);
extern DELPHI_PACKAGE char * __stdcall FSOUND_Stream_GetSyncPointInfo(Fmodtypes::PFSyncPoint Point, unsigned &PCMOffset);
extern DELPHI_PACKAGE int __stdcall FSOUND_Stream_GetOpenState(Fmodtypes::PFSoundStream Stream);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_SetSubStream(Fmodtypes::PFSoundStream Stream, int Index);
extern DELPHI_PACKAGE int __stdcall FSOUND_Stream_GetNumSubStreams(Fmodtypes::PFSoundStream Stream);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_SetSubStreamSentence(Fmodtypes::PFSoundStream Stream, unsigned &SentenceList, int NumItems);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_GetNumTagFields(Fmodtypes::PFSoundStream Stream, int &Num);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_GetTagField(Fmodtypes::PFSoundStream Stream, int Num, Fmodtypes::TFSoundTagFieldType &TagType, char * &Name, void * &Value, int &Length);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_FindTagField(Fmodtypes::PFSoundStream Stream, Fmodtypes::TFSoundTagFieldType TagType, char * Name, void * &Value, int &Length);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_Net_SetProxy(char * Proxy);
extern DELPHI_PACKAGE char * __stdcall FSOUND_Stream_Net_GetLastServerStatus(void);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_Net_SetBufferProperties(int BufferSize, int PreBuffer_Percent, int ReBuffer_Percent);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_Net_GetBufferProperties(int &Buffersize, int &PreBuffer_Percent, int &ReBuffer_Percent);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_Net_SetMetadataCallback(Fmodtypes::PFSoundStream Stream, Fmodtypes::TFMetaDataCallback Callback, int UserData);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Stream_Net_GetStatus(Fmodtypes::PFSoundStream Stream, Fmodtypes::TFSoundStreamNetStatus &Status, int &BufferPercentUsed, int &BitRate, unsigned &Flags);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_CD_Play(System::Byte Drive, int Track);
extern DELPHI_PACKAGE void __stdcall FSOUND_CD_SetPlayMode(System::Byte Drive, int Mode);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_CD_Stop(System::Byte Drive);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_CD_SetPaused(System::Byte Drive, System::ByteBool Paused);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_CD_SetVolume(System::Byte Drive, int Volume);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_CD_SetTrackTime(System::Byte Drive, int ms);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_CD_OpenTray(System::Byte Drive, System::Byte Open);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_CD_GetPaused(System::Byte Drive);
extern DELPHI_PACKAGE int __stdcall FSOUND_CD_GetTrack(System::Byte Drive);
extern DELPHI_PACKAGE int __stdcall FSOUND_CD_GetNumTracks(System::Byte Drive);
extern DELPHI_PACKAGE int __stdcall FSOUND_CD_GetVolume(System::Byte Drive);
extern DELPHI_PACKAGE int __stdcall FSOUND_CD_GetTrackLength(System::Byte Drive, int Track);
extern DELPHI_PACKAGE int __stdcall FSOUND_CD_GetTrackTime(System::Byte Drive);
extern DELPHI_PACKAGE Fmodtypes::PFSoundDSPUnit __stdcall FSOUND_DSP_Create(Fmodtypes::TFSoundDSPCallback Callback, int Priority, int Param);
extern DELPHI_PACKAGE void __stdcall FSOUND_DSP_Free(Fmodtypes::PFSoundDSPUnit DSPUnit);
extern DELPHI_PACKAGE void __stdcall FSOUND_DSP_SetPriority(Fmodtypes::PFSoundDSPUnit DSPUnit, int Priority);
extern DELPHI_PACKAGE int __stdcall FSOUND_DSP_GetPriority(Fmodtypes::PFSoundDSPUnit DSPUnit);
extern DELPHI_PACKAGE void __stdcall FSOUND_DSP_SetActive(Fmodtypes::PFSoundDSPUnit DSPUnit, System::ByteBool Active);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_DSP_GetActive(Fmodtypes::PFSoundDSPUnit DSPUnit);
extern DELPHI_PACKAGE Fmodtypes::PFSoundDSPUnit __stdcall FSOUND_DSP_GetClearUnit(void);
extern DELPHI_PACKAGE Fmodtypes::PFSoundDSPUnit __stdcall FSOUND_DSP_GetSFXUnit(void);
extern DELPHI_PACKAGE Fmodtypes::PFSoundDSPUnit __stdcall FSOUND_DSP_GetMusicUnit(void);
extern DELPHI_PACKAGE Fmodtypes::PFSoundDSPUnit __stdcall FSOUND_DSP_GetClipAndCopyUnit(void);
extern DELPHI_PACKAGE Fmodtypes::PFSoundDSPUnit __stdcall FSOUND_DSP_GetFFTUnit(void);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_DSP_MixBuffers(void * DestBuffer, void * SrcBuffer, int Len, int Freq, int Vol, int Pan, unsigned Mode);
extern DELPHI_PACKAGE void __stdcall FSOUND_DSP_ClearMixBuffer(void);
extern DELPHI_PACKAGE int __stdcall FSOUND_DSP_GetBufferLength(void);
extern DELPHI_PACKAGE int __stdcall FSOUND_DSP_GetBufferLengthTotal(void);
extern DELPHI_PACKAGE Winapi::Windows::PSingle __stdcall FSOUND_DSP_GetSpectrum(void);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Reverb_SetProperties(Fmodtypes::TFSoundReverbProperties &Prop);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Reverb_GetProperties(Fmodtypes::TFSoundReverbProperties &Prop);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Reverb_SetChannelProperties(int Channel, Fmodtypes::TFSoundReverbChannelProperties &Prop);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Reverb_GetChannelProperties(int Channel, Fmodtypes::TFSoundReverbChannelProperties &Prop);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Record_SetDriver(int OutputType);
extern DELPHI_PACKAGE int __stdcall FSOUND_Record_GetNumDrivers(void);
extern DELPHI_PACKAGE char * __stdcall FSOUND_Record_GetDriverName(int Id);
extern DELPHI_PACKAGE int __stdcall FSOUND_Record_GetDriver(void);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Record_StartSample(Fmodtypes::PFSoundSample Sptr, System::ByteBool Loop);
extern DELPHI_PACKAGE System::ByteBool __stdcall FSOUND_Record_Stop(void);
extern DELPHI_PACKAGE int __stdcall FSOUND_Record_GetPosition(void);
extern DELPHI_PACKAGE void __stdcall FSOUND_File_SetCallbacks(Fmodtypes::TFSoundOpenCallback OpenCallback, Fmodtypes::TFSoundCloseCallback CloseCallback, Fmodtypes::TFSoundReadCallback ReadCallback, Fmodtypes::TFSoundSeekCallback SeekCallback, Fmodtypes::TFSoundTellCallback TellCallback);
extern DELPHI_PACKAGE Fmodtypes::PFMusicModule __stdcall FMUSIC_LoadSong(const char * Name);
extern DELPHI_PACKAGE Fmodtypes::PFMusicModule __stdcall FMUSIC_LoadSongEx(void * Name_Or_Data, int Offset, int Length, unsigned Mode, int &SampleList, int SampleListNum);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetOpenState(Fmodtypes::PFMusicModule Module);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_FreeSong(Fmodtypes::PFMusicModule Module);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_PlaySong(Fmodtypes::PFMusicModule Module);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_StopSong(Fmodtypes::PFMusicModule Module);
extern DELPHI_PACKAGE void __stdcall FMUSIC_StopAllSongs(void);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_SetZxxCallback(Fmodtypes::PFMusicModule Module, Fmodtypes::TFMusicCallback Callback);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_SetRowCallback(Fmodtypes::PFMusicModule Module, Fmodtypes::TFMusicCallback Callback, int RowStep);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_SetOrderCallback(Fmodtypes::PFMusicModule Module, Fmodtypes::TFMusicCallback Callback, int OrderStep);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_SetInstCallback(Fmodtypes::PFMusicModule Module, Fmodtypes::TFMusicCallback Callback, int Instrument);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_SetSample(Fmodtypes::PFMusicModule Module, int SampNo, Fmodtypes::PFSoundSample Sptr);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_SetUserData(Fmodtypes::PFMusicModule Module, int userdata);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_OptimizeChannels(Fmodtypes::PFMusicModule Module, int MaxChannels, int MinVolume);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_SetReverb(System::ByteBool Reverb);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_SetLooping(Fmodtypes::PFMusicModule Module, System::ByteBool Looping);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_SetOrder(Fmodtypes::PFMusicModule Module, int Order);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_SetPaused(Fmodtypes::PFMusicModule Module, System::ByteBool Pause);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_SetMasterVolume(Fmodtypes::PFMusicModule Module, int Volume);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_SetMasterSpeed(Fmodtypes::PFMusicModule Module, float speed);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_SetPanSeperation(Fmodtypes::PFMusicModule Module, float PanSep);
extern DELPHI_PACKAGE char * __stdcall FMUSIC_GetName(Fmodtypes::PFMusicModule Module);
extern DELPHI_PACKAGE Fmodtypes::TFMusicTypes __stdcall FMUSIC_GetType(Fmodtypes::PFMusicModule Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetNumOrders(Fmodtypes::PFMusicModule Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetNumPatterns(Fmodtypes::PFMusicModule Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetNumInstruments(Fmodtypes::PFMusicModule Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetNumSamples(Fmodtypes::PFMusicModule Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetNumChannels(Fmodtypes::PFMusicModule Module);
extern DELPHI_PACKAGE Fmodtypes::PFSoundSample __stdcall FMUSIC_GetSample(Fmodtypes::PFMusicModule Module, int SampNo);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetPatternLength(Fmodtypes::PFMusicModule Module, int OrderNo);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_IsFinished(Fmodtypes::PFMusicModule Module);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_IsPlaying(Fmodtypes::PFMusicModule Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetMasterVolume(Fmodtypes::PFMusicModule Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetGlobalVolume(Fmodtypes::PFMusicModule Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetOrder(Fmodtypes::PFMusicModule Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetPattern(Fmodtypes::PFMusicModule Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetSpeed(Fmodtypes::PFMusicModule Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetBPM(Fmodtypes::PFMusicModule Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetRow(Fmodtypes::PFMusicModule Module);
extern DELPHI_PACKAGE System::ByteBool __stdcall FMUSIC_GetPaused(Fmodtypes::PFMusicModule Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetTime(Fmodtypes::PFMusicModule Module);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetRealChannel(Fmodtypes::PFMusicModule Module, int modchannel);
extern DELPHI_PACKAGE int __stdcall FMUSIC_GetUserData(Fmodtypes::PFMusicModule Module);
}	/* namespace Fmod */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMOD)
using namespace Fmod;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FmodHPP
