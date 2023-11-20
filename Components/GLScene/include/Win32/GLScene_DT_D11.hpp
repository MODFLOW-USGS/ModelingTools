// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLScene_DT_D11.dpk' rev: 35.00 (Windows)

#ifndef Glscene_dt_d11HPP
#define Glscene_dt_d11HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// (rtl)
#include <SysInit.hpp>
#include <FRFaceEditor.hpp>
#include <FRMaterialPreview.hpp>
#include <FRColorEditor.hpp>
#include <FRTextureEdit.hpp>
#include <FRTrackBarEdit.hpp>
#include <FLibMaterialPicker.hpp>
#include <FMaterialEditorForm.hpp>
#include <FGuiLayoutEditor.hpp>
#include <FGuiSkinEditor.hpp>
#include <FShaderMemo.hpp>
#include <FShaderUniformEditor.hpp>
#include <FVectorEditor.hpp>
#include <FSceneEditor.hpp>
#include <FPlugInManagerEditor.hpp>
#include <FXCollectionEditor.hpp>
#include <FInfo.hpp>
#include <GLSceneFormDesign.hpp>
#include <GLXCollectionRegister.hpp>
#include <GLSceneRegister.hpp>
#include <System.UITypes.hpp>	// (rtl)
#include <Winapi.Windows.hpp>	// (rtl)
#include <Winapi.PsAPI.hpp>	// (rtl)
#include <System.Character.hpp>	// (rtl)
#include <System.Internal.ExcUtils.hpp>	// (rtl)
#include <System.SysUtils.hpp>	// (rtl)
#include <System.VarUtils.hpp>	// (rtl)
#include <System.Variants.hpp>	// (rtl)
#include <System.TypInfo.hpp>	// (rtl)
#include <System.Math.hpp>	// (rtl)
#include <System.Generics.Defaults.hpp>	// (rtl)
#include <System.Rtti.hpp>	// (rtl)
#include <System.TimeSpan.hpp>	// (rtl)
#include <System.Classes.hpp>	// (rtl)
#include <System.Messaging.hpp>	// (rtl)
#include <System.Actions.hpp>	// (rtl)
#include <Winapi.ShellAPI.hpp>	// (rtl)
#include <System.DateUtils.hpp>	// (rtl)
#include <System.IOUtils.hpp>	// (rtl)
#include <System.IniFiles.hpp>	// (rtl)
#include <System.Win.Registry.hpp>	// (rtl)
#include <System.UIConsts.hpp>	// (rtl)
#include <Vcl.Graphics.hpp>	// (vcl)
#include <System.SyncObjs.hpp>	// (rtl)
#include <Winapi.UxTheme.hpp>	// (rtl)
#include <Vcl.ActnList.hpp>	// (vcl)
#include <System.AnsiStrings.hpp>	// (rtl)
#include <System.Win.ComObj.hpp>	// (rtl)
#include <Winapi.MsCTF.hpp>	// (rtl)
#include <Vcl.GraphUtil.hpp>	// (vcl)
#include <Vcl.Controls.hpp>	// (vcl)
#include <Vcl.StdCtrls.hpp>	// (vcl)
#include <Vcl.Clipbrd.hpp>	// (vcl)
#include <Vcl.Printers.hpp>	// (vcl)
#include <Vcl.ComCtrls.hpp>	// (vcl)
#include <System.HelpIntfs.hpp>	// (rtl)
#include <Vcl.Dialogs.hpp>	// (vcl)
#include <Vcl.ExtCtrls.hpp>	// (vcl)
#include <Vcl.Themes.hpp>	// (vcl)
#include <Vcl.Menus.hpp>	// (vcl)
#include <Winapi.FlatSB.hpp>	// (rtl)
#include <Vcl.Forms.hpp>	// (vcl)
#include <GLVectorGeometry.hpp>
#include <GLCrossPlatform.hpp>
#include <GLSLog.hpp>
#include <GLPersistentClasses.hpp>
#include <GLColor.hpp>
#include <Vcl.Imaging.jpeg.hpp>	// (vclimg)
#include <Vcl.Imaging.pngimage.hpp>	// (vclimg)
#include <GR32_System.hpp>	// (GR32_DSGN_RS11_1)
#include <GR32_Bindings.hpp>	// (GR32_DSGN_RS11_1)
#include <GR32_LowLevel.hpp>	// (GR32_DSGN_RS11_1)
#include <GR32_Blend.hpp>	// (GR32_DSGN_RS11_1)
#include <GR32_Filters.hpp>	// (GR32_DSGN_RS11_1)
#include <GR32_Rasterizers.hpp>	// (GR32_DSGN_RS11_1)
#include <GR32_Transforms.hpp>	// (GR32_DSGN_RS11_1)
#include <GR32_Resamplers.hpp>	// (GR32_DSGN_RS11_1)
#include <GR32_XPThemes.hpp>	// (GR32_DSGN_RS11_1)
#include <GR32_MicroTiles.hpp>	// (GR32_DSGN_RS11_1)
#include <GR32_Image.hpp>	// (GR32_DSGN_RS11_1)
#include <GR32_Backends_Generic.hpp>	// (GR32_DSGN_RS11_1)
#include <GR32_Backends_VCL.hpp>	// (GR32_DSGN_RS11_1)
#include <GR32.hpp>	// (GR32_DSGN_RS11_1)
#include <OpenGLAdapter.hpp>
#include <XOpenGL.hpp>
#include <GLContext.hpp>
#include <Vcl.Buttons.hpp>	// (vcl)
#include <Vcl.ExtDlgs.hpp>	// (vcl)
#include <GLGraphics.hpp>
#include <GLCoordinates.hpp>
#include <GLXCollection.hpp>
#include <GLVectorLists.hpp>
#include <GLMaterial.hpp>
#include <GLScene.hpp>
#include <GLTexture.hpp>
#include <GLObjects.hpp>
#include <GLBitmapFont.hpp>
#include <GLHUDObjects.hpp>
#include <GLWin32Context.hpp>
#include <GLWin32Viewer.hpp>
#include <Winapi.OpenGL.hpp>	// (rtl)
#include <GLTeapot.hpp>
#include <GLGeomObjects.hpp>
#include <GLProcTextures.hpp>
#include <GLTextureImageEditors.hpp>
#include <Vcl.Grids.hpp>	// (vcl)
#include <GLWindowsFont.hpp>
#include <GLWindows.hpp>
#include <GLSMemo.hpp>
#include <GLSLParameter.hpp>
#include <IDEMessages.hpp>	// (designide)
#include <Vcl.CaptionedDockTree.hpp>	// (vcl)
#include <Vcl.DockTabSet.hpp>	// (vcl)
#include <Vcl.CategoryButtons.hpp>	// (vcl)
#include <Vcl.ButtonGroup.hpp>	// (vcl)
#include <Vcl.SysStyles.hpp>	// (vcl)
#include <Winapi.D2D1.hpp>	// (rtl)
#include <Vcl.Direct2D.hpp>	// (vcl)
#include <Vcl.Styles.hpp>	// (vcl)
#include <Vcl.ImageCollection.hpp>	// (vclwinx)
#include <BrandingAPI.hpp>	// (designide)
#include <Winapi.GDIPOBJ.hpp>	// (rtl)
#include <DebugAPI.hpp>	// (designide)
#include <System.Devices.hpp>	// (rtl)
#include <Proxies.hpp>	// (designide)
#include <Vcl.AxCtrls.hpp>	// (vcl)
#include <Vcl.AppEvnts.hpp>	// (vcl)
#include <TreeIntf.hpp>	// (designide)
#include <TopLevels.hpp>	// (designide)
#include <StFilSys.hpp>	// (designide)
#include <IDEHelp.hpp>	// (designide)
#include <ComponentDesigner.hpp>	// (designide)
#include <IDETheme.Utils.hpp>	// (designide)
#include <PercentageDockTree.hpp>	// (designide)
#include <Vcl.WinXCtrls.hpp>	// (vclwinx)
#include <WaitDialog.hpp>	// (designide)
#include <Winapi.Mapi.hpp>	// (rtl)
#include <Vcl.ExtActns.hpp>	// (vcl)
#include <Vcl.ActnMenus.hpp>	// (vclactnband)
#include <Vcl.ActnMan.hpp>	// (vclactnband)
#include <Vcl.PlatformDefaultStyleActnCtrls.hpp>	// (vclactnband)
#include <BaseDock.hpp>	// (designide)
#include <DeskUtil.hpp>	// (designide)
#include <DeskForm.hpp>	// (designide)
#include <DockForm.hpp>	// (designide)
#include <Xml.Win.msxmldom.hpp>	// (xmlrtl)
#include <Xml.xmldom.hpp>	// (xmlrtl)
#include <ToolsAPI.hpp>	// (designide)
#include <DesignEditors.hpp>	// (designide)
#include <VCLEditors.hpp>	// (designide)
#include <GLObjectManager.hpp>
#include <GLAnimatedSprite.hpp>
#include <GLCadencer.hpp>
#include <GLCustomShader.hpp>
#include <GLAsmShader.hpp>
#include <GLHeightData.hpp>
#include <GLAsyncHDS.hpp>
#include <GLAsyncTimer.hpp>
#include <GLAtmosphere.hpp>
#include <GLBlur.hpp>
#include <GLBumpmapHDS.hpp>
#include <GLScreen.hpp>
#include <GLSmoothNavigator.hpp>
#include <GLMesh.hpp>
#include <GLMeshUtils.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLManager.hpp>
#include <GLCollision.hpp>
#include <GLCompositeImage.hpp>
#include <GLConsole.hpp>
#include <GLEllipseCollision.hpp>
#include <GLMultiPolygon.hpp>
#include <GLSpaceText.hpp>
#include <GLIsolines.hpp>
#include <GLROAMPatch.hpp>
#include <GLTerrainRenderer.hpp>
#include <GLProxyObjects.hpp>
#include <GLMultiProxy.hpp>
#include <GLDCE.hpp>
#include <GLDynamicTexture.hpp>
#include <GLParticleFX.hpp>
#include <GLExplosionFx.hpp>
#include <GLExtrusion.hpp>
#include <GLMultisampleImage.hpp>
#include <GLFBORenderer.hpp>
#include <GLFeedback.hpp>
#include <GLFireFX.hpp>
#include <GLFPSMovement.hpp>
#include <OpenGL1x.hpp>
#include <GLGameMenu.hpp>
#include <GLGraph.hpp>
#include <GLHeightTileFileHDS.hpp>
#include <GLImposter.hpp>
#include <GLLensFlare.hpp>
#include <GLLinePFX.hpp>
#include <GLTextureCombiners.hpp>
#include <GLMaterialEx.hpp>
#include <GLMaterialMultiProxy.hpp>
#include <GLMirror.hpp>
#include <GLParticles.hpp>
#include <GLPerlin.hpp>
#include <GLPerlinPFX.hpp>
#include <GLPhongShader.hpp>
#include <GLPolyhedron.hpp>
#include <GLPortal.hpp>
#include <GLPostEffects.hpp>
#include <GLProjectedTextures.hpp>
#include <GLSArchiveManager.hpp>
#include <GLScriptBase.hpp>
#include <GLShaderCombiner.hpp>
#include <GLShadowHDS.hpp>
#include <GLShadowPlane.hpp>
#include <GLShadowVolume.hpp>
#include <GLSceneForm.hpp>
#include <GLSkyBox.hpp>
#include <GLSkydome.hpp>
#include <GLSLanguage.hpp>
#include <GLSLShader.hpp>
#include <GLSLBumpShader.hpp>
#include <GLSLDiffuseSpecularShader.hpp>
#include <GLSLProjectedTextures.hpp>
#include <GLTexLensFlare.hpp>
#include <GLTexturedHDS.hpp>
#include <GLTextureSharingShader.hpp>
#include <GLThorFX.hpp>
#include <GLTilePlane.hpp>
#include <GLTrail.hpp>
#include <GLTree.hpp>
#include <GLFileTIN.hpp>
#include <GLWaterPlane.hpp>
#include <GLzBuffer.hpp>
#include <GLFileDDS.hpp>
#include <DDSImage.hpp>
#include <GLFileTGA.hpp>
#include <GLFile3DSSceneObjects.hpp>
#include <GLFile3DS.hpp>
#include <GLFileASE.hpp>
#include <GLFileB3D.hpp>
#include <GLFileGL2.hpp>
#include <GLFileGTS.hpp>
#include <GLFileLMTS.hpp>
#include <LWObjects.hpp>
#include <GLFileLWO.hpp>
#include <GLFileMD2.hpp>
#include <GLFileMD3.hpp>
#include <GLFileMD5.hpp>
#include <GLFileMDC.hpp>
#include <GLFileMS3D.hpp>
#include <GLFileNMF.hpp>
#include <GLFileNurbs.hpp>
#include <GLFileOBJ.hpp>
#include <GLFileOCT.hpp>
#include <GLFilePLY.hpp>
#include <GLBSP.hpp>
#include <GLFileQ3BSP.hpp>
#include <GLFileSMD.hpp>
#include <GLFileSTL.hpp>
#include <GLFileVRML.hpp>
#include <GLSoundFileObjects.hpp>
#include <GLFileWAV.hpp>
#include <GLFileMP3.hpp>
#include <GLFileO3TC.hpp>
#include <GLFileHDR.hpp>
#include <GLFileJPEG.hpp>
#include <GLFilePNG.hpp>
#include <GLFileBMP.hpp>
#include <FileTGA.hpp>
#include <GLSound.hpp>
#include <GLJoystick.hpp>
#include <GLScreenSaver.hpp>
#include <GLFullScreenViewer.hpp>
#include <GLBehaviours.hpp>
#include <GLApplicationFileIO.hpp>
#include <GLBaseClasses.hpp>
#include <GLVectorTypes.hpp>
#include <GLState.hpp>
#include <OpenGLTokens.hpp>
#include <GLTextureFormat.hpp>
#include <GLPipelineTransformation.hpp>
#include <GLUtils.hpp>
#include <GLSelection.hpp>
#include <GLImageUtils.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLGeometryBB.hpp>
#include <GLSilhouette.hpp>
#include <DXTC.hpp>
#include <VRMLParser.hpp>
#include <GLBaseMeshSilhouette.hpp>
#include <GLTypes.hpp>
#include <GLOctree.hpp>
#include <Q3BSP.hpp>
#include <FileOCT.hpp>
#include <GLParametricSurfaces.hpp>
#include <GLCurvesAndSurfaces.hpp>
#include <FileMD3.hpp>
#include <FileMD2.hpp>
#include <FileGL2.hpp>
#include <FileB3D.hpp>
#include <Types3DS.hpp>
#include <File3DS.hpp>
#include <GLSpline.hpp>
#include <GLNodes.hpp>
#include <GLGui.hpp>
#include <GLVfsPAK.hpp>
#include <GLUserShader.hpp>
#include <GLTimeEventsMgr.hpp>
#include <GLTexCombineShader.hpp>
#include <GLPolynomials.hpp>
#include <GLNavigator.hpp>
#include <GLSLPostShaders.hpp>
#include <GLSimpleNavigation.hpp>
#include <GLPerlinNoise3D.hpp>
#include <GLPerlinBase.hpp>
#include <GLOutlineShader.hpp>
#include <GLMultiMaterialShader.hpp>
#include <GLMaterialScript.hpp>
#include <GLHiddenLineShader.hpp>
#include <GLHeightTileFile.hpp>
#include <GLGizmo.hpp>
#include <GLCanvas.hpp>
#include <GLFBO.hpp>
#include <GLEParticleMasksManager.hpp>
#include <GLCelShader.hpp>
#include <GLCameraController.hpp>
#include <GLBumpShader.hpp>
#include <GLAVIRecorder.hpp>
#include <GLSVfw.hpp>
#include <GLPlugInManager.hpp>
#include <GLPlugInIntf.hpp>
#include <GLStrings.hpp>
#include <GLPictureRegisteredFormats.hpp>
#include <Utils3DS.hpp>
#include <Const3DS.hpp>
#include <GLSRGBE.hpp>
#include <GLDCEMisc.hpp>
// PRG_EXT: .bpl
// BPI_DIR: ..\..\lib\Win32
// OBJ_DIR: ..\..\lib\Win32
// OBJ_EXT: .obj

//-- user supplied -----------------------------------------------------------

namespace Glscene_dt_d11
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glscene_dt_d11 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSCENE_DT_D11)
using namespace Glscene_dt_d11;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glscene_dt_d11HPP
