//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("OfficeVCL5.res");
USEUNIT("autocombo.pas");
USEUNIT("basecombo.pas");
USEFORMNS("BaseComboForm.pas", Basecomboform, frmCustomCombo);
USEUNIT("colorcombo.pas");
USEUNIT("ColorCombo2000.pas");
USEFORMNS("ColorForm2000.pas", Colorform2000, frmColorCombo2000);
USEUNIT("combobox.pas");
USEUNIT("ComboEdit.pas");
USEFORMNS("ComboForm.pas", Comboform, frmCombo);
USEUNIT("fontcombo.pas");
USEUNIT("glyphcombo.pas");
USEUNIT("glyphdsgn.pas");
USEFORMNS("glyphform.pas", Glyphform, frmGlyphComboDsgn);
USEUNIT("mrucombo.pas");
USEUNIT("OfficeBalloon.pas");
USEUNIT("OfficeBar.pas");
USEUNIT("OfficeButtons.pas");
USEUNIT("officeconsts.pas");
USEUNIT("OfficeControls.pas");
USEUNIT("OfficeDateTime.pas");
USEUNIT("OfficeEdit.pas");
USEUNIT("OfficeHint.pas");
USEFORMNS("OfficeHintDsgn.pas", Officehintdsgn, frmHintShape);
USEUNIT("OfficePanel.pas");
USEUNIT("OfficeReg.pas");
USERES("OfficeReg.dcr");
USEUNIT("OfficeToolBar.pas");
USEUNIT("officetypes.pas");
USEUNIT("OfficeUtils.pas");
USEUNIT("SpectrumCombo.pas");
USEFORMNS("SpectrumComboForm.pas", Spectrumcomboform, frmSpectrumCombo);
USEUNIT("trackcombo.pas");
USEFORMNS("TrackComboForm.pas", Trackcomboform, frmTrackCombo);
USEUNIT("treecombo.pas");
USEFORMNS("TreeComboForm.pas", Treecomboform, frmTreeCombo);
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vclx50.bpi");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
