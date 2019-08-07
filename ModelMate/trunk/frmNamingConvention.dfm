object FormNamingConvention: TFormNamingConvention
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'UCODE Naming Convention'
  ClientHeight = 359
  ClientWidth = 716
  Color = clInactiveCaptionText
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  PixelsPerInch = 120
  TextHeight = 22
  object JvRichEdit1: TJvRichEdit
    Left = 0
    Top = 0
    Width = 716
    Height = 318
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabStop = False
    AdvancedTypography = True
    Align = alTop
    AutoAdvancedTypography = False
    BevelOuter = bvSpace
    BorderStyle = bsNone
    Color = clInactiveCaptionText
    Flat = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Arial'
    Font.Style = []
    Lines.Strings = (
      ''
      
        'User-specified names for parameters, observations, predictions, ' +
        'prior-information items, and '
      'groups need to conform to the following two rules:'
      ''
      
        'Rule 1. The first character needs to be a letter of the English ' +
        'alphabet.'
      ''
      
        'Rule 2. All characters after the first letter need to be a lette' +
        'r, digit, or member of the set: "_", ".", '
      
        '":", "&", "#", "@" (underscore, dot, colon, ampersand, number si' +
        'gn, at symbol).'
      ''
      
        'Names of parameters and all groups are limited to 12 characters.' +
        ' Names of observations, '
      
        'predictions, and prior-information items are limited to 20 chara' +
        'cters.')
    ParentFlat = False
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
  end
  object BitBtn1: TBitBtn
    Left = 309
    Top = 318
    Width = 96
    Height = 32
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    DoubleBuffered = True
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 0
  end
end
