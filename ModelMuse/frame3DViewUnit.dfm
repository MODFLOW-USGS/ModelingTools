object frame3DView: Tframe3DView
  Left = 0
  Top = 0
  Width = 315
  Height = 182
  TabOrder = 0
  TabStop = True
  object glWidModelView: TGLWidget
    Left = 0
    Top = 0
    Width = 315
    Height = 182
    Hint = '3D View of model'
    Options = []
    Align = alClient
    ShowHint = True
    TabOrder = 0
    OnMouseDown = glWidModelViewMouseDown
    OnMouseMove = glWidModelViewMouseMove
    OnMouseUp = glWidModelViewMouseUp
    OnResize = glWidModelViewResize
    OnRender = glWidModelViewRender
  end
end
