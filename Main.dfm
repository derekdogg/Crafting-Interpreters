object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Form4'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Button1: TButton
    Left = 24
    Top = 24
    Width = 89
    Height = 25
    Caption = 'Init Chunk'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 144
    Top = 32
    Width = 409
    Height = 369
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object Button2: TButton
    Left = 24
    Top = 64
    Width = 89
    Height = 25
    Caption = 'Scanner'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 32
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Button3'
    TabOrder = 3
    OnClick = Button3Click
  end
end
