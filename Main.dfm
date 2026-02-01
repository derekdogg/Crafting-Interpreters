object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Form4'
  ClientHeight = 685
  ClientWidth = 1140
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
    Width = 104
    Height = 25
    Caption = 'Init Chunk'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 144
    Top = 32
    Width = 409
    Height = 417
    Lines.Strings = (
      '(1 + 2*3) - (4 / -5)')
    TabOrder = 1
  end
  object Button2: TButton
    Left = 569
    Top = 144
    Width = 72
    Height = 25
    Caption = 'Scanner >>'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Memo2: TMemo
    Left = 656
    Top = 32
    Width = 417
    Height = 417
    Lines.Strings = (
      'Memo2')
    TabOrder = 3
  end
  object Button3: TButton
    Left = 24
    Top = 80
    Width = 104
    Height = 25
    Caption = 'Test Stack'
    TabOrder = 4
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 24
    Top = 128
    Width = 104
    Height = 25
    Caption = 'Test Strings'
    TabOrder = 5
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 24
    Top = 176
    Width = 104
    Height = 25
    Caption = 'Test Chunk'
    TabOrder = 6
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 24
    Top = 216
    Width = 104
    Height = 25
    Caption = 'Add Value Constant'
    TabOrder = 7
    OnClick = Button6Click
  end
end
