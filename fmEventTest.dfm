object frmEventTest: TfrmEventTest
  Left = 0
  Top = 0
  Caption = 'Event Callback Test'
  ClientHeight = 400
  ClientWidth = 500
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  TextHeight = 15
  object Memo1: TMemo
    Left = 0
    Top = 240
    Width = 500
    Height = 160
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 500
    Height = 200
    Align = alClient
    Caption = 'Click here or press keys'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = Panel1Click
    OnMouseDown = Panel1MouseDown
    OnMouseUp = Panel1MouseUp
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 200
    Width = 500
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 100
      Height = 25
      Caption = 'Button 1'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 116
      Top = 8
      Width = 100
      Height = 25
      Caption = 'Button 2'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
end
