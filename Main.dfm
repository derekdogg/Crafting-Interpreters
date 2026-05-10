object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Lox Interpreter'
  ClientHeight = 730
  ClientWidth = 1300
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  OnDestroy = FormDestroy
  object Splitter1: TSplitter
    Left = 560
    Top = 0
    Width = 5
    Height = 730
    ExplicitLeft = 556
    ExplicitHeight = 685
  end
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 560
    Height = 730
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 54
      Height = 15
      Caption = 'Script Pad'
    end
    object Memo1: TMemo
      Left = 8
      Top = 28
      Width = 544
      Height = 200
      Lines.Strings = (
        '(1 + 2*3) - (4 / -5)')
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object Button1: TButton
      Left = 8
      Top = 234
      Width = 100
      Height = 25
      Caption = 'Run Script'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Label2: TLabel
      Left = 8
      Top = 268
      Width = 52
      Height = 15
      Caption = 'Test Tree'
    end
    object TestTree: TTreeView
      Left = 8
      Top = 288
      Width = 544
      Height = 360
      StateImages = StateImages
      ReadOnly = True
      TabOrder = 2
      OnClick = TestTreeClick
    end
    object BtnPopulate: TButton
      Left = 8
      Top = 656
      Width = 120
      Height = 25
      Caption = 'Refresh Tests'
      TabOrder = 3
      OnClick = BtnPopulateClick
    end
    object BtnRunSelected: TButton
      Left = 136
      Top = 656
      Width = 120
      Height = 25
      Caption = 'Run Selected'
      TabOrder = 4
      OnClick = BtnRunSelectedClick
    end
    object BtnCheckAll: TButton
      Left = 264
      Top = 656
      Width = 90
      Height = 25
      Caption = 'Check All'
      TabOrder = 5
      OnClick = BtnCheckAllClick
    end
    object BtnUncheckAll: TButton
      Left = 360
      Top = 656
      Width = 90
      Height = 25
      Caption = 'Uncheck All'
      TabOrder = 6
      OnClick = BtnUncheckAllClick
    end
    object BtnRunAll: TButton
      Left = 458
      Top = 656
      Width = 90
      Height = 25
      Caption = 'Run All'
      TabOrder = 7
      OnClick = BtnRunAllClick
    end
    object ProgressBar1: TProgressBar
      Left = 8
      Top = 690
      Width = 544
      Height = 16
      TabOrder = 8
    end
    object LblStatus: TLabel
      Left = 8
      Top = 710
      Width = 40
      Height = 15
      Caption = 'Ready.'
    end
  end
  object PanelRight: TPanel
    Left = 565
    Top = 0
    Width = 735
    Height = 730
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Label3: TLabel
      Left = 8
      Top = 8
      Width = 40
      Height = 15
      Caption = 'Output'
    end
    object Memo2: TMemo
      Left = 8
      Top = 28
      Width = 719
      Height = 694
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
  object StateImages: TImageList
    Left = 480
    Top = 288
  end
end
