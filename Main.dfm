object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Lox Interpreter'
  ClientHeight = 800
  ClientWidth = 1400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 300
    Height = 800
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object Label2: TLabel
      Left = 8
      Top = 8
      Width = 52
      Height = 15
      Caption = 'Test Tree'
    end
    object TestTree: TTreeView
      Left = 8
      Top = 28
      Width = 284
      Height = 680
      Anchors = [akLeft, akTop, akRight, akBottom]
      StateImages = StateImages
      ReadOnly = True
      TabOrder = 0
      OnClick = TestTreeClick
    end
    object BtnPopulate: TButton
      Left = 8
      Top = 714
      Width = 80
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Refresh'
      TabOrder = 1
      OnClick = BtnPopulateClick
    end
    object BtnRunSelected: TButton
      Left = 92
      Top = 714
      Width = 80
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Run Sel'
      TabOrder = 2
      OnClick = BtnRunSelectedClick
    end
    object BtnCheckAll: TButton
      Left = 8
      Top = 744
      Width = 66
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'All'
      TabOrder = 3
      OnClick = BtnCheckAllClick
    end
    object BtnUncheckAll: TButton
      Left = 78
      Top = 744
      Width = 66
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'None'
      TabOrder = 4
      OnClick = BtnUncheckAllClick
    end
    object BtnRunAll: TButton
      Left = 176
      Top = 714
      Width = 116
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Run All'
      TabOrder = 5
      OnClick = BtnRunAllClick
    end
    object ProgressBar1: TProgressBar
      Left = 8
      Top = 775
      Width = 284
      Height = 16
      Anchors = [akLeft, akRight, akBottom]
      TabOrder = 6
    end
    object LblStatus: TLabel
      Left = 148
      Top = 748
      Width = 40
      Height = 15
      Anchors = [akLeft, akBottom]
      Caption = 'Ready.'
    end
  end
  object Splitter1: TSplitter
    Left = 300
    Top = 0
    Width = 5
    Height = 800
  end
  object PanelRight: TPanel
    Left = 305
    Top = 0
    Width = 1095
    Height = 800
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 54
      Height = 15
      Caption = 'Script Pad'
    end
    object Memo1: TSynEdit
      Left = 8
      Top = 28
      Width = 1079
      Height = 420
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Consolas'
      Font.Style = []
      TabOrder = 0
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clGray
      Gutter.Font.Height = -11
      Gutter.Font.Name = 'Consolas'
      Gutter.Font.Style = []
      Gutter.ShowLineNumbers = True
      Gutter.AutoSize = True
      Lines.Strings = (
        '// Hello Lox!'
        'var x = (1 + 2*3) - (4 / -5);'
        'print x;')
    end
    object Button1: TButton
      Left = 8
      Top = 454
      Width = 100
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Run Script'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Label3: TLabel
      Left = 8
      Top = 486
      Width = 40
      Height = 15
      Anchors = [akLeft, akBottom]
      Caption = 'Output'
    end
    object Splitter2: TSplitter
      Left = 0
      Top = 445
      Width = 1095
      Height = 5
      Cursor = crVSplit
      Align = alNone
      Anchors = [akLeft, akRight, akBottom]
    end
    object Memo2: TMemo
      Left = 8
      Top = 506
      Width = 1079
      Height = 286
      Anchors = [akLeft, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Consolas'
      Font.Style = []
      ScrollBars = ssBoth
      TabOrder = 2
    end
  end
  object StateImages: TImageList
    Left = 200
    Top = 40
  end
end
