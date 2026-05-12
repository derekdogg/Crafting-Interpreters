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
  KeyPreview = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 300
    Top = 0
    Width = 5
    Height = 800
  end
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 300
    Height = 800
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      300
      800)
    object Label2: TLabel
      Left = 8
      Top = 8
      Width = 46
      Height = 15
      Caption = 'Test Tree'
    end
    object LblStatus: TLabel
      Left = 148
      Top = 748
      Width = 35
      Height = 15
      Anchors = [akLeft, akBottom]
      Caption = 'Ready.'
    end
    object TestTree: TTreeView
      Left = 8
      Top = 28
      Width = 284
      Height = 680
      Anchors = [akLeft, akTop, akRight, akBottom]
      Indent = 19
      ReadOnly = True
      StateImages = StateImages
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
  end
  object PanelRight: TPanel
    Left = 305
    Top = 0
    Width = 1095
    Height = 800
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      1095
      800)
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 53
      Height = 15
      Caption = 'Script Pad'
    end
    object Label3: TLabel
      Left = 8
      Top = 486
      Width = 38
      Height = 15
      Anchors = [akLeft, akBottom]
      Caption = 'Output'
    end
    object Splitter2: TSplitter
      Left = 0
      Top = 445
      Width = 1095
      Height = 5
      Align = alNone
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
      Font.Quality = fqClearTypeNatural
      TabOrder = 0
      UseCodeFolding = False
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clGray
      Gutter.Font.Height = -11
      Gutter.Font.Name = 'Consolas'
      Gutter.Font.Style = []
      Gutter.Font.Quality = fqClearTypeNatural
      Gutter.ShowLineNumbers = True
      Gutter.Bands = <
        item
          Kind = gbkMarks
          Width = 13
        end
        item
          Kind = gbkLineNumbers
        end
        item
          Kind = gbkFold
        end
        item
          Kind = gbkTrackChanges
        end
        item
          Kind = gbkMargin
          Width = 3
        end>
      Lines.Strings = (
        'print "Press BtnTestClick to see event enque working";'
        'var running = true;'
        'while (running) {'
        '  processMessages();'
        '  while (events.hasItems()) {'
        '    var e = events.dequeue();'
        '    print e;'
        '    if (e == "keydown:escape") running = false;'
        '  }'
        '}'
        'print "Done.";')
      SelectedColor.Alpha = 0.400000005960464500
    end
    object Button1: TButton
      Left = 8
      Top = 454
      Width = 120
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Run Script (F5)'
      TabOrder = 1
      OnClick = Button1Click
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
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 2
    end
    object Button2: TButton
      Left = 352
      Top = 456
      Width = 75
      Height = 25
      Caption = 'BtnTestClick'
      TabOrder = 3
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 433
      Top = 456
      Width = 75
      Height = 25
      Caption = 'Button3'
      TabOrder = 4
      OnClick = Button3Click
    end
  end
  object StateImages: TImageList
    Left = 200
    Top = 40
  end
end
