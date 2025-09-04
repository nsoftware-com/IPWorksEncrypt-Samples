object FormEzrand: TFormEzrand
  Left = 0
  Top = 0
  Anchors = [akLeft, akTop, akRight, akBottom]
  Caption = 'EzRand Demo'
  ClientHeight = 310
  ClientWidth = 431
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    431
    310)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 0
    Width = 415
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'This demo shows how to generate random numbers and byte arrays u' +
      'sing various algorithms such as ISAAC, Crypto API, etc.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    ExplicitWidth = 421
  end
  object Label2: TLabel
    Left = 8
    Top = 43
    Width = 49
    Height = 13
    Caption = 'Algorithm:'
  end
  object Label5: TLabel
    Left = 219
    Top = 43
    Width = 28
    Height = 13
    Caption = 'Seed:'
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 67
    Width = 415
    Height = 235
    ActivePage = tsIntegers
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsIntegers: TTabSheet
      Caption = 'Integers'
      ExplicitWidth = 607
      ExplicitHeight = 206
      DesignSize = (
        407
        207)
      object Label3: TLabel
        Left = 3
        Top = 20
        Width = 49
        Height = 13
        Caption = 'Min Value:'
      end
      object Label4: TLabel
        Left = 111
        Top = 20
        Width = 53
        Height = 13
        Caption = 'Max Value:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clHighlight
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object txtMinValue: TEdit
        Left = 58
        Top = 17
        Width = 47
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clDefault
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        Text = '0'
      end
      object txtMaxValue: TEdit
        Left = 170
        Top = 17
        Width = 47
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clDefault
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        Text = '100'
      end
      object btnIntClear: TButton
        Left = 270
        Top = 10
        Width = 49
        Height = 28
        Anchors = [akTop, akRight]
        Caption = 'Clear'
        TabOrder = 3
        OnClick = btnIntClearClick
        ExplicitLeft = 470
      end
      object btnGetNextInt: TButton
        Left = 325
        Top = 10
        Width = 79
        Height = 28
        Anchors = [akTop, akRight]
        Caption = 'Get Next Int'
        TabOrder = 4
        OnClick = btnGetNextIntClick
        ExplicitLeft = 525
      end
      object txtIntOutput: TMemo
        Left = 3
        Top = 44
        Width = 401
        Height = 160
        Anchors = [akLeft, akTop, akRight, akBottom]
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 2
        ExplicitWidth = 601
        ExplicitHeight = 159
      end
    end
    object tsBytes: TTabSheet
      Caption = 'Bytes'
      ImageIndex = 1
      ExplicitWidth = 607
      ExplicitHeight = 206
      DesignSize = (
        407
        207)
      object Label7: TLabel
        Left = 3
        Top = 20
        Width = 67
        Height = 13
        Caption = 'Bytes Length:'
      end
      object txtBytesOutput: TMemo
        Left = 3
        Top = 44
        Width = 401
        Height = 160
        Anchors = [akLeft, akTop, akRight, akBottom]
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 3
        ExplicitWidth = 601
        ExplicitHeight = 159
      end
      object btnGetNextBytes: TButton
        Left = 325
        Top = 10
        Width = 79
        Height = 28
        Anchors = [akTop, akRight]
        Caption = 'Get Next Bytes'
        TabOrder = 2
        OnClick = btnGetNextBytesClick
        ExplicitLeft = 525
      end
      object btnBytesClear: TButton
        Left = 270
        Top = 10
        Width = 49
        Height = 28
        Anchors = [akTop, akRight]
        Caption = 'Clear'
        TabOrder = 1
        OnClick = btnBytesClearClick
        ExplicitLeft = 470
      end
      object txtBytesLength: TEdit
        Left = 76
        Top = 17
        Width = 125
        Height = 21
        TabOrder = 0
        Text = '16'
      end
    end
  end
  object cboAlgorithms: TComboBox
    Left = 63
    Top = 40
    Width = 150
    Height = 21
    TabOrder = 1
    Text = 'ISAAC'
    OnClick = cboAlgorithmsClick
    Items.Strings = (
      'ISAAC'
      'MS Crypto API'
      'Platform')
  end
  object txtSeed: TEdit
    Left = 253
    Top = 40
    Width = 150
    Height = 21
    TabOrder = 2
    OnChange = txtSeedChange
  end
  object ipcEzRand1: TipcEzRand
    Left = 248
    Top = 64
  end
end


