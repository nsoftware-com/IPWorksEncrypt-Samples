object FormHash: TFormHash
  Left = 0
  Top = 0
  Caption = 'Hash Demo'
  ClientHeight = 478
  ClientWidth = 672
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 355
    Height = 13
    Caption = 
      'This demo shows how to hash a file or string using a variety of ' +
      'algorithms.'
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 27
    Width = 657
    Height = 83
    Caption = 'Configuration'
    TabOrder = 0
    object Label3: TLabel
      Left = 16
      Top = 20
      Width = 76
      Height = 13
      Caption = 'Hash Algorithm:'
    end
    object Label4: TLabel
      Left = 279
      Top = 20
      Width = 69
      Height = 13
      Caption = 'Input Source: '
    end
    object cboAlgorithm: TComboBox
      Left = 98
      Top = 17
      Width = 148
      Height = 21
      ItemIndex = 0
      TabOrder = 0
      Text = 'SHA1'
      Items.Strings = (
        'SHA1'
        'SHA-224'
        'SHA-256'
        'SHA-384'
        'SHA-512'
        'MD2'
        'MD4'
        'MD5'
        'RIPEMD-160'
        'MD5SHA1'
        'HMAC-MD5'
        'HMAC-SHA1'
        'HMAC-SHA224'
        'HMAC-SHA256')
    end
    object chkEncode: TCheckBox
      Left = 508
      Top = 19
      Width = 145
      Height = 17
      Caption = 'Hex Encode Hash Value'
      Checked = True
      Ctl3D = False
      ParentCtl3D = False
      State = cbChecked
      TabOrder = 2
    end
    object cboInputSource: TComboBox
      Left = 354
      Top = 17
      Width = 148
      Height = 21
      ItemIndex = 0
      TabOrder = 1
      Text = 'String'
      OnChange = cboInputSourceChange
      Items.Strings = (
        'String'
        'File')
    end
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 447
    Width = 657
    Height = 25
    TabOrder = 1
  end
  object grpInputFile: TGroupBox
    Left = 8
    Top = 116
    Width = 657
    Height = 61
    Caption = 'Input File'
    TabOrder = 2
    object Label2: TLabel
      Left = 16
      Top = 23
      Width = 20
      Height = 13
      Caption = 'File:'
    end
    object txtInputFile: TEdit
      Left = 98
      Top = 20
      Width = 471
      Height = 21
      TabOrder = 0
    end
    object btnBrowseInput: TButton
      Left = 575
      Top = 16
      Width = 67
      Height = 25
      Caption = 'Browse...'
      TabOrder = 1
      OnClick = btnBrowseInputClick
    end
  end
  object grpInputString: TGroupBox
    Left = 8
    Top = 183
    Width = 657
    Height = 162
    Caption = 'Input String'
    TabOrder = 3
    object Label6: TLabel
      Left = 16
      Top = 23
      Width = 30
      Height = 13
      Caption = 'Input:'
    end
    object txtInputString: TMemo
      Left = 98
      Top = 30
      Width = 471
      Height = 123
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
  end
  object GroupBox5: TGroupBox
    Left = 8
    Top = 351
    Width = 657
    Height = 90
    Caption = 'Hash Value'
    TabOrder = 4
    object Label9: TLabel
      Left = 16
      Top = 58
      Width = 57
      Height = 13
      Caption = 'Hash Value:'
    end
    object txtHashValue: TEdit
      Left = 98
      Top = 55
      Width = 487
      Height = 21
      TabOrder = 0
    end
    object btnHash: TButton
      Left = 16
      Top = 24
      Width = 104
      Height = 25
      Caption = 'Compute Hash'
      TabOrder = 1
      OnClick = btnHashClick
    end
  end
  object dlgInputFile: TOpenDialog
    InitialDir = 'C:\'
    Left = 104
    Top = 72
  end
  object ipcHash1: TipcHash
    OnProgress = ipcHash1Progress
    Left = 160
    Top = 72
  end
end


