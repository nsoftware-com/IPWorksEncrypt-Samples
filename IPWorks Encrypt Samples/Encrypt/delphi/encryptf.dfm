object FormEncrypt: TFormEncrypt
  Left = 0
  Top = 0
  Caption = 'Encrypt Demo'
  ClientHeight = 388
  ClientWidth = 813
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
    Width = 441
    Height = 13
    Caption = 
      'This demo shows how to encrypt and decrypt using symmetric algor' +
      'ithms like AES, DES, etc.'
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 27
    Width = 793
    Height = 83
    Caption = 'Configuration'
    TabOrder = 0
    object Label3: TLabel
      Left = 16
      Top = 20
      Width = 103
      Height = 13
      Caption = 'Encryption Algorithm:'
    end
    object Label4: TLabel
      Left = 320
      Top = 20
      Width = 74
      Height = 13
      Caption = 'Key Password: '
    end
    object Label2: TLabel
      Left = 560
      Top = 17
      Width = 216
      Height = 52
      Caption = 
        'A key password is used to generate the Key and IV. When using a ' +
        'key password you only need to save this value. This makes key ma' +
        'nagement much simpler.'
      WordWrap = True
    end
    object cboAlgorithms: TComboBox
      Left = 125
      Top = 17
      Width = 148
      Height = 21
      ItemIndex = 0
      TabOrder = 0
      Text = 'AES'
      Items.Strings = (
        'AES'
        'Blowfish'
        'CAST'
        'DES'
        'IDEA'
        'RC2'
        'RC4'
        'TEA'
        'TripleDES'
        'Twofish')
    end
    object txtKeyPassword: TEdit
      Left = 400
      Top = 17
      Width = 140
      Height = 21
      TabOrder = 1
      Text = 'password'
    end
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 356
    Width = 793
    Height = 25
    TabOrder = 1
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 116
    Width = 793
    Height = 234
    ActivePage = tsFile
    TabOrder = 2
    object tsFile: TTabSheet
      Caption = 'File to File'
      object Label7: TLabel
        Left = 15
        Top = 79
        Width = 49
        Height = 13
        Caption = 'Input File:'
      end
      object Label8: TLabel
        Left = 15
        Top = 125
        Width = 57
        Height = 13
        Caption = 'Output File:'
      end
      object GroupBox2: TGroupBox
        Left = 15
        Top = 16
        Width = 194
        Height = 57
        Caption = 'Action'
        TabOrder = 0
        object rbEncrypt: TRadioButton
          Left = 16
          Top = 24
          Width = 73
          Height = 17
          Caption = 'Encrypt'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object rbDecrypt: TRadioButton
          Left = 109
          Top = 23
          Width = 73
          Height = 17
          Caption = 'Decrypt'
          TabOrder = 1
        end
      end
      object txtInputFile: TEdit
        Left = 15
        Top = 98
        Width = 657
        Height = 21
        TabOrder = 1
      end
      object txtOutputFile: TEdit
        Left = 15
        Top = 144
        Width = 657
        Height = 21
        TabOrder = 2
      end
      object chbOverwrite: TCheckBox
        Left = 15
        Top = 171
        Width = 177
        Height = 17
        Caption = 'Overwrite Existing File'
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 3
      end
      object btnGo: TButton
        Left = 384
        Top = 171
        Width = 75
        Height = 25
        Caption = 'Go!'
        TabOrder = 4
        OnClick = btnGoClick
      end
      object btnBrowseInput: TButton
        Left = 678
        Top = 96
        Width = 94
        Height = 25
        Caption = 'Browse...'
        TabOrder = 5
        OnClick = btnBrowseInputClick
      end
      object btnBrowseOutput: TButton
        Left = 678
        Top = 141
        Width = 94
        Height = 25
        Caption = 'Browse...'
        TabOrder = 6
        OnClick = btnBrowseOutputClick
      end
    end
    object tsString: TTabSheet
      Caption = 'String to String'
      ImageIndex = 1
      object Label5: TLabel
        Left = 335
        Top = 104
        Width = 115
        Height = 52
        Caption = 
          'For the purposes of this demo the encrypted data is hex encoded ' +
          'for simplicity of display.'
        WordWrap = True
      end
      object txtDecryptedString: TMemo
        Left = 3
        Top = 7
        Width = 326
        Height = 196
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
      object txtEncryptedString: TMemo
        Left = 456
        Top = 7
        Width = 326
        Height = 196
        ScrollBars = ssBoth
        TabOrder = 1
        WordWrap = False
      end
      object btnEncrypt: TButton
        Left = 335
        Top = 24
        Width = 115
        Height = 25
        Caption = 'Encrypt->'
        TabOrder = 2
        OnClick = btnEncryptClick
      end
      object btnDecrypt: TButton
        Left = 335
        Top = 55
        Width = 115
        Height = 25
        Caption = '<-Decrypt'
        TabOrder = 3
        OnClick = btnDecryptClick
      end
    end
  end
  object dlgInputFile: TOpenDialog
    InitialDir = 'C:\'
    Left = 72
    Top = 72
  end
  object dlgOutputFile: TSaveDialog
    InitialDir = 'C:\'
    Left = 128
    Top = 72
  end
  object ipcEzCrypt1: TipcEzCrypt
    OnProgress = ipcEzCrypt1Progress
    Left = 200
    Top = 72
  end
end


