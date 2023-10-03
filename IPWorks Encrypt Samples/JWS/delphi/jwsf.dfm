object FormJws: TFormJws
  Left = 0
  Top = 0
  Caption = 'JWS Demo'
  ClientHeight = 572
  ClientWidth = 656
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 421
    Height = 13
    Caption = 
      'This demo shows how to sign and verify JSON Web Signatures using' +
      ' various algorithms.'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 35
    Width = 49
    Height = 13
    Caption = 'Algorithm:'
  end
  object Label7: TLabel
    Left = 8
    Top = 293
    Width = 42
    Height = 13
    Caption = 'Payload:'
  end
  object Label8: TLabel
    Left = 335
    Top = 293
    Width = 56
    Height = 13
    Caption = 'JWS String:'
  end
  object cboAlgorithm: TComboBox
    Left = 63
    Top = 32
    Width = 145
    Height = 21
    Style = csDropDownList
    DropDownCount = 12
    TabOrder = 0
    OnChange = cboAlgorithmChange
    Items.Strings = (
      'HS256'
      'HS384'
      'HS512'
      'RS256'
      'RS384'
      'RS512'
      'PS256'
      'PS384'
      'PS512'
      'ES256'
      'ES384'
      'ES512')
  end
  object gbHmac: TGroupBox
    Left = 8
    Top = 59
    Width = 641
    Height = 62
    Caption = 'HMAC'
    TabOrder = 1
    object Label3: TLabel
      Left = 18
      Top = 24
      Width = 22
      Height = 13
      Caption = 'Key:'
    end
    object txtKey: TEdit
      Left = 55
      Top = 24
      Width = 570
      Height = 21
      TabOrder = 0
      Text = 'txAVam2uGT20a+ZJC1VWVGCM8tFYSKyJlw+2fgS/BdA='
    end
  end
  object gbRsaEcdsa: TGroupBox
    Left = 8
    Top = 142
    Width = 640
    Height = 145
    Caption = 'RSA/ECDSA'
    TabOrder = 2
    object Label4: TLabel
      Left = 18
      Top = 27
      Width = 131
      Height = 13
      Caption = 'Private Key Certificate File:'
    end
    object Label5: TLabel
      Left = 18
      Top = 107
      Width = 124
      Height = 13
      Caption = 'Public Key Certificate File:'
    end
    object Label6: TLabel
      Left = 18
      Top = 67
      Width = 108
      Height = 13
      Caption = 'Private Key Password:'
    end
    object txtPrivateFile: TEdit
      Left = 155
      Top = 24
      Width = 390
      Height = 21
      TabOrder = 0
    end
    object btnBrowsePrivate: TButton
      Left = 551
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Browse'
      TabOrder = 1
      OnClick = btnBrowsePrivateClick
    end
    object txtPublicFile: TEdit
      Left = 155
      Top = 104
      Width = 390
      Height = 21
      TabOrder = 2
    end
    object btnBrowsePublic: TButton
      Left = 551
      Top = 104
      Width = 75
      Height = 25
      Caption = 'Browse'
      TabOrder = 3
      OnClick = btnBrowsePublicClick
    end
    object txtPassword: TEdit
      Left = 155
      Top = 64
      Width = 390
      Height = 21
      PasswordChar = '*'
      TabOrder = 4
    end
  end
  object btnSign: TButton
    Left = 133
    Top = 539
    Width = 75
    Height = 25
    Caption = 'Sign >>>'
    TabOrder = 3
    OnClick = btnSignClick
  end
  object btnVerify: TButton
    Left = 464
    Top = 539
    Width = 75
    Height = 25
    Caption = '<<< Verify'
    TabOrder = 4
    OnClick = btnVerifyClick
  end
  object txtMessage: TMemo
    Left = 8
    Top = 312
    Width = 313
    Height = 221
    TabOrder = 5
  end
  object txtSigned: TMemo
    Left = 335
    Top = 312
    Width = 313
    Height = 221
    TabOrder = 6
  end
  object OpenDialog1: TOpenDialog
    Left = 584
    Top = 198
  end
  object ipcJWS1: TipcJWS
    CertStore = 'MY'
    Left = 320
    Top = 528
  end
end


