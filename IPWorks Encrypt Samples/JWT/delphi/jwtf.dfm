object FormJWT: TFormJWT
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'FormJWT'
  ClientHeight = 476
  ClientWidth = 668
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
    Width = 443
    Height = 13
    Caption = 
      'This demo shows how to sign and verify JSON Web Tokens (JWT'#39's) u' +
      'sing various algorithms.'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 27
    Width = 640
    Height = 54
    Caption = 'Signing Info'
    TabOrder = 10
    object Label2: TLabel
      Left = 18
      Top = 24
      Width = 49
      Height = 13
      Caption = 'Algorithm:'
    end
  end
  object cboAlgorithm: TComboBox
    Left = 95
    Top = 48
    Width = 170
    Height = 21
    Style = csDropDownList
    DropDownCount = 12
    ItemIndex = 0
    TabOrder = 0
    Text = 'HS256'
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
    Top = 93
    Width = 641
    Height = 62
    Caption = 'HMAC'
    TabOrder = 6
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
    Top = 161
    Width = 640
    Height = 128
    Caption = 'RSA/ECDSA'
    Enabled = False
    TabOrder = 7
    object Label4: TLabel
      Left = 18
      Top = 27
      Width = 131
      Height = 13
      Caption = 'Private Key Certificate File:'
    end
    object Label5: TLabel
      Left = 16
      Top = 91
      Width = 124
      Height = 13
      Caption = 'Public Key Certificate File:'
    end
    object Label6: TLabel
      Left = 18
      Top = 59
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
      Enabled = False
      TabOrder = 1
      OnClick = btnBrowsePrivateClick
    end
    object txtPublicFile: TEdit
      Left = 155
      Top = 88
      Width = 390
      Height = 21
      TabOrder = 3
    end
    object btnBrowsePublic: TButton
      Left = 551
      Top = 90
      Width = 75
      Height = 25
      Caption = 'Browse'
      Enabled = False
      TabOrder = 4
      OnClick = btnBrowsePublicClick
    end
    object txtPassword: TEdit
      Left = 155
      Top = 55
      Width = 390
      Height = 21
      PasswordChar = '*'
      TabOrder = 2
    end
  end
  object btnSign: TButton
    Left = 294
    Top = 352
    Width = 75
    Height = 25
    Caption = 'Sign >>>'
    TabOrder = 8
    OnClick = btnSignClick
  end
  object btnVerify: TButton
    Left = 294
    Top = 383
    Width = 75
    Height = 25
    Caption = '<<< Verify'
    TabOrder = 9
    OnClick = btnVerifyClick
  end
  object GroupBox2: TGroupBox
    Left = 7
    Top = 303
    Width = 273
    Height = 164
    Caption = 'Claim Info'
    TabOrder = 11
    object Label9: TLabel
      Left = 16
      Top = 27
      Width = 76
      Height = 13
      Caption = 'Claim Audience:'
    end
    object Label10: TLabel
      Left = 16
      Top = 54
      Width = 68
      Height = 13
      Caption = 'Claim Subject:'
    end
    object Label11: TLabel
      Left = 16
      Top = 81
      Width = 62
      Height = 13
      Caption = 'Claim Issuer:'
    end
    object Label12: TLabel
      Left = 16
      Top = 108
      Width = 66
      Height = 13
      Caption = 'Claim JWT Id:'
    end
    object Label13: TLabel
      Left = 16
      Top = 135
      Width = 78
      Height = 13
      Caption = 'Claim Issued At:'
    end
  end
  object txtAud: TEdit
    Left = 105
    Top = 327
    Width = 159
    Height = 21
    TabOrder = 1
    Text = 'test'
  end
  object txtSub: TEdit
    Left = 106
    Top = 354
    Width = 158
    Height = 21
    TabOrder = 2
    Text = 'test'
  end
  object txtIss: TEdit
    Left = 105
    Top = 381
    Width = 159
    Height = 21
    TabOrder = 3
    Text = 'test'
  end
  object txtJti: TEdit
    Left = 105
    Top = 408
    Width = 159
    Height = 21
    TabOrder = 4
    Text = '1234'
  end
  object txtIat: TEdit
    Left = 105
    Top = 435
    Width = 159
    Height = 21
    TabOrder = 5
    Text = '123456'
  end
  object GroupBox3: TGroupBox
    Left = 383
    Top = 303
    Width = 265
    Height = 164
    Caption = 'Encoded JWT'
    TabOrder = 12
    object txtSigned: TMemo
      Left = 16
      Top = 19
      Width = 233
      Height = 134
      TabOrder = 0
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 624
    Top = 278
  end
  object ipaJWT1: TipaJWT
    CertStore = 'MY'
    RecipientCertStore = 'MY'
    SignerCertStore = 'MY'
    Left = 327
    Top = 416
  end
end


