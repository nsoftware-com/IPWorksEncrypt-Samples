(*
 * IPWorks Encrypt 2024 Delphi Edition - Sample Project
 *
 * This sample project demonstrates the usage of IPWorks Encrypt in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/ipworksencrypt
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 *)
unit jwtf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, FileCtrl, ipacore, ipatypes, ipajwt;

type
  TFormJWT = class(TForm)
    cboAlgorithm: TComboBox;
    gbHmac: TGroupBox;
    Label3: TLabel;
    txtKey: TEdit;
    gbRsaEcdsa: TGroupBox;
    Label4: TLabel;
    txtPrivateFile: TEdit;
    btnBrowsePrivate: TButton;
    Label5: TLabel;
    txtPublicFile: TEdit;
    btnBrowsePublic: TButton;
    Label6: TLabel;
    txtPassword: TEdit;
    btnSign: TButton;
    btnVerify: TButton;
    txtSigned: TMemo;
    OpenDialog1: TOpenDialog;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    txtAud: TEdit;
    Label9: TLabel;
    txtSub: TEdit;
    Label10: TLabel;
    txtIss: TEdit;
    Label11: TLabel;
    txtJti: TEdit;
    Label12: TLabel;
    txtIat: TEdit;
    Label13: TLabel;
    ipaJWT1: TipaJWT;
    Label2: TLabel;
    GroupBox3: TGroupBox;
    procedure cboAlgorithmChange(Sender: TObject);
    procedure btnBrowsePrivateClick(Sender: TObject);
    procedure btnBrowsePublicClick(Sender: TObject);
    procedure btnSignClick(Sender: TObject);
    procedure btnVerifyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormJWT: TFormJWT;

implementation

{$R *.dfm}

procedure TFormJWT.btnBrowsePrivateClick(Sender: TObject);
begin
  if (OpenDialog1.Execute()) then
  begin
    txtPrivateFile.Text := OpenDialog1.FileName;
    OpenDialog1.InitialDir := OpenDialog1.GetNamePath();
  end;
end;

procedure TFormJWT.btnBrowsePublicClick(Sender: TObject);
begin
   if (OpenDialog1.Execute()) then
  begin
    txtPublicFile.Text := OpenDialog1.FileName;
    OpenDialog1.InitialDir := OpenDialog1.GetNamePath();
  end;
end;

procedure TFormJWT.btnSignClick(Sender: TObject);
begin
  ipaJWT1.Reset();
  if CompareStr(cboAlgorithm.Text,'HS256') = 0 then
  begin
    ipaJWT1.SigningAlgorithm:=TipajwtSigningAlgorithms.saHS256;
    ipaJWT1.Config('KeyEncoding=1');
    ipaJWT1.Key:=txtKey.Text;
  end
  else if CompareStr(cboAlgorithm.Text,'HS384') = 0 then
  begin
    ipaJWT1.SigningAlgorithm:=ipajwt.TipajwtSigningAlgorithms.saHS384;
    ipaJWT1.Config('KeyEncoding=1');
    ipaJWT1.Key:=txtKey.Text;
  end
  else if CompareStr(cboAlgorithm.Text,'HS512') = 0 then
  begin
    ipaJWT1.SigningAlgorithm:=ipajwt.TipajwtSigningAlgorithms.saHS512;
    ipaJWT1.Config('KeyEncoding=1');
    ipaJWT1.Key:=txtKey.Text;
  end
  else if CompareStr(cboAlgorithm.Text,'RS256') = 0 then
  begin
    ipaJWT1.SigningAlgorithm:=ipajwt.TipajwtSigningAlgorithms.saRS256;
    ipaJWT1.CertStoreType:=TipajwtCertStoreTypes.cstPFXFile;
    ipaJWT1.CertStore:=txtPrivateFile.Text;
    ipaJWT1.CertStorePassword:=txtPassword.Text;
    ipaJWT1.CertSubject:='*';
  end
  else if CompareStr(cboAlgorithm.Text,'RS384') = 0 then
  begin
    ipaJWT1.SigningAlgorithm:=ipajwt.TipajwtSigningAlgorithms.saRS384;
    ipaJWT1.CertStoreType:=TipajwtCertStoreTypes.cstPFXFile;
    ipaJWT1.CertStore:=txtPrivateFile.Text;
    ipaJWT1.CertStorePassword:=txtPassword.Text;
    ipaJWT1.CertSubject:='*';
  end
  else if CompareStr(cboAlgorithm.Text,'RS512') = 0 then
  begin
    ipaJWT1.SigningAlgorithm:=ipajwt.TipajwtSigningAlgorithms.saRS512;
    ipaJWT1.CertStoreType:=TipajwtCertStoreTypes.cstPFXFile;
    ipaJWT1.CertStore:=txtPrivateFile.Text;
    ipaJWT1.CertStorePassword:=txtPassword.Text;
    ipaJWT1.CertSubject:='*';
  end
  else if CompareStr(cboAlgorithm.Text,'PS256') = 0 then
  begin
    ipaJWT1.SigningAlgorithm:=ipajwt.TipajwtSigningAlgorithms.saPS256;
    ipaJWT1.CertStoreType:=TipajwtCertStoreTypes.cstPFXFile;
    ipaJWT1.CertStore:=txtPrivateFile.Text;
    ipaJWT1.CertStorePassword:=txtPassword.Text;
    ipaJWT1.CertSubject:='*';
  end
  else if CompareStr(cboAlgorithm.Text,'PS384') = 0 then
  begin
    ipaJWT1.SigningAlgorithm:=ipajwt.TipajwtSigningAlgorithms.saPS384;
    ipaJWT1.CertStoreType:=TipajwtCertStoreTypes.cstPFXFile;
    ipaJWT1.CertStore:=txtPrivateFile.Text;
    ipaJWT1.CertStorePassword:=txtPassword.Text;
    ipaJWT1.CertSubject:='*';
  end
  else if CompareStr(cboAlgorithm.Text,'PS512') = 0 then
  begin
    ipaJWT1.SigningAlgorithm:=ipajwt.TipajwtSigningAlgorithms.saPS512;
    ipaJWT1.CertStoreType:=TipajwtCertStoreTypes.cstPFXFile;
    ipaJWT1.CertStore:=txtPrivateFile.Text;
    ipaJWT1.CertStorePassword:=txtPassword.Text;
    ipaJWT1.CertSubject:='*';
  end
  else if CompareStr(cboAlgorithm.Text,'ES256') = 0 then
  begin
    ipaJWT1.SigningAlgorithm:=ipajwt.TipajwtSigningAlgorithms.saES256;
    ipaJWT1.CertStoreType:=TipajwtCertStoreTypes.cstPEMKeyFile;
    ipaJWT1.CertStore:=txtPrivateFile.Text;
    ipaJWT1.CertStorePassword:=txtPassword.Text;
    ipaJWT1.CertSubject:='*';
  end
  else if CompareStr(cboAlgorithm.Text,'ES384') = 0 then
  begin
    ipaJWT1.SigningAlgorithm:=ipajwt.TipajwtSigningAlgorithms.saES384;
    ipaJWT1.CertStoreType:=TipajwtCertStoreTypes.cstPEMKeyFile;
    ipaJWT1.CertStore:=txtPrivateFile.Text;
    ipaJWT1.CertStorePassword:=txtPassword.Text;
    ipaJWT1.CertSubject:='*';
  end
  else if CompareStr(cboAlgorithm.Text,'ES512') = 0 then
  begin
    ipaJWT1.SigningAlgorithm:=ipajwt.TipajwtSigningAlgorithms.saES512;
    ipaJWT1.CertStoreType:=TipajwtCertStoreTypes.cstPEMKeyFile;
    ipaJWT1.CertStore:=txtPrivateFile.Text;
    ipaJWT1.CertStorePassword:=txtPassword.Text;
    ipaJWT1.CertSubject:='*';
  end;
  ipaJWT1.ClaimAudience:=txtAud.Text;
  ipaJWT1.ClaimSubject:=txtSub.Text;
  ipaJWT1.ClaimIssuer:=txtIss.Text;
  ipaJWT1.ClaimJWTId:=txtJti.Text;
  ipaJWT1.ClaimIssuedAt:=txtIat.Text;
  ipaJWT1.Sign();
  txtSigned.Text:=ipaJWT1.EncodedJWT;
  txtAud.Text := '';
  txtSub.Text := '';
  txtIss.Text := '';
  txtJti.Text := '';
  txtIat.Text := '';
end;

procedure TFormJWT.btnVerifyClick(Sender: TObject);
begin

  ipaJWT1.Reset();
  if (CompareStr(cboAlgorithm.Text,'HS256') = 0) or (CompareStr(cboAlgorithm.Text,'HS384') = 0) or (CompareStr(cboAlgorithm.Text,'HS512') = 0) then
  begin
    ipaJWT1.Config('KeyEncoding=1');
    ipaJWT1.Key:=txtKey.Text;
  end
  else if (CompareStr(cboAlgorithm.Text,'RS256') = 0) or (CompareStr(cboAlgorithm.Text,'RS384') = 0) or (CompareStr(cboAlgorithm.Text,'RS512') = 0) or (CompareStr(cboAlgorithm.Text,'PS256') = 0) or (CompareStr(cboAlgorithm.Text,'PS384') = 0) or (CompareStr(cboAlgorithm.Text,'PS512') = 0) then
  begin
    ipaJWT1.SignerCertStoreType:=TipajwtCertStoreTypes.cstPEMKeyFile;
    ipaJWT1.SignerCertStore:=txtPublicFile.Text;
    ipaJWT1.SignerCertSubject:='*';
  end
  else if (CompareStr(cboAlgorithm.Text,'ES256') = 0) or (CompareStr(cboAlgorithm.Text,'ES384') = 0) or (CompareStr(cboAlgorithm.Text,'ES512') = 0) then
  begin
    ipaJWT1.SignerCertStoreType:=TipajwtCertStoreTypes.cstPublicKeyFile;
    ipaJWT1.SignerCertStore:=txtPublicFile.Text;
    ipaJWT1.SignerCertSubject:='*';
  end;
  ipaJWT1.EncodedJWT:=txtSigned.Text;
  ipaJWT1.Verify();
  txtSigned.Text:='';
  txtAud.Text := ipaJWT1.ClaimAudience;
  txtSub.Text := ipaJWT1.ClaimSubject;
  txtIss.Text := ipaJWT1.ClaimIssuer;
  txtJti.Text := ipaJWT1.ClaimJWTId;
  txtIat.Text := ipaJWT1.ClaimIssuedAt;
end;

procedure TFormJWT.cboAlgorithmChange(Sender: TObject);
begin
  if CompareStr(cboAlgorithm.Text,'HS256') = 0 then
  begin
    txtKey.Enabled:=true;
    txtPrivateFile.Enabled := false;
    btnBrowsePrivate.Enabled:=false;
    txtPassword.Enabled:=false;
    txtPublicFile.Enabled := false;
    btnBrowsePublic.Enabled := false;
    txtKey.Text := 'txAVam2uGT20a+ZJC1VWVGCM8tFYSKyJlw+2fgS/BdA=';
  end
  else if CompareStr(cboAlgorithm.Text,'HS384') = 0   then
  begin
    txtKey.Enabled:=true;
    txtPrivateFile.Enabled := false;
    btnBrowsePrivate.Enabled:=false;
    txtPassword.Enabled:=false;
    txtPublicFile.Enabled := false;
    btnBrowsePublic.Enabled := false;
    txtKey.Text := '5C/iq/SVHc1i++8elF0u3Cg8w1D1Nj8Idrsw2zzIQeLrolmPk5d26f6MxTE3Npy2';
  end
  else if CompareStr(cboAlgorithm.Text,'HS512') = 0 then
  begin
    txtKey.Enabled:=true;
    txtPrivateFile.Enabled := false;
    btnBrowsePrivate.Enabled:=false;
    txtPassword.Enabled:=false;
    txtPublicFile.Enabled := false;
    btnBrowsePublic.Enabled := false;
    txtKey.Text := 'AGVJSwvgVMU0cspZ7ChlxURcgCcdj7QV6nm0fr0C/rNtuh8F5uA7nCs4efKuWUDBw7/s9ikfTm0Kx4uZ3SYXcA==';
  end
  else if (CompareStr(cboAlgorithm.Text,'RS256') = 0) or (CompareStr(cboAlgorithm.Text,'RS384') = 0) or (CompareStr(cboAlgorithm.Text,'RS512') = 0) or (CompareStr(cboAlgorithm.Text,'PS256') = 0) or (CompareStr(cboAlgorithm.Text,'PS384') = 0) or (CompareStr(cboAlgorithm.Text,'PS512') = 0) then
  begin
    gbRsaEcdsa.Enabled:=true;
    txtKey.Enabled:=false;
    txtPrivateFile.Enabled := true;
    btnBrowsePrivate.Enabled:=true;
    txtPassword.Enabled:=true;
    txtPublicFile.Enabled := true;
    btnBrowsePublic.Enabled := true;
    txtPrivateFile.Text:='..\..\testrsapriv.pfx';
    txtPassword.Text:='test';
    txtPublicFile.Text:='..\..\testrsapub.cer';
  end
  else if (CompareStr(cboAlgorithm.Text,'ES256') = 0) or (CompareStr(cboAlgorithm.Text,'ES384') = 0) or (CompareStr(cboAlgorithm.Text,'ES512') = 0) then
  begin
    gbRsaEcdsa.Enabled:=true;
    txtKey.Enabled:=false;
    txtPrivateFile.Enabled := true;
    btnBrowsePrivate.Enabled:=true;
    txtPassword.Enabled:=true;
    txtPublicFile.Enabled := true;
    btnBrowsePublic.Enabled := true;
    txtPrivateFile.Text:='..\..\testeccpriv.txt';
    txtPassword.Text:='';
    txtPublicFile.Text:='..\..\testeccpub.txt';
  end;
end;

procedure TFormJWT.FormShow(Sender: TObject);
begin
  cboAlgorithm.ItemIndex:=0;
  txtKey.Enabled:=true;
  txtPrivateFile.Enabled := false;
  btnBrowsePrivate.Enabled:=false;
  txtPassword.Enabled:=false;
  txtPublicFile.Enabled := false;
  btnBrowsePublic.Enabled := false;
  txtKey.Text := 'txAVam2uGT20a+ZJC1VWVGCM8tFYSKyJlw+2fgS/BdA=';
end;

end.

