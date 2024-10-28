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
unit jwsf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, FileCtrl, ipccore, ipctypes, ipcjws;

type
  TFormJws = class(TForm)
    Label2: TLabel;
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
    Label7: TLabel;
    Label8: TLabel;
    btnSign: TButton;
    btnVerify: TButton;
    txtMessage: TMemo;
    txtSigned: TMemo;
    OpenDialog1: TOpenDialog;
    ipcJWS1: TipcJWS;
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
  FormJws: TFormJws;

implementation

{$R *.dfm}

procedure TFormJws.btnBrowsePrivateClick(Sender: TObject);
begin
  if (OpenDialog1.Execute()) then
  begin
    txtPrivateFile.Text := OpenDialog1.FileName;
    OpenDialog1.InitialDir := OpenDialog1.GetNamePath();
  end;
end;

procedure TFormJws.btnBrowsePublicClick(Sender: TObject);
begin
   if (OpenDialog1.Execute()) then
  begin
    txtPublicFile.Text := OpenDialog1.FileName;
    OpenDialog1.InitialDir := OpenDialog1.GetNamePath();
  end;
end;

procedure TFormJws.btnSignClick(Sender: TObject);
begin
ipcJWS1.Reset();
  if CompareStr(cboAlgorithm.Text,'HS256') = 0 then
  begin
    ipcJWS1.Algorithm:=jwsHS256;
	ipcJWS1.Config('KeyEncoding=1');
    ipcJWS1.Key:=txtKey.Text;
  end
  else if CompareStr(cboAlgorithm.Text,'HS384') = 0 then
  begin
    ipcJWS1.Algorithm:=jwsHS384;
	ipcJWS1.Config('KeyEncoding=1');
    ipcJWS1.Key:=txtKey.Text;
  end
  else if CompareStr(cboAlgorithm.Text,'HS512') = 0 then
  begin
    ipcJWS1.Algorithm:=jwsHS512;
	ipcJWS1.Config('KeyEncoding=1');
    ipcJWS1.Key:=txtKey.Text;
  end
  else if CompareStr(cboAlgorithm.Text,'RS256') = 0 then
  begin
    ipcJWS1.Algorithm:=jwsRS256;
    ipcJWS1.CertStoreType:=cstPFXFile;
    ipcJWS1.CertStore:=txtPrivateFile.Text;
    ipcJWS1.CertStorePassword:=txtPassword.Text;
    ipcJWS1.CertSubject:='*';
  end
  else if CompareStr(cboAlgorithm.Text,'RS384') = 0 then
  begin
    ipcJWS1.Algorithm:=jwsRS384;
    ipcJWS1.CertStoreType:=cstPFXFile;
    ipcJWS1.CertStore:=txtPrivateFile.Text;
    ipcJWS1.CertStorePassword:=txtPassword.Text;
    ipcJWS1.CertSubject:='*';
  end
  else if CompareStr(cboAlgorithm.Text,'RS512') = 0 then
  begin
    ipcJWS1.Algorithm:=jwsRS512;
    ipcJWS1.CertStoreType:=cstPFXFile;
    ipcJWS1.CertStore:=txtPrivateFile.Text;
    ipcJWS1.CertStorePassword:=txtPassword.Text;
    ipcJWS1.CertSubject:='*';
  end
  else if CompareStr(cboAlgorithm.Text,'PS256') = 0 then
  begin
    ipcJWS1.Algorithm:=jwsPS256;
    ipcJWS1.CertStoreType:=cstPFXFile;
    ipcJWS1.CertStore:=txtPrivateFile.Text;
    ipcJWS1.CertStorePassword:=txtPassword.Text;
    ipcJWS1.CertSubject:='*';
  end
  else if CompareStr(cboAlgorithm.Text,'PS384') = 0 then
  begin
    ipcJWS1.Algorithm:=jwsPS384;
    ipcJWS1.CertStoreType:=cstPFXFile;
    ipcJWS1.CertStore:=txtPrivateFile.Text;
    ipcJWS1.CertStorePassword:=txtPassword.Text;
    ipcJWS1.CertSubject:='*';
  end
  else if CompareStr(cboAlgorithm.Text,'PS512') = 0 then
  begin
    ipcJWS1.Algorithm:=jwsPS512;
    ipcJWS1.CertStoreType:=cstPFXFile;
    ipcJWS1.CertStore:=txtPrivateFile.Text;
    ipcJWS1.CertStorePassword:=txtPassword.Text;
    ipcJWS1.CertSubject:='*';
  end
  else if CompareStr(cboAlgorithm.Text,'ES256') = 0 then
  begin
    ipcJWS1.Algorithm:=jwsES256;
    ipcJWS1.CertStoreType:=cstPEMKeyFile;
    ipcJWS1.CertStore:=txtPrivateFile.Text;
    ipcJWS1.CertStorePassword:=txtPassword.Text;
    ipcJWS1.CertSubject:='*';
  end
  else if CompareStr(cboAlgorithm.Text,'ES384') = 0 then
  begin
    ipcJWS1.Algorithm:=jwsES384;
    ipcJWS1.CertStoreType:=cstPEMKeyFile;
    ipcJWS1.CertStore:=txtPrivateFile.Text;
    ipcJWS1.CertStorePassword:=txtPassword.Text;
    ipcJWS1.CertSubject:='*';
  end
  else if CompareStr(cboAlgorithm.Text,'ES512') = 0 then
  begin
    ipcJWS1.Algorithm:=jwsES512;
    ipcJWS1.CertStoreType:=cstPEMKeyFile;
    ipcJWS1.CertStore:=txtPrivateFile.Text;
    ipcJWS1.CertStorePassword:=txtPassword.Text;
    ipcJWS1.CertSubject:='*';
  end;
  ipcJWS1.InputMessage:=txtMessage.Text;
  ipcJWS1.Sign();
  txtSigned.Text:=ipcJWS1.OutputMessage;
  txtMessage.Text:='';
end;

procedure TFormJws.btnVerifyClick(Sender: TObject);
begin
ipcJWS1.Reset();
  if (CompareStr(cboAlgorithm.Text,'HS256') = 0) or (CompareStr(cboAlgorithm.Text,'HS384') = 0) or (CompareStr(cboAlgorithm.Text,'HS512') = 0) then
  begin
	ipcJWS1.Config('KeyEncoding=1');
    ipcJWS1.Key:=txtKey.Text;
  end
  else if (CompareStr(cboAlgorithm.Text,'RS256') = 0) or (CompareStr(cboAlgorithm.Text,'RS384') = 0) or (CompareStr(cboAlgorithm.Text,'RS512') = 0) or (CompareStr(cboAlgorithm.Text,'PS256') = 0) or (CompareStr(cboAlgorithm.Text,'PS384') = 0) or (CompareStr(cboAlgorithm.Text,'PS512') = 0) then
  begin
    ipcJWS1.CertStoreType:=cstPEMKeyFile;
    ipcJWS1.CertStore:=txtPublicFile.Text;
    ipcJWS1.CertSubject:='*';
  end
  else if (CompareStr(cboAlgorithm.Text,'ES256') = 0) or (CompareStr(cboAlgorithm.Text,'ES384') = 0) or (CompareStr(cboAlgorithm.Text,'ES512') = 0) then
  begin
    ipcJWS1.CertStoreType:=cstPublicKeyFile;
    ipcJWS1.CertStore:=txtPublicFile.Text;
    ipcJWS1.CertSubject:='*';
  end;
  ipcJWS1.InputMessage:=txtSigned.Text;
  ipcJWS1.Verify();
  txtMessage.Text:=ipcJWS1.OutputMessage;
  txtSigned.Text:='';
end;

procedure TFormJws.cboAlgorithmChange(Sender: TObject);
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

procedure TFormJws.FormShow(Sender: TObject);
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

