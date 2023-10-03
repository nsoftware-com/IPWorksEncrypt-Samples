unit certmgrf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ipccore, ipctypes, ipccertmgr;

type
  TFormCertmgr = class(TForm)
    CertificateStore: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    AvailableCertificates: TListBox;
    CertificateInfo: TMemo;
    Label3: TLabel;
    ipcCertMgr1: TipcCertMgr;
    lblCaption: TLabel;
    cmdOK: TButton;


    procedure ipcCertmgr1StoreList(Sender: TObject;
      const CertStore: String);
    procedure FormCreate(Sender: TObject);
    procedure AvailableCertificatesClick(Sender: TObject);
    procedure CertificateStoreChange(Sender: TObject);
    procedure cmdOKClick(Sender: TObject);
    procedure ipcCertmgr1CertList(Sender: TObject; CertEncoded: string; CertEncodedB: TArray<System.Byte>;
      const CertSubject, CertIssuer, CertSerialNumber: String;
      HasPrivateKey: Boolean);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormCertmgr: TFormCertmgr;

implementation

{$R *.DFM}






procedure TFormCertmgr.ipcCertmgr1StoreList(Sender: TObject;
  const CertStore: String);
begin
        CertificateStore.Items.Add(CertStore);
end;

procedure TFormCertmgr.FormCreate(Sender: TObject);
begin
        ipcCertmgr1.ListCertificateStores();
  if CertificateStore.Items.Count > 0 then
  begin
  	CertificateStore.ItemIndex := 0;
	  CertificateStoreChange(nil);
  end;
end;

procedure TFormCertmgr.AvailableCertificatesClick(Sender: TObject);
begin
          ipcCertmgr1.CertSubject := AvailableCertificates.Items.Strings[AvailableCertificates.ItemIndex];
	CertificateInfo.Text :=
  	'Issuer: ' + ipcCertmgr1.CertIssuer + #13 + #10 +
		'Subject: ' + ipcCertmgr1.CertSubject + #13 + #10 +
		'Version: ' + ipcCertmgr1.CertVersion + #13 + #10 +
		'Serial Number: ' + ipcCertmgr1.CertSerialNumber + #13 + #10 +
		'Signature Algorithm: ' + ipcCertmgr1.CertSignatureAlgorithm + #13 + #10 +
		'Effective Date: ' + ipcCertmgr1.CertEffectiveDate + #13 + #10 +
		'Expiration Date: ' + ipcCertmgr1.CertExpirationDate + #13 + #10 +
		'Public Key Algorithm: ' + ipcCertmgr1.CertPublicKeyAlgorithm + #13 + #10 +
		'Public Key Length: ' + IntToStr(ipcCertmgr1.CertPublicKeyLength) + #13 + #10 +
		'Public Key: ' + ipcCertmgr1.CertPublicKey;
end;

procedure TFormCertmgr.CertificateStoreChange(Sender: TObject);
begin
        AvailableCertificates.Clear;
    CertificateInfo.Clear;
    ipcCertmgr1.CertStore := CertificateStore.Items.Strings[CertificateStore.ItemIndex];
    //ipcCertmgr1.Action := CertmgrListStoreCertificates;
    ipcCertmgr1.ListStoreCertificates();
    if AvailableCertificates.Items.Count > 0 then
  	begin
  		AvailableCertificates.ItemIndex := 0;
	  	AvailableCertificatesClick(nil);
    end;
end;

procedure TFormCertmgr.cmdOKClick(Sender: TObject);
begin
FormCertmgr.Close ;
end;

procedure TFormCertmgr.ipcCertmgr1CertList(Sender: TObject;
  CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer,
  CertSerialNumber: String; HasPrivateKey: Boolean);
begin
     AvailableCertificates.Items.Add(CertSubject);
end;

end.
