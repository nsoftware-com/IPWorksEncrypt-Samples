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
unit encryptf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, FileCtrl, ipccore, ipctypes, ipcezcrypt;

type
  TFormEncrypt = class(TForm)
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label2: TLabel;
    cboAlgorithms: TComboBox;
    txtKeyPassword: TEdit;
    ProgressBar1: TProgressBar;
    PageControl1: TPageControl;
    tsFile: TTabSheet;
    Label8: TLabel;
    GroupBox2: TGroupBox;
    rbEncrypt: TRadioButton;
    rbDecrypt: TRadioButton;
    txtInputFile: TEdit;
    txtOutputFile: TEdit;
    chbOverwrite: TCheckBox;
    btnGo: TButton;
    btnBrowseInput: TButton;
    btnBrowseOutput: TButton;
    tsString: TTabSheet;
    txtEncryptedString: TMemo;
    btnEncrypt: TButton;
    btnDecrypt: TButton;
    dlgInputFile: TOpenDialog;
    dlgOutputFile: TSaveDialog;
    txtDecryptedString: TMemo;
    Label5: TLabel;
    ipcEzCrypt1: TipcEzCrypt;
    procedure btnBrowseInputClick(Sender: TObject);
    procedure btnBrowseOutputClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure btnEncryptClick(Sender: TObject);
    procedure btnDecryptClick(Sender: TObject);
    procedure ipcEzCrypt1Progress(Sender: TObject; BytesProcessed: Int64;
      PercentProcessed: Integer);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormEncrypt: TFormEncrypt;

implementation

{$R *.dfm}

procedure TFormEncrypt.btnBrowseInputClick(Sender: TObject);
begin
  if (dlgInputFile.Execute()) then
  begin
    txtInputFile.Text := dlgInputFile.FileName;
    dlgInputFile.InitialDir := dlgInputFile.GetNamePath();
  end;
end;

procedure TFormEncrypt.btnBrowseOutputClick(Sender: TObject);
begin
  if (dlgOutputFile.Execute()) then
  begin
    txtOutputFile.Text := dlgOutputFile.FileName;
    dlgOutputFile.InitialDir := dlgOutputFile.GetNamePath();
  end;
end;

procedure TFormEncrypt.btnDecryptClick(Sender: TObject);
begin
  try
    begin
      ProgressBar1.Position := 0;
      ipcEzCrypt1.Algorithm := TipcEzCryptAlgorithms(cboAlgorithms.ItemIndex);
      ipcEzCrypt1.UseHex := true;
      ipcEzCrypt1.InputMessage := txtEncryptedString.Text;
      ipcEzCrypt1.KeyPassword := txtKeyPassword.Text;
      ipcEzCrypt1.Decrypt();
      txtDecryptedString.Text := ipcEzCrypt1.OutputMessage;
    end;
  Except
    on ipcex: Exception do
      Application.ShowException(&ipcex);
  end;
end;

procedure TFormEncrypt.btnEncryptClick(Sender: TObject);
begin
  try
    begin
      ProgressBar1.Position := 0;
      ipcEzCrypt1.Algorithm := TipcEzCryptAlgorithms(cboAlgorithms.ItemIndex);
      ipcEzCrypt1.UseHex := true;
      ipcEzCrypt1.InputMessage := txtDecryptedString.Text;
      ipcEzCrypt1.KeyPassword := txtKeyPassword.Text;
      ipcEzCrypt1.Encrypt();
      txtEncryptedString.Text := ipcEzCrypt1.OutputMessage;
    end;
  Except
    on ipcex: Exception do
      Application.ShowException(&ipcex);
  end;
end;

procedure TFormEncrypt.btnGoClick(Sender: TObject);
begin
  try
    begin
      Screen.Cursor := crHourGlass;
      ProgressBar1.Position := 0;
      ipcEzCrypt1.Algorithm := TipcEzCryptAlgorithms(cboAlgorithms.ItemIndex);
      ipcEzCrypt1.Overwrite := chbOverwrite.Checked;
      ipcEzCrypt1.InputFile := txtInputFile.Text;
      ipcEzCrypt1.OutputFile := txtOutputFile.Text;
      ipcEzCrypt1.KeyPassword := txtKeyPassword.Text;
      if (rbEncrypt.Checked) then
      begin
        ipcEzCrypt1.Encrypt();
      end
      else
      begin
        ipcEzCrypt1.Decrypt();
      end;
    end;
  Except
    on ipcex: Exception Do
      Application.ShowException(&ipcex);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormEncrypt.ipcEzCrypt1Progress(Sender: TObject;
  BytesProcessed: Int64; PercentProcessed: Integer);
begin
  ProgressBar1.Position := PercentProcessed;
end;

end.

