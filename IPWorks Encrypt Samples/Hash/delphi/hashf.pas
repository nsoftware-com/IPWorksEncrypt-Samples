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
unit hashf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, FileCtrl, ipccore, ipctypes, ipchash;

type
  TFormHash = class(TForm)
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label2: TLabel;
    ProgressBar1: TProgressBar;
    txtInputFile: TEdit;
    dlgInputFile: TOpenDialog;
    cboInputSource: TComboBox;
    cboAlgorithm: TComboBox;
    grpInputFile: TGroupBox;
    grpInputString: TGroupBox;
    chkEncode: TCheckBox;
    txtInputString: TMemo;
    txtHashValue: TEdit;
    ipcHash1: TipcHash;
    btnHash: TButton;
    procedure ipcHash1Progress(Sender: TObject; BytesProcessed: Int64;
      PercentProcessed: Integer);
    procedure cboInputSourceChange(Sender: TObject);
    procedure btnBrowseInputClick(Sender: TObject);
    procedure btnHashClick(Sender: TObject);
    

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormHash: TFormHash;

implementation

{$R *.dfm}

procedure TFormHash.btnBrowseInputClick(Sender: TObject);
begin
  if (dlgInputFile.Execute()) then
  begin
		txtInputFile.Text := dlgInputFile.FileName;
		dlgInputFile.InitialDir := dlgInputFile.GetNamePath();
	end;
end;

procedure TFormHash.btnHashClick(Sender: TObject);
begin
  try
		Screen.Cursor := crHourGlass;
		ipcHash1.Reset();
		ipcHash1.Algorithm := TipcHashAlgorithms(cboAlgorithm.ItemIndex);
		ipcHash1.EncodeHash := chkEncode.Checked;

		if (cboInputSource.ItemIndex = 0) then// String
    begin
			ipcHash1.InputMessage := txtInputString.Text;
    end
		else
    begin
			ipcHash1.InputFile := txtInputFile.Text;
    end;

		ipcHash1.ComputeHash();
		txtHashValue.Text := ipcHash1.HashValue;

	Except
    on ipcex: Exception do
		Application.ShowException(&ipcex);
	end;
	Screen.Cursor := crDefault;
end;

procedure TFormHash.cboInputSourceChange(Sender: TObject);
begin
  if cboInputSource.ItemIndex = 0 then
  begin
    grpInputFile.Enabled := false;
    grpInputString.Enabled := true;
  end
  else
  begin
    grpInputFile.Enabled := true;
    grpInputString.Enabled := false;
  end;
end;

procedure TFormHash.ipcHash1Progress(Sender: TObject; BytesProcessed: Int64;
  PercentProcessed: Integer);
begin
  ProgressBar1.Position := PercentProcessed;
end;

end.

