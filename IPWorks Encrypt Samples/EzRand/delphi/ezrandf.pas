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
unit ezrandf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, FileCtrl, ipccore, ipctypes, ipcezrand;

type
  TFormEzrand = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    PageControl1: TPageControl;
    tsIntegers: TTabSheet;
    Label3: TLabel;
    Label4: TLabel;
    txtMinValue: TEdit;
    txtMaxValue: TEdit;
    btnIntClear: TButton;
    btnGetNextInt: TButton;
    txtIntOutput: TMemo;
    tsBytes: TTabSheet;
    Label7: TLabel;
    txtBytesOutput: TMemo;
    btnGetNextBytes: TButton;
    btnBytesClear: TButton;
    txtBytesLength: TEdit;
    cboAlgorithms: TComboBox;
    ipcEzRand1: TipcEzRand;
    Label5: TLabel;
    txtSeed: TEdit;
    procedure btnGetNextIntClick(Sender: TObject);
    procedure btnGetNextBytesClick(Sender: TObject);
    procedure btnIntClearClick(Sender: TObject);
    procedure btnBytesClearClick(Sender: TObject);
    procedure txtSeedChange(Sender: TObject);
    procedure cboAlgorithmsClick(Sender: TObject);

  private
    function displayBytes(inputBytes : TBytes) : String;
    function selectAlgorithm() : TipcezrandAlgorithms;
  public
    { Public declarations }
  end;

var
  FormEzrand: TFormEzrand;

implementation

{$R *.dfm}

function TFormEzrand.displayBytes(inputBytes: TBytes)  : String;
var
  i : Integer;
  hexString : String;
begin
  hexString := '';

  for i := Low(inputBytes) to High(inputBytes) do
  begin
    hexString := Format('%s%.2x ', [hexString, inputBytes[i]]);
  end;

  Result := hexString;
end;

function TFormEzrand.selectAlgorithm : TipcezrandAlgorithms;
begin
  case cboAlgorithms.ItemIndex of
    0 : Result := raISAAC;
    1 : Result := raMSCryptoAPI;
    2 : Result := raPlatform;
    3 : Result := raSecurePlatform;
  else
    ShowMessage('Unknown Algorithm');
    Result := ipcEzRand1.Algorithm;
  end;
end;

procedure TFormEzrand.btnGetNextIntClick(Sender: TObject);
begin
  try
    begin
      ipcEzRand1.Min := StrToInt(txtMinValue.Text);
      ipcEzRand1.Max := StrToInt(txtMaxValue.Text);

      ipcEzRand1.GetNextInt;
      txtIntOutput.Lines.Add(IntToStr(ipcEzRand1.RandInt));
    end;
  Except
    on ipcex: Exception do
      Application.ShowException(&ipcex);
  end;
end;

procedure TFormEzrand.btnGetNextBytesClick(Sender: TObject);
var
  i : Integer;
begin
  try
    begin
      ipcEzRand1.RandBytesLength := StrToInt(txtBytesLength.Text);

      ipcEzRand1.GetNextBytes;
      txtBytesOutput.Lines.Add(displayBytes(ipcEzRand1.RandBytesB));
    end;
  Except
    on ipcex: Exception do
      Application.ShowException(&ipcex);
  end;
end;

procedure TFormEzrand.btnIntClearClick(Sender: TObject);
begin
  ipcEzRand1.Reset;
  txtIntOutput.Clear;
end;

procedure TFormEzrand.cboAlgorithmsClick(Sender: TObject);
begin
  ipcEzRand1.Algorithm := selectAlgorithm;
end;

procedure TFormEzrand.txtSeedChange(Sender: TObject);
begin
  ipcEzRand1.Seed := txtSeed.Text;
end;

procedure TFormEzrand.btnBytesClearClick(Sender: TObject);
begin
  ipcEzRand1.Reset;
  txtBytesOutput.Clear;
end;

end.

