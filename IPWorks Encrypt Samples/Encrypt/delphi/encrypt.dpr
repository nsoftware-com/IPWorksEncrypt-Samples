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

program encrypt;

uses
  Forms,
  encryptf in 'encryptf.pas' {FormEncrypt};

begin
  Application.Initialize;

  Application.CreateForm(TFormEncrypt, FormEncrypt);
  Application.Run;
end.


         
