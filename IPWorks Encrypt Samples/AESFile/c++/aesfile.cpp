/*
 * IPWorks Encrypt 2024 C++ Edition - Sample Project
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
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "../../include/ipworksencrypt.h"
#define LINE_LEN 100
#define MESSAGE_LEN 1024

int main(int argc, char **argv)
{
	
  int ret_code = 0;
  char inputFile[LINE_LEN];
  char outputFile[LINE_LEN];
  char operation[LINE_LEN];
  char password[LINE_LEN];
  
  AESFile aesfile;

  printf("******************************************************************************\n");
  printf("* This demo show how to encrypt and decrypt using our standard file format   *\n");
  printf("* via the AESFile component. The component can also be used to encrypt and   *\n");
  printf("* decrypt in memory.                                                         *\n");
  printf("******************************************************************************\n");

  printf("\nSelect an operation:\n");
  printf("1) Encrypt\n");
  printf("2) Decrypt\n");
  printf("\nOperation: ");
  fgets(operation, LINE_LEN, stdin);
  operation[strlen(operation)-1] = '\0';
  
  // A password will be used to generate the XTS-AES master key
  // When using a password you only need to save this value.
  printf("\nPassword: ");
  fgets(password, LINE_LEN, stdin);
  password[strlen(password)-1] = '\0';

  printf("Input File: ");
  fgets(inputFile, LINE_LEN, stdin);
  inputFile[strlen(inputFile)-1] = '\0';

  printf("Output File: ");
  fgets(outputFile, LINE_LEN, stdin);
  outputFile[strlen(outputFile)-1] = '\0';

  int op = atoi(operation);
  if (op == 1) // Encrypt
  {
    aesfile.SetInputFile(inputFile);
    aesfile.SetOutputFile(outputFile);
    aesfile.SetPassword(password);
    ret_code = aesfile.Encrypt();
    if (ret_code) // Got an error.
    {
	    printf( "\nError: %d", ret_code );
	    printf( " \"%s\"\n", aesfile.GetLastError());
	  }
    else
    {
      printf("\nEncryption successful!");
    }
  } 
  else if (op == 2) // Decrypt
  {
    aesfile.SetInputFile(inputFile);
    aesfile.SetOutputFile(outputFile);
    aesfile.SetPassword(password);
    ret_code = aesfile.Decrypt();
    if (ret_code) // Got an error.
    {
	    printf( "\nError: %d", ret_code );
	    printf( " \"%s\"\n", aesfile.GetLastError());
	  }
    else
    {
      printf("\nDecryption successful!");
    }
  } 
  else 
  {
    printf("\nInvalid operation specified.");
  }

	fprintf(stderr, "\n\npress <return> to continue...");
	getchar();
	exit(ret_code);
	return 0;
}

