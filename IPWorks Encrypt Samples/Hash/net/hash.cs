/*
 * IPWorks Encrypt 2024 .NET Edition - Sample Project
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
 * 
 */

﻿using System;
using nsoftware.IPWorksEncrypt;

class hashDemo
{
  private static Hash hash = new nsoftware.IPWorksEncrypt.Hash();

  static void Main(string[] args)
  {
    if (args.Length < 4)
    {
      Console.WriteLine("usage: hash /f inputfile /s inputstring [/hex] /alg algorithm\n");
      Console.WriteLine("  inputfile    the path to the input file (specify this or inputstring, but not both)");
      Console.WriteLine("  inputstring  the message to hash");
      Console.WriteLine("  /hex         whether to hex encode the hash value (optional)");
      Console.WriteLine("  algorithm    the hash algorithm to use, chosen from");
      Console.WriteLine("               {SHA1, SHA224, SHA256, SHA384, SHA512, MD2, MD4, MD5, RIPEMD160, MD5SHA1, HMACMD5, HMACSHA1,");
      Console.WriteLine("                HMACSHA224, HMACSHA256, HMACSHA384, HMACSHA512, HMACRIPEMD160, SHA3-224, SHA3-256, SHA3-384, SHA3-512}");
      Console.WriteLine("\nExample: hash /f c:\\myfile.txt /hex /alg sha1\n");
    }
    else
    {
      System.Collections.Generic.Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

      SelectAlgorithm(myArgs["alg"]);

      // Set up the hash.
      if (myArgs.ContainsKey("f")) hash.InputFile = myArgs["f"];
      if (myArgs.ContainsKey("s")) hash.InputMessage = myArgs["s"];
      hash.EncodeHash = myArgs.ContainsKey("hex");

      // Perform the hash.
      hash.ComputeHash();
      Console.WriteLine("Hash complete! Hash value: " + hash.HashValue);
    }
  }
  
  private static void SelectAlgorithm(string algo)
  {
    switch (algo.ToLower())
    {
      case "sha1":
        hash.Algorithm = HashAlgorithms.haSHA1;
        break;
      case "sha224":
        hash.Algorithm = HashAlgorithms.haSHA224;
        break;
      case "sha256":
        hash.Algorithm = HashAlgorithms.haSHA256;
        break;
      case "sha384":
        hash.Algorithm = HashAlgorithms.haSHA384;
        break;
      case "sha512":
        hash.Algorithm = HashAlgorithms.haSHA512;
        break;
      case "md2":
        hash.Algorithm = HashAlgorithms.haMD2;
        break;
      case "md4":
        hash.Algorithm = HashAlgorithms.haMD4;
        break;
      case "md5":
        hash.Algorithm = HashAlgorithms.haMD5;
        break;
      case "ripemd160":
        hash.Algorithm = HashAlgorithms.haRIPEMD160;
        break;
      case "md5sha1":
        hash.Algorithm = HashAlgorithms.haMD5SHA1;
        break;
      case "hmacmd5":
        hash.Algorithm = HashAlgorithms.haHMACMD5;
        break;
      case "hmacsha1":
        hash.Algorithm = HashAlgorithms.haHMACSHA1;
        break;
      case "hmacsha224":
        hash.Algorithm = HashAlgorithms.haHMACSHA224;
        break;
      case "hmacsha256":
        hash.Algorithm = HashAlgorithms.haHMACSHA256;
        break;
      case "hmacsha384":
        hash.Algorithm = HashAlgorithms.haHMACSHA384;
        break;
      case "hmacsha512":
        hash.Algorithm = HashAlgorithms.haHMACSHA512;
        break;
      case "hmacripemd160":
        hash.Algorithm = HashAlgorithms.haHMACRIPEMD160;
        break;
      case "sha3-224":
        hash.Algorithm = HashAlgorithms.haSHA3_224;
        break;
      case "sha3-256":
        hash.Algorithm = HashAlgorithms.haSHA3_256;
        break;
      case "sha3-384":
        hash.Algorithm = HashAlgorithms.haSHA3_384;
        break;
      case "sha3-512":
        hash.Algorithm = HashAlgorithms.haSHA3_512;
        break;
      default:
        throw new Exception("Invalid algorithm selection.\n");
    }
  }
}




class ConsoleDemo
{
  /// <summary>
  /// Takes a list of switch arguments or name-value arguments and turns it into a dictionary.
  /// </summary>
  public static System.Collections.Generic.Dictionary<string, string> ParseArgs(string[] args)
  {
    System.Collections.Generic.Dictionary<string, string> dict = new System.Collections.Generic.Dictionary<string, string>();

    for (int i = 0; i < args.Length; i++)
    {
      // Add a key to the dictionary for each argument.
      if (args[i].StartsWith("/"))
      {
        // If the next argument does NOT start with a "/", then it is a value.
        if (i + 1 < args.Length && !args[i + 1].StartsWith("/"))
        {
          // Save the value and skip the next entry in the list of arguments.
          dict.Add(args[i].ToLower().TrimStart('/'), args[i + 1]);
          i++;
        }
        else
        {
          // If the next argument starts with a "/", then we assume the current one is a switch.
          dict.Add(args[i].ToLower().TrimStart('/'), "");
        }
      }
      else
      {
        // If the argument does not start with a "/", store the argument based on the index.
        dict.Add(i.ToString(), args[i].ToLower());
      }
    }
    return dict;
  }
  /// <summary>
  /// Asks for user input interactively and returns the string response.
  /// </summary>
  public static string Prompt(string prompt, string defaultVal)
  {
    Console.Write(prompt + (defaultVal.Length > 0 ? " [" + defaultVal + "]": "") + ": ");
    string val = Console.ReadLine();
    if (val.Length == 0) val = defaultVal;
    return val;
  }
}