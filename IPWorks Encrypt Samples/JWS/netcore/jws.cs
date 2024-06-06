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

class jwsDemo
{
  private static JWS jws = new nsoftware.IPWorksEncrypt.JWS();

  static void Main(string[] args)
  {
    if (args.Length < 8)
    {
      Console.WriteLine("usage: jws /a action /alg algorithm /k key /i input [/p keypassword]\n");
      Console.WriteLine("  action       chosen from {sign, verify}");
      Console.WriteLine("  algorithm    the HMAC or RSA algorithm to use, chosen from {HS256, HS384, HS512, RS256, RS384, RS512, PS256, PS384, PS512}");
      Console.WriteLine("  key          for HMAC, the base64 key or '0' to generate a key");
      Console.WriteLine("               for RSA, the path to the key certificate (private for signing, public for verifying)");
      Console.WriteLine("  input        the payload string to sign or JWS string to verify");
      Console.WriteLine("  keypassword  the key certificate password (required only for private certificates with passwords)");
      Console.WriteLine("\nExamples: jws /a sign /alg HS256 /k txAVam2uGT20a+ZJC1VWVGCM8tFYSKyJlw+2fgS/BdA= /i \"Test message\"");
      Console.WriteLine("          jws /a sign /alg HS512 /k 0 /i \"Test message\"");
      Console.WriteLine("          jws /a sign /alg RS384 /k .\\testrsapriv.pfx /i \"Test message\" /p test");
      Console.WriteLine("          jws /a verify /alg HS256 /k ygIg4/Ut0KwUK2nS6fnflj1C5pAhgiXmVzqRqR2WTyU= /i eyJhbGciOiJIUzI1NiJ9.SGVsbG8.Deg4sWY8OL1pbXh6zVy7Wkr2brjVUrMBrIzeY5WlxM4");
      Console.WriteLine("          jws /a verify /alg PS256 /k .\\testrsapub.cer /i eyJhbGciOiJQUzI1NiJ9.SGVsbG8.AqVXRmp7nmy74WQSoFrpY-Y4flb60n2e_XTjl51t0P1l-BqSCFj79wfaNf9-MJxCYbHkuFPjwkBq9-vvzxse0V-Bd0cjlXA9RY-LRn_wRHXRZUqParsZhsvWSqHY8MC4xAkXWCJuiDPWIuvDnd8mJDr_7vVbjIRipfifPkMMn3ePSvRSXWSBobalZxM320sYhReDgCZi5Mjb21cMSdowWj048AXFM86yL50UTh5rl2op3dG5JB9JbqBwVPDybdG7TK9r_84LYAajbTF7MepyMGWMAP7oSV1G-zBnBqpUC-HpTMRC-9xt9G3H0t1lUPePOBwB5ZdMeABrkFOSTwcIbQ\n");
    }
    else
    {
      try
      {
        System.Collections.Generic.Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);
        string action = myArgs["a"].ToLower();
        string algo = myArgs["alg"].ToLower();
        string key = myArgs["k"];
        string input = myArgs["i"];
        string keyPassword = myArgs.ContainsKey("p") ? myArgs["p"] : "";

        // Perform the action.
        if (action == "sign")
        {
          // Set the proper algorithm and key.
          switch (algo)
          {
            case "hs256":
              jws.Algorithm = JWSAlgorithms.jwsHS256;
              if (key == "0") key = GenerateBase64Key(algo);
              jws.Config("KeyEncoding=1"); // base64
              jws.Key = key;
              break;
            case "hs384":
              jws.Algorithm = JWSAlgorithms.jwsHS384;
              if (key == "0") key = GenerateBase64Key(algo);
              jws.Config("KeyEncoding=1"); // base64
              jws.Key = key;
              break;
            case "hs512":
              jws.Algorithm = JWSAlgorithms.jwsHS512;
              if (key == "0") key = GenerateBase64Key(algo);
              jws.Config("KeyEncoding=1"); // base64
              jws.Key = key;
              break;
            case "rs256":
              jws.Algorithm = JWSAlgorithms.jwsRS256;
              jws.Certificate = new Certificate(CertStoreTypes.cstPFXFile, key, keyPassword, "*");
              break;
            case "rs384":
              jws.Algorithm = JWSAlgorithms.jwsRS384;
              jws.Certificate = new Certificate(CertStoreTypes.cstPFXFile, key, keyPassword, "*");
              break;
            case "rs512":
              jws.Algorithm = JWSAlgorithms.jwsRS512;
              jws.Certificate = new Certificate(CertStoreTypes.cstPFXFile, key, keyPassword, "*");
              break;
            case "ps256":
              jws.Algorithm = JWSAlgorithms.jwsPS256;
              jws.Certificate = new Certificate(CertStoreTypes.cstPFXFile, key, keyPassword, "*");
              break;
            case "ps384":
              jws.Algorithm = JWSAlgorithms.jwsPS384;
              jws.Certificate = new Certificate(CertStoreTypes.cstPFXFile, key, keyPassword, "*");
              break;
            case "ps512":
              jws.Algorithm = JWSAlgorithms.jwsPS512;
              jws.Certificate = new Certificate(CertStoreTypes.cstPFXFile, key, keyPassword, "*");
              break;
            default:
              throw new Exception("Invalid algorithm selection.\n");
          }

          // Sign payload and display output.
          jws.InputMessage = input;
          jws.Sign();
          Console.Write("Payload signed:\n" + jws.OutputMessage);
        }
        else if (action == "verify")
        {
          switch (algo)
          {
            case "hs256":
            case "hs384":
            case "hs512":
              jws.Config("KeyEncoding=1"); // base64
              jws.Key = key;
              break;
            case "rs256":
            case "rs384":
            case "rs512":
            case "ps256":
            case "ps384":
            case "ps512":
              jws.Certificate = new Certificate(key);
              break;
            default:
              throw new Exception("Invalid algorithm selection.\n");
          }

          // Verify JWS string and display output.
          jws.InputMessage = input;
          jws.Verify();
          Console.Write("JWS string verified:\n" + jws.OutputMessage);
        }
        else
        {
          throw new Exception("Invalid action.\n");
        }
      }
      catch (Exception ex)
      {
        Console.WriteLine(ex.Message);
      }
    }
  }

  private static string GenerateBase64Key(string algo)
  {
    string base64key = "";
    try
    {
      // Generate key.
      EzRand ezrand = new EzRand();
      switch (algo)
      {
        case "hs256":
          ezrand.RandBytesLength = 32;
          break;
        case "hs384":
          ezrand.RandBytesLength = 48;
          break;
        case "hs512":
          ezrand.RandBytesLength = 64;
          break;
        default:
          throw new Exception("Invalid algorithm selection.\n");
      }

      ezrand.GetNextBytes();
      base64key = Convert.ToBase64String(ezrand.RandBytesB);
      Console.WriteLine("Key generated for signing: " + base64key);
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
    }
    return base64key;
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
      // Add an key to the dictionary for each argument
      if (args[i].StartsWith("/"))
      {
        // If the next argument does NOT start with a "/" then it is a value.
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