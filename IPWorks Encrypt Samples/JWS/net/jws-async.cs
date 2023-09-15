/*
 * IPWorks Encrypt 2022 .NET Edition - Sample Project
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

using System.Collections.Generic;
﻿using System;
using System.Threading.Tasks;
using nsoftware.async.IPWorksEncrypt;

class jwsDemo
{
  private static Jws jws = new nsoftware.async.IPWorksEncrypt.Jws();

  static async Task Main(string[] args)
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
        Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);
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
              jws.Algorithm = JwsAlgorithms.jwsHS256;
              if (key == "0") key = await GenerateBase64Key(algo);
              await jws.Config("KeyEncoding=1"); // base64
              jws.Key = key;
              break;
            case "hs384":
              jws.Algorithm = JwsAlgorithms.jwsHS384;
              if (key == "0") key = await GenerateBase64Key(algo);
              await jws.Config("KeyEncoding=1"); // base64
              jws.Key = key;
              break;
            case "hs512":
              jws.Algorithm = JwsAlgorithms.jwsHS512;
              if (key == "0") key = await GenerateBase64Key(algo);
              await jws.Config("KeyEncoding=1"); // base64
              jws.Key = key;
              break;
            case "rs256":
              jws.Algorithm = JwsAlgorithms.jwsRS256;
              jws.Certificate = new Certificate(CertStoreTypes.cstPFXFile, key, keyPassword, "*");
              break;
            case "rs384":
              jws.Algorithm = JwsAlgorithms.jwsRS384;
              jws.Certificate = new Certificate(CertStoreTypes.cstPFXFile, key, keyPassword, "*");
              break;
            case "rs512":
              jws.Algorithm = JwsAlgorithms.jwsRS512;
              jws.Certificate = new Certificate(CertStoreTypes.cstPFXFile, key, keyPassword, "*");
              break;
            case "ps256":
              jws.Algorithm = JwsAlgorithms.jwsPS256;
              jws.Certificate = new Certificate(CertStoreTypes.cstPFXFile, key, keyPassword, "*");
              break;
            case "ps384":
              jws.Algorithm = JwsAlgorithms.jwsPS384;
              jws.Certificate = new Certificate(CertStoreTypes.cstPFXFile, key, keyPassword, "*");
              break;
            case "ps512":
              jws.Algorithm = JwsAlgorithms.jwsPS512;
              jws.Certificate = new Certificate(CertStoreTypes.cstPFXFile, key, keyPassword, "*");
              break;
            default:
              throw new Exception("Invalid algorithm selection.\n");
          }

          // Sign payload and display output.
          jws.InputMessage = input;
          await jws.Sign();
          Console.Write("Payload signed:\n" + jws.OutputMessage);
        }
        else if (action == "verify")
        {
          switch (algo)
          {
            case "hs256":
            case "hs384":
            case "hs512":
              await jws.Config("KeyEncoding=1"); // base64
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
          await jws.Verify();
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

  private static async Task<string> GenerateBase64Key(string algo)
  {
    string base64key = "";
    try
    {
      // Generate key.
      Ezrand ezrand = new Ezrand();
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

      await ezrand.GetNextBytes();
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
  public static Dictionary<string, string> ParseArgs(string[] args)
  {
    Dictionary<string, string> dict = new Dictionary<string, string>();

    for (int i = 0; i < args.Length; i++)
    {
      // If it starts with a "/" check the next argument.
      // If the next argument does NOT start with a "/" then this is paired, and the next argument is the value.
      // Otherwise, the next argument starts with a "/" and the current argument is a switch.

      // If it doesn't start with a "/" then it's not paired and we assume it's a standalone argument.

      if (args[i].StartsWith("/"))
      {
        // Either a paired argument or a switch.
        if (i + 1 < args.Length && !args[i + 1].StartsWith("/"))
        {
          // Paired argument.
          dict.Add(args[i].TrimStart('/'), args[i + 1]);
          // Skip the value in the next iteration.
          i++;
        }
        else
        {
          // Switch, no value.
          dict.Add(args[i].TrimStart('/'), "");
        }
      }
      else
      {
        // Standalone argument. The argument is the value, use the index as a key.
        dict.Add(i.ToString(), args[i]);
      }
    }
    return dict;
  }

  public static string Prompt(string prompt, string defaultVal)
  {
    Console.Write(prompt + (defaultVal.Length > 0 ? " [" + defaultVal + "]": "") + ": ");
    string val = Console.ReadLine();
    if (val.Length == 0) val = defaultVal;
    return val;
  }
}