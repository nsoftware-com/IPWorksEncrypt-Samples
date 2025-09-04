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

class jwtDemo
{
  private static JWT jwt = new nsoftware.IPWorksEncrypt.JWT();

  static void Main(string[] args)
  {
    if (args.Length < 8)
    {
      Console.WriteLine("usage: jwt /a action /alg algorithm /k key /i input [/p keypassword]\n");
      Console.WriteLine("  action       chosen from {sign, verify}");
      Console.WriteLine("  algorithm    the HMAC or RSA algorithm to use, chosen from {HS256, HS384, HS512, RS256, RS384, RS512, PS256, PS384, PS512}");
      Console.WriteLine("  key          for HMAC, the base64 key or '0' to use hard-coded key");
      Console.WriteLine("               for RSA, the path to the key certificate (private for signing, public for verifying)");
      Console.WriteLine("  input        a configuration string of headers/claim to sign or payload to verify");
      Console.WriteLine("               format: 'name=value,...' with names chosen from {kid, aud, sub, iss, jti, iat}");
      Console.WriteLine("  keypassword  the key certificate password (required only for private certificates with passwords)");
      Console.WriteLine("\nExamples: jwt /a sign /alg HS256 /k txAVam2uGT20a+ZJC1VWVGCM8tFYSKyJlw+2fgS/BdA= /i \"kid=4321\"");
      Console.WriteLine("          jwt /a sign /alg HS512 /k 0 /i \"kid=8554,aud=test,sub=test,iss=me\"");
      Console.WriteLine("          jwt /a sign /alg RS384 /k .\\testrsapriv.pfx /i \"kid=1234,aud=test\" /p test");
      Console.WriteLine("          jwt /a verify /alg HS256 /k ygIg4/Ut0KwUK2nS6fnflj1C5pAhgiXmVzqRqR2WTyU= /i eyJhbGciOiJIUzI1NiIsImtpZCI6Ijg1NTQifQ.eyJhdWQiOlsidGVzdCJdLCJpc3MiOiJtZSIsInN1YiI6InRlc3QifQ.Jzmq1XLymTbGZZC0pTnrPO5lAEeMInIKgVPulutqP4k");
      Console.WriteLine("          jwt /a verify /alg PS256 /k .\\testrsapub.cer /i eyJhbGciOiJQUzI1NiIsImtpZCI6Ijg1NTQifQ.eyJhdWQiOlsidGVzdCJdLCJpc3MiOiJtZSIsInN1YiI6InRlc3QifQ.BoQqXWB5YGUh3bid2OpGfW1yZw3j--KCZiNc9nyaMVQefLZUDyeoOHzfN9tNR1jEH9bQHPiRbkr2HyzIZzMdDiKHasg1vGnfUnuaN4y_b5gb6n4qw4AUxTjpPzu3sQfbBuKZUca99sx3YTR76KqAw57rS-gMQG0-YMTNJdEapE7igLlsCqCiPojqB-9KcJoj6PtsVqfds0RVCV_v_LUJvgWJaPznn80wpyAr-_VG07XO7qRSZTShoH7akkGgh0SrFn6TWHsCbF1UBzmkwzk7gYSInq1nUPJ31-up7F_kcbpXRmk8Kg0nZKn_Ol0uFyF3y5e8Tfw6FSjt0gHgUB9XlQ\n");
    }
    else
    {
      jwt.OnHeaderParam += jwt_OnHeaderParam;
      jwt.OnClaimInfo += jwt_OnClaimInfo;

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
          ParseMessageIntoJWT(input, jwt);

          // Set the proper algorithm and key.
          switch (algo)
          {
            case "hs256":
              jwt.SigningAlgorithm = JWTSigningAlgorithms.saHS256;
              if (key == "0") key = "txAVam2uGT20a+ZJC1VWVGCM8tFYSKyJlw+2fgS/BdA=";
              jwt.Config("KeyEncoding=1"); // base64
              jwt.Key = key;
              break;
            case "hs384":
              jwt.SigningAlgorithm = JWTSigningAlgorithms.saHS384;
              if (key == "0") key = "5C/iq/SVHc1i++8elF0u3Cg8w1D1Nj8Idrsw2zzIQeLrolmPk5d26f6MxTE3Npy2";
              jwt.Config("KeyEncoding=1"); // base64
              jwt.Key = key;
              break;
            case "hs512":
              jwt.SigningAlgorithm = JWTSigningAlgorithms.saHS512;
              if (key == "0") key = "AGVJSwvgVMU0cspZ7ChlxURcgCcdj7QV6nm0fr0C/rNtuh8F5uA7nCs4efKuWUDBw7/s9ikfTm0Kx4uZ3SYXcA==";
              jwt.Config("KeyEncoding=1"); // base64
              jwt.Key = key;
              break;
            case "rs256":
              jwt.SigningAlgorithm = JWTSigningAlgorithms.saRS256;
              jwt.Certificate = new Certificate(CertStoreTypes.cstPFXFile, key, keyPassword, "*");
              break;
            case "rs384":
              jwt.SigningAlgorithm = JWTSigningAlgorithms.saRS384;
              jwt.Certificate = new Certificate(CertStoreTypes.cstPFXFile, key, keyPassword, "*");
              break;
            case "rs512":
              jwt.SigningAlgorithm = JWTSigningAlgorithms.saRS512;
              jwt.Certificate = new Certificate(CertStoreTypes.cstPFXFile, key, keyPassword, "*");
              break;
            case "ps256":
              jwt.SigningAlgorithm = JWTSigningAlgorithms.saPS256;
              jwt.Certificate = new Certificate(CertStoreTypes.cstPFXFile, key, keyPassword, "*");
              break;
            case "ps384":
              jwt.SigningAlgorithm = JWTSigningAlgorithms.saPS384;
              jwt.Certificate = new Certificate(CertStoreTypes.cstPFXFile, key, keyPassword, "*");
              break;
            case "ps512":
              jwt.SigningAlgorithm = JWTSigningAlgorithms.saPS512;
              jwt.Certificate = new Certificate(CertStoreTypes.cstPFXFile, key, keyPassword, "*");
              break;
            default:
              throw new Exception("Invalid algorithm selection.\n");
          }

          jwt.Sign();
          Console.WriteLine("Encoded JWT: " + jwt.EncodedJWT);
        }
        else if (action == "verify")
        {
          switch (algo)
          {
            case "hs256":
              if (key == "0") key = "txAVam2uGT20a+ZJC1VWVGCM8tFYSKyJlw+2fgS/BdA=";
              jwt.Config("KeyEncoding=1"); // base64
              jwt.Key = key;
              break;
            case "hs384":
              if (key == "0") key = "5C/iq/SVHc1i++8elF0u3Cg8w1D1Nj8Idrsw2zzIQeLrolmPk5d26f6MxTE3Npy2";
              jwt.Config("KeyEncoding=1"); // base64
              jwt.Key = key;
              break;
            case "hs512":
              if (key == "0") key = "AGVJSwvgVMU0cspZ7ChlxURcgCcdj7QV6nm0fr0C/rNtuh8F5uA7nCs4efKuWUDBw7/s9ikfTm0Kx4uZ3SYXcA==";
              jwt.Config("KeyEncoding=1"); // base64
              jwt.Key = key;
              break;
            case "rs256":
            case "rs384":
            case "rs512":
            case "ps256":
            case "ps384":
            case "ps512":
              jwt.SignerCert = new Certificate(key);
              break;
            default:
              throw new Exception("Invalid algorithm selection.\n");
          }

          jwt.EncodedJWT = input;
          jwt.Verify();
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

  private static void ParseMessageIntoJWT(string message, JWT token)
  {
    string[] argsMsg = message.Split(",");

    foreach (string arg in argsMsg)
    {
      string name = arg.Split("=")[0];
      string value = arg.Split("=")[1];
      try
      {
        switch (name)
        {
          case "kid":
            token.KeyId = value;
            break;
          case "aud":
            token.ClaimAudience = value;
            break;
          case "sub":
            token.ClaimSubject = value;
            break;
          case "iss":
            token.ClaimIssuer = value;
            break;
          case "jti":
            token.ClaimJWTId = value;
            break;
          case "iat":
            token.ClaimIssuedAt = value;
            break;
          default:
            throw new Exception("Invalid header or claim.\n");
        }
      }
      catch (Exception ex)
      {
        Console.WriteLine(ex.Message);
      }
    }
  }

  private static void jwt_OnHeaderParam(object sender, JWTHeaderParamEventArgs e)
  {
    Console.WriteLine("\"" + e.Name + "\"=\"" + e.Value + "\"");
  }

  private static void jwt_OnClaimInfo(object sender, JWTClaimInfoEventArgs e)
  {
    Console.WriteLine("\"" + e.Name + "\"=\"" + e.Value + "\"");
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