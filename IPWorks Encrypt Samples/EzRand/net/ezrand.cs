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

class ezrandDemo
{
  private static EzRand ezrand = new nsoftware.IPWorksEncrypt.EzRand();

  static void Main(string[] args)
  {
    if (args.Length < 4)
    {
      Console.WriteLine("usage: ezrand [/i /f from /t to] /c count [/l length] [/s seed] /alg algorithm\n");
      Console.WriteLine("  /i           whether to generate random integers instead of bytes (optional)");
      Console.WriteLine("  from         the lower bound of the random number to be generated (inclusive) (optional, default 0)");
      Console.WriteLine("  to           the upper bound of the random number to be generated (exclusive) (optional, default 100)");
      Console.WriteLine("  count        the number of random integers or byte arrays to generate");
      Console.WriteLine("  length       the length of the byte array to be generated (optional, default 16)");
      Console.WriteLine("  seed         the seed to use (optional)");
      Console.WriteLine("  algorithm    the random number algorithm to use, chosen from {ISAAC, CryptoAPI, Platform, SecurePlatform, RC4Random}");
      Console.WriteLine("\nExample: ezrand /i /f 0 /t 100 /c 5 /alg ISAAC\n");
    }
    else
    {
      System.Collections.Generic.Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);
      bool ints = myArgs.ContainsKey("i");
      int count = int.Parse(myArgs["c"]);

      SelectAlgorithm(myArgs["alg"]);

      // Set up the random integer or byte generation.
      if (myArgs.ContainsKey("l")) ezrand.RandBytesLength = int.Parse(myArgs["l"]);
      if (myArgs.ContainsKey("f")) ezrand.Min = int.Parse(myArgs["f"]);
      if (myArgs.ContainsKey("t")) ezrand.Max = int.Parse(myArgs["t"]);
      if (myArgs.ContainsKey("s")) ezrand.Seed = myArgs["s"];

      // Perform the random integer or byte generation.
      if (ints)
      {
        for (int i = 0; i < count; i++)
        {
          ezrand.GetNextInt();
          Console.WriteLine(ezrand.RandInt);
        }
      }
      else
      {
        for (int i = 0; i < count; i++)
        {
          ezrand.GetNextBytes();

          // Display each byte as 2 uppercase hexadecimal characters.
          string result = "";
          foreach (byte myByte in ezrand.RandBytesB)
          {
            result += myByte.ToString("X2") + " ";
          }

          Console.WriteLine(result);
        }
      }
    }
  }

  private static void SelectAlgorithm(string algo)
  {
    switch (algo.ToLower())
    {
      case "isaac":
        ezrand.Algorithm = EzRandAlgorithms.raISAAC;
        break;
      case "cryptoapi":
        ezrand.Algorithm = EzRandAlgorithms.raMSCryptoAPI;
        break;
      case "platform":
        ezrand.Algorithm = EzRandAlgorithms.raPlatform;
        break;
      case "secureplatform":
        ezrand.Algorithm = EzRandAlgorithms.raSecurePlatform;
        break;
      case "rc4random":
        ezrand.Algorithm = EzRandAlgorithms.raRC4Random;
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