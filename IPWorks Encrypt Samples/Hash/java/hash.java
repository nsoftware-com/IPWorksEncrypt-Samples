/*
 * IPWorks Encrypt 2024 Java Edition - Sample Project
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

import java.io.*;
import ipworksencrypt.*;

public class hash extends ConsoleDemo {

  public static void main(String[] args) {
    

    try {   
      Hash hash1 = new Hash();
      
      System.out.println("******************************************************************************");
      System.out.println("* This demo show how to hash a file or string using a variety of algorithms  *");
      System.out.println("******************************************************************************");      
      
      while (true) {
        System.out.println("\nSpecify an algorithm:");
        System.out.println("0)  SHA1");
        System.out.println("1)  SHA-224");
        System.out.println("2)  SHA-256");
        System.out.println("3)  SHA-384");
        System.out.println("4)  SHA-512");
        System.out.println("5)  MD2");
        System.out.println("6)  MD4");
        System.out.println("7)  MD5");
        System.out.println("8)  RIPEMD-160");
        System.out.println("9)  MD5SHA1");
        System.out.println("10) HMAC-MD5");
        System.out.println("11) HMAC-SHA1");
        System.out.println("12) HMAC-SHA224");
        System.out.println("13) HMAC-SHA256");
        System.out.println("14) HMAC-SHA384");
        System.out.println("15) HMAC-SHA512");
        System.out.println("16) HMAC-RIPEMD160");
        System.out.println("17) SHA-3-224");
        System.out.println("18) SHA-3-256");
        System.out.println("19) SHA-3-384");
        System.out.println("20) SHA-3-512");
        hash1.setAlgorithm(Integer.parseInt(prompt("Algorithm", ":", "0")));
        
        hash1.setEncodeHash(ask("\nHex Encode Hash Value") == 'y');
        
        System.out.println("\nSelect an input source:");
        System.out.println("0) String");
        System.out.println("1) File");        
        
        if (Integer.parseInt(prompt("Input Source", ":", "0")) == 0) { //String
          System.out.println("Enter the message. To end the message, enter \":q\" on a single line by itself.");
            String inputString = "";
            String temp = prompt("Input String", ":\n");
          while (true) {
            if (temp.equals(":q"))
              break;
            inputString += temp + "\n";
            temp = input();
          }
          hash1.setInputMessage(inputString);
        }
        else {
          hash1.setInputFile(prompt("Input File"));
        }
        
        System.out.println("\nComputing Hash....");
        hash1.computeHash();
        System.out.println("\nHash Value: " + new String(hash1.getHashValue()));
        
        if (ask("\nDo you wish to hash another message") != 'y')
          break;
      } 
    } catch (IPWorksEncryptException ex) {
      displayError(ex);
    } catch (Exception ex) {
      displayError(ex);
    }
  }
}


class ConsoleDemo {
  private static BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));

  static String input() {
    try {
      return bf.readLine();
    } catch (IOException ioe) {
      return "";
    }
  }
  static char read() {
    return input().charAt(0);
  }

  static String prompt(String label) {
    return prompt(label, ":");
  }
  static String prompt(String label, String punctuation) {
    System.out.print(label + punctuation + " ");
    return input();
  }
  static String prompt(String label, String punctuation, String defaultVal) {
      System.out.print(label + " [" + defaultVal + "]" + punctuation + " ");
      String response = input();
      if (response.equals(""))
        return defaultVal;
      else
        return response;
  }

  static char ask(String label) {
    return ask(label, "?");
  }
  static char ask(String label, String punctuation) {
    return ask(label, punctuation, "(y/n)");
  }
  static char ask(String label, String punctuation, String answers) {
    System.out.print(label + punctuation + " " + answers + " ");
    return Character.toLowerCase(read());
  }

  static void displayError(Exception e) {
    System.out.print("Error");
    if (e instanceof IPWorksEncryptException) {
      System.out.print(" (" + ((IPWorksEncryptException) e).getCode() + ")");
    }
    System.out.println(": " + e.getMessage());
    e.printStackTrace();
  }

  /**
   * Takes a list of switch arguments or name-value arguments and turns it into a map.
   */
  static java.util.Map<String, String> parseArgs(String[] args) {
    java.util.Map<String, String> map = new java.util.HashMap<String, String>();
    
    for (int i = 0; i < args.length; i++) {
      // Add a key to the map for each argument.
      if (args[i].startsWith("-")) {
        // If the next argument does NOT start with a "-" then it is a value.
        if (i + 1 < args.length && !args[i + 1].startsWith("-")) {
          // Save the value and skip the next entry in the list of arguments.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), args[i + 1]);
          i++;
        } else {
          // If the next argument starts with a "-", then we assume the current one is a switch.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), "");
        }
      } else {
        // If the argument does not start with a "-", store the argument based on the index.
        map.put(Integer.toString(i), args[i].toLowerCase());
      }
    }
    return map;
  }
}



